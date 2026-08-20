program midi_probe;
// ============================================================================
// Look at what arrives on a MIDI port. No audio, no patch, no voice pool —
// just the wire, so that when a note does not sound there is somewhere to find
// out whether it ever arrived.
//
//   midi_probe --list                 every port on the machine
//   midi_probe [port] [seconds]       open, connect, print what comes in
//   midi_probe [port] [seconds] --patch <f.patch> --wav <out.wav> [--poly N]
//                                     ...and PLAY it, to a file
//
// The --patch form is the whole chain with no audio device in it: wire to voice
// pool to samples on disk. It renders in step with the wall clock, so the file
// it writes is what the device would have played — which makes the live path
// something that can be measured rather than only listened to.
//
// `port` is "24:0" or any part of a port's name; with none given it connects to
// everything that can send. With no seconds it listens until Ctrl-C.
//
// Without a MIDI keyboard, this is still the whole test rig — alsa-utils are a
// sequencer of somebody else's making, so nothing here is our code checking our
// own work:
//
//   midi_probe 14:0 &                            listen on Midi Through
//   aplaymidi -p 14:0 job/midi/bach-air-violin1.mid
//
// or, live and by hand:
//
//   aseqdump -l                                  what exists
//   aconnect <ours> <theirs>                     wire it the other way round
// ============================================================================
{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, SedaiMIDIInput, SedaiPatchVoices;

type
  // The handlers are method pointers, so they need something to live on.
  TProbe = class
  private
    FStart: QWord;
    FNoteOn, FNoteOff, FCtrl, FBend: Integer;
    FQuiet: Boolean;
    FPool: TSedaiPatchVoicePool;   // optional: nil = only look, do not play
    function Stamp: string;
  public
    constructor Create(AQuiet: Boolean);
    property Pool: TSedaiPatchVoicePool read FPool write FPool;
    procedure Note(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean);
    procedure Ctrl(AChannel, AController, AValue: Byte);
    procedure Bend(AChannel: Byte; AValue: Integer);
    property NoteOnCount: Integer read FNoteOn;
    property NoteOffCount: Integer read FNoteOff;
    property CtrlCount: Integer read FCtrl;
    property BendCount: Integer read FBend;
  end;

constructor TProbe.Create(AQuiet: Boolean);
begin
  inherited Create;
  FStart := GetTickCount64;
  FNoteOn := 0; FNoteOff := 0; FCtrl := 0; FBend := 0;
  FQuiet := AQuiet;
  FPool := nil;
end;

function TProbe.Stamp: string;
begin
  Result := Format('%8.3f', [(GetTickCount64 - FStart) / 1000.0]);
end;

// Note names, because 61 means nothing and C#4 means something.
function NoteName(ANote: Byte): string;
const
  NAMES: array[0..11] of string =
    ('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B');
begin
  Result := NAMES[ANote mod 12] + IntToStr(Integer(ANote) div 12 - 1);
end;

procedure TProbe.Note(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean);
begin
  if ANoteOn then Inc(FNoteOn) else Inc(FNoteOff);
  if FPool <> nil then
  begin
    if ANoteOn then FPool.PostNoteOn(ANote, AVelocity / 127.0)
    else FPool.PostNoteOff(ANote);
  end;
  if FQuiet then Exit;
  if ANoteOn then
    WriteLn(Format('%s  ch%-2d  note on   %3d %-4s  vel %3d',
                   [Stamp, AChannel + 1, ANote, NoteName(ANote), AVelocity]))
  else
    WriteLn(Format('%s  ch%-2d  note off  %3d %-4s',
                   [Stamp, AChannel + 1, ANote, NoteName(ANote)]));
end;

procedure TProbe.Ctrl(AChannel, AController, AValue: Byte);
var
  Extra: string;
begin
  Inc(FCtrl);
  if FPool <> nil then
    case AController of
      64:       FPool.PostSustain(AValue >= 64);
      120, 123: FPool.PostAllNotesOff;
    end;
  if FQuiet then Exit;
  // Name the three that the voice pool actually acts on, so it is obvious when
  // a pedal is the reason something is or is not ringing.
  case AController of
     1: Extra := '  (mod wheel)';
    64: if AValue >= 64 then Extra := '  (sustain DOWN)' else Extra := '  (sustain up)';
   120: Extra := '  (all sound off)';
   123: Extra := '  (all notes off)';
  else  Extra := '';
  end;
  WriteLn(Format('%s  ch%-2d  cc %-3d    %3d%s',
                 [Stamp, AChannel + 1, AController, AValue, Extra]));
end;

procedure TProbe.Bend(AChannel: Byte; AValue: Integer);
begin
  Inc(FBend);
  // The default two-semitone range; the wire says how far the wheel moved, not
  // what that is supposed to mean.
  if FPool <> nil then FPool.PostPitchBend(AValue / 8192.0 * 2.0);
  if FQuiet then Exit;
  WriteLn(Format('%s  ch%-2d  bend      %6d  (%+.2f of range)',
                 [Stamp, AChannel + 1, AValue, AValue / 8192.0]));
end;

// ---------------------------------------------------------------------------

var
  MIDI: TSedaiMIDIInput;
  Probe: TProbe;
  Ports: TSedaiMIDIPortArray;
  I, N, Args: Integer;
  Spec: string;
  Secs: Single;
  Deadline, Started: QWord;
  Quiet: Boolean = False;
  DoList: Boolean = False;
  Total: Int64 = 0;
  PatchFile: string = '';
  WavFile: string = '';
  Poly: Integer = 16;
  Pool: TSedaiPatchVoicePool = nil;
  Pcm: TMemoryStream = nil;
  Rendered: Int64 = 0;
  Want: Int64;
  Todo, C, K: Integer;
  V, Peak, Sum: Single;
  Smp: SmallInt;

const
  SR    = 44100;
  BLOCK = 128;      // 2.9 ms: short enough that the wall clock stays honest

// A plain 16-bit WAV. Written here rather than through SedaiAudioFileWriter so
// this harness has no dependency beyond the two units it is testing.
procedure SaveWav(const AName: string; AChannels: Integer);
var
  F: TFileStream;
  U32: LongWord;
  W: Word;
  Bytes: LongWord;
begin
  Bytes := Pcm.Size;
  F := TFileStream.Create(AName, fmCreate);
  try
    F.WriteBuffer('RIFF', 4);
    U32 := 36 + Bytes;                F.WriteBuffer(U32, 4);
    F.WriteBuffer('WAVEfmt ', 8);
    U32 := 16;                        F.WriteBuffer(U32, 4);
    W := 1;                           F.WriteBuffer(W, 2);
    W := AChannels;                   F.WriteBuffer(W, 2);
    U32 := SR;                        F.WriteBuffer(U32, 4);
    U32 := SR * 2 * AChannels;        F.WriteBuffer(U32, 4);
    W := 2 * AChannels;               F.WriteBuffer(W, 2);
    W := 16;                          F.WriteBuffer(W, 2);
    F.WriteBuffer('data', 4);
    F.WriteBuffer(Bytes, 4);
    Pcm.Position := 0;
    F.CopyFrom(Pcm, Bytes);
  finally
    F.Free;
  end;
end;

procedure ListPorts;
var
  J: Integer;
begin
  if not MIDI.Enumerate(Ports) then
  begin
    WriteLn(StdErr, '  ', MIDI.LastError);
    Halt(1);
  end;
  WriteLn;
  WriteLn('  ', TSedaiMIDIInput.BackendName, ' — ', Length(Ports), ' porte');
  WriteLn;
  WriteLn('   addr    puo'' inviare   nome');
  WriteLn('   ------  ------------   ----------------------------------------');
  for J := 0 to High(Ports) do
    WriteLn(Format('   %3d:%-2d  %-12s   %s: %s',
                   [Ports[J].Client, Ports[J].Port,
                    BoolToStr(Ports[J].CanSend, 'si', 'no'),
                    Ports[J].ClientName, Ports[J].PortName]));
  WriteLn;
  WriteLn('  Quelle con "si" possono mandarci note. Per provare senza tastiera:');
  WriteLn('    midi_probe 14:0 &');
  WriteLn('    aplaymidi -p 14:0 job/midi/bach-air-violin1.mid');
  WriteLn;
end;

begin
  Spec := '';
  Secs := 0;
  Args := 0;
  for I := 1 to ParamCount do
  begin
    if (ParamStr(I) = '--list') or (ParamStr(I) = '-l') then DoList := True
    else if ParamStr(I) = '--quiet' then Quiet := True
    else if Copy(ParamStr(I), 1, 8) = '--patch=' then
      PatchFile := Copy(ParamStr(I), 9, Length(ParamStr(I)))
    else if Copy(ParamStr(I), 1, 6) = '--wav=' then
      WavFile := Copy(ParamStr(I), 7, Length(ParamStr(I)))
    else if Copy(ParamStr(I), 1, 7) = '--poly=' then
      Poly := StrToIntDef(Copy(ParamStr(I), 8, Length(ParamStr(I))), 16)
    else
    begin
      Inc(Args);
      if Args = 1 then Spec := ParamStr(I)
      else if Args = 2 then Secs := StrToFloatDef(ParamStr(I), 0);
    end;
  end;

  if not TSedaiMIDIInput.Available then
  begin
    WriteLn(StdErr, 'nessun backend MIDI: la libreria di sistema non c''e''.');
    WriteLn(StdErr, 'su Linux serve libasound2 (il pacchetto runtime, non il -dev).');
    Halt(1);
  end;

  MIDI := TSedaiMIDIInput.Create;
  Probe := nil;
  try
    if DoList then
    begin
      ListPorts;
      Halt(0);
    end;

    if not MIDI.Open('midi_probe') then
    begin
      WriteLn(StdErr, MIDI.LastError);
      Halt(1);
    end;

    if Spec <> '' then N := MIDI.ConnectByName(Spec)
    else N := MIDI.ConnectAnything;

    WriteLn;
    // MME has no address to publish: there, a port IS a device, and the only
    // thing worth printing is which one we took.
    if MIDI.PortSpec <> '' then
      WriteLn('  ', TSedaiMIDIInput.BackendName, ' — la nostra porta e'' ', MIDI.PortSpec)
    else
      WriteLn('  ', TSedaiMIDIInput.BackendName, ' — nessun indirizzo da pubblicare');
    if N = 0 then
    begin
      // Not fatal: something can still be wired to us from the other side with
      // aconnect while this is running. But say it, loudly — a probe that hears
      // nothing because it is connected to nothing looks exactly like a probe
      // that hears nothing because the keyboard is broken.
      if Spec <> '' then
        WriteLn('  NIENTE COLLEGATO: nessuna porta corrisponde a "', Spec, '".')
      else
        WriteLn('  NIENTE COLLEGATO: nessuna porta puo'' inviare.');
      WriteLn('  Si puo'' collegare da fuori:  aconnect <sorgente> ', MIDI.PortSpec);
    end
    else
      WriteLn('  collegate ', N, ' porte in ingresso');
    if Secs > 0 then
      WriteLn(Format('  in ascolto per %.1f s...', [Secs]))
    else
      WriteLn('  in ascolto, Ctrl-C per uscire...');
    WriteLn;

    Probe := TProbe.Create(Quiet);
    MIDI.OnNote := @Probe.Note;
    MIDI.OnController := @Probe.Ctrl;
    MIDI.OnPitchBend := @Probe.Bend;

    if PatchFile <> '' then
    begin
      Pool := TSedaiPatchVoicePool.Create;
      if not Pool.LoadFromFile(PatchFile, Poly) then
      begin
        WriteLn(StdErr, Pool.LastError);
        Halt(1);
      end;
      if Pool.LastError <> '' then WriteLn('  ', Pool.LastError);
      if Pool.Warnings <> '' then Write(Pool.Warnings);
      Pool.Prepare(SR, BLOCK);
      Pool.Reset;
      Probe.Pool := Pool;
      Pcm := TMemoryStream.Create;
      WriteLn('  patch: ', ExtractFileName(PatchFile), ', ', Pool.VoiceCount,
              ' voci, resa in tempo reale su ', WavFile);
      WriteLn;
    end;

    Peak := 0.0; Sum := 0.0;
    Started := GetTickCount64;
    Deadline := Started + Round(Secs * 1000);
    repeat
      Total := Total + MIDI.Poll;

      if Pool <> nil then
      begin
        // Keep up with the wall clock, never run ahead of it: rendering early
        // would place events later than they arrived, and the point of this
        // harness is that the file is what the device would have played.
        Want := (Int64(GetTickCount64 - Started) * SR) div 1000;
        while Rendered < Want do
        begin
          Todo := BLOCK;
          if Want - Rendered < Todo then Todo := Integer(Want - Rendered);
          if Todo < 1 then Break;
          Pool.Render(Todo);
          for K := 0 to Todo - 1 do
            for C := 0 to Pool.OutputCount - 1 do
            begin
              V := Pool.MixSample(C, K);
              if Abs(V) > Peak then Peak := Abs(V);
              Sum := Sum + V * V;
              if V > 1.0 then V := 1.0 else if V < -1.0 then V := -1.0;
              Smp := Round(V * 32767.0);
              Pcm.WriteBuffer(Smp, 2);
            end;
          Rendered := Rendered + Todo;
        end;
      end;

      // 1 ms: this is a probe, and the point is to see arrival as it happens
      // rather than to be frugal.
      Sleep(1);
    until (Secs > 0) and (GetTickCount64 >= Deadline);

    if Pool <> nil then
    begin
      if WavFile <> '' then SaveWav(WavFile, Pool.OutputCount);
      WriteLn;
      WriteLn(Format('  resi %d campioni (%.1f s), picco %.4f, RMS %.4f',
                     [Rendered, Rendered / SR, Peak,
                      Sqrt(Sum / (Rendered * Pool.OutputCount + 1))]));
      if Pool.EventsDropped > 0 then
        WriteLn('  ATTENZIONE: ', Pool.EventsDropped, ' eventi persi (coda piena).');
      if Peak < 1e-6 then
        WriteLn('  ATTENZIONE: silenzio assoluto — le note sono arrivate ma non hanno suonato.');
    end;

    WriteLn;
    WriteLn(Format('  %d messaggi: %d note on, %d note off, %d cc, %d bend',
                   [Total, Probe.NoteOnCount, Probe.NoteOffCount,
                    Probe.CtrlCount, Probe.BendCount]));
    if Probe.NoteOnCount <> Probe.NoteOffCount then
      WriteLn('  ATTENZIONE: note on e note off non pari — qualche nota resterebbe appesa.');
    if MIDI.RingDropped > 0 then
      WriteLn('  ATTENZIONE: ', MIDI.RingDropped, ' messaggi persi (anello pieno).');
    WriteLn;
  finally
    Probe.Free;
    MIDI.Free;
    Pool.Free;
    Pcm.Free;
  end;
end.
