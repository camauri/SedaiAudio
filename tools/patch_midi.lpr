program patch_midi;
// ============================================================================
// Render a MIDI file through a .patch.
//
//   patch_midi <file.mid> <file.patch> <out.wav> [polyphony] [seconds] [transpose] [gain]
//
// This is the note-event layer that live MIDI input would also plug into: the
// player calls back with note on/off and the voice pool does the rest. Doing it
// from a file first means the whole path is verifiable without a MIDI device or
// a loopback driver.
// ============================================================================
{$mode objfpc}{$H+}
uses SysUtils, Classes, SedaiAudioTypes, SedaiMIDIPlayer, SedaiPatchVoices;

const
  SAMPLE_RATE = 44100;
  BLOCK = 256;

type
  // OnNoteEvent is a method pointer, so it needs an object to live on.
  TNoteBridge = class
  private
    FPool: TSedaiPatchVoicePool;
    FTranspose: Integer;
    FOnCount, FOffCount, FMaxVoices, FCtrlCount: Integer;
  public
    constructor Create(APool: TSedaiPatchVoicePool; ATranspose: Integer);
    procedure Handle(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean);
    procedure Ctrl(AChannel, AController, AValue: Byte);
    property OnCount: Integer read FOnCount;
    property CtrlCount: Integer read FCtrlCount;
    property OffCount: Integer read FOffCount;
    property MaxVoices: Integer read FMaxVoices;
  end;

constructor TNoteBridge.Create(APool: TSedaiPatchVoicePool; ATranspose: Integer);
begin
  inherited Create;
  FPool := APool;
  FTranspose := ATranspose;
  FOnCount := 0; FOffCount := 0; FMaxVoices := 0; FCtrlCount := 0;
end;

// The file's controllers, straight through. Whether a mod wheel written into a
// score does anything is the patch's business — a `cc` module is how it says
// yes — so this bridge forwards everything and decides nothing.
procedure TNoteBridge.Ctrl(AChannel, AController, AValue: Byte);
begin
  Inc(FCtrlCount);
  FPool.SetControl(AController, AValue / 127.0);
end;

procedure TNoteBridge.Handle(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean);
var
  N: Integer;
begin
  N := ANote + FTranspose;
  if (N < 0) or (N > 127) then Exit;
  // A note-on with velocity 0 is a note-off; the player already resolves that,
  // but honour it here too in case a file uses the other convention.
  if ANoteOn and (AVelocity > 0) then
  begin
    // The file's velocity, not a flat full strength. Every shipped instrument
    // is bit-identical at 127, so nothing that used to render one way renders
    // differently unless the file really does play softly.
    FPool.NoteOn(N, AVelocity / 127.0);
    Inc(FOnCount);
    if FPool.ActiveVoices > FMaxVoices then FMaxVoices := FPool.ActiveVoices;
  end
  else
  begin
    FPool.NoteOff(N);
    Inc(FOffCount);
  end;
end;

var
  Player: TSedaiMIDIPlayer;
  Pool: TSedaiPatchVoicePool;
  Bridge: TNoteBridge;
  Outp: TMemoryStream;
  Poly, Transpose, I, C, Todo, Done, Total: Integer;
  Secs, V, Peak, Gain: Single;
  S: SmallInt;

procedure SaveWav(const AFilename: string; ARate, AChannels: Integer);
var
  F: TFileStream;
  DataBytes, U32: LongWord;
  W: Word;
begin
  DataBytes := Outp.Size;
  F := TFileStream.Create(AFilename, fmCreate);
  try
    F.WriteBuffer('RIFF', 4);
    U32 := 36 + DataBytes;              F.WriteBuffer(U32, 4);
    F.WriteBuffer('WAVEfmt ', 8);
    U32 := 16;                          F.WriteBuffer(U32, 4);
    W := 1;                             F.WriteBuffer(W, 2);
    W := AChannels;                     F.WriteBuffer(W, 2);
    U32 := ARate;                       F.WriteBuffer(U32, 4);
    U32 := ARate * 2 * AChannels;       F.WriteBuffer(U32, 4);
    W := 2 * AChannels;                 F.WriteBuffer(W, 2);
    W := 16;                            F.WriteBuffer(W, 2);
    F.WriteBuffer('data', 4);
    F.WriteBuffer(DataBytes, 4);
    Outp.Position := 0;
    F.CopyFrom(Outp, DataBytes);
  finally
    F.Free;
  end;
end;

begin
  if ParamCount < 3 then
  begin
    WriteLn('usage: patch_midi <file.mid> <file.patch> <out.wav> [polyphony] [seconds] [transpose] [gain]');
    Halt(2);
  end;
  Poly := 16;      if ParamCount >= 4 then Poly := StrToIntDef(ParamStr(4), 16);
  Secs := 0;       if ParamCount >= 5 then Secs := StrToFloatDef(ParamStr(5), 0);
  Transpose := 0;  if ParamCount >= 6 then Transpose := StrToIntDef(ParamStr(6), 0);
  // Voices sum, so a chord is louder than a note. Gain staging is the user's
  // call rather than something guessed at: a dynamic normaliser would pump.
  Gain := 1.0;     if ParamCount >= 7 then Gain := StrToFloatDef(ParamStr(7), 1.0);

  Pool := TSedaiPatchVoicePool.Create;
  Player := TSedaiMIDIPlayer.Create;
  Outp := TMemoryStream.Create;
  Bridge := nil;
  try
    if not Pool.LoadFromFile(ParamStr(2), Poly) then
    begin
      WriteLn(StdErr, Pool.LastError);
      Halt(1);
    end;
    if Pool.LastError <> '' then WriteLn('  ', Pool.LastError);
    if Pool.Warnings <> '' then Write(Pool.Warnings);
    Pool.Prepare(SAMPLE_RATE, BLOCK);
    Pool.Reset;
    Pool.MasterGain := Gain;

    Player.SetSampleRate(SAMPLE_RATE);
    if not Player.LoadFromFile(ParamStr(1)) then
    begin
      WriteLn(StdErr, 'cannot load MIDI file: ', ParamStr(1));
      Halt(1);
    end;

    Bridge := TNoteBridge.Create(Pool, Transpose);
    Player.OnNoteEvent := @Bridge.Handle;
    Player.OnControlEvent := @Bridge.Ctrl;

    if Secs <= 0 then Secs := Player.GetDurationSeconds + 2.0;   // let tails ring
    Total := Round(Secs * SAMPLE_RATE);

    WriteLn(Format('  %s  %.1f s, %d voices, patch %s',
                   [ExtractFileName(ParamStr(1)), Player.GetDurationSeconds,
                    Poly, ExtractFileName(ParamStr(2))]));

    Player.Rewind;
    Player.Play;
    Done := 0; Peak := 0.0;

    while Done < Total do
    begin
      Todo := Total - Done;
      if Todo > BLOCK then Todo := BLOCK;

      // Advance the score first so this block's note events are already applied.
      Player.AdvanceSamples(Todo);
      Pool.Render(Todo);

      for I := 0 to Todo - 1 do
        for C := 0 to Pool.OutputCount - 1 do
        begin
          V := Pool.MixSample(C, I);
          if Abs(V) > Peak then Peak := Abs(V);
          if V > 1.0 then V := 1.0 else if V < -1.0 then V := -1.0;
          S := Round(V * 32767.0);
          Outp.WriteBuffer(S, 2);
        end;
      Inc(Done, Todo);
    end;

    SaveWav(ParamStr(3), SAMPLE_RATE, Pool.OutputCount);
    WriteLn(Format('  %d note-on, %d note-off, %d controller, %d voices at once, peak %.3f -> %s',
                   [Bridge.OnCount, Bridge.OffCount, Bridge.CtrlCount,
                    Bridge.MaxVoices, Peak, ParamStr(3)]));
  finally
    Bridge.Free;
    Outp.Free;
    Player.Free;
    Pool.Free;
  end;
end.
