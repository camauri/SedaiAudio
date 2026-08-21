program arr_render;
// ============================================================================
// Render an ARRANGEMENT: several instruments, each standing somewhere, driven
// by one MIDI file, summed into one stereo output. Offline, no audio device.
//
//   arr_render <file.arr> <song.mid> <out.wav> [seconds]
//   arr_render <file.arr>                       -- just describe it
//
// A .patch says what an instrument IS; a .arr says where it stands and how loud
// it is against the others. This is the thing that plays the second one.
// ============================================================================
{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Math,
  SedaiArrangement, SedaiMIDIPlayer, SedaiPatchVoices;

const
  BLOCK = 512;

type
  // OnNoteEvent is a method pointer, so it needs somewhere to live.
  TBridge = class
  private
    FArr: TSedaiArrangement;
    FOn, FOff, FLost: Integer;
  public
    constructor Create(AArr: TSedaiArrangement);
    procedure Note(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean);
    property NoteOns: Integer read FOn;
    property NoteOffs: Integer read FOff;
    property Unrouted: Integer read FLost;
  end;

constructor TBridge.Create(AArr: TSedaiArrangement);
begin
  inherited Create;
  FArr := AArr; FOn := 0; FOff := 0; FLost := 0;
end;

procedure TBridge.Note(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean);
var
  P: TSedaiArrPart;
begin
  // MIDI channels are 0-based on the wire and 1-based everywhere a person
  // writes them down, including in the .arr file.
  P := FArr.PartForChannel(AChannel + 1);
  if P = nil then
  begin
    // Counted, not silently dropped: a channel nobody claimed is the usual
    // reason half an arrangement is missing.
    Inc(FLost);
    Exit;
  end;
  if ANoteOn and (AVelocity > 0) then
  begin
    P.Pool.NoteOn(ANote, AVelocity / 127.0);
    Inc(FOn);
  end
  else
  begin
    P.Pool.NoteOff(ANote);
    Inc(FOff);
  end;
end;

var
  Arr: TSedaiArrangement;
  Player: TSedaiMIDIPlayer;
  Bridge: TBridge;
  Pcm: TMemoryStream;
  Buf: array[0..BLOCK * 2 - 1] of Single;
  Secs, Peak, V: Single;
  Total, Done, Todo, I: Integer;
  S: SmallInt;

procedure SaveWav(const AName: string; ARate: Integer);
var
  F: TFileStream;
  U32, Bytes: LongWord;
  W: Word;
begin
  Bytes := Pcm.Size;
  F := TFileStream.Create(AName, fmCreate);
  try
    F.WriteBuffer('RIFF', 4);
    U32 := 36 + Bytes;            F.WriteBuffer(U32, 4);
    F.WriteBuffer('WAVEfmt ', 8);
    U32 := 16;                    F.WriteBuffer(U32, 4);
    W := 1;                       F.WriteBuffer(W, 2);
    W := 2;                       F.WriteBuffer(W, 2);
    U32 := ARate;                 F.WriteBuffer(U32, 4);
    U32 := ARate * 4;             F.WriteBuffer(U32, 4);
    W := 4;                       F.WriteBuffer(W, 2);
    W := 16;                      F.WriteBuffer(W, 2);
    F.WriteBuffer('data', 4);
    F.WriteBuffer(Bytes, 4);
    Pcm.Position := 0;
    F.CopyFrom(Pcm, Bytes);
  finally
    F.Free;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('usage: arr_render <file.arr> <song.mid> <out.wav> [seconds]');
    WriteLn('       arr_render <file.arr>            solo descrivi');
    Halt(2);
  end;

  Arr := TSedaiArrangement.Create;
  Player := nil; Bridge := nil; Pcm := nil;
  try
    if not Arr.LoadFromFile(ParamStr(1)) then
    begin
      WriteLn(StdErr, ExtractFileName(ParamStr(1)), ': ', Arr.LastError);
      Halt(1);
    end;
    if Arr.Warnings <> '' then Write(Arr.Warnings);
    WriteLn;
    Write(Arr.Describe);

    if ParamCount < 3 then Halt(0);

    Arr.Prepare(Arr.SampleRate, BLOCK);
    Arr.Reset;

    Player := TSedaiMIDIPlayer.Create;
    Player.SetSampleRate(Arr.SampleRate);
    if not Player.LoadFromFile(ParamStr(2)) then
    begin
      WriteLn(StdErr, 'cannot load MIDI file: ', ParamStr(2));
      Halt(1);
    end;
    Bridge := TBridge.Create(Arr);
    Player.OnNoteEvent := @Bridge.Note;

    Secs := 0;
    if ParamCount >= 4 then Secs := StrToFloatDef(ParamStr(4), 0);
    if Secs <= 0 then Secs := Player.GetDurationSeconds + 2.0;   // let tails ring
    Total := Round(Secs * Arr.SampleRate);

    Pcm := TMemoryStream.Create;
    Player.Rewind;
    Player.Play;
    Done := 0; Peak := 0;
    while Done < Total do
    begin
      Todo := Total - Done;
      if Todo > BLOCK then Todo := BLOCK;
      // The score first, so this block's notes are already standing.
      Player.AdvanceSamples(Todo);
      Arr.Render(Todo, @Buf[0]);
      for I := 0 to Todo * 2 - 1 do
      begin
        V := Buf[I];
        if Abs(V) > Peak then Peak := Abs(V);
        S := Round(V * 32767.0);
        Pcm.WriteBuffer(S, 2);
      end;
      Inc(Done, Todo);
    end;

    SaveWav(ParamStr(3), Arr.SampleRate);
    WriteLn;
    WriteLn(Format('  %.1f s, %d note on, %d note off, picco %.3f -> %s',
                   [Secs, Bridge.NoteOns, Bridge.NoteOffs, Peak, ParamStr(3)]));
    if Bridge.Unrouted > 0 then
      WriteLn(Format('  ATTENZIONE: %d eventi su canali che nessuna parte reclama.',
                     [Bridge.Unrouted]));
    WriteLn;
  finally
    Pcm.Free;
    Bridge.Free;
    Player.Free;
    Arr.Free;
  end;
end.
