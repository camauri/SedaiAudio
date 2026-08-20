program patch_play;
// Render a patch POLYPHONICALLY to a WAV, playing a list of notes.
//
//   patch_play <file.patch> <out.wav> <seconds> <polyphony> <notes> [gate-seconds]
//
//   notes    a comma-separated list of MIDI note numbers played together
//            (60,64,67 = a C major triad), or of note:onset pairs in seconds
//            (60:0,64:0.4,67:0.8 = an arpeggio)
//
//            @ adds a VELOCITY, 0..1:  60@0.25  is the same note struck softly,
//            and 60@0.25:0.5 is that note half a second in. Say nothing and it
//            is 1.0, which is what everything did before velocity existed.
//
// 60 sounds the patch's own base frequency; a semitone is 1/12 of a volt.
{$mode objfpc}{$H+}
uses SysUtils, Classes, Math, SedaiPatchGraph, SedaiPatchModules, SedaiPatchVoices;

const
  SAMPLE_RATE = 44100;
  BLOCK = 256;

type
  TNoteEvent = record
    Note: Integer;
    Onset: Single;
    Velocity: Single;
    Fired, Released: Boolean;
  end;

var
  Pool: TSedaiPatchVoicePool;
  Outp: TMemoryStream;
  Events: array of TNoteEvent;
  I, C, Total, Done, Todo, MaxActive: Integer;
  Secs, GateSec, V, Peak, T: Single;
  Poly: Integer;
  S: SmallInt;
  Parts: TStringArray;
  Bits: TStringArray;
  Head: string;
  At: Integer;

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
  if ParamCount < 5 then
  begin
    WriteLn('usage: patch_play <file.patch> <out.wav> <seconds> <polyphony> <notes> [gate-seconds]');
    WriteLn('       notes: 60,64,67   or   60:0,64:0.4,67:0.8');
    WriteLn('              60@0.25 = struck softly;  60@0.25:0.5 = softly, half a second in');
    Halt(2);
  end;
  Secs := StrToFloatDef(ParamStr(3), 3.0);
  Poly := StrToIntDef(ParamStr(4), 8);
  GateSec := Secs * 0.5;
  if ParamCount >= 6 then GateSec := StrToFloatDef(ParamStr(6), GateSec);

  Parts := ParamStr(5).Split([','], TStringSplitOptions.ExcludeEmpty);
  SetLength(Events, Length(Parts));
  for I := 0 to High(Parts) do
  begin
    Bits := Parts[I].Split([':'], TStringSplitOptions.ExcludeEmpty);
    // The note may carry a velocity after '@'. Split that off FIRST, because
    // the onset is separated by ':' and the two must not fight over the string.
    Head := Trim(Bits[0]);
    At := Pos('@', Head);
    if At > 0 then
    begin
      Events[I].Velocity := StrToFloatDef(Copy(Head, At + 1, Length(Head)), 1.0);
      Head := Copy(Head, 1, At - 1);
    end
    else
      Events[I].Velocity := 1.0;
    if Events[I].Velocity < 0.0 then Events[I].Velocity := 0.0;
    if Events[I].Velocity > 1.0 then Events[I].Velocity := 1.0;
    Events[I].Note := StrToIntDef(Head, 60);
    if Length(Bits) > 1 then Events[I].Onset := StrToFloatDef(Trim(Bits[1]), 0.0)
                        else Events[I].Onset := 0.0;
    Events[I].Fired := False;
    Events[I].Released := False;
  end;

  Pool := TSedaiPatchVoicePool.Create;
  Outp := TMemoryStream.Create;
  try
    if not Pool.LoadFromFile(ParamStr(1), Poly) then
    begin
      WriteLn(StdErr, Pool.LastError);
      Halt(1);
    end;
    if Pool.LastError <> '' then WriteLn('  ', Pool.LastError);
    if Pool.Warnings <> '' then Write(Pool.Warnings);
    Write(Pool.Describe);

    Pool.Prepare(SAMPLE_RATE, BLOCK);
    Pool.Reset;

    Total := Round(Secs * SAMPLE_RATE);
    Done := 0; Peak := 0.0; MaxActive := 0;

    while Done < Total do
    begin
      Todo := Total - Done;
      if Todo > BLOCK then Todo := BLOCK;
      T := Done / SAMPLE_RATE;

      for I := 0 to High(Events) do
      begin
        if (not Events[I].Fired) and (T >= Events[I].Onset) then
        begin
          Pool.NoteOn(Events[I].Note, Events[I].Velocity);
          Events[I].Fired := True;
        end;
        if Events[I].Fired and (not Events[I].Released) and
           (T >= Events[I].Onset + GateSec) then
        begin
          Pool.NoteOff(Events[I].Note);
          Events[I].Released := True;
        end;
      end;

      Pool.Render(Todo);
      if Pool.ActiveVoices > MaxActive then MaxActive := Pool.ActiveVoices;

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

    SaveWav(ParamStr(2), SAMPLE_RATE, Pool.OutputCount);
    WriteLn(Format('  %d notes, %d voices available, %d sounding at once, peak %.3f -> %s',
                   [Length(Events), Pool.VoiceCount, MaxActive, Peak, ParamStr(2)]));
  finally
    Outp.Free;
    Pool.Free;
  end;
end.
