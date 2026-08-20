program patch_render;
// Render a .patch file to a WAV. This is P1's deliverable: the graph runs.
//
//   patch_render <file.patch> <out.wav> <seconds> [semitones] [gate-seconds]
//
// The note module (if the patch declares one) is driven with the given pitch in
// volts-per-octave and a gate that opens at 0 and closes after gate-seconds.
{$mode objfpc}{$H+}
uses SysUtils, Classes, SedaiPatchGraph, SedaiPatchModules, SedaiPatchFile;

const
  SAMPLE_RATE = 44100;
  BLOCK = 256;

var
  Graph: TSedaiPatchGraph;
  Res: TSedaiPatchLoadResult;
  Outp: TMemoryStream;
  Note: TSedaiModNote;
  I, C, N, Todo, Done, GateSamples, Total: Integer;
  Semis, GateSec, Secs, V: Single;
  S: SmallInt;
  Peak: Single;

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
    WriteLn('usage: patch_render <file.patch> <out.wav> <seconds> [semitones] [gate-seconds]');
    Halt(2);
  end;
  Secs := StrToFloatDef(ParamStr(3), 2.0);
  Semis := 0.0; if ParamCount >= 4 then Semis := StrToFloatDef(ParamStr(4), 0.0);
  GateSec := Secs * 0.6; if ParamCount >= 5 then GateSec := StrToFloatDef(ParamStr(5), GateSec);

  Graph := TSedaiPatchGraph.Create;
  Outp := TMemoryStream.Create;
  try
    Res := LoadPatchFromFile(Graph, ParamStr(1));
    if Res.Warnings <> '' then Write(StdErr, Res.Warnings);
    if not Res.Success then
    begin
      if Res.ErrorLine > 0 then
        WriteLn(StdErr, Format('%s:%d: %s', [ParamStr(1), Res.ErrorLine, Res.ErrorText]))
      else
        WriteLn(StdErr, Res.ErrorText);
      Halt(1);
    end;

    if not Graph.Compile(Res.ForceSampleRate) then
    begin
      WriteLn(StdErr, 'patch will not compile: ', Graph.LastError);
      Halt(1);
    end;

    Write(Graph.Describe);
    if Res.ForceSampleRate then
      WriteLn('  (mode = sample: every stage forced to per-sample)');

    Graph.Prepare(SAMPLE_RATE, BLOCK);
    Graph.ResetState;

    Note := nil;
    if Graph.ModuleByName('note') is TSedaiModNote then
      Note := TSedaiModNote(Graph.ModuleByName('note'));

    Total := Round(Secs * SAMPLE_RATE);
    GateSamples := Round(GateSec * SAMPLE_RATE);
    Done := 0;
    Peak := 0.0;

    while Done < Total do
    begin
      Todo := Total - Done;
      if Todo > BLOCK then Todo := BLOCK;

      if Note <> nil then
      begin
        // Volts per octave: a semitone is 1/12 of a volt.
        if Done < GateSamples then Note.SetNote(Semis / 12.0, 1.0)
                              else Note.SetNote(Semis / 12.0, 0.0);
      end;

      Graph.Render(Todo);
      for I := 0 to Todo - 1 do
        for C := 0 to Graph.OutputCount - 1 do
        begin
          V := Graph.OutputSample(C, I);
          if Abs(V) > Peak then Peak := Abs(V);
          if V > 1.0 then V := 1.0 else if V < -1.0 then V := -1.0;
          S := Round(V * 32767.0);
          Outp.WriteBuffer(S, 2);
        end;
      Inc(Done, Todo);
    end;

    SaveWav(ParamStr(2), SAMPLE_RATE, Graph.OutputCount);
    WriteLn(Format('  rendered %.2f s, %d channel(s) -> %s   peak %.3f',
                   [Secs, Graph.OutputCount, ParamStr(2), Peak]));
  finally
    Outp.Free;
    Graph.Free;
  end;
end.
