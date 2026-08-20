program patch_fx;
// ============================================================================
// Process an audio file THROUGH a patch.
//
//   patch_fx <in.wav> <file.patch> <out.wav> [tail-seconds] [gain]
//
// The patch must contain a module of type `input`; its output is the incoming
// audio. Everything else is the same graph, the same scheduler and the same
// modules used to synthesise from scratch — which is the design's claim that
// "FX are the same machine", made true rather than asserted.
//
// Reads whatever SedaiAudioFileReader handles (WAV, AIFF, FLAC, OGG, MP3);
// multi-channel input is summed to mono, because a patch port carries one
// signal. Writes 16-bit mono WAV.
// ============================================================================
{$mode objfpc}{$H+}
uses SysUtils, Classes, SedaiAudioTypes, SedaiAudioFileReader,
     SedaiPatchGraph, SedaiPatchModules, SedaiPatchFile;

const
  BLOCK = 256;

var
  Graph: TSedaiPatchGraph;
  Res: TSedaiPatchLoadResult;
  Reader: TSedaiAudioFileReader;
  Outp: TMemoryStream;
  Raw: array of Single;
  Chan: array of array of Single;
  Inputs: array of TSedaiModInput;
  SR, Ch, Got, I, K, TailBlocks, B: Integer;
  Tail, Gain, V, Peak: Single;
  S: SmallInt;
  M: TSedaiPatchModule;

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
    WriteLn('usage: patch_fx <in.wav> <file.patch> <out.wav> [tail-seconds] [gain]');
    Halt(2);
  end;
  Tail := 1.0;  if ParamCount >= 4 then Tail := StrToFloatDef(ParamStr(4), 1.0);
  Gain := 1.0;  if ParamCount >= 5 then Gain := StrToFloatDef(ParamStr(5), 1.0);

  Reader := TSedaiAudioFileReader.Create;
  Graph := TSedaiPatchGraph.Create;
  Outp := TMemoryStream.Create;
  try
    if not Reader.OpenFile(ParamStr(1)) then
    begin
      WriteLn(StdErr, 'cannot open audio file: ', ParamStr(1));
      Halt(1);
    end;
    SR := Reader.Info.SampleRate;
    Ch := Reader.Info.Channels;
    if Ch < 1 then Ch := 1;

    Res := LoadPatchFromFile(Graph, ParamStr(2));
    if Res.Warnings <> '' then Write(StdErr, Res.Warnings);
    if not Res.Success then
    begin
      if Res.ErrorLine > 0 then
        WriteLn(StdErr, Format('%s:%d: %s', [ParamStr(2), Res.ErrorLine, Res.ErrorText]))
      else
        WriteLn(StdErr, Res.ErrorText);
      Halt(1);
    end;
    if not Graph.Compile(Res.ForceSampleRate) then
    begin
      WriteLn(StdErr, 'patch will not compile: ', Graph.LastError);
      Halt(1);
    end;

    // Find the input module by TYPE, not by name, so the patch may call it
    // whatever reads best.
    // Collect EVERY input module: a patch declares how many inputs it wants and
    // which channel each takes, exactly as `output` lines declare the outputs.
    SetLength(Inputs, 0);
    for I := 0 to Graph.ModuleCount - 1 do
    begin
      M := Graph.ModuleAt(I);
      if M is TSedaiModInput then
      begin
        SetLength(Inputs, Length(Inputs) + 1);
        Inputs[High(Inputs)] := TSedaiModInput(M);
      end;
    end;
    if Length(Inputs) = 0 then
    begin
      WriteLn(StdErr, 'the patch has no module of type "input"; an effect patch');
      WriteLn(StdErr, 'needs at least one, and its output is the incoming audio');
      Halt(1);
    end;

    Graph.Prepare(SR, BLOCK);
    Graph.ResetState;

    SetLength(Raw, BLOCK * Ch);
    SetLength(Chan, Ch, BLOCK);
    Peak := 0.0;

    WriteLn(Format('  %s  %.1f s, %d Hz, %d ch  ->  %s',
                   [ExtractFileName(ParamStr(1)), Reader.Info.Duration, SR, Ch,
                    ExtractFileName(ParamStr(2))]));
    Write(Graph.Describe);

    repeat
      Got := Reader.ReadSamples(@Raw[0], BLOCK);
      if Got <= 0 then Break;
      // De-interleave. Each input module gets the channel it asked for; a patch
      // with one input on a stereo file takes the left, which is a choice the
      // patch made rather than a silent sum.
      for K := 0 to Ch - 1 do
        for I := 0 to Got - 1 do Chan[K][I] := Raw[I * Ch + K];
      for K := 0 to High(Inputs) do
        if Inputs[K].Channel < Ch then Inputs[K].SetBlock(@Chan[Inputs[K].Channel][0], Got)
                                  else Inputs[K].SetBlock(nil, Got);
      Graph.Render(Got);
      for I := 0 to Got - 1 do
        for K := 0 to Graph.OutputCount - 1 do
        begin
          V := Graph.OutputSample(K, I) * Gain;
          if Abs(V) > Peak then Peak := Abs(V);
          if V > 1.0 then V := 1.0 else if V < -1.0 then V := -1.0;
          S := Round(V * 32767.0);
          Outp.WriteBuffer(S, 2);
        end;
    until Got < BLOCK;

    // Keep rendering silence so reverb and delay tails ring out instead of
    // being chopped off at the last input sample.
    TailBlocks := Round(Tail * SR) div BLOCK;
    for B := 1 to TailBlocks do
    begin
      for K := 0 to High(Inputs) do Inputs[K].SetBlock(nil, BLOCK);
      Graph.Render(BLOCK);
      for I := 0 to BLOCK - 1 do
        for K := 0 to Graph.OutputCount - 1 do
        begin
          V := Graph.OutputSample(K, I) * Gain;
          if Abs(V) > Peak then Peak := Abs(V);
          if V > 1.0 then V := 1.0 else if V < -1.0 then V := -1.0;
          S := Round(V * 32767.0);
          Outp.WriteBuffer(S, 2);
        end;
    end;

    SaveWav(ParamStr(3), SR, Graph.OutputCount);
    WriteLn(Format('  %.1f s written, %d in / %d out, peak %.3f -> %s',
                   [Outp.Size / 2 / Graph.OutputCount / SR, Length(Inputs),
                    Graph.OutputCount, Peak, ParamStr(3)]));
  finally
    Outp.Free;
    Graph.Free;
    Reader.Free;
  end;
end.
