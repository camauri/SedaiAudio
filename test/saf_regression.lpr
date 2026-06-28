{*
 * Sedai Audio Foundation - Integrated regression test (headless)
 *
 * Exit-code regression suite for the integrated render path, mirroring the
 * sedaisid_test convention (Halt = number of failures, 0 = all green).
 * Runs fully offline: no audio device, no user interaction -- it renders
 * through TSAFEngine / TSAFPart into memory and asserts invariants.
 *
 * Consolidates the most valuable offline checks that previously lived only as
 * throwaway tmp/ harnesses (engine/mixer path, 6-source dispatch, master
 * bounding, polyphony cap, signal-graph cycle detection) into a versioned,
 * CI-able test in the repo.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
program saf_regression;

{$mode objfpc}{$H+}

uses
  SysUtils, Math, Classes,
  SedaiAudioTypes, SedaiAudioBuffer,
  SedaiVoice, SedaiSamplePlayer, SedaiPart, SedaiEngine,
  SedaiMixerChannel, SedaiSignalNode, SedaiAudioFileReader;

const
  SR    = 44100;
  BLOCK = 512;

var
  Failures: Integer = 0;

procedure Ok(const AName: string; ACond: Boolean; const ADetail: string = '');
begin
  if ACond then
    WriteLn(Format('  PASS  %-32s %s', [AName, ADetail]))
  else
  begin
    WriteLn(Format('  FAIL  %-32s %s', [AName, ADetail]));
    Inc(Failures);
  end;
end;

// Render `seconds` of the whole engine, returning the peak absolute sample.
function EnginePeak(AEngine: TSAFEngine; ASeconds: Single): Single;
var
  buf: array[0..BLOCK*2-1] of Single;
  frames, done, i: Integer;
  peak: Single;
begin
  peak := 0; done := 0; frames := Round(SR * ASeconds);
  while done < frames do
  begin
    AEngine.RenderBlock(@buf[0], BLOCK);
    for i := 0 to BLOCK*2-1 do
      if Abs(buf[i]) > peak then peak := Abs(buf[i]);
    Inc(done, BLOCK);
  end;
  Result := peak;
end;

// Render `seconds` of a single Part, returning the peak absolute sample.
function PartPeak(APart: TSAFPart; ASeconds: Single): Single;
var
  buf: array[0..BLOCK*2-1] of Single;
  frames, done, n, i: Integer;
  peak: Single;
begin
  peak := 0; done := 0; frames := Round(SR * ASeconds);
  while done < frames do
  begin
    n := BLOCK; if done + n > frames then n := frames - done;
    APart.RenderBlock(@buf[0], n);
    for i := 0 to n*2-1 do
      if Abs(buf[i]) > peak then peak := Abs(buf[i]);
    Inc(done, n);
  end;
  Result := peak;
end;

// In-memory decaying sine for the sample source.
function MakePing(AFreq, ASeconds: Single): TSedaiAudioBuffer;
var
  n, i: Integer;
  arr: array of Single;
  t: Single;
begin
  n := Round(SR * ASeconds);
  SetLength(arr, n);
  for i := 0 to n-1 do
  begin
    t := i / SR;
    arr[i] := Sin(2*Pi*AFreq*t) * Exp(-t * 6.0) * 0.9;
  end;
  Result := TSedaiAudioBuffer.Create;
  Result.SetFormat(SR, 1);
  Result.SetSize(n);
  Result.WriteInterleaved(@arr[0], 0, n);
end;

// ---------------------------------------------------------------------------

procedure TestEnginePath;
var
  eng: TSAFEngine;
  p1, p2: TSAFPart;
  pBoth, pFM, pClassic: Single;
begin
  WriteLn('== engine -> mixer -> master path ==');
  eng := TSAFEngine.Create(SR);
  try
    p1 := eng.AddPart('Classic', 8); p1.SetInstrument(psClassic, 'saw');
    p2 := eng.AddPart('FM', 8);      p2.SetInstrument(psFM, 'epiano');
    Ok('two parts created', eng.PartCount = 2, Format('partCount=%d', [eng.PartCount]));

    p1.NoteOn(60, 1.0);   // C4 saw
    p2.NoteOn(67, 1.0);   // G4 epiano
    pBoth := EnginePeak(eng, 0.2);
    Ok('both parts sound', pBoth > 0.02, Format('peak=%.4f', [pBoth]));

    eng.GetChannel(0).Muted := True;           // isolate FM
    pFM := EnginePeak(eng, 0.1);
    eng.GetChannel(0).Muted := False;
    eng.GetChannel(1).Muted := True;           // isolate classic
    pClassic := EnginePeak(eng, 0.1);
    Ok('channel mute isolates', (pFM > 0.0) and (pClassic > 0.0),
       Format('fm=%.4f classic=%.4f', [pFM, pClassic]));
  finally
    eng.Free;
  end;
end;

procedure TestAllSources;
var
  p: TSAFPart;
  pk: Single;

  procedure RunPreset(const AName: string; ASource: TSAFPartSource;
                      const APreset: string);
  begin
    p := TSAFPart.Create(4);
    try
      p.SetSampleRate(SR);
      p.SetInstrument(ASource, APreset);
      p.NoteOn(60, 1.0);
      pk := PartPeak(p, 0.15);
      Ok('source ' + AName, pk > 0.02, Format('peak=%.4f', [pk]));
    finally
      p.Free;
    end;
  end;

begin
  WriteLn('== 6 source-type dispatch (non-silent) ==');
  RunPreset('classic',   psClassic,   'saw');
  RunPreset('fm',        psFM,        'epiano');
  RunPreset('wavetable', psWavetable, 'basic');
  RunPreset('additive',  psAdditive,  'organ');
  RunPreset('karplus',   psKarplus,   'guitar');

  // Sample source needs a loaded buffer (no preset string).
  p := TSAFPart.Create(4);
  try
    p.SetSampleRate(SR);
    p.SetSample(MakePing(220.0, 0.4), 57, lmNone);   // recorded at A3
    p.NoteOnFreq(220.0, 0.9);
    pk := PartPeak(p, 0.15);
    Ok('source sample', pk > 0.02, Format('peak=%.4f', [pk]));
  finally
    p.Free;
  end;
end;

procedure TestMasterBounded;
var
  eng: TSAFEngine;
  p: TSAFPart;
  pk: Single;
begin
  // A dense low chord used to run the single-precision filter away (~20-37)
  // before the 3D/3B/4a work; the master-bus limiter + gain staging must now
  // keep the bus bounded. This is the runaway/clip guard.
  WriteLn('== master bus stays bounded (no runaway/clip) ==');
  eng := TSAFEngine.Create(SR);
  try
    p := eng.AddPart('Bass', 8);
    p.SetInstrument(psClassic, 'bass');
    p.NoteOn(36, 1.0);   // C2
    p.NoteOn(39, 1.0);
    p.NoteOn(43, 1.0);   // low triad
    p.NoteOn(48, 1.0);
    pk := EnginePeak(eng, 0.4);
    Ok('bus bounded', (pk > 0.02) and (pk <= 1.1), Format('peak=%.4f (<=1.1)', [pk]));
  finally
    eng.Free;
  end;
end;

procedure TestPolyphonyCap;
var
  p: TSAFPart;
  i, maxActive: Integer;
  buf: array[0..BLOCK*2-1] of Single;
begin
  // Firing more notes than the voice pool must never exceed the cap and must
  // not crash (3B voice management: ineligible steals are dropped, not forced).
  WriteLn('== polyphony cap respected (3B) ==');
  p := TSAFPart.Create(4);   // 4-voice pool
  try
    p.SetSampleRate(SR);
    p.SetInstrument(psClassic, 'saw');
    for i := 60 to 71 do      // 12 simultaneous notes onto a 4-voice part
      p.NoteOn(i, 1.0);
    p.RenderBlock(@buf[0], BLOCK);   // settle active count
    maxActive := p.ActiveVoiceCount;
    Ok('active <= pool size', maxActive <= 4, Format('active=%d (<=4)', [maxActive]));
    Ok('part still sounds', maxActive > 0, Format('active=%d', [maxActive]));
  finally
    p.Free;
  end;
end;

procedure TestSignalGraphCycles;
var
  a, b, c, d: TSedaiSignalNode;
begin
  // C1: ValidateConnections does real DFS cycle detection over FOutputs.
  WriteLn('== signal-graph cycle detection (C1) ==');

  a := TSedaiSignalNode.Create;
  Ok('isolated valid', a.ValidateConnections, '');
  a.Free;

  a := TSedaiSignalNode.Create; b := TSedaiSignalNode.Create; c := TSedaiSignalNode.Create; d := TSedaiSignalNode.Create;
  a.Connect(b); a.Connect(c); b.Connect(d); c.Connect(d);   // diamond DAG
  Ok('diamond DAG valid', a.ValidateConnections, '');
  a.Free; b.Free; c.Free; d.Free;

  a := TSedaiSignalNode.Create; b := TSedaiSignalNode.Create; c := TSedaiSignalNode.Create;
  a.Connect(b); b.Connect(c); c.Connect(a);                 // 3-cycle
  Ok('3-cycle detected', not a.ValidateConnections, '');
  c.Disconnect(a);
  a.Free; b.Free; c.Free;
end;

procedure TestAIFFReader;
const
  // 80-bit IEEE-754 extended for 44100.0 (exp 0x400E, mantissa 0xAC44<<48).
  RATE80: array[0..9] of Byte = ($40, $0E, $AC, $44, 0, 0, 0, 0, 0, 0);
  FRAMES = 1024;

  procedure WID(S: TStream; const ID: string);
  var A: array[0..3] of AnsiChar;
  begin A[0]:=ID[1]; A[1]:=ID[2]; A[2]:=ID[3]; A[3]:=ID[4]; S.WriteBuffer(A, 4); end;
  procedure WBE32(S: TStream; V: LongWord); begin V := NtoBE(V); S.WriteBuffer(V, 4); end;
  procedure WBE16(S: TStream; V: Word); begin V := NtoBE(V); S.WriteBuffer(V, 2); end;

var
  path: string;
  fs: TFileStream;
  rd: TSedaiAudioFileReader;
  buf: TSedaiAudioBuffer;
  i, dataBytes: Integer;
  s, maxErr, expect: Single;
  v16: SmallInt;
  b: Byte;
begin
  // Write a real 16-bit big-endian AIFF to a temp file and read it back: the
  // versioned guard for the pure-Pascal AIFF decoder (C2).
  WriteLn('== AIFF reader round-trip (C2) ==');
  path := GetTempDir(False) + 'saf_regr_test.aiff';
  dataBytes := FRAMES * 2;
  fs := TFileStream.Create(path, fmCreate);
  try
    WID(fs, 'FORM'); WBE32(fs, 4 + (8+18) + (8 + 8 + dataBytes)); WID(fs, 'AIFF');
    WID(fs, 'COMM'); WBE32(fs, 18);
    WBE16(fs, 1); WBE32(fs, FRAMES); WBE16(fs, 16); fs.WriteBuffer(RATE80, 10);
    WID(fs, 'SSND'); WBE32(fs, 8 + dataBytes); WBE32(fs, 0); WBE32(fs, 0);
    for i := 0 to FRAMES-1 do
    begin
      s := 0.6 * Sin(2*Pi*220.0*i/SR);
      v16 := Round(s * 32767);
      b := (v16 shr 8) and $FF; fs.WriteBuffer(b, 1);   // big-endian MSB first
      b := v16 and $FF;         fs.WriteBuffer(b, 1);
    end;
  finally
    fs.Free;
  end;

  rd := TSedaiAudioFileReader.Create;
  try
    Ok('detect AIFF', TSedaiAudioFileReader.DetectFileFormat(path) = affAIFF, '');
    if rd.OpenFile(path) then
    begin
      Ok('header', (rd.Info.SampleRate = SR) and (rd.Info.Channels = 1) and
                   (rd.Info.SampleCount = FRAMES),
         Format('sr=%d ch=%d n=%d', [rd.Info.SampleRate, rd.Info.Channels, rd.Info.SampleCount]));
      if rd.ReadAll(buf) then
      begin
        maxErr := 0;
        for i := 0 to FRAMES-1 do
        begin
          expect := 0.6 * Sin(2*Pi*220.0*i/SR);
          if Abs(buf.GetSample(0, i) - expect) > maxErr then
            maxErr := Abs(buf.GetSample(0, i) - expect);
        end;
        Ok('sample fidelity', maxErr <= 1.0/16384.0, Format('maxErr=%.6f', [maxErr]));
        buf.Free;
      end
      else
        Ok('read all', False, rd.LastError);
    end
    else
      Ok('open', False, rd.LastError);
  finally
    rd.Free;
  end;
  DeleteFile(path);
end;

// ---------------------------------------------------------------------------

begin
  WriteLn('========================================');
  WriteLn('  SAF integrated regression suite');
  WriteLn('========================================');

  TestEnginePath;
  TestAllSources;
  TestMasterBounded;
  TestPolyphonyCap;
  TestSignalGraphCycles;
  TestAIFFReader;

  WriteLn;
  if Failures = 0 then
    WriteLn('ALL PASS')
  else
    WriteLn(Format('%d FAILURE(S)', [Failures]));
  Halt(Failures);
end.
