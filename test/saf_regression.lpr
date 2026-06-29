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
  SedaiMixerChannel, SedaiSignalNode, SedaiAudioFileReader, SedaiAudioFileWriter;

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

procedure TestAIFFWriter;
const
  FRAMES = 1500;   // odd*even mix; exercises the SSND even-boundary pad
var
  src, bA, bW: TSedaiAudioBuffer;
  arr: array of Single;
  aifPath, wavPath: string;
  i, ch, diffs: Integer;

  function WriteFmt(const APath: string; AFmt: TAudioExportFormat): Boolean;
  var
    w: TSedaiAudioFileWriter;
    st: TAudioExportSettings;
  begin
    st := TSedaiAudioFileWriter.GetDefaultSettings(AFmt);
    st.SampleRate := SR; st.Channels := 2;
    st.DitherType := dtNone;   // deterministic: identical PCM on both paths
    w := TSedaiAudioFileWriter.Create;
    try
      Result := w.CreateFile(APath, st) and w.WriteBuffer(src);
      w.Close;
    finally
      w.Free;
    end;
  end;

  function ReadBack(const APath: string; out ABuf: TSedaiAudioBuffer): Boolean;
  var rd: TSedaiAudioFileReader;
  begin
    ABuf := nil;
    rd := TSedaiAudioFileReader.Create;
    try
      Result := rd.OpenFile(APath) and rd.ReadAll(ABuf) and (ABuf <> nil);
    finally
      rd.Free;
    end;
  end;

begin
  // Residual #1: the AIFF *writer* (big-endian PCM) must round-trip through the
  // proven reader bit-exactly against the WAV path (same PCM converters, only
  // byte order differs). 24-bit stereo covers the multi-byte swap + pad.
  WriteLn('== AIFF writer vs WAV oracle (residual #1) ==');
  SetLength(arr, FRAMES * 2);
  for i := 0 to FRAMES - 1 do
  begin
    arr[i*2]   := 0.55 * Sin(2*Pi*180.0*i/SR) + 0.25 * Sin(2*Pi*901.0*i/SR);
    arr[i*2+1] := 0.50 * Sin(2*Pi*227.0*i/SR) - 0.20 * Sin(2*Pi*640.0*i/SR);
  end;
  src := TSedaiAudioBuffer.Create;
  src.SetFormat(SR, 2);
  src.SetSize(FRAMES);
  src.WriteInterleaved(@arr[0], 0, FRAMES);

  aifPath := GetTempDir(False) + 'saf_regr_w.aiff';
  wavPath := GetTempDir(False) + 'saf_regr_w.wav';

  bA := nil; bW := nil;
  try
    Ok('write AIFF 24', WriteFmt(aifPath, aefAIFF24), '');
    Ok('write WAV 24',  WriteFmt(wavPath, aefWAV24), '');
    Ok('detect AIFF', TSedaiAudioFileReader.DetectFileFormat(aifPath) = affAIFF, '');
    if ReadBack(aifPath, bA) and ReadBack(wavPath, bW) then
    begin
      Ok('frame count', (bA.SampleCount = FRAMES) and (bW.SampleCount = FRAMES),
         Format('aiff=%d wav=%d', [bA.SampleCount, bW.SampleCount]));
      diffs := 0;
      for i := 0 to FRAMES - 1 do
        for ch := 0 to 1 do
          if bA.GetSample(ch, i) <> bW.GetSample(ch, i) then Inc(diffs);
      Ok('AIFF == WAV bit-exact', diffs = 0, Format('%d/%d differ', [diffs, FRAMES*2]));
    end
    else
      Ok('read back', False, '');
  finally
    bA.Free; bW.Free; src.Free;
    DeleteFile(aifPath); DeleteFile(wavPath);
  end;
end;

// Search upward from the executable for a committed 'data/fixtures' directory.
function FindFixtures: string;
var
  dir, cand: string;
  i: Integer;
begin
  Result := '';
  dir := ExtractFilePath(ParamStr(0));
  for i := 0 to 6 do
  begin
    cand := IncludeTrailingPathDelimiter(dir) + 'data' + PathDelim + 'fixtures';
    if DirectoryExists(cand) then Exit(IncludeTrailingPathDelimiter(cand));
    dir := ExtractFileDir(ExcludeTrailingPathDelimiter(dir));
    if dir = '' then Break;
  end;
end;

procedure TestFLACReader;
const
  SEEK_AT = 2000;
var
  fxDir, flacPath, wavPath: string;
  rF, rW: TSedaiAudioFileReader;
  bF, bW: TSedaiAudioBuffer;
  i, ch, n, diffs: Integer;
  fbuf: array[0..1] of Single;
  okSeek: Boolean;
begin
  // C2 (compressed): pure-Pascal FLAC decoder must be lossless == its WAV
  // oracle. 16/24-bit divide by the same power-of-two scale, so the float
  // output is bit-identical sample-for-sample. Also exercises Seek.
  WriteLn('== FLAC decoder vs WAV oracle (C2) ==');
  fxDir := FindFixtures;
  if fxDir = '' then
  begin
    Ok('fixtures present', False, 'data/fixtures not found');
    Exit;
  end;

  // Bit-exact decode across mono 16, stereo 16 and stereo 24-bit.
  bF := nil; bW := nil;
  rF := TSedaiAudioFileReader.Create;
  rW := TSedaiAudioFileReader.Create;
  try
    flacPath := fxDir + 'tone_s24_stereo.flac';
    wavPath  := fxDir + 'tone_s24_stereo.wav';
    Ok('detect FLAC', TSedaiAudioFileReader.DetectFileFormat(flacPath) = affFLAC, '');
    if rF.OpenFile(flacPath) and rW.OpenFile(wavPath) then
    begin
      Ok('header', (rF.Info.SampleRate = rW.Info.SampleRate) and
                   (rF.Info.Channels = rW.Info.Channels) and
                   (rF.Info.SampleCount = rW.Info.SampleCount),
         Format('sr=%d ch=%d n=%d', [rF.Info.SampleRate, rF.Info.Channels, rF.Info.SampleCount]));
      if rF.ReadAll(bF) and rW.ReadAll(bW) then
      begin
        n := bW.SampleCount; diffs := 0;
        for i := 0 to n - 1 do
          for ch := 0 to bW.Channels - 1 do
            if bF.GetSample(ch, i) <> bW.GetSample(ch, i) then Inc(diffs);
        Ok('24-bit bit-exact', diffs = 0, Format('%d/%d differ', [diffs, n * bW.Channels]));
      end
      else
        Ok('read all', False, rF.LastError + ' / ' + rW.LastError);
    end
    else
      Ok('open', False, rF.LastError + ' / ' + rW.LastError);
  finally
    bF.Free; bW.Free; rF.Free; rW.Free;
  end;

  // Seek: land on an absolute frame and match the WAV at that frame.
  bW := nil;
  rF := TSedaiAudioFileReader.Create;
  rW := TSedaiAudioFileReader.Create;
  try
    if rF.OpenFile(fxDir + 'tone_s16_stereo.flac') and
       rW.OpenFile(fxDir + 'tone_s16_stereo.wav') and rW.ReadAll(bW) then
    begin
      okSeek := rF.Seek(SEEK_AT);
      Ok('seek ok', okSeek, '');
      if okSeek and (rF.ReadSamples(@fbuf[0], 1) = 1) then
        Ok('seek lands bit-exact',
           (fbuf[0] = bW.GetSample(0, SEEK_AT)) and (fbuf[1] = bW.GetSample(1, SEEK_AT)),
           Format('flac=(%.6f,%.6f) wav=(%.6f,%.6f)',
             [fbuf[0], fbuf[1], bW.GetSample(0, SEEK_AT), bW.GetSample(1, SEEK_AT)]))
      else
        Ok('seek read', False, '');
    end
    else
      Ok('open for seek', False, rF.LastError);
  finally
    bW.Free; rF.Free; rW.Free;
  end;
end;

procedure TestVorbisReader;
const
  SEEK_AT = 3000;
var
  fxDir, oggPath, wavPath: string;
  rO, rW: TSedaiAudioFileReader;
  bO, bW: TSedaiAudioBuffer;
  i, n: Integer;
  dr, dd, rr, corr: Double;
  sbuf: array[0..1] of Single;
begin
  // OGG Vorbis is lossy: compare the pure-Pascal decoder against its WAV oracle
  // with a tolerance (normalized cross-correlation + length within one long
  // block). The granulepos trim makes the length match the source exactly.
  WriteLn('== OGG Vorbis decoder vs WAV oracle (tolerance) ==');
  fxDir := FindFixtures;
  if fxDir = '' then begin Ok('fixtures present', False, 'data/fixtures not found'); Exit; end;

  oggPath := fxDir + 'tone_s16_stereo.ogg';
  wavPath := fxDir + 'tone_s16_stereo.wav';
  Ok('detect OGG', TSedaiAudioFileReader.DetectFileFormat(oggPath) = affOGG, '');

  bO := nil; bW := nil;
  rO := TSedaiAudioFileReader.Create;
  rW := TSedaiAudioFileReader.Create;
  try
    if rO.OpenFile(oggPath) and rW.OpenFile(wavPath) then
    begin
      Ok('header', (rO.Info.SampleRate = rW.Info.SampleRate) and
                   (rO.Info.Channels = rW.Info.Channels),
         Format('sr=%d ch=%d', [rO.Info.SampleRate, rO.Info.Channels]));
      if rO.ReadAll(bO) and rW.ReadAll(bW) then
      begin
        Ok('length within a block', Abs(bO.SampleCount - bW.SampleCount) <= 2048,
           Format('ogg=%d wav=%d', [bO.SampleCount, bW.SampleCount]));
        // Normalized cross-correlation on channel 0 at zero lag.
        n := bO.SampleCount; if bW.SampleCount < n then n := bW.SampleCount;
        dr := 0; dd := 0; rr := 0;
        for i := 0 to n - 1 do
        begin
          dr := dr + bO.GetSample(0, i) * bW.GetSample(0, i);
          dd := dd + Sqr(bO.GetSample(0, i));
          rr := rr + Sqr(bW.GetSample(0, i));
        end;
        if (dd > 0) and (rr > 0) then corr := dr / Sqrt(dd * rr) else corr := 0;
        Ok('decode correlation > 0.9', corr > 0.9, Format('corr=%.4f', [corr]));
      end
      else
        Ok('read all', False, rO.LastError + ' / ' + rW.LastError);
    end
    else
      Ok('open', False, rO.LastError + ' / ' + rW.LastError);
  finally
    bO.Free; bW.Free; rO.Free; rW.Free;
  end;

  // Seek: a linear decode-discard seek must reproduce the full-decode samples
  // exactly (same decode path), so seek-then-read is bit-identical to ReadAll.
  bO := nil;
  rO := TSedaiAudioFileReader.Create;
  rW := TSedaiAudioFileReader.Create;
  try
    if rW.OpenFile(oggPath) and rW.ReadAll(bO) and rO.OpenFile(oggPath) then
    begin
      if rO.Seek(SEEK_AT) and (rO.ReadSamples(@sbuf[0], 1) = 1) then
        Ok('seek lands bit-exact',
           (sbuf[0] = bO.GetSample(0, SEEK_AT)) and (sbuf[1] = bO.GetSample(1, SEEK_AT)),
           Format('seek=(%.6f,%.6f) full=(%.6f,%.6f)',
             [sbuf[0], sbuf[1], bO.GetSample(0, SEEK_AT), bO.GetSample(1, SEEK_AT)]))
      else
        Ok('seek + read', False, rO.LastError);
    end
    else
      Ok('open for seek', False, rO.LastError + ' / ' + rW.LastError);
  finally
    bO.Free; rO.Free; rW.Free;
  end;
end;

procedure TestMP3Reader;
var
  fxDir, mp3Path, wavPath: string;
  rM, rW: TSedaiAudioFileReader;
  bM, bW: TSedaiAudioBuffer;
  i, lag, bestLag, nm, nw: Integer;
  dr, dd, rr, c, bestCorr: Double;
begin
  // MP3 is lossy and carries encoder/decoder delay (no gapless trim), so the
  // decoded stream is longer and shifted vs the source. Validate via the best
  // integer-lag normalized cross-correlation on channel 0.
  WriteLn('== MP3 decoder vs WAV oracle (tolerance) ==');
  fxDir := FindFixtures;
  if fxDir = '' then begin Ok('fixtures present', False, 'data/fixtures not found'); Exit; end;

  mp3Path := fxDir + 'tone_s16_stereo.mp3';
  wavPath := fxDir + 'tone_s16_stereo.wav';
  Ok('detect MP3', TSedaiAudioFileReader.DetectFileFormat(mp3Path) = affMP3, '');

  bM := nil; bW := nil;
  rM := TSedaiAudioFileReader.Create;
  rW := TSedaiAudioFileReader.Create;
  try
    if rM.OpenFile(mp3Path) and rW.OpenFile(wavPath) then
    begin
      Ok('header', (rM.Info.SampleRate = rW.Info.SampleRate) and
                   (rM.Info.Channels = rW.Info.Channels),
         Format('sr=%d ch=%d', [rM.Info.SampleRate, rM.Info.Channels]));
      if rM.ReadAll(bM) and rW.ReadAll(bW) then
      begin
        nm := bM.SampleCount; nw := bW.SampleCount;
        bestCorr := -2; bestLag := 0;
        for lag := -3000 to 3000 do
        begin
          dr := 0; dd := 0; rr := 0;
          i := 0;
          while i < nw do
          begin
            if (i + lag >= 0) and (i + lag < nm) then
            begin
              dr := dr + bM.GetSample(0, i + lag) * bW.GetSample(0, i);
              dd := dd + Sqr(bM.GetSample(0, i + lag));
              rr := rr + Sqr(bW.GetSample(0, i));
            end;
            Inc(i, 3);
          end;
          if (dd > 0) and (rr > 0) then
          begin c := dr / Sqrt(dd * rr); if c > bestCorr then begin bestCorr := c; bestLag := lag; end; end;
        end;
        Ok('decode correlation > 0.98', bestCorr > 0.98, Format('corr=%.4f lag=%d', [bestCorr, bestLag]));
      end
      else
        Ok('read all', False, rM.LastError + ' / ' + rW.LastError);
    end
    else
      Ok('open', False, rM.LastError + ' / ' + rW.LastError);
  finally
    bM.Free; bW.Free; rM.Free; rW.Free;
  end;
end;

procedure TestExactPitch;
const
  NREND = 16384;          // ~2.7 Hz Goertzel bin
  TARGET = 269.0;         // microtonal: nearest note is C4 = 261.63 Hz

  function Goertzel(const arr: array of Single; freq: Single): Double;
  var w, c, s0, s1, s2: Double; i: Integer;
  begin
    w := 2*Pi*freq/SR; c := 2*Cos(w); s1 := 0; s2 := 0;
    for i := 0 to NREND-1 do begin s0 := arr[i] + c*s1 - s2; s2 := s1; s1 := s0; end;
    Result := Sqrt(s1*s1 + s2*s2 - c*s1*s2);
  end;

  // Render a Part's left channel after a microtonal note; assert the energy at
  // the exact frequency dominates the energy at the nearest-note frequency.
  procedure Check(const AName: string; APart: TSAFPart);
  var
    arr: array of Single;
    buf: array[0..BLOCK*2-1] of Single;
    done, n, i, note: Integer;
    fNote, mExact, mNote: Double;
  begin
    SetLength(arr, NREND);
    APart.NoteOnFreq(TARGET, 1.0);
    done := 0;
    while done < NREND do
    begin
      n := BLOCK; if done + n > NREND then n := NREND - done;
      APart.RenderBlock(@buf[0], n);
      for i := 0 to n-1 do arr[done+i] := buf[i*2];
      Inc(done, n);
    end;
    note := Round(69.0 + 12.0 * Log2(TARGET / 440.0));
    fNote := 440.0 * Power(2.0, (note - 69) / 12.0);
    mExact := Goertzel(arr, TARGET);
    mNote  := Goertzel(arr, fNote);
    Ok(AName, mExact > mNote * 1.3,
       Format('exact=%.3g note=%.3g', [mExact, mNote]));
  end;

var
  p: TSAFPart;
begin
  // Residual #2: exact-Hz pitch. Karplus is the source that was note-quantized
  // (pitch baked into the delay line at pluck time); the others already tracked
  // the voice frequency per sample and serve as regression guards.
  WriteLn('== exact-Hz pitch (residual #2) ==');
  p := TSAFPart.Create(4);
  try
    p.SetSampleRate(SR);
    p.SetInstrument(psKarplus, 'guitar');
    Check('karplus on pitch', p);
  finally p.Free; end;

  p := TSAFPart.Create(4);
  try
    p.SetSampleRate(SR);
    p.SetSample(MakePing(261.63, 1.0), 60, lmForward);  // C4 sine, looped
    Check('sample on pitch', p);
  finally p.Free; end;
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
  TestAIFFWriter;
  TestFLACReader;
  TestVorbisReader;
  TestMP3Reader;
  TestExactPitch;

  WriteLn;
  if Failures = 0 then
    WriteLn('ALL PASS')
  else
    WriteLn(Format('%d FAILURE(S)', [Failures]));
  Halt(Failures);
end.
