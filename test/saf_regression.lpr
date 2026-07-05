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
  SedaiVoice, SedaiSamplePlayer, SedaiPart, SedaiEngine, SedaiInstrumentPreset,
  SedaiFMOperator, SedaiAdditiveGenerator, SedaiPartialGenerator, SedaiReedGenerator,
  SedaiMixerChannel, SedaiSignalNode, SedaiAudioFileReader, SedaiAudioFileWriter,
  SedaiFLACEncoder, SedaiFLACDecoder, SedaiAutoSpace, SedaiBodyResonator,
  SedaiSpatialChain, SedaiConvolver;

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
  WriteLn('== 7 source-type dispatch (non-silent) ==');
  RunPreset('classic',   psClassic,   'saw');
  RunPreset('fm',        psFM,        'epiano');
  RunPreset('wavetable', psWavetable, 'basic');
  RunPreset('additive',  psAdditive,  'organ');
  RunPreset('karplus',   psKarplus,   'guitar');
  RunPreset('sid',       psSID,       'lead');

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

procedure TestFLACWriter;
var
  fxDir, wavPath: string;
  rW: TSedaiAudioFileReader;
  bW: TSedaiAudioBuffer;
  src, back: array of Single;
  enc: TSedaiFLACEncoder;
  dec: TSedaiFLACDecoder;
  ms: TMemoryStream;
  ch, i, got, cap, n, diffs: Integer;
  total: Int64;
  buf: array[0..8191] of Single;
  // 24-bit writer-path round-trip
  wr: TSedaiAudioFileWriter;
  settings: TAudioExportSettings;
  rdr: TSedaiAudioFileReader;
  b24, bb24: TSedaiAudioBuffer;
  s24: array of Single;
  n24, ch24: Integer;
begin
  // Encode the 16-bit stereo fixture to FLAC, decode it back, require bit-exact
  // (lossless). Exercises the pure-Pascal FLAC encoder + decoder together.
  WriteLn('== FLAC encoder round-trip (lossless) ==');
  fxDir := FindFixtures;
  if fxDir = '' then begin Ok('fixtures present', False, 'data/fixtures not found'); Exit; end;
  wavPath := fxDir + 'tone_s16_stereo.wav';

  bW := nil; rW := TSedaiAudioFileReader.Create;
  try
    if not (rW.OpenFile(wavPath) and rW.ReadAll(bW)) then
    begin Ok('open wav', False, rW.LastError); Exit; end;
    ch := bW.Channels; n := bW.SampleCount;
    SetLength(src, n * ch);
    bW.ReadInterleaved(@src[0], 0, n);
  finally rW.Free; end;

  ms := TMemoryStream.Create;
  enc := TSedaiFLACEncoder.Create;
  try
    Ok('encoder init', enc.Init(ms, 44100, ch, 16), enc.LastError);
    enc.WriteFrames(@src[0], n);
    enc.Finalize;
    Ok('encoded non-empty', ms.Size > 0, Format('%d bytes', [ms.Size]));

    ms.Position := 0;
    dec := TSedaiFLACDecoder.Create;
    cap := 1 shl 20; SetLength(back, cap); total := 0;
    try
      if dec.OpenStream(ms) then
      begin
        repeat
          got := dec.ReadFrames(@buf[0], 4096 div ch);
          if got > 0 then
          begin
            if (total + got) * ch > cap then begin cap := cap * 2; SetLength(back, cap); end;
            Move(buf[0], back[total * ch], got * ch * SizeOf(Single));
            total := total + got;
          end;
        until got = 0;
      end;
    finally dec.Free; end;

    diffs := 0;
    if total = n then
      for i := 0 to n * ch - 1 do
        if src[i] <> back[i] then Inc(diffs);
    Ok('round-trip bit-exact', (total = n) and (diffs = 0),
       Format('frames %d/%d, %d differ', [Integer(total), n, diffs]));
  finally
    enc.Free; ms.Free; bW.Free;
  end;

  // 24-bit, through the full TSedaiAudioFileWriter (aefFLAC24) + reader path.
  b24 := nil; bb24 := nil;
  rdr := TSedaiAudioFileReader.Create;
  try
    if rdr.OpenFile(fxDir + 'tone_s24_stereo.wav') and rdr.ReadAll(b24) then
    begin
      ch24 := b24.Channels; n24 := b24.SampleCount;
      SetLength(s24, n24 * ch24);
      b24.ReadInterleaved(@s24[0], 0, n24);
      ms := TMemoryStream.Create;
      wr := TSedaiAudioFileWriter.Create;
      try
        FillChar(settings, SizeOf(settings), 0);
        settings.Format := aefFLAC24;
        settings.SampleRate := 44100;
        settings.Channels := ch24;
        if wr.CreateStream(ms, False, settings) then
        begin
          wr.WriteSamples(@s24[0], n24);
          wr.Close;
          ms.Position := 0;
          if rdr.OpenStream(ms, False) and rdr.ReadAll(bb24) then
          begin
            diffs := 0;
            if bb24.SampleCount = n24 then
              for i := 0 to n24 - 1 do
                if (b24.GetSample(0, i) <> bb24.GetSample(0, i)) or
                   (b24.GetSample(1, i) <> bb24.GetSample(1, i)) then Inc(diffs);
            Ok('24-bit writer round-trip bit-exact',
               (bb24.SampleCount = n24) and (diffs = 0),
               Format('frames %d/%d, %d differ', [bb24.SampleCount, n24, diffs]));
          end
          else Ok('24-bit read back', False, rdr.LastError);
        end
        else Ok('24-bit writer create', False, wr.LastError);
      finally
        wr.Free; ms.Free;
      end;
    end;
  finally
    rdr.Free; b24.Free; bb24.Free;
  end;
end;

procedure TestVorbisReader;
const
  SEEK_AT = 3000;
  FAR_SEEK = 5000;   // > 2*blocksize1, triggers the granulepos bisection path
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
      // Near-start seek uses the linear restart path.
      if rO.Seek(SEEK_AT) and (rO.ReadSamples(@sbuf[0], 1) = 1) then
        Ok('seek lands bit-exact',
           (sbuf[0] = bO.GetSample(0, SEEK_AT)) and (sbuf[1] = bO.GetSample(1, SEEK_AT)),
           Format('seek=(%.6f,%.6f) full=(%.6f,%.6f)',
             [sbuf[0], sbuf[1], bO.GetSample(0, SEEK_AT), bO.GetSample(1, SEEK_AT)]))
      else
        Ok('seek + read', False, rO.LastError);
      // Far seek exercises the granulepos bisection fast-path; must also be exact.
      if (bO.SampleCount > FAR_SEEK) and rO.Seek(FAR_SEEK) and (rO.ReadSamples(@sbuf[0], 1) = 1) then
        Ok('bisection seek bit-exact',
           (sbuf[0] = bO.GetSample(0, FAR_SEEK)) and (sbuf[1] = bO.GetSample(1, FAR_SEEK)),
           Format('seek=(%.6f,%.6f) full=(%.6f,%.6f)',
             [sbuf[0], sbuf[1], bO.GetSample(0, FAR_SEEK), bO.GetSample(1, FAR_SEEK)]));
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
  // MP3 is lossy; the Xing/LAME gapless trim aligns the decoded stream to the
  // source (length match, zero lag). Validate the trimmed length and the best
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
        Ok('gapless length match', Abs(nm - nw) <= 2, Format('mp3=%d wav=%d', [nm, nw]));
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

procedure TestInstrumentRegistry;
var
  reg: TSedaiInstrumentRegistry;
  basses: TIntArray;
  part: TSAFPart;
  buf: array of Single;
  i, frames: Integer;
  peak: Single;
  ms: TMemoryStream;
  reg2, reg3: TSedaiInstrumentRegistry;
  added, idx: Integer;
  authored: TInstrumentPreset;
begin
  // Phase A: the instrument catalog. Browse by category/tag (technique hidden)
  // and load an instrument into a Part, which must then render audible audio.
  WriteLn('== Instrument preset registry (phase A) ==');
  reg := InstrumentRegistry;
  Ok('catalog non-empty', reg.Count >= 20, Format('%d instruments', [reg.Count]));

  basses := reg.ListByCategory(icBass);
  // Bass spans techniques: Classic + FM + Plucked.
  Ok('Bass spans techniques', Length(basses) >= 3, Format('%d basses', [Length(basses)]));

  Ok('find by name', reg.FindByName('FM E-Piano') >= 0, '');
  Ok('miss by name', reg.FindByName('does-not-exist') = -1, '');
  Ok('browse by character', Length(reg.ListByTag('bright')) > 0,
     Format('%d "bright"', [Length(reg.ListByTag('bright'))]));

  // Load an instrument into a Part and confirm it produces sound.
  frames := 2048;
  SetLength(buf, frames * 2);
  part := TSAFPart.Create;
  try
    part.SetSampleRate(SR);
    Ok('apply instrument', reg.ApplyToPartByName('Classic Lead', part), '');
    // The preset sizes the part to its suggested polyphony (icLead default = 6).
    Ok('preset sizes the part', part.Polyphony = 6, Format('poly=%d', [part.Polyphony]));
    part.NoteOn(60, 1.0);
    FillChar(buf[0], frames * 2 * SizeOf(Single), 0);
    part.RenderBlock(@buf[0], frames);
    peak := 0;
    for i := 0 to frames * 2 - 1 do
      if Abs(buf[i]) > peak then peak := Abs(buf[i]);
    Ok('loaded instrument is audible', peak > 0.001, Format('peak=%.4f', [peak]));
  finally
    part.Free;
  end;

  // .safinst round-trip: save the built-in catalog, reload into an empty
  // registry, and check the count + a spot preset survive.
  ms := TMemoryStream.Create;
  reg2 := TSedaiInstrumentRegistry.CreateEmpty;
  try
    reg.SaveToStream(ms, 'Builtins');
    ms.Position := 0;
    added := reg2.LoadFromStream(ms);
    Ok('.safinst round-trip count', added = reg.Count, Format('%d/%d', [added, reg.Count]));
    idx := reg2.FindByName('FM E-Piano');
    Ok('.safinst preset preserved',
       (idx >= 0) and (reg2.Get(idx).Technique = psFM) and
       (reg2.Get(idx).PresetKey = 'epiano') and (reg2.Get(idx).Category = icKeys) and
       (reg2.Get(idx).Polyphony = 16), '');   // icKeys default polyphony round-trips
  finally
    reg2.Free; ms.Free;
  end;

  // Authored common-layer override (envelope) round-trips through .safinst.
  authored := Default(TInstrumentPreset);
  authored.Name := 'Slow Pad'; authored.Category := icPad;
  authored.Technique := psClassic; authored.PresetKey := 'pad';
  authored.Common.OverrideEnvelope := True;
  authored.Common.Attack := 0.5; authored.Common.Decay := 0.2;
  authored.Common.Sustain := 0.8; authored.Common.Release := 1.5;
  ms := TMemoryStream.Create;
  reg2 := TSedaiInstrumentRegistry.CreateEmpty;
  try
    reg2.AddPreset(authored);
    reg2.SaveToStream(ms, 'Custom');
    ms.Position := 0;
    reg3 := TSedaiInstrumentRegistry.CreateEmpty;
    try
      reg3.LoadFromStream(ms);
      idx := reg3.FindByName('Slow Pad');
      Ok('override env round-trips',
         (idx >= 0) and reg3.Get(idx).Common.OverrideEnvelope and
         (Abs(reg3.Get(idx).Common.Attack - 0.5) < 1e-6) and
         (Abs(reg3.Get(idx).Common.Release - 1.5) < 1e-6), '');
    finally
      reg3.Free;
    end;
  finally
    reg2.Free; ms.Free;
  end;
end;

procedure TestFMParams;
var
  params, params2: TFMParams;
  partA, partB: TSAFPart;
  bufA, bufB: array of Single;
  i, frames, op: Integer;
  maxDiff, diff, peakC: Single;
  reg2, reg3: TSedaiInstrumentRegistry;
  authored: TInstrumentPreset;
  ms: TMemoryStream;
  idx: Integer;

  procedure RenderPart(APart: TSAFPart; var ABuf: array of Single; AFrames: Integer);
  begin
    APart.SetSampleRate(SR);
    APart.NoteOn(60, 1.0);
    FillChar(ABuf[0], AFrames * 2 * SizeOf(Single), 0);
    APart.RenderBlock(@ABuf[0], AFrames);
  end;

begin
  // Author side: a full FM parameter block must reproduce the named built-in
  // bit-for-bit (ExplodeFMParams + ConfigureFMVoiceFromParams == ConfigureFMVoice),
  // round-trip through .safinst, and actually change the sound when edited.
  WriteLn('== FM parameter block (author side) ==');

  frames := 4096;
  SetLength(bufA, frames * 2);
  SetLength(bufB, frames * 2);

  // 1. Explode a built-in and confirm the params-driven path is bit-identical.
  params := ExplodeFMParams('epiano');
  partA := TSAFPart.Create;
  partB := TSAFPart.Create;
  try
    partA.SetInstrument(psFM, 'epiano');          // named-preset path
    partB.SetSampleRate(SR);
    partB.SetInstrument(psFM, 'epiano');
    partB.SetFMParams(params);                    // full-block path
    RenderPart(partA, bufA, frames);
    RenderPart(partB, bufB, frames);
    maxDiff := 0;
    peakC := 0;
    for i := 0 to frames * 2 - 1 do
    begin
      diff := Abs(bufA[i] - bufB[i]);
      if diff > maxDiff then maxDiff := diff;
      if Abs(bufA[i]) > peakC then peakC := Abs(bufA[i]);
    end;
    Ok('FM block reproduces named preset', (maxDiff = 0) and (peakC > 0.001),
       Format('maxDiff=%.3g peak=%.4f', [maxDiff, peakC]));
  finally
    partA.Free; partB.Free;
  end;

  // 2. Editing a parameter changes the rendered output.
  params2 := params;
  for op := 0 to MAX_FM_OPERATORS - 1 do
    params2.Ops[op].Ratio := params2.Ops[op].Ratio * 2.0;   // octave-ish shift
  partA := TSAFPart.Create;
  partB := TSAFPart.Create;
  try
    partA.SetSampleRate(SR); partA.SetInstrument(psFM, 'epiano'); partA.SetFMParams(params);
    partB.SetSampleRate(SR); partB.SetInstrument(psFM, 'epiano'); partB.SetFMParams(params2);
    RenderPart(partA, bufA, frames);
    RenderPart(partB, bufB, frames);
    maxDiff := 0;
    for i := 0 to frames * 2 - 1 do
    begin
      diff := Abs(bufA[i] - bufB[i]);
      if diff > maxDiff then maxDiff := diff;
    end;
    Ok('edited FM param changes sound', maxDiff > 1e-4, Format('maxDiff=%.4f', [maxDiff]));
  finally
    partA.Free; partB.Free;
  end;

  // 3. .safinst round-trip of a full FM block.
  authored := Default(TInstrumentPreset);
  authored.Name := 'Author EP'; authored.Category := icKeys;
  authored.Technique := psFM; authored.PresetKey := 'epiano';
  authored.HasFMParams := True;
  authored.FM := params;
  ms := TMemoryStream.Create;
  reg2 := TSedaiInstrumentRegistry.CreateEmpty;
  try
    reg2.AddPreset(authored);
    reg2.SaveToStream(ms, 'FM Custom');
    ms.Position := 0;
    reg3 := TSedaiInstrumentRegistry.CreateEmpty;
    try
      reg3.LoadFromStream(ms);
      idx := reg3.FindByName('Author EP');
      maxDiff := 0;
      if idx >= 0 then
      begin
        params2 := reg3.Get(idx).FM;
        if Abs(params2.FeedbackLevel - params.FeedbackLevel) > maxDiff then
          maxDiff := Abs(params2.FeedbackLevel - params.FeedbackLevel);
        if Abs(params2.OutputTrim - params.OutputTrim) > maxDiff then
          maxDiff := Abs(params2.OutputTrim - params.OutputTrim);
        for op := 0 to MAX_FM_OPERATORS - 1 do
        begin
          if Abs(params2.Ops[op].Ratio - params.Ops[op].Ratio) > maxDiff then
            maxDiff := Abs(params2.Ops[op].Ratio - params.Ops[op].Ratio);
          if Abs(params2.Ops[op].Level - params.Ops[op].Level) > maxDiff then
            maxDiff := Abs(params2.Ops[op].Level - params.Ops[op].Level);
          if Abs(params2.Ops[op].SustainLevel - params.Ops[op].SustainLevel) > maxDiff then
            maxDiff := Abs(params2.Ops[op].SustainLevel - params.Ops[op].SustainLevel);
        end;
      end;
      Ok('.safinst FM block round-trips',
         (idx >= 0) and reg3.Get(idx).HasFMParams and
         (reg3.Get(idx).FM.Algorithm = params.Algorithm) and (maxDiff < 1e-4),
         Format('maxDiff=%.3g', [maxDiff]));
    finally
      reg3.Free;
    end;
  finally
    reg2.Free; ms.Free;
  end;
end;

type
  // Applies a technique's exploded full-parameter block to a part.
  TConfigureBlockProc = procedure(APart: TSAFPart; const AKey: string);

procedure ApplyClassicBlock(APart: TSAFPart; const AKey: string);
begin APart.SetClassicParams(ExplodeClassicParams(AKey)); end;
procedure ApplyWavetableBlock(APart: TSAFPart; const AKey: string);
begin APart.SetWavetableParams(ExplodeWavetableParams(AKey)); end;
procedure ApplyAdditiveBlock(APart: TSAFPart; const AKey: string);
begin APart.SetAdditiveParams(ExplodeAdditiveParams(AKey)); end;
procedure ApplyKarplusBlock(APart: TSAFPart; const AKey: string);
begin APart.SetKarplusParams(ExplodeKarplusParams(AKey)); end;

procedure TestTechniqueParams;
var
  frames, i: Integer;
  bufA, bufB: array of Single;
  maxDiff, diff: Single;
  reg2, reg3: TSedaiInstrumentRegistry;
  authored: TInstrumentPreset;
  ms: TMemoryStream;
  idx: Integer;

  // Render a part's middle-C note. ASeed forces the RNG so techniques that excite
  // with noise (Karplus) stay deterministic across two renders.
  procedure Render(APart: TSAFPart; var ABuf: array of Single; AFrames: Integer; ASeed: LongInt);
  begin
    APart.SetSampleRate(SR);
    RandSeed := ASeed;
    APart.NoteOn(60, 1.0);
    FillChar(ABuf[0], AFrames * 2 * SizeOf(Single), 0);
    APart.RenderBlock(@ABuf[0], AFrames);
  end;

  // explode a named preset, apply the block to one part and the named key to
  // another, render both and confirm the block path reproduces it bit-for-bit.
  procedure CheckBitExact(ASource: TSAFPartSource; const AKey: string;
    AConfigureBlock: TConfigureBlockProc; ASeed: LongInt);
  var
    pa, pb: TSAFPart;
    j: Integer;
    md, pk: Single;
  begin
    pa := TSAFPart.Create;
    pb := TSAFPart.Create;
    try
      pa.SetSampleRate(SR); pa.SetInstrument(ASource, AKey);
      pb.SetSampleRate(SR); pb.SetInstrument(ASource, AKey);
      AConfigureBlock(pb, AKey);    // apply the exploded full-parameter block
      Render(pa, bufA, frames, ASeed);
      Render(pb, bufB, frames, ASeed);
      md := 0; pk := 0;
      for j := 0 to frames * 2 - 1 do
      begin
        diff := Abs(bufA[j] - bufB[j]);
        if diff > md then md := diff;
        if Abs(bufA[j]) > pk then pk := Abs(bufA[j]);
      end;
      Ok(Format('%s block reproduces preset', [AKey]), (md = 0) and (pk > 0.001),
         Format('maxDiff=%.3g peak=%.4f', [md, pk]));
    finally
      pa.Free; pb.Free;
    end;
  end;

begin
  WriteLn('== Per-technique parameter blocks (author side) ==');
  frames := 4096;
  SetLength(bufA, frames * 2);
  SetLength(bufB, frames * 2);

  // Each technique: the exploded full block reproduces the named preset exactly.
  CheckBitExact(psClassic,   'lead',     @ApplyClassicBlock,   111);
  CheckBitExact(psWavetable, 'supersaw', @ApplyWavetableBlock, 222);
  CheckBitExact(psAdditive,  'organ',    @ApplyAdditiveBlock,  333);
  CheckBitExact(psKarplus,   'guitar',   @ApplyKarplusBlock,   444);

  // .safinst round-trip of each technique's block (spot-check key fields).
  authored := Default(TInstrumentPreset);
  authored.Name := 'RT Classic'; authored.Category := icLead;
  authored.Technique := psClassic; authored.PresetKey := 'lead';
  authored.HasClassicParams := True; authored.Classic := ExplodeClassicParams('lead');
  ms := TMemoryStream.Create;
  reg2 := TSedaiInstrumentRegistry.CreateEmpty;
  try
    reg2.AddPreset(authored);
    authored := Default(TInstrumentPreset);
    authored.Name := 'RT Wave'; authored.Category := icLead;
    authored.Technique := psWavetable; authored.PresetKey := 'supersaw';
    authored.HasWavetableParams := True; authored.Wavetable := ExplodeWavetableParams('supersaw');
    reg2.AddPreset(authored);
    authored := Default(TInstrumentPreset);
    authored.Name := 'RT Add'; authored.Category := icOrgan;
    authored.Technique := psAdditive; authored.PresetKey := 'organ';
    authored.HasAdditiveParams := True; authored.Additive := ExplodeAdditiveParams('organ');
    reg2.AddPreset(authored);
    authored := Default(TInstrumentPreset);
    authored.Name := 'RT KS'; authored.Category := icPlucked;
    authored.Technique := psKarplus; authored.PresetKey := 'guitar';
    authored.HasKarplusParams := True; authored.Karplus := ExplodeKarplusParams('guitar');
    reg2.AddPreset(authored);

    reg2.SaveToStream(ms, 'Technique Blocks');
    ms.Position := 0;
    reg3 := TSedaiInstrumentRegistry.CreateEmpty;
    try
      reg3.LoadFromStream(ms);

      idx := reg3.FindByName('RT Classic');
      Ok('.safinst classic block round-trips',
         (idx >= 0) and reg3.Get(idx).HasClassicParams and
         (reg3.Get(idx).Classic.OscMode = ExplodeClassicParams('lead').OscMode) and
         (Abs(reg3.Get(idx).Classic.FilterCutoff - ExplodeClassicParams('lead').FilterCutoff) < 1e-3), '');

      idx := reg3.FindByName('RT Wave');
      Ok('.safinst wavetable block round-trips',
         (idx >= 0) and reg3.Get(idx).HasWavetableParams and
         (reg3.Get(idx).Wavetable.Kind = wtkSuperSaw) and
         (reg3.Get(idx).Wavetable.UnisonVoices = ExplodeWavetableParams('supersaw').UnisonVoices), '');

      idx := reg3.FindByName('RT Add');
      maxDiff := 0;
      if idx >= 0 then
        for i := 0 to High(reg3.Get(idx).Additive.Levels) do
        begin
          diff := Abs(reg3.Get(idx).Additive.Levels[i] - ExplodeAdditiveParams('organ').Levels[i]);
          if diff > maxDiff then maxDiff := diff;
        end;
      Ok('.safinst additive block round-trips',
         (idx >= 0) and reg3.Get(idx).HasAdditiveParams and
         (reg3.Get(idx).Additive.HarmonicCount = ExplodeAdditiveParams('organ').HarmonicCount) and
         (maxDiff < 1e-4), Format('maxDiff=%.3g', [maxDiff]));

      idx := reg3.FindByName('RT KS');
      Ok('.safinst karplus block round-trips',
         (idx >= 0) and reg3.Get(idx).HasKarplusParams and
         (Abs(reg3.Get(idx).Karplus.Damping - ExplodeKarplusParams('guitar').Damping) < 1e-5) and
         (Abs(reg3.Get(idx).Karplus.Blend - ExplodeKarplusParams('guitar').Blend) < 1e-5), '');
    finally
      reg3.Free;
    end;
  finally
    reg2.Free; ms.Free;
  end;
end;

procedure TestMacros;
var
  reg, reg2: TSedaiInstrumentRegistry;
  pr: TInstrumentPreset;
  pLo, pHi, pDet: TSAFPart;
  bufLo, bufHi, bufB: array of Single;
  frames, i, idx: Integer;
  maxDiff, diff, peakLo: Single;
  ms: TMemoryStream;

  procedure LoadAndRender(out APart: TSAFPart; ALib: TSedaiInstrumentRegistry;
    AMacroVal: Single; var ABuf: array of Single);
  begin
    APart := TSAFPart.Create;
    APart.SetSampleRate(SR);
    ALib.ApplyToPartByName('Macro Pad', APart);
    APart.SetMacroValue(0, AMacroVal);
    APart.NoteOn(60, 1.0);
    FillChar(ABuf[0], frames * 2 * SizeOf(Single), 0);
    APart.RenderBlock(@ABuf[0], frames);
  end;

begin
  WriteLn('== Macros (composer quick-controls) ==');
  frames := 4096;
  SetLength(bufLo, frames * 2); SetLength(bufHi, frames * 2); SetLength(bufB, frames * 2);

  // Author a preset carrying a "Brightness" macro wired to the filter cutoff.
  pr := Default(TInstrumentPreset);
  pr.Name := 'Macro Pad'; pr.Category := icPad;
  pr.Technique := psClassic; pr.PresetKey := 'saw';
  SetLength(pr.Macros, 1);
  pr.Macros[0].Name := 'Brightness';
  pr.Macros[0].Value := 0.0;
  SetLength(pr.Macros[0].Mappings, 1);
  pr.Macros[0].Mappings[0].Dest := mdFilterCutoff;
  pr.Macros[0].Mappings[0].MinVal := 300.0;
  pr.Macros[0].Mappings[0].MaxVal := 8000.0;
  pr.Macros[0].Mappings[0].Curve := mcLinear;

  reg := TSedaiInstrumentRegistry.CreateEmpty;
  try
    reg.AddPreset(pr);

    // Dark (0.0) vs bright (1.0): the cutoff macro must change the timbre.
    LoadAndRender(pLo, reg, 0.0, bufLo);
    Ok('macro loaded onto part', pLo.MacroCount = 1, Format('%d macros', [pLo.MacroCount]));
    LoadAndRender(pHi, reg, 1.0, bufHi);
    maxDiff := 0; peakLo := 0;
    for i := 0 to frames * 2 - 1 do
    begin
      diff := Abs(bufLo[i] - bufHi[i]);
      if diff > maxDiff then maxDiff := diff;
      if Abs(bufLo[i]) > peakLo then peakLo := Abs(bufLo[i]);
    end;
    Ok('macro changes timbre', (maxDiff > 1e-3) and (peakLo > 0.001),
       Format('maxDiff=%.4f', [maxDiff]));

    // Determinism: the same macro value reproduces bit-for-bit on a fresh part.
    LoadAndRender(pDet, reg, 1.0, bufB);
    maxDiff := 0;
    for i := 0 to frames * 2 - 1 do
    begin
      diff := Abs(bufHi[i] - bufB[i]);
      if diff > maxDiff then maxDiff := diff;
    end;
    Ok('macro value reproduces', maxDiff = 0, Format('maxDiff=%.3g', [maxDiff]));
    pLo.Free; pHi.Free; pDet.Free;

    // .safinst round-trip of the macro + its mapping.
    ms := TMemoryStream.Create;
    reg2 := TSedaiInstrumentRegistry.CreateEmpty;
    try
      reg.SaveToStream(ms, 'Macros');
      ms.Position := 0;
      reg2.LoadFromStream(ms);
      idx := reg2.FindByName('Macro Pad');
      Ok('.safinst macro round-trips',
         (idx >= 0) and (Length(reg2.Get(idx).Macros) = 1) and
         (reg2.Get(idx).Macros[0].Name = 'Brightness') and
         (Length(reg2.Get(idx).Macros[0].Mappings) = 1) and
         (reg2.Get(idx).Macros[0].Mappings[0].Dest = mdFilterCutoff) and
         (Abs(reg2.Get(idx).Macros[0].Mappings[0].MaxVal - 8000.0) < 1e-2), '');
    finally
      reg2.Free; ms.Free;
    end;
  finally
    reg.Free;
  end;
end;

procedure TestPartPolyphony;
var
  eng: TSAFEngine;
  pa, pb: TSAFPart;
  i: Integer;
  buf: array of Single;
  frames: Integer;
  peak: Single;
begin
  // Each part is sized independently to exactly the voices its instrument needs;
  // total polyphony is the sum of the parts. Parts of different techniques play
  // together (mixed synthesis). No global cap / no cross-part stealing.
  WriteLn('== Per-part polyphony (independent pools, mixed techniques) ==');
  frames := 256;
  SetLength(buf, frames * 2);
  eng := TSAFEngine.Create(SR);
  try
    pa := eng.AddPart('Strings', 5);   // deliberately-sized pools (any count, incl. odd)
    pb := eng.AddPart('FM Bass', 3);
    pa.SetInstrument(psClassic, 'saw');
    pb.SetInstrument(psFM, 'bass');     // a different technique in the same engine
    Ok('part A sized', pa.Polyphony = 5, Format('A poly=%d', [pa.Polyphony]));
    Ok('part B sized', pb.Polyphony = 3, Format('B poly=%d', [pb.Polyphony]));

    // Overdriving a part caps it at its OWN pool size; the other part is
    // untouched (no shared budget). 9 + 7 requested -> 5 + 3 active.
    for i := 0 to 8 do pa.NoteOn(48 + i, 1.0);
    for i := 0 to 6 do pb.NoteOn(36 + i, 1.0);
    Ok('part A capped at its pool', pa.ActiveVoiceCount = 5,
       Format('A active=%d (pool 5)', [pa.ActiveVoiceCount]));
    Ok('part B capped at its pool', pb.ActiveVoiceCount = 3,
       Format('B active=%d (pool 3)', [pb.ActiveVoiceCount]));
    Ok('engine total = sum of parts', eng.TotalActiveVoices = 8,
       Format('total=%d', [eng.TotalActiveVoices]));

    // Both techniques actually produce audio in the same mix.
    FillChar(buf[0], frames * 2 * SizeOf(Single), 0);
    eng.RenderBlock(@buf[0], frames);
    peak := 0;
    for i := 0 to frames * 2 - 1 do
      if Abs(buf[i]) > peak then peak := Abs(buf[i]);
    Ok('mixed-technique mix is audible', peak > 0.001, Format('peak=%.4f', [peak]));

    // Resizing a part's polyphony takes effect (sized up to the instrument need).
    pa.AllSoundOff;
    pa.SetPolyphony(9);
    Ok('part resized', pa.Polyphony = 9, Format('A poly=%d', [pa.Polyphony]));
    for i := 0 to 8 do pa.NoteOn(48 + i, 1.0);
    Ok('resized part uses new size', pa.ActiveVoiceCount = 9,
       Format('A active=%d (pool 9)', [pa.ActiveVoiceCount]));
  finally
    eng.Free;
  end;
end;

procedure TestEngineMix;
var
  eng: TSAFEngine;
  reg: TSedaiInstrumentRegistry;
  i, sounding, totalActive: Integer;
  pk: Single;

  // Add a part, load a catalogued instrument into it, play some notes.
  function AddVoiced(const APart, AInstrument: string;
    const ANotes: array of Byte): TSAFPart;
  var p: TSAFPart; k: Integer;
  begin
    p := eng.AddPart(APart);
    reg.ApplyToPartByName(AInstrument, p);   // preset also sizes the part
    for k := 0 to High(ANotes) do
      p.NoteOn(ANotes[k], 0.9);
    Result := p;
  end;

begin
  // The whole pipeline: several parts of DIFFERENT techniques, each sized by its
  // preset, played together through engine -> mixer -> master. The full mix must
  // be audible, stay bounded (master limiter), and every part must contribute.
  WriteLn('== Engine multi-part mix (end-to-end, mixed techniques) ==');
  reg := InstrumentRegistry;
  eng := TSAFEngine.Create(SR);
  try
    AddVoiced('Strings',  'Strings',       [60, 64, 67]);   // additive
    AddVoiced('Brass',    'Brass Section', [55, 59, 62]);   // additive
    AddVoiced('Keys',     'FM E-Piano',    [48, 52, 55]);   // FM
    AddVoiced('Lead',     'SID Lead',      [72]);           // SID
    AddVoiced('Bass',     'Plucked Bass',  [36]);           // karplus
    AddVoiced('Perc',     'Karplus Drum',  [40]);           // karplus

    Ok('six parts created', eng.PartCount = 6, Format('parts=%d', [eng.PartCount]));

    // Count parts contributing + total voices BEFORE rendering (short sounds may
    // decay during the render).
    sounding := 0;
    for i := 0 to eng.PartCount - 1 do
      if eng.GetPart(i).ActiveVoiceCount > 0 then Inc(sounding);
    totalActive := eng.TotalActiveVoices;
    Ok('every part is sounding', sounding = 6, Format('%d/6 parts active', [sounding]));
    Ok('engine total = sum of parts', totalActive = 12, Format('total=%d', [totalActive]));

    // The summed mix is audible and the master keeps it bounded (no runaway clip).
    pk := EnginePeak(eng, 0.3);
    Ok('full mix audible', pk > 0.05, Format('peak=%.4f', [pk]));
    Ok('master keeps the mix bounded', pk <= 1.05, Format('peak=%.4f (<=1.05)', [pk]));

    // Per-part channel mute removes one instrument from the mix without killing
    // the rest (mixer routing is per part).
    for i := 0 to eng.PartCount - 1 do eng.GetPart(i).AllSoundOff;
    eng.GetPart(0).NoteOn(60, 0.9);
    eng.GetChannel(0).Muted := True;
    Ok('muted channel is silent', EnginePeak(eng, 0.1) < 1e-4, '');
    eng.GetChannel(0).Muted := False;
    Ok('unmuted channel returns', EnginePeak(eng, 0.1) > 0.01, '');
  finally
    eng.Free;
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

// StereoToMono must respect the buffer layout. File readers produce PLANAR
// buffers (SetFormat forces FInterleaved := False), and a previous bug folded
// them as if interleaved -> it averaged adjacent samples of the LEFT channel,
// i.e. decimation-by-2 (an octave up) instead of (L+R)/2. Guard both the
// averaging and the absence of that octave shift.
procedure TestStereoToMono;
const
  NS = 4096;
  F  = 100.0;   // low freq: the decimation bug would shift it to 200 Hz
var
  b: TSedaiAudioBuffer;
  inter: array of Single;
  i: Integer;
  got, maxErr: Double;
begin
  WriteLn('== StereoToMono layout (planar) ==');
  SetLength(inter, NS * 2);
  for i := 0 to NS - 1 do
  begin
    inter[i*2]     := Sin(2*Pi*F*i/SR);        // L
    inter[i*2 + 1] := 0.5 * Sin(2*Pi*F*i/SR);  // R -> average = 0.75*sin
  end;
  b := TSedaiAudioBuffer.Create;
  try
    b.Allocate(2, NS);
    b.SetFormat(SR, 2);                 // planar (FInterleaved := False)
    b.WriteInterleaved(@inter[0], 0, NS);
    b.StereoToMono;
    maxErr := 0;
    for i := 0 to NS - 1 do
    begin
      got := b.GetSample(0, i);
      if Abs(got - 0.75 * Sin(2*Pi*F*i/SR)) > maxErr then
        maxErr := Abs(got - 0.75 * Sin(2*Pi*F*i/SR));
    end;
    Ok('stereo->mono planar avg',
       (b.Channels = 1) and (b.SampleCount = NS) and (maxErr < 1e-4),
       Format('maxErr=%.2e', [maxErr]));
  finally
    b.Free;
  end;
end;

// Per-harmonic amplitude breakpoint tracks (analysis/resynthesis enabler): a
// track on harmonic 0 = (0,0)->(0.5,1)->(1.0,0.5) must shape the output
// amplitude accordingly, held past the end. Tracks are opt-in (off by default).
procedure TestHarmonicTrack;
const TSR = 48000;
var
  g: TSedaiAdditiveGenerator;
  ts: array[0..2] of Single = (0.0, 0.5, 1.0);
  vs: array[0..2] of Single = (0.0, 1.0, 0.5);
  buf: array of Single;
  i, n: Integer;
  refp: Single;

  function PeakAt(t: Single): Single;
  var a, b, k: Integer;
  begin
    a := Round((t-0.010)*TSR); if a < 0 then a := 0;
    b := Round((t+0.010)*TSR); if b > High(buf) then b := High(buf);
    Result := 0;
    for k := a to b do if Abs(buf[k]) > Result then Result := Abs(buf[k]);
  end;

begin
  WriteLn('== per-harmonic breakpoint tracks ==');
  g := TSedaiAdditiveGenerator.Create;
  try
    g.SetSampleRate(TSR);
    g.HarmonicCount := 1;
    g.AmpEnvelope.AttackTime := 0; g.AmpEnvelope.DecayTime := 0;
    g.AmpEnvelope.SustainLevel := 1; g.AmpEnvelope.ReleaseTime := 0;
    g.SetHarmonicTrack(0, ts, vs);
    g.NoteOn(69, 1.0);
    n := Round(1.2*TSR); SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;
    refp := PeakAt(0.5);
    Ok('track shapes amplitude',
       (refp > 0) and (Abs(PeakAt(0.25)/refp - 0.5) < 0.05)
       and (Abs(PeakAt(0.75)/refp - 0.75) < 0.05)
       and (Abs(PeakAt(1.10)/refp - 0.5) < 0.05),
       Format('r25=%.2f r75=%.2f rHold=%.2f',
         [PeakAt(0.25)/refp, PeakAt(0.75)/refp, PeakAt(1.10)/refp]));
  finally
    g.Free;
  end;
end;

// Per-voice micro-instability (human factor): with jitter+shimmer enabled the
// sustain is no longer dead-steady (amplitude wobbles) and stays bounded; with it
// off (default) the voice is unchanged (covered by the additive round-trip above).
procedure TestMicroInstability;
const TSR = 48000;
var
  g: TSedaiAdditiveGenerator;
  buf: array of Single;
  i, n, w, nw: Integer;
  pk, mean, sd, cv: Double;
  peaks: array of Double;

  function WinPeak(a: Integer): Single;
  var k, e: Integer;
  begin
    Result := 0; e := a + Round(0.05*TSR);
    if e > High(buf) then e := High(buf);
    for k := a to e do if Abs(buf[k]) > Result then Result := Abs(buf[k]);
  end;

begin
  WriteLn('== per-voice micro-instability ==');
  RandSeed := 12345;
  g := TSedaiAdditiveGenerator.Create;
  try
    g.SetSampleRate(TSR);
    g.HarmonicCount := 1; g.SetHarmonicLevel(0, 1.0);
    g.AmpEnvelope.AttackTime := 0; g.AmpEnvelope.DecayTime := 0;
    g.AmpEnvelope.SustainLevel := 1; g.AmpEnvelope.ReleaseTime := 0;
    g.SetMicroInstability(6.0, 0.2, 5.0);   // 6 cents jitter, 20% shimmer, 5 Hz
    g.NoteOn(69, 1.0);
    n := TSR; SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;
    // per-50ms window peaks over the second half (past any settling)
    nw := 0; SetLength(peaks, 20);
    w := n div 2;
    while (w + Round(0.05*TSR) < n) and (nw < 20) do
    begin peaks[nw] := WinPeak(w); Inc(nw); Inc(w, Round(0.05*TSR)); end;
    mean := 0; for i := 0 to nw-1 do mean := mean + peaks[i]; mean := mean/nw;
    sd := 0; for i := 0 to nw-1 do sd := sd + Sqr(peaks[i]-mean); sd := Sqrt(sd/nw);
    cv := sd/(mean+1e-9);
    pk := 0; for i := 0 to n-1 do if Abs(buf[i]) > pk then pk := Abs(buf[i]);
    Ok('shimmer wobbles amplitude', (cv > 0.03) and (pk < 1.5),
       Format('cv=%.3f peak=%.2f', [cv, pk]));
  finally
    g.Free;
  end;
end;

// Per-partial bandwidth ("metal"): with depth 0 the generator is inert (a single
// harmonic + flat envelope is a dead-constant sine, no RNG touched); with depth on,
// each partial's amplitude wobbles (spectral broadening) but stays bounded.
procedure TestBandwidth;
const TSR = 48000;
var
  g: TSedaiAdditiveGenerator;
  buf: array of Single;
  i, n, w, nw: Integer;
  pk, mean, sd, cv: Double;
  peaks: array of Double;

  function WinPeak(a: Integer): Single;
  var k, e: Integer;
  begin
    Result := 0; e := a + Round(0.05*TSR);
    if e > High(buf) then e := High(buf);
    for k := a to e do if Abs(buf[k]) > Result then Result := Abs(buf[k]);
  end;

  // local counter j: a nested function may not use an enclosing-scope var as a
  // for-loop counter ("Illegal counter variable" in FPC).
  function RenderCV(depth: Single): Double;
  var j: Integer;
  begin
    g.SetBandwidth(depth, 30);
    g.NoteOn(69, 1.0);
    n := TSR; SetLength(buf, n);
    for j := 0 to n-1 do buf[j] := g.GenerateSample;
    nw := 0; SetLength(peaks, 20); w := n div 2;
    while (w + Round(0.05*TSR) < n) and (nw < 20) do
    begin peaks[nw] := WinPeak(w); Inc(nw); Inc(w, Round(0.05*TSR)); end;
    mean := 0; for j := 0 to nw-1 do mean := mean + peaks[j]; mean := mean/nw;
    sd := 0; for j := 0 to nw-1 do sd := sd + Sqr(peaks[j]-mean); sd := Sqrt(sd/nw);
    Result := sd/(mean+1e-9);
  end;

begin
  WriteLn('== per-partial bandwidth ("metal") ==');
  RandSeed := 4242;
  g := TSedaiAdditiveGenerator.Create;
  try
    g.SetSampleRate(TSR);
    g.HarmonicCount := 1; g.SetHarmonicLevel(0, 1.0);
    g.AmpEnvelope.AttackTime := 0; g.AmpEnvelope.DecayTime := 0;
    g.AmpEnvelope.SustainLevel := 1; g.AmpEnvelope.ReleaseTime := 0;

    cv := RenderCV(0.0);          // inert: flat sine, no wobble
    Ok('bandwidth 0 is inert', cv < 1e-4, Format('cv=%.5f', [cv]));

    cv := RenderCV(0.10);         // band on: clear per-window wobble, bounded
    pk := 0; for i := 0 to High(buf) do if Abs(buf[i]) > pk then pk := Abs(buf[i]);
    Ok('bandwidth broadens the partial', (cv > 0.02) and (pk < 1.5),
       Format('cv=%.3f peak=%.2f', [cv, pk]));
  finally
    g.Free;
  end;
end;

// Waveguide single-reed generator (TSedaiReedGenerator): a self-oscillating
// physical model (nonlinear reed + bore feedback loop). Verifies: silent before
// note-on; self-oscillates + bounded after; the self-oscillation frequency tracks
// the played note (zero-cross count ~ f0); cylindrical bore is odd-harmonic
// (near-zero energy at 2*f0); note-off decays to silence.
procedure TestReedGenerator;
const TSR = 48000;
var
  g: TSedaiReedGenerator;
  buf: array of Single;
  i, n, zc: Integer;
  pkPre, pkSus, fEst, e1, e2: Single;

  // magnitude at frequency f (Goertzel over the sustain window)
  function MagAt(a, b: Integer; f: Single): Single;
  var k: Integer; w, c, s, q0, q1, q2: Single;
  begin
    w := 2*Pi*f/TSR; c := 2*Cos(w); s := 0; q1 := 0; q2 := 0;
    for k := a to b do begin q0 := c*q1 - q2 + buf[k]; q2 := q1; q1 := q0; end;
    Result := Sqrt(q1*q1 + q2*q2 - c*q1*q2);
  end;
begin
  WriteLn('== waveguide single-reed engine ==');
  g := TSedaiReedGenerator.Create;
  try
    g.SetSampleRate(TSR);
    g.SetReed(0.7, -0.44);
    g.SetBreath(0.55, 0, 0, 5);
    g.SetReflection(0.95);
    g.SetBoreType(rbCylindrical, 0);

    n := TSR;                          // 1 s
    SetLength(buf, n);

    // (1) silent before note-on
    pkPre := 0;
    for i := 0 to 99 do
    begin
      buf[i] := g.GenerateSample;
      if Abs(buf[i]) > pkPre then pkPre := Abs(buf[i]);
    end;
    Ok('reed silent before note-on', pkPre = 0, Format('pk=%.4f', [pkPre]));

    // render a full note
    g.NoteOn(60, 1.0);                 // C4 = 261.63 Hz
    for i := 0 to n - 1 do buf[i] := g.GenerateSample;

    // (2) oscillates + bounded
    pkSus := 0;
    for i := Round(0.4*TSR) to Round(0.9*TSR) do
      if Abs(buf[i]) > pkSus then pkSus := Abs(buf[i]);
    Ok('reed self-oscillates + bounded', (pkSus > 0.01) and (pkSus < 1.0),
       Format('peak=%.3f', [pkSus]));

    // (3) frequency ~ the played note (zero crossings over 0.4..0.9s)
    zc := 0;
    for i := Round(0.4*TSR)+1 to Round(0.9*TSR) do
      if (buf[i-1] <= 0) and (buf[i] > 0) then Inc(zc);
    fEst := zc / 0.5;
    Ok('reed pitch tracks note (C4~261.6)', (fEst > 250) and (fEst < 274),
       Format('f~%.1f Hz', [fEst]));

    // (4) cylindrical => odd harmonics: 2*f0 much weaker than 3*f0
    e1 := MagAt(Round(0.4*TSR), Round(0.9*TSR), 523.25);   // 2*f0
    e2 := MagAt(Round(0.4*TSR), Round(0.9*TSR), 784.88);   // 3*f0
    Ok('reed cylindrical is odd-harmonic (2f0 << 3f0)', e1 < 0.25*e2,
       Format('|2f0|=%.3f |3f0|=%.3f', [e1, e2]));

    // (5) note-off decays to silence
    g.NoteOff;
    for i := 0 to n - 1 do buf[i] := g.GenerateSample;
    pkSus := 0;
    for i := Round(0.5*TSR) to n - 1 do
      if Abs(buf[i]) > pkSus then pkSus := Abs(buf[i]);
    Ok('reed note-off decays to silence', pkSus < 0.005, Format('tail peak=%.4f', [pkSus]));

    // (6) CONICAL (faux-cone sax): STK Saxofony reed (slope +0.3) + breath noise;
    // full harmonic series => 2*f0 is a STRONG harmonic (unlike the clarinet).
    g.Kill;
    g.SetReed(0.7, 0.3);
    g.SetBreath(0.6, 0.2, 0, 5);
    g.SetBoreType(rbConical, 0.2);
    g.NoteOn(60, 1.0);
    for i := 0 to n - 1 do buf[i] := g.GenerateSample;
    pkSus := 0;
    for i := Round(0.4*TSR) to Round(0.9*TSR) do
      if Abs(buf[i]) > pkSus then pkSus := Abs(buf[i]);
    Ok('reed conical self-oscillates + bounded', (pkSus > 0.01) and (pkSus < 1.0),
       Format('peak=%.3f', [pkSus]));
    e1 := MagAt(Round(0.4*TSR), Round(0.9*TSR), 523.25);   // 2*f0
    e2 := MagAt(Round(0.4*TSR), Round(0.9*TSR), 261.63);   // f0
    Ok('reed conical has even harmonics (2f0 present)', e1 > 0.2*e2,
       Format('|2f0|=%.3f |f0|=%.3f', [e1, e2]));

    // (7) velocity scales loudness (soft note quieter, but still oscillating)
    g.Kill; g.NoteOn(60, 1.0);
    for i := 0 to n - 1 do buf[i] := g.GenerateSample;
    e2 := 0; for i := Round(0.4*TSR) to Round(0.9*TSR) do e2 := e2 + buf[i]*buf[i];   // loud energy
    g.Kill; g.NoteOn(60, 0.4);
    for i := 0 to n - 1 do buf[i] := g.GenerateSample;
    e1 := 0; for i := Round(0.4*TSR) to Round(0.9*TSR) do e1 := e1 + buf[i]*buf[i];   // soft energy
    Ok('reed velocity scales loudness (soft < loud, still sounds)',
       (e1 > 0) and (e1 < 0.7 * e2), Format('soft/loud=%.2f', [e1/Max(e2,1e-9)]));
  finally
    g.Free;
  end;
end;

// Free-partial engine (TSedaiPartialGenerator): the second-generation additive
// with N free partials, each a (t,freq,amp) track and a continuous-phase
// oscillator. Verifies: inert at 0 partials; a constant partial plays on pitch at
// its track amplitude; a moving partial tracks the frequency sweep; a partial is
// silent outside its born/death range; the analysis-f0 ratio transposes the whole
// cluster; the sum stays bounded; NoteOff fades out (not a truncation).
procedure TestPartialGenerator;
const TSR = 48000;
var
  g: TSedaiPartialGenerator;
  buf: array of Single;
  i, n: Integer;
  fEarly, fLate, fPlay: Single;
  pkBefore, pkIn, pkAfter, pk, pkRel1, pkRel2, ampPk: Single;
  ts, fs, as_: array[0..1] of Single;

  // rising-edge zero-cross frequency over sample window [a,b]
  function FreqIn(a, b: Integer): Single;
  var k, edges: Integer;
  begin
    if a < 1 then a := 1;
    if b > High(buf) then b := High(buf);
    edges := 0;
    for k := a to b do
      if (buf[k-1] <= 0) and (buf[k] > 0) then Inc(edges);
    if b > a then Result := edges / ((b - a) / TSR) else Result := 0;
  end;

  function PeakIn(a, b: Integer): Single;
  var k: Integer;
  begin
    if a < 0 then a := 0;
    if b > High(buf) then b := High(buf);
    Result := 0;
    for k := a to b do if Abs(buf[k]) > Result then Result := Abs(buf[k]);
  end;

  // one constant partial over [0,dur], set on generator g
  procedure OneConstPartial(freq, amp, dur: Single);
  begin
    ts[0] := 0;    ts[1] := dur;
    fs[0] := freq; fs[1] := freq;
    as_[0] := amp; as_[1] := amp;
    g.SetPartialCount(1);
    g.SetPartial(0, ts, fs, as_);
  end;

begin
  WriteLn('== free-partial engine ==');
  g := TSedaiPartialGenerator.Create;
  try
    g.SetSampleRate(TSR);

    // (1) inert at 0 partials -> silence
    g.ClearPartials;
    g.NoteOn(69, 1.0);
    n := Round(0.2*TSR); SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;
    Ok('0 partials is silent', PeakIn(0, n-1) < 1e-7, Format('pk=%.2e', [PeakIn(0,n-1)]));

    // (2) constant partial: on pitch (440 Hz) at its track amplitude (0.5)
    OneConstPartial(440.0, 0.5, 1.0);
    g.SetAnalysisF0(0);            // no transposition (play at recorded freq)
    g.NoteOn(69, 1.0);
    n := Round(1.0*TSR); SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;
    fEarly := FreqIn(Round(0.1*TSR), Round(0.9*TSR));
    ampPk := PeakIn(Round(0.1*TSR), Round(0.9*TSR));
    Ok('constant partial on pitch', Abs(fEarly - 440) < 2.0, Format('f=%.1f Hz', [fEarly]));
    Ok('constant partial amplitude', Abs(ampPk - 0.5) < 0.02, Format('pk=%.3f', [ampPk]));

    // (3) moving partial: frequency sweep 200 -> 800 Hz is tracked
    ts[0] := 0.0;   ts[1] := 1.0;
    fs[0] := 200.0; fs[1] := 800.0;
    as_[0] := 0.5;  as_[1] := 0.5;
    g.SetPartialCount(1);
    g.SetPartial(0, ts, fs, as_);
    g.NoteOn(69, 1.0);
    n := Round(1.0*TSR); SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;
    fEarly := FreqIn(Round(0.05*TSR), Round(0.15*TSR));   // ~230 Hz region
    fLate  := FreqIn(Round(0.85*TSR), Round(0.95*TSR));   // ~740 Hz region
    Ok('moving partial tracks sweep',
       (fEarly > 180) and (fEarly < 320) and (fLate > 680) and (fLate < 820)
       and (fLate > fEarly + 300),
       Format('early=%.0f late=%.0f', [fEarly, fLate]));

    // (4) born/death: partial spanning [0.3,0.6] is silent before/after, audible in
    ts[0] := 0.3;   ts[1] := 0.6;
    fs[0] := 440.0; fs[1] := 440.0;
    as_[0] := 0.5;  as_[1] := 0.5;
    g.SetPartialCount(1);
    g.SetPartial(0, ts, fs, as_);
    g.NoteOn(69, 1.0);
    n := Round(0.9*TSR); SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;
    pkBefore := PeakIn(0, Round(0.25*TSR));
    pkIn     := PeakIn(Round(0.35*TSR), Round(0.55*TSR));
    pkAfter  := PeakIn(Round(0.65*TSR), n-1);
    Ok('partial born/death range',
       (pkBefore < 1e-6) and (pkIn > 0.4) and (pkAfter < 1e-6),
       Format('before=%.2e in=%.3f after=%.2e', [pkBefore, pkIn, pkAfter]));

    // (5) transposition: analysis f0 = 440; play A5 (880 Hz) -> ratio 2 -> a 300 Hz
    // partial should render at 600 Hz.
    OneConstPartial(300.0, 0.5, 1.0);
    g.SetAnalysisF0(440.0);        // A4 reference; A5 note gives ratio 2
    g.NoteOn(81, 1.0);             // 880 Hz played
    n := Round(0.5*TSR); SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;
    fPlay := FreqIn(Round(0.1*TSR), Round(0.4*TSR));
    Ok('analysis-f0 transposes cluster', Abs(fPlay - 600) < 4.0, Format('f=%.1f Hz', [fPlay]));

    // (6) sum bounded: 8 partials, no runaway / NaN
    g.SetAnalysisF0(0);
    g.SetPartialCount(8);
    for i := 0 to 7 do
    begin
      ts[0] := 0.0; ts[1] := 1.0;
      fs[0] := 200 + i*130; fs[1] := 200 + i*130;
      as_[0] := 0.1; as_[1] := 0.1;
      g.SetPartial(i, ts, fs, as_);
    end;
    g.NoteOn(69, 1.0);
    n := Round(0.5*TSR); SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;
    pk := PeakIn(0, n-1);
    Ok('summed partials bounded', (pk > 0.05) and (pk < 1.0) and (pk = pk),
       Format('pk=%.3f', [pk]));

    // (7) clean release: after NoteOff the output fades (decays), not truncated
    OneConstPartial(440.0, 0.5, 5.0);   // long-lived partial so only release ends it
    g.SetAnalysisF0(0);
    g.SetRelease(0.12);
    g.NoteOn(69, 1.0);
    n := Round(0.2*TSR); SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;    // sustain to 0.2 s
    g.NoteOff;
    n := Round(0.4*TSR); SetLength(buf, n);
    for i := 0 to n-1 do buf[i] := g.GenerateSample;    // capture the release
    pkRel1 := PeakIn(Round(0.00*TSR), Round(0.02*TSR));  // just after NoteOff
    pkRel2 := PeakIn(Round(0.10*TSR), Round(0.12*TSR));  // ~one release-time later
    pkAfter := PeakIn(Round(0.30*TSR), n-1);             // well past the fade
    Ok('release fades out (no truncation)',
       (pkRel1 > 0.4) and (pkRel2 < pkRel1 * 0.6) and (pkAfter < 1e-3),
       Format('t0=%.3f tR=%.3f end=%.2e', [pkRel1, pkRel2, pkAfter]));
  finally
    g.Free;
  end;
end;

// The LIVING additive preset (per-harmonic amplitude tracks + micro-instability
// + breath + natural release + RMS-matched trim) survives a .safinst round-trip:
// the naturalness layer is carried in the file, not just in the analyzer harness.
// Also confirms it loads into a Part and renders audible + bounded.
procedure TestLivingPresetRoundTrip;
const TSR = 48000;
var
  authored: TInstrumentPreset;
  reg, reg2: TSedaiInstrumentRegistry;
  ms: TMemoryStream;
  idx, i, n: Integer;
  gp: TAdditiveParams;
  tracksOk, humanOk, breathOk, residualOk: Boolean;
  part: TSAFPart;
  buf: array of Single;
  pk: Single;
begin
  WriteLn('== living additive preset .safinst round-trip ==');
  authored := Default(TInstrumentPreset);
  authored.Name := 'RT Living'; authored.Category := icStrings;
  authored.Technique := psAdditive; authored.PresetKey := 'strings';
  authored.Polyphony := 8;
  authored.Common.OverrideFilter := True; authored.Common.FilterEnabled := False;
  authored.HasAdditiveParams := True;
  authored.Additive.HarmonicCount := 3;
  authored.Additive.Levels[0] := 1.0; authored.Additive.Levels[1] := 0.5;
  authored.Additive.Levels[2] := 0.25; authored.Additive.Detunes[2] := 3.0;
  authored.Additive.Attack := 0.0; authored.Additive.Decay := 0.0;
  authored.Additive.Sustain := 1.0; authored.Additive.Release := 0.18;
  authored.Additive.OutputTrim := 0.42;
  authored.Additive.JitterCents := 2.2;
  authored.Additive.ShimmerDepth := 0.06;
  authored.Additive.RateHz := 5.0;
  authored.Additive.BreathLevel := 0.015;
  authored.Additive.BreathCutoff := 4000;
  authored.Additive.BandDepth := 0.04;
  authored.Additive.BandCutoff := 45;
  authored.Additive.ResidualLevel := 0.03;
  authored.Additive.ResidualGains[0] := 0.5;
  authored.Additive.ResidualGains[3] := 0.8;
  // harmonic 0: 3 breakpoints; harmonic 2: 2 breakpoints; harmonic 1: none.
  SetLength(authored.Additive.Tracks[0].T, 3); SetLength(authored.Additive.Tracks[0].V, 3);
  authored.Additive.Tracks[0].T[0] := 0.0;  authored.Additive.Tracks[0].V[0] := 0.0;
  authored.Additive.Tracks[0].T[1] := 0.05; authored.Additive.Tracks[0].V[1] := 1.0;
  authored.Additive.Tracks[0].T[2] := 1.0;  authored.Additive.Tracks[0].V[2] := 0.8;
  SetLength(authored.Additive.Tracks[2].T, 2); SetLength(authored.Additive.Tracks[2].V, 2);
  authored.Additive.Tracks[2].T[0] := 0.0; authored.Additive.Tracks[2].V[0] := 0.0;
  authored.Additive.Tracks[2].T[1] := 0.3; authored.Additive.Tracks[2].V[1] := 0.25;

  ms := TMemoryStream.Create;
  reg := TSedaiInstrumentRegistry.CreateEmpty;
  reg2 := TSedaiInstrumentRegistry.CreateEmpty;
  try
    reg.AddPreset(authored);
    reg.SaveToStream(ms, 'Living');
    ms.Position := 0;
    reg2.LoadFromStream(ms);
    idx := reg2.FindByName('RT Living');
    if idx >= 0 then gp := reg2.Get(idx).Additive else gp := Default(TAdditiveParams);

    humanOk := (Abs(gp.JitterCents - 2.2) < 1e-4) and (Abs(gp.ShimmerDepth - 0.06) < 1e-4)
      and (Abs(gp.RateHz - 5.0) < 1e-4);
    breathOk := (Abs(gp.BreathLevel - 0.015) < 1e-4) and (Abs(gp.BreathCutoff - 4000) < 1e-2)
      and (Abs(gp.BandDepth - 0.04) < 1e-4) and (Abs(gp.BandCutoff - 45) < 1e-2);
    residualOk := (Abs(gp.ResidualLevel - 0.03) < 1e-4)
      and (Abs(gp.ResidualGains[0] - 0.5) < 1e-4) and (Abs(gp.ResidualGains[3] - 0.8) < 1e-4)
      and (Abs(gp.ResidualGains[1]) < 1e-6);
    tracksOk := (Length(gp.Tracks[0].T) = 3) and (Length(gp.Tracks[1].T) = 0)
      and (Length(gp.Tracks[2].T) = 2)
      and (Abs(gp.Tracks[0].V[1] - 1.0) < 1e-4) and (Abs(gp.Tracks[0].T[2] - 1.0) < 1e-4)
      and (Abs(gp.Tracks[2].V[1] - 0.25) < 1e-4);

    Ok('living human params round-trip', (idx >= 0) and humanOk,
       Format('jit=%.3f shim=%.3f rate=%.2f', [gp.JitterCents, gp.ShimmerDepth, gp.RateHz]));
    Ok('living breath+band round-trip', (idx >= 0) and breathOk,
       Format('breath=%.4f/%.0f band=%.3f/%.0f',
         [gp.BreathLevel, gp.BreathCutoff, gp.BandDepth, gp.BandCutoff]));
    Ok('living residual round-trip', (idx >= 0) and residualOk,
       Format('res=%.3f g0=%.2f g3=%.2f', [gp.ResidualLevel, gp.ResidualGains[0], gp.ResidualGains[3]]));
    Ok('living tracks round-trip', (idx >= 0) and tracksOk,
       Format('t0=%d t1=%d t2=%d',
         [Length(gp.Tracks[0].T), Length(gp.Tracks[1].T), Length(gp.Tracks[2].T)]));

    // Loads into a Part and renders audible + bounded (RandSeed fixes the human RNG).
    RandSeed := 777;
    part := TSAFPart.Create;
    try
      part.SetSampleRate(TSR);
      if idx >= 0 then part.SetInstrument(psAdditive, reg2.Get(idx).PresetKey);
      part.SetAdditiveParams(gp);
      n := 8192; SetLength(buf, n*2);
      FillChar(buf[0], n*2*SizeOf(Single), 0);
      part.NoteOn(69, 1.0);
      part.RenderBlock(@buf[0], n);
      pk := 0; for i := 0 to n*2-1 do if Abs(buf[i]) > pk then pk := Abs(buf[i]);
      Ok('living preset renders bounded', (idx >= 0) and (pk > 0.001) and (pk < 1.5),
         Format('peak=%.3f', [pk]));
    finally
      part.Free;
    end;
  finally
    reg.Free; reg2.Free; ms.Free;
  end;
end;

// Free-partial preset .safinst round-trip: a psPartial preset carrying N free
// partials (each a breakpoint track of time/freq/amp) survives save->load with
// its trajectories intact, and drives an audible, bounded voice through a Part.
procedure TestPartialPreset;
const TSR = 48000;
var
  authored: TInstrumentPreset;
  reg, reg2: TSedaiInstrumentRegistry;
  ms: TMemoryStream;
  idx, i, n: Integer;
  gp: TPartialParams;
  headerOk, p0Ok, p1Ok: Boolean;
  part: TSAFPart;
  buf: array of Single;
  pk: Single;
begin
  WriteLn('== free-partial preset .safinst round-trip ==');
  authored := Default(TInstrumentPreset);
  authored.Name := 'RT Partial'; authored.Category := icWind;
  authored.Technique := psPartial; authored.PresetKey := '';
  authored.Polyphony := 4;
  authored.Common.OverrideFilter := True; authored.Common.FilterEnabled := False;
  authored.HasPartialParams := True;
  authored.Partial.AnalysisF0 := 440.0;
  authored.Partial.Release := 0.15;
  authored.Partial.OutputTrim := 0.5;
  SetLength(authored.Partial.Partials, 2);
  // partial 0: steady fundamental at 440 Hz over [0, 1.0], 3 breakpoints.
  SetLength(authored.Partial.Partials[0].T, 3);
  SetLength(authored.Partial.Partials[0].F, 3);
  SetLength(authored.Partial.Partials[0].A, 3);
  authored.Partial.Partials[0].T[0] := 0.0;  authored.Partial.Partials[0].F[0] := 440.0; authored.Partial.Partials[0].A[0] := 0.0;
  authored.Partial.Partials[0].T[1] := 0.05; authored.Partial.Partials[0].F[1] := 440.0; authored.Partial.Partials[0].A[1] := 0.6;
  authored.Partial.Partials[0].T[2] := 1.0;  authored.Partial.Partials[0].F[2] := 442.0; authored.Partial.Partials[0].A[2] := 0.4;
  // partial 1: an INHARMONIC upper partial, born late (0.1 s) and dying at 0.7 s.
  SetLength(authored.Partial.Partials[1].T, 2);
  SetLength(authored.Partial.Partials[1].F, 2);
  SetLength(authored.Partial.Partials[1].A, 2);
  authored.Partial.Partials[1].T[0] := 0.1; authored.Partial.Partials[1].F[0] := 973.0; authored.Partial.Partials[1].A[0] := 0.2;
  authored.Partial.Partials[1].T[1] := 0.7; authored.Partial.Partials[1].F[1] := 981.0; authored.Partial.Partials[1].A[1] := 0.1;

  ms := TMemoryStream.Create;
  reg := TSedaiInstrumentRegistry.CreateEmpty;
  reg2 := TSedaiInstrumentRegistry.CreateEmpty;
  try
    reg.AddPreset(authored);
    reg.SaveToStream(ms, 'Partials');
    ms.Position := 0;
    reg2.LoadFromStream(ms);
    idx := reg2.FindByName('RT Partial');
    if idx >= 0 then gp := reg2.Get(idx).Partial else gp := Default(TPartialParams);

    headerOk := (Length(gp.Partials) = 2)
      and (Abs(gp.AnalysisF0 - 440.0) < 1e-3) and (Abs(gp.Release - 0.15) < 1e-4)
      and (Abs(gp.OutputTrim - 0.5) < 1e-4);
    p0Ok := (Length(gp.Partials) = 2) and (Length(gp.Partials[0].T) = 3)
      and (Abs(gp.Partials[0].F[2] - 442.0) < 1e-3)
      and (Abs(gp.Partials[0].A[1] - 0.6) < 1e-4)
      and (Abs(gp.Partials[0].T[2] - 1.0) < 1e-4);
    p1Ok := (Length(gp.Partials) = 2) and (Length(gp.Partials[1].T) = 2)
      and (Abs(gp.Partials[1].T[0] - 0.1) < 1e-4)
      and (Abs(gp.Partials[1].F[0] - 973.0) < 1e-3)
      and (Abs(gp.Partials[1].A[1] - 0.1) < 1e-4);

    Ok('partial preset header round-trip', (idx >= 0) and headerOk,
       Format('n=%d f0=%.2f rel=%.3f trim=%.3f',
         [Length(gp.Partials), gp.AnalysisF0, gp.Release, gp.OutputTrim]));
    Ok('partial 0 track round-trip', (idx >= 0) and p0Ok,
       Format('bp=%d f2=%.2f a1=%.3f', [Length(gp.Partials[0].T), gp.Partials[0].F[2], gp.Partials[0].A[1]]));
    Ok('partial 1 (inharmonic, born/death) round-trip', (idx >= 0) and p1Ok,
       Format('bp=%d t0=%.3f f0=%.2f', [Length(gp.Partials[1].T), gp.Partials[1].T[0], gp.Partials[1].F[0]]));

    // Loads into a Part and renders audible + bounded. NoteOn(69) = 440 Hz, so
    // the transposition ratio is 1 and the partials play at their recorded pitch.
    part := TSAFPart.Create;
    try
      part.SetSampleRate(TSR);
      if idx >= 0 then part.SetInstrument(psPartial, reg2.Get(idx).PresetKey);
      part.SetPartialParams(gp);
      n := 8192; SetLength(buf, n*2);
      FillChar(buf[0], n*2*SizeOf(Single), 0);
      part.NoteOn(69, 1.0);
      part.RenderBlock(@buf[0], n);
      pk := 0; for i := 0 to n*2-1 do if Abs(buf[i]) > pk then pk := Abs(buf[i]);
      Ok('partial preset renders bounded', (idx >= 0) and (pk > 0.001) and (pk < 1.5),
         Format('peak=%.3f', [pk]));
    finally
      part.Free;
    end;
  finally
    reg.Free; reg2.Free; ms.Free;
  end;
end;

// Waveguide-reed preset .safinst round-trip: a psReed preset (physical-model sax
// config) survives save->load with its parameters intact, and drives an audible,
// bounded, self-oscillating voice through a Part.
procedure TestReedPreset;
const TSR = 48000;
var
  authored: TInstrumentPreset;
  reg, reg2: TSedaiInstrumentRegistry;
  ms: TMemoryStream;
  idx, i, n: Integer;
  gp: TReedParams;
  paramsOk: Boolean;
  part: TSAFPart;
  buf: array of Single;
  pk: Single;
begin
  WriteLn('== waveguide-reed preset .safinst round-trip ==');
  authored := Default(TInstrumentPreset);
  authored.Name := 'RT Reed Sax'; authored.Category := icWind;
  authored.Technique := psReed; authored.PresetKey := '';
  authored.Polyphony := 4;
  authored.HasReedParams := True;
  authored.Reed.BoreType := rbConical;
  authored.Reed.BlowPosition := 0.2;
  authored.Reed.ReedOffset := 0.7;
  authored.Reed.ReedSlope := 0.3;
  authored.Reed.Pressure := 0.6;
  authored.Reed.Noise := 0.2;
  authored.Reed.VibDepth := 0.05;
  authored.Reed.VibRate := 5.5;
  authored.Reed.ReflMag := 0.95;
  authored.Reed.OutputTrim := 0.8;

  ms := TMemoryStream.Create;
  reg := TSedaiInstrumentRegistry.CreateEmpty;
  reg2 := TSedaiInstrumentRegistry.CreateEmpty;
  try
    reg.AddPreset(authored);
    reg.SaveToStream(ms, 'Reed');
    ms.Position := 0;
    reg2.LoadFromStream(ms);
    idx := reg2.FindByName('RT Reed Sax');
    if idx >= 0 then gp := reg2.Get(idx).Reed else gp := Default(TReedParams);

    paramsOk := (gp.BoreType = rbConical)
      and (Abs(gp.BlowPosition - 0.2) < 1e-4) and (Abs(gp.ReedSlope - 0.3) < 1e-4)
      and (Abs(gp.Pressure - 0.6) < 1e-4) and (Abs(gp.Noise - 0.2) < 1e-4)
      and (Abs(gp.VibRate - 5.5) < 1e-3) and (Abs(gp.ReflMag - 0.95) < 1e-4)
      and (Abs(gp.OutputTrim - 0.8) < 1e-4);
    Ok('reed preset params round-trip', (idx >= 0) and paramsOk,
       Format('bore=%d pos=%.2f slope=%.2f press=%.2f trim=%.2f',
         [Ord(gp.BoreType), gp.BlowPosition, gp.ReedSlope, gp.Pressure, gp.OutputTrim]));

    RandSeed := 4242;
    part := TSAFPart.Create;
    try
      part.SetSampleRate(TSR);
      if idx >= 0 then part.SetInstrument(psReed, reg2.Get(idx).PresetKey);
      part.SetReedParams(gp);
      n := 24000; SetLength(buf, n*2);            // 0.5 s (reed needs time to start)
      FillChar(buf[0], n*2*SizeOf(Single), 0);
      part.NoteOn(60, 1.0);
      part.RenderBlock(@buf[0], n);
      pk := 0; for i := 0 to n*2-1 do if Abs(buf[i]) > pk then pk := Abs(buf[i]);
      Ok('reed preset renders self-oscillating + bounded',
         (idx >= 0) and (pk > 0.001) and (pk < 1.5), Format('peak=%.3f', [pk]));
    finally
      part.Free;
    end;
  finally
    reg.Free; reg2.Free; ms.Free;
  end;
end;

// Auto-space widener: a dual-mono input becomes a decorrelated stereo image
// while staying MONO-SAFE (the mono sum is preserved bit-for-bit up to float
// rounding). Also: Width=0 is a passthrough for a mono source, and Width>0 makes
// L/R genuinely differ (width was created) without clipping.
procedure TestAutoSpace;
const TSR = 48000;
var
  sp: TSedaiAutoSpace;
  ins, outs: array of Single;
  i, n: Integer;
  monoDiff, sideEnergy, refEnergy, pk, ph: Double;
  passDiff: Double;
begin
  WriteLn('== auto-space stereo widener (mono-safe) ==');
  n := TSR;                        // 1 s
  SetLength(ins, n*2); SetLength(outs, n*2);
  ph := 0;
  for i := 0 to n-1 do
  begin
    ph := ph + 2*Pi*220.0/TSR;     // 220 Hz dual-mono sine
    ins[i*2]   := 0.5*Sin(ph);
    ins[i*2+1] := 0.5*Sin(ph);
  end;

  sp := TSedaiAutoSpace.Create;
  try
    sp.SetSampleRate(TSR);         // exercises the SR-change realloc path
    sp.Width := 0.6; sp.Size := 0.5; sp.Mix := 1.0;
    sp.ProcessBlock(@ins[0], @outs[0], n);

    // mono-safety: out L+R == in L+R (=2*mono) every sample, to float epsilon.
    monoDiff := 0;
    for i := 0 to n-1 do
      monoDiff := Max(monoDiff,
        Abs((outs[i*2] + outs[i*2+1]) - (ins[i*2] + ins[i*2+1])));
    Ok('mono sum preserved (mono-safe)', monoDiff < 1e-4, Format('maxdiff=%.2e', [monoDiff]));

    // width created: the side signal (L-R) carries real energy vs the mid ref.
    sideEnergy := 0; refEnergy := 0; pk := 0;
    for i := 0 to n-1 do
    begin
      sideEnergy := sideEnergy + Sqr(outs[i*2] - outs[i*2+1]);
      refEnergy  := refEnergy  + Sqr(ins[i*2] + ins[i*2+1]);
      if Abs(outs[i*2])   > pk then pk := Abs(outs[i*2]);
      if Abs(outs[i*2+1]) > pk then pk := Abs(outs[i*2+1]);
    end;
    Ok('width created (decorrelated L/R)',
       (sideEnergy / (refEnergy + 1e-9) > 0.05) and (pk < 1.5),
       Format('side/ref=%.3f peak=%.3f', [sideEnergy/(refEnergy+1e-9), pk]));

    // lateral reflections raise the side energy further, still mono-safe & bounded.
    sp.Reflect := 0.6; sp.Reset;
    sp.ProcessBlock(@ins[0], @outs[0], n);
    refEnergy := 0; passDiff := 0; pk := 0;   // reuse refEnergy=side2, passDiff=monoDiff2
    for i := 0 to n-1 do
    begin
      refEnergy := refEnergy + Sqr(outs[i*2] - outs[i*2+1]);
      passDiff := Max(passDiff, Abs((outs[i*2] + outs[i*2+1]) - (ins[i*2] + ins[i*2+1])));
      if Abs(outs[i*2]) > pk then pk := Abs(outs[i*2]);
    end;
    Ok('reflections raise side, stay mono-safe',
       (refEnergy > sideEnergy) and (passDiff < 1e-4) and (pk < 1.5),
       Format('side2=%.1f vs side=%.1f monoDiff=%.2e', [refEnergy, sideEnergy, passDiff]));
    sp.Reflect := 0.0;

    // Width=0 is a passthrough for a mono source (side vanishes -> out == in).
    sp.Width := 0.0; sp.Reset;
    sp.ProcessBlock(@ins[0], @outs[0], n);
    passDiff := 0;
    for i := 0 to n*2-1 do passDiff := Max(passDiff, Abs(outs[i] - ins[i]));
    Ok('width=0 passthrough (mono src)', passDiff < 1e-6, Format('maxdiff=%.2e', [passDiff]));
  finally
    sp.Free;
  end;
end;

// Filtered-noise residual (SMS/DDSP stochastic layer): off = silent when there
// are no partials (the branch is gated, no RNG touched), on = bounded noise, and
// the per-band gains shape the spectrum (HF-band gains -> more high-frequency
// content, measured by normalized first-difference energy -- no ear needed).
procedure TestResidual;
const TSR = 48000;
var
  g: TSedaiAdditiveGenerator;
  n: Integer;
  eOff, eLo, eHi, dOff, dLo, dHi, pOff, pLo, pHi: Double;
  loG, hiG: array[0..RESIDUAL_BANDS-1] of Single;
  bi: Integer;

  // Render `n` samples of the isolated residual; return total energy, and the
  // first-difference energy + peak via out params. Fixed seed -> comparable runs.
  function RenderEnergy(level: Single; const gains: array of Single;
    out diffE, pk: Double): Double;
  var j: Integer; e, d: Double; prev, s: Single;
  begin
    RandSeed := 9001;
    g.SetResidual(level, gains);
    g.NoteOn(69, 1.0);
    e := 0; d := 0; pk := 0; prev := 0;
    for j := 0 to n-1 do
    begin
      s := g.GenerateSample;
      e := e + Sqr(s); d := d + Sqr(s - prev);
      if Abs(s) > pk then pk := Abs(s);
      prev := s;
    end;
    diffE := d; Result := e;
  end;

begin
  WriteLn('== filtered-noise residual (SMS stochastic) ==');
  n := TSR;
  for bi := 0 to RESIDUAL_BANDS-1 do begin loG[bi] := 0; hiG[bi] := 0; end;
  loG[0] := 1; loG[1] := 1;                                   // low bands (250/500 Hz)
  hiG[RESIDUAL_BANDS-2] := 1; hiG[RESIDUAL_BANDS-1] := 1;     // high bands (4k/8k)

  g := TSedaiAdditiveGenerator.Create;
  try
    g.SetSampleRate(TSR);
    g.HarmonicCount := 1; g.SetHarmonicLevel(0, 0.0);   // no partials -> isolate residual
    g.AmpEnvelope.AttackTime := 0; g.AmpEnvelope.DecayTime := 0;
    g.AmpEnvelope.SustainLevel := 1; g.AmpEnvelope.ReleaseTime := 0;

    eOff := RenderEnergy(0.0, loG, dOff, pOff);
    Ok('residual off = silent (no partials)', eOff < 1e-9, Format('energy=%.2e', [eOff]));

    eLo := RenderEnergy(0.2, loG, dLo, pLo);
    eHi := RenderEnergy(0.2, hiG, dHi, pHi);
    Ok('residual on adds bounded noise',
       (eLo > 1e-6) and (eHi > 1e-6) and (pLo < 1.5) and (pHi < 1.5),
       Format('eLo=%.4f eHi=%.4f pk=%.2f/%.2f', [eLo, eHi, pLo, pHi]));
    Ok('band gains shape the spectrum (HF>LF)',
       (dHi/(eHi+1e-12)) > (dLo/(eLo+1e-12)) * 1.5,
       Format('hfRatio=%.3f lfRatio=%.3f', [dHi/(eHi+1e-12), dLo/(eLo+1e-12)]));
  finally
    g.Free;
  end;
end;

// Body resonator ("il tubo"): the instrument's modal bank as a decorrelator ->
// coloured, ringing stereo width that leaves the on-axis (mono) spectrum EXACTLY
// the dry partials (mono-safe by construction). Objective checks: mono-safety, the
// sax bank is HF-weighted vs the guitar (family colour), the modes ring after a
// transient (side tail), and Width=0 is a passthrough.
procedure TestBodyResonator;
const TSR = 48000;
var
  br: TSedaiBodyResonator;
  ins, outs: array of Single;
  i, n, m: Integer;
  ph, monoDiff, passDiff: Double;
  saxHF, guitarHF, saxSide, guitarSide, saxPk, guitarPk, tailE, headE: Double;

  // Render body(kind) over a dual-mono noise input; return the side signal's
  // normalized first-difference energy (an HF proxy), plus total side energy + peak.
  function NoiseSideHF(kind: TBodyKind; out totSide, peak: Double): Double;
  var j: Integer; s, prevS, x: Single; dE, e: Double;
  begin
    RandSeed := 1234;
    br.LoadBody(kind); br.Width := 0.6; br.Mix := 1.0; br.Reset;
    for j := 0 to n-1 do begin x := (Random*2-1)*0.4; ins[j*2] := x; ins[j*2+1] := x; end;
    br.ProcessBlock(@ins[0], @outs[0], n);
    dE := 0; e := 0; peak := 0; prevS := 0;
    for j := 0 to n-1 do
    begin
      s := outs[j*2] - outs[j*2+1];
      e := e + Sqr(s); dE := dE + Sqr(s - prevS); prevS := s;
      if Abs(outs[j*2]) > peak then peak := Abs(outs[j*2]);
      if Abs(outs[j*2+1]) > peak then peak := Abs(outs[j*2+1]);
    end;
    totSide := e;
    if e > 1e-12 then Result := dE/e else Result := 0;
  end;

begin
  WriteLn('== body resonator ("il tubo": radiation) ==');
  n := TSR;
  SetLength(ins, n*2); SetLength(outs, n*2);
  br := TSedaiBodyResonator.Create;
  try
    br.SetSampleRate(TSR);

    // mono-safety on a dual-mono sine (bodySax): on-axis spectrum untouched
    br.LoadBody(bodySax); br.Width := 0.6; br.Mix := 1.0; br.Reset;
    ph := 0;
    for i := 0 to n-1 do begin ph := ph + 2*Pi*330/TSR; ins[i*2]:=0.4*Sin(ph); ins[i*2+1]:=0.4*Sin(ph); end;
    br.ProcessBlock(@ins[0], @outs[0], n);
    monoDiff := 0;
    for i := 0 to n-1 do
      monoDiff := Max(monoDiff, Abs((outs[i*2]+outs[i*2+1]) - (ins[i*2]+ins[i*2+1])));
    Ok('mono sum preserved (on-axis untouched)', monoDiff < 1e-4, Format('maxdiff=%.2e',[monoDiff]));

    // family colour: the sax modal bank is HF-weighted, the guitar is LF-weighted
    saxHF := NoiseSideHF(bodySax, saxSide, saxPk);
    guitarHF := NoiseSideHF(bodyGuitar, guitarSide, guitarPk);
    Ok('body creates bounded width', (saxSide > 1e-3) and (saxPk < 1.5) and (guitarPk < 1.5),
       Format('saxSide=%.3f pk=%.2f/%.2f',[saxSide, saxPk, guitarPk]));
    Ok('sax body HF-weighted vs guitar', saxHF > guitarHF * 1.3,
       Format('saxHF=%.3f guitarHF=%.3f',[saxHF, guitarHF]));

    // transient ring: a single dual-mono impulse -> the modes ring in the side AFTER it
    br.LoadBody(bodyViolin); br.Width := 0.6; br.Mix := 1.0; br.Reset;
    m := Min(n, 4096);
    for i := 0 to n-1 do begin ins[i*2]:=0; ins[i*2+1]:=0; end;
    ins[0] := 0.8; ins[1] := 0.8;
    br.ProcessBlock(@ins[0], @outs[0], m);
    headE := 0; tailE := 0;
    for i := 0 to m-1 do
      if i < 8 then headE := headE + Sqr(outs[i*2]-outs[i*2+1])
      else tailE := tailE + Sqr(outs[i*2]-outs[i*2+1]);
    Ok('body rings on a transient (side tail)', tailE > 1e-4, Format('tailE=%.4f headE=%.4f',[tailE, headE]));

    // Width=0 is a passthrough for a mono source
    br.LoadBody(bodySax); br.Width := 0.0; br.Mix := 1.0; br.Reset;
    ph := 0;
    for i := 0 to n-1 do begin ph := ph + 2*Pi*330/TSR; ins[i*2]:=0.4*Sin(ph); ins[i*2+1]:=0.4*Sin(ph); end;
    br.ProcessBlock(@ins[0], @outs[0], n);
    passDiff := 0;
    for i := 0 to n*2-1 do passDiff := Max(passDiff, Abs(outs[i]-ins[i]));
    Ok('width=0 passthrough (mono src)', passDiff < 1e-6, Format('maxdiff=%.2e',[passDiff]));
  finally
    br.Free;
  end;
end;

// Spatial chain end-to-end: body (C) -> auto-space (D) per Part + a shared room
// (reverb) on the master bus, wired through TSAFSpatialChain and rendered by the
// engine. Objective: the full chain renders audible + bounded, produces stereo
// width, and the ownership/free order (engine first, chain second) is clean.
procedure TestSpatialChain;
const TSR = 44100;
var
  eng: TSAFEngine;
  chain: TSAFSpatialChain;
  part: TSAFPart;
  buf: array of Single;
  frames, done, n, i: Integer;
  pk, sideE, e: Double;
begin
  WriteLn('== spatial chain (body -> auto-space -> room) ==');
  eng := TSAFEngine.Create(TSR);
  chain := TSAFSpatialChain.Create;
  try
    part := eng.AddPart('Solo', 8);
    part.SetInstrument(psAdditive, 'strings');
    chain.SpatializePart(eng, 0, bodyViolin, 0.5, 0.5, 0.5);   // C then D
    chain.SetRoom(eng, roomMedium, 0.25);                       // shared room
    part.NoteOn(60, 1.0);
    frames := TSR div 2;                                        // 0.5 s
    SetLength(buf, 512 * 2);
    pk := 0; sideE := 0; e := 0; done := 0;
    while done < frames do
    begin
      n := 512; if done + n > frames then n := frames - done;
      eng.RenderBlock(@buf[0], n);
      for i := 0 to n - 1 do
      begin
        if Abs(buf[i*2])   > pk then pk := Abs(buf[i*2]);
        if Abs(buf[i*2+1]) > pk then pk := Abs(buf[i*2+1]);
        sideE := sideE + Sqr(buf[i*2] - buf[i*2+1]);
        e := e + Sqr(buf[i*2]) + Sqr(buf[i*2+1]);
      end;
      Inc(done, n);
    end;
    Ok('chain renders audible + bounded', (e > 1e-4) and (pk > 0.0) and (pk < 1.5),
       Format('e=%.3f pk=%.3f', [e, pk]));
    Ok('chain produces stereo width', sideE / (e + 1e-9) > 1e-3,
       Format('side/tot=%.4f', [sideE / (e + 1e-9)]));
  finally
    eng.Free;      // engine first: stops referencing the inserts
    chain.Free;    // then the chain frees the owned effects
  end;
end;

// Convolver ("il tubo" C v2, measured IR): a delta input must return the IR
// per-tap (out L = hL, out R = hR), the process is linear (conv(2x) = 2*conv(x)),
// and no IR = passthrough. Objective, no ear needed.
procedure TestConvolver;
const TSR = 48000;
var
  cv: TSedaiConvolver;
  hL, hR: array[0..4] of Single;
  inp, outp, out2: array[0..31] of Single;
  i: Integer;
  tapOk, linOk, passOk: Boolean;
  d: Single;
begin
  WriteLn('== convolver (measured IR: delta->IR, linear) ==');
  hL[0]:=0.5; hL[1]:=0.3; hL[2]:=-0.2; hL[3]:=0.1; hL[4]:=0.05;
  hR[0]:=0.4; hR[1]:=-0.25; hR[2]:=0.15; hR[3]:=0.0; hR[4]:=-0.1;

  cv := TSedaiConvolver.Create;
  try
    cv.SetSampleRate(TSR);

    // no IR -> passthrough
    for i := 0 to 31 do inp[i] := 0;
    inp[0]:=0.7; inp[1]:=0.7; inp[2]:=-0.3; inp[3]:=-0.3;
    cv.ProcessBlock(@inp[0], @outp[0], 16);
    passOk := True;
    for i := 0 to 31 do if Abs(outp[i]-inp[i]) > 1e-7 then passOk := False;
    Ok('no IR = passthrough', passOk);

    cv.LoadIR(hL, hR);
    cv.Reset;
    // delta on both channels at frame 0
    for i := 0 to 31 do inp[i] := 0;
    inp[0] := 1.0; inp[1] := 1.0;
    cv.ProcessBlock(@inp[0], @outp[0], 16);
    // out[frame k] L should equal hL[k], R equal hR[k] for k=0..4
    tapOk := True;
    for i := 0 to 4 do
    begin
      if Abs(outp[i*2]   - hL[i]) > 1e-6 then tapOk := False;
      if Abs(outp[i*2+1] - hR[i]) > 1e-6 then tapOk := False;
    end;
    Ok('delta returns the IR per-tap', tapOk,
       Format('outL[0..2]=%.2f,%.2f,%.2f', [outp[0], outp[2], outp[4]]));

    // linearity: 2*delta -> 2*IR
    cv.Reset;
    for i := 0 to 31 do inp[i] := 0;
    inp[0] := 2.0; inp[1] := 2.0;
    cv.ProcessBlock(@inp[0], @out2[0], 16);
    linOk := True;
    for i := 0 to 9 do
    begin
      d := Abs(out2[i] - 2.0*outp[i]);
      if d > 1e-6 then linOk := False;
    end;
    Ok('convolution is linear (2x->2y)', linOk);
  finally
    cv.Free;
  end;
end;

// Commutation: the body resonator (C v1) is LTI, so convolving with its captured
// impulse response (C v2) reproduces the live filter exactly -> the two "il tubo"
// paths are equivalent (the commuted-synthesis theorem, on our code).
procedure TestCommutation;
const TSR = 48000; TAPS = 4096;
var
  br: TSedaiBodyResonator;
  cv: TSedaiConvolver;
  imp, cap, hL, hR, sig, brOut, cvOut: array of Single;
  i, n: Integer;
  err, pk: Double;
begin
  WriteLn('== convolver == body resonator (LTI commutation) ==');
  SetLength(imp, TAPS*2); SetLength(cap, TAPS*2);
  SetLength(hL, TAPS); SetLength(hR, TAPS);
  br := TSedaiBodyResonator.Create;
  try
    br.SetSampleRate(TSR); br.LoadBody(bodyViolin); br.Width := 0.6; br.Mix := 1.0; br.Reset;
    for i := 0 to TAPS*2-1 do imp[i] := 0;
    imp[0] := 1.0; imp[1] := 1.0;                 // dual-mono delta
    br.ProcessBlock(@imp[0], @cap[0], TAPS);      // capture the IR
    for i := 0 to TAPS-1 do begin hL[i] := cap[i*2]; hR[i] := cap[i*2+1]; end;

    n := TAPS;
    SetLength(sig, n*2); SetLength(brOut, n*2); SetLength(cvOut, n*2);
    for i := 0 to n*2-1 do sig[i] := 0;
    sig[0]:=0.6; sig[1]:=0.6; sig[8]:=-0.3; sig[9]:=-0.3; sig[20]:=0.2; sig[21]:=0.2;
    br.Reset;
    br.ProcessBlock(@sig[0], @brOut[0], n);

    cv := TSedaiConvolver.Create;
    try
      cv.SetSampleRate(TSR); cv.LoadIR(hL, hR); cv.Reset;
      cv.ProcessBlock(@sig[0], @cvOut[0], n);
    finally cv.Free; end;

    err := 0; pk := 0;
    for i := 0 to (n div 2)*2 - 1 do
    begin
      err := Max(err, Abs(brOut[i] - cvOut[i]));
      if Abs(brOut[i]) > pk then pk := Abs(brOut[i]);
    end;
    Ok('convolve(IR) == live body filter', err < 1e-4, Format('maxdiff=%.2e peak=%.3f', [err, pk]));
  finally
    br.Free;
  end;
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
  TestFLACWriter;
  TestVorbisReader;
  TestMP3Reader;
  TestInstrumentRegistry;
  TestFMParams;
  TestTechniqueParams;
  TestMacros;
  TestPartPolyphony;
  TestEngineMix;
  TestExactPitch;
  TestStereoToMono;
  TestHarmonicTrack;
  TestMicroInstability;
  TestBandwidth;
  TestReedGenerator;
  TestPartialGenerator;
  TestLivingPresetRoundTrip;
  TestPartialPreset;
  TestReedPreset;
  TestAutoSpace;
  TestResidual;
  TestBodyResonator;
  TestSpatialChain;
  TestConvolver;
  TestCommutation;

  WriteLn;
  if Failures = 0 then
    WriteLn('ALL PASS')
  else
    WriteLn(Format('%d FAILURE(S)', [Failures]));
  Halt(Failures);
end.
