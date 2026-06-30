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
  SedaiFMOperator,
  SedaiMixerChannel, SedaiSignalNode, SedaiAudioFileReader, SedaiAudioFileWriter,
  SedaiFLACEncoder, SedaiFLACDecoder;

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
       (reg2.Get(idx).PresetKey = 'epiano') and (reg2.Get(idx).Category = icKeys), '');
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

procedure TestVoiceBudget;
var
  eng: TSAFEngine;
  pa, pb: TSAFPart;
  i, total: Integer;
  buf: array of Single;
  frames: Integer;
begin
  WriteLn('== Global voice budget (shared polyphony) ==');
  frames := 256;
  SetLength(buf, frames * 2);
  eng := TSAFEngine.Create(SR);
  try
    eng.SetGlobalPolyphony(8);
    Ok('global polyphony set', eng.GlobalPolyphony = 8, Format('cap=%d', [eng.GlobalPolyphony]));

    pa := eng.AddPart('A');   // 16-voice pool each, sharing the budget of 8
    pb := eng.AddPart('B');
    pa.SetInstrument(psClassic, 'saw');
    pb.SetInstrument(psClassic, 'saw');

    // One part alone can claim the whole shared budget (B idle): 12 notes
    // requested, capped at 8 active.
    for i := 0 to 11 do pa.NoteOn(48 + i, 1.0);
    Ok('one part uses whole budget', pa.ActiveVoiceCount = 8,
       Format('A active=%d (cap 8)', [pa.ActiveVoiceCount]));
    Ok('budget not exceeded (solo)', eng.TotalActiveVoices = 8,
       Format('total=%d', [eng.TotalActiveVoices]));
    pa.AllSoundOff; pb.AllSoundOff;
    Ok('all-sound-off frees the budget', eng.TotalActiveVoices = 0,
       Format('total=%d', [eng.TotalActiveVoices]));

    // Two parts share the budget: 6 + 6 requested, total capped at 8, both sound.
    for i := 0 to 5 do pa.NoteOn(48 + i, 1.0);
    for i := 0 to 5 do pb.NoteOn(60 + i, 1.0);
    total := eng.TotalActiveVoices;
    Ok('shared budget respected', total = 8, Format('total=%d (cap 8)', [total]));
    Ok('both parts sounding', (pa.ActiveVoiceCount > 0) and (pb.ActiveVoiceCount > 0),
       Format('A=%d B=%d', [pa.ActiveVoiceCount, pb.ActiveVoiceCount]));

    // The mix still renders bounded audio with the cap active.
    FillChar(buf[0], frames * 2 * SizeOf(Single), 0);
    eng.RenderBlock(@buf[0], frames);

    // Raising the budget lets more voices sound at once.
    pa.AllSoundOff; pb.AllSoundOff;
    eng.SetGlobalPolyphony(20);
    for i := 0 to 11 do pa.NoteOn(36 + i, 1.0);
    Ok('raised budget admits more', pa.ActiveVoiceCount = 12,
       Format('A active=%d (cap 20)', [pa.ActiveVoiceCount]));
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
  TestVoiceBudget;
  TestExactPitch;

  WriteLn;
  if Failures = 0 then
    WriteLn('ALL PASS')
  else
    WriteLn(Format('%d FAILURE(S)', [Failures]));
  Halt(Failures);
end.
