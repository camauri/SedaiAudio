{*
 * Sedai Audio Foundation - "hello sound" entry-point demo.
 *
 * Shows the real usage flow with no audio device (renders offline to a WAV):
 *   1. load a shipped instrument library (library/*.safinst)
 *   2. apply an instrument to a TSAFPart
 *   3. play a short phrase (note on/off over time)
 *   4. write the result to a .wav
 *
 * No args  -> a "tour": one phrase from each of the three shipped libraries
 *             (builtin / winds / vcsl), concatenated into saf_play.wav, and the
 *             full catalogue printed.
 * Args     -> saf_play <library.safinst> <instrument name> [out.wav]
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
program saf_play;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math,
  SedaiAudioTypes, SedaiAudioBuffer, SedaiAudioFileWriter,
  SedaiPart, SedaiInstrumentPreset;

const
  SR    = 48000;
  BLOCK = 512;

type
  TNoteEvent = record onSample, offSample: Integer; note: Byte; end;
  TNoteEventArr = array of TNoteEvent;
  TFloatArr  = array of Single;

// Load a .safinst library file into a fresh registry.
function LoadLibrary(const APath: string): TSedaiInstrumentRegistry;
var fs: TFileStream;
begin
  Result := TSedaiInstrumentRegistry.CreateEmpty;
  fs := TFileStream.Create(APath, fmOpenRead);
  try Result.LoadFromStream(fs); finally fs.Free; end;
end;

// A short phrase: a C-major arpeggio up, then a held C-major triad.
function MakePhrase(out ATotal: Integer): TNoteEventArr;
const
  ARP: array[0..3] of Byte = (60, 64, 67, 72);   // C4 E4 G4 C5
var
  i, t, step: Integer;
begin
  SetLength(Result, 4 + 3);
  step := Round(0.35 * SR);
  for i := 0 to 3 do                              // arpeggio, each note 0.4 s
  begin
    Result[i].onSample := i * step;
    Result[i].offSample := i * step + Round(0.40 * SR);
    Result[i].note := ARP[i];
  end;
  t := 4 * step + Round(0.15 * SR);               // held triad after the arpeggio
  for i := 0 to 2 do
  begin
    Result[4 + i].onSample := t;
    Result[4 + i].offSample := t + Round(1.6 * SR);
    Result[4 + i].note := ARP[i];
  end;
  ATotal := t + Round(2.1 * SR);                  // + release tail
end;

// Render one instrument playing the phrase, appended (stereo) to AMaster.
procedure RenderInstrument(AReg: TSedaiInstrumentRegistry; const AName: string;
  var AMaster: TFloatArr);
var
  part: TSAFPart;
  phrase: TNoteEventArr;
  blk: array of Single;
  total, pos, i, k, base: Integer;
begin
  if AReg.FindByName(AName) < 0 then
  begin
    WriteLn('  (skipped: "', AName, '" not in this library)');
    Exit;
  end;
  phrase := MakePhrase(total);
  part := TSAFPart.Create;
  try
    part.SetSampleRate(SR);
    AReg.ApplyToPartByName(AName, part);
    part.SetPolyphony(6);                          // allow the triad to overlap
    SetLength(blk, BLOCK * 2);
    base := Length(AMaster);
    SetLength(AMaster, base + total * 2);
    pos := 0;
    while pos < total do
    begin
      // fire note-on / note-off events that fall in this block
      for i := 0 to High(phrase) do
      begin
        if (phrase[i].onSample >= pos) and (phrase[i].onSample < pos + BLOCK) then
          part.NoteOn(phrase[i].note, 0.9);
        if (phrase[i].offSample >= pos) and (phrase[i].offSample < pos + BLOCK) then
          part.NoteOff(phrase[i].note);
      end;
      FillChar(blk[0], BLOCK * 2 * SizeOf(Single), 0);
      part.RenderBlock(@blk[0], BLOCK);
      for k := 0 to BLOCK * 2 - 1 do
        if pos * 2 + k < total * 2 then
          AMaster[base + pos * 2 + k] := blk[k];
      pos := pos + BLOCK;
    end;
  finally
    part.Free;
  end;
  WriteLn('  played "', AName, '"');
end;

function TechStr(AT: TSAFPartSource): string;
begin
  case AT of
    psFM: Result := 'FM'; psWavetable: Result := 'wavetable';
    psAdditive: Result := 'additive'; psSample: Result := 'sample';
    psKarplus: Result := 'karplus'; psSID: Result := 'SID';
    psPartial: Result := 'partial'; psReed: Result := 'reed';
  else Result := 'classic'; end;
end;

procedure PrintCatalogue(AReg: TSedaiInstrumentRegistry; const ALib: string);
var i: Integer; p: TInstrumentPreset;
begin
  WriteLn('--- ', ALib, ' (', AReg.Count, ' instruments) ---');
  for i := 0 to AReg.Count - 1 do
  begin
    p := AReg.Get(i);
    WriteLn(Format('    %-16s [%s]', [p.Name, TechStr(p.Technique)]));
  end;
end;

procedure WriteWav(const AMaster: TFloatArr; const APath: string);
var
  frames, i: Integer;
  pk: Single;
  buf: TFloatArr;
  stereo: TSedaiAudioBuffer;
  wr: TSedaiAudioFileWriter;
  st: TAudioExportSettings;
begin
  frames := Length(AMaster) div 2;
  buf := Copy(AMaster);
  pk := 0; for i := 0 to High(buf) do if Abs(buf[i]) > pk then pk := Abs(buf[i]);
  if pk > 1.0 then for i := 0 to High(buf) do buf[i] := buf[i] / pk;   // clip-guard
  stereo := TSedaiAudioBuffer.Create;
  stereo.Allocate(2, frames); stereo.SetFormat(SR, 2);
  stereo.WriteInterleaved(@buf[0], 0, frames);
  st := TSedaiAudioFileWriter.GetDefaultSettings(aefWAV24);
  st.SampleRate := SR; st.Channels := 2; st.DitherType := dtNone;
  wr := TSedaiAudioFileWriter.Create;
  try
    if wr.CreateFile(APath, st) and wr.WriteBuffer(stereo) then
      WriteLn('wrote ', APath, Format('  (%.1f s)', [frames / SR]));
    wr.Close;
  finally wr.Free; end;
  stereo.Free;
end;

var
  reg: TSedaiInstrumentRegistry;
  master: TFloatArr;
  outName: string;
begin
  WriteLn('Sedai Audio Foundation - play demo');
  WriteLn('==================================');
  SetLength(master, 0);

  if ParamCount >= 2 then
  begin
    // targeted: saf_play <library.safinst> <instrument> [out.wav]
    reg := LoadLibrary(ParamStr(1));
    try
      PrintCatalogue(reg, ExtractFileName(ParamStr(1)));
      if ParamCount >= 3 then outName := ParamStr(3) else outName := 'saf_play.wav';
      RenderInstrument(reg, ParamStr(2), master);
    finally reg.Free; end;
    if Length(master) > 0 then WriteWav(master, outName);
    Halt(0);
  end;

  // default: a tour of the three shipped libraries -> saf_play.wav
  reg := LoadLibrary('library/builtin.safinst');
  try PrintCatalogue(reg, 'builtin.safinst'); RenderInstrument(reg, 'FM E-Piano', master); finally reg.Free; end;
  reg := LoadLibrary('library/winds.safinst');
  try PrintCatalogue(reg, 'winds.safinst'); RenderInstrument(reg, 'Alto Sax', master); finally reg.Free; end;
  reg := LoadLibrary('library/vcsl.safinst');
  try PrintCatalogue(reg, 'vcsl.safinst'); RenderInstrument(reg, 'Saxello', master); finally reg.Free; end;

  WriteWav(master, 'saf_play.wav');
  WriteLn('done.');
end.
