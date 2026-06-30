{*
 * Sedai Audio Foundation - Instrument Preset Registry (phase A, consumer side)
 *
 * Composer-facing catalog of instruments. A composer browses by Category and
 * Character (NOT by synthesis technique — that is hidden implementation detail),
 * auditions, and loads an instrument into a TSAFPart.
 *
 * This first slice models a preset as a composer-facing IDENTITY (name /
 * category / character tags / description) plus an internal reference to the
 * synthesis technique + the existing per-engine preset key. Loading it simply
 * calls TSAFPart.SetInstrument(technique, key), so the built-in sounds are the
 * existing ConfigureXxxVoice presets, now catalogued. The full per-technique
 * parameter blocks, authorable macros and .safinst save/load (the sound-designer
 * "author" side) come in later slices — see tmp/SAF_PRESET_DESIGN.md.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0 OR Commercial
 *}
unit SedaiInstrumentPreset;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiPart, SedaiFMOperator,
  SedaiAudioTypes, SedaiVoice, SedaiFilter;   // TWaveformType / TVoiceOscMode / TFilterType for .safinst enum parsing

type
  // Composer-facing role of a sound. The PRIMARY browse axis (with character
  // tags). Spans synthesis techniques: "all Bass" includes FM/classic/karplus.
  TInstrumentCategory = (
    icBass, icLead, icPad, icKeys, icStrings, icBrass, icWind,
    icPlucked, icBells, icOrgan, icDrums, icArp, icFX, icSynth, icOther);

  // One catalogued instrument.
  TInstrumentPreset = record
    Name: string;                 // display name; unique key in the registry
    Category: TInstrumentCategory;
    Tags: string;                 // space-separated character tags (lowercase)
    Description: string;
    Technique: TSAFPartSource;    // internal: which generator
    PresetKey: string;            // internal: key for ConfigureXxxVoice
    Polyphony: Integer;           // suggested voice count for the part (0 = leave as-is)
    Common: TPartCommonOverride;  // optional envelope/filter/level overrides
    // Author side: a full per-technique parameter block overrides the named key
    // for that technique (every parameter editable). Only the block matching the
    // preset's Technique is consulted.
    HasFMParams: Boolean;
    FM: TFMParams;
    HasClassicParams: Boolean;
    Classic: TClassicParams;
    HasWavetableParams: Boolean;
    Wavetable: TWavetableParams;
    HasAdditiveParams: Boolean;
    Additive: TAdditiveParams;
    HasKarplusParams: Boolean;
    Karplus: TKarplusParams;
    Macros: TMacroArray;          // composer quick-controls authored on the preset
  end;

  TIntArray = array of Integer;

  { TSedaiInstrumentRegistry }
  TSedaiInstrumentRegistry = class
  private
    FPresets: array of TInstrumentPreset;
    FCount: Integer;
    procedure RegisterBuiltins;
  public
    constructor Create;                       // seeded with the built-in instruments
    constructor CreateEmpty;                  // no built-ins (e.g. for loading a file)
    procedure Add(const AName: string; ACategory: TInstrumentCategory;
      const ATags: string; ATechnique: TSAFPartSource; const APresetKey: string;
      const ADescription: string = '');
    procedure AddPreset(const APreset: TInstrumentPreset);
    function Count: Integer;
    function Get(AIndex: Integer): TInstrumentPreset;
    function FindByName(const AName: string): Integer;        // -1 if absent
    function ListByCategory(ACategory: TInstrumentCategory): TIntArray;
    function ListByTag(const ATag: string): TIntArray;        // whole-tag match
    // Configure a Part with the catalogued instrument (by index or name).
    function ApplyToPart(AIndex: Integer; APart: TSAFPart): Boolean;
    function ApplyToPartByName(const AName: string; APart: TSAFPart): Boolean;
    // .safinst (versioned text) library save / load.
    procedure SaveToStream(AStream: TStream; const ALibraryName: string);
    function LoadFromStream(AStream: TStream): Integer;       // returns presets added
    procedure SaveToFile(const AFileName, ALibraryName: string);
    function LoadFromFile(const AFileName: string): Integer;
  end;

// Category display label (for browse UIs / debugging).
function InstrumentCategoryName(ACategory: TInstrumentCategory): string;
// A sensible default voice count for a role (overridable per preset / per part).
function DefaultPolyphony(ACategory: TInstrumentCategory): Integer;
// Whether a common-layer override carries any active group.
function HasCommonOverride(const AOverride: TPartCommonOverride): Boolean;

// Lazily-created global registry, seeded with the built-in instruments.
function InstrumentRegistry: TSedaiInstrumentRegistry;

implementation

var
  GRegistry: TSedaiInstrumentRegistry = nil;

// A sensible starting voice count per role: sustained / overlapping-tail
// instruments need more (pads, keys with pedal, bells, plucks), tight
// monophonic-ish roles need fewer. Always overridable per preset / per part.
function DefaultPolyphony(ACategory: TInstrumentCategory): Integer;
begin
  case ACategory of
    icBass:    Result := 6;
    icLead:    Result := 6;
    icPad:     Result := 12;
    icKeys:    Result := 16;   // piano-style release tails overlap a lot
    icStrings: Result := 12;
    icBrass:   Result := 8;
    icWind:    Result := 6;
    icPlucked: Result := 12;   // plucks ring on while new ones start
    icBells:   Result := 12;   // long decays overlap
    icOrgan:   Result := 12;
    icDrums:   Result := 8;
    icArp:     Result := 8;
    icFX:      Result := 6;
    icSynth:   Result := 8;
  else
    Result := 8;
  end;
end;

function InstrumentCategoryName(ACategory: TInstrumentCategory): string;
begin
  case ACategory of
    icBass: Result := 'Bass';
    icLead: Result := 'Lead';
    icPad: Result := 'Pad';
    icKeys: Result := 'Keys';
    icStrings: Result := 'Strings';
    icBrass: Result := 'Brass';
    icWind: Result := 'Wind';
    icPlucked: Result := 'Plucked';
    icBells: Result := 'Bells';
    icOrgan: Result := 'Organ';
    icDrums: Result := 'Drums';
    icArp: Result := 'Arp';
    icFX: Result := 'FX';
    icSynth: Result := 'Synth';
  else
    Result := 'Other';
  end;
end;

function InstrumentRegistry: TSedaiInstrumentRegistry;
begin
  if GRegistry = nil then
    GRegistry := TSedaiInstrumentRegistry.Create;
  Result := GRegistry;
end;

{ TSedaiInstrumentRegistry }

constructor TSedaiInstrumentRegistry.Create;
begin
  inherited Create;
  FCount := 0;
  SetLength(FPresets, 32);
  RegisterBuiltins;
end;

constructor TSedaiInstrumentRegistry.CreateEmpty;
begin
  inherited Create;
  FCount := 0;
  SetLength(FPresets, 32);
end;

function HasCommonOverride(const AOverride: TPartCommonOverride): Boolean;
begin
  Result := AOverride.OverrideEnvelope or AOverride.OverrideFilter or AOverride.OverrideLevel;
end;

procedure TSedaiInstrumentRegistry.Add(const AName: string;
  ACategory: TInstrumentCategory; const ATags: string;
  ATechnique: TSAFPartSource; const APresetKey: string; const ADescription: string);
begin
  if FCount >= Length(FPresets) then
    SetLength(FPresets, Length(FPresets) * 2);
  FPresets[FCount].Name := AName;
  FPresets[FCount].Category := ACategory;
  FPresets[FCount].Tags := LowerCase(ATags);
  FPresets[FCount].Description := ADescription;
  FPresets[FCount].Technique := ATechnique;
  FPresets[FCount].PresetKey := APresetKey;
  FPresets[FCount].Polyphony := DefaultPolyphony(ACategory);   // sensible per-role default
  Inc(FCount);
end;

procedure TSedaiInstrumentRegistry.AddPreset(const APreset: TInstrumentPreset);
begin
  if FCount >= Length(FPresets) then
    SetLength(FPresets, Length(FPresets) * 2);
  FPresets[FCount] := APreset;
  Inc(FCount);
end;

function TSedaiInstrumentRegistry.Count: Integer;
begin
  Result := FCount;
end;

function TSedaiInstrumentRegistry.Get(AIndex: Integer): TInstrumentPreset;
begin
  Result := FPresets[AIndex];
end;

function TSedaiInstrumentRegistry.FindByName(const AName: string): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if SameText(FPresets[i].Name, AName) then Exit(i);
end;

function TSedaiInstrumentRegistry.ListByCategory(ACategory: TInstrumentCategory): TIntArray;
var i, n: Integer;
begin
  SetLength(Result, FCount);
  n := 0;
  for i := 0 to FCount - 1 do
    if FPresets[i].Category = ACategory then begin Result[n] := i; Inc(n); end;
  SetLength(Result, n);
end;

function TSedaiInstrumentRegistry.ListByTag(const ATag: string): TIntArray;
var i, n: Integer; needle: string;
begin
  SetLength(Result, FCount);
  n := 0;
  needle := ' ' + LowerCase(Trim(ATag)) + ' ';
  for i := 0 to FCount - 1 do
    if Pos(needle, ' ' + FPresets[i].Tags + ' ') > 0 then
    begin Result[n] := i; Inc(n); end;
  SetLength(Result, n);
end;

function TSedaiInstrumentRegistry.ApplyToPart(AIndex: Integer; APart: TSAFPart): Boolean;
begin
  Result := False;
  if (AIndex < 0) or (AIndex >= FCount) or (APart = nil) then Exit;
  APart.SetInstrument(FPresets[AIndex].Technique, FPresets[AIndex].PresetKey);
  // Size the part to the instrument's suggested polyphony before the per-voice
  // blocks/macros are applied, so any newly-created voices get configured too.
  if FPresets[AIndex].Polyphony > 0 then
    APart.Polyphony := FPresets[AIndex].Polyphony;
  // Author side: a full per-technique block (if present for the matching
  // technique) replaces the named-key timbre. Clear the others so a reused Part
  // doesn't carry a stale block.
  if (FPresets[AIndex].Technique = psFM) and FPresets[AIndex].HasFMParams then
    APart.SetFMParams(FPresets[AIndex].FM)
  else
    APart.ClearFMParams;
  if (FPresets[AIndex].Technique = psClassic) and FPresets[AIndex].HasClassicParams then
    APart.SetClassicParams(FPresets[AIndex].Classic)
  else
    APart.ClearClassicParams;
  if (FPresets[AIndex].Technique = psWavetable) and FPresets[AIndex].HasWavetableParams then
    APart.SetWavetableParams(FPresets[AIndex].Wavetable)
  else
    APart.ClearWavetableParams;
  if (FPresets[AIndex].Technique = psAdditive) and FPresets[AIndex].HasAdditiveParams then
    APart.SetAdditiveParams(FPresets[AIndex].Additive)
  else
    APart.ClearAdditiveParams;
  if (FPresets[AIndex].Technique = psKarplus) and FPresets[AIndex].HasKarplusParams then
    APart.SetKarplusParams(FPresets[AIndex].Karplus)
  else
    APart.ClearKarplusParams;
  // Composer macros authored on the preset (applied on top; empty -> cleared).
  if Length(FPresets[AIndex].Macros) > 0 then
    APart.SetMacros(FPresets[AIndex].Macros)
  else
    APart.ClearMacros;
  if HasCommonOverride(FPresets[AIndex].Common) then
    APart.SetCommonOverride(FPresets[AIndex].Common)
  else
    APart.ClearCommonOverride;
  Result := True;
end;

// --- name <-> enum helpers for the .safinst text format ---
function CategoryFromName(const AName: string): TInstrumentCategory;
var c: TInstrumentCategory;
begin
  for c := Low(TInstrumentCategory) to High(TInstrumentCategory) do
    if SameText(InstrumentCategoryName(c), AName) then Exit(c);
  Result := icOther;
end;

function TechniqueName(ATechnique: TSAFPartSource): string;
begin
  case ATechnique of
    psFM: Result := 'psFM';
    psWavetable: Result := 'psWavetable';
    psAdditive: Result := 'psAdditive';
    psSample: Result := 'psSample';
    psKarplus: Result := 'psKarplus';
    psSID: Result := 'psSID';
  else
    Result := 'psClassic';
  end;
end;

function TechniqueFromName(const AName: string): TSAFPartSource;
begin
  if SameText(AName, 'psFM') then Result := psFM
  else if SameText(AName, 'psWavetable') then Result := psWavetable
  else if SameText(AName, 'psAdditive') then Result := psAdditive
  else if SameText(AName, 'psSample') then Result := psSample
  else if SameText(AName, 'psKarplus') then Result := psKarplus
  else if SameText(AName, 'psSID') then Result := psSID
  else Result := psClassic;
end;

procedure TSedaiInstrumentRegistry.SaveToStream(AStream: TStream; const ALibraryName: string);
var
  sl: TStringList;
  i, op, mp: Integer;
  p: TInstrumentPreset;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings; fs.DecimalSeparator := '.';
  sl := TStringList.Create;
  try
    sl.Add('; SAF instrument library (.safinst) v1');
    sl.Add('@library ' + ALibraryName);
    for i := 0 to FCount - 1 do
    begin
      p := FPresets[i];
      sl.Add('@preset');
      sl.Add('name=' + p.Name);
      sl.Add('category=' + InstrumentCategoryName(p.Category));
      if p.Tags <> '' then sl.Add('tags=' + p.Tags);
      if p.Description <> '' then sl.Add('desc=' + p.Description);
      sl.Add('technique=' + TechniqueName(p.Technique));
      sl.Add('key=' + p.PresetKey);
      if p.Polyphony > 0 then sl.Add(Format('poly=%d', [p.Polyphony]));
      if p.Common.OverrideEnvelope then
        sl.Add(Format('env=%s,%s,%s,%s',
          [FloatToStr(p.Common.Attack, fs), FloatToStr(p.Common.Decay, fs),
           FloatToStr(p.Common.Sustain, fs), FloatToStr(p.Common.Release, fs)]));
      if p.Common.OverrideFilter then
        sl.Add(Format('filter=%d,%s,%s',
          [Ord(p.Common.FilterEnabled), FloatToStr(p.Common.FilterCutoff, fs),
           FloatToStr(p.Common.FilterResonance, fs)]));
      if p.Common.OverrideLevel then
        sl.Add('level=' + FloatToStr(p.Common.OutputLevel, fs));
      // Author side: full FM parameter block (one line per operator).
      if (p.Technique = psFM) and p.HasFMParams then
      begin
        sl.Add(Format('fmalg=%d', [p.FM.Algorithm]));
        sl.Add('fmfb=' + FloatToStr(p.FM.FeedbackLevel, fs));
        sl.Add('fmtrim=' + FloatToStr(p.FM.OutputTrim, fs));
        for op := 0 to MAX_FM_OPERATORS - 1 do
          sl.Add(Format('fmop=%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%d,%s,%s,%s',
            [op,
             FloatToStr(p.FM.Ops[op].Ratio, fs),
             FloatToStr(p.FM.Ops[op].Level, fs),
             FloatToStr(p.FM.Ops[op].AttackRate, fs),
             FloatToStr(p.FM.Ops[op].Decay1Rate, fs),
             FloatToStr(p.FM.Ops[op].Decay2Rate, fs),
             FloatToStr(p.FM.Ops[op].ReleaseRate, fs),
             FloatToStr(p.FM.Ops[op].SustainLevel, fs),
             FloatToStr(p.FM.Ops[op].AttackLevel, fs),
             FloatToStr(p.FM.Ops[op].Decay1Level, fs),
             Ord(p.FM.Ops[op].Fixed),
             FloatToStr(p.FM.Ops[op].FixedFreq, fs),
             FloatToStr(p.FM.Ops[op].Detune, fs),
             FloatToStr(p.FM.Ops[op].VelocitySensitivity, fs)]));
      end;
      // Author side: full CLASSIC parameter block.
      if (p.Technique = psClassic) and p.HasClassicParams then
      begin
        sl.Add(Format('clsmode=%d', [Ord(p.Classic.OscMode)]));
        for op := 0 to High(p.Classic.Oscs) do
          sl.Add(Format('clsosc=%d,%d,%d,%s,%s,%s',
            [op, Ord(p.Classic.Oscs[op].Enabled), Ord(p.Classic.Oscs[op].Waveform),
             FloatToStr(p.Classic.Oscs[op].Level, fs),
             FloatToStr(p.Classic.Oscs[op].DetuneCents, fs),
             FloatToStr(p.Classic.Oscs[op].PulseWidth, fs)]));
        sl.Add(Format('clsflt=%d,%d,%s,%s',
          [Ord(p.Classic.FilterEnabled), Ord(p.Classic.FilterType),
           FloatToStr(p.Classic.FilterCutoff, fs), FloatToStr(p.Classic.FilterResonance, fs)]));
        sl.Add(Format('clsenv=%s,%s,%s,%s',
          [FloatToStr(p.Classic.Attack, fs), FloatToStr(p.Classic.Decay, fs),
           FloatToStr(p.Classic.Sustain, fs), FloatToStr(p.Classic.Release, fs)]));
        sl.Add('clstrim=' + FloatToStr(p.Classic.OutputTrim, fs));
      end;
      // Author side: full WAVETABLE parameter block.
      if (p.Technique = psWavetable) and p.HasWavetableParams then
      begin
        sl.Add(Format('wtkind=%d', [Ord(p.Wavetable.Kind)]));
        sl.Add(Format('wtsteps=%d', [p.Wavetable.Steps]));
        sl.Add(Format('wtuni=%d,%s,%s',
          [p.Wavetable.UnisonVoices, FloatToStr(p.Wavetable.UnisonDetune, fs),
           FloatToStr(p.Wavetable.UnisonSpread, fs)]));
        sl.Add(Format('wtenv=%s,%s,%s,%s',
          [FloatToStr(p.Wavetable.Attack, fs), FloatToStr(p.Wavetable.Decay, fs),
           FloatToStr(p.Wavetable.Sustain, fs), FloatToStr(p.Wavetable.Release, fs)]));
        sl.Add('wttrim=' + FloatToStr(p.Wavetable.OutputTrim, fs));
      end;
      // Author side: full ADDITIVE parameter block (only non-zero harmonics).
      if (p.Technique = psAdditive) and p.HasAdditiveParams then
      begin
        sl.Add(Format('addcount=%d', [p.Additive.HarmonicCount]));
        for op := 0 to High(p.Additive.Levels) do
          if (p.Additive.Levels[op] <> 0) or (p.Additive.Detunes[op] <> 0) then
            sl.Add(Format('addh=%d,%s,%s',
              [op, FloatToStr(p.Additive.Levels[op], fs),
               FloatToStr(p.Additive.Detunes[op], fs)]));
        sl.Add(Format('addenv=%s,%s,%s,%s',
          [FloatToStr(p.Additive.Attack, fs), FloatToStr(p.Additive.Decay, fs),
           FloatToStr(p.Additive.Sustain, fs), FloatToStr(p.Additive.Release, fs)]));
        sl.Add('addtrim=' + FloatToStr(p.Additive.OutputTrim, fs));
      end;
      // Author side: full KARPLUS parameter block.
      if (p.Technique = psKarplus) and p.HasKarplusParams then
      begin
        sl.Add('ksdamp=' + FloatToStr(p.Karplus.Damping, fs));
        sl.Add('ksblend=' + FloatToStr(p.Karplus.Blend, fs));
        sl.Add(Format('ksenv=%s,%s,%s,%s',
          [FloatToStr(p.Karplus.Attack, fs), FloatToStr(p.Karplus.Decay, fs),
           FloatToStr(p.Karplus.Sustain, fs), FloatToStr(p.Karplus.Release, fs)]));
        sl.Add('kstrim=' + FloatToStr(p.Karplus.OutputTrim, fs));
      end;
      // Composer macros: a 'macro=' header followed by its 'map=' mappings.
      for op := 0 to High(p.Macros) do
      begin
        sl.Add(Format('macro=%s,%s', [p.Macros[op].Name, FloatToStr(p.Macros[op].Value, fs)]));
        for mp := 0 to High(p.Macros[op].Mappings) do
          sl.Add(Format('map=%d,%s,%s,%d',
            [Ord(p.Macros[op].Mappings[mp].Dest),
             FloatToStr(p.Macros[op].Mappings[mp].MinVal, fs),
             FloatToStr(p.Macros[op].Mappings[mp].MaxVal, fs),
             Ord(p.Macros[op].Mappings[mp].Curve)]));
      end;
    end;
    sl.SaveToStream(AStream);
  finally
    sl.Free;
  end;
end;

function TSedaiInstrumentRegistry.LoadFromStream(AStream: TStream): Integer;
var
  sl: TStringList;
  i, eq, n, opIdx, mi, mj: Integer;
  line, k, v, rest: string;
  cur: TInstrumentPreset;
  have: Boolean;
  fs: TFormatSettings;

  function NextTok: string;
  var pc: Integer;
  begin
    pc := Pos(',', rest);
    if pc = 0 then begin Result := Trim(rest); rest := ''; end
    else begin Result := Trim(Copy(rest, 1, pc - 1)); rest := Copy(rest, pc + 1, MaxInt); end;
  end;

  procedure Flush;
  begin
    if have and (cur.Name <> '') then begin AddPreset(cur); Inc(n); end;
    have := False;
  end;

begin
  fs := DefaultFormatSettings; fs.DecimalSeparator := '.';
  n := 0; have := False;
  cur := Default(TInstrumentPreset);
  sl := TStringList.Create;
  try
    sl.LoadFromStream(AStream);
    for i := 0 to sl.Count - 1 do
    begin
      line := Trim(sl[i]);
      if (line = '') or (line[1] = ';') then Continue;
      if line[1] = '@' then
      begin
        if SameText(Copy(line, 1, 7), '@preset') then
        begin
          Flush;
          cur := Default(TInstrumentPreset);
          cur.Category := icOther;
          cur.Technique := psClassic;
          have := True;
        end;
        Continue;   // @library and any other directive = metadata, ignored
      end;
      eq := Pos('=', line);
      if (eq = 0) or (not have) then Continue;
      k := LowerCase(Trim(Copy(line, 1, eq - 1)));
      v := Trim(Copy(line, eq + 1, MaxInt));
      if k = 'name' then cur.Name := v
      else if k = 'category' then cur.Category := CategoryFromName(v)
      else if k = 'tags' then cur.Tags := LowerCase(v)
      else if k = 'desc' then cur.Description := v
      else if k = 'technique' then cur.Technique := TechniqueFromName(v)
      else if k = 'key' then cur.PresetKey := v
      else if k = 'poly' then cur.Polyphony := StrToIntDef(v, 0)
      else if k = 'env' then
      begin
        rest := v;
        cur.Common.OverrideEnvelope := True;
        cur.Common.Attack := StrToFloatDef(NextTok, 0, fs);
        cur.Common.Decay := StrToFloatDef(NextTok, 0, fs);
        cur.Common.Sustain := StrToFloatDef(NextTok, 1, fs);
        cur.Common.Release := StrToFloatDef(NextTok, 0, fs);
      end
      else if k = 'filter' then
      begin
        rest := v;
        cur.Common.OverrideFilter := True;
        cur.Common.FilterEnabled := StrToIntDef(NextTok, 1) <> 0;
        cur.Common.FilterCutoff := StrToFloatDef(NextTok, 2500, fs);
        cur.Common.FilterResonance := StrToFloatDef(NextTok, 0.2, fs);
      end
      else if k = 'level' then
      begin
        cur.Common.OverrideLevel := True;
        cur.Common.OutputLevel := StrToFloatDef(v, 1.0, fs);
      end
      else if k = 'fmalg' then
      begin
        cur.HasFMParams := True;
        cur.FM.Algorithm := StrToIntDef(v, 1);
      end
      else if k = 'fmfb' then
      begin
        cur.HasFMParams := True;
        cur.FM.FeedbackLevel := StrToFloatDef(v, 0, fs);
      end
      else if k = 'fmtrim' then
      begin
        cur.HasFMParams := True;
        cur.FM.OutputTrim := StrToFloatDef(v, 1.0, fs);
      end
      else if k = 'fmop' then
      begin
        cur.HasFMParams := True;
        rest := v;
        opIdx := StrToIntDef(NextTok, 0);
        if (opIdx >= 0) and (opIdx < MAX_FM_OPERATORS) then
        begin
          cur.FM.Ops[opIdx].Ratio := StrToFloatDef(NextTok, 1.0, fs);
          cur.FM.Ops[opIdx].Level := StrToFloatDef(NextTok, 0, fs);
          cur.FM.Ops[opIdx].AttackRate := StrToFloatDef(NextTok, 99, fs);
          cur.FM.Ops[opIdx].Decay1Rate := StrToFloatDef(NextTok, 0, fs);
          cur.FM.Ops[opIdx].Decay2Rate := StrToFloatDef(NextTok, 0, fs);
          cur.FM.Ops[opIdx].ReleaseRate := StrToFloatDef(NextTok, 0, fs);
          cur.FM.Ops[opIdx].SustainLevel := StrToFloatDef(NextTok, 1.0, fs);
          cur.FM.Ops[opIdx].AttackLevel := StrToFloatDef(NextTok, 1.0, fs);
          cur.FM.Ops[opIdx].Decay1Level := StrToFloatDef(NextTok, 1.0, fs);
          cur.FM.Ops[opIdx].Fixed := StrToIntDef(NextTok, 0) <> 0;
          cur.FM.Ops[opIdx].FixedFreq := StrToFloatDef(NextTok, 0, fs);
          cur.FM.Ops[opIdx].Detune := StrToFloatDef(NextTok, 0, fs);
          cur.FM.Ops[opIdx].VelocitySensitivity := StrToFloatDef(NextTok, 0, fs);
        end;
      end
      // --- CLASSIC block ---
      else if k = 'clsmode' then
      begin
        cur.HasClassicParams := True;
        cur.Classic.OscMode := TVoiceOscMode(StrToIntDef(v, 0));
      end
      else if k = 'clsosc' then
      begin
        cur.HasClassicParams := True;
        rest := v;
        opIdx := StrToIntDef(NextTok, 0);
        if (opIdx >= 0) and (opIdx <= High(cur.Classic.Oscs)) then
        begin
          cur.Classic.Oscs[opIdx].Enabled := StrToIntDef(NextTok, 0) <> 0;
          cur.Classic.Oscs[opIdx].Waveform := TWaveformType(StrToIntDef(NextTok, 0));
          cur.Classic.Oscs[opIdx].Level := StrToFloatDef(NextTok, 0, fs);
          cur.Classic.Oscs[opIdx].DetuneCents := StrToFloatDef(NextTok, 0, fs);
          cur.Classic.Oscs[opIdx].PulseWidth := StrToFloatDef(NextTok, 0.5, fs);
        end;
      end
      else if k = 'clsflt' then
      begin
        cur.HasClassicParams := True;
        rest := v;
        cur.Classic.FilterEnabled := StrToIntDef(NextTok, 1) <> 0;
        cur.Classic.FilterType := TFilterType(StrToIntDef(NextTok, 0));
        cur.Classic.FilterCutoff := StrToFloatDef(NextTok, 2500, fs);
        cur.Classic.FilterResonance := StrToFloatDef(NextTok, 0.2, fs);
      end
      else if k = 'clsenv' then
      begin
        cur.HasClassicParams := True;
        rest := v;
        cur.Classic.Attack := StrToFloatDef(NextTok, 0, fs);
        cur.Classic.Decay := StrToFloatDef(NextTok, 0, fs);
        cur.Classic.Sustain := StrToFloatDef(NextTok, 1, fs);
        cur.Classic.Release := StrToFloatDef(NextTok, 0, fs);
      end
      else if k = 'clstrim' then
      begin
        cur.HasClassicParams := True;
        cur.Classic.OutputTrim := StrToFloatDef(v, 1.0, fs);
      end
      // --- WAVETABLE block ---
      else if k = 'wtkind' then
      begin
        cur.HasWavetableParams := True;
        cur.Wavetable.Kind := TWavetableKind(StrToIntDef(v, 0));
      end
      else if k = 'wtsteps' then
      begin
        cur.HasWavetableParams := True;
        cur.Wavetable.Steps := StrToIntDef(v, 0);
      end
      else if k = 'wtuni' then
      begin
        cur.HasWavetableParams := True;
        rest := v;
        cur.Wavetable.UnisonVoices := StrToIntDef(NextTok, 1);
        cur.Wavetable.UnisonDetune := StrToFloatDef(NextTok, 0, fs);
        cur.Wavetable.UnisonSpread := StrToFloatDef(NextTok, 0, fs);
      end
      else if k = 'wtenv' then
      begin
        cur.HasWavetableParams := True;
        rest := v;
        cur.Wavetable.Attack := StrToFloatDef(NextTok, 0, fs);
        cur.Wavetable.Decay := StrToFloatDef(NextTok, 0, fs);
        cur.Wavetable.Sustain := StrToFloatDef(NextTok, 1, fs);
        cur.Wavetable.Release := StrToFloatDef(NextTok, 0, fs);
      end
      else if k = 'wttrim' then
      begin
        cur.HasWavetableParams := True;
        cur.Wavetable.OutputTrim := StrToFloatDef(v, 1.0, fs);
      end
      // --- ADDITIVE block ---
      else if k = 'addcount' then
      begin
        cur.HasAdditiveParams := True;
        cur.Additive.HarmonicCount := StrToIntDef(v, 1);
      end
      else if k = 'addh' then
      begin
        cur.HasAdditiveParams := True;
        rest := v;
        opIdx := StrToIntDef(NextTok, 0);
        if (opIdx >= 0) and (opIdx <= High(cur.Additive.Levels)) then
        begin
          cur.Additive.Levels[opIdx] := StrToFloatDef(NextTok, 0, fs);
          cur.Additive.Detunes[opIdx] := StrToFloatDef(NextTok, 0, fs);
        end;
      end
      else if k = 'addenv' then
      begin
        cur.HasAdditiveParams := True;
        rest := v;
        cur.Additive.Attack := StrToFloatDef(NextTok, 0, fs);
        cur.Additive.Decay := StrToFloatDef(NextTok, 0, fs);
        cur.Additive.Sustain := StrToFloatDef(NextTok, 1, fs);
        cur.Additive.Release := StrToFloatDef(NextTok, 0, fs);
      end
      else if k = 'addtrim' then
      begin
        cur.HasAdditiveParams := True;
        cur.Additive.OutputTrim := StrToFloatDef(v, 1.0, fs);
      end
      // --- KARPLUS block ---
      else if k = 'ksdamp' then
      begin
        cur.HasKarplusParams := True;
        cur.Karplus.Damping := StrToFloatDef(v, 0.996, fs);
      end
      else if k = 'ksblend' then
      begin
        cur.HasKarplusParams := True;
        cur.Karplus.Blend := StrToFloatDef(v, 0.5, fs);
      end
      else if k = 'ksenv' then
      begin
        cur.HasKarplusParams := True;
        rest := v;
        cur.Karplus.Attack := StrToFloatDef(NextTok, 0, fs);
        cur.Karplus.Decay := StrToFloatDef(NextTok, 0, fs);
        cur.Karplus.Sustain := StrToFloatDef(NextTok, 1, fs);
        cur.Karplus.Release := StrToFloatDef(NextTok, 0, fs);
      end
      else if k = 'kstrim' then
      begin
        cur.HasKarplusParams := True;
        cur.Karplus.OutputTrim := StrToFloatDef(v, 1.0, fs);
      end
      // --- MACROS ---
      else if k = 'macro' then
      begin
        rest := v;
        SetLength(cur.Macros, Length(cur.Macros) + 1);
        cur.Macros[High(cur.Macros)].Name := NextTok;
        cur.Macros[High(cur.Macros)].Value := StrToFloatDef(NextTok, 0, fs);
      end
      else if (k = 'map') and (Length(cur.Macros) > 0) then
      begin
        rest := v;
        mi := High(cur.Macros);
        mj := Length(cur.Macros[mi].Mappings);
        SetLength(cur.Macros[mi].Mappings, mj + 1);
        cur.Macros[mi].Mappings[mj].Dest := TMacroDest(StrToIntDef(NextTok, 0));
        cur.Macros[mi].Mappings[mj].MinVal := StrToFloatDef(NextTok, 0, fs);
        cur.Macros[mi].Mappings[mj].MaxVal := StrToFloatDef(NextTok, 1, fs);
        cur.Macros[mi].Mappings[mj].Curve := TMacroCurve(StrToIntDef(NextTok, 0));
      end;
    end;
    Flush;
  finally
    sl.Free;
  end;
  Result := n;
end;

procedure TSedaiInstrumentRegistry.SaveToFile(const AFileName, ALibraryName: string);
var fsr: TFileStream;
begin
  fsr := TFileStream.Create(AFileName, fmCreate);
  try SaveToStream(fsr, ALibraryName); finally fsr.Free; end;
end;

function TSedaiInstrumentRegistry.LoadFromFile(const AFileName: string): Integer;
var fsr: TFileStream;
begin
  fsr := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try Result := LoadFromStream(fsr); finally fsr.Free; end;
end;

function TSedaiInstrumentRegistry.ApplyToPartByName(const AName: string; APart: TSAFPart): Boolean;
begin
  Result := ApplyToPart(FindByName(AName), APart);
end;

procedure TSedaiInstrumentRegistry.RegisterBuiltins;
begin
  // Seed = the existing ConfigureXxxVoice presets, now catalogued by category +
  // character. Sounds are unchanged; only the browse metadata is new.

  // --- Classic / subtractive (psClassic) ---
  Add('Sine',          icSynth, 'soft clean sine',     psClassic, 'sine');
  Add('Square',        icSynth, 'hollow retro square',  psClassic, 'square');
  Add('Saw',           icSynth, 'bright classic saw',   psClassic, 'saw');
  Add('Triangle',      icSynth, 'soft mellow triangle', psClassic, 'triangle');
  Add('Pulse',         icSynth, 'thin retro pulse',     psClassic, 'pulse');
  Add('Noise',         icFX,    'noise percussive',     psClassic, 'noise');
  Add('Classic Lead',  icLead,  'bright lead',          psClassic, 'lead');
  Add('Classic Bass',  icBass,  'warm sub bass',        psClassic, 'bass');
  Add('Classic Pad',   icPad,   'warm soft evolving',   psClassic, 'pad');

  // --- FM (psFM) ---
  Add('FM E-Piano',    icKeys,  'warm electric',        psFM, 'epiano');
  Add('FM Brass',      icBrass, 'bright',               psFM, 'brass');
  Add('FM Bell',       icBells, 'metallic bright',      psFM, 'bell');
  Add('FM Organ',      icOrgan, 'clean',                psFM, 'organ');
  Add('FM Bass',       icBass,  'punchy',               psFM, 'bass');

  // --- Wavetable (psWavetable) ---
  Add('PWM',           icLead,  'moving hollow',        psWavetable, 'pwm');
  Add('SuperSaw',      icLead,  'wide big bright',      psWavetable, 'supersaw');

  // --- Additive (psAdditive) ---
  Add('Drawbar Organ', icOrgan, 'clean drawbar',        psAdditive, 'organ');
  Add('Glass Bell',    icBells, 'metallic clean',       psAdditive, 'bell');
  Add('Strings',       icStrings, 'warm ensemble',      psAdditive, 'strings');
  Add('Choir',         icPad,   'vocal warm evolving',  psAdditive, 'choir');
  Add('Brass Section', icBrass, 'bright section',       psAdditive, 'brass');
  Add('Flute',         icWind,  'soft breathy',         psAdditive, 'flute');

  // --- Karplus-Strong / plucked (psKarplus) ---
  Add('Guitar',        icPlucked, 'plucked bright',     psKarplus, 'guitar');
  Add('Plucked Bass',  icBass,  'plucked round',        psKarplus, 'bass');
  Add('Harp',          icPlucked, 'bright singing',     psKarplus, 'harp');
  Add('Muted Pluck',   icPlucked, 'short staccato',     psKarplus, 'mute');
  Add('Karplus Drum',  icDrums, 'percussive',           psKarplus, 'drum');

  // --- SID-flavoured oscillator (psSID) ---
  Add('SID Lead',      icLead,  'bright retro chip',    psSID, 'lead');
  Add('SID Pulse',     icLead,  'thin retro chip',      psSID, 'pulse');
  Add('SID Bass',      icBass,  'punchy retro chip',    psSID, 'bass');
  Add('SID Triangle',  icSynth, 'soft retro chip',      psSID, 'triangle');
  Add('SID Noise',     icDrums, 'noise retro chip',     psSID, 'noise');
end;

initialization

finalization
  FreeAndNil(GRegistry);

end.
