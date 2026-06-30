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
  Classes, SysUtils, SedaiPart, SedaiFMOperator;

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
    Common: TPartCommonOverride;  // optional envelope/filter/level overrides
    HasFMParams: Boolean;         // author side: full FM block overrides the key
    FM: TFMParams;                // every operator editable (when HasFMParams)
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
// Whether a common-layer override carries any active group.
function HasCommonOverride(const AOverride: TPartCommonOverride): Boolean;

// Lazily-created global registry, seeded with the built-in instruments.
function InstrumentRegistry: TSedaiInstrumentRegistry;

implementation

var
  GRegistry: TSedaiInstrumentRegistry = nil;

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
  // Author side: a full FM block (if present) replaces the named-key timbre.
  if (FPresets[AIndex].Technique = psFM) and FPresets[AIndex].HasFMParams then
    APart.SetFMParams(FPresets[AIndex].FM)
  else
    APart.ClearFMParams;
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
  else Result := psClassic;
end;

procedure TSedaiInstrumentRegistry.SaveToStream(AStream: TStream; const ALibraryName: string);
var
  sl: TStringList;
  i, op: Integer;
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
    end;
    sl.SaveToStream(AStream);
  finally
    sl.Free;
  end;
end;

function TSedaiInstrumentRegistry.LoadFromStream(AStream: TStream): Integer;
var
  sl: TStringList;
  i, eq, n, opIdx: Integer;
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
end;

initialization

finalization
  FreeAndNil(GRegistry);

end.
