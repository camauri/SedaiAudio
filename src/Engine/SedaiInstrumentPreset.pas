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
  Classes, SysUtils, SedaiPart;

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
  end;

  TIntArray = array of Integer;

  { TSedaiInstrumentRegistry }
  TSedaiInstrumentRegistry = class
  private
    FPresets: array of TInstrumentPreset;
    FCount: Integer;
    procedure RegisterBuiltins;
  public
    constructor Create;
    procedure Add(const AName: string; ACategory: TInstrumentCategory;
      const ATags: string; ATechnique: TSAFPartSource; const APresetKey: string;
      const ADescription: string = '');
    function Count: Integer;
    function Get(AIndex: Integer): TInstrumentPreset;
    function FindByName(const AName: string): Integer;        // -1 if absent
    function ListByCategory(ACategory: TInstrumentCategory): TIntArray;
    function ListByTag(const ATag: string): TIntArray;        // whole-tag match
    // Configure a Part with the catalogued instrument (by index or name).
    function ApplyToPart(AIndex: Integer; APart: TSAFPart): Boolean;
    function ApplyToPartByName(const AName: string; APart: TSAFPart): Boolean;
  end;

// Category display label (for browse UIs / debugging).
function InstrumentCategoryName(ACategory: TInstrumentCategory): string;

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
  Result := True;
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
