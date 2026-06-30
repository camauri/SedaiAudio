{*
 * Sedai Audio Foundation - Part / Instrument
 *
 * TSAFPart is a single monotimbral instrument: a pool of universal voices
 * (TSedaiVoice = oscillators / FM / wavetable) managed by a TSedaiVoiceManager,
 * configured from a simple preset, and rendered to a stereo-interleaved buffer.
 *
 * In the Part/Instrument architecture the Part owns monotimbrality (one
 * instrument for its whole voice pool, as TSedaiVoiceManager is designed);
 * multitimbrality lives one level up in TSAFEngine, which hosts many Parts and
 * routes each to its own mixer channel.
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiPart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioBuffer,
  SedaiOscillator, SedaiFilter, SedaiFMOperator, SedaiWavetableGenerator,
  SedaiAdditiveGenerator, SedaiSamplePlayer, SedaiKarplusGenerator,
  SedaiEnvelope, SedaiVoice, SedaiVoiceManager, SedaiModulationMatrix;

type
  // Which generator the part's voices use. Mirrors TVoiceSourceType but is the
  // public, instrument-level selector exposed by the Part.
  TSAFPartSource = (psClassic, psFM, psWavetable, psAdditive, psSample, psKarplus);

  // Optional "common layer" overrides applied on top of the preset's own values
  // (envelope / filter / output level). Each Override* flag gates one group; all
  // False (the default) leaves the preset untouched. Used by the instrument
  // preset system so authored patches can tweak these without a new preset key.
  TPartCommonOverride = record
    OverrideEnvelope: Boolean;
    Attack, Decay, Sustain, Release: Single;
    OverrideFilter: Boolean;
    FilterEnabled: Boolean;
    FilterCutoff, FilterResonance: Single;
    OverrideLevel: Boolean;
    OutputLevel: Single;
  end;

  // Full FM parameter block — the sound-designer "author" view of an FM patch
  // (every operator). When a preset carries one, it fully configures the FM
  // voice instead of the named ConfigureFMVoice key.
  TFMOperatorParams = record
    Ratio: Double;
    Fixed: Boolean;
    FixedFreq: Single;
    Detune: Single;
    Level: Single;
    VelocitySensitivity: Single;
    AttackRate, Decay1Rate, Decay2Rate, ReleaseRate: Single;
    AttackLevel, Decay1Level, SustainLevel: Single;
  end;

  TFMParams = record
    Algorithm: Integer;        // 1..32
    FeedbackLevel: Single;
    OutputTrim: Single;
    Ops: array[0..MAX_FM_OPERATORS-1] of TFMOperatorParams;
  end;

  // Full CLASSIC (subtractive) parameter block — the author view of an analog
  // voice: 3 oscillators (combine mode + per-osc enable/waveform/level/detune/
  // pulse-width), the filter, and the amp envelope.
  TClassicOscParams = record
    Enabled: Boolean;
    Waveform: TWaveformType;
    Level: Single;
    DetuneCents: Single;
    PulseWidth: Single;
  end;
  TClassicParams = record
    OscMode: TVoiceOscMode;
    Oscs: array[0..MAX_OSCILLATORS-1] of TClassicOscParams;
    FilterEnabled: Boolean;
    FilterType: TFilterType;
    FilterCutoff: Single;
    FilterResonance: Single;
    Attack, Decay, Sustain, Release: Single;
    OutputTrim: Single;
  end;

  // Full WAVETABLE parameter block. The table itself is procedurally generated
  // (basic / PWM / SuperSaw), so the block records the kind + step count rather
  // than the raw samples, plus the unison spread and amp envelope.
  TWavetableKind = (wtkBasic, wtkPWM, wtkSuperSaw);
  TWavetableParams = record
    Kind: TWavetableKind;
    Steps: Integer;            // PWM/SuperSaw resolution (ignored for basic)
    UnisonVoices: Integer;
    UnisonDetune: Single;
    UnisonSpread: Single;
    Attack, Decay, Sustain, Release: Single;
    OutputTrim: Single;
  end;

  // Full ADDITIVE parameter block — per-harmonic level + detune (the generator
  // resets phases on note-on, so level+detune fully describe the timbre) plus
  // the additive generator's own amp envelope.
  TAdditiveParams = record
    HarmonicCount: Integer;
    Levels: array[0..ADDITIVE_MAX_HARMONICS-1] of Single;
    Detunes: array[0..ADDITIVE_MAX_HARMONICS-1] of Single;   // cents
    Attack, Decay, Sustain, Release: Single;
    OutputTrim: Single;
  end;

  // Full KARPLUS-STRONG parameter block — string damping/blend + the gating
  // amp envelope.
  TKarplusParams = record
    Damping: Single;
    Blend: Single;
    Attack, Decay, Sustain, Release: Single;
    OutputTrim: Single;
  end;

  // Stored per-Part modulation. Re-applied to every voice of the pool in
  // ApplyToVoice, so the whole instrument shares the same modulation and it
  // survives a preset reconfigure. LFOs are unlimited (FPartLFOs grows freely).
  TPartLFOConfig = record
    Rate: Single;
    Waveform: TWaveformType;
  end;
  TPartModRouting = record
    SourceIsLFO: Boolean;
    LFOIndex: Integer;          // index into FPartLFOs when SourceIsLFO
    Source: TModSourceType;     // used when not SourceIsLFO
    Dest: TModDestType;
    Amount: Single;
    Bipolar: Boolean;
  end;

  // Per-oscillator override (classic source only). Active=False -> keep the
  // preset's setting for that oscillator.
  TPartOscConfig = record
    Active: Boolean;
    Enabled: Boolean;
    Waveform: TWaveformType;
    Level: Single;
    DetuneCents: Single;
  end;

  { TSAFPart }
  // A monotimbral instrument backed by a polyphonic universal-voice manager.
  TSAFPart = class
  private
    FManager: TSedaiVoiceManager;
    FSource: TSAFPartSource;
    FPreset: string;
    FSampleRate: Cardinal;
    FName: string;
    FCustomTable: TSedaiWavetable;  // owned; shared (read-only) by the pool voices
    FSampleData: TSedaiAudioBuffer; // owned; shared (read-only) by the pool players
    FSampleRootNote: Integer;       // MIDI note the sample was recorded at
    FSampleLoop: TLoopMode;         // loop mode for the sample

    // Instrument-level modulation, applied to the whole voice pool.
    FPartLFOs: array of TPartLFOConfig;
    FPartMods: array of TPartModRouting;

    // Instrument-level oscillator config (classic source).
    FOscMode: TVoiceOscMode;
    FOscCfg: array[0..2] of TPartOscConfig;

    // Common-layer overrides (envelope/filter/level) applied after the preset.
    FCommon: TPartCommonOverride;

    // Full FM parameter block (author side). When FHasFMParams is set and the
    // source is psFM, ApplyToVoice configures voices from FFMParams instead of
    // the named ConfigureFMVoice preset.
    FHasFMParams: Boolean;
    FFMParams: TFMParams;

    // Full per-technique parameter blocks (author side), one per non-sample
    // technique. When the matching Has* flag is set and the source matches,
    // ApplyToVoice configures voices from the block instead of the named key.
    FHasClassicParams: Boolean;
    FClassicParams: TClassicParams;
    FHasWavetableParams: Boolean;
    FWavetableParams: TWavetableParams;
    FHasAdditiveParams: Boolean;
    FAdditiveParams: TAdditiveParams;
    FHasKarplusParams: Boolean;
    FKarplusParams: TKarplusParams;

    // Voice configuration callback (of object) applied to every voice in the
    // pool by FManager.ConfigureAllVoices. Reads FSource / FPreset / modulation.
    procedure ApplyToVoice(AVoice: TSedaiVoice);

  public
    constructor Create(AMaxVoices: Integer = DEFAULT_MAX_VOICES);
    destructor Destroy; override;

    procedure SetSampleRate(AValue: Cardinal);

    // Select the instrument: source generator + a named preset. Re-applies the
    // configuration to all voices in the pool.
    procedure SetInstrument(ASource: TSAFPartSource; const APreset: string);

    // Make this a wavetable instrument driven by a loaded/custom wavetable.
    // The Part takes ownership of ATable and shares it (read-only) with every
    // voice in the pool. Pass APreset as the display name.
    procedure SetCustomWavetable(ATable: TSedaiWavetable; const APreset: string = 'loaded');

    // Make this a sample instrument. The Part takes ownership of ABuffer and
    // shares it (read-only) with every voice's player. ARootNote = the pitch the
    // sample was recorded at; ALoop selects one-shot vs forward loop.
    procedure SetSample(ABuffer: TSedaiAudioBuffer; ARootNote: Integer = 60;
      ALoop: TLoopMode = lmNone);

    // Note control (forwarded to the voice manager).
    procedure NoteOn(ANote: Byte; AVelocity: Single);

    // Trigger a note at an explicit frequency (Hz). Returns the allocated pool
    // slot (-1 if none free), so callers can keep a stable handle to the voice.
    // Classic/wavetable sources get the exact frequency; FM stays note-driven.
    function NoteOnFreq(AFreq, AVelocity: Single): Integer;

    procedure NoteOff(ANote: Byte);
    procedure AllNotesOff;
    procedure AllSoundOff;

    // ------------------------------------------------------------------------
    // MODULATION (instrument-level; shared by the whole voice pool)
    // ------------------------------------------------------------------------
    // Append an LFO to the instrument and return its index. Unlimited.
    function AddLFO(ARate: Single; AWaveform: TWaveformType): Integer;
    // Route a fixed source (envelope/velocity/keytrack/...) to a destination.
    procedure AddModulation(ASource: TModSourceType; ADest: TModDestType;
      AAmount: Single; ABipolar: Boolean = True);
    // Route an LFO (index from AddLFO) to a destination.
    procedure AddModulationLFO(ALFOIndex: Integer; ADest: TModDestType;
      AAmount: Single; ABipolar: Boolean = True);
    // Drop all routings and added LFOs (back to a clean instrument).
    procedure ClearModulation;

    // ------------------------------------------------------------------------
    // OSCILLATOR CONFIG (classic source; applied to the whole pool)
    // ------------------------------------------------------------------------
    // How the 3 oscillators combine: mix / ring-mod / sync.
    procedure SetOscMode(AMode: TVoiceOscMode);
    // Override one oscillator (0..2). Overrides the preset's setting for it.
    procedure ConfigureOscillator(AIndex: Integer; AEnabled: Boolean;
      AWaveform: TWaveformType; ALevel, ADetuneCents: Single);
    // Convenience: osc2 = square, one octave below the fundamental.
    procedure EnableSubOscillator(ALevel: Single = 0.5);

    // Common-layer overrides (envelope/filter/output level) applied on top of
    // the preset. Stored and re-applied to the whole pool. ClearCommonOverride
    // returns to the preset's own values.
    procedure SetCommonOverride(const AOverride: TPartCommonOverride);
    procedure ClearCommonOverride;

    // Full FM parameter block (author side). When set and the source is psFM,
    // every voice is configured from this block (every operator editable)
    // instead of a named built-in. ClearFMParams returns to the named preset.
    procedure SetFMParams(const AParams: TFMParams);
    procedure ClearFMParams;

    // Full per-technique parameter blocks (author side), one per non-sample
    // technique. Set stores the block + reconfigures the pool; Clear returns to
    // the named preset. Each applies only when the source matches its technique.
    procedure SetClassicParams(const AParams: TClassicParams);
    procedure ClearClassicParams;
    procedure SetWavetableParams(const AParams: TWavetableParams);
    procedure ClearWavetableParams;
    procedure SetAdditiveParams(const AParams: TAdditiveParams);
    procedure ClearAdditiveParams;
    procedure SetKarplusParams(const AParams: TKarplusParams);
    procedure ClearKarplusParams;

    // Render a block of audio into AOutput (stereo, interleaved L/R).
    procedure RenderBlock(AOutput: PSingle; AFrameCount: Integer);

    function ActiveVoiceCount: Integer;

    // Direct access for advanced setup.
    property VoiceManager: TSedaiVoiceManager read FManager;
    property Source: TSAFPartSource read FSource;
    property Preset: string read FPreset;
    property Name: string read FName write FName;
    property SampleRate: Cardinal read FSampleRate;
  end;

// Stand-alone preset configurators (reused by the facade/engine if needed).
procedure ConfigureClassicVoice(AVoice: TSedaiVoice; const APreset: string);
procedure ConfigureFMVoice(AVoice: TSedaiVoice; const APreset: string);
// Configure an FM voice from a full parameter block (author side).
procedure ConfigureFMVoiceFromParams(AVoice: TSedaiVoice; const AParams: TFMParams);
// Capture the FM parameter block produced by a named ConfigureFMVoice preset,
// so a designer can start from a built-in and edit every operator.
function ExplodeFMParams(const APresetKey: string): TFMParams;
procedure ConfigureWavetableVoice(AVoice: TSedaiVoice; const APreset: string);
procedure ConfigureAdditiveVoice(AVoice: TSedaiVoice; const APreset: string);
procedure ConfigureKarplusVoice(AVoice: TSedaiVoice; const APreset: string);
// Full-parameter-block configurators + explode (author side), one per technique.
procedure ConfigureClassicVoiceFromParams(AVoice: TSedaiVoice; const AParams: TClassicParams);
function ExplodeClassicParams(const APresetKey: string): TClassicParams;
procedure ConfigureWavetableVoiceFromParams(AVoice: TSedaiVoice; const AParams: TWavetableParams);
function ExplodeWavetableParams(const APresetKey: string): TWavetableParams;
procedure ConfigureAdditiveVoiceFromParams(AVoice: TSedaiVoice; const AParams: TAdditiveParams);
function ExplodeAdditiveParams(const APresetKey: string): TAdditiveParams;
procedure ConfigureKarplusVoiceFromParams(AVoice: TSedaiVoice; const AParams: TKarplusParams);
function ExplodeKarplusParams(const APresetKey: string): TKarplusParams;

// Build a generator-side wavetable (frames + mipmaps) from loaded data.
// Returns nil if the data is not loaded. Caller owns the result.
function BuildWavetableFromData(const AData: TWavetable): TSedaiWavetable;

implementation

// ============================================================================
// PRESET CONFIGURATORS
// ============================================================================

procedure ConfigureClassicVoice(AVoice: TSedaiVoice; const APreset: string);
var
  P: string;
  Trim: Single;
begin
  AVoice.SetSourceType(vstOscillators);
  P := LowerCase(APreset);

  // Sensible defaults (overridden per preset below).
  AVoice.SetOscillatorEnabled(0, True);
  AVoice.SetOscillatorWaveform(0, wtSawtooth);
  AVoice.FilterEnabled := True;
  AVoice.SetFilterType(ftLowPass);
  AVoice.SetFilterCutoff(2500.0);
  AVoice.SetFilterResonance(0.2);
  AVoice.SetEnvelopeADSR(0, 0.01, 0.1, 0.7, 0.3);

  // Per-preset output trim (gain staging): the raw classic voice peaked well
  // over 1.0 on the richer waveforms (square/pulse 1.52, saw 1.38, lead 1.60,
  // bass 1.80), pinning the master limiter. Trims = ~0.85/peak measured offline
  // (saf_preset_levels, note A3, vel 0.9) -> ~0.85 peak ceiling. The mellow
  // tones (sine/triangle/pad ~0.64 peak) keep their full level (Trim 1.0).
  Trim := 0.62;   // base = saw level

  if P = 'sine' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSine);
    AVoice.SetFilterCutoff(10000.0);
    Trim := 1.0;
  end
  else if P = 'square' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSquare);
    Trim := 0.56;
  end
  else if (P = 'saw') or (P = 'sawtooth') then
  begin
    AVoice.SetOscillatorWaveform(0, wtSawtooth);
    Trim := 0.62;
  end
  else if P = 'triangle' then
  begin
    AVoice.SetOscillatorWaveform(0, wtTriangle);
    Trim := 1.0;
  end
  else if P = 'pulse' then
  begin
    AVoice.SetOscillatorWaveform(0, wtPulse);
    Trim := 0.56;
  end
  else if P = 'noise' then
  begin
    AVoice.SetOscillatorWaveform(0, wtNoise);
    AVoice.FilterEnabled := False;
    Trim := 0.5;
  end
  else if P = 'lead' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSawtooth);
    AVoice.SetFilterCutoff(2500.0);
    AVoice.SetFilterResonance(0.6);
    AVoice.SetEnvelopeADSR(0, 0.01, 0.1, 0.8, 0.2);
    Trim := 0.53;
  end
  else if P = 'bass' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSquare);
    AVoice.SetFilterCutoff(800.0);
    AVoice.SetFilterResonance(0.4);
    AVoice.SetEnvelopeADSR(0, 0.01, 0.2, 0.6, 0.15);
    Trim := 0.47;
  end
  else if P = 'pad' then
  begin
    AVoice.SetOscillatorWaveform(0, wtTriangle);
    AVoice.SetFilterCutoff(1800.0);
    AVoice.SetEnvelopeADSR(0, 0.5, 0.5, 0.7, 1.0);
    Trim := 1.0;
  end;

  AVoice.OutputLevel := Trim;
end;

procedure ConfigureFMVoice(AVoice: TSedaiVoice; const APreset: string);
var
  FM: TSedaiFMSynth;
  Op: TSedaiFMOperator;
  P: string;
  I: Integer;
  Trim: Single;
begin
  AVoice.SetSourceType(vstFM);
  FM := AVoice.GetFMSynth;   // created on demand
  if FM = nil then Exit;

  P := LowerCase(APreset);

  // Start from a clean slate: silence all operators, then build the preset.
  for I := 0 to 5 do
    FM.GetOperator(I).Level := 0.0;

  // Per-preset output trim (gain staging): the raw FM presets span ~0.17..2.9
  // peak, which clips and pins the shared master limiter. Each Trim = roughly
  // min(0.9/peak, 0.32/rms) measured offline (note 60, stable filter) → ~0.9
  // peak ceiling, consistent RMS where the preset isn't peak-limited.
  Trim := 1.0;

  if P = 'epiano' then
  begin
    Trim := 0.72;
    FM.Algorithm := 5;
    FM.FeedbackLevel := 0.3;
    Op := FM.GetOperator(0);
    Op.Ratio := 1.0;  Op.Level := 0.9;
    Op.AttackRate := 95; Op.Decay1Rate := 70; Op.SustainLevel := 0.3; Op.ReleaseRate := 60;
    Op := FM.GetOperator(1);
    Op.Ratio := 14.0; Op.Level := 0.5;
    Op.AttackRate := 95; Op.Decay1Rate := 85; Op.SustainLevel := 0.0; Op.ReleaseRate := 70;
  end
  else if P = 'brass' then
  begin
    Trim := 0.42;
    FM.Algorithm := 1;
    FM.FeedbackLevel := 0.5;
    Op := FM.GetOperator(0);
    Op.Ratio := 1.0; Op.Level := 0.9;
    Op.AttackRate := 80; Op.Decay1Rate := 50; Op.SustainLevel := 0.8; Op.ReleaseRate := 50;
    Op := FM.GetOperator(1);
    Op.Ratio := 1.0; Op.Level := 0.7;
    Op.AttackRate := 70; Op.Decay1Rate := 60; Op.SustainLevel := 0.5; Op.ReleaseRate := 50;
  end
  else if P = 'bell' then
  begin
    // Gong-style bell: the 1.0:3.5 carrier:modulator ratio gives the metallic
    // inharmonic strike, but the carrier now rings long (slow decay) while the
    // modulator decays faster, so the bright clang settles into a sustained
    // bell tone instead of the old short ping. (Ear-confirmed over Chowning 1.4.)
    Trim := 0.47;
    FM.Algorithm := 1;
    FM.FeedbackLevel := 0.0;
    Op := FM.GetOperator(0);
    Op.Ratio := 1.0; Op.Level := 0.8;
    Op.AttackRate := 99; Op.Decay1Rate := 26; Op.SustainLevel := 0.0; Op.ReleaseRate := 26;
    Op := FM.GetOperator(1);
    Op.Ratio := 3.5; Op.Level := 0.9;
    Op.AttackRate := 99; Op.Decay1Rate := 44; Op.SustainLevel := 0.0; Op.ReleaseRate := 44;
  end
  else if P = 'organ' then
  begin
    Trim := 3.90;        // additive /6 → quiet; boosted back up
    FM.Algorithm := 32;  // all carriers (additive)
    FM.GetOperator(0).Ratio := 0.5; FM.GetOperator(0).Level := 0.6;
    FM.GetOperator(1).Ratio := 1.0; FM.GetOperator(1).Level := 0.8;
    FM.GetOperator(2).Ratio := 2.0; FM.GetOperator(2).Level := 0.5;
    FM.GetOperator(3).Ratio := 3.0; FM.GetOperator(3).Level := 0.3;
    FM.GetOperator(4).Ratio := 4.0; FM.GetOperator(4).Level := 0.2;
    FM.GetOperator(5).Ratio := 6.0; FM.GetOperator(5).Level := 0.1;
  end
  else if P = 'bass' then
  begin
    Trim := 0.31;
    FM.Algorithm := 1;
    FM.FeedbackLevel := 0.6;
    Op := FM.GetOperator(0);
    Op.Ratio := 1.0; Op.Level := 0.9;
    Op.AttackRate := 95; Op.Decay1Rate := 60; Op.SustainLevel := 0.5; Op.ReleaseRate := 70;
    Op := FM.GetOperator(1);
    Op.Ratio := 1.0; Op.Level := 0.8;
    Op.AttackRate := 90; Op.Decay1Rate := 70; Op.SustainLevel := 0.3; Op.ReleaseRate := 70;
  end
  else  // default: simple 2-op FM
  begin
    Trim := 0.36;
    FM.Algorithm := 1;
    FM.FeedbackLevel := 0.2;
    FM.GetOperator(0).Ratio := 1.0; FM.GetOperator(0).Level := 0.8;
    FM.GetOperator(1).Ratio := 2.0; FM.GetOperator(1).Level := 0.5;
  end;

  AVoice.OutputLevel := Trim;
end;

procedure ConfigureFMVoiceFromParams(AVoice: TSedaiVoice; const AParams: TFMParams);
var
  FM: TSedaiFMSynth;
  Op: TSedaiFMOperator;
  I, Alg: Integer;
begin
  AVoice.SetSourceType(vstFM);
  FM := AVoice.GetFMSynth;
  if FM = nil then Exit;
  Alg := AParams.Algorithm;
  if Alg < 1 then Alg := 1 else if Alg > 32 then Alg := 32;
  FM.Algorithm := Alg;
  FM.FeedbackLevel := AParams.FeedbackLevel;
  for I := 0 to MAX_FM_OPERATORS - 1 do
  begin
    Op := FM.GetOperator(I);
    Op.Ratio := AParams.Ops[I].Ratio;
    Op.Fixed := AParams.Ops[I].Fixed;
    Op.FixedFreq := AParams.Ops[I].FixedFreq;
    Op.Detune := AParams.Ops[I].Detune;
    Op.Level := AParams.Ops[I].Level;
    Op.VelocitySensitivity := AParams.Ops[I].VelocitySensitivity;
    Op.AttackRate := AParams.Ops[I].AttackRate;
    Op.Decay1Rate := AParams.Ops[I].Decay1Rate;
    Op.Decay2Rate := AParams.Ops[I].Decay2Rate;
    Op.ReleaseRate := AParams.Ops[I].ReleaseRate;
    Op.AttackLevel := AParams.Ops[I].AttackLevel;
    Op.Decay1Level := AParams.Ops[I].Decay1Level;
    Op.SustainLevel := AParams.Ops[I].SustainLevel;
  end;
  AVoice.OutputLevel := AParams.OutputTrim;
end;

function ExplodeFMParams(const APresetKey: string): TFMParams;
var
  V: TSedaiVoice;
  FM: TSedaiFMSynth;
  Op: TSedaiFMOperator;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);   // no managed fields in TFMParams
  V := TSedaiVoice.Create;
  try
    ConfigureFMVoice(V, APresetKey);
    FM := V.GetFMSynth;
    if FM = nil then Exit;
    Result.Algorithm := FM.Algorithm;
    Result.FeedbackLevel := FM.FeedbackLevel;
    Result.OutputTrim := V.OutputLevel;
    for I := 0 to MAX_FM_OPERATORS - 1 do
    begin
      Op := FM.GetOperator(I);
      Result.Ops[I].Ratio := Op.Ratio;
      Result.Ops[I].Fixed := Op.Fixed;
      Result.Ops[I].FixedFreq := Op.FixedFreq;
      Result.Ops[I].Detune := Op.Detune;
      Result.Ops[I].Level := Op.Level;
      Result.Ops[I].VelocitySensitivity := Op.VelocitySensitivity;
      Result.Ops[I].AttackRate := Op.AttackRate;
      Result.Ops[I].Decay1Rate := Op.Decay1Rate;
      Result.Ops[I].Decay2Rate := Op.Decay2Rate;
      Result.Ops[I].ReleaseRate := Op.ReleaseRate;
      Result.Ops[I].AttackLevel := Op.AttackLevel;
      Result.Ops[I].Decay1Level := Op.Decay1Level;
      Result.Ops[I].SustainLevel := Op.SustainLevel;
    end;
  finally
    V.Free;
  end;
end;

// ---- CLASSIC ----------------------------------------------------------------

procedure ConfigureClassicVoiceFromParams(AVoice: TSedaiVoice; const AParams: TClassicParams);
var
  I: Integer;
begin
  AVoice.SetSourceType(vstOscillators);
  AVoice.OscMode := AParams.OscMode;
  for I := 0 to MAX_OSCILLATORS - 1 do
  begin
    AVoice.SetOscillatorEnabled(I, AParams.Oscs[I].Enabled);
    AVoice.SetOscillatorWaveform(I, AParams.Oscs[I].Waveform);
    AVoice.SetOscillatorLevel(I, AParams.Oscs[I].Level);
    AVoice.SetOscillatorDetune(I, AParams.Oscs[I].DetuneCents);
    AVoice.SetOscillatorPulseWidth(I, AParams.Oscs[I].PulseWidth);
  end;
  AVoice.FilterEnabled := AParams.FilterEnabled;
  AVoice.SetFilterType(AParams.FilterType);
  AVoice.SetFilterCutoff(AParams.FilterCutoff);
  AVoice.SetFilterResonance(AParams.FilterResonance);
  AVoice.SetEnvelopeADSR(0, AParams.Attack, AParams.Decay, AParams.Sustain, AParams.Release);
  AVoice.OutputLevel := AParams.OutputTrim;
end;

function ExplodeClassicParams(const APresetKey: string): TClassicParams;
var
  V: TSedaiVoice;
  Osc: TSedaiOscillator;
  Env: TSedaiEnvelope;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);   // no managed fields
  V := TSedaiVoice.Create;
  try
    ConfigureClassicVoice(V, APresetKey);
    Result.OscMode := V.OscMode;
    for I := 0 to MAX_OSCILLATORS - 1 do
    begin
      Osc := V.Oscillators[I];
      Result.Oscs[I].Enabled := V.GetOscillatorEnabled(I);
      Result.Oscs[I].Level := V.GetOscillatorLevel(I);
      Result.Oscs[I].DetuneCents := V.GetOscillatorDetune(I);
      if Osc <> nil then
      begin
        Result.Oscs[I].Waveform := Osc.Waveform;
        Result.Oscs[I].PulseWidth := Osc.PulseWidth;
      end;
    end;
    Result.FilterEnabled := V.FilterEnabled;
    Result.FilterType := V.Filter.FilterType;
    Result.FilterCutoff := V.Filter.Cutoff;     // fresh voice: == base cutoff
    Result.FilterResonance := V.Filter.Resonance;
    Env := V.Envelopes[0];
    if Env <> nil then
    begin
      Result.Attack := Env.AttackTime;
      Result.Decay := Env.DecayTime;
      Result.Sustain := Env.SustainLevel;
      Result.Release := Env.ReleaseTime;
    end;
    Result.OutputTrim := V.OutputLevel;
  finally
    V.Free;
  end;
end;

// ---- WAVETABLE --------------------------------------------------------------

procedure ConfigureWavetableVoiceFromParams(AVoice: TSedaiVoice; const AParams: TWavetableParams);
var
  WT: TSedaiWavetableGenerator;
begin
  AVoice.SetSourceType(vstWavetable);
  WT := AVoice.GetWavetableGenerator;
  if WT = nil then Exit;
  case AParams.Kind of
    wtkPWM:      WT.CreatePWMWavetable(AParams.Steps);
    wtkSuperSaw: WT.CreateSuperSawWavetable(AParams.Steps);
  else
    WT.CreateBasicWavetable;
  end;
  WT.UnisonVoices := AParams.UnisonVoices;
  WT.UnisonDetune := AParams.UnisonDetune;
  WT.UnisonSpread := AParams.UnisonSpread;
  AVoice.SetEnvelopeADSR(0, AParams.Attack, AParams.Decay, AParams.Sustain, AParams.Release);
  AVoice.OutputLevel := AParams.OutputTrim;
end;

function ExplodeWavetableParams(const APresetKey: string): TWavetableParams;
var
  V: TSedaiVoice;
  WT: TSedaiWavetableGenerator;
  Env: TSedaiEnvelope;
  P: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  // Table kind/step count is procedural and not readable from the generator, so
  // it is classified from the preset key (matches ConfigureWavetableVoice).
  P := LowerCase(APresetKey);
  if P = 'pwm' then begin Result.Kind := wtkPWM; Result.Steps := 64; end
  else if P = 'supersaw' then begin Result.Kind := wtkSuperSaw; Result.Steps := 32; end
  else begin Result.Kind := wtkBasic; Result.Steps := 0; end;
  V := TSedaiVoice.Create;
  try
    ConfigureWavetableVoice(V, APresetKey);
    WT := V.GetWavetableGenerator;
    if WT <> nil then
    begin
      Result.UnisonVoices := WT.UnisonVoices;
      Result.UnisonDetune := WT.UnisonDetune;
      Result.UnisonSpread := WT.UnisonSpread;
    end;
    Env := V.Envelopes[0];
    if Env <> nil then
    begin
      Result.Attack := Env.AttackTime;
      Result.Decay := Env.DecayTime;
      Result.Sustain := Env.SustainLevel;
      Result.Release := Env.ReleaseTime;
    end;
    Result.OutputTrim := V.OutputLevel;
  finally
    V.Free;
  end;
end;

// ---- ADDITIVE ---------------------------------------------------------------

procedure ConfigureAdditiveVoiceFromParams(AVoice: TSedaiVoice; const AParams: TAdditiveParams);
var
  AG: TSedaiAdditiveGenerator;
  I, Count: Integer;
begin
  AVoice.SetSourceType(vstAdditive);
  AG := AVoice.GetAdditiveGenerator;
  if AG = nil then Exit;
  Count := AParams.HarmonicCount;
  if Count < 1 then Count := 1
  else if Count > ADDITIVE_MAX_HARMONICS then Count := ADDITIVE_MAX_HARMONICS;
  AG.HarmonicCount := Count;                 // set first so UpdateActiveHarmonics covers them
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    AG.SetHarmonicLevel(I, AParams.Levels[I]);
    AG.SetHarmonicDetune(I, AParams.Detunes[I]);
  end;
  AG.AmpEnvelope.AttackTime := AParams.Attack;
  AG.AmpEnvelope.DecayTime := AParams.Decay;
  AG.AmpEnvelope.SustainLevel := AParams.Sustain;
  AG.AmpEnvelope.ReleaseTime := AParams.Release;
  AVoice.OutputLevel := AParams.OutputTrim;
end;

function ExplodeAdditiveParams(const APresetKey: string): TAdditiveParams;
var
  V: TSedaiVoice;
  AG: TSedaiAdditiveGenerator;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  V := TSedaiVoice.Create;
  try
    ConfigureAdditiveVoice(V, APresetKey);
    AG := V.GetAdditiveGenerator;
    if AG <> nil then
    begin
      Result.HarmonicCount := AG.HarmonicCount;
      for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
      begin
        Result.Levels[I] := AG.GetHarmonicLevel(I);
        Result.Detunes[I] := AG.GetHarmonicDetune(I);
      end;
      Result.Attack := AG.AmpEnvelope.AttackTime;
      Result.Decay := AG.AmpEnvelope.DecayTime;
      Result.Sustain := AG.AmpEnvelope.SustainLevel;
      Result.Release := AG.AmpEnvelope.ReleaseTime;
    end;
    Result.OutputTrim := V.OutputLevel;
  finally
    V.Free;
  end;
end;

// ---- KARPLUS-STRONG ---------------------------------------------------------

procedure ConfigureKarplusVoiceFromParams(AVoice: TSedaiVoice; const AParams: TKarplusParams);
var
  KS: TSedaiKarplusGenerator;
begin
  AVoice.SetSourceType(vstKarplus);
  KS := AVoice.GetKarplusGenerator;
  if KS = nil then Exit;
  KS.Damping := AParams.Damping;
  KS.Blend := AParams.Blend;
  AVoice.SetEnvelopeADSR(0, AParams.Attack, AParams.Decay, AParams.Sustain, AParams.Release);
  AVoice.OutputLevel := AParams.OutputTrim;
end;

function ExplodeKarplusParams(const APresetKey: string): TKarplusParams;
var
  V: TSedaiVoice;
  KS: TSedaiKarplusGenerator;
  Env: TSedaiEnvelope;
begin
  FillChar(Result, SizeOf(Result), 0);
  V := TSedaiVoice.Create;
  try
    ConfigureKarplusVoice(V, APresetKey);
    KS := V.GetKarplusGenerator;
    if KS <> nil then
    begin
      Result.Damping := KS.Damping;
      Result.Blend := KS.Blend;
    end;
    Env := V.Envelopes[0];
    if Env <> nil then
    begin
      Result.Attack := Env.AttackTime;
      Result.Decay := Env.DecayTime;
      Result.Sustain := Env.SustainLevel;
      Result.Release := Env.ReleaseTime;
    end;
    Result.OutputTrim := V.OutputLevel;
  finally
    V.Free;
  end;
end;

procedure ConfigureWavetableVoice(AVoice: TSedaiVoice; const APreset: string);
var
  WT: TSedaiWavetableGenerator;
  P: string;
  Trim: Single;
begin
  AVoice.SetSourceType(vstWavetable);
  WT := AVoice.GetWavetableGenerator;  // created on demand
  if WT = nil then Exit;

  P := LowerCase(APreset);

  // Per-preset output trim (gain staging, same scheme as the FM presets).
  Trim := 0.72;  // basic

  if P = 'pwm' then
  begin
    Trim := 0.49;
    WT.CreatePWMWavetable(64);
  end
  else if P = 'supersaw' then
  begin
    Trim := 0.66;
    WT.CreateSuperSawWavetable(32);
    WT.UnisonVoices := 7;
    WT.UnisonDetune := 20.0;
    WT.UnisonSpread := 0.7;
  end
  else
    WT.CreateBasicWavetable;

  // Wavetable voices use the shared amp envelope for their lifecycle.
  AVoice.SetEnvelopeADSR(0, 0.01, 0.2, 0.7, 0.3);
  AVoice.OutputLevel := Trim;
end;

procedure ConfigureAdditiveVoice(AVoice: TSedaiVoice; const APreset: string);
var
  AG: TSedaiAdditiveGenerator;
  P: string;
  Trim: Single;
begin
  AVoice.SetSourceType(vstAdditive);
  AG := AVoice.GetAdditiveGenerator;  // created on demand
  if AG = nil then Exit;

  P := LowerCase(APreset);

  // Harmonic spectrum + amplitude-envelope shaping per preset. The additive
  // generator owns its own amp envelope (the voice amp env is bypassed), so the
  // envelope is what gives organ/bell/strings their character. Trim = gain stage
  // (additive sums many harmonics; conservative ceilings, tunable in polish).
  Trim := 0.6;
  if P = 'sine' then
  begin
    AG.LoadPreset(apSine); Trim := 0.9;
    AG.AmpEnvelope.AttackTime := 0.01; AG.AmpEnvelope.DecayTime := 0.1;
    AG.AmpEnvelope.SustainLevel := 0.8; AG.AmpEnvelope.ReleaseTime := 0.3;
  end
  else if (P = 'saw') or (P = 'sawtooth') then
  begin
    AG.LoadPreset(apSaw); Trim := 0.42;   // was 0.5: peaked 1.01
    AG.AmpEnvelope.AttackTime := 0.01; AG.AmpEnvelope.DecayTime := 0.1;
    AG.AmpEnvelope.SustainLevel := 0.8; AG.AmpEnvelope.ReleaseTime := 0.3;
  end
  else if P = 'square' then
  begin
    AG.LoadPreset(apSquare); Trim := 0.5;
    AG.AmpEnvelope.AttackTime := 0.01; AG.AmpEnvelope.DecayTime := 0.1;
    AG.AmpEnvelope.SustainLevel := 0.8; AG.AmpEnvelope.ReleaseTime := 0.3;
  end
  else if P = 'triangle' then
  begin
    AG.LoadPreset(apTriangle); Trim := 0.7;
    AG.AmpEnvelope.AttackTime := 0.01; AG.AmpEnvelope.DecayTime := 0.1;
    AG.AmpEnvelope.SustainLevel := 0.8; AG.AmpEnvelope.ReleaseTime := 0.3;
  end
  else if P = 'organ' then
  begin
    AG.LoadPreset(apOrgan); Trim := 0.5;
    AG.AmpEnvelope.AttackTime := 0.01; AG.AmpEnvelope.DecayTime := 0.0;
    AG.AmpEnvelope.SustainLevel := 1.0; AG.AmpEnvelope.ReleaseTime := 0.12;
  end
  else if P = 'bell' then
  begin
    AG.LoadPreset(apBell); Trim := 0.50;   // was 0.6: peaked 1.01
    AG.AmpEnvelope.AttackTime := 0.001; AG.AmpEnvelope.DecayTime := 1.6;
    AG.AmpEnvelope.SustainLevel := 0.0; AG.AmpEnvelope.ReleaseTime := 0.8;
  end
  else if P = 'strings' then
  begin
    AG.LoadPreset(apStrings); Trim := 0.42;   // was 0.5: peaked 1.02
    AG.AmpEnvelope.AttackTime := 0.3; AG.AmpEnvelope.DecayTime := 0.2;
    AG.AmpEnvelope.SustainLevel := 0.85; AG.AmpEnvelope.ReleaseTime := 0.5;
  end
  else if P = 'choir' then
  begin
    AG.LoadPreset(apChoir); Trim := 0.37;   // was 0.6: peaked 1.37
    AG.AmpEnvelope.AttackTime := 0.25; AG.AmpEnvelope.DecayTime := 0.2;
    AG.AmpEnvelope.SustainLevel := 0.9; AG.AmpEnvelope.ReleaseTime := 0.5;
  end
  else if P = 'brass' then
  begin
    AG.LoadPreset(apBrass); Trim := 0.39;   // was 0.5: peaked 1.10
    AG.AmpEnvelope.AttackTime := 0.05; AG.AmpEnvelope.DecayTime := 0.1;
    AG.AmpEnvelope.SustainLevel := 0.85; AG.AmpEnvelope.ReleaseTime := 0.2;
  end
  else if P = 'flute' then
  begin
    AG.LoadPreset(apFlute); Trim := 0.7;
    AG.AmpEnvelope.AttackTime := 0.08; AG.AmpEnvelope.DecayTime := 0.1;
    AG.AmpEnvelope.SustainLevel := 0.9; AG.AmpEnvelope.ReleaseTime := 0.2;
  end
  else
  begin
    AG.LoadPreset(apOrgan); Trim := 0.5;
    AG.AmpEnvelope.AttackTime := 0.01; AG.AmpEnvelope.DecayTime := 0.0;
    AG.AmpEnvelope.SustainLevel := 1.0; AG.AmpEnvelope.ReleaseTime := 0.12;
  end;

  AVoice.OutputLevel := Trim;
end;

procedure ConfigureKarplusVoice(AVoice: TSedaiVoice; const APreset: string);
var
  KS: TSedaiKarplusGenerator;
  P: string;
  Damping, Blend, Trim: Single;
begin
  AVoice.SetSourceType(vstKarplus);
  KS := AVoice.GetKarplusGenerator;   // created on demand
  if KS = nil then Exit;

  P := LowerCase(APreset);

  // Damping = sustain length; Blend = string (0.5) vs percussive (<0.5).
  Damping := 0.996; Blend := 0.5; Trim := 0.9;   // guitar default
  if P = 'bass' then
  begin
    Damping := 0.998; Blend := 0.5; Trim := 0.95;   // long, round
  end
  else if P = 'harp' then
  begin
    Damping := 0.9965; Blend := 0.5; Trim := 0.85;  // bright, singing
  end
  else if (P = 'mute') or (P = 'staccato') then
  begin
    Damping := 0.986; Blend := 0.5; Trim := 0.9;    // short, palm-muted
  end
  else if P = 'drum' then
  begin
    Damping := 0.96; Blend := 0.42; Trim := 0.9;    // percussive / drum-like
  end;

  KS.Damping := Damping;
  KS.Blend := Blend;

  // The string self-decays; the amp envelope just gates (instant attack, full
  // sustain, short release so note-off fades it).
  AVoice.SetEnvelopeADSR(0, 0.0, 0.0, 1.0, 0.15);
  AVoice.OutputLevel := Trim;
end;

function BuildWavetableFromData(const AData: TWavetable): TSedaiWavetable;
var
  I, TableSize: Integer;
begin
  Result := nil;
  if (not AData.IsLoaded) or (AData.WaveCount <= 0) or (AData.SampleLength <= 0) then
    Exit;

  // One generator table sized to the source frame length (2048 for Serum/Vital/
  // Surge, the cycle length for single-cycle WAVs). AddFrame resizes per frame.
  TableSize := AData.SampleLength;
  Result := TSedaiWavetable.Create(TableSize);
  Result.Name := AData.Name;
  for I := 0 to AData.WaveCount - 1 do
    Result.AddFrame(AData.Samples[I]);
end;

// ============================================================================
// TSAFPart
// ============================================================================

constructor TSAFPart.Create(AMaxVoices: Integer);
var
  I: Integer;
begin
  inherited Create;

  FManager := TSedaiVoiceManager.Create;
  FManager.MaxVoices := AMaxVoices;
  FSource := psClassic;
  FPreset := 'saw';
  FSampleRate := SEDAI_DEFAULT_SAMPLE_RATE;
  FName := 'Part';
  FCustomTable := nil;
  FSampleData := nil;
  FSampleRootNote := 60;
  FSampleLoop := lmNone;

  FOscMode := vomMix;
  for I := 0 to 2 do
    FOscCfg[I].Active := False;

  // Apply the default instrument so the pool is immediately playable.
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

destructor TSAFPart.Destroy;
begin
  FManager.Free;       // voices reference FCustomTable / FSampleData but don't own them
  FCustomTable.Free;   // nil-safe
  // TSedaiAudioBuffer shadows Free with a non-virtual data-clear (not nil-safe),
  // so go through FreeAndNil (TObject.Free) to destroy the object safely.
  FreeAndNil(FSampleData);
  inherited Destroy;
end;

procedure TSAFPart.SetSampleRate(AValue: Cardinal);
begin
  FSampleRate := AValue;
  FManager.SetSampleRate(AValue);
end;

procedure TSAFPart.ApplyToVoice(AVoice: TSedaiVoice);
var
  I: Integer;
  LFOMap: array of Integer;
begin
  // 1. Preset: source generator + timbre.
  case FSource of
    psFM:
      if FHasFMParams then
        ConfigureFMVoiceFromParams(AVoice, FFMParams)
      else
        ConfigureFMVoice(AVoice, FPreset);
    psClassic:
      if FHasClassicParams then
        ConfigureClassicVoiceFromParams(AVoice, FClassicParams)
      else
        ConfigureClassicVoice(AVoice, FPreset);
    psAdditive:
      if FHasAdditiveParams then
        ConfigureAdditiveVoiceFromParams(AVoice, FAdditiveParams)
      else
        ConfigureAdditiveVoice(AVoice, FPreset);
    psKarplus:
      if FHasKarplusParams then
        ConfigureKarplusVoiceFromParams(AVoice, FKarplusParams)
      else
        ConfigureKarplusVoice(AVoice, FPreset);
    psSample:
      if Assigned(FSampleData) then
      begin
        AVoice.SetSourceType(vstSample);
        AVoice.GetSamplePlayer.LoadSample(FSampleData, False);  // shared, not owned
        AVoice.GetSamplePlayer.RootNote := FSampleRootNote;
        AVoice.GetSamplePlayer.LoopMode := FSampleLoop;
        AVoice.GetSamplePlayer.Interpolation := imLinear;
        // Sampler amp envelope: near-instant attack, hold, short release.
        AVoice.SetEnvelopeADSR(0, 0.001, 0.0, 1.0, 0.1);
        AVoice.OutputLevel := 0.9;
      end
      else
        ConfigureClassicVoice(AVoice, FPreset);  // no data -> audible fallback
    psWavetable:
      if Assigned(FCustomTable) then
      begin
        // Loaded/custom wavetable: share the Part's table (read-only) with the voice.
        AVoice.SetSourceType(vstWavetable);
        AVoice.GetWavetableGenerator.LoadWavetable(FCustomTable, False);
        AVoice.SetEnvelopeADSR(0, 0.01, 0.2, 0.7, 0.3);
        AVoice.OutputLevel := 0.7;  // conservative gain stage for arbitrary tables
      end
      else if FHasWavetableParams then
        ConfigureWavetableVoiceFromParams(AVoice, FWavetableParams)
      else
        ConfigureWavetableVoice(AVoice, FPreset);
  else
    ConfigureClassicVoice(AVoice, FPreset);
  end;

  // 1b. Oscillator config (classic source only): combine mode + per-oscillator
  //     overrides on top of the preset. vomMix + no overrides = unchanged.
  if FSource = psClassic then
  begin
    AVoice.OscMode := FOscMode;
    for I := 0 to 2 do
      if FOscCfg[I].Active then
      begin
        AVoice.SetOscillatorEnabled(I, FOscCfg[I].Enabled);
        AVoice.SetOscillatorWaveform(I, FOscCfg[I].Waveform);
        AVoice.SetOscillatorLevel(I, FOscCfg[I].Level);
        AVoice.SetOscillatorDetune(I, FOscCfg[I].DetuneCents);
      end;
  end;

  // 1c. Common-layer overrides on top of the preset (off by default).
  if FCommon.OverrideEnvelope then
    AVoice.SetEnvelopeADSR(0, FCommon.Attack, FCommon.Decay, FCommon.Sustain, FCommon.Release);
  if FCommon.OverrideFilter then
  begin
    AVoice.FilterEnabled := FCommon.FilterEnabled;
    if FCommon.FilterEnabled then
    begin
      AVoice.SetFilterType(ftLowPass);
      AVoice.SetFilterCutoff(FCommon.FilterCutoff);
      AVoice.SetFilterResonance(FCommon.FilterResonance);
    end;
  end;
  if FCommon.OverrideLevel then
    AVoice.OutputLevel := FCommon.OutputLevel;

  // 2. Rebuild the modulation layer from the Part's stored config. Done from a
  //    clean slate every time so the call is idempotent (no slot/LFO build-up
  //    across reconfigures). LFO indices are captured per-voice into LFOMap so
  //    routings resolve to the right LFO regardless of the built-in count.
  AVoice.ClearModulation;
  AVoice.ResetExtraLFOs;

  SetLength(LFOMap, Length(FPartLFOs));
  for I := 0 to High(FPartLFOs) do
    LFOMap[I] := AVoice.AddLFO(FPartLFOs[I].Rate, FPartLFOs[I].Waveform);

  for I := 0 to High(FPartMods) do
  begin
    if FPartMods[I].SourceIsLFO then
    begin
      if (FPartMods[I].LFOIndex >= 0) and (FPartMods[I].LFOIndex < Length(LFOMap)) then
        AVoice.AddModulationLFO(LFOMap[FPartMods[I].LFOIndex], FPartMods[I].Dest,
          FPartMods[I].Amount, FPartMods[I].Bipolar);
    end
    else
      AVoice.AddModulation(FPartMods[I].Source, FPartMods[I].Dest,
        FPartMods[I].Amount, FPartMods[I].Bipolar);
  end;
end;

procedure TSAFPart.SetInstrument(ASource: TSAFPartSource; const APreset: string);
begin
  FSource := ASource;
  FPreset := APreset;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.SetCommonOverride(const AOverride: TPartCommonOverride);
begin
  FCommon := AOverride;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.ClearCommonOverride;
begin
  FillChar(FCommon, SizeOf(FCommon), 0);   // all Override* := False
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.SetFMParams(const AParams: TFMParams);
begin
  FFMParams := AParams;
  FHasFMParams := True;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.ClearFMParams;
begin
  if not FHasFMParams then Exit;   // already on the named preset; no-op
  FHasFMParams := False;
  FillChar(FFMParams, SizeOf(FFMParams), 0);   // no managed fields in TFMParams
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.SetClassicParams(const AParams: TClassicParams);
begin
  FClassicParams := AParams;
  FHasClassicParams := True;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.ClearClassicParams;
begin
  if not FHasClassicParams then Exit;
  FHasClassicParams := False;
  FillChar(FClassicParams, SizeOf(FClassicParams), 0);   // no managed fields
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.SetWavetableParams(const AParams: TWavetableParams);
begin
  FWavetableParams := AParams;
  FHasWavetableParams := True;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.ClearWavetableParams;
begin
  if not FHasWavetableParams then Exit;
  FHasWavetableParams := False;
  FillChar(FWavetableParams, SizeOf(FWavetableParams), 0);
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.SetAdditiveParams(const AParams: TAdditiveParams);
begin
  FAdditiveParams := AParams;
  FHasAdditiveParams := True;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.ClearAdditiveParams;
begin
  if not FHasAdditiveParams then Exit;
  FHasAdditiveParams := False;
  FillChar(FAdditiveParams, SizeOf(FAdditiveParams), 0);
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.SetKarplusParams(const AParams: TKarplusParams);
begin
  FKarplusParams := AParams;
  FHasKarplusParams := True;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.ClearKarplusParams;
begin
  if not FHasKarplusParams then Exit;
  FHasKarplusParams := False;
  FillChar(FKarplusParams, SizeOf(FKarplusParams), 0);
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.SetCustomWavetable(ATable: TSedaiWavetable; const APreset: string);
begin
  if FCustomTable <> ATable then
  begin
    FCustomTable.Free;   // nil-safe; drop any previously owned table
    FCustomTable := ATable;
  end;
  FSource := psWavetable;
  FPreset := APreset;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.SetSample(ABuffer: TSedaiAudioBuffer; ARootNote: Integer;
  ALoop: TLoopMode);
begin
  if FSampleData <> ABuffer then
  begin
    FreeAndNil(FSampleData);   // drop any previously owned buffer (nil-safe)
    FSampleData := ABuffer;
  end;
  FSampleRootNote := ARootNote;
  FSampleLoop := ALoop;
  FSource := psSample;
  FPreset := 'sample';
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.NoteOn(ANote: Byte; AVelocity: Single);
begin
  FManager.NoteOn(ANote, AVelocity);
end;

function TSAFPart.NoteOnFreq(AFreq, AVelocity: Single): Integer;
begin
  Result := -1;
  if AFreq <= 0.0 then Exit;

  // The manager allocates by nearest MIDI note (key-tracking / stealing) and
  // primes the voice with the exact Hz so every source latches on pitch -
  // including Karplus, whose pitch is baked into the delay line at pluck time.
  // FM stays note-driven (its synth ignores the voice frequency).
  FManager.NoteOnFreq(AFreq, AVelocity);
  Result := FManager.LastVoiceIndex;
end;

procedure TSAFPart.NoteOff(ANote: Byte);
begin
  FManager.NoteOff(ANote);
end;

procedure TSAFPart.AllNotesOff;
begin
  FManager.AllNotesOff;
end;

procedure TSAFPart.AllSoundOff;
begin
  FManager.AllSoundOff;
end;

function TSAFPart.AddLFO(ARate: Single; AWaveform: TWaveformType): Integer;
begin
  SetLength(FPartLFOs, Length(FPartLFOs) + 1);
  FPartLFOs[High(FPartLFOs)].Rate := ARate;
  FPartLFOs[High(FPartLFOs)].Waveform := AWaveform;
  Result := High(FPartLFOs);
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.AddModulation(ASource: TModSourceType; ADest: TModDestType;
  AAmount: Single; ABipolar: Boolean);
begin
  SetLength(FPartMods, Length(FPartMods) + 1);
  with FPartMods[High(FPartMods)] do
  begin
    SourceIsLFO := False;
    LFOIndex := -1;
    Source := ASource;
    Dest := ADest;
    Amount := AAmount;
    Bipolar := ABipolar;
  end;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.AddModulationLFO(ALFOIndex: Integer; ADest: TModDestType;
  AAmount: Single; ABipolar: Boolean);
begin
  SetLength(FPartMods, Length(FPartMods) + 1);
  with FPartMods[High(FPartMods)] do
  begin
    SourceIsLFO := True;
    LFOIndex := ALFOIndex;
    Source := mstNone;
    Dest := ADest;
    Amount := AAmount;
    Bipolar := ABipolar;
  end;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.ClearModulation;
begin
  SetLength(FPartMods, 0);
  SetLength(FPartLFOs, 0);
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.SetOscMode(AMode: TVoiceOscMode);
begin
  FOscMode := AMode;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.ConfigureOscillator(AIndex: Integer; AEnabled: Boolean;
  AWaveform: TWaveformType; ALevel, ADetuneCents: Single);
begin
  if (AIndex < 0) or (AIndex > 2) then Exit;
  FOscCfg[AIndex].Active := True;
  FOscCfg[AIndex].Enabled := AEnabled;
  FOscCfg[AIndex].Waveform := AWaveform;
  FOscCfg[AIndex].Level := ALevel;
  FOscCfg[AIndex].DetuneCents := ADetuneCents;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.EnableSubOscillator(ALevel: Single);
begin
  // Sub-oscillator: osc2 = square, one octave below the fundamental.
  ConfigureOscillator(2, True, wtSquare, ALevel, -1200.0);
end;

procedure TSAFPart.RenderBlock(AOutput: PSingle; AFrameCount: Integer);
begin
  // VoiceManager outputs stereo interleaved; it clears AOutput internally.
  FManager.ProcessBlock(nil, AOutput, AFrameCount);
end;

function TSAFPart.ActiveVoiceCount: Integer;
begin
  Result := FManager.ActiveVoiceCount;
end;

end.
