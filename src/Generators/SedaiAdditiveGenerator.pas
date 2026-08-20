{*
 * Sedai Audio Foundation - Additive Generator
 *
 * TSedaiAdditiveGenerator provides additive synthesis with up to 64 harmonics.
 * Each harmonic can have independent level, detune, and envelope. Includes
 * preset waveforms (sine, saw, square, triangle, organ, bell, strings).
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAdditiveGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode,
  SedaiOscillator, SedaiEnvelope, SedaiRandom;

const
  ADDITIVE_MAX_HARMONICS = 64;
  ADDITIVE_DEFAULT_HARMONICS = 32;
  ADDITIVE_MAX_UNISON = 8;    // max detuned copies per voice for the "section" mode
  TWO_PI = 2 * Pi;

  // Filtered-noise residual: a small fixed bank of ~octave-spaced band-pass
  // filters. White noise through these, weighted by per-band gains, is the SMS
  // "stochastic" layer (breath/bow/reed noise). 6 bands span ~250 Hz..8 kHz.
  RESIDUAL_BANDS = 6;
  RESIDUAL_BAND_HZ: array[0..RESIDUAL_BANDS-1] of Single =
    (250, 500, 1000, 2000, 4000, 8000);
  RESIDUAL_BAND_Q = 2.0;

type
  { TAdditivePreset }
  TAdditivePreset = (
    apSine,         // Pure sine wave (fundamental only)
    apSaw,          // Sawtooth (all harmonics 1/n)
    apSquare,       // Square (odd harmonics 1/n)
    apTriangle,     // Triangle (odd harmonics 1/n^2)
    apOrgan,        // Organ drawbar simulation
    apBell,         // Bell-like inharmonic
    apStrings,      // String ensemble
    apChoir,        // Choir/vocal
    apBrass,        // Brass-like
    apFlute,        // Flute-like
    apCustom        // User-defined
  );

  { TSedaiAdditiveGenerator }
  // Additive synthesis generator with per-harmonic control
  TSedaiAdditiveGenerator = class(TSedaiSignalGenerator)
  private
    // This voice's own randomness. It feeds the breath and band noise, the
    // slow drift of the vibrato LFOs, the scattered harmonic phases and the
    // per-copy detune of the unison — all the things that must differ between
    // two voices for a section to sound like a section rather than one player
    // amplified. Drawn from the global generator they were at the mercy of
    // whatever else in the program had drawn a number first.
    FRandom: TSedaiRandom;
    FHarmonicCount: Integer;

    // Per-harmonic state
    FHarmonicLevels: array[0..ADDITIVE_MAX_HARMONICS - 1] of Single;
    FHarmonicDetune: array[0..ADDITIVE_MAX_HARMONICS - 1] of Single;  // In cents
    FHarmonicPhases: array[0..ADDITIVE_MAX_HARMONICS - 1] of Single;

    // Optional per-harmonic envelopes
    FHarmonicEnvelopes: array[0..ADDITIVE_MAX_HARMONICS - 1] of TSedaiEnvelope;
    FUseHarmonicEnvelopes: Boolean;

    // Optional per-harmonic amplitude BREAKPOINT TRACKS (analysis/resynthesis).
    // When FUseHarmonicTracks is on, a harmonic's level over time is read from
    // its (time,value) track by linear interpolation (held past the last point)
    // instead of the static FHarmonicLevels[]. FTrackCursor advances forward with
    // FNoteTime so lookup is O(1)/sample. Designed to extend: a parallel
    // frequency track can be added later without disturbing this path.
    FHarmonicTrackT: array[0..ADDITIVE_MAX_HARMONICS - 1] of array of Single;
    FHarmonicTrackV: array[0..ADDITIVE_MAX_HARMONICS - 1] of array of Single;
    FTrackCursor: array[0..ADDITIVE_MAX_HARMONICS - 1] of Integer;
    FUseHarmonicTracks: Boolean;
    FNoteTime: Double;    // seconds since NoteOn (drives the tracks)

    // Per-voice HUMAN micro-instability + airy breath (opt-in; all 0 = inert, so
    // existing presets/tracks are unchanged). Pitch jitter (cents peak) + amplitude
    // shimmer (fraction peak) are smooth random-target LFOs at ~FModRate Hz, seeded
    // per-voice at NoteOn -> two voices on one note are never identical (ensemble
    // "chorus"). Breath = one-pole low-passed white noise (airy, not hiss).
    FJitterCents: Single;   // peak pitch deviation (cents); 0 = off
    FShimmerDepth: Single;  // peak amplitude deviation (fraction); 0 = off
    FModRate: Single;       // micro-instability rate (Hz)
    FBreathLevel: Single;   // breath layer level; 0 = off
    FBreathCut: Single;     // breath low-pass cutoff (Hz)
    FBreathCoeff: Single;   // cached one-pole coeff for FBreathCut
    FLfoPhase: Single;      // 0..1 ramp for the random-target LFOs
    FLfoPCur, FLfoPTgt: Single;   // pitch LFO current/target
    FLfoACur, FLfoATgt: Single;   // amp LFO current/target
    // Regular vibrato: a sinusoidal pitch oscillation (unlike the random jitter),
    // fading in after an onset delay the way a string player leans into it. 0 = off.
    FVibDepthCents: Single; // peak pitch deviation (cents); 0 = off
    FVibRate: Single;       // vibrato rate (Hz), ~5.5-6.5 for strings
    FVibDelay: Single;      // onset delay (s) before vibrato starts fading in
    FVibPhase: Single;      // 0..1 vibrato oscillator phase
    FPitchMod: Single;      // per-sample pitch multiplier (1 = none)
    FAmpMod: Single;        // per-sample amplitude multiplier (1 = none)
    FBreathState: Single;   // one-pole LP state for breath noise

    // Per-partial BANDWIDTH ("bandwidth-enhanced partials", Loris-style). Each
    // active harmonic gets its OWN one-pole low-passed white-noise amplitude
    // modulator (~FBandCut Hz), normalized to ~unit std, scaled by FBandDepth:
    // level *= 1 + FBandDepth * normNoise. This slight, decorrelated fast AM
    // broadens each partial into a narrow band -> the "metal" character (vs the
    // sterile "plastic" of pure sinusoids). FBandDepth 0 = off (inert; no RNG
    // touched, so existing presets/tracks stay bit-identical). ~0.04 = tuned.
    FBandDepth: Single;     // per-partial band depth (fraction peak-ish); 0 = off
    FBandCut: Single;       // band-noise low-pass cutoff (Hz)
    FBandCoeff: Single;     // cached one-pole coeff for FBandCut
    FBandNorm: Single;      // std-normalization factor for the LP noise
    FBandState: array[0..ADDITIVE_MAX_HARMONICS - 1] of Single;  // per-partial LP state

    // ---- Unison / ensemble ("section") -------------------------------------
    // One additive voice can render N slightly-detuned, phase- and vibrato-
    // decorrelated COPIES of itself, summed -> a lone note reads as a whole
    // section instead of one player (fixes the "cheap synth strings" comb).
    // FUnisonVoices = 1 -> completely inert: the single-voice path runs
    // bit-identical and no extra RNG is touched. Opt-in via SetUnison.
    // Detune is even-spaced across the ensemble PLUS a per-copy random jitter of
    // +/-(spacing/2), so intonation is IRREGULAR (incommensurate beating = many
    // players, not a regular chorus). Each copy has independent vibrato/jitter
    // (its own phase AND a small rate spread, so they drift apart over time) and
    // an optional staggered onset. The sum is normalized by 1/sqrt(N): a
    // decorrelated sum grows ~sqrt(N), so /N would make the section too quiet
    // relative to a soloist.
    // TODO (deferred to post-ear-check v1.1): per-copy TIMBRE spread (a small
    // random per-harmonic level tilt per copy) and per-copy static GAIN spread
    // (+/-1-2 dB) would decorrelate more than pitch alone. Pitch + independent
    // vibrato is the bulk of the "section" cue; add these only if it still reads
    // synthetic at ear-check.
    FUnisonVoices: Integer;   // 1 = off
    FUnisonDetune: Single;    // total intonation spread across the ensemble (cents)
    FUnisonAttack: Single;    // max onset stagger (s); 0 = all copies enter together
    FUniNorm: Single;         // cached 1/sqrt(FUnisonVoices)
    FUniDetuneRatio: array[0..ADDITIVE_MAX_UNISON - 1] of Single;  // static per-copy pitch ratio
    FUniOnset: array[0..ADDITIVE_MAX_UNISON - 1] of Single;        // per-copy entry time (s)
    FUniVibRate: array[0..ADDITIVE_MAX_UNISON - 1] of Single;      // per-copy vibrato rate (Hz)
    FUniModRate: array[0..ADDITIVE_MAX_UNISON - 1] of Single;      // per-copy jitter LFO rate (Hz)
    FUniVibPhase: array[0..ADDITIVE_MAX_UNISON - 1] of Single;     // per-copy vibrato phase
    FUniLfoPhase: array[0..ADDITIVE_MAX_UNISON - 1] of Single;     // per-copy jitter LFO ramp
    FUniLfoPCur, FUniLfoPTgt: array[0..ADDITIVE_MAX_UNISON - 1] of Single;  // pitch LFO
    FUniLfoACur, FUniLfoATgt: array[0..ADDITIVE_MAX_UNISON - 1] of Single;  // amp LFO
    FUniPitchMod, FUniAmpMod: array[0..ADDITIVE_MAX_UNISON - 1] of Single;  // per-copy modulators
    FUniPhases: array[0..ADDITIVE_MAX_UNISON - 1, 0..ADDITIVE_MAX_HARMONICS - 1] of Single;
    FUniBandState: array[0..ADDITIVE_MAX_UNISON - 1, 0..ADDITIVE_MAX_HARMONICS - 1] of Single;

    // Filtered-noise RESIDUAL (SMS/DDSP "stochastic" layer). White noise shaped by
    // a bank of RESIDUAL_BANDS band-pass filters with per-band gains (the measured
    // residual spectral envelope), scaled by FResidualLevel and the amp envelope.
    // This is the breath/bow/reed noise pure partials discard. More general than
    // FBreathLevel (a single one-pole): the band gains give the residual its
    // instrument-specific colour (airy vs hissy vs buzzy). Opt-in: level 0 = off
    // (no RNG touched -> existing presets bit-identical).
    FResidualLevel: Single;   // overall residual level; 0 = off
    FResidualGains: array[0..RESIDUAL_BANDS-1] of Single;   // per-band gains
    FResidualBP: array[0..RESIDUAL_BANDS-1] of record
      b0, b1, b2, a1, a2: Single;   // RBJ band-pass coefficients (a0-normalized)
      x1, x2, y1, y2: Single;       // biquad state (Direct Form I)
    end;

    // Main envelope
    FAmpEnvelope: TSedaiEnvelope;

    // State
    FCurrentPreset: TAdditivePreset;
    FNote: Integer;
    FVelocity: Single;
    FGateOpen: Boolean;
    FReleasing: Boolean;

    // Performance optimization
    FActiveHarmonics: Integer;   // Number of non-zero harmonics
    FNyquistLimit: Single;       // Half sample rate

    procedure SetHarmonicCount(AValue: Integer);
    procedure UpdateActiveHarmonics;
    procedure UpdateNyquistLimit;
    procedure ApplyPreset(APreset: TAdditivePreset);

    function CalculateSample: Single;
    function CalculateUnison: Single;
    function TrackLevel(AHarmonic: Integer): Single;
    procedure UpdateModulation;
    procedure UpdateUnisonModulation;
    procedure SeedUnison;
    procedure RecalcBreathCoeff;
    procedure RecalcBandCoeff;
    procedure RecalcResidualCoeffs;
    procedure ResetResidualState;

  public
    constructor Create; override;
    destructor Destroy; override;
    // Make this voice's randomness reproducible. Call it before NoteOn, which
    // is where the phases, the detune and the drift are drawn.
    procedure SetSeed(ASeed: QWord);

    // From TSedaiAudioObject
    procedure SampleRateChanged; override;

    // Note control
    procedure NoteOn(ANote: Integer; AVelocity: Single);
    procedure NoteOff;
    procedure Kill;

    // Generate samples
    function GenerateSample: Single; override;
    procedure GenerateBlock(AOutput: PSingle; AFrameCount: Integer); override;

    // Harmonic control
    procedure SetHarmonicLevel(AHarmonic: Integer; ALevel: Single);
    procedure SetHarmonicDetune(AHarmonic: Integer; ADetuneCents: Single);
    procedure SetAllHarmonics(const ALevels: array of Single);
    procedure ClearAllHarmonics;
    // Per-harmonic amplitude breakpoint track (analysis/resynthesis). Times in
    // seconds (ascending from 0), values 0..1. Sets FUseHarmonicTracks on and
    // seeds the static level with the track peak so active-harmonic/Nyquist
    // accounting stays correct.
    procedure SetHarmonicTrack(AHarmonic: Integer; const ATimes, AValues: array of Single);
    procedure ClearHarmonicTracks;
    // Per-voice human micro-instability. AJitterCents = peak pitch wobble (cents),
    // AShimmerDepth = peak amplitude wobble (fraction), ARateHz = ~5. All 0 = off.
    procedure SetMicroInstability(AJitterCents, AShimmerDepth, ARateHz: Single);
    // Regular vibrato: sinusoidal pitch oscillation (distinct from the random jitter).
    // ADepthCents = peak deviation (strings ~15-30c), ARateHz ~5.5-6.5, ADelaySec =
    // onset delay before it fades in (~0.3 s). ADepthCents 0 = off.
    procedure SetVibrato(ADepthCents, ARateHz, ADelaySec: Single);
    // Unison / ensemble ("section"): render AVoices detuned + phase/vibrato-
    // decorrelated copies of this voice (1 = off, bit-identical single voice).
    // ADetuneCents = total intonation spread across the ensemble (~10-20c for
    // strings); AAttackSpreadSec = max per-copy onset stagger (0 = together).
    // Turns a single held note into a section. Combine with SetVibrato/
    // SetMicroInstability so each copy's vibrato/jitter is independent.
    procedure SetUnison(AVoices: Integer; ADetuneCents, AAttackSpreadSec: Single);
    // Airy breath layer. ALevel 0 = off; ACutoffHz shapes it ("air" low / "hiss" high).
    procedure SetBreath(ALevel, ACutoffHz: Single);
    // Per-partial bandwidth ("metal"). ADepth = fractional AM depth per partial
    // (~0.04 tuned; 0 = off), ACutoffHz = band-noise low-pass (~45 Hz). Each
    // active harmonic gets an independent LP-noise modulator.
    procedure SetBandwidth(ADepth, ACutoffHz: Single);
    // Filtered-noise residual (SMS/DDSP stochastic layer). ALevel 0 = off. AGains =
    // per-band gains (up to RESIDUAL_BANDS, ~octave-spaced 250 Hz..8 kHz) = the
    // residual spectral envelope. White noise band-shaped by these + scaled by the
    // amp envelope is added to the partials (breath/bow/reed noise).
    procedure SetResidual(ALevel: Single; const AGains: array of Single);
    function GetHarmonicLevel(AHarmonic: Integer): Single;
    function GetHarmonicDetune(AHarmonic: Integer): Single;
    function GetResidualGain(ABand: Integer): Single;
    function GetHarmonicEnvelope(AHarmonic: Integer): TSedaiEnvelope;

    // Preset control
    procedure LoadPreset(APreset: TAdditivePreset);
    procedure LoadSineWave;
    procedure LoadSawWave;
    procedure LoadSquareWave;
    procedure LoadTriangleWave;
    procedure LoadOrganWave;
    procedure LoadBellWave;
    procedure LoadStringsWave;
    procedure LoadChoirWave;
    procedure LoadBrassWave;
    procedure LoadFluteWave;

    // Properties
    property HarmonicCount: Integer read FHarmonicCount write SetHarmonicCount;
    property UseHarmonicEnvelopes: Boolean read FUseHarmonicEnvelopes write FUseHarmonicEnvelopes;
    property UseHarmonicTracks: Boolean read FUseHarmonicTracks write FUseHarmonicTracks;
    property JitterCents: Single read FJitterCents write FJitterCents;
    property ShimmerDepth: Single read FShimmerDepth write FShimmerDepth;
    property ModRate: Single read FModRate write FModRate;
    property VibDepthCents: Single read FVibDepthCents;
    property VibRateHz: Single read FVibRate;
    property VibDelaySec: Single read FVibDelay;
    property UnisonVoices: Integer read FUnisonVoices;
    property UnisonDetune: Single read FUnisonDetune;
    property UnisonAttackSpread: Single read FUnisonAttack;
    property BreathLevel: Single read FBreathLevel write FBreathLevel;
    property BreathCutoff: Single read FBreathCut;
    property BandDepth: Single read FBandDepth write FBandDepth;
    property BandCutoff: Single read FBandCut;
    property ResidualLevel: Single read FResidualLevel;
    property AmpEnvelope: TSedaiEnvelope read FAmpEnvelope;
    property CurrentPreset: TAdditivePreset read FCurrentPreset;
    property Note: Integer read FNote;
    property Velocity: Single read FVelocity;
    property GateOpen: Boolean read FGateOpen;
    property Releasing: Boolean read FReleasing;
  end;

implementation

{ TSedaiAdditiveGenerator }

constructor TSedaiAdditiveGenerator.Create;
var
  I: Integer;
begin
  inherited Create;

  FRandom.Seed(SedaiNextSeed);
  FHarmonicCount := ADDITIVE_DEFAULT_HARMONICS;
  FCurrentPreset := apSaw;
  FNote := -1;
  FVelocity := 1.0;
  FGateOpen := False;
  FReleasing := False;
  FUseHarmonicEnvelopes := False;
  FUseHarmonicTracks := False;
  FNoteTime := 0;
  FJitterCents := 0; FShimmerDepth := 0; FModRate := 5.0;
  FVibDepthCents := 0; FVibRate := 6.0; FVibDelay := 0.3; FVibPhase := 0;
  FBreathLevel := 0; FBreathCut := 4000;
  FLfoPhase := 0; FLfoPCur := 0; FLfoPTgt := 0; FLfoACur := 0; FLfoATgt := 0;
  FPitchMod := 1; FAmpMod := 1; FBreathState := 0;
  FBandDepth := 0; FBandCut := 45;
  FUnisonVoices := 1; FUnisonDetune := 0; FUnisonAttack := 0; FUniNorm := 1;
  FResidualLevel := 0;
  for I := 0 to RESIDUAL_BANDS - 1 do FResidualGains[I] := 0;
  RecalcBreathCoeff;
  RecalcBandCoeff;
  RecalcResidualCoeffs;
  ResetResidualState;
  FActiveHarmonics := 0;
  FNyquistLimit := FSampleRate * 0.5;

  // Initialize harmonic arrays
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    FHarmonicLevels[I] := 0;
    FHarmonicDetune[I] := 0;
    FHarmonicPhases[I] := 0;
    FHarmonicTrackT[I] := nil;
    FHarmonicTrackV[I] := nil;
    FTrackCursor[I] := 0;
    FBandState[I] := 0;
    FHarmonicEnvelopes[I] := TSedaiEnvelope.Create;
    FHarmonicEnvelopes[I].SetSampleRate(FSampleRate);
  end;

  // Create main amplitude envelope
  FAmpEnvelope := TSedaiEnvelope.Create;
  FAmpEnvelope.SetSampleRate(FSampleRate);
  FAmpEnvelope.AttackTime := 0.01;
  FAmpEnvelope.DecayTime := 0.2;
  FAmpEnvelope.SustainLevel := 0.7;
  FAmpEnvelope.ReleaseTime := 0.3;

  // Default to saw wave
  LoadSawWave;
end;

procedure TSedaiAdditiveGenerator.SetSeed(ASeed: QWord);
begin
  FRandom.Seed(ASeed);
end;

destructor TSedaiAdditiveGenerator.Destroy;
var
  I: Integer;
begin
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
    FHarmonicEnvelopes[I].Free;
  FAmpEnvelope.Free;
  inherited Destroy;
end;

procedure TSedaiAdditiveGenerator.SampleRateChanged;
var
  I: Integer;
begin
  inherited SampleRateChanged;
  UpdateNyquistLimit;
  RecalcBreathCoeff;
  RecalcBandCoeff;
  RecalcResidualCoeffs;
  FAmpEnvelope.SetSampleRate(FSampleRate);
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
    FHarmonicEnvelopes[I].SetSampleRate(FSampleRate);
end;

procedure TSedaiAdditiveGenerator.SetHarmonicCount(AValue: Integer);
begin
  FHarmonicCount := EnsureRange(AValue, 1, ADDITIVE_MAX_HARMONICS);
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.UpdateActiveHarmonics;
var
  I: Integer;
begin
  FActiveHarmonics := 0;
  for I := 0 to FHarmonicCount - 1 do
    if FHarmonicLevels[I] > 0.001 then
      FActiveHarmonics := I + 1;
end;

procedure TSedaiAdditiveGenerator.UpdateNyquistLimit;
begin
  FNyquistLimit := FSampleRate * 0.5;
end;

procedure TSedaiAdditiveGenerator.ApplyPreset(APreset: TAdditivePreset);
begin
  FCurrentPreset := APreset;
  case APreset of
    apSine: LoadSineWave;
    apSaw: LoadSawWave;
    apSquare: LoadSquareWave;
    apTriangle: LoadTriangleWave;
    apOrgan: LoadOrganWave;
    apBell: LoadBellWave;
    apStrings: LoadStringsWave;
    apChoir: LoadChoirWave;
    apBrass: LoadBrassWave;
    apFlute: LoadFluteWave;
  end;
end;

function TSedaiAdditiveGenerator.CalculateSample: Single;
var
  I: Integer;
  HarmonicFreq: Single;
  PhaseInc: Single;
  HarmonicLevel: Single;
  DetuneRatio: Single;
begin
  Result := 0;

  for I := 0 to FActiveHarmonics - 1 do
  begin
    if FHarmonicLevels[I] > 0.001 then
    begin
      // Get harmonic level: breakpoint track > per-harmonic env > static level
      if FUseHarmonicTracks then
        HarmonicLevel := TrackLevel(I)
      else if FUseHarmonicEnvelopes then
        HarmonicLevel := FHarmonicLevels[I] * FHarmonicEnvelopes[I].Process
      else
        HarmonicLevel := FHarmonicLevels[I];

      // Per-partial bandwidth: each active harmonic gets an independent one-pole
      // LP-noise AM (opt-in; broadens the partial -> "metal"). State advances per
      // active harmonic every sample so the bands stay decorrelated.
      if FBandDepth > 0 then
      begin
        FBandState[I] := FBandState[I] + FBandCoeff * ((FRandom.NextBipolar) - FBandState[I]);
        HarmonicLevel := HarmonicLevel * (1.0 + FBandDepth * FBandNorm * FBandState[I]);
        if HarmonicLevel < 0 then HarmonicLevel := 0;
      end;

      if HarmonicLevel > 0.001 then
      begin
        // Calculate harmonic frequency with detune
        // Detune in cents: 1 cent = 2^(1/1200)
        if FHarmonicDetune[I] <> 0 then
          DetuneRatio := Power(2, FHarmonicDetune[I] / 1200)
        else
          DetuneRatio := 1.0;

        HarmonicFreq := FFrequency * FPitchMod * (I + 1) * DetuneRatio;

        // Check Nyquist limit
        if HarmonicFreq < FNyquistLimit then
        begin
          // Add harmonic contribution (sine wave)
          Result := Result + Sin(FHarmonicPhases[I] * TWO_PI) * HarmonicLevel;

          // Advance phase
          PhaseInc := HarmonicFreq / FSampleRate;
          FHarmonicPhases[I] := FHarmonicPhases[I] + PhaseInc;
          if FHarmonicPhases[I] >= 1.0 then
            FHarmonicPhases[I] := FHarmonicPhases[I] - 1.0;
        end;
      end;
    end;
  end;
end;

// Ensemble sum: render FUnisonVoices detuned + decorrelated copies of the
// harmonic stack and mix them. The harmonic LEVEL (track/env/static) is shared
// across copies (the section plays one timbre); only phase, static detune,
// per-copy pitch modulation and per-partial band AM differ. Copies are combined
// with their own amplitude shimmer + optional staggered onset, then normalized
// by 1/sqrt(N). Called only when FUnisonVoices >= 2.
function TSedaiAdditiveGenerator.CalculateUnison: Single;
var
  I, c: Integer;
  baseLevel, lvl, harmDetune, HarmonicFreq, PhaseInc, fade, acc: Single;
  sumc: array[0..ADDITIVE_MAX_UNISON - 1] of Single;
begin
  for c := 0 to FUnisonVoices - 1 do sumc[c] := 0;

  for I := 0 to FActiveHarmonics - 1 do
  begin
    if FHarmonicLevels[I] <= 0.001 then Continue;

    // Shared base level for this harmonic, computed ONCE (track > env > static)
    // so the breakpoint-track cursor advances exactly once per sample.
    if FUseHarmonicTracks then
      baseLevel := TrackLevel(I)
    else if FUseHarmonicEnvelopes then
      baseLevel := FHarmonicLevels[I] * FHarmonicEnvelopes[I].Process
    else
      baseLevel := FHarmonicLevels[I];
    if baseLevel <= 0.001 then Continue;

    if FHarmonicDetune[I] <> 0 then
      harmDetune := Power(2, FHarmonicDetune[I] / 1200)
    else
      harmDetune := 1.0;

    for c := 0 to FUnisonVoices - 1 do
    begin
      lvl := baseLevel;
      // Per-copy band AM ("metal"), decorrelated per copy.
      if FBandDepth > 0 then
      begin
        FUniBandState[c][I] := FUniBandState[c][I] +
          FBandCoeff * ((FRandom.NextBipolar) - FUniBandState[c][I]);
        lvl := lvl * (1.0 + FBandDepth * FBandNorm * FUniBandState[c][I]);
        if lvl < 0 then lvl := 0;
      end;

      HarmonicFreq := FFrequency * FUniPitchMod[c] * FUniDetuneRatio[c] * (I + 1) * harmDetune;
      if HarmonicFreq < FNyquistLimit then
      begin
        sumc[c] := sumc[c] + Sin(FUniPhases[c][I] * TWO_PI) * lvl;
        PhaseInc := HarmonicFreq / FSampleRate;
        FUniPhases[c][I] := FUniPhases[c][I] + PhaseInc;
        if FUniPhases[c][I] >= 1.0 then FUniPhases[c][I] := FUniPhases[c][I] - 1.0;
      end;
    end;
  end;

  // Combine: per-copy shimmer + optional staggered onset fade, then /sqrt(N).
  acc := 0;
  for c := 0 to FUnisonVoices - 1 do
  begin
    if FUnisonAttack > 0 then
    begin
      fade := (FNoteTime - FUniOnset[c]) / 0.005;   // ~5 ms fade-in at each entry
      if fade < 0 then fade := 0 else if fade > 1 then fade := 1;
    end
    else
      fade := 1.0;
    acc := acc + sumc[c] * FUniAmpMod[c] * fade;
  end;
  Result := acc * FUniNorm;
end;

procedure TSedaiAdditiveGenerator.NoteOn(ANote: Integer; AVelocity: Single);
var
  I: Integer;
begin
  FNote := ANote;
  FVelocity := EnsureRange(AVelocity, 0, 1);
  FGateOpen := True;
  FReleasing := False;

  // Calculate frequency from MIDI note
  FFrequency := 440 * Power(2, (ANote - 69) / 12);

  // Reset phases and per-harmonic track state
  FNoteTime := 0;
  // Per-voice micro-instability: fresh random LFO seeds so voices differ. Only
  // touch the RNG when the feature is active, so with everything off the voice is
  // bit-identical to before (and deterministic RandSeed-based tests are unaffected).
  FLfoPhase := 0; FPitchMod := 1; FAmpMod := 1; FBreathState := 0;
  FLfoPCur := 0; FLfoPTgt := 0; FLfoACur := 0; FLfoATgt := 0;
  ResetResidualState;
  // Per-voice DECORRELATION for the living/vibrato layer: give each voice a random
  // vibrato phase (no lock-step wobble) and, crucially, random INITIAL HARMONIC
  // PHASES. With zero phases every voice is a bit-identical waveform, so a chord /
  // section sums coherently -> the "cheap synth strings" comb sound. Decorrelating
  // the phases makes stacked voices read as several distinct players.
  if FUnisonVoices >= 2 then
    // Ensemble mode: seed independent per-copy state (own detune/phases/vibrato/
    // jitter). Kept separate so the N=1 path below stays bit-identical (its exact
    // RNG call order is unchanged when unison is off).
    SeedUnison
  else if (FJitterCents > 0) or (FShimmerDepth > 0) or (FBreathLevel > 0) or (FVibDepthCents > 0) then
  begin
    FLfoPCur := FRandom.NextBipolar; FLfoPTgt := FRandom.NextBipolar;
    FLfoACur := FRandom.NextBipolar; FLfoATgt := FRandom.NextBipolar;
    FVibPhase := FRandom.NextFloat;
    for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
    begin
      FHarmonicPhases[I] := FRandom.NextFloat;      // 0..1 (turns); decorrelates stacked voices
      FTrackCursor[I] := 0; FBandState[I] := 0;
      if FUseHarmonicEnvelopes then FHarmonicEnvelopes[I].Trigger;
    end;
  end
  else
  begin
    FVibPhase := 0;
    for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
    begin
      FHarmonicPhases[I] := 0;
      FTrackCursor[I] := 0;
      FBandState[I] := 0;
      if FUseHarmonicEnvelopes then
        FHarmonicEnvelopes[I].Trigger;
    end;
  end;

  // Trigger amplitude envelope
  FAmpEnvelope.Trigger;
end;

procedure TSedaiAdditiveGenerator.NoteOff;
var
  I: Integer;
begin
  FGateOpen := False;
  FReleasing := True;

  // Release amplitude envelope
  FAmpEnvelope.Release;

  // Release harmonic envelopes if used
  if FUseHarmonicEnvelopes then
    for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
      FHarmonicEnvelopes[I].Release;
end;

procedure TSedaiAdditiveGenerator.Kill;
var
  I: Integer;
begin
  FGateOpen := False;
  FReleasing := False;
  FNote := -1;
  FNoteTime := 0;

  // Reset all phases
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    FHarmonicPhases[I] := 0;
    FTrackCursor[I] := 0;
    FBandState[I] := 0;
    FHarmonicEnvelopes[I].Reset;
  end;
  ResetResidualState;

  FAmpEnvelope.Reset;
end;

function TSedaiAdditiveGenerator.GenerateSample: Single;
var
  EnvValue: Single;
  ri: Integer;
  noise, bp, resAcc: Single;
begin
  if (not FGateOpen) and (not FReleasing) then
  begin
    Result := 0;
    Exit;
  end;

  // Get envelope value
  EnvValue := FAmpEnvelope.Process;

  // Check if envelope has finished
  if FReleasing and (FAmpEnvelope.State = esIdle) then
  begin
    FReleasing := False;
    Result := 0;
    Exit;
  end;

  // Per-voice micro-instability: update pitch/amp modulators (before generating)
  UpdateModulation;

  // Calculate sample (CalculateSample applies FPitchMod to the harmonic freqs;
  // FAmpMod is the amplitude shimmer). tracks read the CURRENT FNoteTime.
  // In ensemble mode CalculateUnison sums the detuned copies and folds each
  // copy's own shimmer in (so no outer FAmpMod). breath/residual below stay a
  // single shared "air" layer over the whole section.
  if FUnisonVoices >= 2 then
    Result := CalculateUnison * EnvValue * FVelocity * FAmplitude
  else
    Result := CalculateSample * EnvValue * FVelocity * FAmplitude * FAmpMod;

  // Airy breath layer: one-pole low-passed white noise, following the envelope
  if FBreathLevel > 0 then
  begin
    FBreathState := FBreathState + FBreathCoeff * ((FRandom.NextBipolar) - FBreathState);
    Result := Result + FBreathState * FBreathLevel * EnvValue * FVelocity * FAmplitude;
  end;

  // Filtered-noise residual (SMS/DDSP stochastic layer): one white-noise source
  // through the band-pass bank, summed with the per-band gains = the residual
  // spectral envelope. Following the amp envelope so it fades with the note.
  if FResidualLevel > 0 then
  begin
    noise := FRandom.NextBipolar;
    resAcc := 0;
    for ri := 0 to RESIDUAL_BANDS - 1 do
      with FResidualBP[ri] do
      begin
        // RBJ band-pass, Direct Form I (double-free single precision is fine here)
        bp := b0 * noise + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;
        x2 := x1; x1 := noise;
        y2 := y1; y1 := bp;
        resAcc := resAcc + FResidualGains[ri] * bp;
      end;
    Result := Result + resAcc * FResidualLevel * EnvValue * FVelocity * FAmplitude;
  end;

  // Advance the per-note clock that drives the breakpoint tracks
  if FSampleRate > 0 then
    FNoteTime := FNoteTime + 1.0 / FSampleRate;
end;

procedure TSedaiAdditiveGenerator.GenerateBlock(AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
begin
  for I := 0 to AFrameCount - 1 do
    AOutput[I] := GenerateSample;
end;

procedure TSedaiAdditiveGenerator.SetHarmonicLevel(AHarmonic: Integer; ALevel: Single);
begin
  if (AHarmonic >= 0) and (AHarmonic < ADDITIVE_MAX_HARMONICS) then
  begin
    FHarmonicLevels[AHarmonic] := EnsureRange(ALevel, 0, 1);
    UpdateActiveHarmonics;
  end;
end;

procedure TSedaiAdditiveGenerator.SetHarmonicDetune(AHarmonic: Integer; ADetuneCents: Single);
begin
  if (AHarmonic >= 0) and (AHarmonic < ADDITIVE_MAX_HARMONICS) then
    FHarmonicDetune[AHarmonic] := ADetuneCents;
end;

procedure TSedaiAdditiveGenerator.SetAllHarmonics(const ALevels: array of Single);
var
  I: Integer;
begin
  ClearAllHarmonics;
  for I := 0 to Min(High(ALevels), ADDITIVE_MAX_HARMONICS - 1) do
    FHarmonicLevels[I] := EnsureRange(ALevels[I], 0, 1);
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.ClearAllHarmonics;
var
  I: Integer;
begin
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    FHarmonicLevels[I] := 0;
    FHarmonicDetune[I] := 0;
  end;
  FActiveHarmonics := 0;
end;

// Interpolated amplitude of a harmonic's breakpoint track at the current
// FNoteTime. Linear between points; holds the first/last value outside the
// range. FTrackCursor advances monotonically with FNoteTime (O(1)/sample).
function TSedaiAdditiveGenerator.TrackLevel(AHarmonic: Integer): Single;
var
  n, c: Integer;
  t0, t1, v0, v1: Single;
begin
  n := Length(FHarmonicTrackT[AHarmonic]);
  if n = 0 then Exit(FHarmonicLevels[AHarmonic]);   // no track: static fallback
  if n = 1 then Exit(FHarmonicTrackV[AHarmonic][0]);

  if FNoteTime >= FHarmonicTrackT[AHarmonic][n - 1] then
    Exit(FHarmonicTrackV[AHarmonic][n - 1]);        // hold last value past the end

  c := FTrackCursor[AHarmonic];
  if c < 0 then c := 0
  else if c > n - 2 then c := n - 2;
  // advance forward to the segment containing FNoteTime (monotonic clock)
  while (c < n - 2) and (FNoteTime > FHarmonicTrackT[AHarmonic][c + 1]) do
    Inc(c);
  FTrackCursor[AHarmonic] := c;

  t0 := FHarmonicTrackT[AHarmonic][c];
  t1 := FHarmonicTrackT[AHarmonic][c + 1];
  v0 := FHarmonicTrackV[AHarmonic][c];
  v1 := FHarmonicTrackV[AHarmonic][c + 1];
  if (FNoteTime <= t0) or (t1 <= t0) then Exit(v0);
  Result := v0 + (v1 - v0) * ((FNoteTime - t0) / (t1 - t0));
end;

procedure TSedaiAdditiveGenerator.SetHarmonicTrack(AHarmonic: Integer;
  const ATimes, AValues: array of Single);
var
  n, i: Integer;
  peak: Single;
begin
  if (AHarmonic < 0) or (AHarmonic >= ADDITIVE_MAX_HARMONICS) then Exit;
  n := Length(ATimes);
  if Length(AValues) < n then n := Length(AValues);
  SetLength(FHarmonicTrackT[AHarmonic], n);
  SetLength(FHarmonicTrackV[AHarmonic], n);
  peak := 0;
  for i := 0 to n - 1 do
  begin
    FHarmonicTrackT[AHarmonic][i] := ATimes[i];
    FHarmonicTrackV[AHarmonic][i] := AValues[i];
    if AValues[i] > peak then peak := AValues[i];
  end;
  FTrackCursor[AHarmonic] := 0;
  // seed the static level with the track peak so active-harmonic / Nyquist
  // accounting (which reads FHarmonicLevels) still counts this harmonic
  FHarmonicLevels[AHarmonic] := peak;
  FUseHarmonicTracks := True;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.ClearHarmonicTracks;
var
  i: Integer;
begin
  for i := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    FHarmonicTrackT[i] := nil;
    FHarmonicTrackV[i] := nil;
    FTrackCursor[i] := 0;
  end;
  FUseHarmonicTracks := False;
end;

procedure TSedaiAdditiveGenerator.RecalcBreathCoeff;
begin
  if (FSampleRate > 0) and (FBreathCut > 0) then
    FBreathCoeff := 1.0 - Exp(-2.0 * Pi * FBreathCut / FSampleRate)
  else
    FBreathCoeff := 1.0;
end;

// One-pole coeff + std-normalization for the per-partial band noise. A one-pole
// LP of white noise (uniform [-1,1], variance 1/3) has output variance
// a/(2-a)*(1/3); normalize by sqrt(3*(2-a)/a) so FBandDepth is the true
// fractional AM depth regardless of cutoff/sample-rate.
procedure TSedaiAdditiveGenerator.RecalcBandCoeff;
var
  a: Single;
begin
  if (FSampleRate > 0) and (FBandCut > 0) then
    a := 1.0 - Exp(-2.0 * Pi * FBandCut / FSampleRate)
  else
    a := 1.0;
  FBandCoeff := a;
  if a > 0 then
    FBandNorm := Sqrt(3.0 * (2.0 - a) / a)
  else
    FBandNorm := 1.0;
end;

// Advance the two random-target LFOs and derive the per-sample pitch/amp
// modulators. Inert (mults = 1) while both depths are 0.
procedure TSedaiAdditiveGenerator.UpdateModulation;
var
  inc, lp, la, vib, venv: Single;
begin
  if FUnisonVoices >= 2 then
  begin
    UpdateUnisonModulation;
    Exit;
  end;
  if (FJitterCents <= 0) and (FShimmerDepth <= 0) and (FVibDepthCents <= 0) then
  begin
    FPitchMod := 1.0; FAmpMod := 1.0;
    Exit;
  end;
  FAmpMod := 1.0; FPitchMod := 1.0;

  // --- random micro-instability (jitter / shimmer) ---
  if (FJitterCents > 0) or (FShimmerDepth > 0) then
  begin
    if FSampleRate > 0 then inc := FModRate / FSampleRate else inc := 0;
    FLfoPhase := FLfoPhase + inc;
    if FLfoPhase >= 1.0 then
    begin
      FLfoPhase := FLfoPhase - 1.0;
      FLfoPCur := FLfoPTgt; FLfoPTgt := FRandom.NextBipolar;
      FLfoACur := FLfoATgt; FLfoATgt := FRandom.NextBipolar;
    end;
    lp := FLfoPCur + (FLfoPTgt - FLfoPCur) * FLfoPhase;   // -1..1, smooth
    la := FLfoACur + (FLfoATgt - FLfoACur) * FLfoPhase;
    if FJitterCents > 0 then FPitchMod := Power(2, (FJitterCents * lp) / 1200);
    if FShimmerDepth > 0 then FAmpMod := 1.0 + FShimmerDepth * la;
  end;

  // --- regular vibrato (sinusoidal pitch), onset-delayed then ramping in ---
  if FVibDepthCents > 0 then
  begin
    if FSampleRate > 0 then FVibPhase := FVibPhase + FVibRate / FSampleRate;
    if FVibPhase >= 1.0 then FVibPhase := FVibPhase - 1.0;
    venv := (FNoteTime - FVibDelay) / 0.4;               // fade in over ~0.4 s
    if venv < 0 then venv := 0 else if venv > 1 then venv := 1;
    vib := Sin(2 * Pi * FVibPhase) * venv;
    FPitchMod := FPitchMod * Power(2, (FVibDepthCents * vib) / 1200);
  end;
end;

// Per-copy modulation for ensemble mode: same jitter+vibrato law as the scalar
// path, but each copy carries its own LFO/vibrato state and rate, so the section
// wobbles independently (not in lock-step). Fills FUniPitchMod/FUniAmpMod, read
// by CalculateUnison. The vibrato onset delay is shared (the section leans in
// together). Static per-copy detune lives in FUniDetuneRatio (set at NoteOn).
procedure TSedaiAdditiveGenerator.UpdateUnisonModulation;
var
  c: Integer;
  inc, lp, la, vib, venv: Single;
begin
  for c := 0 to FUnisonVoices - 1 do
  begin
    FUniPitchMod[c] := 1.0; FUniAmpMod[c] := 1.0;
    if (FJitterCents <= 0) and (FShimmerDepth <= 0) and (FVibDepthCents <= 0) then
      Continue;

    // --- random micro-instability (per-copy) ---
    if (FJitterCents > 0) or (FShimmerDepth > 0) then
    begin
      if FSampleRate > 0 then inc := FUniModRate[c] / FSampleRate else inc := 0;
      FUniLfoPhase[c] := FUniLfoPhase[c] + inc;
      if FUniLfoPhase[c] >= 1.0 then
      begin
        FUniLfoPhase[c] := FUniLfoPhase[c] - 1.0;
        FUniLfoPCur[c] := FUniLfoPTgt[c]; FUniLfoPTgt[c] := FRandom.NextBipolar;
        FUniLfoACur[c] := FUniLfoATgt[c]; FUniLfoATgt[c] := FRandom.NextBipolar;
      end;
      lp := FUniLfoPCur[c] + (FUniLfoPTgt[c] - FUniLfoPCur[c]) * FUniLfoPhase[c];
      la := FUniLfoACur[c] + (FUniLfoATgt[c] - FUniLfoACur[c]) * FUniLfoPhase[c];
      if FJitterCents > 0 then FUniPitchMod[c] := Power(2, (FJitterCents * lp) / 1200);
      if FShimmerDepth > 0 then FUniAmpMod[c] := 1.0 + FShimmerDepth * la;
    end;

    // --- regular vibrato (per-copy phase + rate), onset-delayed then ramping ---
    if FVibDepthCents > 0 then
    begin
      if FSampleRate > 0 then FUniVibPhase[c] := FUniVibPhase[c] + FUniVibRate[c] / FSampleRate;
      if FUniVibPhase[c] >= 1.0 then FUniVibPhase[c] := FUniVibPhase[c] - 1.0;
      venv := (FNoteTime - FVibDelay) / 0.4;                // fade in over ~0.4 s
      if venv < 0 then venv := 0 else if venv > 1 then venv := 1;
      vib := Sin(2 * Pi * FUniVibPhase[c]) * venv;
      FUniPitchMod[c] := FUniPitchMod[c] * Power(2, (FVibDepthCents * vib) / 1200);
    end;
  end;
end;

// Seed independent per-copy state at NoteOn (ensemble mode only). Even-spaced
// intonation + per-copy random jitter of +/-(spacing/2) (irregular tuning);
// per-copy vibrato/jitter rate spread of +/-4% (so copies drift apart); random
// vibrato phase and initial harmonic phases (decorrelation); optional staggered
// onset. Caches FUniNorm = 1/sqrt(N).
procedure TSedaiAdditiveGenerator.SeedUnison;
var
  c, h: Integer;
  baseCents, spacing, jit: Single;
begin
  if FUnisonVoices < 2 then begin FUniNorm := 1.0; Exit; end;
  FUniNorm := 1.0 / Sqrt(FUnisonVoices);
  spacing := FUnisonDetune / (FUnisonVoices - 1);
  for c := 0 to FUnisonVoices - 1 do
  begin
    baseCents := (c - (FUnisonVoices - 1) / 2) * spacing;
    jit := (FRandom.NextFloat - 0.5) * spacing;                       // +/- spacing/2
    FUniDetuneRatio[c] := Power(2, (baseCents + jit) / 1200);
    FUniVibRate[c] := FVibRate * (1 + 0.04 * (FRandom.NextBipolar));
    FUniModRate[c] := FModRate * (1 + 0.04 * (FRandom.NextBipolar));
    FUniVibPhase[c] := FRandom.NextFloat;
    FUniLfoPhase[c] := 0;
    FUniLfoPCur[c] := FRandom.NextBipolar; FUniLfoPTgt[c] := FRandom.NextBipolar;
    FUniLfoACur[c] := FRandom.NextBipolar; FUniLfoATgt[c] := FRandom.NextBipolar;
    FUniPitchMod[c] := 1.0; FUniAmpMod[c] := 1.0;
    if FUnisonAttack > 0 then
      FUniOnset[c] := (c / (FUnisonVoices - 1)) * FUnisonAttack
    else
      FUniOnset[c] := 0;
    for h := 0 to ADDITIVE_MAX_HARMONICS - 1 do
    begin
      FUniPhases[c][h] := FRandom.NextFloat;   // decorrelate stacked copies
      FUniBandState[c][h] := 0;
    end;
  end;
  // Shared per-harmonic state (amplitude tracks + optional envelopes) resets too.
  for h := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    FTrackCursor[h] := 0;
    if FUseHarmonicEnvelopes then FHarmonicEnvelopes[h].Trigger;
  end;
end;

procedure TSedaiAdditiveGenerator.SetVibrato(ADepthCents, ARateHz, ADelaySec: Single);
begin
  if ADepthCents < 0 then ADepthCents := 0;
  FVibDepthCents := ADepthCents;
  if ARateHz > 0 then FVibRate := ARateHz;
  if ADelaySec >= 0 then FVibDelay := ADelaySec;
end;

procedure TSedaiAdditiveGenerator.SetUnison(AVoices: Integer; ADetuneCents, AAttackSpreadSec: Single);
begin
  if AVoices < 1 then AVoices := 1
  else if AVoices > ADDITIVE_MAX_UNISON then AVoices := ADDITIVE_MAX_UNISON;
  FUnisonVoices := AVoices;
  if ADetuneCents < 0 then ADetuneCents := 0;
  FUnisonDetune := ADetuneCents;
  if AAttackSpreadSec < 0 then AAttackSpreadSec := 0;
  FUnisonAttack := AAttackSpreadSec;
  if FUnisonVoices > 1 then FUniNorm := 1.0 / Sqrt(FUnisonVoices) else FUniNorm := 1.0;
end;

procedure TSedaiAdditiveGenerator.SetMicroInstability(AJitterCents, AShimmerDepth, ARateHz: Single);
begin
  if AJitterCents < 0 then AJitterCents := 0;
  if AShimmerDepth < 0 then AShimmerDepth := 0;
  FJitterCents := AJitterCents;
  FShimmerDepth := AShimmerDepth;
  if ARateHz > 0 then FModRate := ARateHz;
end;

procedure TSedaiAdditiveGenerator.SetBreath(ALevel, ACutoffHz: Single);
begin
  if ALevel < 0 then ALevel := 0;
  FBreathLevel := ALevel;
  if ACutoffHz > 0 then FBreathCut := ACutoffHz;
  RecalcBreathCoeff;
end;

procedure TSedaiAdditiveGenerator.SetBandwidth(ADepth, ACutoffHz: Single);
begin
  if ADepth < 0 then ADepth := 0;
  FBandDepth := ADepth;
  if ACutoffHz > 0 then FBandCut := ACutoffHz;
  RecalcBandCoeff;
end;

// RBJ band-pass (constant 0 dB peak) coefficients for each residual band.
procedure TSedaiAdditiveGenerator.RecalcResidualCoeffs;
var
  i: Integer;
  w0, cw, sw, alpha, a0: Single;
begin
  for i := 0 to RESIDUAL_BANDS - 1 do
  begin
    if (FSampleRate > 0) and (RESIDUAL_BAND_HZ[i] < FSampleRate * 0.45) then
    begin
      w0 := TWO_PI * RESIDUAL_BAND_HZ[i] / FSampleRate;
      cw := Cos(w0); sw := Sin(w0);
      alpha := sw / (2.0 * RESIDUAL_BAND_Q);
      a0 := 1.0 + alpha;
      FResidualBP[i].b0 :=  alpha / a0;
      FResidualBP[i].b1 :=  0.0;
      FResidualBP[i].b2 := -alpha / a0;
      FResidualBP[i].a1 := (-2.0 * cw) / a0;
      FResidualBP[i].a2 := (1.0 - alpha) / a0;
    end
    else
    begin
      // band above Nyquist: make it a silent pass-through (no contribution)
      FResidualBP[i].b0 := 0; FResidualBP[i].b1 := 0; FResidualBP[i].b2 := 0;
      FResidualBP[i].a1 := 0; FResidualBP[i].a2 := 0;
    end;
  end;
end;

procedure TSedaiAdditiveGenerator.ResetResidualState;
var
  i: Integer;
begin
  for i := 0 to RESIDUAL_BANDS - 1 do
  begin
    FResidualBP[i].x1 := 0; FResidualBP[i].x2 := 0;
    FResidualBP[i].y1 := 0; FResidualBP[i].y2 := 0;
  end;
end;

procedure TSedaiAdditiveGenerator.SetResidual(ALevel: Single; const AGains: array of Single);
var
  i, n: Integer;
begin
  if ALevel < 0 then ALevel := 0;
  FResidualLevel := ALevel;
  n := Length(AGains);
  for i := 0 to RESIDUAL_BANDS - 1 do
    if i < n then FResidualGains[i] := AGains[i] else FResidualGains[i] := 0;
  RecalcResidualCoeffs;
end;

function TSedaiAdditiveGenerator.GetHarmonicLevel(AHarmonic: Integer): Single;
begin
  if (AHarmonic >= 0) and (AHarmonic < ADDITIVE_MAX_HARMONICS) then
    Result := FHarmonicLevels[AHarmonic]
  else
    Result := 0;
end;

function TSedaiAdditiveGenerator.GetHarmonicDetune(AHarmonic: Integer): Single;
begin
  if (AHarmonic >= 0) and (AHarmonic < ADDITIVE_MAX_HARMONICS) then
    Result := FHarmonicDetune[AHarmonic]
  else
    Result := 0;
end;

function TSedaiAdditiveGenerator.GetResidualGain(ABand: Integer): Single;
begin
  if (ABand >= 0) and (ABand < RESIDUAL_BANDS) then
    Result := FResidualGains[ABand]
  else
    Result := 0;
end;

function TSedaiAdditiveGenerator.GetHarmonicEnvelope(AHarmonic: Integer): TSedaiEnvelope;
begin
  if (AHarmonic >= 0) and (AHarmonic < ADDITIVE_MAX_HARMONICS) then
    Result := FHarmonicEnvelopes[AHarmonic]
  else
    Result := nil;
end;

procedure TSedaiAdditiveGenerator.LoadPreset(APreset: TAdditivePreset);
begin
  ApplyPreset(APreset);
end;

procedure TSedaiAdditiveGenerator.LoadSineWave;
begin
  ClearAllHarmonics;
  FHarmonicLevels[0] := 1.0;  // Only fundamental
  FCurrentPreset := apSine;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.LoadSawWave;
var
  I: Integer;
begin
  ClearAllHarmonics;
  // Sawtooth: 1/n for each harmonic
  for I := 0 to FHarmonicCount - 1 do
    FHarmonicLevels[I] := 1.0 / (I + 1);
  FCurrentPreset := apSaw;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.LoadSquareWave;
var
  I: Integer;
begin
  ClearAllHarmonics;
  // Square: only odd harmonics, 1/n
  for I := 0 to FHarmonicCount - 1 do
  begin
    if ((I + 1) mod 2) = 1 then  // Odd harmonics (1, 3, 5, ...)
      FHarmonicLevels[I] := 1.0 / (I + 1)
    else
      FHarmonicLevels[I] := 0;
  end;
  FCurrentPreset := apSquare;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.LoadTriangleWave;
var
  I: Integer;
  N: Integer;
  Sign: Single;
begin
  ClearAllHarmonics;
  // Triangle: only odd harmonics, 1/n^2, alternating sign
  Sign := 1.0;
  for I := 0 to FHarmonicCount - 1 do
  begin
    N := I + 1;
    if (N mod 2) = 1 then  // Odd harmonics
    begin
      FHarmonicLevels[I] := Sign / (N * N);
      Sign := -Sign;  // Alternate sign
    end
    else
      FHarmonicLevels[I] := 0;
  end;
  // Note: Since we're using abs(sin), we need positive levels
  for I := 0 to FHarmonicCount - 1 do
    FHarmonicLevels[I] := Abs(FHarmonicLevels[I]);
  FCurrentPreset := apTriangle;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.LoadOrganWave;
begin
  ClearAllHarmonics;
  // Organ drawbar simulation (8', 4', 2 2/3', 2', 1 3/5', 1 1/3', 1')
  FHarmonicLevels[0] := 0.8;   // 8' (fundamental)
  FHarmonicLevels[1] := 0.6;   // 4' (2nd harmonic)
  FHarmonicLevels[2] := 0.4;   // 2 2/3' (3rd harmonic)
  FHarmonicLevels[3] := 0.5;   // 2' (4th harmonic)
  FHarmonicLevels[4] := 0.3;   // 1 3/5' (5th harmonic)
  FHarmonicLevels[5] := 0.25;  // 1 1/3' (6th harmonic)
  FHarmonicLevels[7] := 0.35;  // 1' (8th harmonic)
  FCurrentPreset := apOrgan;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.LoadBellWave;
begin
  ClearAllHarmonics;
  // Bell: strong fundamental, sparse inharmonic partials
  FHarmonicLevels[0] := 1.0;
  FHarmonicLevels[2] := 0.3;
  FHarmonicLevels[4] := 0.5;
  FHarmonicLevels[6] := 0.2;
  FHarmonicLevels[8] := 0.4;
  FHarmonicLevels[11] := 0.15;
  FHarmonicLevels[15] := 0.25;
  FHarmonicLevels[19] := 0.1;

  // Add inharmonicity for bell-like quality
  FHarmonicDetune[4] := 5;
  FHarmonicDetune[6] := -3;
  FHarmonicDetune[8] := 8;
  FHarmonicDetune[11] := -5;
  FHarmonicDetune[15] := 12;
  FHarmonicDetune[19] := -8;

  FCurrentPreset := apBell;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.LoadStringsWave;
var
  I: Integer;
begin
  ClearAllHarmonics;
  // Strings: rich harmonics with gradual rolloff
  for I := 0 to FHarmonicCount - 1 do
  begin
    FHarmonicLevels[I] := 1.0 / Power(I + 1, 0.8);
    // Add slight random detuning for chorus-like effect
    FHarmonicDetune[I] := (FRandom.NextFloat - 0.5) * 4;
  end;
  FCurrentPreset := apStrings;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.LoadChoirWave;
var
  I: Integer;
begin
  ClearAllHarmonics;
  // Choir/vocal: emphasis on lower harmonics, formant-like structure
  FHarmonicLevels[0] := 1.0;    // Fundamental
  FHarmonicLevels[1] := 0.7;    // 2nd
  FHarmonicLevels[2] := 0.5;    // 3rd
  FHarmonicLevels[3] := 0.6;    // 4th (formant area)
  FHarmonicLevels[4] := 0.4;    // 5th
  FHarmonicLevels[5] := 0.3;    // 6th
  FHarmonicLevels[7] := 0.2;    // 8th
  FHarmonicLevels[9] := 0.15;   // 10th
  FHarmonicLevels[11] := 0.1;   // 12th

  // Slight vibrato-like detuning
  for I := 0 to 11 do
    FHarmonicDetune[I] := (FRandom.NextFloat - 0.5) * 3;

  FCurrentPreset := apChoir;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.LoadBrassWave;
var
  I: Integer;
begin
  ClearAllHarmonics;
  // Brass: strong harmonics, brighter than saw
  for I := 0 to FHarmonicCount - 1 do
  begin
    // Brass has more energy in mid-harmonics
    if I < 8 then
      FHarmonicLevels[I] := 1.0 / Power(I + 1, 0.6)
    else
      FHarmonicLevels[I] := 1.0 / Power(I + 1, 1.2);
  end;
  FCurrentPreset := apBrass;
  UpdateActiveHarmonics;
end;

procedure TSedaiAdditiveGenerator.LoadFluteWave;
begin
  ClearAllHarmonics;
  // Flute: mostly fundamental with weak odd harmonics
  FHarmonicLevels[0] := 1.0;    // Fundamental (strong)
  FHarmonicLevels[1] := 0.1;    // 2nd (weak)
  FHarmonicLevels[2] := 0.15;   // 3rd (slightly stronger)
  FHarmonicLevels[4] := 0.05;   // 5th (very weak)
  FCurrentPreset := apFlute;
  UpdateActiveHarmonics;
end;

end.
