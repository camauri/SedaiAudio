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
  SedaiOscillator, SedaiEnvelope;

const
  ADDITIVE_MAX_HARMONICS = 64;
  ADDITIVE_DEFAULT_HARMONICS = 32;
  TWO_PI = 2 * Pi;

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
    function TrackLevel(AHarmonic: Integer): Single;
    procedure UpdateModulation;
    procedure RecalcBreathCoeff;
    procedure RecalcBandCoeff;

  public
    constructor Create; override;
    destructor Destroy; override;

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
    // Airy breath layer. ALevel 0 = off; ACutoffHz shapes it ("air" low / "hiss" high).
    procedure SetBreath(ALevel, ACutoffHz: Single);
    // Per-partial bandwidth ("metal"). ADepth = fractional AM depth per partial
    // (~0.04 tuned; 0 = off), ACutoffHz = band-noise low-pass (~45 Hz). Each
    // active harmonic gets an independent LP-noise modulator.
    procedure SetBandwidth(ADepth, ACutoffHz: Single);
    function GetHarmonicLevel(AHarmonic: Integer): Single;
    function GetHarmonicDetune(AHarmonic: Integer): Single;
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
    property BreathLevel: Single read FBreathLevel write FBreathLevel;
    property BreathCutoff: Single read FBreathCut;
    property BandDepth: Single read FBandDepth write FBandDepth;
    property BandCutoff: Single read FBandCut;
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
  FBreathLevel := 0; FBreathCut := 4000;
  FLfoPhase := 0; FLfoPCur := 0; FLfoPTgt := 0; FLfoACur := 0; FLfoATgt := 0;
  FPitchMod := 1; FAmpMod := 1; FBreathState := 0;
  FBandDepth := 0; FBandCut := 45;
  RecalcBreathCoeff;
  RecalcBandCoeff;
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
        FBandState[I] := FBandState[I] + FBandCoeff * ((Random * 2 - 1) - FBandState[I]);
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
  if (FJitterCents > 0) or (FShimmerDepth > 0) or (FBreathLevel > 0) then
  begin
    FLfoPCur := Random * 2 - 1; FLfoPTgt := Random * 2 - 1;
    FLfoACur := Random * 2 - 1; FLfoATgt := Random * 2 - 1;
  end;
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    FHarmonicPhases[I] := 0;
    FTrackCursor[I] := 0;
    FBandState[I] := 0;
    if FUseHarmonicEnvelopes then
      FHarmonicEnvelopes[I].Trigger;
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

  FAmpEnvelope.Reset;
end;

function TSedaiAdditiveGenerator.GenerateSample: Single;
var
  EnvValue: Single;
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
  Result := CalculateSample * EnvValue * FVelocity * FAmplitude * FAmpMod;

  // Airy breath layer: one-pole low-passed white noise, following the envelope
  if FBreathLevel > 0 then
  begin
    FBreathState := FBreathState + FBreathCoeff * ((Random * 2 - 1) - FBreathState);
    Result := Result + FBreathState * FBreathLevel * EnvValue * FVelocity * FAmplitude;
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
  inc, lp, la: Single;
begin
  if (FJitterCents <= 0) and (FShimmerDepth <= 0) then
  begin
    FPitchMod := 1.0; FAmpMod := 1.0;
    Exit;
  end;
  if FSampleRate > 0 then inc := FModRate / FSampleRate else inc := 0;
  FLfoPhase := FLfoPhase + inc;
  if FLfoPhase >= 1.0 then
  begin
    FLfoPhase := FLfoPhase - 1.0;
    FLfoPCur := FLfoPTgt; FLfoPTgt := Random * 2 - 1;
    FLfoACur := FLfoATgt; FLfoATgt := Random * 2 - 1;
  end;
  lp := FLfoPCur + (FLfoPTgt - FLfoPCur) * FLfoPhase;   // -1..1, smooth
  la := FLfoACur + (FLfoATgt - FLfoACur) * FLfoPhase;
  if FJitterCents > 0 then
    FPitchMod := Power(2, (FJitterCents * lp) / 1200)    // cents -> ratio
  else
    FPitchMod := 1.0;
  if FShimmerDepth > 0 then
    FAmpMod := 1.0 + FShimmerDepth * la
  else
    FAmpMod := 1.0;
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
    FHarmonicDetune[I] := (Random - 0.5) * 4;
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
    FHarmonicDetune[I] := (Random - 0.5) * 3;

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
