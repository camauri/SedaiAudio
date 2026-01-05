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
  FActiveHarmonics := 0;
  FNyquistLimit := FSampleRate * 0.5;

  // Initialize harmonic arrays
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    FHarmonicLevels[I] := 0;
    FHarmonicDetune[I] := 0;
    FHarmonicPhases[I] := 0;
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
      // Get harmonic level (with optional envelope)
      if FUseHarmonicEnvelopes then
        HarmonicLevel := FHarmonicLevels[I] * FHarmonicEnvelopes[I].Process
      else
        HarmonicLevel := FHarmonicLevels[I];

      if HarmonicLevel > 0.001 then
      begin
        // Calculate harmonic frequency with detune
        // Detune in cents: 1 cent = 2^(1/1200)
        if FHarmonicDetune[I] <> 0 then
          DetuneRatio := Power(2, FHarmonicDetune[I] / 1200)
        else
          DetuneRatio := 1.0;

        HarmonicFreq := FFrequency * (I + 1) * DetuneRatio;

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

  // Reset phases
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    FHarmonicPhases[I] := 0;
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

  // Reset all phases
  for I := 0 to ADDITIVE_MAX_HARMONICS - 1 do
  begin
    FHarmonicPhases[I] := 0;
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

  // Calculate sample from all harmonics
  Result := CalculateSample * EnvValue * FVelocity * FAmplitude;
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
