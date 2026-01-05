{*
 * Sedai Audio Foundation - Noise Generator
 *
 * TSedaiNoiseGenerator provides white, pink, and brown noise generation.
 * Uses optimized algorithms for accurate spectral characteristics.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiNoiseGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiOscillator;

type
  // Noise type
  TNoiseType = (
    ntWhite,          // White noise (flat spectrum)
    ntPink,           // Pink noise (-3dB/octave)
    ntBrown,          // Brown/red noise (-6dB/octave)
    ntBlue,           // Blue noise (+3dB/octave)
    ntViolet          // Violet noise (+6dB/octave)
  );

  { TSedaiNoiseGenerator }
  // Multi-type noise generator
  TSedaiNoiseGenerator = class(TSedaiSignalGenerator)
  private
    FNoiseType: TNoiseType;
    FSeed: Cardinal;              // Random seed

    // Pink noise state (Voss-McCartney algorithm)
    FPinkRows: array[0..15] of Single;
    FPinkRunningSum: Single;
    FPinkIndex: Integer;
    FPinkIndexMask: Integer;

    // Brown noise state
    FBrownState: Single;

    // Blue/Violet noise state (differentiator)
    FLastSample: Single;
    FLastSample2: Single;

    procedure SetNoiseType(AValue: TNoiseType);
    function GenerateWhiteNoise: Single;
    function GeneratePinkNoise: Single;
    function GenerateBrownNoise: Single;
    function GenerateBlueNoise: Single;
    function GenerateVioletNoise: Single;

    // Fast random number generator
    function FastRandom: Single;

  public
    constructor Create; override;

    procedure Reset; override;

    function GenerateSample: Single; override;

    // Set random seed
    procedure SetSeed(ASeed: Cardinal);

    property NoiseType: TNoiseType read FNoiseType write SetNoiseType;
    property Seed: Cardinal read FSeed write SetSeed;
  end;

implementation

{ TSedaiNoiseGenerator }

constructor TSedaiNoiseGenerator.Create;
var
  I: Integer;
begin
  inherited Create;

  FNoiseType := ntWhite;
  FSeed := 22222;

  // Initialize pink noise state
  for I := 0 to 15 do
    FPinkRows[I] := 0.0;
  FPinkRunningSum := 0.0;
  FPinkIndex := 0;
  FPinkIndexMask := (1 shl 4) - 1;  // 16 rows

  FBrownState := 0.0;
  FLastSample := 0.0;
  FLastSample2 := 0.0;
end;

procedure TSedaiNoiseGenerator.Reset;
var
  I: Integer;
begin
  inherited Reset;

  for I := 0 to 15 do
    FPinkRows[I] := 0.0;
  FPinkRunningSum := 0.0;
  FPinkIndex := 0;

  FBrownState := 0.0;
  FLastSample := 0.0;
  FLastSample2 := 0.0;
end;

procedure TSedaiNoiseGenerator.SetNoiseType(AValue: TNoiseType);
begin
  if FNoiseType <> AValue then
  begin
    FNoiseType := AValue;
    Reset;
  end;
end;

procedure TSedaiNoiseGenerator.SetSeed(ASeed: Cardinal);
begin
  FSeed := ASeed;
  if FSeed = 0 then FSeed := 1;
end;

function TSedaiNoiseGenerator.FastRandom: Single;
begin
  // Linear congruential generator
  FSeed := FSeed * 1664525 + 1013904223;
  // Convert to float in range -1 to +1
  Result := (FSeed / 2147483648.0) - 1.0;
end;

function TSedaiNoiseGenerator.GenerateWhiteNoise: Single;
begin
  Result := FastRandom;
end;

function TSedaiNoiseGenerator.GeneratePinkNoise: Single;
var
  White, NewRandom: Single;
  NumZeros, N: Integer;
begin
  // Voss-McCartney algorithm for pink noise
  // Based on counting trailing zeros

  // Generate white noise sample
  White := FastRandom;

  // Count trailing zeros of index
  FPinkIndex := (FPinkIndex + 1) and FPinkIndexMask;
  if FPinkIndex <> 0 then
  begin
    // Find first set bit (number of trailing zeros)
    NumZeros := 0;
    N := FPinkIndex;
    while (N and 1) = 0 do
    begin
      Inc(NumZeros);
      N := N shr 1;
    end;

    // Update the corresponding row
    FPinkRunningSum := FPinkRunningSum - FPinkRows[NumZeros];
    NewRandom := FastRandom;
    FPinkRunningSum := FPinkRunningSum + NewRandom;
    FPinkRows[NumZeros] := NewRandom;
  end;

  // Sum and normalize
  Result := (FPinkRunningSum + White) * 0.0625;  // /16

  // Clamp
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

function TSedaiNoiseGenerator.GenerateBrownNoise: Single;
var
  White: Single;
begin
  // Brown noise: integrate white noise
  White := FastRandom;

  // Leaky integration
  FBrownState := FBrownState + White * 0.02;

  // Prevent runaway
  if FBrownState > 1.0 then
    FBrownState := 1.0
  else if FBrownState < -1.0 then
    FBrownState := -1.0;

  // Slight decay to prevent DC buildup
  FBrownState := FBrownState * 0.999;

  Result := FBrownState;
end;

function TSedaiNoiseGenerator.GenerateBlueNoise: Single;
var
  White, Blue: Single;
begin
  // Blue noise: differentiate white noise
  White := FastRandom;

  // First-order differentiator
  Blue := White - FLastSample;
  FLastSample := White;

  // Normalize (differentiation boosts high frequencies)
  Result := Blue * 0.5;
end;

function TSedaiNoiseGenerator.GenerateVioletNoise: Single;
var
  White, Violet: Single;
begin
  // Violet noise: second-order differentiation
  White := FastRandom;

  // Second-order differentiator
  Violet := White - 2.0 * FLastSample + FLastSample2;
  FLastSample2 := FLastSample;
  FLastSample := White;

  // Normalize
  Result := Violet * 0.25;
end;

function TSedaiNoiseGenerator.GenerateSample: Single;
begin
  case FNoiseType of
    ntWhite:  Result := GenerateWhiteNoise;
    ntPink:   Result := GeneratePinkNoise;
    ntBrown:  Result := GenerateBrownNoise;
    ntBlue:   Result := GenerateBlueNoise;
    ntViolet: Result := GenerateVioletNoise;
    else      Result := GenerateWhiteNoise;
  end;

  Result := Result * FAmplitude;
end;

end.
