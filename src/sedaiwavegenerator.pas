{*
 * Sedai Audio Foundation - Professional audio synthesis library
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    You may redistribute and/or modify it under the terms of the GNU GPL v3
 *    as published by the Free Software Foundation.
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

unit SedaiWaveGenerator;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, ctypes, SedaiAudioTypes;

type
  TSedaiWaveGenerator = class
  private
    class var FNoiseState: Cardinal; // For noise generation

  public
    // Basic waveform generators
    class function GenerateSine(APhase: Single): Single;
    class function GenerateSquare(APhase: Single): Single;
    class function GenerateSawtooth(APhase: Single): Single;
    class function GenerateTriangle(APhase: Single): Single;
    class function GenerateNoise: Single;

    // Advanced waveform generators
    class function GeneratePulse(APhase: Single; APulseWidth: Single = 0.5): Single;
    class function GenerateSquareHarmonic(APhase: Single; AHarmonics: Integer = 5): Single;
    class function GenerateSawtoothHarmonic(APhase: Single; AHarmonics: Integer = 10): Single;
    class function GenerateTriangleHarmonic(APhase: Single; AHarmonics: Integer = 5): Single;

    // Specialized generators
    class function GenerateWhiteNoise: Single;
    class function GeneratePinkNoise: Single;
    class function GenerateBrownNoise: Single;

    // Waveform with harmonics/distortion
    class function GenerateDistortedSine(APhase: Single; ADistortion: Single = 0.0): Single;
    class function GenerateWarmSquare(APhase: Single; AWarmth: Single = 0.1): Single;
    class function GenerateFMSine(APhase: Single; AModPhase: Single; AModDepth: Single): Single;

    // Utility functions
    class function GenerateWaveform(AWaveType: TWaveType; APhase: Single): Single;
    class function GenerateWaveformPW(AWaveType: TWaveType; APhase: Single; APulseWidth: Single): Single;
    class function NormalizePhase(APhase: Single): Single;
    class procedure InitializeNoise(ASeed: Cardinal = 0);

    // Anti-aliasing versions (for high frequencies)
    class function GenerateBandLimitedSquare(APhase: Single; ASampleRate: Single; AFrequency: Single): Single;
    class function GenerateBandLimitedSawtooth(APhase: Single; ASampleRate: Single; AFrequency: Single): Single;
  end;

implementation

class procedure TSedaiWaveGenerator.InitializeNoise(ASeed: Cardinal);
begin
  if ASeed = 0 then
    FNoiseState := GetTickCount64 and $FFFFFFFF
  else
    FNoiseState := ASeed;
end;

class function TSedaiWaveGenerator.NormalizePhase(APhase: Single): Single;
begin
  Result := APhase;
  while Result >= 2.0 * Pi do
    Result := Result - 2.0 * Pi;
  while Result < 0.0 do
    Result := Result + 2.0 * Pi;
end;

// Basic sine wave generator
class function TSedaiWaveGenerator.GenerateSine(APhase: Single): Single;
begin
  Result := Sin(APhase);
end;

// Basic square wave generator
class function TSedaiWaveGenerator.GenerateSquare(APhase: Single): Single;
begin
  if Sin(APhase) >= 0 then
    Result := 1.0
  else
    Result := -1.0;
end;

// Basic sawtooth wave generator
class function TSedaiWaveGenerator.GenerateSawtooth(APhase: Single): Single;
var
  ANormalizedPhase: Single;
begin
  ANormalizedPhase := Frac(APhase / (2 * Pi));
  Result := (ANormalizedPhase * 2.0) - 1.0;
end;

// Basic triangle wave generator
class function TSedaiWaveGenerator.GenerateTriangle(APhase: Single): Single;
var
  ANormalizedPhase: Single;
begin
  ANormalizedPhase := Frac(APhase / (2 * Pi));
  if ANormalizedPhase < 0.5 then
    Result := (ANormalizedPhase * 4.0) - 1.0
  else
    Result := 3.0 - (ANormalizedPhase * 4.0);
end;

// White noise generator using linear congruential generator
class function TSedaiWaveGenerator.GenerateNoise: Single;
begin
  // Linear congruential generator
  FNoiseState := (FNoiseState * 1664525 + 1013904223) and $FFFFFFFF;
  Result := (FNoiseState / $7FFFFFFF) - 1.0; // Convert to -1.0 to 1.0
end;

// Pulse wave with adjustable pulse width
class function TSedaiWaveGenerator.GeneratePulse(APhase: Single; APulseWidth: Single): Single;
var
  ANormalizedPhase: Single;
  AClampedWidth: Single;
begin
  AClampedWidth := APulseWidth;
  if AClampedWidth < 0.01 then AClampedWidth := 0.01;
  if AClampedWidth > 0.99 then AClampedWidth := 0.99;

  ANormalizedPhase := Frac(APhase / (2 * Pi));
  if ANormalizedPhase < AClampedWidth then
    Result := 1.0
  else
    Result := -1.0;
end;

// Square wave with limited harmonics (anti-aliasing)
class function TSedaiWaveGenerator.GenerateSquareHarmonic(APhase: Single; AHarmonics: Integer): Single;
var
  i: Integer;
  AHarmonic: Integer;
begin
  Result := 0.0;
  for i := 0 to AHarmonics - 1 do
  begin
    AHarmonic := (i * 2) + 1; // Odd harmonics only: 1, 3, 5, 7...
    Result := Result + (Sin(APhase * AHarmonic) / AHarmonic);
  end;
  Result := Result * (4.0 / Pi); // Normalize
end;

// Sawtooth wave with limited harmonics
class function TSedaiWaveGenerator.GenerateSawtoothHarmonic(APhase: Single; AHarmonics: Integer): Single;
var
  i: Integer;
begin
  Result := 0.0;
  for i := 1 to AHarmonics do
  begin
    Result := Result + (Sin(APhase * i) / i);
  end;
  Result := Result * (2.0 / Pi); // Normalize
end;

// Triangle wave with limited harmonics
class function TSedaiWaveGenerator.GenerateTriangleHarmonic(APhase: Single; AHarmonics: Integer): Single;
var
  i: Integer;
  AHarmonic: Integer;
  ASign: Single;
begin
  Result := 0.0;
  ASign := 1.0;
  for i := 0 to AHarmonics - 1 do
  begin
    AHarmonic := (i * 2) + 1; // Odd harmonics only
    Result := Result + (ASign * Sin(APhase * AHarmonic) / (AHarmonic * AHarmonic));
    ASign := -ASign; // Alternate signs
  end;
  Result := Result * (8.0 / (Pi * Pi)); // Normalize
end;

// White noise (alias for GenerateNoise)
class function TSedaiWaveGenerator.GenerateWhiteNoise: Single;
begin
  Result := GenerateNoise;
end;

// Pink noise (1/f noise) - simplified version
class function TSedaiWaveGenerator.GeneratePinkNoise: Single;
var
  i: Integer;
  ASum: Single;
begin
  ASum := 0.0;
  // Generate multiple octaves of white noise with decreasing amplitude
  for i := 0 to 5 do
  begin
    ASum := ASum + (GenerateWhiteNoise / (i + 1));
  end;
  Result := ASum / 6.0; // Normalize
end;

// Brown noise (Brownian noise) - simplified version
class function TSedaiWaveGenerator.GenerateBrownNoise: Single;
begin
  // Simplified brown noise - integrate white noise
  // In real implementation, you'd maintain a running sum
  Result := GenerateWhiteNoise * 0.1; // Much quieter than white noise
end;

// Sine wave with harmonic distortion
class function TSedaiWaveGenerator.GenerateDistortedSine(APhase: Single; ADistortion: Single): Single;
var
  ABaseSine: Single;
begin
  ABaseSine := Sin(APhase);
  if ADistortion <= 0.0 then
  begin
    Result := ABaseSine;
  end
  else
  begin
    // Add harmonics based on distortion amount
    Result := ABaseSine +
              (ADistortion * Sin(APhase * 2) * 0.5) +
              (ADistortion * Sin(APhase * 3) * 0.25) +
              (ADistortion * Sin(APhase * 4) * 0.125);

    // Normalize to prevent clipping
    Result := Result / (1.0 + ADistortion);
  end;
end;

// Square wave with "warmth" (rounded edges)
class function TSedaiWaveGenerator.GenerateWarmSquare(APhase: Single; AWarmth: Single): Single;
var
  AHarmonics: Integer;
begin
  // More harmonics = sharper edges, fewer harmonics = warmer
  AHarmonics := Round(10 * (1.0 - AWarmth)) + 1;
  if AHarmonics < 1 then AHarmonics := 1;
  if AHarmonics > 10 then AHarmonics := 10;

  Result := GenerateSquareHarmonic(APhase, AHarmonics);
end;

// FM synthesis - sine with frequency modulation
class function TSedaiWaveGenerator.GenerateFMSine(APhase: Single; AModPhase: Single; AModDepth: Single): Single;
var
  AModulatedPhase: Single;
begin
  AModulatedPhase := APhase + (AModDepth * Sin(AModPhase));
  Result := Sin(AModulatedPhase);
end;

// Generate any waveform type
class function TSedaiWaveGenerator.GenerateWaveform(AWaveType: TWaveType; APhase: Single): Single;
begin
  case AWaveType of
    wtSine:     Result := GenerateSine(APhase);
    wtSquare:   Result := GenerateSquare(APhase);
    wtSawtooth: Result := GenerateSawtooth(APhase);
    wtTriangle: Result := GenerateTriangle(APhase);
    wtNoise:    Result := GenerateNoise;
    wtFM:       Result := 0.0; // FM is handled separately in audio processor
  else
    Result := 0.0;
  end;
end;

// Generate waveform with pulse width support (for square/pulse waves)
class function TSedaiWaveGenerator.GenerateWaveformPW(AWaveType: TWaveType; APhase: Single; APulseWidth: Single): Single;
begin
  case AWaveType of
    wtSine:     Result := GenerateSine(APhase);
    wtSquare:   Result := GeneratePulse(APhase, APulseWidth);  // Use pulse with variable width
    wtSawtooth: Result := GenerateSawtooth(APhase);
    wtTriangle: Result := GenerateTriangle(APhase);
    wtNoise:    Result := GenerateNoise;
    wtFM:       Result := 0.0;
  else
    Result := 0.0;
  end;
end;

// Band-limited square wave (anti-aliasing)
class function TSedaiWaveGenerator.GenerateBandLimitedSquare(APhase: Single; ASampleRate: Single; AFrequency: Single): Single;
var
  AMaxHarmonics: Integer;
begin
  // Calculate maximum harmonics before aliasing
  AMaxHarmonics := Trunc(ASampleRate / (2.0 * AFrequency));
  if AMaxHarmonics > 20 then AMaxHarmonics := 20; // Limit for performance
  if AMaxHarmonics < 1 then AMaxHarmonics := 1;

  Result := GenerateSquareHarmonic(APhase, AMaxHarmonics);
end;

// Band-limited sawtooth wave
class function TSedaiWaveGenerator.GenerateBandLimitedSawtooth(APhase: Single; ASampleRate: Single; AFrequency: Single): Single;
var
  AMaxHarmonics: Integer;
begin
  AMaxHarmonics := Trunc(ASampleRate / (2.0 * AFrequency));
  if AMaxHarmonics > 30 then AMaxHarmonics := 30;
  if AMaxHarmonics < 1 then AMaxHarmonics := 1;

  Result := GenerateSawtoothHarmonic(APhase, AMaxHarmonics);
end;

// Initialize noise generator at unit initialization
initialization
  TSedaiWaveGenerator.InitializeNoise;

end.
