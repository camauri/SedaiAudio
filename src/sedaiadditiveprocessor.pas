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

unit SedaiAdditiveProcessor;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiADSRProcessor;

const
  MAX_HARMONICS = 32;  // Up to 32 harmonics

type
  // Harmonic definition
  THarmonic = record
    Number: Integer;      // Harmonic number (1 = fundamental, 2 = octave, etc.)
    Amplitude: Single;    // Amplitude of this harmonic (0.0-1.0)
    Phase: Single;        // Starting phase offset (0.0-2π)
    IsActive: Boolean;    // Whether this harmonic is enabled
  end;

  // Additive synthesis structure
  TAdditiveSynthesis = record
    IsActive: Boolean;
    Harmonics: array[0..MAX_HARMONICS-1] of THarmonic;
    HarmonicCount: Integer;
    CurrentPhase: Single;  // Phase accumulator for fundamental
    ADSR: TADSR;
    Amplitude: Single;
    FilterCutoff: Single;   // Optional filter for brightness control
    InharmonicFactor: Single;  // 0.0 = harmonic, >0.0 = inharmonic (for bells, etc.)
  end;

  { TSedaiAdditiveProcessor }

  TSedaiAdditiveProcessor = class
  public
    // Core processing
    class function ProcessAdditive(var AAddSynth: TAdditiveSynthesis;
                                  ABaseFreq: Single; ASampleRate: Single;
                                  ADeltaTime: Single): Single;

    // Initialization
    class procedure InitializeAdditive(var AAddSynth: TAdditiveSynthesis);

    // Harmonic control
    class procedure SetHarmonic(var AAddSynth: TAdditiveSynthesis;
                               AHarmonicNum: Integer; AAmplitude: Single;
                               APhase: Single = 0.0);
    class procedure SetHarmonicSeries(var AAddSynth: TAdditiveSynthesis;
                                     const AAmplitudes: array of Single);
    class procedure ClearHarmonics(var AAddSynth: TAdditiveSynthesis);

    // Preset harmonic spectra
    class function CreateOrganSpectrum: TAdditiveSynthesis;
    class function CreateStringsSpectrum: TAdditiveSynthesis;
    class function CreateBrassSpectrum: TAdditiveSynthesis;
    class function CreateFluteSpectrum: TAdditiveSynthesis;
    class function CreateBellSpectrum: TAdditiveSynthesis;
    class function CreateVoiceSpectrum: TAdditiveSynthesis;

    // Control
    class procedure StartAdditiveAttack(var AAddSynth: TAdditiveSynthesis);
    class procedure StartAdditiveRelease(var AAddSynth: TAdditiveSynthesis);
  end;

implementation

// Initialize additive synthesis
class procedure TSedaiAdditiveProcessor.InitializeAdditive(var AAddSynth: TAdditiveSynthesis);
var
  i: Integer;
begin
  FillChar(AAddSynth, SizeOf(TAdditiveSynthesis), 0);
  AAddSynth.IsActive := True;
  AAddSynth.HarmonicCount := 0;
  AAddSynth.CurrentPhase := 0.0;
  AAddSynth.Amplitude := 0.7;
  AAddSynth.FilterCutoff := 1.0;
  AAddSynth.InharmonicFactor := 0.0;  // Pure harmonic by default

  // Initialize ADSR for smooth envelope
  AAddSynth.ADSR := CreateADSR(0.01, 0.1, 0.9, 0.3);

  // Initialize harmonics
  for i := 0 to MAX_HARMONICS - 1 do
  begin
    AAddSynth.Harmonics[i].Number := i + 1;
    AAddSynth.Harmonics[i].Amplitude := 0.0;
    AAddSynth.Harmonics[i].Phase := 0.0;
    AAddSynth.Harmonics[i].IsActive := False;
  end;
end;

// Set individual harmonic
class procedure TSedaiAdditiveProcessor.SetHarmonic(var AAddSynth: TAdditiveSynthesis;
  AHarmonicNum: Integer; AAmplitude: Single; APhase: Single);
begin
  if (AHarmonicNum < 1) or (AHarmonicNum > MAX_HARMONICS) then Exit;

  with AAddSynth.Harmonics[AHarmonicNum - 1] do
  begin
    Number := AHarmonicNum;
    Amplitude := AAmplitude;
    Phase := APhase;
    IsActive := (AAmplitude > 0.0);
  end;

  // Update count
  if AAmplitude > 0.0 then
    if AHarmonicNum > AAddSynth.HarmonicCount then
      AAddSynth.HarmonicCount := AHarmonicNum;
end;

// Set harmonic series from array
class procedure TSedaiAdditiveProcessor.SetHarmonicSeries(var AAddSynth: TAdditiveSynthesis;
  const AAmplitudes: array of Single);
var
  i: Integer;
begin
  ClearHarmonics(AAddSynth);

  for i := 0 to High(AAmplitudes) do
  begin
    if i >= MAX_HARMONICS then Break;
    SetHarmonic(AAddSynth, i + 1, AAmplitudes[i]);
  end;
end;

// Clear all harmonics
class procedure TSedaiAdditiveProcessor.ClearHarmonics(var AAddSynth: TAdditiveSynthesis);
var
  i: Integer;
begin
  for i := 0 to MAX_HARMONICS - 1 do
  begin
    AAddSynth.Harmonics[i].Amplitude := 0.0;
    AAddSynth.Harmonics[i].IsActive := False;
  end;
  AAddSynth.HarmonicCount := 0;
end;

// Core additive synthesis processing
class function TSedaiAdditiveProcessor.ProcessAdditive(var AAddSynth: TAdditiveSynthesis;
  ABaseFreq: Single; ASampleRate: Single; ADeltaTime: Single): Single;
var
  i: Integer;
  APhaseIncrement: Single;
  AHarmonicPhase: Single;
  AHarmonicFreq: Single;
  ASample: Single;
  AHarmonicSample: Single;
begin
  Result := 0.0;

  if not AAddSynth.IsActive then Exit;

  // Update ADSR
  TSedaiADSRProcessor.ProcessADSR(AAddSynth.ADSR, ADeltaTime);

  // Calculate phase increment for fundamental
  APhaseIncrement := (ABaseFreq * 2.0 * Pi) / ASampleRate;

  // Sum all active harmonics
  ASample := 0.0;
  for i := 0 to AAddSynth.HarmonicCount - 1 do
  begin
    if AAddSynth.Harmonics[i].IsActive then
    begin
      // Calculate harmonic frequency (with optional inharmonicity)
      if AAddSynth.InharmonicFactor > 0.0 then
        // Inharmonic: frequency = n * f0 * (1 + b * n^2)
        AHarmonicFreq := AAddSynth.Harmonics[i].Number *
          (1.0 + AAddSynth.InharmonicFactor * Sqr(AAddSynth.Harmonics[i].Number))
      else
        // Pure harmonic
        AHarmonicFreq := AAddSynth.Harmonics[i].Number;

      // Calculate phase for this harmonic
      AHarmonicPhase := AAddSynth.CurrentPhase * AHarmonicFreq +
                       AAddSynth.Harmonics[i].Phase;

      // Generate sine wave for this harmonic
      AHarmonicSample := Sin(AHarmonicPhase) * AAddSynth.Harmonics[i].Amplitude;

      // Apply filter cutoff (simple amplitude scaling for high harmonics)
      if AAddSynth.FilterCutoff < 1.0 then
      begin
        if i > 8 then  // Only affect harmonics above 8th
          AHarmonicSample := AHarmonicSample * AAddSynth.FilterCutoff;
      end;

      ASample := ASample + AHarmonicSample;
    end;
  end;

  // Normalize by number of active harmonics to prevent clipping
  if AAddSynth.HarmonicCount > 0 then
    ASample := ASample / Sqrt(AAddSynth.HarmonicCount);

  // Apply amplitude and ADSR
  Result := ASample * AAddSynth.Amplitude * AAddSynth.ADSR.Level;

  // Update phase
  AAddSynth.CurrentPhase := AAddSynth.CurrentPhase + APhaseIncrement;

  // Wrap phase to prevent overflow
  if AAddSynth.CurrentPhase > 2.0 * Pi then
    AAddSynth.CurrentPhase := AAddSynth.CurrentPhase - 2.0 * Pi;
end;

// Preset: Organ spectrum (drawbar-style)
class function TSedaiAdditiveProcessor.CreateOrganSpectrum: TAdditiveSynthesis;
begin
  InitializeAdditive(Result);

  // Hammond drawbar-style: 16', 8', 5 1/3', 4', 2 2/3', 2', 1 3/5', 1 1/3'
  // Corresponding to harmonics: 1, 2, 3, 4, 6, 8, 10, 12
  SetHarmonic(Result, 1, 0.8);   // 16' (sub-fundamental)
  SetHarmonic(Result, 2, 1.0);   // 8' (fundamental)
  SetHarmonic(Result, 3, 0.7);   // 5 1/3'
  SetHarmonic(Result, 4, 0.6);   // 4'
  SetHarmonic(Result, 6, 0.5);   // 2 2/3'
  SetHarmonic(Result, 8, 0.4);   // 2'
  SetHarmonic(Result, 10, 0.3);  // 1 3/5'
  SetHarmonic(Result, 12, 0.3);  // 1 1/3'

  Result.ADSR := CreateADSR(0.001, 0.01, 0.95, 0.05);  // Instant attack, sustained
  Result.Amplitude := 0.6;
end;

// Preset: String spectrum (rich overtones with 1/n rolloff)
class function TSedaiAdditiveProcessor.CreateStringsSpectrum: TAdditiveSynthesis;
var
  i: Integer;
begin
  InitializeAdditive(Result);

  // Sawtooth-like spectrum with 1/n amplitude rolloff
  for i := 1 to 16 do
    SetHarmonic(Result, i, 1.0 / i);

  Result.ADSR := CreateADSR(0.5, 0.3, 0.85, 1.0);  // Slow attack
  Result.Amplitude := 0.5;
  Result.FilterCutoff := 0.7;  // Darker sound
end;

// Preset: Brass spectrum (strong odd harmonics)
class function TSedaiAdditiveProcessor.CreateBrassSpectrum: TAdditiveSynthesis;
begin
  InitializeAdditive(Result);

  // Strong odd harmonics (square wave-like but more refined)
  SetHarmonic(Result, 1, 1.0);
  SetHarmonic(Result, 3, 0.7);
  SetHarmonic(Result, 5, 0.5);
  SetHarmonic(Result, 7, 0.35);
  SetHarmonic(Result, 9, 0.25);
  SetHarmonic(Result, 11, 0.15);

  // Add some even harmonics for richness
  SetHarmonic(Result, 2, 0.3);
  SetHarmonic(Result, 4, 0.2);

  Result.ADSR := CreateADSR(0.08, 0.2, 0.8, 0.3);
  Result.Amplitude := 0.6;
end;

// Preset: Flute spectrum (mostly fundamental with weak harmonics)
class function TSedaiAdditiveProcessor.CreateFluteSpectrum: TAdditiveSynthesis;
begin
  InitializeAdditive(Result);

  // Mostly fundamental with very weak upper harmonics
  SetHarmonic(Result, 1, 1.0);
  SetHarmonic(Result, 2, 0.2);
  SetHarmonic(Result, 3, 0.15);
  SetHarmonic(Result, 4, 0.1);
  SetHarmonic(Result, 5, 0.05);

  Result.ADSR := CreateADSR(0.05, 0.1, 0.7, 0.2);
  Result.Amplitude := 0.7;
end;

// Preset: Bell spectrum (inharmonic partials)
class function TSedaiAdditiveProcessor.CreateBellSpectrum: TAdditiveSynthesis;
begin
  InitializeAdditive(Result);

  // Inharmonic partials typical of bells
  SetHarmonic(Result, 1, 1.0, 0.0);
  SetHarmonic(Result, 2, 0.6, 0.3);
  SetHarmonic(Result, 3, 0.4, 0.7);
  SetHarmonic(Result, 5, 0.3, 1.2);
  SetHarmonic(Result, 7, 0.2, 1.8);

  // Enable inharmonicity for bell-like quality
  Result.InharmonicFactor := 0.0001;

  Result.ADSR := CreateADSR(0.001, 1.5, 0.2, 3.0);  // Sharp attack, long decay
  Result.Amplitude := 0.65;
end;

// Preset: Voice spectrum (formant-like)
class function TSedaiAdditiveProcessor.CreateVoiceSpectrum: TAdditiveSynthesis;
begin
  InitializeAdditive(Result);

  // Formant-like spectrum mimicking vowel sounds
  SetHarmonic(Result, 1, 0.8);
  SetHarmonic(Result, 2, 0.9);   // First formant region
  SetHarmonic(Result, 3, 1.0);   // First formant peak
  SetHarmonic(Result, 4, 0.7);
  SetHarmonic(Result, 5, 0.6);   // Second formant region
  SetHarmonic(Result, 6, 0.8);   // Second formant peak
  SetHarmonic(Result, 7, 0.5);
  SetHarmonic(Result, 9, 0.4);
  SetHarmonic(Result, 11, 0.3);

  Result.ADSR := CreateADSR(0.15, 0.2, 0.85, 0.6);
  Result.Amplitude := 0.5;
end;

// Control methods
class procedure TSedaiAdditiveProcessor.StartAdditiveAttack(var AAddSynth: TAdditiveSynthesis);
begin
  TSedaiADSRProcessor.StartADSRAttack(AAddSynth.ADSR);
  AAddSynth.CurrentPhase := 0.0;  // Reset phase for consistent attack
end;

class procedure TSedaiAdditiveProcessor.StartAdditiveRelease(var AAddSynth: TAdditiveSynthesis);
begin
  TSedaiADSRProcessor.StartADSRRelease(AAddSynth.ADSR);
end;

end.
