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

unit SedaiWavetableProcessor;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiADSRProcessor;

type
  // Note: TWavetable is defined in SedaiAudioTypes

  // Wavetable oscillator
  TWavetableOscillator = record
    IsActive: Boolean;
    Wavetable: TWavetable;
    WavePosition: Single;      // 0.0 to WaveCount-1 (interpolated)
    SamplePosition: Single;    // Current position in wave
    ADSR: TADSR;
    Amplitude: Single;
    FreqRatio: Single;
  end;

  // Wavetable synthesis structure
  TWavetableSynthesis = record
    IsActive: Boolean;
    Oscillators: array[0..3] of TWavetableOscillator; // Up to 4 oscillators
    OscillatorCount: Integer;
    MixMode: Integer; // 0=add, 1=multiply, 2=ring mod
    FilterCutoff: Single;
    FilterResonance: Single;
  end;

  { TSedaiWavetableProcessor }

  TSedaiWavetableProcessor = class
  public
    // Core wavetable processing
    class function ProcessWavetable(var AWTSynth: TWavetableSynthesis;
                                   ABaseFreq: Single; ASampleRate: Single;
                                   ADeltaTime: Single): Single;

    // Wavetable management
    class function CreateBuiltinWavetable(AType: TWavetableType): TWavetable;
    class procedure UnloadWavetable(var AWavetable: TWavetable);

    // Oscillator control
    class procedure SetOscillatorWave(var AOsc: TWavetableOscillator;
                                     APosition: Single);
    class procedure ScanWaves(var AOsc: TWavetableOscillator;
                             ASpeed: Single; ADeltaTime: Single);

    // Synthesis setup
    class procedure InitializeWavetableSynth(var AWTSynth: TWavetableSynthesis);
    class procedure SetupDualOscillator(var AWTSynth: TWavetableSynthesis;
                                       const AWavetable1, AWavetable2: TWavetable);

    // Presets
    class function CreateSerum: TWavetableSynthesis;
    class function CreateWasp: TWavetableSynthesis;
    class function CreatePPG: TWavetableSynthesis;
    class function CreateVocal: TWavetableSynthesis;
    class function CreateMetallic: TWavetableSynthesis;
    class function CreateGlass: TWavetableSynthesis;
    class function CreateOrgan: TWavetableSynthesis;
    class function CreateEvolving: TWavetableSynthesis;
    class function CreateDigitalChaos: TWavetableSynthesis;

    // Custom wavetable from external data (e.g., loaded WAV files)
    class function CreateFromCustomWavetable(const AWavetable: TWavetable): TWavetableSynthesis;

    // Control
    class procedure StartWavetableAttack(var AWTSynth: TWavetableSynthesis);
    class procedure StartWavetableRelease(var AWTSynth: TWavetableSynthesis);

    // Status check - returns True if all oscillators have finished release (ADSR in Idle)
    class function IsWavetableReleaseComplete(const AWTSynth: TWavetableSynthesis): Boolean;

    // Internal functions
    class function GenerateWavetableSample(const AWavetable: TWavetable;
                                          AWavePos, ASamplePos: Single): Single;
  end;

implementation

// Initialize wavetable synthesis
class procedure TSedaiWavetableProcessor.InitializeWavetableSynth(var AWTSynth: TWavetableSynthesis);
var
  i: Integer;
begin
  FillChar(AWTSynth, SizeOf(TWavetableSynthesis), 0);
  AWTSynth.IsActive := True;
  AWTSynth.OscillatorCount := 1;
  AWTSynth.MixMode := 0;
  AWTSynth.FilterCutoff := 1.0;

  for i := 0 to 3 do
  begin
    with AWTSynth.Oscillators[i] do
    begin
      IsActive := (i = 0);
      WavePosition := 0.0;
      SamplePosition := 0.0;
      Amplitude := 0.6;  // Good volume
      FreqRatio := 1.0;

      // FIXED: ADSR for MIDI - INFINITE SUSTAIN until NOTE OFF
      ADSR := CreateADSR(
        0.01,  // 10ms attack - anti click
        0.05,  // Fast decay
        0.9,   // HIGH sustain (90%) - THE KEY!
        0.5    // Medium release when NOTE OFF arrives
      );
    end;
  end;
end;

// Add this implementation
class procedure TSedaiWavetableProcessor.SetupDualOscillator(var AWTSynth: TWavetableSynthesis;
  const AWavetable1, AWavetable2: TWavetable);
begin
  AWTSynth.OscillatorCount := 2;

  // Setup first oscillator
  AWTSynth.Oscillators[0].IsActive := True;
  AWTSynth.Oscillators[0].Wavetable := AWavetable1;
  AWTSynth.Oscillators[0].WavePosition := 0.0;
  AWTSynth.Oscillators[0].SamplePosition := 0.0;
  AWTSynth.Oscillators[0].Amplitude := 0.5;
  AWTSynth.Oscillators[0].FreqRatio := 1.0;
  AWTSynth.Oscillators[0].ADSR := CreateADSR(0.1, 0.1, 0.8, 0.5);

  // Setup second oscillator
  AWTSynth.Oscillators[1].IsActive := True;
  AWTSynth.Oscillators[1].Wavetable := AWavetable2;
  AWTSynth.Oscillators[1].WavePosition := 0.0;
  AWTSynth.Oscillators[1].SamplePosition := 0.0;
  AWTSynth.Oscillators[1].Amplitude := 0.5;
  AWTSynth.Oscillators[1].FreqRatio := 1.02; // Slight detune
  AWTSynth.Oscillators[1].ADSR := CreateADSR(0.1, 0.1, 0.8, 0.5);
end;

// Core wavetable processing with interpolation
class function TSedaiWavetableProcessor.ProcessWavetable(var AWTSynth: TWavetableSynthesis;
  ABaseFreq: Single; ASampleRate: Single; ADeltaTime: Single): Single;
var
  i: Integer;
  AOscOutput: Single;
  ATotalOutput: Single;
begin
  Result := 0.0;
  ATotalOutput := 0.0;

  if not AWTSynth.IsActive then Exit;

  // Process each oscillator
  for i := 0 to AWTSynth.OscillatorCount - 1 do
  begin
    with AWTSynth.Oscillators[i] do
    begin
      if IsActive and Wavetable.IsLoaded then
      begin
        // Update ADSR
        TSedaiADSRProcessor.ProcessADSR(ADSR, ADeltaTime);

        // Calculate sample position increment
        SamplePosition := SamplePosition +
          (ABaseFreq * FreqRatio * Wavetable.SampleLength / ASampleRate);

        // Wrap sample position
        while SamplePosition >= Wavetable.SampleLength do
          SamplePosition := SamplePosition - Wavetable.SampleLength;

        // Generate oscillator output with wavetable interpolation
        AOscOutput := GenerateWavetableSample(Wavetable, WavePosition, SamplePosition);
        AOscOutput := AOscOutput * Amplitude * ADSR.Level;

        // Mix oscillators based on mix mode
        case AWTSynth.MixMode of
          0: ATotalOutput := ATotalOutput + AOscOutput; // Add
          1: if i = 0 then ATotalOutput := AOscOutput   // Multiply
             else ATotalOutput := ATotalOutput * AOscOutput;
          2: if i = 0 then ATotalOutput := AOscOutput   // Ring mod
             else ATotalOutput := ATotalOutput * AOscOutput * 2.0;
        end;
      end;
    end;
  end;

  // Apply simple lowpass filter simulation
  Result := ATotalOutput;
  if AWTSynth.FilterCutoff < 1.0 then
    Result := Result * AWTSynth.FilterCutoff;
end;

// Generate interpolated sample from wavetable
class function TSedaiWavetableProcessor.GenerateWavetableSample(const AWavetable: TWavetable;
  AWavePos, ASamplePos: Single): Single;
var
  AWaveIndex1, AWaveIndex2: Integer;
  ASampleIndex1, ASampleIndex2: Integer;
  AWaveFrac, ASampleFrac: Single;
  ASample1, ASample2, ASample3, ASample4: Single;
  AInterp1, AInterp2: Single;
begin
  // Bilinear interpolation between waves and samples
  AWaveIndex1 := Trunc(AWavePos);
  AWaveIndex2 := AWaveIndex1 + 1;
  AWaveFrac := Frac(AWavePos);

  ASampleIndex1 := Trunc(ASamplePos);
  ASampleIndex2 := ASampleIndex1 + 1;
  ASampleFrac := Frac(ASamplePos);

  // Clamp indices
  if AWaveIndex1 >= AWavetable.WaveCount then AWaveIndex1 := AWavetable.WaveCount - 1;
  if AWaveIndex2 >= AWavetable.WaveCount then AWaveIndex2 := 0; // Wrap
  if ASampleIndex2 >= AWavetable.SampleLength then ASampleIndex2 := 0; // Wrap

  // Get four corner samples
  ASample1 := AWavetable.Samples[AWaveIndex1][ASampleIndex1];
  ASample2 := AWavetable.Samples[AWaveIndex1][ASampleIndex2];
  ASample3 := AWavetable.Samples[AWaveIndex2][ASampleIndex1];
  ASample4 := AWavetable.Samples[AWaveIndex2][ASampleIndex2];

  // Interpolate
  AInterp1 := ASample1 + (ASample2 - ASample1) * ASampleFrac;
  AInterp2 := ASample3 + (ASample4 - ASample3) * ASampleFrac;

  Result := AInterp1 + (AInterp2 - AInterp1) * AWaveFrac;
end;

// Create builtin wavetables
class function TSedaiWavetableProcessor.CreateBuiltinWavetable(AType: TWavetableType): TWavetable;
var
  i, j: Integer;
  APhase: Single;
begin
  Result.IsLoaded := False;
  Result.WaveCount := 64;  // 64 waves per table
  Result.SampleLength := 2048; // 2048 samples per wave

  SetLength(Result.Samples, Result.WaveCount, Result.SampleLength);

  case AType of
    wtSerum:
      begin
        Result.Name := 'Serum Basic';
        // Generate morphing sine to saw
        for i := 0 to Result.WaveCount - 1 do
        begin
          for j := 0 to Result.SampleLength - 1 do
          begin
            APhase := (j / Result.SampleLength) * 2.0 * Pi;
            // Morph from sine to saw
            Result.Samples[i][j] :=
              Sin(APhase) * (1.0 - i / Result.WaveCount) +
              ((j / Result.SampleLength) * 2.0 - 1.0) * (i / Result.WaveCount);
          end;
        end;
      end;

    wtWasp:
      begin
        Result.Name := 'Wasp Analog';
        // Generate morphing square to pulse waves
        for i := 0 to Result.WaveCount - 1 do
        begin
          for j := 0 to Result.SampleLength - 1 do
          begin
            APhase := j / Result.SampleLength;
            // Pulse width modulation
            if APhase < (0.1 + 0.8 * i / Result.WaveCount) then
              Result.Samples[i][j] := 1.0
            else
              Result.Samples[i][j] := -1.0;
          end;
        end;
      end;

    wtPPG:
      begin
        Result.Name := 'PPG Digital';
        // Generate digital waveforms
        for i := 0 to Result.WaveCount - 1 do
        begin
          for j := 0 to Result.SampleLength - 1 do
          begin
            APhase := (j / Result.SampleLength) * 2.0 * Pi;
            // Quantized sine waves
            Result.Samples[i][j] :=
              Round(Sin(APhase * (1 + i div 8)) * (4 + i div 8)) / (4 + i div 8);
          end;
        end;
      end;

    wtVocal:
      begin
        Result.Name := 'Vocal Formants';
        // Generate formant-based vocal wavetable
        for i := 0 to Result.WaveCount - 1 do
        begin
          for j := 0 to Result.SampleLength - 1 do
          begin
            APhase := (j / Result.SampleLength) * 2.0 * Pi;
            // Simulate vocal formants with multiple sine waves
            // Formant frequencies morph from 'ah' to 'ee' sound
            Result.Samples[i][j] :=
              Sin(APhase) * 0.5 +  // Fundamental
              Sin(APhase * (2.0 + i / 32.0)) * 0.3 +  // First formant
              Sin(APhase * (3.5 + i / 16.0)) * 0.2 +  // Second formant
              Sin(APhase * (5.0 + i / 10.0)) * 0.15;  // Third formant
          end;
        end;
      end;

    wtMetallic:
      begin
        Result.Name := 'Metallic';
        // Generate inharmonic metallic wavetable
        for i := 0 to Result.WaveCount - 1 do
        begin
          for j := 0 to Result.SampleLength - 1 do
          begin
            APhase := (j / Result.SampleLength) * 2.0 * Pi;
            // Inharmonic partials create metallic timbre
            Result.Samples[i][j] :=
              Sin(APhase * 1.0) * 0.4 +
              Sin(APhase * 2.51) * 0.25 * (1.0 - i / 128.0) +
              Sin(APhase * 3.93) * 0.2 * (1.0 - i / 96.0) +
              Sin(APhase * 5.19) * 0.15 * (1.0 - i / 64.0);
          end;
        end;
      end;

    wtGlass:
      begin
        Result.Name := 'Glass';
        // Generate bright bell-like wavetable
        for i := 0 to Result.WaveCount - 1 do
        begin
          for j := 0 to Result.SampleLength - 1 do
          begin
            APhase := (j / Result.SampleLength) * 2.0 * Pi;
            // High harmonics with exponential decay
            Result.Samples[i][j] :=
              Sin(APhase) * 0.3 +
              Sin(APhase * 3) * 0.25 +
              Sin(APhase * 5) * 0.2 +
              Sin(APhase * 7) * 0.15 * (1.0 - i / 80.0) +
              Sin(APhase * 9) * 0.1 * (1.0 - i / 64.0);
          end;
        end;
      end;

    wtOrgan:
      begin
        Result.Name := 'Hammond Organ';
        // Generate Hammond-style additive wavetable
        for i := 0 to Result.WaveCount - 1 do
        begin
          for j := 0 to Result.SampleLength - 1 do
          begin
            APhase := (j / Result.SampleLength) * 2.0 * Pi;
            // Hammond drawbar-style harmonic series
            // Morphs between different drawbar settings
            Result.Samples[i][j] :=
              Sin(APhase * 1) * (0.5 + i / 256.0) +      // 16'
              Sin(APhase * 2) * 0.3 +                    // 8'
              Sin(APhase * 3) * (0.4 - i / 256.0) +      // 5 1/3'
              Sin(APhase * 4) * 0.25 +                   // 4'
              Sin(APhase * 6) * (0.2 + i / 320.0);       // 2 2/3'
          end;
        end;
      end;

    wtEvolving:
      begin
        Result.Name := 'Evolving Pad';
        // Generate complex evolving wavetable
        for i := 0 to Result.WaveCount - 1 do
        begin
          for j := 0 to Result.SampleLength - 1 do
          begin
            APhase := (j / Result.SampleLength) * 2.0 * Pi;
            // Multiple harmonics with phase relationships that evolve
            Result.Samples[i][j] :=
              Sin(APhase + i / 20.0) * 0.3 +
              Sin(APhase * 2 - i / 15.0) * 0.25 +
              Sin(APhase * 3 + i / 10.0) * 0.2 +
              Sin(APhase * 5 - i / 25.0) * 0.15 +
              Sin(APhase * 7 + i / 30.0) * 0.1;
          end;
        end;
      end;

    wtDigital:
      begin
        Result.Name := 'Digital Chaos';
        // Generate chaotic digital wavetable
        for i := 0 to Result.WaveCount - 1 do
        begin
          for j := 0 to Result.SampleLength - 1 do
          begin
            APhase := (j / Result.SampleLength) * 2.0 * Pi;
            // Complex modulation and bit-crushing effect
            // Ensure divisor is always >= 1 to avoid division by zero
            Result.Samples[i][j] :=
              Round(Sin(APhase * (1 + i / 32.0) + Sin(APhase * 7) * (i / 128.0)) * Max(1, 8 - i div 10)) /
              Max(1, 8 - i div 10);
          end;
        end;
      end;
  else
    begin
      // Default: Simple sine wave for unhandled types
      Result.Name := 'Default Sine';
      for i := 0 to Result.WaveCount - 1 do
      begin
        for j := 0 to Result.SampleLength - 1 do
        begin
          APhase := (j / Result.SampleLength) * 2.0 * Pi;
          Result.Samples[i][j] := Sin(APhase);
        end;
      end;
    end;
  end;

  Result.IsLoaded := True;
end;

// Oscillator control
class procedure TSedaiWavetableProcessor.SetOscillatorWave(var AOsc: TWavetableOscillator;
  APosition: Single);
begin
  if AOsc.Wavetable.IsLoaded then
  begin
    AOsc.WavePosition := APosition;
    if AOsc.WavePosition < 0 then AOsc.WavePosition := 0;
    if AOsc.WavePosition >= AOsc.Wavetable.WaveCount then
      AOsc.WavePosition := AOsc.Wavetable.WaveCount - 1;
  end;
end;

class procedure TSedaiWavetableProcessor.ScanWaves(var AOsc: TWavetableOscillator;
  ASpeed: Single; ADeltaTime: Single);
begin
  if AOsc.Wavetable.IsLoaded then
  begin
    AOsc.WavePosition := AOsc.WavePosition + (ASpeed * ADeltaTime);

    // Wrap around
    while AOsc.WavePosition >= AOsc.Wavetable.WaveCount do
      AOsc.WavePosition := AOsc.WavePosition - AOsc.Wavetable.WaveCount;
    while AOsc.WavePosition < 0 do
      AOsc.WavePosition := AOsc.WavePosition + AOsc.Wavetable.WaveCount;
  end;
end;

// Presets
class function TSedaiWavetableProcessor.CreateSerum: TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);
  Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtSerum);
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.7;  // Higher volume

  // MIDI ADSR for Serum - SUSTAIN until NOTE OFF
  Result.Oscillators[0].ADSR := CreateADSR(
    0.01,  // Fast attack
    0.1,   // Decay
    0.95,  // VERY HIGH sustain - stays here until NOTE OFF!
    0.3    // Release when NOTE OFF
  );
  Result.OscillatorCount := 1;
end;

class function TSedaiWavetableProcessor.CreateWasp: TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);
  Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtWasp);
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.7;

  // MIDI ADSR for Wasp - SUSTAIN until NOTE OFF
  Result.Oscillators[0].ADSR := CreateADSR(
    0.005, // Very fast attack
    0.05,  // Decay
    0.9,   // HIGH sustain - stays here!
    0.25   // Fast release when NOTE OFF
  );
  Result.OscillatorCount := 1;
end;

class function TSedaiWavetableProcessor.CreatePPG: TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);
  Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtPPG);
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.7;

  // MIDI ADSR for PPG - SUSTAIN until NOTE OFF
  Result.Oscillators[0].ADSR := CreateADSR(
    0.002, // Almost instant attack
    0.02,  // Fast decay
    0.95,  // VERY HIGH sustain - stays here!
    0.15   // Fast release when NOTE OFF
  );
  Result.OscillatorCount := 1;
end;

class function TSedaiWavetableProcessor.CreateVocal: TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);
  Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtVocal);
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.6;
  Result.Oscillators[0].FreqRatio := 1.0;

  // Vocal-like slow attack and sustain
  Result.Oscillators[0].ADSR := CreateADSR(
    0.15,  // Slow attack for natural vocal envelope
    0.2,   // Gradual decay
    0.85,  // High sustain
    0.8    // Slow release
  );
  Result.OscillatorCount := 1;
  Result.FilterCutoff := 0.8;  // Slight filtering for realism
end;

class function TSedaiWavetableProcessor.CreateMetallic: TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);
  Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtMetallic);
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.5;
  Result.Oscillators[0].FreqRatio := 1.0;

  // Sharp attack for metallic percussion
  Result.Oscillators[0].ADSR := CreateADSR(
    0.001, // Very fast attack
    0.3,   // Medium decay
    0.4,   // Lower sustain for bell-like envelope
    1.5    // Long release for shimmer
  );

  // Add second oscillator slightly detuned
  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].Wavetable := CreateBuiltinWavetable(wtMetallic);
  Result.Oscillators[1].Amplitude := 0.4;
  Result.Oscillators[1].FreqRatio := 1.007; // Slight detune
  Result.Oscillators[1].ADSR := CreateADSR(0.001, 0.35, 0.35, 1.6);
  Result.OscillatorCount := 2;
end;

class function TSedaiWavetableProcessor.CreateGlass: TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);
  Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtGlass);
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.55;
  Result.Oscillators[0].FreqRatio := 1.0;

  // Bright bell-like envelope
  Result.Oscillators[0].ADSR := CreateADSR(
    0.005, // Fast attack
    0.4,   // Decay to create bell character
    0.3,   // Low sustain
    2.0    // Very long release for shimmer
  );

  // Add octave up for brightness
  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].Wavetable := CreateBuiltinWavetable(wtGlass);
  Result.Oscillators[1].Amplitude := 0.25;
  Result.Oscillators[1].FreqRatio := 2.0; // Octave up
  Result.Oscillators[1].ADSR := CreateADSR(0.005, 0.5, 0.2, 2.2);
  Result.OscillatorCount := 2;
end;

class function TSedaiWavetableProcessor.CreateOrgan: TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);
  Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtOrgan);
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.7;
  Result.Oscillators[0].FreqRatio := 1.0;

  // Organ-like instant attack and sustain
  Result.Oscillators[0].ADSR := CreateADSR(
    0.001, // Instant attack
    0.01,  // Minimal decay
    0.95,  // Very high sustain - organ sustains at full level
    0.05   // Fast release - organ clicks off
  );
  Result.OscillatorCount := 1;
end;

class function TSedaiWavetableProcessor.CreateEvolving: TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);
  Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtEvolving);
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.4;
  Result.Oscillators[0].FreqRatio := 1.0;
  Result.Oscillators[0].WavePosition := 0.0;

  // Very slow evolving pad
  Result.Oscillators[0].ADSR := CreateADSR(
    1.5,   // Very slow attack
    0.8,   // Gradual decay
    0.9,   // High sustain
    2.5    // Very long release
  );

  // Add detuned oscillator for richness
  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].Wavetable := CreateBuiltinWavetable(wtEvolving);
  Result.Oscillators[1].Amplitude := 0.4;
  Result.Oscillators[1].FreqRatio := 0.997; // Slight detune
  Result.Oscillators[1].WavePosition := 32.0; // Start at different wave position
  Result.Oscillators[1].ADSR := CreateADSR(1.6, 0.9, 0.88, 2.6);

  // Add third oscillator for complexity
  Result.Oscillators[2].IsActive := True;
  Result.Oscillators[2].Wavetable := CreateBuiltinWavetable(wtEvolving);
  Result.Oscillators[2].Amplitude := 0.35;
  Result.Oscillators[2].FreqRatio := 2.003; // Octave + slight detune
  Result.Oscillators[2].WavePosition := 16.0;
  Result.Oscillators[2].ADSR := CreateADSR(1.7, 1.0, 0.85, 2.7);

  Result.OscillatorCount := 3;
  Result.FilterCutoff := 0.85;
end;

class function TSedaiWavetableProcessor.CreateDigitalChaos: TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);
  Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtDigital);
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.6;
  Result.Oscillators[0].FreqRatio := 1.0;

  // Punchy digital character
  Result.Oscillators[0].ADSR := CreateADSR(
    0.002, // Very fast attack
    0.1,   // Quick decay
    0.7,   // Medium sustain
    0.3    // Medium release
  );

  // Add heavily detuned oscillator for chaos
  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].Wavetable := CreateBuiltinWavetable(wtDigital);
  Result.Oscillators[1].Amplitude := 0.5;
  Result.Oscillators[1].FreqRatio := 1.5; // Fifth up
  Result.Oscillators[1].WavePosition := 48.0; // Different wave position
  Result.Oscillators[1].ADSR := CreateADSR(0.003, 0.12, 0.65, 0.35);

  Result.OscillatorCount := 2;
  Result.MixMode := 2; // Ring modulation for extra chaos
end;

// Create synth from custom/loaded wavetable (e.g., AKWF files)
class function TSedaiWavetableProcessor.CreateFromCustomWavetable(const AWavetable: TWavetable): TWavetableSynthesis;
begin
  InitializeWavetableSynth(Result);

  if not AWavetable.IsLoaded then
  begin
    // Fallback to Serum if wavetable not loaded
    Result.Oscillators[0].Wavetable := CreateBuiltinWavetable(wtSerum);
  end
  else
  begin
    // Use the custom wavetable
    Result.Oscillators[0].Wavetable := AWavetable;
  end;

  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].Amplitude := 0.7;
  Result.Oscillators[0].FreqRatio := 1.0;
  Result.Oscillators[0].WavePosition := 0.0;

  // Good generic ADSR for custom wavetables
  Result.Oscillators[0].ADSR := CreateADSR(
    0.01,  // Fast attack
    0.1,   // Medium decay
    0.9,   // High sustain
    0.3    // Medium release
  );
  Result.OscillatorCount := 1;
end;

// Control methods
class procedure TSedaiWavetableProcessor.StartWavetableAttack(var AWTSynth: TWavetableSynthesis);
var
  i: Integer;
begin
  for i := 0 to AWTSynth.OscillatorCount - 1 do
    TSedaiADSRProcessor.StartADSRAttack(AWTSynth.Oscillators[i].ADSR);
end;

class procedure TSedaiWavetableProcessor.StartWavetableRelease(var AWTSynth: TWavetableSynthesis);
var
  i: Integer;
begin
  for i := 0 to AWTSynth.OscillatorCount - 1 do
    TSedaiADSRProcessor.StartADSRRelease(AWTSynth.Oscillators[i].ADSR);
end;

class function TSedaiWavetableProcessor.IsWavetableReleaseComplete(const AWTSynth: TWavetableSynthesis): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to AWTSynth.OscillatorCount - 1 do
  begin
    if TSedaiADSRProcessor.IsADSRActive(AWTSynth.Oscillators[i].ADSR) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

class procedure TSedaiWavetableProcessor.UnloadWavetable(var AWavetable: TWavetable);
begin
  SetLength(AWavetable.Samples, 0, 0);
  AWavetable.IsLoaded := False;
  AWavetable.WaveCount := 0;
  AWavetable.SampleLength := 0;
end;

end.
