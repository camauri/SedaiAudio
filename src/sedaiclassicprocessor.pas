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

unit SedaiClassicProcessor;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiADSRProcessor, SedaiWaveGenerator;

type
  // Classic oscillator
  TClassicOscillator = record
    IsActive: Boolean;
    WaveType: TWaveType;
    Phase: Single;
    Frequency: Single;
    Amplitude: Single;
    FreqRatio: Single;      // For detuning/harmonics
    PulseWidth: Single;     // For pulse waves
    ADSR: TADSR;
  end;

  // Classic synthesis structure
  TClassicSynthesis = record
    IsActive: Boolean;
    Oscillators: array[0..2] of TClassicOscillator; // Up to 3 oscillators
    OscillatorCount: Integer;

    // Subtractive synthesis parameters
    FilterType: Integer;    // 0=LP, 1=HP, 2=BP
    FilterCutoff: Single;   // 0.0 to 1.0
    FilterResonance: Single; // 0.0 to 1.0
    FilterEnvAmount: Single; // Filter envelope amount
    FilterADSR: TADSR;      // Filter envelope

    // LFO
    LFORate: Single;
    LFODepth: Single;
    LFOTarget: Integer;     // 0=pitch, 1=filter, 2=amp
    LFOPhase: Single;

    // Effects
    DistortionAmount: Single;
    NoiseLevel: Single;
  end;

  TSedaiClassicProcessor = class
  public
    // Core classic processing
    class function ProcessClassic(var AClassicSynth: TClassicSynthesis;
                                 ABaseFreq: Single; ASampleRate: Single;
                                 ADeltaTime: Single): Single;

    // Synthesis setup
    class procedure InitializeClassicSynth(var AClassicSynth: TClassicSynthesis);
    class procedure SetupSingleOscillator(var AClassicSynth: TClassicSynthesis;
                                         AWaveType: TWaveType);
    class procedure SetupDualOscillator(var AClassicSynth: TClassicSynthesis;
                                       AWave1, AWave2: TWaveType; ADetune: Single = 0.02);

    // Oscillator control
    class procedure SetOscillatorWave(var AClassicSynth: TClassicSynthesis;
                                     AOscIndex: Integer; AWaveType: TWaveType);
    class procedure SetOscillatorDetune(var AClassicSynth: TClassicSynthesis;
                                       AOscIndex: Integer; ADetune: Single);

    // Filter control
    class procedure SetFilter(var AClassicSynth: TClassicSynthesis;
                             AType: Integer; ACutoff, AResonance: Single);

    // LFO control
    class procedure SetLFO(var AClassicSynth: TClassicSynthesis;
                          ARate, ADepth: Single; ATarget: Integer);

    // Presets
    class function CreateAnalogLead: TClassicSynthesis;
    class function CreateAnalogBass: TClassicSynthesis;
    class function CreateAnalogPad: TClassicSynthesis;

    // NEW: Additional presets
    class function CreateStrings: TClassicSynthesis;
    class function CreateBrass: TClassicSynthesis;
    class function CreateOrgan: TClassicSynthesis;
    class function CreatePluck: TClassicSynthesis;
    class function CreateSynthKeys: TClassicSynthesis;
    class function CreateWarmBass: TClassicSynthesis;

    // Control
    class procedure StartClassicAttack(var AClassicSynth: TClassicSynthesis);
    class procedure StartClassicRelease(var AClassicSynth: TClassicSynthesis);

    // Status check - returns True if all oscillators have finished release (ADSR in Idle)
    class function IsClassicReleaseComplete(const AClassicSynth: TClassicSynthesis): Boolean;
  end;

implementation

// Initialize classic synthesis
class procedure TSedaiClassicProcessor.InitializeClassicSynth(var AClassicSynth: TClassicSynthesis);
var
  i: Integer;
begin
  FillChar(AClassicSynth, SizeOf(TClassicSynthesis), 0);
  AClassicSynth.IsActive := True;
  AClassicSynth.OscillatorCount := 1;
  AClassicSynth.FilterCutoff := 1.0;
  AClassicSynth.FilterResonance := 0.0;
  AClassicSynth.LFORate := 5.0;

  for i := 0 to 2 do
  begin
    with AClassicSynth.Oscillators[i] do
    begin
      IsActive := (i = 0); // Only first oscillator active by default
      WaveType := wtSine;
      Phase := 0.0;
      Amplitude := 0.5;
      FreqRatio := 1.0;
      PulseWidth := 0.5;
      ADSR := CreateADSR(0.1, 0.1, 0.8, 0.5);
    end;
  end;
end;

// Debug counter for sample output
var
  GDebugSampleCounter: Integer = 0;

// Core classic synthesis processing
class function TSedaiClassicProcessor.ProcessClassic(var AClassicSynth: TClassicSynthesis;
  ABaseFreq: Single; ASampleRate: Single; ADeltaTime: Single): Single;
var
  i: Integer;
  AOscOutput: Single;
  ATotalOutput: Single;
  ALFOValue: Single;
begin
  Result := 0.0;
  ATotalOutput := 0.0;

  if not AClassicSynth.IsActive then Exit;

  // Update LFO
  AClassicSynth.LFOPhase := AClassicSynth.LFOPhase +
    (AClassicSynth.LFORate * 2.0 * Pi / ASampleRate);
  if AClassicSynth.LFOPhase >= 2.0 * Pi then
    AClassicSynth.LFOPhase := AClassicSynth.LFOPhase - 2.0 * Pi;

  ALFOValue := Sin(AClassicSynth.LFOPhase) * AClassicSynth.LFODepth;

  // Process each oscillator
  for i := 0 to AClassicSynth.OscillatorCount - 1 do
  begin
    with AClassicSynth.Oscillators[i] do
    begin
      if IsActive then
      begin
        // Update ADSR
        TSedaiADSRProcessor.ProcessADSR(ADSR, ADeltaTime);

        // Calculate frequency with detune and LFO
        Frequency := ABaseFreq * FreqRatio;
        if AClassicSynth.LFOTarget = 0 then // Pitch LFO
          Frequency := Frequency * (1.0 + ALFOValue);

        // Update phase
        Phase := Phase + (Frequency * 2.0 * Pi / ASampleRate);
        if Phase >= 2.0 * Pi then
          Phase := Phase - 2.0 * Pi;

        // Generate oscillator output
        AOscOutput := TSedaiWaveGenerator.GenerateWaveform(WaveType, Phase);

        // Apply ADSR and amplitude
        AOscOutput := AOscOutput * Amplitude * ADSR.Level;
        ATotalOutput := ATotalOutput + AOscOutput;

        {$IFDEF SEDAI_DEBUG_SYNTH}
        // Debug output ogni 44100 samples (circa 1 secondo)
        Inc(GDebugSampleCounter);
        if (GDebugSampleCounter mod 44100) = 0 then
        begin
          WriteLn(Format('DEBUG ProcessClassic: Osc%d WaveType=%d Phase=%.2f Freq=%.1f Amp=%.2f ADSR.Level=%.4f ADSR.Phase=%d Output=%.4f',
            [i, Ord(WaveType), Phase, Frequency, Amplitude, ADSR.Level, Ord(ADSR.Phase), AOscOutput]));
        end;
        {$ENDIF}
      end;
    end;
  end;

  // Apply simple filter (simplified implementation)
  if AClassicSynth.FilterCutoff < 1.0 then
    ATotalOutput := ATotalOutput * AClassicSynth.FilterCutoff;

  Result := ATotalOutput;
end;

// Setup presets
class function TSedaiClassicProcessor.CreateAnalogLead: TClassicSynthesis;
begin
  InitializeClassicSynth(Result);
  SetupSingleOscillator(Result, wtSawtooth);
  SetFilter(Result, 0, 0.7, 0.3); // Lowpass with some resonance
  SetLFO(Result, 6.0, 0.1, 1); // Filter LFO
  Result.DistortionAmount := 0.2;
end;

class function TSedaiClassicProcessor.CreateAnalogBass: TClassicSynthesis;
begin
  InitializeClassicSynth(Result);
  SetupSingleOscillator(Result, wtSquare);
  SetFilter(Result, 0, 0.4, 0.6); // Deep lowpass with resonance
  Result.Oscillators[0].PulseWidth := 0.3; // Narrow pulse
end;

class function TSedaiClassicProcessor.CreateAnalogPad: TClassicSynthesis;
begin
  InitializeClassicSynth(Result);
  SetupDualOscillator(Result, wtSawtooth, wtSquare, 0.02); // Slight detune
  SetFilter(Result, 0, 0.6, 0.2);
  SetLFO(Result, 3.0, 0.05, 1); // Slow filter sweep
end;

// Setup methods
class procedure TSedaiClassicProcessor.SetupSingleOscillator(var AClassicSynth: TClassicSynthesis;
  AWaveType: TWaveType);
begin
  AClassicSynth.OscillatorCount := 1;
  AClassicSynth.Oscillators[0].IsActive := True;
  AClassicSynth.Oscillators[0].WaveType := AWaveType;
  AClassicSynth.Oscillators[0].Amplitude := 0.8;
end;

class procedure TSedaiClassicProcessor.SetupDualOscillator(var AClassicSynth: TClassicSynthesis;
  AWave1, AWave2: TWaveType; ADetune: Single);
begin
  AClassicSynth.OscillatorCount := 2;

  AClassicSynth.Oscillators[0].IsActive := True;
  AClassicSynth.Oscillators[0].WaveType := AWave1;
  AClassicSynth.Oscillators[0].Amplitude := 0.5;
  AClassicSynth.Oscillators[0].FreqRatio := 1.0;

  AClassicSynth.Oscillators[1].IsActive := True;
  AClassicSynth.Oscillators[1].WaveType := AWave2;
  AClassicSynth.Oscillators[1].Amplitude := 0.5;
  AClassicSynth.Oscillators[1].FreqRatio := 1.0 + ADetune;
end;

// Control methods
class procedure TSedaiClassicProcessor.SetFilter(var AClassicSynth: TClassicSynthesis;
  AType: Integer; ACutoff, AResonance: Single);
begin
  AClassicSynth.FilterType := AType;
  AClassicSynth.FilterCutoff := ACutoff;
  AClassicSynth.FilterResonance := AResonance;
end;

class procedure TSedaiClassicProcessor.SetLFO(var AClassicSynth: TClassicSynthesis;
  ARate, ADepth: Single; ATarget: Integer);
begin
  AClassicSynth.LFORate := ARate;
  AClassicSynth.LFODepth := ADepth;
  AClassicSynth.LFOTarget := ATarget;
end;

class procedure TSedaiClassicProcessor.StartClassicAttack(var AClassicSynth: TClassicSynthesis);
var
  i: Integer;
begin
  for i := 0 to AClassicSynth.OscillatorCount - 1 do
    TSedaiADSRProcessor.StartADSRAttack(AClassicSynth.Oscillators[i].ADSR);
end;

class procedure TSedaiClassicProcessor.StartClassicRelease(var AClassicSynth: TClassicSynthesis);
var
  i: Integer;
begin
  for i := 0 to AClassicSynth.OscillatorCount - 1 do
    TSedaiADSRProcessor.StartADSRRelease(AClassicSynth.Oscillators[i].ADSR);
end;

class function TSedaiClassicProcessor.IsClassicReleaseComplete(const AClassicSynth: TClassicSynthesis): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to AClassicSynth.OscillatorCount - 1 do
  begin
    if TSedaiADSRProcessor.IsADSRActive(AClassicSynth.Oscillators[i].ADSR) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

// Additional control methods
class procedure TSedaiClassicProcessor.SetOscillatorWave(var AClassicSynth: TClassicSynthesis;
  AOscIndex: Integer; AWaveType: TWaveType);
begin
  if (AOscIndex >= 0) and (AOscIndex < 3) then
    AClassicSynth.Oscillators[AOscIndex].WaveType := AWaveType;
end;

class procedure TSedaiClassicProcessor.SetOscillatorDetune(var AClassicSynth: TClassicSynthesis;
  AOscIndex: Integer; ADetune: Single);
begin
  if (AOscIndex >= 0) and (AOscIndex < 3) then
    AClassicSynth.Oscillators[AOscIndex].FreqRatio := 1.0 + ADetune;
end;

// NEW PRESETS

// Strings - warm, slow attack, multiple detuned oscillators
class function TSedaiClassicProcessor.CreateStrings: TClassicSynthesis;
begin
  InitializeClassicSynth(Result);

  // Three detuned sawtooth oscillators for richness
  Result.OscillatorCount := 3;
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].WaveType := wtSawtooth;
  Result.Oscillators[0].Amplitude := 0.35;
  Result.Oscillators[0].FreqRatio := 1.0;
  Result.Oscillators[0].ADSR := CreateADSR(0.8, 0.3, 0.85, 1.2); // Slow attack

  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].WaveType := wtSawtooth;
  Result.Oscillators[1].Amplitude := 0.35;
  Result.Oscillators[1].FreqRatio := 1.005; // Slightly sharp
  Result.Oscillators[1].ADSR := Result.Oscillators[0].ADSR;

  Result.Oscillators[2].IsActive := True;
  Result.Oscillators[2].WaveType := wtSawtooth;
  Result.Oscillators[2].Amplitude := 0.35;
  Result.Oscillators[2].FreqRatio := 0.995; // Slightly flat
  Result.Oscillators[2].ADSR := Result.Oscillators[0].ADSR;

  SetFilter(Result, 0, 0.65, 0.15); // Gentle lowpass
  SetLFO(Result, 4.5, 0.03, 1); // Subtle vibrato on filter
end;

// Brass - bright, moderate attack, pwm
class function TSedaiClassicProcessor.CreateBrass: TClassicSynthesis;
begin
  InitializeClassicSynth(Result);

  Result.OscillatorCount := 2;
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].WaveType := wtSawtooth;
  Result.Oscillators[0].Amplitude := 0.5;
  Result.Oscillators[0].FreqRatio := 1.0;
  Result.Oscillators[0].ADSR := CreateADSR(0.12, 0.15, 0.75, 0.3);

  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].WaveType := wtSquare;
  Result.Oscillators[1].Amplitude := 0.4;
  Result.Oscillators[1].FreqRatio := 1.01;
  Result.Oscillators[1].ADSR := Result.Oscillators[0].ADSR;

  SetFilter(Result, 0, 0.75, 0.4); // Bright with resonance
  SetLFO(Result, 5.5, 0.08, 1); // Filter modulation
  Result.DistortionAmount := 0.15; // Slight saturation
end;

// Organ - instant attack, sustained, drawbar-like
class function TSedaiClassicProcessor.CreateOrgan: TClassicSynthesis;
begin
  InitializeClassicSynth(Result);

  // Multiple sine oscillators at harmonic ratios (simulating drawbars)
  Result.OscillatorCount := 3;
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].WaveType := wtSine;
  Result.Oscillators[0].Amplitude := 0.4;
  Result.Oscillators[0].FreqRatio := 1.0; // Fundamental
  Result.Oscillators[0].ADSR := CreateADSR(0.001, 0.01, 0.95, 0.05);

  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].WaveType := wtSine;
  Result.Oscillators[1].Amplitude := 0.3;
  Result.Oscillators[1].FreqRatio := 2.0; // Octave up
  Result.Oscillators[1].ADSR := Result.Oscillators[0].ADSR;

  Result.Oscillators[2].IsActive := True;
  Result.Oscillators[2].WaveType := wtSine;
  Result.Oscillators[2].Amplitude := 0.25;
  Result.Oscillators[2].FreqRatio := 3.0; // Fifth above octave
  Result.Oscillators[2].ADSR := Result.Oscillators[0].ADSR;

  SetFilter(Result, 0, 0.9, 0.1); // Open filter
  SetLFO(Result, 6.0, 0.015, 0); // Subtle vibrato on pitch
end;

// Pluck - percussive, fast attack, no sustain
class function TSedaiClassicProcessor.CreatePluck: TClassicSynthesis;
begin
  InitializeClassicSynth(Result);

  Result.OscillatorCount := 2;
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].WaveType := wtTriangle;
  Result.Oscillators[0].Amplitude := 0.6;
  Result.Oscillators[0].FreqRatio := 1.0;
  Result.Oscillators[0].ADSR := CreateADSR(0.001, 0.08, 0.0, 0.15); // Fast decay, no sustain

  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].WaveType := wtSawtooth;
  Result.Oscillators[1].Amplitude := 0.3;
  Result.Oscillators[1].FreqRatio := 2.0; // Octave up
  Result.Oscillators[1].ADSR := CreateADSR(0.001, 0.05, 0.0, 0.1);

  SetFilter(Result, 0, 0.8, 0.25);
  Result.FilterEnvAmount := 0.5; // Filter envelope for plucky sound
end;

// Synth Keys - electric piano-like, bell-like attack
class function TSedaiClassicProcessor.CreateSynthKeys: TClassicSynthesis;
begin
  InitializeClassicSynth(Result);

  Result.OscillatorCount := 2;
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].WaveType := wtTriangle;
  Result.Oscillators[0].Amplitude := 0.5;
  Result.Oscillators[0].FreqRatio := 1.0;
  Result.Oscillators[0].ADSR := CreateADSR(0.005, 0.4, 0.35, 0.8);

  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].WaveType := wtSine;
  Result.Oscillators[1].Amplitude := 0.4;
  Result.Oscillators[1].FreqRatio := 2.003; // Slightly detuned octave
  Result.Oscillators[1].ADSR := CreateADSR(0.005, 0.3, 0.25, 0.7);

  SetFilter(Result, 0, 0.7, 0.2);
end;

// Warm Bass - sub bass with character
class function TSedaiClassicProcessor.CreateWarmBass: TClassicSynthesis;
begin
  InitializeClassicSynth(Result);

  Result.OscillatorCount := 2;
  Result.Oscillators[0].IsActive := True;
  Result.Oscillators[0].WaveType := wtTriangle; // Warm low end
  Result.Oscillators[0].Amplitude := 0.6;
  Result.Oscillators[0].FreqRatio := 1.0;
  Result.Oscillators[0].ADSR := CreateADSR(0.01, 0.08, 0.8, 0.2);

  Result.Oscillators[1].IsActive := True;
  Result.Oscillators[1].WaveType := wtSquare; // Add harmonics
  Result.Oscillators[1].Amplitude := 0.3;
  Result.Oscillators[1].FreqRatio := 1.0;
  Result.Oscillators[1].PulseWidth := 0.25; // Narrow pulse
  Result.Oscillators[1].ADSR := Result.Oscillators[0].ADSR;

  SetFilter(Result, 0, 0.35, 0.5); // Deep lowpass with resonance
  Result.DistortionAmount := 0.1; // Subtle saturation
end;

end.
