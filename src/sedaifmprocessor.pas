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

unit SedaiFMProcessor;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, ctypes, SedaiAudioTypes, SedaiADSRProcessor;

type
  TSedaiFMProcessor = class
  public
    // FM Synthesis core
    class function ProcessFM(var AFMSynth: TFMSynthesis; ABaseFreq: Single;
                            ASampleRate: Single; ADeltaTime: Single): Single;
    class procedure UpdateFMOperators(var AFMSynth: TFMSynthesis; ADeltaTime: Single);

    // FM Configuration
    class procedure InitializeFM(var AFMSynth: TFMSynthesis; AAlgorithm: TFMAlgorithm);
    class procedure SetupSimpleFM(var AFMSynth: TFMSynthesis; ACarrierRatio, AModulatorRatio: Single;
                                 AModDepth: Single);
    class procedure SetupDX7Style(var AFMSynth: TFMSynthesis; AAlgorithm: TFMAlgorithm);

    // Operator control
    class procedure SetOperatorADSR(var AFMSynth: TFMSynthesis; AOperatorIndex: Integer;
                                   AAttack, ADecay, ASustain, ARelease: Single);
    class procedure SetOperatorRatio(var AFMSynth: TFMSynthesis; AOperatorIndex: Integer;
                                    ARatio: Single);
    class procedure SetOperatorAmplitude(var AFMSynth: TFMSynthesis; AOperatorIndex: Integer;
                                        AAmplitude: Single);
    class procedure SetOperatorFeedback(var AFMSynth: TFMSynthesis; AOperatorIndex: Integer;
                                       AFeedback: Single);

    // ADSR control for operators
    class procedure StartFMAttack(var AFMSynth: TFMSynthesis);
    class procedure StartFMRelease(var AFMSynth: TFMSynthesis);
    class procedure ForceOperatorPhase(var AFMSynth: TFMSynthesis; AOperatorIndex: Integer;
                                      APhase: TADSRPhase);

    // Status check - returns True if all operators have finished release (ADSR in Idle)
    class function IsFMReleaseComplete(const AFMSynth: TFMSynthesis): Boolean;

    // Preset creation
    class function CreateElectricPiano: TFMSynthesis;
    class function CreateBrass: TFMSynthesis;
    class function CreateBell: TFMSynthesis;
    class function CreateOrgan: TFMSynthesis;
    class function CreateSynthLead: TFMSynthesis;
    class function CreateSynthBass: TFMSynthesis;

    // NEW: Additional FM presets
    class function CreateChoir: TFMSynthesis;
    class function CreateFMStrings: TFMSynthesis;
    class function CreateFMPad: TFMSynthesis;
    class function CreateMarimba: TFMSynthesis;
    class function CreateFlute: TFMSynthesis;
    class function CreateChurchBell: TFMSynthesis;

    // Algorithm implementations
    class function ProcessSimpleFM(var AFMSynth: TFMSynthesis; ABaseFreq: Single;
                                  ASampleRate: Single): Single;
    class function ProcessStackFM(var AFMSynth: TFMSynthesis; ABaseFreq: Single;
                                 ASampleRate: Single): Single;
    class function ProcessParallelFM(var AFMSynth: TFMSynthesis; ABaseFreq: Single;
                                    ASampleRate: Single): Single;
    class function ProcessFeedbackFM(var AFMSynth: TFMSynthesis; ABaseFreq: Single;
                                    ASampleRate: Single): Single;
    class function ProcessComplexFM(var AFMSynth: TFMSynthesis; ABaseFreq: Single;
                                   ASampleRate: Single): Single;

    // Utility functions
    class function IsFMActive(const AFMSynth: TFMSynthesis): Boolean;
    class procedure ResetFM(var AFMSynth: TFMSynthesis);
    class function GetFMInfo(const AFMSynth: TFMSynthesis): string;
  end;

implementation

// Main FM processing function
class function TSedaiFMProcessor.ProcessFM(var AFMSynth: TFMSynthesis; ABaseFreq: Single; ASampleRate: Single; ADeltaTime: Single): Single;
begin
  Result := 0.0;

  if not AFMSynth.IsActive then
  begin
    //WriteLn('DEBUG ProcessFM: FM not active!');
    Exit;
  end;

  //// DEBUG
  //if Random(5000) = 0 then
  //  WriteLn('DEBUG ProcessFM: Algorithm=', Ord(AFMSynth.Algorithm), ', OpCount=', AFMSynth.OperatorCount, ', ModDepth=', AFMSynth.ModulationDepth:0:3);

  // Update all operator ADSR envelopes
  UpdateFMOperators(AFMSynth, ADeltaTime);

  // Process based on algorithm
  case AFMSynth.Algorithm of
    fmSimple:   Result := ProcessSimpleFM(AFMSynth, ABaseFreq, ASampleRate);
    fmStack:    Result := ProcessStackFM(AFMSynth, ABaseFreq, ASampleRate);
    fmParallel: Result := ProcessParallelFM(AFMSynth, ABaseFreq, ASampleRate);
    fmFeedback: Result := ProcessFeedbackFM(AFMSynth, ABaseFreq, ASampleRate);
    fmComplex:  Result := ProcessComplexFM(AFMSynth, ABaseFreq, ASampleRate);
  end;

  // Apply global modulation depth
  Result := Result * AFMSynth.ModulationDepth;

  //// DEBUG
  //if Random(5000) = 0 then
  //  WriteLn('DEBUG ProcessFM: Final result=', Result:0:6);
end;

// Update all FM operators
class procedure TSedaiFMProcessor.UpdateFMOperators(var AFMSynth: TFMSynthesis; ADeltaTime: Single);
var
  i: Integer;
begin
  for i := 0 to AFMSynth.OperatorCount - 1 do
  begin
    with AFMSynth.Operators[i] do
    begin
      // Update ADSR envelope
      TSedaiADSRProcessor.ProcessADSR(ADSR, ADeltaTime);

      // Update amplitude based on ADSR
      //Amplitude := Amplitude * ADSR.Level;
    end;
  end;

  // Check if any operator is still active
  AFMSynth.IsActive := False;
  for i := 0 to AFMSynth.OperatorCount - 1 do
  begin
    if TSedaiADSRProcessor.IsADSRActive(AFMSynth.Operators[i].ADSR) then
    begin
      AFMSynth.IsActive := True;
      Break;
    end;
  end;
end;

// Initialize FM synthesis
class procedure TSedaiFMProcessor.InitializeFM(var AFMSynth: TFMSynthesis; AAlgorithm: TFMAlgorithm);
var
  i: Integer;
begin
  FillChar(AFMSynth, SizeOf(TFMSynthesis), 0);
  AFMSynth.IsActive := True;
  AFMSynth.Algorithm := AAlgorithm;
  AFMSynth.ModulationDepth := 1.0;
  AFMSynth.CarrierIndex := 0;

  case AAlgorithm of
    fmSimple:   AFMSynth.OperatorCount := 2;
    fmStack:    AFMSynth.OperatorCount := 3;
    fmParallel: AFMSynth.OperatorCount := 4;
    fmFeedback: AFMSynth.OperatorCount := 2;
    fmComplex:  AFMSynth.OperatorCount := 4;
  end;

  // Initialize operators
  for i := 0 to AFMSynth.OperatorCount - 1 do
  begin
    with AFMSynth.Operators[i] do
    begin
      Frequency := 440.0;
      FreqRatio := 1.0;
      Phase := 0.0;
      Amplitude := 0.5;
      FeedbackLevel := 0.0;
      LastOutput := 0.0;
      ADSR := TSedaiADSRProcessor.CreateADSR(0.1, 0.1, 0.8, 0.5);
    end;
  end;
end;

// Setup simple 2-operator FM
class procedure TSedaiFMProcessor.SetupSimpleFM(var AFMSynth: TFMSynthesis;
  ACarrierRatio, AModulatorRatio: Single; AModDepth: Single);
begin
  InitializeFM(AFMSynth, fmSimple);

  // Operator 0: Carrier
  AFMSynth.Operators[0].FreqRatio := ACarrierRatio;
  AFMSynth.Operators[0].Amplitude := 1.0;
  AFMSynth.CarrierIndex := 0;

  // Operator 1: Modulator
  AFMSynth.Operators[1].FreqRatio := AModulatorRatio;
  AFMSynth.Operators[1].Amplitude := AModDepth;

  AFMSynth.ModulationDepth := 0.8;
end;

// Setup DX7-style algorithms
class procedure TSedaiFMProcessor.SetupDX7Style(var AFMSynth: TFMSynthesis; AAlgorithm: TFMAlgorithm);
begin
  InitializeFM(AFMSynth, AAlgorithm);

  case AAlgorithm of
    fmStack:
      begin
        // Op 2 -> Op 1 -> Op 0 (carrier)
        AFMSynth.Operators[0].FreqRatio := 1.0;   // Carrier
        AFMSynth.Operators[1].FreqRatio := 2.0;   // Modulator 1
        AFMSynth.Operators[2].FreqRatio := 3.0;   // Modulator 2
        AFMSynth.CarrierIndex := 0;
      end;

    fmParallel:
      begin
        // Op 1, 2, 3 -> Op 0 (carrier)
        AFMSynth.Operators[0].FreqRatio := 1.0;   // Carrier
        AFMSynth.Operators[1].FreqRatio := 1.41;  // Modulator 1
        AFMSynth.Operators[2].FreqRatio := 2.0;   // Modulator 2
        AFMSynth.Operators[3].FreqRatio := 4.0;   // Modulator 3
        AFMSynth.CarrierIndex := 0;
      end;
  end;
end;

// Operator control methods
class procedure TSedaiFMProcessor.SetOperatorADSR(var AFMSynth: TFMSynthesis; AOperatorIndex: Integer;
  AAttack, ADecay, ASustain, ARelease: Single);
begin
  if (AOperatorIndex >= 0) and (AOperatorIndex < AFMSynth.OperatorCount) then
  begin
    AFMSynth.Operators[AOperatorIndex].ADSR :=
      TSedaiADSRProcessor.CreateADSR(AAttack, ADecay, ASustain, ARelease);
  end;
end;

class procedure TSedaiFMProcessor.SetOperatorRatio(var AFMSynth: TFMSynthesis;
  AOperatorIndex: Integer; ARatio: Single);
begin
  if (AOperatorIndex >= 0) and (AOperatorIndex < AFMSynth.OperatorCount) then
    AFMSynth.Operators[AOperatorIndex].FreqRatio := ARatio;
end;

class procedure TSedaiFMProcessor.SetOperatorAmplitude(var AFMSynth: TFMSynthesis;
  AOperatorIndex: Integer; AAmplitude: Single);
begin
  if (AOperatorIndex >= 0) and (AOperatorIndex < AFMSynth.OperatorCount) then
    AFMSynth.Operators[AOperatorIndex].Amplitude := AAmplitude;
end;

class procedure TSedaiFMProcessor.SetOperatorFeedback(var AFMSynth: TFMSynthesis;
  AOperatorIndex: Integer; AFeedback: Single);
begin
  if (AOperatorIndex >= 0) and (AOperatorIndex < AFMSynth.OperatorCount) then
    AFMSynth.Operators[AOperatorIndex].FeedbackLevel := AFeedback;
end;

// ADSR control
class procedure TSedaiFMProcessor.StartFMAttack(var AFMSynth: TFMSynthesis);
var
  i: Integer;
begin
  for i := 0 to AFMSynth.OperatorCount - 1 do
    TSedaiADSRProcessor.StartADSRAttack(AFMSynth.Operators[i].ADSR);
  AFMSynth.IsActive := True;
end;

class procedure TSedaiFMProcessor.StartFMRelease(var AFMSynth: TFMSynthesis);
var
  i: Integer;
begin
  for i := 0 to AFMSynth.OperatorCount - 1 do
    TSedaiADSRProcessor.StartADSRRelease(AFMSynth.Operators[i].ADSR);
end;

class procedure TSedaiFMProcessor.ForceOperatorPhase(var AFMSynth: TFMSynthesis;
  AOperatorIndex: Integer; APhase: TADSRPhase);
begin
  if (AOperatorIndex >= 0) and (AOperatorIndex < AFMSynth.OperatorCount) then
    TSedaiADSRProcessor.ForceADSRPhase(AFMSynth.Operators[AOperatorIndex].ADSR, APhase);
end;

class function TSedaiFMProcessor.IsFMReleaseComplete(const AFMSynth: TFMSynthesis): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to AFMSynth.OperatorCount - 1 do
  begin
    if TSedaiADSRProcessor.IsADSRActive(AFMSynth.Operators[i].ADSR) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

// Algorithm implementations
class function TSedaiFMProcessor.ProcessSimpleFM(var AFMSynth: TFMSynthesis;
  ABaseFreq: Single; ASampleRate: Single): Single;
var
  ACarrierFreq, AModulatorFreq: Single;
  AModulatorOutput: Single;
  ACarrierPhase: Single;
begin
  with AFMSynth do
  begin
    //// DEBUG
    //if Random(3000) = 0 then
    //begin
    //  WriteLn('DEBUG SimpleFM: Op0 ADSR Level=', Operators[0].ADSR.Level:0:3, ' Phase=', Ord(Operators[0].ADSR.Phase));
    //  WriteLn('DEBUG SimpleFM: Op1 ADSR Level=', Operators[1].ADSR.Level:0:3, ' Phase=', Ord(Operators[1].ADSR.Phase));
    //  WriteLn('DEBUG SimpleFM: Op0 Amp=', Operators[0].Amplitude:0:3, ' Op1 Amp=', Operators[1].Amplitude:0:3);
    //end;

    // Calculate frequencies
    ACarrierFreq := ABaseFreq * Operators[0].FreqRatio;
    AModulatorFreq := ABaseFreq * Operators[1].FreqRatio;

    // Update modulator phase
    Operators[1].Phase := Operators[1].Phase + (AModulatorFreq * 2.0 * Pi / ASampleRate);
    if Operators[1].Phase >= 2.0 * Pi then
      Operators[1].Phase := Operators[1].Phase - 2.0 * Pi;

    // Generate modulator output
    AModulatorOutput := Sin(Operators[1].Phase) * Operators[1].Amplitude * Operators[1].ADSR.Level;

    // Modulate carrier frequency
    ACarrierPhase := Operators[0].Phase + AModulatorOutput;

    // Update carrier phase
    Operators[0].Phase := Operators[0].Phase + (ACarrierFreq * 2.0 * Pi / ASampleRate);
    if Operators[0].Phase >= 2.0 * Pi then
      Operators[0].Phase := Operators[0].Phase - 2.0 * Pi;

    // Generate final output
    Result := Sin(ACarrierPhase) * Operators[0].Amplitude * Operators[0].ADSR.Level;
  end;
end;

class function TSedaiFMProcessor.ProcessStackFM(var AFMSynth: TFMSynthesis;
  ABaseFreq: Single; ASampleRate: Single): Single;
var
  i: Integer;
  AFreq: Single;
  AOutput: Single;
  AModulation: Single;
begin
  AModulation := 0.0;

  with AFMSynth do
  begin
    // Process operators in reverse order (modulators first)
    for i := OperatorCount - 1 downto 1 do
    begin
      AFreq := ABaseFreq * Operators[i].FreqRatio;

      // Update phase
      Operators[i].Phase := Operators[i].Phase + (AFreq * 2.0 * Pi / ASampleRate);
      if Operators[i].Phase >= 2.0 * Pi then
        Operators[i].Phase := Operators[i].Phase - 2.0 * Pi;

      // Generate modulator output
      AOutput := Sin(Operators[i].Phase + AModulation) * Operators[i].Amplitude * Operators[i].ADSR.Level;
      AModulation := AModulation + AOutput;
    end;

    // Process carrier with accumulated modulation
    AFreq := ABaseFreq * Operators[0].FreqRatio;
    Operators[0].Phase := Operators[0].Phase + (AFreq * 2.0 * Pi / ASampleRate);
    if Operators[0].Phase >= 2.0 * Pi then
      Operators[0].Phase := Operators[0].Phase - 2.0 * Pi;

    Result := Sin(Operators[0].Phase + AModulation) * Operators[0].Amplitude * Operators[0].ADSR.Level;
  end;
end;

class function TSedaiFMProcessor.ProcessParallelFM(var AFMSynth: TFMSynthesis;
  ABaseFreq: Single; ASampleRate: Single): Single;
var
  i: Integer;
  AFreq: Single;
  AModulation: Single;
  ACarrierPhase: Single;
begin
  AModulation := 0.0;

  with AFMSynth do
  begin
    // Process all modulators in parallel
    for i := 1 to OperatorCount - 1 do
    begin
      AFreq := ABaseFreq * Operators[i].FreqRatio;

      // Update phase
      Operators[i].Phase := Operators[i].Phase + (AFreq * 2.0 * Pi / ASampleRate);
      if Operators[i].Phase >= 2.0 * Pi then
        Operators[i].Phase := Operators[i].Phase - 2.0 * Pi;

      // Add modulator output
      AModulation := AModulation + Sin(Operators[i].Phase) * Operators[i].Amplitude * Operators[i].ADSR.Level;
    end;

    // Process carrier with parallel modulation
    AFreq := ABaseFreq * Operators[0].FreqRatio;
    Operators[0].Phase := Operators[0].Phase + (AFreq * 2.0 * Pi / ASampleRate);
    if Operators[0].Phase >= 2.0 * Pi then
      Operators[0].Phase := Operators[0].Phase - 2.0 * Pi;

    ACarrierPhase := Operators[0].Phase + AModulation;
    Result := Sin(ACarrierPhase) * Operators[0].Amplitude * Operators[0].ADSR.Level;
  end;
end;

class function TSedaiFMProcessor.ProcessFeedbackFM(var AFMSynth: TFMSynthesis;
  ABaseFreq: Single; ASampleRate: Single): Single;
var
  ACarrierFreq, AModulatorFreq: Single;
  AModulatorOutput: Single;
  AFeedbackMod: Single;
begin
  with AFMSynth do
  begin
    // Calculate frequencies
    ACarrierFreq := ABaseFreq * Operators[0].FreqRatio;
    AModulatorFreq := ABaseFreq * Operators[1].FreqRatio;

    // Apply feedback to modulator
    AFeedbackMod := Operators[1].LastOutput * Operators[1].FeedbackLevel;

    // Update modulator phase
    Operators[1].Phase := Operators[1].Phase + (AModulatorFreq * 2.0 * Pi / ASampleRate);
    if Operators[1].Phase >= 2.0 * Pi then
      Operators[1].Phase := Operators[1].Phase - 2.0 * Pi;

    // Generate modulator output with feedback
    AModulatorOutput := Sin(Operators[1].Phase + AFeedbackMod) * Operators[1].Amplitude * Operators[1].ADSR.Level;
    Operators[1].LastOutput := AModulatorOutput;

    // Update carrier phase
    Operators[0].Phase := Operators[0].Phase + (ACarrierFreq * 2.0 * Pi / ASampleRate);
    if Operators[0].Phase >= 2.0 * Pi then
      Operators[0].Phase := Operators[0].Phase - 2.0 * Pi;

    // Generate final output
    Result := Sin(Operators[0].Phase + AModulatorOutput) * Operators[0].Amplitude * Operators[0].ADSR.Level;
  end;
end;

class function TSedaiFMProcessor.ProcessComplexFM(var AFMSynth: TFMSynthesis;
  ABaseFreq: Single; ASampleRate: Single): Single;
var
  i: Integer;
  AFreq: Single;
  AOutput: array[0..3] of Single;
  AFinalOutput: Single;
begin
  with AFMSynth do
  begin
    // Generate all operator outputs
    for i := 0 to OperatorCount - 1 do
    begin
      AFreq := ABaseFreq * Operators[i].FreqRatio;

      // Update phase
      Operators[i].Phase := Operators[i].Phase + (AFreq * 2.0 * Pi / ASampleRate);
      if Operators[i].Phase >= 2.0 * Pi then
        Operators[i].Phase := Operators[i].Phase - 2.0 * Pi;

      // Generate basic output
      AOutput[i] := Sin(Operators[i].Phase) * Operators[i].Amplitude * Operators[i].ADSR.Level;
    end;

    // Complex algorithm: Op3->Op2, Op2->Op1, Op1+Op0 as carriers
    AOutput[2] := Sin(Operators[2].Phase + AOutput[3]);
    AOutput[1] := Sin(Operators[1].Phase + AOutput[2]);

    AFinalOutput := AOutput[1] + AOutput[0];
    Result := AFinalOutput * 0.5; // Normalize
  end;
end;

// Preset creation functions
class function TSedaiFMProcessor.CreateElectricPiano: TFMSynthesis;
begin
  InitializeFM(Result, fmSimple);
  SetupSimpleFM(Result, 1.0, 14.0, 2.5);

  // Carrier ADSR
  SetOperatorADSR(Result, 0, 0.01, 0.8, 0.0, 2.0);
  // Modulator ADSR
  SetOperatorADSR(Result, 1, 0.01, 0.3, 0.0, 1.0);

  Result.ModulationDepth := 0.9;

  //// DEBUG: Verifica i valori
  //WriteLn('DEBUG EPlano: Op0 A=', Result.Operators[0].ADSR.Attack:0:3, ' D=', Result.Operators[0].ADSR.Decay:0:3, ' S=', Result.Operators[0].ADSR.Sustain:0:3, ' R=', Result.Operators[0].ADSR.Release:0:3);
  //WriteLn('DEBUG EPlano: Op1 A=', Result.Operators[1].ADSR.Attack:0:3, ' D=', Result.Operators[1].ADSR.Decay:0:3, ' S=', Result.Operators[1].ADSR.Sustain:0:3, ' R=', Result.Operators[1].ADSR.Release:0:3);
end;

class function TSedaiFMProcessor.CreateBrass: TFMSynthesis;
begin
  InitializeFM(Result, fmFeedback);
  SetupSimpleFM(Result, 1.0, 1.0, 1.8);

  // Add feedback for brass character
  SetOperatorFeedback(Result, 1, 0.3);

  // Brass ADSR
  SetOperatorADSR(Result, 0, 0.1, 0.2, 0.7, 0.8);
  SetOperatorADSR(Result, 1, 0.1, 0.1, 0.8, 0.5);

  Result.ModulationDepth := 1.2;
end;

class function TSedaiFMProcessor.CreateBell: TFMSynthesis;
begin
  InitializeFM(Result, fmParallel);

  // Bell harmonics
  SetOperatorRatio(Result, 0, 1.0);   // Fundamental
  SetOperatorRatio(Result, 1, 2.76);  // Bell partial
  SetOperatorRatio(Result, 2, 5.4);   // Bell partial
  SetOperatorRatio(Result, 3, 8.93);  // Bell partial

  // Fast attack, long decay
  SetOperatorADSR(Result, 0, 0.01, 3.0, 0.0, 4.0);
  SetOperatorADSR(Result, 1, 0.01, 2.0, 0.0, 3.0);
  SetOperatorADSR(Result, 2, 0.01, 1.5, 0.0, 2.0);
  SetOperatorADSR(Result, 3, 0.01, 1.0, 0.0, 1.5);

  Result.ModulationDepth := 0.8;
end;

class function TSedaiFMProcessor.CreateOrgan: TFMSynthesis;
begin
  InitializeFM(Result, fmStack);

  // Organ-like harmonics
  SetOperatorRatio(Result, 0, 1.0);   // Fundamental
  SetOperatorRatio(Result, 1, 2.0);   // Octave
  SetOperatorRatio(Result, 2, 3.0);   // Fifth

  // Organ ADSR - sustained
  SetOperatorADSR(Result, 0, 0.3, 0.1, 0.9, 1.0);
  SetOperatorADSR(Result, 1, 0.2, 0.1, 0.8, 0.8);
  SetOperatorADSR(Result, 2, 0.1, 0.1, 0.7, 0.6);

  Result.ModulationDepth := 0.7;
end;

class function TSedaiFMProcessor.CreateSynthLead: TFMSynthesis;
begin
  InitializeFM(Result, fmFeedback);
  SetupSimpleFM(Result, 1.0, 3.5, 4.0); // Aggressive modulation

  // Add strong feedback
  SetOperatorFeedback(Result, 1, 0.6);

  // Synth lead ADSR
  SetOperatorADSR(Result, 0, 0.05, 0.3, 0.6, 0.5);
  SetOperatorADSR(Result, 1, 0.01, 0.2, 0.8, 0.3);

  Result.ModulationDepth := 1.5;
end;

class function TSedaiFMProcessor.CreateSynthBass: TFMSynthesis;
begin
  InitializeFM(Result, fmSimple);
  SetupSimpleFM(Result, 1.0, 0.5, 1.2); // Sub-harmonic modulation

  // Bass ADSR
  SetOperatorADSR(Result, 0, 0.01, 0.1, 0.8, 0.3);
  SetOperatorADSR(Result, 1, 0.01, 0.05, 0.9, 0.2);

  Result.ModulationDepth := 1.0;
end;

// NEW FM PRESETS

// Choir - warm, vocal-like FM timbre
class function TSedaiFMProcessor.CreateChoir: TFMSynthesis;
begin
  InitializeFM(Result, fmStack);

  // Stack multiple harmonics for voice-like sound
  SetOperatorRatio(Result, 0, 1.0);   // Fundamental
  SetOperatorRatio(Result, 1, 2.997); // Slightly detuned octave
  SetOperatorRatio(Result, 2, 4.01);  // Slightly detuned 5th
  SetOperatorRatio(Result, 3, 5.99);  // Formant-like partial

  // Slow attack, sustained
  SetOperatorADSR(Result, 0, 0.5, 0.3, 0.8, 1.2);
  SetOperatorADSR(Result, 1, 0.5, 0.3, 0.75, 1.1);
  SetOperatorADSR(Result, 2, 0.5, 0.4, 0.7, 1.0);
  SetOperatorADSR(Result, 3, 0.5, 0.5, 0.65, 0.9);

  Result.ModulationDepth := 0.8;
end;

// FM Strings - rich, sustained FM string ensemble
class function TSedaiFMProcessor.CreateFMStrings: TFMSynthesis;
begin
  InitializeFM(Result, fmParallel);

  // Multiple detuned operators for richness
  SetOperatorRatio(Result, 0, 1.0);
  SetOperatorRatio(Result, 1, 1.003);  // Slight detune
  SetOperatorRatio(Result, 2, 0.997);  // Slight detune opposite
  SetOperatorRatio(Result, 3, 2.01);   // Octave with detune

  SetOperatorAmplitude(Result, 0, 0.35);
  SetOperatorAmplitude(Result, 1, 0.35);
  SetOperatorAmplitude(Result, 2, 0.35);
  SetOperatorAmplitude(Result, 3, 0.25);

  // Slow attack like strings
  SetOperatorADSR(Result, 0, 0.7, 0.3, 0.85, 1.3);
  SetOperatorADSR(Result, 1, 0.7, 0.3, 0.85, 1.3);
  SetOperatorADSR(Result, 2, 0.7, 0.3, 0.85, 1.3);
  SetOperatorADSR(Result, 3, 0.7, 0.3, 0.8, 1.2);

  Result.ModulationDepth := 0.6;
end;

// FM Pad - atmospheric, evolving pad sound
class function TSedaiFMProcessor.CreateFMPad: TFMSynthesis;
begin
  InitializeFM(Result, fmComplex);

  // Complex modulation for evolving timbre
  SetOperatorRatio(Result, 0, 1.0);
  SetOperatorRatio(Result, 1, 1.5);   // Fifth
  SetOperatorRatio(Result, 2, 2.003); // Octave slightly sharp
  SetOperatorRatio(Result, 3, 3.99);  // Harmonic slightly flat

  // Very slow attack, high sustain
  SetOperatorADSR(Result, 0, 1.2, 0.5, 0.9, 2.0);
  SetOperatorADSR(Result, 1, 1.0, 0.4, 0.85, 1.8);
  SetOperatorADSR(Result, 2, 1.3, 0.6, 0.8, 1.9);
  SetOperatorADSR(Result, 3, 1.1, 0.5, 0.75, 1.7);

  Result.ModulationDepth := 1.2;
end;

// Marimba - percussive, wooden mallet sound
class function TSedaiFMProcessor.CreateMarimba: TFMSynthesis;
begin
  InitializeFM(Result, fmSimple);

  // High modulator ratio for bright attack
  SetupSimpleFM(Result, 1.0, 3.5, 2.0);

  // Fast attack, moderate decay, no sustain
  SetOperatorADSR(Result, 0, 0.001, 0.5, 0.0, 0.8);
  SetOperatorADSR(Result, 1, 0.001, 0.3, 0.0, 0.5);

  Result.ModulationDepth := 1.5;
end;

// Flute - breathy, airy wind instrument
class function TSedaiFMProcessor.CreateFlute: TFMSynthesis;
begin
  InitializeFM(Result, fmSimple);

  // Simple ratio for pure flute tone
  SetupSimpleFM(Result, 1.0, 1.0, 0.4);

  // Medium attack, sustained
  SetOperatorADSR(Result, 0, 0.08, 0.1, 0.7, 0.4);
  SetOperatorADSR(Result, 1, 0.08, 0.1, 0.6, 0.3);

  SetOperatorAmplitude(Result, 0, 0.8);
  SetOperatorAmplitude(Result, 1, 0.6);

  Result.ModulationDepth := 0.5; // Light modulation for breathy quality
end;

// Church Bell - large resonant bell with inharmonic partials
class function TSedaiFMProcessor.CreateChurchBell: TFMSynthesis;
begin
  InitializeFM(Result, fmParallel);

  // Inharmonic bell partials (not exact multiples)
  SetOperatorRatio(Result, 0, 0.56);   // Hum tone
  SetOperatorRatio(Result, 1, 1.0);    // Fundamental
  SetOperatorRatio(Result, 2, 2.41);   // Minor third
  SetOperatorRatio(Result, 3, 3.76);   // Perfect fifth

  // Very fast attack, very long decay
  SetOperatorADSR(Result, 0, 0.001, 5.0, 0.0, 8.0);
  SetOperatorADSR(Result, 1, 0.001, 4.0, 0.0, 7.0);
  SetOperatorADSR(Result, 2, 0.001, 3.5, 0.0, 6.0);
  SetOperatorADSR(Result, 3, 0.001, 3.0, 0.0, 5.0);

  SetOperatorAmplitude(Result, 0, 0.3);
  SetOperatorAmplitude(Result, 1, 0.4);
  SetOperatorAmplitude(Result, 2, 0.3);
  SetOperatorAmplitude(Result, 3, 0.25);

  Result.ModulationDepth := 0.7;
end;

// Utility functions
class function TSedaiFMProcessor.IsFMActive(const AFMSynth: TFMSynthesis): Boolean;
begin
  Result := AFMSynth.IsActive;
end;

class procedure TSedaiFMProcessor.ResetFM(var AFMSynth: TFMSynthesis);
var
  i: Integer;
begin
  for i := 0 to AFMSynth.OperatorCount - 1 do
  begin
    AFMSynth.Operators[i].Phase := 0.0;
    AFMSynth.Operators[i].LastOutput := 0.0;
    ResetADSR(AFMSynth.Operators[i].ADSR);  // <-- CORREZIONE: senza TSedaiADSRProcessor
  end;
  AFMSynth.IsActive := False;
end;

class function TSedaiFMProcessor.GetFMInfo(const AFMSynth: TFMSynthesis): string;
var
  AAlgorithmName: string;
begin
  case AFMSynth.Algorithm of
    fmSimple: AAlgorithmName := 'Simple FM';
    fmStack: AAlgorithmName := 'Stack FM';
    fmParallel: AAlgorithmName := 'Parallel FM';
    fmFeedback: AAlgorithmName := 'Feedback FM';
    fmComplex: AAlgorithmName := 'Complex FM';
  end;

  Result := Format('FM: %s, %d operators, depth=%.2f, active=%s',
    [AAlgorithmName, AFMSynth.OperatorCount, AFMSynth.ModulationDepth,
     BoolToStr(AFMSynth.IsActive, True)]);
end;

end.
