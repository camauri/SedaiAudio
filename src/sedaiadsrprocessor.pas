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

unit SedaiADSRProcessor;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes;

type
  // ADSR curve types
  TADSRCurve = (acLinear, acExponential, acLogarithmic, acSCurve);

  TSedaiADSRProcessor = class
  public
    // Core ADSR processing
    class procedure ProcessADSR(var AADSR: TADSR; ADeltaTime: Single; ACurve: TADSRCurve = acExponential);
    class procedure StartADSRAttack(var AADSR: TADSR);
    class procedure StartADSRRelease(var AADSR: TADSR);
    class procedure ForceADSRPhase(var AADSR: TADSR; APhase: TADSRPhase);

    // ADSR state queries
    class function IsADSRActive(const AADSR: TADSR): Boolean;
    class function IsADSRInSustain(const AADSR: TADSR): Boolean;
    class function IsADSRReleasing(const AADSR: TADSR): Boolean;
    class function GetADSRProgress(const AADSR: TADSR): Single; // 0.0 to 1.0

    // ADSR creation and manipulation
    class function CreateADSR(AAttack, ADecay, ASustain, ARelease: Single): TADSR;
    class function CreateOrganADSR: TADSR;    // Slow attack, long sustain
    class function CreatePianoADSR: TADSR;    // Fast attack, long release
    class function CreateStringADSR: TADSR;   // Medium attack, sustain
    class function CreatePercussionADSR: TADSR; // Very fast attack, no sustain

    // ADSR validation and utilities
    class function ValidateADSR(const AADSR: TADSR): Boolean;
    class procedure ClampADSRTimes(var AADSR: TADSR; AMinTime: Single = 0.001; AMaxTime: Single = 10.0);
    class function ADSRToString(const AADSR: TADSR): string;

    // Advanced ADSR features
    class procedure SetADSRVelocity(var AADSR: TADSR; AVelocity: Single); // 0.0 to 1.0
    class procedure ModulateADSR(var AADSR: TADSR; AAttackMod, ADecayMod, ASustainMod, AReleaseMod: Single);

    // Curve calculation functions
    class function CalculateLinearCurve(AProgress: Single): Single;
    class function CalculateExponentialCurve(AProgress: Single; AExponent: Single = 3.0): Single;
    class function CalculateLogarithmicCurve(AProgress: Single): Single;
    class function CalculateSCurve(AProgress: Single): Single;
  end;

implementation

// Main ADSR processing with different curve types
class procedure TSedaiADSRProcessor.ProcessADSR(var AADSR: TADSR; ADeltaTime: Single; ACurve: TADSRCurve);
const
  // Minimum attack/release time to prevent audio clicks (5ms)
  MIN_ATTACK_TIME = 0.005;
  MIN_RELEASE_TIME = 0.005;
var
  AProgress: Single;
  ACurveValue: Single;
  AEffectiveAttack: Single;
  AEffectiveRelease: Single;
begin
  case AADSR.Phase of
    apAttack:
      begin
        AADSR.Timer := AADSR.Timer + ADeltaTime;
        // Enforce minimum attack time to prevent clicks
        AEffectiveAttack := AADSR.Attack;
        if AEffectiveAttack < MIN_ATTACK_TIME then
          AEffectiveAttack := MIN_ATTACK_TIME;

        AProgress := AADSR.Timer / AEffectiveAttack;
        if AProgress >= 1.0 then
        begin
          AADSR.Level := 1.0;
          AADSR.Phase := apDecay;
          AADSR.Timer := 0;
        end
        else
        begin
          // Apply curve to attack
          case ACurve of
            acLinear: ACurveValue := CalculateLinearCurve(AProgress);
            acExponential: ACurveValue := CalculateExponentialCurve(AProgress, 2.0);
            acLogarithmic: ACurveValue := CalculateLogarithmicCurve(AProgress);
            acSCurve: ACurveValue := CalculateSCurve(AProgress);
          else
            ACurveValue := AProgress;
          end;
          // Always update level - the curve handles smooth start
          AADSR.Level := ACurveValue;
        end;
      end;

    apDecay:
      begin
        AADSR.Timer := AADSR.Timer + ADeltaTime;
        if AADSR.Decay > 0 then
        begin
          AProgress := AADSR.Timer / AADSR.Decay;
          if AProgress >= 1.0 then
          begin
            AADSR.Level := AADSR.Sustain;
            AADSR.Phase := apSustain;
          end
          else
          begin
            // Apply curve to decay (inverted)
            case ACurve of
              acLinear: ACurveValue := CalculateLinearCurve(1.0 - AProgress);
              acExponential: ACurveValue := CalculateExponentialCurve(1.0 - AProgress, 3.0);
              acLogarithmic: ACurveValue := CalculateLogarithmicCurve(1.0 - AProgress);
              acSCurve: ACurveValue := CalculateSCurve(1.0 - AProgress);
            else
              ACurveValue := 1.0 - AProgress;
            end;
            AADSR.Level := AADSR.Sustain + ((1.0 - AADSR.Sustain) * ACurveValue);
          end;
        end
        else
        begin
          AADSR.Level := AADSR.Sustain;
          AADSR.Phase := apSustain;
        end;
      end;

    apSustain:
      begin
        AADSR.Level := AADSR.Sustain;
        // Stay in sustain until note off
      end;

    apRelease:
      begin
        AADSR.Timer := AADSR.Timer + ADeltaTime;
        // Enforce minimum release time to prevent clicks
        AEffectiveRelease := AADSR.Release;
        if AEffectiveRelease < MIN_RELEASE_TIME then
          AEffectiveRelease := MIN_RELEASE_TIME;

        AProgress := AADSR.Timer / AEffectiveRelease;
        if AProgress >= 1.0 then
        begin
          AADSR.Level := 0.0;
          AADSR.Phase := apIdle;
        end
        else
        begin
          // Apply curve to release (inverted)
          case ACurve of
            acLinear: ACurveValue := CalculateLinearCurve(1.0 - AProgress);
            acExponential: ACurveValue := CalculateExponentialCurve(1.0 - AProgress, 4.0);
            acLogarithmic: ACurveValue := CalculateLogarithmicCurve(1.0 - AProgress);
            acSCurve: ACurveValue := CalculateSCurve(1.0 - AProgress);
          else
            ACurveValue := 1.0 - AProgress;
          end;
          // FIX: Use ReleaseStartLevel instead of Sustain for correct release curve (anti-click)
          AADSR.Level := AADSR.ReleaseStartLevel * ACurveValue;
        end;
      end;

    apIdle:
      AADSR.Level := 0.0;
  end;
end;

// Start ADSR from attack phase
class procedure TSedaiADSRProcessor.StartADSRAttack(var AADSR: TADSR);
begin
  AADSR.Phase := apAttack;
  AADSR.Timer := 0.0;
  // FIX: Don't reset to 0 if there's still audio from previous note (anti-click)
  // Only reset if level is already very low
  if AADSR.Level < 0.001 then
    AADSR.Level := 0.0;
  // Otherwise keep current level and attack from there
end;

// Trigger release phase
class procedure TSedaiADSRProcessor.StartADSRRelease(var AADSR: TADSR);
begin
  if AADSR.Phase <> apIdle then
  begin
    // FIX: Store the current level to use as release starting point (anti-click)
    AADSR.ReleaseStartLevel := AADSR.Level;
    AADSR.Phase := apRelease;
    AADSR.Timer := 0.0;
    // Level is already at the correct value
  end;
end;

// Force ADSR to specific phase
class procedure TSedaiADSRProcessor.ForceADSRPhase(var AADSR: TADSR; APhase: TADSRPhase);
begin
  AADSR.Phase := APhase;
  AADSR.Timer := 0.0;
  case APhase of
    apIdle:
      begin
        AADSR.Level := 0.0;
        AADSR.ReleaseStartLevel := 0.0;
      end;
    apAttack:
      begin
        AADSR.Level := 0.0;
        AADSR.ReleaseStartLevel := 0.0;
      end;
    apDecay:
      begin
        AADSR.Level := 1.0;
        AADSR.ReleaseStartLevel := 0.0;
      end;
    apSustain:
      begin
        AADSR.Level := AADSR.Sustain;
        AADSR.ReleaseStartLevel := 0.0;
      end;
    apRelease:
      begin
        // Keep current level
        AADSR.ReleaseStartLevel := AADSR.Level;
      end;
  end;
end;

// Check if ADSR is active (not idle)
class function TSedaiADSRProcessor.IsADSRActive(const AADSR: TADSR): Boolean;
begin
  Result := AADSR.Phase <> apIdle;
end;

// Check if ADSR is in sustain phase
class function TSedaiADSRProcessor.IsADSRInSustain(const AADSR: TADSR): Boolean;
begin
  Result := AADSR.Phase = apSustain;
end;

// Check if ADSR is releasing
class function TSedaiADSRProcessor.IsADSRReleasing(const AADSR: TADSR): Boolean;
begin
  Result := AADSR.Phase = apRelease;
end;

// Get ADSR progress (0.0 to 1.0)
class function TSedaiADSRProcessor.GetADSRProgress(const AADSR: TADSR): Single;
var
  ATotalTime: Single;
  ACurrentTime: Single;
begin
  case AADSR.Phase of
    apIdle: Result := 0.0;
    apAttack:
      if AADSR.Attack > 0 then
        Result := AADSR.Timer / AADSR.Attack
      else
        Result := 1.0;
    apDecay:
      if AADSR.Decay > 0 then
        Result := AADSR.Timer / AADSR.Decay
      else
        Result := 1.0;
    apSustain: Result := 1.0;
    apRelease:
      if AADSR.Release > 0 then
        Result := AADSR.Timer / AADSR.Release
      else
        Result := 1.0;
  else
    Result := 0.0;
  end;
end;

// Create ADSR with specified parameters
class function TSedaiADSRProcessor.CreateADSR(AAttack, ADecay, ASustain, ARelease: Single): TADSR;
begin
  Result.Attack := AAttack;
  Result.Decay := ADecay;
  Result.Sustain := ASustain;
  Result.Release := ARelease;
  Result.Phase := apIdle;
  Result.Level := 0.0;
  Result.Timer := 0.0;
  Result.ReleaseStartLevel := 0.0; // Initialize new field
end;

// Preset ADSR configurations
class function TSedaiADSRProcessor.CreateOrganADSR: TADSR;
begin
  Result := CreateADSR(0.5, 0.2, 0.8, 1.0); // Slow attack, long sustain
end;

class function TSedaiADSRProcessor.CreatePianoADSR: TADSR;
begin
  Result := CreateADSR(0.01, 0.3, 0.3, 2.0); // Fast attack, long release
end;

class function TSedaiADSRProcessor.CreateStringADSR: TADSR;
begin
  Result := CreateADSR(0.2, 0.1, 0.9, 0.8); // Medium attack, high sustain
end;

class function TSedaiADSRProcessor.CreatePercussionADSR: TADSR;
begin
  Result := CreateADSR(0.001, 0.05, 0.0, 0.2); // Very fast, no sustain
end;

// Validate ADSR parameters
class function TSedaiADSRProcessor.ValidateADSR(const AADSR: TADSR): Boolean;
begin
  Result := (AADSR.Attack >= 0.0) and
            (AADSR.Decay >= 0.0) and
            (AADSR.Sustain >= 0.0) and (AADSR.Sustain <= 1.0) and
            (AADSR.Release >= 0.0);
end;

// Clamp ADSR times to reasonable ranges
class procedure TSedaiADSRProcessor.ClampADSRTimes(var AADSR: TADSR; AMinTime: Single; AMaxTime: Single);
begin
  if AADSR.Attack < AMinTime then AADSR.Attack := AMinTime;
  if AADSR.Attack > AMaxTime then AADSR.Attack := AMaxTime;

  if AADSR.Decay < AMinTime then AADSR.Decay := AMinTime;
  if AADSR.Decay > AMaxTime then AADSR.Decay := AMaxTime;

  if AADSR.Release < AMinTime then AADSR.Release := AMinTime;
  if AADSR.Release > AMaxTime then AADSR.Release := AMaxTime;

  if AADSR.Sustain < 0.0 then AADSR.Sustain := 0.0;
  if AADSR.Sustain > 1.0 then AADSR.Sustain := 1.0;
end;

// Convert ADSR to string representation
class function TSedaiADSRProcessor.ADSRToString(const AADSR: TADSR): string;
var
  APhaseStr: string;
begin
  // Convert phase to string directly
  case AADSR.Phase of
    apIdle: APhaseStr := 'Idle';
    apAttack: APhaseStr := 'Attack';
    apDecay: APhaseStr := 'Decay';
    apSustain: APhaseStr := 'Sustain';
    apRelease: APhaseStr := 'Release';
  else
    APhaseStr := 'Unknown';
  end;

  Result := Format('ADSR: A=%.3f D=%.3f S=%.3f R=%.3f [%s L=%.3f]',
    [AADSR.Attack, AADSR.Decay, AADSR.Sustain, AADSR.Release,
     APhaseStr, AADSR.Level]);
end;

// Apply velocity to ADSR (affects attack and sustain)
class procedure TSedaiADSRProcessor.SetADSRVelocity(var AADSR: TADSR; AVelocity: Single);
var
  AClampedVelocity: Single;
begin
  AClampedVelocity := AVelocity;
  if AClampedVelocity < 0.0 then AClampedVelocity := 0.0;
  if AClampedVelocity > 1.0 then AClampedVelocity := 1.0;

  // Higher velocity = faster attack, higher sustain
  AADSR.Attack := AADSR.Attack * (2.0 - AClampedVelocity); // Faster with high velocity
  AADSR.Sustain := AADSR.Sustain * AClampedVelocity;       // Louder with high velocity
end;

// Modulate ADSR parameters
class procedure TSedaiADSRProcessor.ModulateADSR(var AADSR: TADSR; AAttackMod, ADecayMod, ASustainMod, AReleaseMod: Single);
begin
  AADSR.Attack := AADSR.Attack * AAttackMod;
  AADSR.Decay := AADSR.Decay * ADecayMod;
  AADSR.Sustain := AADSR.Sustain * ASustainMod;
  AADSR.Release := AADSR.Release * AReleaseMod;

  // Clamp to safe ranges
  ClampADSRTimes(AADSR);
end;

// Curve calculation functions
class function TSedaiADSRProcessor.CalculateLinearCurve(AProgress: Single): Single;
begin
  Result := AProgress;
end;

class function TSedaiADSRProcessor.CalculateExponentialCurve(AProgress: Single; AExponent: Single): Single;
begin
  Result := Power(AProgress, AExponent);
end;

class function TSedaiADSRProcessor.CalculateLogarithmicCurve(AProgress: Single): Single;
begin
  if AProgress <= 0.0 then
    Result := 0.0
  else if AProgress >= 1.0 then
    Result := 1.0
  else
    Result := Ln(1.0 + AProgress * 9.0) / Ln(10.0); // Log base 10 of (1 + progress*9)
end;

class function TSedaiADSRProcessor.CalculateSCurve(AProgress: Single): Single;
begin
  // S-curve using smoothstep function: 3x² - 2x³
  Result := AProgress * AProgress * (3.0 - 2.0 * AProgress);
end;

end.
