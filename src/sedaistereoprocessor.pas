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

unit SedaiStereoProcessor;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes;

type
  // Stereo processing algorithms
  TPanLaw = (plLinear, plConstantPower, plSineCosine, pl3dB, pl6dB);

  TSedaiStereoProcessor = class
  public
    // Core panning functions
    class procedure CalculatePan(APan: TPanPosition; AMonoSample: Single;
                                out ALeft, ARight: Single; APanLaw: TPanLaw = plConstantPower);

    // Advanced stereo processing
    class procedure ApplyStereoWidth(var ALeft, ARight: Single; AWidth: Single);
    class procedure ApplyStereoBalance(var ALeft, ARight: Single; ABalance: Single);
    class procedure SwapChannels(var ALeft, ARight: Single);

    // Mixing functions
    class function MixStereoSamples(const ASamples: array of TStereoSample): TStereoSample;
    class function MixStereoSamplesWithClipping(const ASamples: array of TStereoSample): TStereoSample;
    class procedure AddStereoSample(var ATarget: TStereoSample; const ASource: TStereoSample);

    // Conversion functions
    class function MonoToStereo(AMono: Single; APan: TPanPosition = 0.0): TStereoSample;
    class function StereoToMono(const AStereo: TStereoSample; AMethod: Integer = 0): Single;

    // Utility functions
    class function CreateStereoSample(ALeft, ARight: Single): TStereoSample;
    class function ClipStereoSample(const ASample: TStereoSample): TStereoSample;
    class function GetStereoSamplePeak(const ASample: TStereoSample): Single;
    class function IsStereoSampleSilent(const ASample: TStereoSample; AThreshold: Single = 0.001): Boolean;

    // Analysis functions
    class function CalculatePhaseCorrelation(const ALeft, ARight: array of Single): Single;
    class function CalculateStereoWidth(const ALeft, ARight: array of Single): Single;
    class function DetectMonoCompatibility(const AStereo: TStereoSample): Boolean;

    // Special effects
    class procedure ApplyPseudoStereo(AMono: Single; out ALeft, ARight: Single; ADelay: Single = 0.02);
    class procedure ApplyHaasEffect(var ALeft, ARight: Single; ADelayMs: Single);
  end;

implementation

// Core panning calculation with different pan laws
class procedure TSedaiStereoProcessor.CalculatePan(APan: TPanPosition; AMonoSample: Single;
  out ALeft, ARight: Single; APanLaw: TPanLaw);
var
  AClampedPan: TPanPosition;
  ALeftGain, ARightGain: Single;
  APanAngle: Single;
begin
  AClampedPan := ClampPan(APan);

  case APanLaw of
    plLinear:
      begin
        // Simple linear panning
        ALeftGain := (1.0 - AClampedPan) * 0.5;
        ARightGain := (1.0 + AClampedPan) * 0.5;
      end;

    plConstantPower:
      begin
        // Constant power panning (most common)
        ALeftGain := Sqrt((1.0 - AClampedPan) * 0.5);
        ARightGain := Sqrt((1.0 + AClampedPan) * 0.5);
      end;

    plSineCosine:
      begin
        // Sine/Cosine panning (smoothest)
        APanAngle := (AClampedPan + 1.0) * Pi * 0.25; // 0 to Pi/2
        ALeftGain := Cos(APanAngle);
        ARightGain := Sin(APanAngle);
      end;

    pl3dB:
      begin
        // -3dB center panning
        ALeftGain := (1.0 - AClampedPan) * 0.7071; // sqrt(0.5)
        ARightGain := (1.0 + AClampedPan) * 0.7071;
      end;

    pl6dB:
      begin
        // -6dB center panning (linear)
        ALeftGain := (1.0 - AClampedPan) * 0.5;
        ARightGain := (1.0 + AClampedPan) * 0.5;
      end;

  else
    // Default to constant power
    ALeftGain := Sqrt((1.0 - AClampedPan) * 0.5);
    ARightGain := Sqrt((1.0 + AClampedPan) * 0.5);
  end;

  ALeft := AMonoSample * ALeftGain;
  ARight := AMonoSample * ARightGain;
end;

// Stereo width control (0.0 = mono, 1.0 = normal, >1.0 = enhanced)
class procedure TSedaiStereoProcessor.ApplyStereoWidth(var ALeft, ARight: Single; AWidth: Single);
var
  AMid, ASide: Single;
begin
  // Convert to Mid/Side
  AMid := (ALeft + ARight) * 0.5;
  ASide := (ALeft - ARight) * 0.5;

  // Apply width to side channel
  ASide := ASide * AWidth;

  // Convert back to Left/Right
  ALeft := AMid + ASide;
  ARight := AMid - ASide;
end;

// Stereo balance control (-1.0 = full left, +1.0 = full right)
class procedure TSedaiStereoProcessor.ApplyStereoBalance(var ALeft, ARight: Single; ABalance: Single);
var
  AClampedBalance: Single;
begin
  AClampedBalance := ClampPan(ABalance);

  if AClampedBalance < 0.0 then
  begin
    // Attenuate right channel
    ARight := ARight * (1.0 + AClampedBalance);
  end
  else if AClampedBalance > 0.0 then
  begin
    // Attenuate left channel
    ALeft := ALeft * (1.0 - AClampedBalance);
  end;
  // If balance = 0.0, no change
end;

// Swap left and right channels
class procedure TSedaiStereoProcessor.SwapChannels(var ALeft, ARight: Single);
var
  ATemp: Single;
begin
  ATemp := ALeft;
  ALeft := ARight;
  ARight := ATemp;
end;

// Mix multiple stereo samples
class function TSedaiStereoProcessor.MixStereoSamples(const ASamples: array of TStereoSample): TStereoSample;
var
  i: Integer;
begin
  Result.Left := 0.0;
  Result.Right := 0.0;

  for i := 0 to Length(ASamples) - 1 do
  begin
    Result.Left := Result.Left + ASamples[i].Left;
    Result.Right := Result.Right + ASamples[i].Right;
  end;
end;

// Mix with automatic clipping protection
class function TSedaiStereoProcessor.MixStereoSamplesWithClipping(const ASamples: array of TStereoSample): TStereoSample;
begin
  Result := MixStereoSamples(ASamples);
  Result := ClipStereoSample(Result);
end;

// Add one stereo sample to another
class procedure TSedaiStereoProcessor.AddStereoSample(var ATarget: TStereoSample; const ASource: TStereoSample);
begin
  ATarget.Left := ATarget.Left + ASource.Left;
  ATarget.Right := ATarget.Right + ASource.Right;
end;

// Convert mono to stereo with panning
class function TSedaiStereoProcessor.MonoToStereo(AMono: Single; APan: TPanPosition): TStereoSample;
begin
  CalculatePan(APan, AMono, Result.Left, Result.Right);
end;

// Convert stereo to mono (0=average, 1=left only, 2=right only)
class function TSedaiStereoProcessor.StereoToMono(const AStereo: TStereoSample; AMethod: Integer): Single;
begin
  case AMethod of
    0: Result := (AStereo.Left + AStereo.Right) * 0.5; // Average
    1: Result := AStereo.Left;  // Left only
    2: Result := AStereo.Right; // Right only
  else
    Result := (AStereo.Left + AStereo.Right) * 0.5; // Default to average
  end;
end;

// Create stereo sample from left/right values
class function TSedaiStereoProcessor.CreateStereoSample(ALeft, ARight: Single): TStereoSample;
begin
  Result.Left := ALeft;
  Result.Right := ARight;
end;

// Clip stereo sample to prevent overload
class function TSedaiStereoProcessor.ClipStereoSample(const ASample: TStereoSample): TStereoSample;
begin
  Result.Left := ASample.Left;
  Result.Right := ASample.Right;

  if Result.Left > 1.0 then Result.Left := 1.0
  else if Result.Left < -1.0 then Result.Left := -1.0;

  if Result.Right > 1.0 then Result.Right := 1.0
  else if Result.Right < -1.0 then Result.Right := -1.0;
end;

// Get peak level of stereo sample
class function TSedaiStereoProcessor.GetStereoSamplePeak(const ASample: TStereoSample): Single;
var
  ALeftAbs, ARightAbs: Single;
begin
  ALeftAbs := Abs(ASample.Left);
  ARightAbs := Abs(ASample.Right);

  if ALeftAbs > ARightAbs then
    Result := ALeftAbs
  else
    Result := ARightAbs;
end;

// Check if stereo sample is effectively silent
class function TSedaiStereoProcessor.IsStereoSampleSilent(const ASample: TStereoSample; AThreshold: Single): Boolean;
begin
  Result := (Abs(ASample.Left) < AThreshold) and (Abs(ASample.Right) < AThreshold);
end;

// Calculate phase correlation between left and right channels
class function TSedaiStereoProcessor.CalculatePhaseCorrelation(const ALeft, ARight: array of Single): Single;
var
  i: Integer;
  ASumLR, ASumLL, ASumRR: Single;
  ACount: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
  begin
    Result := 0.0;
    Exit;
  end;

  ACount := Length(ALeft);
  if ACount = 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  ASumLR := 0.0;
  ASumLL := 0.0;
  ASumRR := 0.0;

  for i := 0 to ACount - 1 do
  begin
    ASumLR := ASumLR + (ALeft[i] * ARight[i]);
    ASumLL := ASumLL + (ALeft[i] * ALeft[i]);
    ASumRR := ASumRR + (ARight[i] * ARight[i]);
  end;

  if (ASumLL = 0.0) or (ASumRR = 0.0) then
    Result := 0.0
  else
    Result := ASumLR / Sqrt(ASumLL * ASumRR);
end;

// Calculate stereo width from left/right channels
class function TSedaiStereoProcessor.CalculateStereoWidth(const ALeft, ARight: array of Single): Single;
var
  i: Integer;
  AMid, ASide: Single;
  ASidePower, AMidPower: Single;
  ACount: Integer;
begin
  ACount := Length(ALeft);
  if (ACount = 0) or (ACount <> Length(ARight)) then
  begin
    Result := 0.0;
    Exit;
  end;

  ASidePower := 0.0;
  AMidPower := 0.0;

  for i := 0 to ACount - 1 do
  begin
    AMid := (ALeft[i] + ARight[i]) * 0.5;
    ASide := (ALeft[i] - ARight[i]) * 0.5;

    AMidPower := AMidPower + (AMid * AMid);
    ASidePower := ASidePower + (ASide * ASide);
  end;

  if AMidPower = 0.0 then
    Result := 1.0
  else
    Result := Sqrt(ASidePower / AMidPower);
end;

// Check if stereo signal is mono-compatible
class function TSedaiStereoProcessor.DetectMonoCompatibility(const AStereo: TStereoSample): Boolean;
var
  AMonoSum: Single;
  AThreshold: Single;
begin
  AThreshold := 0.001; // Very small threshold
  AMonoSum := AStereo.Left + AStereo.Right;

  // If the mono sum is significantly different from 2x either channel,
  // there might be phase issues
  Result := (Abs(AMonoSum - 2.0 * AStereo.Left) < AThreshold) and
            (Abs(AMonoSum - 2.0 * AStereo.Right) < AThreshold);
end;

// Create pseudo-stereo from mono signal using delay
class procedure TSedaiStereoProcessor.ApplyPseudoStereo(AMono: Single; out ALeft, ARight: Single; ADelay: Single);
begin
  // Simple pseudo-stereo: original on left, slightly delayed on right
  // In a real implementation, you'd need a delay buffer
  ALeft := AMono;
  ARight := AMono * 0.7; // Slightly attenuated for width

  // Note: Real pseudo-stereo would require a circular buffer for delay
  // This is a simplified version
end;

// Apply Haas effect (precedence effect) for stereo width
class procedure TSedaiStereoProcessor.ApplyHaasEffect(var ALeft, ARight: Single; ADelayMs: Single);
var
  ADelayFactor: Single;
begin
  // Simplified Haas effect - attenuate the "delayed" channel
  // Real implementation would need delay buffers

  if ADelayMs > 0 then
  begin
    // Delay right channel effect
    ADelayFactor := 1.0 - (ADelayMs / 40.0); // Up to 40ms delay
    if ADelayFactor < 0.5 then ADelayFactor := 0.5;
    ARight := ARight * ADelayFactor;
  end
  else if ADelayMs < 0 then
  begin
    // Delay left channel effect
    ADelayFactor := 1.0 - (Abs(ADelayMs) / 40.0);
    if ADelayFactor < 0.5 then ADelayFactor := 0.5;
    ALeft := ALeft * ADelayFactor;
  end;
  // If ADelayMs = 0, no effect
end;

end.
