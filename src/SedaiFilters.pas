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

unit SedaiFilters;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes;

type
  // Filter types
  TFilterType = (ftLowPass, ftHighPass, ftBandPass, ftNotch, ftAllPass, ftPeaking);

  // Filter modes/slopes
  TFilterSlope = (fs12dB, fs24dB, fs48dB); // 2-pole, 4-pole, 8-pole

  // Biquad filter coefficients
  TBiquadCoeffs = record
    a0, a1, a2: Single;  // Numerator coefficients
    b1, b2: Single;      // Denominator coefficients
  end;

  // Biquad filter state
  TBiquadState = record
    x1, x2: Single;  // Input history
    y1, y2: Single;  // Output history
  end;

  // Complete biquad filter
  TBiquadFilter = record
    Coeffs: TBiquadCoeffs;
    State: TBiquadState;
    FilterType: TFilterType;
    Frequency: Single;
    Q: Single;
    Gain: Single; // For peaking/shelving filters
    SampleRate: Single;
  end;

  // Multi-pole filter (cascaded biquads)
  TMultiPoleFilter = record
    Stages: array[0..3] of TBiquadFilter; // Up to 4 stages = 48dB/oct
    NumStages: Integer;
    FilterType: TFilterType;
  end;

  TSedaiFilters = class
  public
    // Biquad filter creation and control
    class function CreateBiquadFilter(AType: TFilterType; AFreq, AQ: Single;
                                      ASampleRate: Single = 44100): TBiquadFilter;
    class procedure UpdateFilterCoeffs(var AFilter: TBiquadFilter);
    class procedure ResetFilterState(var AFilter: TBiquadFilter);

    // Biquad filter processing
    class function ProcessBiquadSample(var AFilter: TBiquadFilter; ASample: Single): Single;
    class procedure ProcessBiquadBuffer(var AFilter: TBiquadFilter;
                                        var ABuffer: array of Single; ACount: Integer);

    // Multi-pole filter creation and control
    class function CreateMultiPoleFilter(AType: TFilterType; AFreq, AQ: Single;
                                         ASlope: TFilterSlope; ASampleRate: Single = 44100): TMultiPoleFilter;
    class procedure UpdateMultiPoleFilter(var AFilter: TMultiPoleFilter; AFreq, AQ: Single);
    class procedure ResetMultiPoleFilter(var AFilter: TMultiPoleFilter);

    // Multi-pole filter processing
    class function ProcessMultiPoleSample(var AFilter: TMultiPoleFilter; ASample: Single): Single;
    class procedure ProcessMultiPoleBuffer(var AFilter: TMultiPoleFilter;
                                           var ABuffer: array of Single; ACount: Integer);

    // Coefficient calculation functions (Audio EQ Cookbook formulas)
    class procedure CalculateLowPassCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
    class procedure CalculateHighPassCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
    class procedure CalculateBandPassCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
    class procedure CalculateNotchCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
    class procedure CalculateAllPassCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
    class procedure CalculatePeakingCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, AGain, ASampleRate: Single);

    // Utility functions
    class function ClampFrequency(AFreq, ASampleRate: Single): Single;
    class function ClampQ(AQ: Single): Single;
  end;

implementation

const
  TWO_PI = 2.0 * Pi;
  MIN_Q = 0.1;
  MAX_Q = 100.0;

// Create a biquad filter with specified parameters
class function TSedaiFilters.CreateBiquadFilter(AType: TFilterType; AFreq, AQ, ASampleRate: Single): TBiquadFilter;
begin
  Result.FilterType := AType;
  Result.Frequency := ClampFrequency(AFreq, ASampleRate);
  Result.Q := ClampQ(AQ);
  Result.Gain := 0.0;
  Result.SampleRate := ASampleRate;

  ResetFilterState(Result);
  UpdateFilterCoeffs(Result);
end;

// Update filter coefficients based on current parameters
class procedure TSedaiFilters.UpdateFilterCoeffs(var AFilter: TBiquadFilter);
begin
  case AFilter.FilterType of
    ftLowPass:  CalculateLowPassCoeffs(AFilter.Coeffs, AFilter.Frequency, AFilter.Q, AFilter.SampleRate);
    ftHighPass: CalculateHighPassCoeffs(AFilter.Coeffs, AFilter.Frequency, AFilter.Q, AFilter.SampleRate);
    ftBandPass: CalculateBandPassCoeffs(AFilter.Coeffs, AFilter.Frequency, AFilter.Q, AFilter.SampleRate);
    ftNotch:    CalculateNotchCoeffs(AFilter.Coeffs, AFilter.Frequency, AFilter.Q, AFilter.SampleRate);
    ftAllPass:  CalculateAllPassCoeffs(AFilter.Coeffs, AFilter.Frequency, AFilter.Q, AFilter.SampleRate);
    ftPeaking:  CalculatePeakingCoeffs(AFilter.Coeffs, AFilter.Frequency, AFilter.Q, AFilter.Gain, AFilter.SampleRate);
  end;
end;

// Reset filter state (clear history)
class procedure TSedaiFilters.ResetFilterState(var AFilter: TBiquadFilter);
begin
  AFilter.State.x1 := 0.0;
  AFilter.State.x2 := 0.0;
  AFilter.State.y1 := 0.0;
  AFilter.State.y2 := 0.0;
end;

// Process a single sample through biquad filter
class function TSedaiFilters.ProcessBiquadSample(var AFilter: TBiquadFilter; ASample: Single): Single;
var
  y: Single;
const
  DENORMAL_FIX = 1.0e-18;  // Tiny value to prevent denormals
begin
  with AFilter do
  begin
    // Biquad difference equation: y[n] = a0*x[n] + a1*x[n-1] + a2*x[n-2] - b1*y[n-1] - b2*y[n-2]
    y := Coeffs.a0 * ASample +
         Coeffs.a1 * State.x1 +
         Coeffs.a2 * State.x2 -
         Coeffs.b1 * State.y1 -
         Coeffs.b2 * State.y2;

    // Anti-denormal: add tiny DC offset to prevent CPU slowdown from denormal numbers
    y := y + DENORMAL_FIX;

    // Soft clamp to prevent runaway resonance
    if y > 4.0 then y := 4.0
    else if y < -4.0 then y := -4.0;

    // Update state
    State.x2 := State.x1;
    State.x1 := ASample;
    State.y2 := State.y1;
    State.y1 := y;

    Result := y;
  end;
end;

// Process a buffer of samples
class procedure TSedaiFilters.ProcessBiquadBuffer(var AFilter: TBiquadFilter; var ABuffer: array of Single; ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do
    ABuffer[i] := ProcessBiquadSample(AFilter, ABuffer[i]);
end;

// Create multi-pole filter (cascaded biquads)
class function TSedaiFilters.CreateMultiPoleFilter(AType: TFilterType; AFreq, AQ: Single;
                                                   ASlope: TFilterSlope; ASampleRate: Single): TMultiPoleFilter;
var
  i: Integer;
  AStageQ: Single;
begin
  Result.FilterType := AType;

  // Determine number of stages based on slope
  case ASlope of
    fs12dB: Result.NumStages := 1;
    fs24dB: Result.NumStages := 2;
    fs48dB: Result.NumStages := 4;
  end;

  // For multi-stage filters:
  // - First stage uses user's Q (for resonance character)
  // - Subsequent stages use Butterworth Q (0.707) for clean slope
  for i := 0 to Result.NumStages - 1 do
  begin
    if i = 0 then
      AStageQ := AQ  // First stage: user's resonance
    else
      AStageQ := 0.707;  // Other stages: Butterworth for slope
    Result.Stages[i] := CreateBiquadFilter(AType, AFreq, AStageQ, ASampleRate);
  end;
end;

// Update multi-pole filter parameters
class procedure TSedaiFilters.UpdateMultiPoleFilter(var AFilter: TMultiPoleFilter; AFreq, AQ: Single);
var
  i: Integer;
  AStageQ: Single;
begin
  // First stage uses user's Q (resonance), others use Butterworth
  for i := 0 to AFilter.NumStages - 1 do
  begin
    if i = 0 then
      AStageQ := AQ
    else
      AStageQ := 0.707;
    AFilter.Stages[i].Frequency := AFreq;
    AFilter.Stages[i].Q := AStageQ;
    UpdateFilterCoeffs(AFilter.Stages[i]);
  end;
end;

// Reset multi-pole filter
class procedure TSedaiFilters.ResetMultiPoleFilter(var AFilter: TMultiPoleFilter);
var
  i: Integer;
begin
  for i := 0 to AFilter.NumStages - 1 do
    ResetFilterState(AFilter.Stages[i]);
end;

// Process sample through multi-pole filter
class function TSedaiFilters.ProcessMultiPoleSample(var AFilter: TMultiPoleFilter; ASample: Single): Single;
var
  i: Integer;
begin
  Result := ASample;
  for i := 0 to AFilter.NumStages - 1 do
    Result := ProcessBiquadSample(AFilter.Stages[i], Result);
end;

// Process buffer through multi-pole filter
class procedure TSedaiFilters.ProcessMultiPoleBuffer(var AFilter: TMultiPoleFilter; var ABuffer: array of Single; ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do
    ABuffer[i] := ProcessMultiPoleSample(AFilter, ABuffer[i]);
end;

// Audio EQ Cookbook formulas - Low Pass Filter
class procedure TSedaiFilters.CalculateLowPassCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
var
  omega, sn, cs, alpha: Single;
  a0_temp, b0, b1, b2, a1_temp, a2_temp: Single;
begin
  omega := TWO_PI * AFreq / ASampleRate;
  sn := Sin(omega);
  cs := Cos(omega);
  alpha := sn / (2.0 * AQ);

  b0 := (1.0 - cs) / 2.0;
  b1 := 1.0 - cs;
  b2 := (1.0 - cs) / 2.0;
  a0_temp := 1.0 + alpha;
  a1_temp := -2.0 * cs;
  a2_temp := 1.0 - alpha;

  // Normalize by a0
  ACoeffs.a0 := b0 / a0_temp;
  ACoeffs.a1 := b1 / a0_temp;
  ACoeffs.a2 := b2 / a0_temp;
  ACoeffs.b1 := a1_temp / a0_temp;
  ACoeffs.b2 := a2_temp / a0_temp;
end;

// Audio EQ Cookbook formulas - High Pass Filter
class procedure TSedaiFilters.CalculateHighPassCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
var
  omega, sn, cs, alpha: Single;
  a0_temp, b0, b1, b2, a1_temp, a2_temp: Single;
begin
  omega := TWO_PI * AFreq / ASampleRate;
  sn := Sin(omega);
  cs := Cos(omega);
  alpha := sn / (2.0 * AQ);

  b0 := (1.0 + cs) / 2.0;
  b1 := -(1.0 + cs);
  b2 := (1.0 + cs) / 2.0;
  a0_temp := 1.0 + alpha;
  a1_temp := -2.0 * cs;
  a2_temp := 1.0 - alpha;

  // Normalize by a0
  ACoeffs.a0 := b0 / a0_temp;
  ACoeffs.a1 := b1 / a0_temp;
  ACoeffs.a2 := b2 / a0_temp;
  ACoeffs.b1 := a1_temp / a0_temp;
  ACoeffs.b2 := a2_temp / a0_temp;
end;

// Audio EQ Cookbook formulas - Band Pass Filter
// Using "constant 0 dB peak gain" variant for consistent volume across Q values
class procedure TSedaiFilters.CalculateBandPassCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
var
  omega, sn, cs, alpha: Single;
  a0_temp, b0, b1, b2, a1_temp, a2_temp: Single;
begin
  omega := TWO_PI * AFreq / ASampleRate;
  sn := Sin(omega);
  cs := Cos(omega);
  alpha := sn / (2.0 * AQ);

  // BPF with constant 0 dB peak gain (peak gain = 1, independent of Q)
  // This ensures volume stays consistent as Q changes
  b0 := AQ * alpha;  // = sn/2 - peak normalized
  b1 := 0.0;
  b2 := -AQ * alpha; // = -sn/2
  a0_temp := 1.0 + alpha;
  a1_temp := -2.0 * cs;
  a2_temp := 1.0 - alpha;

  // Normalize by a0
  ACoeffs.a0 := b0 / a0_temp;
  ACoeffs.a1 := b1 / a0_temp;
  ACoeffs.a2 := b2 / a0_temp;
  ACoeffs.b1 := a1_temp / a0_temp;
  ACoeffs.b2 := a2_temp / a0_temp;
end;

// Audio EQ Cookbook formulas - Notch Filter
class procedure TSedaiFilters.CalculateNotchCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
var
  omega, sn, cs, alpha: Single;
  a0_temp, b0, b1, b2, a1_temp, a2_temp: Single;
begin
  omega := TWO_PI * AFreq / ASampleRate;
  sn := Sin(omega);
  cs := Cos(omega);
  alpha := sn / (2.0 * AQ);

  b0 := 1.0;
  b1 := -2.0 * cs;
  b2 := 1.0;
  a0_temp := 1.0 + alpha;
  a1_temp := -2.0 * cs;
  a2_temp := 1.0 - alpha;

  // Normalize by a0
  ACoeffs.a0 := b0 / a0_temp;
  ACoeffs.a1 := b1 / a0_temp;
  ACoeffs.a2 := b2 / a0_temp;
  ACoeffs.b1 := a1_temp / a0_temp;
  ACoeffs.b2 := a2_temp / a0_temp;
end;

// Audio EQ Cookbook formulas - All Pass Filter
class procedure TSedaiFilters.CalculateAllPassCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, ASampleRate: Single);
var
  omega, sn, cs, alpha: Single;
  a0_temp, b0, b1, b2, a1_temp, a2_temp: Single;
begin
  omega := TWO_PI * AFreq / ASampleRate;
  sn := Sin(omega);
  cs := Cos(omega);
  alpha := sn / (2.0 * AQ);

  b0 := 1.0 - alpha;
  b1 := -2.0 * cs;
  b2 := 1.0 + alpha;
  a0_temp := 1.0 + alpha;
  a1_temp := -2.0 * cs;
  a2_temp := 1.0 - alpha;

  // Normalize by a0
  ACoeffs.a0 := b0 / a0_temp;
  ACoeffs.a1 := b1 / a0_temp;
  ACoeffs.a2 := b2 / a0_temp;
  ACoeffs.b1 := a1_temp / a0_temp;
  ACoeffs.b2 := a2_temp / a0_temp;
end;

// Audio EQ Cookbook formulas - Peaking EQ Filter
class procedure TSedaiFilters.CalculatePeakingCoeffs(var ACoeffs: TBiquadCoeffs; AFreq, AQ, AGain, ASampleRate: Single);
var
  omega, sn, cs, alpha, A: Single;
  a0_temp, b0, b1, b2, a1_temp, a2_temp: Single;
begin
  omega := TWO_PI * AFreq / ASampleRate;
  sn := Sin(omega);
  cs := Cos(omega);
  alpha := sn / (2.0 * AQ);
  A := Power(10.0, AGain / 40.0); // Convert dB to linear gain

  b0 := 1.0 + alpha * A;
  b1 := -2.0 * cs;
  b2 := 1.0 - alpha * A;
  a0_temp := 1.0 + alpha / A;
  a1_temp := -2.0 * cs;
  a2_temp := 1.0 - alpha / A;

  // Normalize by a0
  ACoeffs.a0 := b0 / a0_temp;
  ACoeffs.a1 := b1 / a0_temp;
  ACoeffs.a2 := b2 / a0_temp;
  ACoeffs.b1 := a1_temp / a0_temp;
  ACoeffs.b2 := a2_temp / a0_temp;
end;

// Clamp frequency to valid range (avoid Nyquist issues)
class function TSedaiFilters.ClampFrequency(AFreq, ASampleRate: Single): Single;
var
  MaxFreq: Single;
begin
  MaxFreq := ASampleRate * 0.49; // Stay below Nyquist
  if AFreq < 20.0 then
    Result := 20.0
  else if AFreq > MaxFreq then
    Result := MaxFreq
  else
    Result := AFreq;
end;

// Clamp Q to reasonable range
class function TSedaiFilters.ClampQ(AQ: Single): Single;
begin
  if AQ < MIN_Q then
    Result := MIN_Q
  else if AQ > MAX_Q then
    Result := MAX_Q
  else
    Result := AQ;
end;

end.
