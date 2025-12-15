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

unit SedaiAudioEffects;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes;

type
  // Delay line for various effects
  TDelayLine = record
    Buffer: array of Single;
    BufferSize: Integer;
    WritePos: Integer;
    SampleRate: Single;
  end;

  // Simple Delay Effect
  TDelayEffect = record
    DelayLine: TDelayLine;
    DelayTime: Single;      // in seconds
    Feedback: Single;       // 0.0 to 0.99
    Mix: Single;            // 0.0 (dry) to 1.0 (wet)
    SampleRate: Single;
  end;

  // Reverb Effect (Schroeder reverb with comb + allpass filters)
  TReverbEffect = record
    CombFilters: array[0..3] of TDelayLine;
    AllPassFilters: array[0..1] of TDelayLine;
    CombFeedback: array[0..3] of Single;
    RoomSize: Single;       // 0.0 to 1.0
    Damping: Single;        // 0.0 to 1.0
    Mix: Single;            // 0.0 (dry) to 1.0 (wet)
    SampleRate: Single;
  end;

  // Chorus Effect
  TChorusEffect = record
    DelayLine: TDelayLine;
    LFOPhase: Single;
    LFORate: Single;        // Hz
    Depth: Single;          // in milliseconds
    Mix: Single;            // 0.0 (dry) to 1.0 (wet)
    SampleRate: Single;
  end;

  // Flanger Effect
  TFlangerEffect = record
    DelayLine: TDelayLine;
    LFOPhase: Single;
    LFORate: Single;        // Hz
    Depth: Single;          // in milliseconds
    Feedback: Single;       // 0.0 to 0.99
    Mix: Single;            // 0.0 (dry) to 1.0 (wet)
    SampleRate: Single;
  end;

  // Distortion/Overdrive Effect
  TDistortionEffect = record
    Drive: Single;          // 1.0 to 20.0+
    Tone: Single;           // 0.0 to 1.0 (low-pass filter cutoff)
    Mix: Single;            // 0.0 (dry) to 1.0 (wet)
  end;

  TSedaiAudioEffects = class
  public
    // Delay Line management
    class procedure CreateDelayLine(var ADelayLine: TDelayLine; AMaxTimeSec: Single; ASampleRate: Single);
    class procedure ClearDelayLine(var ADelayLine: TDelayLine);
    class procedure WriteToDelay(var ADelayLine: TDelayLine; ASample: Single);
    class function ReadFromDelay(var ADelayLine: TDelayLine; ADelaySamples: Integer): Single;
    class function ReadFromDelayInterpolated(var ADelayLine: TDelayLine; ADelaySamples: Single): Single;

    // Delay Effect
    class function CreateDelayEffect(ADelayTime, AFeedback, AMix: Single; ASampleRate: Single = 44100): TDelayEffect;
    class function ProcessDelaySample(var AEffect: TDelayEffect; ASample: Single): Single;
    class procedure ProcessDelayBuffer(var AEffect: TDelayEffect; var ABuffer: array of Single; ACount: Integer);

    // Reverb Effect
    class function CreateReverbEffect(ARoomSize, ADamping, AMix: Single; ASampleRate: Single = 44100): TReverbEffect;
    class function ProcessReverbSample(var AEffect: TReverbEffect; ASample: Single): Single;
    class procedure ProcessReverbBuffer(var AEffect: TReverbEffect; var ABuffer: array of Single; ACount: Integer);

    // Chorus Effect
    class function CreateChorusEffect(ARate, ADepth, AMix: Single; ASampleRate: Single = 44100): TChorusEffect;
    class function ProcessChorusSample(var AEffect: TChorusEffect; ASample: Single): Single;
    class procedure ProcessChorusBuffer(var AEffect: TChorusEffect; var ABuffer: array of Single; ACount: Integer);

    // Flanger Effect
    class function CreateFlangerEffect(ARate, ADepth, AFeedback, AMix: Single; ASampleRate: Single = 44100): TFlangerEffect;
    class function ProcessFlangerSample(var AEffect: TFlangerEffect; ASample: Single): Single;
    class procedure ProcessFlangerBuffer(var AEffect: TFlangerEffect; var ABuffer: array of Single; ACount: Integer);

    // Distortion Effect
    class function CreateDistortionEffect(ADrive, ATone, AMix: Single): TDistortionEffect;
    class function ProcessDistortionSample(var AEffect: TDistortionEffect; ASample: Single): Single;
    class procedure ProcessDistortionBuffer(var AEffect: TDistortionEffect; var ABuffer: array of Single; ACount: Integer);

    // Utility functions
    class function SoftClip(ASample: Single): Single;
    class function HardClip(ASample: Single): Single;
    class function TanhDistortion(ASample, ADrive: Single): Single;
  end;

implementation

const
  TWO_PI = 2.0 * Pi;

// Create a delay line with specified maximum delay time
class procedure TSedaiAudioEffects.CreateDelayLine(var ADelayLine: TDelayLine; AMaxTimeSec, ASampleRate: Single);
begin
  ADelayLine.SampleRate := ASampleRate;
  ADelayLine.BufferSize := Round(AMaxTimeSec * ASampleRate) + 1;
  SetLength(ADelayLine.Buffer, ADelayLine.BufferSize);
  ADelayLine.WritePos := 0;
  ClearDelayLine(ADelayLine);
end;

// Clear delay line buffer
class procedure TSedaiAudioEffects.ClearDelayLine(var ADelayLine: TDelayLine);
var
  i: Integer;
begin
  for i := 0 to ADelayLine.BufferSize - 1 do
    ADelayLine.Buffer[i] := 0.0;
end;

// Write sample to delay line
class procedure TSedaiAudioEffects.WriteToDelay(var ADelayLine: TDelayLine; ASample: Single);
begin
  ADelayLine.Buffer[ADelayLine.WritePos] := ASample;
  ADelayLine.WritePos := (ADelayLine.WritePos + 1) mod ADelayLine.BufferSize;
end;

// Read from delay line (no interpolation)
class function TSedaiAudioEffects.ReadFromDelay(var ADelayLine: TDelayLine; ADelaySamples: Integer): Single;
var
  ReadPos: Integer;
begin
  ReadPos := ADelayLine.WritePos - ADelaySamples;
  while ReadPos < 0 do
    ReadPos := ReadPos + ADelayLine.BufferSize;
  Result := ADelayLine.Buffer[ReadPos mod ADelayLine.BufferSize];
end;

// Read from delay line with linear interpolation (for smooth modulation)
class function TSedaiAudioEffects.ReadFromDelayInterpolated(var ADelayLine: TDelayLine; ADelaySamples: Single): Single;
var
  IntDelay, ReadPos1, ReadPos2: Integer;
  Frac, Sample1, Sample2: Single;
begin
  IntDelay := Trunc(ADelaySamples);
  Frac := ADelaySamples - IntDelay;

  ReadPos1 := ADelayLine.WritePos - IntDelay;
  ReadPos2 := ReadPos1 - 1;

  while ReadPos1 < 0 do
    ReadPos1 := ReadPos1 + ADelayLine.BufferSize;
  while ReadPos2 < 0 do
    ReadPos2 := ReadPos2 + ADelayLine.BufferSize;

  Sample1 := ADelayLine.Buffer[ReadPos1 mod ADelayLine.BufferSize];
  Sample2 := ADelayLine.Buffer[ReadPos2 mod ADelayLine.BufferSize];

  Result := Sample1 * (1.0 - Frac) + Sample2 * Frac;
end;

// Create delay effect
class function TSedaiAudioEffects.CreateDelayEffect(ADelayTime, AFeedback, AMix, ASampleRate: Single): TDelayEffect;
begin
  Result.DelayTime := ADelayTime;
  Result.Feedback := AFeedback;
  Result.Mix := AMix;
  Result.SampleRate := ASampleRate;
  CreateDelayLine(Result.DelayLine, 2.0, ASampleRate); // Max 2 seconds
end;

// Process single sample through delay
class function TSedaiAudioEffects.ProcessDelaySample(var AEffect: TDelayEffect; ASample: Single): Single;
var
  DelaySamples: Integer;
  DelayedSample, OutputSample: Single;
begin
  DelaySamples := Round(AEffect.DelayTime * AEffect.SampleRate);

  DelayedSample := ReadFromDelay(AEffect.DelayLine, DelaySamples);
  OutputSample := ASample * (1.0 - AEffect.Mix) + DelayedSample * AEffect.Mix;

  WriteToDelay(AEffect.DelayLine, ASample + DelayedSample * AEffect.Feedback);

  Result := OutputSample;
end;

// Process buffer through delay
class procedure TSedaiAudioEffects.ProcessDelayBuffer(var AEffect: TDelayEffect; var ABuffer: array of Single; ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do
    ABuffer[i] := ProcessDelaySample(AEffect, ABuffer[i]);
end;

// Create reverb effect (Schroeder reverb)
class function TSedaiAudioEffects.CreateReverbEffect(ARoomSize, ADamping, AMix, ASampleRate: Single): TReverbEffect;
const
  // Comb filter delays (in samples at 44.1kHz, scaled for other rates)
  CombDelays: array[0..3] of Single = (0.0297, 0.0371, 0.0411, 0.0437);
  // Allpass filter delays
  AllPassDelays: array[0..1] of Single = (0.005, 0.0017);
var
  i: Integer;
  Scale: Single;
begin
  Result.RoomSize := ARoomSize;
  Result.Damping := ADamping;
  Result.Mix := AMix;
  Result.SampleRate := ASampleRate;

  Scale := ASampleRate / 44100.0;

  // Create comb filters
  for i := 0 to 3 do
  begin
    CreateDelayLine(Result.CombFilters[i], CombDelays[i] * (1.0 + ARoomSize) * Scale, ASampleRate);
    Result.CombFeedback[i] := 0.7 + ARoomSize * 0.28; // 0.7 to 0.98
  end;

  // Create allpass filters
  for i := 0 to 1 do
    CreateDelayLine(Result.AllPassFilters[i], AllPassDelays[i] * Scale, ASampleRate);
end;

// Process single sample through reverb
class function TSedaiAudioEffects.ProcessReverbSample(var AEffect: TReverbEffect; ASample: Single): Single;
var
  i: Integer;
  CombOut, AllPassOut, WetSignal: Single;
  DelaySamples: Integer;
  Delayed: Single;
begin
  WetSignal := 0.0;

  // Process through parallel comb filters
  for i := 0 to 3 do
  begin
    DelaySamples := AEffect.CombFilters[i].BufferSize - 1;
    Delayed := ReadFromDelay(AEffect.CombFilters[i], DelaySamples);

    // Apply damping (simple low-pass)
    Delayed := Delayed * (1.0 - AEffect.Damping);

    CombOut := ASample + Delayed * AEffect.CombFeedback[i];
    WriteToDelay(AEffect.CombFilters[i], CombOut);

    WetSignal := WetSignal + Delayed;
  end;

  WetSignal := WetSignal * 0.25; // Mix comb outputs

  // Process through series allpass filters
  for i := 0 to 1 do
  begin
    DelaySamples := AEffect.AllPassFilters[i].BufferSize - 1;
    Delayed := ReadFromDelay(AEffect.AllPassFilters[i], DelaySamples);

    AllPassOut := -WetSignal + Delayed;
    WriteToDelay(AEffect.AllPassFilters[i], WetSignal + Delayed * 0.5);
    WetSignal := AllPassOut;
  end;

  // Mix dry and wet
  Result := ASample * (1.0 - AEffect.Mix) + WetSignal * AEffect.Mix;
end;

// Process buffer through reverb
class procedure TSedaiAudioEffects.ProcessReverbBuffer(var AEffect: TReverbEffect; var ABuffer: array of Single; ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do
    ABuffer[i] := ProcessReverbSample(AEffect, ABuffer[i]);
end;

// Create chorus effect
class function TSedaiAudioEffects.CreateChorusEffect(ARate, ADepth, AMix, ASampleRate: Single): TChorusEffect;
begin
  Result.LFORate := ARate;
  Result.Depth := ADepth;
  Result.Mix := AMix;
  Result.SampleRate := ASampleRate;
  Result.LFOPhase := 0.0;
  CreateDelayLine(Result.DelayLine, 0.05, ASampleRate); // Max 50ms delay
end;

// Process single sample through chorus
class function TSedaiAudioEffects.ProcessChorusSample(var AEffect: TChorusEffect; ASample: Single): Single;
var
  LFO, DelayTime, DelayedSample: Single;
begin
  // Generate LFO (sine wave)
  LFO := Sin(AEffect.LFOPhase);
  AEffect.LFOPhase := AEffect.LFOPhase + (TWO_PI * AEffect.LFORate / AEffect.SampleRate);
  if AEffect.LFOPhase >= TWO_PI then
    AEffect.LFOPhase := AEffect.LFOPhase - TWO_PI;

  // Modulate delay time
  DelayTime := 0.02 + (LFO * 0.5 + 0.5) * (AEffect.Depth / 1000.0); // Base delay + modulation
  DelayedSample := ReadFromDelayInterpolated(AEffect.DelayLine, DelayTime * AEffect.SampleRate);

  WriteToDelay(AEffect.DelayLine, ASample);

  // Mix dry and wet
  Result := ASample * (1.0 - AEffect.Mix) + DelayedSample * AEffect.Mix;
end;

// Process buffer through chorus
class procedure TSedaiAudioEffects.ProcessChorusBuffer(var AEffect: TChorusEffect; var ABuffer: array of Single; ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do
    ABuffer[i] := ProcessChorusSample(AEffect, ABuffer[i]);
end;

// Create flanger effect
class function TSedaiAudioEffects.CreateFlangerEffect(ARate, ADepth, AFeedback, AMix, ASampleRate: Single): TFlangerEffect;
begin
  Result.LFORate := ARate;
  Result.Depth := ADepth;
  Result.Feedback := AFeedback;
  Result.Mix := AMix;
  Result.SampleRate := ASampleRate;
  Result.LFOPhase := 0.0;
  CreateDelayLine(Result.DelayLine, 0.01, ASampleRate); // Max 10ms delay
end;

// Process single sample through flanger
class function TSedaiAudioEffects.ProcessFlangerSample(var AEffect: TFlangerEffect; ASample: Single): Single;
var
  LFO, DelayTime, DelayedSample, FeedbackSample: Single;
begin
  // Generate LFO (sine wave)
  LFO := Sin(AEffect.LFOPhase);
  AEffect.LFOPhase := AEffect.LFOPhase + (TWO_PI * AEffect.LFORate / AEffect.SampleRate);
  if AEffect.LFOPhase >= TWO_PI then
    AEffect.LFOPhase := AEffect.LFOPhase - TWO_PI;

  // Modulate delay time (shorter delay than chorus)
  DelayTime := 0.001 + (LFO * 0.5 + 0.5) * (AEffect.Depth / 1000.0);
  DelayedSample := ReadFromDelayInterpolated(AEffect.DelayLine, DelayTime * AEffect.SampleRate);

  // Apply feedback
  FeedbackSample := ASample + DelayedSample * AEffect.Feedback;
  WriteToDelay(AEffect.DelayLine, FeedbackSample);

  // Mix dry and wet
  Result := ASample * (1.0 - AEffect.Mix) + DelayedSample * AEffect.Mix;
end;

// Process buffer through flanger
class procedure TSedaiAudioEffects.ProcessFlangerBuffer(var AEffect: TFlangerEffect; var ABuffer: array of Single; ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do
    ABuffer[i] := ProcessFlangerSample(AEffect, ABuffer[i]);
end;

// Create distortion effect
class function TSedaiAudioEffects.CreateDistortionEffect(ADrive, ATone, AMix: Single): TDistortionEffect;
begin
  Result.Drive := ADrive;
  Result.Tone := ATone;
  Result.Mix := AMix;
end;

// Process single sample through distortion
class function TSedaiAudioEffects.ProcessDistortionSample(var AEffect: TDistortionEffect; ASample: Single): Single;
var
  Distorted: Single;
begin
  // Apply distortion using tanh
  Distorted := TanhDistortion(ASample, AEffect.Drive);

  // Simple tone control (one-pole low-pass)
  // In a real implementation, you'd maintain state for this filter

  // Mix dry and wet
  Result := ASample * (1.0 - AEffect.Mix) + Distorted * AEffect.Mix;
end;

// Process buffer through distortion
class procedure TSedaiAudioEffects.ProcessDistortionBuffer(var AEffect: TDistortionEffect; var ABuffer: array of Single; ACount: Integer);
var
  i: Integer;
begin
  for i := 0 to ACount - 1 do
    ABuffer[i] := ProcessDistortionSample(AEffect, ABuffer[i]);
end;

// Soft clipping function
class function TSedaiAudioEffects.SoftClip(ASample: Single): Single;
begin
  if ASample > 1.0 then
    Result := 1.0
  else if ASample < -1.0 then
    Result := -1.0
  else
    Result := ASample;
end;

// Hard clipping function
class function TSedaiAudioEffects.HardClip(ASample: Single): Single;
begin
  if Abs(ASample) <= 1.0/3.0 then
    Result := 2.0 * ASample
  else if Abs(ASample) <= 2.0/3.0 then
    Result := Sign(ASample) * (3.0 - Power(2.0 - 3.0 * Abs(ASample), 2)) / 3.0
  else
    Result := Sign(ASample);
end;

// Tanh distortion (warm overdrive)
class function TSedaiAudioEffects.TanhDistortion(ASample, ADrive: Single): Single;
begin
  Result := Tanh(ASample * ADrive) / Tanh(ADrive);
end;

end.
