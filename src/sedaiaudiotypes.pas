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

unit SedaiAudioTypes;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils;

type
  // Waveform types
  TWaveType = (wtSine, wtSquare, wtSawtooth, wtTriangle, wtNoise, wtFM);

  // End modes for anti-click
  TEndMode = (emImmediate, emZeroCrossing, emFadeOut, emADSR);

  // Wavetable structure
  TWavetable = record
    Name: string;
    WaveCount: Integer;
    SampleLength: Integer;
    Samples: array of array of Single; // [wave][sample]
    IsLoaded: Boolean;
  end;

  // Wavetable types
  TWavetableType = (wtClassic, wtSerum, wtWasp, wtPPG, wtDigital, wtVocal,
                    wtMetallic, wtGlass, wtOrgan, wtEvolving, wtCustom);

  // NEW: Wavetable file formats - MOVED HERE from SedaiWavetableLoader
  TWavetableFormat = (
    wtfUnknown,
    wtfSerum,       // .wav (2048 samples/frame)
    wtfVital,       // .wav (Serum compatible)
    wtfSurge,       // .wt format
    wtfGeneric      // Generic .wav
  );

  // NEW: Wavetable metadata - MOVED HERE
  TWavetableInfo = record
    Name: string;
    Format: TWavetableFormat;
    FrameCount: Integer;
    SampleRate: Integer;
    FileSize: Int64;
    IsValid: Boolean;
  end;

  // Effect types
  TEffectType = (efNone, efReverb, efDelay, efChorus, efFlanger, efPhaser,
                 efDistortion, efCompressor, efEQ, efFilter);

  // Platform detection
  TPlatform = (ptUnknown, ptRaspberryPi4, ptRaspberryPi5, ptDesktop, ptWorkstation);

  // ADSR envelope phases
  TADSRPhase = (apIdle, apAttack, apDecay, apSustain, apRelease);

  // Stereo types
  TStereoMode = (smMono, smStereo);
  TPanPosition = Single;
  TPanLaw = (plLinear, plConstantPower, plSineCosine, pl3dB, pl6dB);

  // FM Synthesis types
  TFMAlgorithm = (fmSimple, fmStack, fmParallel, fmFeedback, fmComplex);

  // ADSR envelope structure
  TADSR = record
    Attack: Single;
    Decay: Single;
    Sustain: Single;
    Release: Single;
    Phase: TADSRPhase;
    Level: Single;
    Timer: Single;
    ReleaseStartLevel: Single; // Level at which release phase started (anti-click)
  end;

  // Stereo sample structure
  TStereoSample = record
    Left: Single;
    Right: Single;
  end;

  // FM Operator structure
  TFMOperator = record
    Frequency: Single;
    FreqRatio: Single;       // Frequency ratio relative to base frequency
    Phase: Single;           // Current phase (0 to 2π)
    Amplitude: Single;       // Operator amplitude
    FeedbackLevel: Single;   // Self-feedback amount (0.0 to 1.0)
    LastOutput: Single;      // Previous output for feedback
    ADSR: TADSR;            // Envelope for this operator
  end;

  // FM Synthesis structure
  TFMSynthesis = record
    IsActive: Boolean;
    Algorithm: TFMAlgorithm;
    OperatorCount: Integer;
    Operators: array[0..5] of TFMOperator;  // Up to 6 operators (DX7 style)
    ModulationDepth: Single;
    CarrierIndex: Integer;   // Which operator is the carrier
  end;

  // ADSR Phase name array
  TADSRPhaseNameArray = array[TADSRPhase] of string;

// Helper functions
function ClampPan(APan: TPanPosition): TPanPosition;
function PanToString(APan: TPanPosition): string;
function CreateADSR(AAttack, ADecay, ASustain, ARelease: Single): TADSR;
procedure ResetADSR(var AADSR: TADSR);

// ADSR helper
function GetADSRPhaseNames: TADSRPhaseNameArray;

// NEW: Wavetable format helpers - MOVED HERE
function WavetableFormatToString(AFormat: TWavetableFormat): string;
function GetSupportedWavetableExtensions: TStringArray;
function IsWavetableExtension(const AExtension: string): Boolean;

implementation

function ClampPan(APan: TPanPosition): TPanPosition;
begin
  if APan < -1.0 then Result := -1.0
  else if APan > 1.0 then Result := 1.0
  else Result := APan;
end;

function PanToString(APan: TPanPosition): string;
begin
  if APan < -0.9 then Result := 'Hard Left'
  else if APan < -0.5 then Result := 'Left'
  else if APan < -0.1 then Result := 'Center-Left'
  else if APan < 0.1 then Result := 'Center'
  else if APan < 0.5 then Result := 'Center-Right'
  else if APan < 0.9 then Result := 'Right'
  else Result := 'Hard Right';
end;

function CreateADSR(AAttack, ADecay, ASustain, ARelease: Single): TADSR;
begin
  Result.Attack := AAttack;
  Result.Decay := ADecay;
  Result.Sustain := ASustain;
  Result.Release := ARelease;
  Result.Phase := apIdle;
  Result.Level := 0.0;
  Result.Timer := 0.0;
end;

procedure ResetADSR(var AADSR: TADSR);
begin
  AADSR.Phase := apIdle;
  AADSR.Level := 0.0;
  AADSR.Timer := 0.0;
end;

function GetADSRPhaseNames: TADSRPhaseNameArray;
begin
  Result[apIdle] := 'Idle';
  Result[apAttack] := 'Attack';
  Result[apDecay] := 'Decay';
  Result[apSustain] := 'Sustain';
  Result[apRelease] := 'Release';
end;

// NEW: Wavetable format helper functions - MOVED HERE
function WavetableFormatToString(AFormat: TWavetableFormat): string;
begin
  case AFormat of
    wtfSerum: Result := 'Serum';
    wtfVital: Result := 'Vital';
    wtfSurge: Result := 'Surge';
    wtfGeneric: Result := 'Generic WAV';
  else
    Result := 'Unknown';
  end;
end;

function GetSupportedWavetableExtensions: TStringArray;
begin
  SetLength(Result, 3);
  Result[0] := '.wav';
  Result[1] := '.wt';
  Result[2] := '.flac';
end;

function IsWavetableExtension(const AExtension: string): Boolean;
var
  AExt: string;
  AExts: TStringArray;
  i: Integer;
begin
  Result := False;
  AExt := LowerCase(AExtension);
  AExts := GetSupportedWavetableExtensions;

  for i := 0 to Length(AExts) - 1 do
  begin
    if AExt = AExts[i] then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

end.
