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

unit SedaiSynthesisEngine;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, SedaiAudioTypes,
  SedaiClassicProcessor, SedaiFMProcessor, SedaiWavetableProcessor,
  SedaiWavetableLoader;

type
  // Synthesis engine types
  TSynthEngineType = (setClassic, setFM, setWavetable, setHybrid);

  // Unified synthesis voice - COMPLETELY SEPARATE
  TSynthesisVoice = record
    IsActive: Boolean;
    EngineType: TSynthEngineType;

    // Each synthesis type is completely separate
    ClassicSynth: TClassicSynthesis;      // Classic synthesis
    FMSynth: TFMSynthesis;                // FM synthesis
    WavetableSynth: TWavetableSynthesis;  // Wavetable synthesis

    // Common properties
    Frequency: Single;
    Amplitude: Single;
    PanPosition: TPanPosition;
    VoiceIndex: Integer;
  end;

  { TSedaiSynthesisEngine }

  TSedaiSynthesisEngine = class
  private
    FVoices: array of TSynthesisVoice;
    FVoiceCount: Integer;

  public
    constructor Create(AVoiceCount: Integer = 32);
    destructor Destroy; override;

    // Voice management
    function AllocateVoice: Integer;
    procedure ReleaseVoice(AVoiceIndex: Integer);
    procedure ReleaseAllVoices;

    // Synthesis control - COMPLETELY SEPARATE ENGINES
    procedure PlayClassic(AVoiceIndex: Integer; AFreq: Single;
                         const AClassicPreset: string; AAmplitude: Single = 0.3);
    procedure PlayFM(AVoiceIndex: Integer; AFreq: Single;
                    const AFMPreset: string; AAmplitude: Single = 0.3);
    procedure PlayWavetable(AVoiceIndex: Integer; AFreq: Single;
                           const AWavetableType: TWavetableType;
                           AAmplitude: Single = 0.3);
    procedure PlayWavetableCustom(AVoiceIndex: Integer; AFreq: Single;
                                  const ACustomWavetable: TWavetable;
                                  AAmplitude: Single = 0.3);

    // Processing - CLEAR SEPARATION
    function ProcessVoice(AVoiceIndex: Integer; ASampleRate: Single;
                         ADeltaTime: Single): Single;
    function ProcessAllVoices(ASampleRate: Single; ADeltaTime: Single): Single;

    // Voice control
    procedure NoteOn(AVoiceIndex: Integer);
    procedure NoteOff(AVoiceIndex: Integer);
    procedure SetVoicePan(AVoiceIndex: Integer; APan: TPanPosition);

    // Status
    function GetActiveVoiceCount: Integer;
    function IsVoiceActive(AVoiceIndex: Integer): Boolean;
    function IsVoiceReleaseComplete(AVoiceIndex: Integer): Boolean;
    function GetVoiceInfo(AVoiceIndex: Integer): string;
    function GetMaxVoices: Integer;
  end;

implementation

constructor TSedaiSynthesisEngine.Create(AVoiceCount: Integer);
var
  i: Integer;
begin
  inherited Create;
  FVoiceCount := AVoiceCount;
  SetLength(FVoices, FVoiceCount);

  // Initialize all voices
  for i := 0 to FVoiceCount - 1 do
  begin
    FillChar(FVoices[i], SizeOf(TSynthesisVoice), 0);
    FVoices[i].IsActive := False;
    FVoices[i].EngineType := setClassic;
    FVoices[i].Amplitude := 0.3;
    FVoices[i].PanPosition := 0.0;
    FVoices[i].VoiceIndex := i;

    // Initialize synthesis engines - ALL SEPARATE
    TSedaiClassicProcessor.InitializeClassicSynth(FVoices[i].ClassicSynth);
    TSedaiFMProcessor.InitializeFM(FVoices[i].FMSynth, fmSimple);
    TSedaiWavetableProcessor.InitializeWavetableSynth(FVoices[i].WavetableSynth);
  end;
end;

destructor TSedaiSynthesisEngine.Destroy;
var
  i: Integer;
begin
  // Cleanup wavetables
  for i := 0 to FVoiceCount - 1 do
  begin
    if FVoices[i].WavetableSynth.Oscillators[0].Wavetable.IsLoaded then
      TSedaiWavetableProcessor.UnloadWavetable(FVoices[i].WavetableSynth.Oscillators[0].Wavetable);
  end;

  SetLength(FVoices, 0);
  inherited Destroy;
end;

// Allocate free voice
function TSedaiSynthesisEngine.AllocateVoice: Integer;
var
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
  begin
    if not FVoices[i].IsActive then
    begin
      Result := i;
      FVoices[i].IsActive := True;
      Exit;
    end;
  end;
  Result := -1; // No free voice
end;

procedure TSedaiSynthesisEngine.ReleaseVoice(AVoiceIndex: Integer);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
    FVoices[AVoiceIndex].IsActive := False;
end;

procedure TSedaiSynthesisEngine.ReleaseAllVoices;
var
  i: Integer;
begin
  for i := 0 to FVoiceCount - 1 do
    FVoices[i].IsActive := False;
end;

// Play different synthesis types - COMPLETELY SEPARATE
procedure TSedaiSynthesisEngine.PlayClassic(AVoiceIndex: Integer; AFreq: Single;
  const AClassicPreset: string; AAmplitude: Single);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
  begin
    with FVoices[AVoiceIndex] do
    begin
      EngineType := setClassic;
      Frequency := AFreq;
      Amplitude := AAmplitude;

      // Load classic preset using ONLY the classic processor
      if AClassicPreset = 'lead' then
        ClassicSynth := TSedaiClassicProcessor.CreateAnalogLead
      else if AClassicPreset = 'bass' then
        ClassicSynth := TSedaiClassicProcessor.CreateAnalogBass
      else if AClassicPreset = 'pad' then
        ClassicSynth := TSedaiClassicProcessor.CreateAnalogPad
      else if AClassicPreset = 'sine' then
      begin
        TSedaiClassicProcessor.InitializeClassicSynth(ClassicSynth);
        TSedaiClassicProcessor.SetupSingleOscillator(ClassicSynth, wtSine);
      end
      else if AClassicPreset = 'square' then
      begin
        TSedaiClassicProcessor.InitializeClassicSynth(ClassicSynth);
        TSedaiClassicProcessor.SetupSingleOscillator(ClassicSynth, wtSquare);
      end
      else if AClassicPreset = 'saw' then
      begin
        TSedaiClassicProcessor.InitializeClassicSynth(ClassicSynth);
        TSedaiClassicProcessor.SetupSingleOscillator(ClassicSynth, wtSawtooth);
      end
      else if AClassicPreset = 'triangle' then
      begin
        TSedaiClassicProcessor.InitializeClassicSynth(ClassicSynth);
        TSedaiClassicProcessor.SetupSingleOscillator(ClassicSynth, wtTriangle);
      end
      else
      begin
        TSedaiClassicProcessor.InitializeClassicSynth(ClassicSynth);
        TSedaiClassicProcessor.SetupSingleOscillator(ClassicSynth, wtSine);
      end;

      TSedaiClassicProcessor.StartClassicAttack(ClassicSynth);
      IsActive := True;
    end;
  end;
end;

procedure TSedaiSynthesisEngine.PlayFM(AVoiceIndex: Integer; AFreq: Single;
  const AFMPreset: string; AAmplitude: Single);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
  begin
    with FVoices[AVoiceIndex] do
    begin
      EngineType := setFM;
      Frequency := AFreq;
      Amplitude := AAmplitude;

      // Load FM preset using ONLY the FM processor
      if AFMPreset = 'epiano' then
        FMSynth := TSedaiFMProcessor.CreateElectricPiano
      else if AFMPreset = 'brass' then
        FMSynth := TSedaiFMProcessor.CreateBrass
      else if AFMPreset = 'bell' then
        FMSynth := TSedaiFMProcessor.CreateBell
      else if AFMPreset = 'organ' then
        FMSynth := TSedaiFMProcessor.CreateOrgan
      else if AFMPreset = 'lead' then
        FMSynth := TSedaiFMProcessor.CreateSynthLead
      else if AFMPreset = 'bass' then
        FMSynth := TSedaiFMProcessor.CreateSynthBass
      else
        FMSynth := TSedaiFMProcessor.CreateElectricPiano;

      TSedaiFMProcessor.StartFMAttack(FMSynth);
      IsActive := True;
    end;
  end;
end;

procedure TSedaiSynthesisEngine.PlayWavetable(AVoiceIndex: Integer; AFreq: Single;
  const AWavetableType: TWavetableType; AAmplitude: Single);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
  begin
    with FVoices[AVoiceIndex] do
    begin
      EngineType := setWavetable;
      Frequency := AFreq;
      Amplitude := AAmplitude;

      // Setup wavetable using ONLY the wavetable processor
      case AWavetableType of
        wtSerum: WavetableSynth := TSedaiWavetableProcessor.CreateSerum;
        wtWasp: WavetableSynth := TSedaiWavetableProcessor.CreateWasp;
        wtPPG: WavetableSynth := TSedaiWavetableProcessor.CreatePPG;
        else WavetableSynth := TSedaiWavetableProcessor.CreateSerum;
      end;

      TSedaiWavetableProcessor.StartWavetableAttack(WavetableSynth);
      IsActive := True;
    end;
  end;
end;

procedure TSedaiSynthesisEngine.PlayWavetableCustom(AVoiceIndex: Integer; AFreq: Single;
  const ACustomWavetable: TWavetable; AAmplitude: Single);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
  begin
    with FVoices[AVoiceIndex] do
    begin
      EngineType := setWavetable;
      Frequency := AFreq;
      Amplitude := AAmplitude;

      // Use custom wavetable from loaded file (e.g., AKWF)
      WavetableSynth := TSedaiWavetableProcessor.CreateFromCustomWavetable(ACustomWavetable);

      TSedaiWavetableProcessor.StartWavetableAttack(WavetableSynth);
      IsActive := True;
    end;
  end;
end;

// Process individual voice - CLEAR SEPARATION OF ENGINES
function TSedaiSynthesisEngine.ProcessVoice(AVoiceIndex: Integer;
  ASampleRate: Single; ADeltaTime: Single): Single;
var
  AReleaseComplete: Boolean;
begin
  Result := 0.0;

  if (AVoiceIndex < 0) or (AVoiceIndex >= FVoiceCount) or
     (not FVoices[AVoiceIndex].IsActive) then Exit;

  AReleaseComplete := False;

  with FVoices[AVoiceIndex] do
  begin
    case EngineType of
      setClassic:
        begin
          // ONLY classic synthesis processing
          Result := TSedaiClassicProcessor.ProcessClassic(ClassicSynth,
                                                         Frequency, ASampleRate, ADeltaTime);
          Result := Result * Amplitude;
          // Check if release is complete
          AReleaseComplete := TSedaiClassicProcessor.IsClassicReleaseComplete(ClassicSynth);
        end;

      setFM:
        begin
          // ONLY FM synthesis processing
          Result := TSedaiFMProcessor.ProcessFM(FMSynth, Frequency,
                                               ASampleRate, ADeltaTime);
          Result := Result * Amplitude;
          // Check if release is complete
          AReleaseComplete := TSedaiFMProcessor.IsFMReleaseComplete(FMSynth);
        end;

      setWavetable:
        begin
          // ONLY wavetable synthesis processing
          Result := TSedaiWavetableProcessor.ProcessWavetable(WavetableSynth,
                                                             Frequency, ASampleRate, ADeltaTime);
          Result := Result * Amplitude;
          // Check if release is complete
          AReleaseComplete := TSedaiWavetableProcessor.IsWavetableReleaseComplete(WavetableSynth);
        end;

      setHybrid:
        begin
          // Hybrid mode - mix multiple engines (future implementation)
          Result := 0.0; // TODO: Implement hybrid synthesis
        end;
    end;

    // Auto-release voice when ADSR is complete (all oscillators in Idle phase)
    if AReleaseComplete then
      IsActive := False;
  end;
end;

// Process all voices
function TSedaiSynthesisEngine.ProcessAllVoices(ASampleRate: Single; ADeltaTime: Single): Single;
var
  i: Integer;
  VoiceSample: Single;
begin
  Result := 0.0;

  for i := 0 to FVoiceCount - 1 do
  begin
    if FVoices[i].IsActive then
    begin
      VoiceSample := ProcessVoice(i, ASampleRate, ADeltaTime);
      Result := Result + VoiceSample;
    end;
  end;

  // Simple normalization to prevent clipping
  if Result > 1.0 then Result := 1.0
  else if Result < -1.0 then Result := -1.0;
end;

// Voice control
procedure TSedaiSynthesisEngine.NoteOn(AVoiceIndex: Integer);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
  begin
    case FVoices[AVoiceIndex].EngineType of
      setClassic: TSedaiClassicProcessor.StartClassicAttack(FVoices[AVoiceIndex].ClassicSynth);
      setFM: TSedaiFMProcessor.StartFMAttack(FVoices[AVoiceIndex].FMSynth);
      setWavetable: TSedaiWavetableProcessor.StartWavetableAttack(FVoices[AVoiceIndex].WavetableSynth);
    end;
  end;
end;

procedure TSedaiSynthesisEngine.NoteOff(AVoiceIndex: Integer);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
  begin
    case FVoices[AVoiceIndex].EngineType of
      setClassic: TSedaiClassicProcessor.StartClassicRelease(FVoices[AVoiceIndex].ClassicSynth);
      setFM: TSedaiFMProcessor.StartFMRelease(FVoices[AVoiceIndex].FMSynth);
      setWavetable: TSedaiWavetableProcessor.StartWavetableRelease(FVoices[AVoiceIndex].WavetableSynth);
    end;
  end;
end;

procedure TSedaiSynthesisEngine.SetVoicePan(AVoiceIndex: Integer; APan: TPanPosition);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
    FVoices[AVoiceIndex].PanPosition := ClampPan(APan);
end;

// Status functions
function TSedaiSynthesisEngine.GetActiveVoiceCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FVoiceCount - 1 do
    if FVoices[i].IsActive then Inc(Result);
end;

function TSedaiSynthesisEngine.IsVoiceActive(AVoiceIndex: Integer): Boolean;
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
    Result := FVoices[AVoiceIndex].IsActive
  else
    Result := False;
end;

function TSedaiSynthesisEngine.IsVoiceReleaseComplete(AVoiceIndex: Integer): Boolean;
begin
  Result := False;
  if (AVoiceIndex < 0) or (AVoiceIndex >= FVoiceCount) then Exit;
  if not FVoices[AVoiceIndex].IsActive then
  begin
    Result := True;  // Already inactive
    Exit;
  end;

  // Check if all ADSR envelopes in the voice have finished releasing
  case FVoices[AVoiceIndex].EngineType of
    setClassic:
      Result := TSedaiClassicProcessor.IsClassicReleaseComplete(FVoices[AVoiceIndex].ClassicSynth);
    setFM:
      Result := TSedaiFMProcessor.IsFMReleaseComplete(FVoices[AVoiceIndex].FMSynth);
    setWavetable:
      Result := TSedaiWavetableProcessor.IsWavetableReleaseComplete(FVoices[AVoiceIndex].WavetableSynth);
    else
      Result := False;
  end;
end;

function TSedaiSynthesisEngine.GetVoiceInfo(AVoiceIndex: Integer): string;
var
  EngineNames: array[TSynthEngineType] of string;
begin
  EngineNames[setClassic] := 'Classic';
  EngineNames[setFM] := 'FM';
  EngineNames[setWavetable] := 'Wavetable';
  EngineNames[setHybrid] := 'Hybrid';

  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
  begin
    with FVoices[AVoiceIndex] do
    begin
      if IsActive then
        Result := Format('Voice %d: %s, %.1f Hz, Amp %.2f, Pan %s',
          [AVoiceIndex, EngineNames[EngineType], Frequency, Amplitude, PanToString(PanPosition)])
      else
        Result := Format('Voice %d: Inactive', [AVoiceIndex]);
    end;
  end
  else
    Result := 'Invalid voice index';
end;

function TSedaiSynthesisEngine.GetMaxVoices: Integer;
begin
  Result := FVoiceCount;  // oppure Length(FVoices)
end;

end.
