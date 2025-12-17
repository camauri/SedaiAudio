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
  SedaiWavetableLoader, SedaiFilters;

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

    // Per-voice filter
    FilterEnabled: Boolean;
    Filter: TMultiPoleFilter;
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
    procedure ProcessAllVoicesStereo(ASampleRate: Single; ADeltaTime: Single;
                                      out ALeftSample, ARightSample: Single);

    // Voice control
    procedure NoteOn(AVoiceIndex: Integer);
    procedure NoteOff(AVoiceIndex: Integer);
    procedure RetriggerVoice(AVoiceIndex: Integer);      // Re-trigger attack without reinitializing preset
    procedure RetriggerVoiceHard(AVoiceIndex: Integer);  // Hard reset ADSR to zero (SID-style for trills)
    procedure SetVoicePan(AVoiceIndex: Integer; APan: TPanPosition);
    procedure SetVoiceFrequency(AVoiceIndex: Integer; AFreq: Single);
    procedure SetVoiceADSR(AVoiceIndex: Integer; AAttack, ADecay, ASustain, ARelease: Single);
    procedure SetVoicePulseWidth(AVoiceIndex: Integer; APulseWidth: Single);

    // Per-voice filter control
    procedure SetVoiceFilter(AVoiceIndex: Integer; AEnabled: Boolean;
                            AFilterType: TFilterType; AFreq, AQ: Single;
                            ASlope: TFilterSlope);
    procedure SetVoiceFilterEnabled(AVoiceIndex: Integer; AEnabled: Boolean);
    procedure SetVoiceFilterParams(AVoiceIndex: Integer; AFreq, AQ: Single);
    procedure ResetVoiceFilter(AVoiceIndex: Integer);

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

    // Initialize per-voice filter (disabled by default)
    FVoices[i].FilterEnabled := False;
    FVoices[i].Filter := TSedaiFilters.CreateMultiPoleFilter(ftLowPass, 10000.0, 0.707, fs24dB, 44100);
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

    // Apply per-voice filter if enabled
    if FilterEnabled then
    begin
      Result := TSedaiFilters.ProcessMultiPoleSample(Filter, Result);
      // DEBUG: Verify filter is being applied (check first stage freq)
      // WriteLn('Filter applied: Freq=', Filter.Stages[0].Frequency:0:1, ' Q=', Filter.Stages[0].Q:0:2);
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

// Process all voices with stereo panning
procedure TSedaiSynthesisEngine.ProcessAllVoicesStereo(ASampleRate: Single;
  ADeltaTime: Single; out ALeftSample, ARightSample: Single);
var
  i: Integer;
  VoiceSample: Single;
  APan, ALeftGain, ARightGain: Single;
begin
  ALeftSample := 0.0;
  ARightSample := 0.0;

  for i := 0 to FVoiceCount - 1 do
  begin
    if FVoices[i].IsActive then
    begin
      VoiceSample := ProcessVoice(i, ASampleRate, ADeltaTime);

      // Apply constant-power panning
      // Pan: -1.0 = full left, 0.0 = center, 1.0 = full right
      APan := FVoices[i].PanPosition;
      // Convert linear pan to equal-power pan (constant power)
      // Left = cos(pan * pi/4 + pi/4), Right = sin(pan * pi/4 + pi/4)
      // Simplified: Left = sqrt((1 - pan) / 2), Right = sqrt((1 + pan) / 2)
      ALeftGain := Sqrt((1.0 - APan) * 0.5);
      ARightGain := Sqrt((1.0 + APan) * 0.5);

      ALeftSample := ALeftSample + VoiceSample * ALeftGain;
      ARightSample := ARightSample + VoiceSample * ARightGain;
    end;
  end;

  // Simple normalization to prevent clipping
  if ALeftSample > 1.0 then ALeftSample := 1.0
  else if ALeftSample < -1.0 then ALeftSample := -1.0;
  if ARightSample > 1.0 then ARightSample := 1.0
  else if ARightSample < -1.0 then ARightSample := -1.0;
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

procedure TSedaiSynthesisEngine.RetriggerVoice(AVoiceIndex: Integer);
begin
  // Re-trigger the attack phase without reinitializing the preset (soft)
  // Preserves current level to avoid clicks
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
  begin
    case FVoices[AVoiceIndex].EngineType of
      setClassic: TSedaiClassicProcessor.StartClassicAttack(FVoices[AVoiceIndex].ClassicSynth);
      setFM: TSedaiFMProcessor.StartFMAttack(FVoices[AVoiceIndex].FMSynth);
      setWavetable: TSedaiWavetableProcessor.StartWavetableAttack(FVoices[AVoiceIndex].WavetableSynth);
    end;
    FVoices[AVoiceIndex].IsActive := True;
  end;
end;

procedure TSedaiSynthesisEngine.RetriggerVoiceHard(AVoiceIndex: Integer);
begin
  // Re-trigger with hard reset - always resets ADSR level to zero
  // This is the authentic SID behavior needed for trills and arpeggios
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
  begin
    case FVoices[AVoiceIndex].EngineType of
      setClassic: TSedaiClassicProcessor.StartClassicAttackHard(FVoices[AVoiceIndex].ClassicSynth);
      setFM: TSedaiFMProcessor.StartFMAttack(FVoices[AVoiceIndex].FMSynth);  // TODO: Add hard version for FM
      setWavetable: TSedaiWavetableProcessor.StartWavetableAttack(FVoices[AVoiceIndex].WavetableSynth);  // TODO: Add hard version
    end;
    FVoices[AVoiceIndex].IsActive := True;
  end;
end;

procedure TSedaiSynthesisEngine.SetVoicePan(AVoiceIndex: Integer; APan: TPanPosition);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
    FVoices[AVoiceIndex].PanPosition := ClampPan(APan);
end;

procedure TSedaiSynthesisEngine.SetVoiceFrequency(AVoiceIndex: Integer; AFreq: Single);
begin
  if (AVoiceIndex >= 0) and (AVoiceIndex < FVoiceCount) then
    FVoices[AVoiceIndex].Frequency := AFreq;
end;

procedure TSedaiSynthesisEngine.SetVoiceADSR(AVoiceIndex: Integer;
  AAttack, ADecay, ASustain, ARelease: Single);
var
  i: Integer;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= FVoiceCount) then Exit;

  // Update ADSR for all oscillators in the classic synth
  with FVoices[AVoiceIndex] do
  begin
    if EngineType = setClassic then
    begin
      for i := 0 to High(ClassicSynth.Oscillators) do
      begin
        ClassicSynth.Oscillators[i].ADSR.Attack := AAttack;
        ClassicSynth.Oscillators[i].ADSR.Decay := ADecay;
        ClassicSynth.Oscillators[i].ADSR.Sustain := ASustain;
        ClassicSynth.Oscillators[i].ADSR.Release := ARelease;
      end;
    end;
  end;
end;

procedure TSedaiSynthesisEngine.SetVoicePulseWidth(AVoiceIndex: Integer; APulseWidth: Single);
var
  i: Integer;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= FVoiceCount) then Exit;

  // Clamp pulse width
  if APulseWidth < 0.01 then APulseWidth := 0.01;
  if APulseWidth > 0.99 then APulseWidth := 0.99;

  // Update pulse width for all oscillators in the classic synth
  with FVoices[AVoiceIndex] do
  begin
    if EngineType = setClassic then
    begin
      for i := 0 to High(ClassicSynth.Oscillators) do
        ClassicSynth.Oscillators[i].PulseWidth := APulseWidth;
    end;
  end;
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

// ============================================================================
// PER-VOICE FILTER CONTROL
// ============================================================================

procedure TSedaiSynthesisEngine.SetVoiceFilter(AVoiceIndex: Integer; AEnabled: Boolean;
                                               AFilterType: TFilterType; AFreq, AQ: Single;
                                               ASlope: TFilterSlope);
var
  AOldFilter: TMultiPoleFilter;
  i: Integer;
  APreserveState: Boolean;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= FVoiceCount) then Exit;

  with FVoices[AVoiceIndex] do
  begin
    // Check if we should preserve filter state (same type and slope)
    APreserveState := FilterEnabled and
                      (Filter.FilterType = AFilterType) and
                      (Filter.NumStages > 0);

    if APreserveState then
      AOldFilter := Filter;

    FilterEnabled := AEnabled;
    if AEnabled then
    begin
      // Create new filter with specified parameters
      Filter := TSedaiFilters.CreateMultiPoleFilter(AFilterType, AFreq, AQ, ASlope, 44100);

      // Preserve filter state to avoid clicks when updating parameters
      if APreserveState and (AOldFilter.NumStages = Filter.NumStages) then
      begin
        for i := 0 to Filter.NumStages - 1 do
          Filter.Stages[i].State := AOldFilter.Stages[i].State;
      end;
    end;
  end;
end;

procedure TSedaiSynthesisEngine.SetVoiceFilterEnabled(AVoiceIndex: Integer; AEnabled: Boolean);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= FVoiceCount) then Exit;
  FVoices[AVoiceIndex].FilterEnabled := AEnabled;
end;

procedure TSedaiSynthesisEngine.SetVoiceFilterParams(AVoiceIndex: Integer; AFreq, AQ: Single);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= FVoiceCount) then Exit;

  with FVoices[AVoiceIndex] do
  begin
    if FilterEnabled then
      TSedaiFilters.UpdateMultiPoleFilter(Filter, AFreq, AQ);
  end;
end;

procedure TSedaiSynthesisEngine.ResetVoiceFilter(AVoiceIndex: Integer);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= FVoiceCount) then Exit;

  with FVoices[AVoiceIndex] do
  begin
    if FilterEnabled then
      TSedaiFilters.ResetMultiPoleFilter(Filter);
  end;
end;

end.
