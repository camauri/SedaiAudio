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

unit SedaiAudioChip;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, ctypes,
  SedaiAudioTypes, SedaiSynthesisEngine, SedaiFilters,
  SedaiStereoProcessor, SedaiWavetableLoader, SDL2;

// MIDI Integration support - SIMPLIFIED
type
  TMIDIUpdateProc = procedure(ADeltaTimeSeconds: Single);

// Global MIDI callback - SIMPLIFIED
var
  GlobalMIDIUpdateProc: TMIDIUpdateProc = nil;

type
  TSedaiAudioChip = class
  private
    FDevice: TSDL_AudioDeviceID;
    FSpec: TSDL_AudioSpec;
    FIsInitialized: Boolean;
    FMasterVolume: Single;
    FSampleRate: Cardinal;

  public
    FSynthEngine: TSedaiSynthesisEngine; // Public for callback access

    constructor Create(AVoiceCount: Integer = 32);
    destructor Destroy; override;

    function Initialize: Boolean;
    procedure Shutdown;

    // Simplified interface
    procedure PlayNote(AFreq: Single; const APreset: string = 'sine';
                      ADurationMs: Integer = 1000);
    procedure PlayFMNote(AFreq: Single; const APreset: string = 'epiano';
                        ADurationMs: Integer = 1000);
    procedure PlayWavetableNote(AFreq: Single; const AWavetableType: string = 'serum';
                               ADurationMs: Integer = 1000);
    procedure PlayCustomWavetableNote(AFreq: Single; const ACustomWavetable: TWavetable;
                                     ADurationMs: Integer = 1000);

    // Advanced voice control
    function PlayNoteAdvanced(AFreq: Single; const APreset: string): Integer;
    function PlayFMNoteAdvanced(AFreq: Single; const APreset: string): Integer;
    function PlayWavetableNoteAdvanced(AFreq: Single; const AWavetableType: string): Integer;
    function PlayCustomWavetableNoteAdvanced(AFreq: Single; const ACustomWavetable: TWavetable): Integer;

    // Fixed voice control (no auto-allocation)
    procedure PlayOnVoice(AVoiceIndex: Integer; AFreq: Single; const APreset: string);
    procedure ReleaseVoice(AVoiceIndex: Integer);

    procedure NoteOff(AVoiceIndex: Integer);
    procedure RetriggerVoice(AVoiceIndex: Integer);
    procedure RetriggerVoiceHard(AVoiceIndex: Integer);  // SID-style hard reset for trills
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

    // System control
    procedure StopAll;
    function GetActiveVoices: Integer;
    function GetMaxVoices: Integer;
    procedure SetMasterVolume(AVolume: Single);
    function GetMasterVolume: Single;
    function GetSampleRate: Cardinal;

    // Status and debugging
    procedure PrintVoiceStatus;
    function GetVoiceInfo(AVoiceIndex: Integer): string;

    // MIDI integration - SIMPLIFIED
    procedure SetMIDIUpdateCallback(ACallback: TMIDIUpdateProc);
    procedure ClearMIDIUpdateCallback;
  end;

// MAIN AUDIO CALLBACK - COMPLETE INTEGRATION
procedure UnifiedAudioCallback(UserData: Pointer; Stream: pcuint8; Len: LongInt); cdecl;

implementation

// UNIFIED AUDIO CALLBACK - STEREO OUTPUT
procedure UnifiedAudioCallback(UserData: Pointer; Stream: pcuint8; Len: LongInt); cdecl;
var
  Chip: TSedaiAudioChip;
  FrameCount, i: Integer;
  LeftSample, RightSample: Single;
  LeftOutput, RightOutput: SmallInt;
  DeltaTime, TotalDeltaTime: Single;
  StereoBuffer: PSmallInt;
begin
  Chip := TSedaiAudioChip(UserData);
  if not Assigned(Chip) then
  begin
    // Fill with silence if no chip
    FillChar(Stream^, Len, 0);
    Exit;
  end;

  // Stereo: each frame = 2 samples (L + R), so frame count = len / 4
  FrameCount := Len div (2 * SizeOf(SmallInt));
  DeltaTime := 1.0 / Chip.FSampleRate;
  TotalDeltaTime := DeltaTime * FrameCount;
  StereoBuffer := PSmallInt(Stream);

  // STEP 1: UPDATE MIDI SEQUENCER FIRST (CRITICAL!)
  if Assigned(GlobalMIDIUpdateProc) then
  begin
    try
      GlobalMIDIUpdateProc(TotalDeltaTime);
    except
      on E: Exception do
      begin
        // Silent error handling in audio callback
      end;
    end;
  end;

  // STEP 2: GENERATE STEREO AUDIO SAMPLES
  try
    for i := 0 to FrameCount - 1 do
    begin
      // Generate stereo sample using synthesis engine
      Chip.FSynthEngine.ProcessAllVoicesStereo(Chip.FSampleRate, DeltaTime,
                                                LeftSample, RightSample);

      // Apply master volume
      LeftSample := LeftSample * Chip.FMasterVolume;
      RightSample := RightSample * Chip.FMasterVolume;

      // Hard clip to prevent damage
      if LeftSample > 1.0 then LeftSample := 1.0
      else if LeftSample < -1.0 then LeftSample := -1.0;
      if RightSample > 1.0 then RightSample := 1.0
      else if RightSample < -1.0 then RightSample := -1.0;

      // Convert to 16-bit stereo (interleaved L/R)
      LeftOutput := Round(LeftSample * 32767);
      RightOutput := Round(RightSample * 32767);
      StereoBuffer[i * 2] := LeftOutput;
      StereoBuffer[i * 2 + 1] := RightOutput;
    end;
  except
    on E: Exception do
    begin
      // Fill remainder with silence on error
      FillChar(Stream^, Len, 0);
    end;
  end;
end;

constructor TSedaiAudioChip.Create(AVoiceCount: Integer);
begin
  inherited Create;
  FSynthEngine := TSedaiSynthesisEngine.Create(AVoiceCount);
  FMasterVolume := 0.7;
  FIsInitialized := False;
  FSampleRate := 44100;
end;

destructor TSedaiAudioChip.Destroy;
begin
  Shutdown;
  FreeAndNil(FSynthEngine);
  inherited Destroy;
end;

// INITIALIZATION - CRITICAL FIX
function TSedaiAudioChip.Initialize: Boolean;
var
  DesiredSpec: TSDL_AudioSpec;
  i, ADeviceCount: Integer;
  ADeviceName: PChar;
begin
  Result := False;

  {$IFDEF SEDAI_DEBUG_AUDIO}
  WriteLn('DEBUG: Starting SDL Audio initialization...');
  {$ENDIF}

  if SDL_InitSubSystem(SDL_INIT_AUDIO) < 0 then
  begin
    WriteLn('ERROR: SDL Audio initialization failed: ', SDL_GetError);
    Exit;
  end;

  {$IFDEF SEDAI_DEBUG_AUDIO}
  WriteLn('DEBUG: SDL Audio subsystem initialized successfully');

  // List available audio devices
  ADeviceCount := SDL_GetNumAudioDevices(0);
  WriteLn('DEBUG: Found ', ADeviceCount, ' audio devices:');
  for i := 0 to ADeviceCount - 1 do
  begin
    ADeviceName := SDL_GetAudioDeviceName(i, 0);
    if ADeviceName <> nil then
      WriteLn('  Device ', i, ': ', ADeviceName)
    else
      WriteLn('  Device ', i, ': (unnamed)');
  end;
  {$ELSE}
  ADeviceCount := SDL_GetNumAudioDevices(0); // Suppress unused variable warning
  {$ENDIF}

  // Setup audio specification
  FillChar(DesiredSpec, SizeOf(DesiredSpec), 0);
  DesiredSpec.freq := FSampleRate;
  DesiredSpec.format := AUDIO_S16SYS;
  DesiredSpec.channels := 2; // Stereo
  DesiredSpec.samples := 1024;
  DesiredSpec.callback := @UnifiedAudioCallback; // FIXED: Use unified callback
  DesiredSpec.userdata := Self;

  {$IFDEF SEDAI_DEBUG_AUDIO}
  WriteLn('DEBUG: Opening audio device with unified callback...');
  {$ENDIF}

  FDevice := SDL_OpenAudioDevice(nil, 0, @DesiredSpec, @FSpec,
                                 SDL_AUDIO_ALLOW_FREQUENCY_CHANGE);
  if FDevice = 0 then
  begin
    WriteLn('ERROR: Audio device open failed: ', SDL_GetError);
    SDL_QuitSubSystem(SDL_INIT_AUDIO);
    Exit;
  end;

  {$IFDEF SEDAI_DEBUG_AUDIO}
  WriteLn('DEBUG: Audio device opened successfully, ID: ', FDevice);
  WriteLn('DEBUG: Actual audio spec:');
  WriteLn('  Frequency: ', FSpec.freq);
  WriteLn('  Format: ', FSpec.format);
  WriteLn('  Channels: ', FSpec.channels);
  WriteLn('  Samples: ', FSpec.samples);
  {$ENDIF}

  FSampleRate := FSpec.freq;
  FIsInitialized := True;

  {$IFDEF SEDAI_DEBUG_AUDIO}
  WriteLn('DEBUG: Starting audio playback...');
  {$ENDIF}
  SDL_PauseAudioDevice(FDevice, 0); // Start playing

  WriteLn('SEDAI Audio initialized with MIDI support: ', FSampleRate, ' Hz');
  {$IFDEF SEDAI_DEBUG_AUDIO}
  WriteLn('Unified audio callback active and running');
  {$ENDIF}

  Result := True;
end;

procedure TSedaiAudioChip.Shutdown;
begin
  if FDevice <> 0 then
  begin
    SDL_PauseAudioDevice(FDevice, 1);
    SDL_CloseAudioDevice(FDevice);
    FDevice := 0;
  end;

  if FIsInitialized then
  begin
    SDL_QuitSubSystem(SDL_INIT_AUDIO);
    FIsInitialized := False;
  end;
end;

// MIDI integration methods
procedure TSedaiAudioChip.SetMIDIUpdateCallback(ACallback: TMIDIUpdateProc);
begin
  GlobalMIDIUpdateProc := ACallback;
  WriteLn('MIDI callback registered with audio chip');
end;

procedure TSedaiAudioChip.ClearMIDIUpdateCallback;
begin
  GlobalMIDIUpdateProc := nil;
  WriteLn('MIDI callback cleared from audio chip');
end;

// Simplified playback methods
procedure TSedaiAudioChip.PlayNote(AFreq: Single; const APreset: string;
  ADurationMs: Integer);
var
  VoiceIndex: Integer;
begin
  if not FIsInitialized then
  begin
    WriteLn('WARNING: Audio not initialized');
    Exit;
  end;

  VoiceIndex := FSynthEngine.AllocateVoice;
  if VoiceIndex >= 0 then
    FSynthEngine.PlayClassic(VoiceIndex, AFreq, APreset)
  else
    WriteLn('WARNING: No free voices available');
end;

procedure TSedaiAudioChip.PlayFMNote(AFreq: Single; const APreset: string;
  ADurationMs: Integer);
var
  VoiceIndex: Integer;
begin
  if not FIsInitialized then
  begin
    WriteLn('WARNING: Audio not initialized');
    Exit;
  end;

  VoiceIndex := FSynthEngine.AllocateVoice;
  if VoiceIndex >= 0 then
    FSynthEngine.PlayFM(VoiceIndex, AFreq, APreset)
  else
    WriteLn('WARNING: No free voices available');
end;

procedure TSedaiAudioChip.PlayWavetableNote(AFreq: Single;
  const AWavetableType: string; ADurationMs: Integer);
var
  VoiceIndex: Integer;
  WTType: TWavetableType;
begin
  if not FIsInitialized then
  begin
    WriteLn('WARNING: Audio not initialized');
    Exit;
  end;

  // Convert string to wavetable type
  if AWavetableType = 'serum' then WTType := wtSerum
  else if AWavetableType = 'wasp' then WTType := wtWasp
  else if AWavetableType = 'ppg' then WTType := wtPPG
  else if AWavetableType = 'classic' then WTType := wtClassic
  else if AWavetableType = 'digital' then WTType := wtDigital
  else if AWavetableType = 'vocal' then WTType := wtVocal
  else WTType := wtSerum; // Default

  VoiceIndex := FSynthEngine.AllocateVoice;
  if VoiceIndex >= 0 then
    FSynthEngine.PlayWavetable(VoiceIndex, AFreq, WTType)
  else
    WriteLn('WARNING: No free voices available');
end;

procedure TSedaiAudioChip.PlayCustomWavetableNote(AFreq: Single;
  const ACustomWavetable: TWavetable; ADurationMs: Integer);
var
  VoiceIndex: Integer;
begin
  if not FIsInitialized then
  begin
    WriteLn('WARNING: Audio not initialized');
    Exit;
  end;

  if not ACustomWavetable.IsLoaded then
  begin
    WriteLn('WARNING: Custom wavetable not loaded, using default');
    PlayWavetableNote(AFreq, 'serum', ADurationMs);
    Exit;
  end;

  VoiceIndex := FSynthEngine.AllocateVoice;
  if VoiceIndex >= 0 then
    FSynthEngine.PlayWavetableCustom(VoiceIndex, AFreq, ACustomWavetable)
  else
    WriteLn('WARNING: No free voices available');
end;

// Advanced methods returning voice index
function TSedaiAudioChip.PlayNoteAdvanced(AFreq: Single; const APreset: string): Integer;
begin
  Result := -1;
  if not FIsInitialized then Exit;

  Result := FSynthEngine.AllocateVoice;
  if Result >= 0 then
    FSynthEngine.PlayClassic(Result, AFreq, APreset);
end;

function TSedaiAudioChip.PlayFMNoteAdvanced(AFreq: Single; const APreset: string): Integer;
begin
  Result := -1;
  if not FIsInitialized then Exit;

  Result := FSynthEngine.AllocateVoice;
  if Result >= 0 then
    FSynthEngine.PlayFM(Result, AFreq, APreset);
end;

function TSedaiAudioChip.PlayWavetableNoteAdvanced(AFreq: Single; const AWavetableType: string): Integer;
var
  WTType: TWavetableType;
begin
  Result := -1;
  if not FIsInitialized then Exit;

  // Convert string to enum
  if AWavetableType = 'serum' then WTType := wtSerum
  else if AWavetableType = 'wasp' then WTType := wtWasp
  else if AWavetableType = 'ppg' then WTType := wtPPG
  else WTType := wtSerum;

  Result := FSynthEngine.AllocateVoice;
  if Result >= 0 then
    FSynthEngine.PlayWavetable(Result, AFreq, WTType);
end;

function TSedaiAudioChip.PlayCustomWavetableNoteAdvanced(AFreq: Single;
  const ACustomWavetable: TWavetable): Integer;
begin
  Result := -1;
  if not FIsInitialized then Exit;

  if not ACustomWavetable.IsLoaded then
  begin
    WriteLn('WARNING: Custom wavetable not loaded');
    Exit;
  end;

  Result := FSynthEngine.AllocateVoice;
  if Result >= 0 then
    FSynthEngine.PlayWavetableCustom(Result, AFreq, ACustomWavetable);
end;

// Fixed voice control - plays on specific voice index (no auto-allocation)
procedure TSedaiAudioChip.PlayOnVoice(AVoiceIndex: Integer; AFreq: Single;
  const APreset: string);
begin
  if not FIsInitialized then Exit;
  if AVoiceIndex < 0 then Exit;

  // Play directly on the specified voice
  FSynthEngine.PlayClassic(AVoiceIndex, AFreq, APreset);
end;

procedure TSedaiAudioChip.ReleaseVoice(AVoiceIndex: Integer);
begin
  if FIsInitialized then
    FSynthEngine.ReleaseVoice(AVoiceIndex);
end;

// Voice control
procedure TSedaiAudioChip.NoteOff(AVoiceIndex: Integer);
begin
  if FIsInitialized then
    FSynthEngine.NoteOff(AVoiceIndex);
end;

procedure TSedaiAudioChip.RetriggerVoice(AVoiceIndex: Integer);
begin
  if FIsInitialized then
    FSynthEngine.RetriggerVoice(AVoiceIndex);
end;

procedure TSedaiAudioChip.RetriggerVoiceHard(AVoiceIndex: Integer);
begin
  if FIsInitialized then
    FSynthEngine.RetriggerVoiceHard(AVoiceIndex);
end;

procedure TSedaiAudioChip.SetVoicePan(AVoiceIndex: Integer; APan: TPanPosition);
begin
  if FIsInitialized then
    FSynthEngine.SetVoicePan(AVoiceIndex, APan);
end;

procedure TSedaiAudioChip.SetVoiceFrequency(AVoiceIndex: Integer; AFreq: Single);
begin
  if FIsInitialized then
    FSynthEngine.SetVoiceFrequency(AVoiceIndex, AFreq);
end;

procedure TSedaiAudioChip.SetVoiceADSR(AVoiceIndex: Integer;
  AAttack, ADecay, ASustain, ARelease: Single);
begin
  if FIsInitialized then
    FSynthEngine.SetVoiceADSR(AVoiceIndex, AAttack, ADecay, ASustain, ARelease);
end;

procedure TSedaiAudioChip.SetVoicePulseWidth(AVoiceIndex: Integer; APulseWidth: Single);
begin
  if FIsInitialized then
    FSynthEngine.SetVoicePulseWidth(AVoiceIndex, APulseWidth);
end;

// Per-voice filter control
procedure TSedaiAudioChip.SetVoiceFilter(AVoiceIndex: Integer; AEnabled: Boolean;
                                         AFilterType: TFilterType; AFreq, AQ: Single;
                                         ASlope: TFilterSlope);
begin
  if FIsInitialized then
    FSynthEngine.SetVoiceFilter(AVoiceIndex, AEnabled, AFilterType, AFreq, AQ, ASlope);
end;

procedure TSedaiAudioChip.SetVoiceFilterEnabled(AVoiceIndex: Integer; AEnabled: Boolean);
begin
  if FIsInitialized then
    FSynthEngine.SetVoiceFilterEnabled(AVoiceIndex, AEnabled);
end;

procedure TSedaiAudioChip.SetVoiceFilterParams(AVoiceIndex: Integer; AFreq, AQ: Single);
begin
  if FIsInitialized then
    FSynthEngine.SetVoiceFilterParams(AVoiceIndex, AFreq, AQ);
end;

// System control
procedure TSedaiAudioChip.StopAll;
begin
  if FIsInitialized then
    FSynthEngine.ReleaseAllVoices;
end;

function TSedaiAudioChip.GetActiveVoices: Integer;
begin
  if FIsInitialized then
    Result := FSynthEngine.GetActiveVoiceCount
  else
    Result := 0;
end;

function TSedaiAudioChip.GetMaxVoices: Integer;
begin
  if Assigned(FSynthEngine) then
    Result := FSynthEngine.GetMaxVoices
  else
    Result := 0;
end;

procedure TSedaiAudioChip.SetMasterVolume(AVolume: Single);
begin
  if AVolume < 0.0 then AVolume := 0.0
  else if AVolume > 1.0 then AVolume := 1.0;
  FMasterVolume := AVolume;
end;

function TSedaiAudioChip.GetMasterVolume: Single;
begin
  Result := FMasterVolume;
end;

function TSedaiAudioChip.GetSampleRate: Cardinal;
begin
  Result := FSampleRate;
end;

// Status and debugging
procedure TSedaiAudioChip.PrintVoiceStatus;
var
  i: Integer;
  ActiveCount: Integer;
begin
  ActiveCount := GetActiveVoices;
  WriteLn('=== SEDAI Voice Status ===');
  WriteLn('Active voices: ', ActiveCount, '/', GetMaxVoices);
  WriteLn('Master volume: ', Format('%.2f', [FMasterVolume]));
  WriteLn('Sample rate: ', FSampleRate, ' Hz');
  WriteLn('MIDI Integration: ', BoolToStr(Assigned(GlobalMIDIUpdateProc), True));

  if ActiveCount > 0 then
  begin
    WriteLn('Active voice details:');
    for i := 0 to GetMaxVoices - 1 do
    begin
      if FSynthEngine.IsVoiceActive(i) then
        WriteLn('  ', GetVoiceInfo(i));
    end;
  end;
  WriteLn('========================');
end;

function TSedaiAudioChip.GetVoiceInfo(AVoiceIndex: Integer): string;
begin
  if FIsInitialized then
    Result := FSynthEngine.GetVoiceInfo(AVoiceIndex)
  else
    Result := 'Audio not initialized';
end;

end.
