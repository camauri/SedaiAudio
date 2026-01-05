{*
 * Sedai Audio Foundation - Audio Engine
 *
 * This unit provides SDL2-based audio output and voice management.
 * It bridges the modular synthesis components with real-time audio playback.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAudioEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SDL2,
  SedaiAudioTypes, SedaiOscillator, SedaiEnvelope, SedaiFilter,
  SedaiVoice, SedaiSIDEvo;

const
  ENGINE_MAX_VOICES = 64;
  ENGINE_DEFAULT_SAMPLE_RATE = 44100;
  ENGINE_DEFAULT_BUFFER_SIZE = 1024;

type
  // Voice state for the engine
  TEngineVoice = record
    Voice: TSedaiVoice;
    Active: Boolean;
    ReleaseTime: QWord;  // Tick count when released
  end;

var
  // Global engine state
  GEngineInitialized: Boolean = False;
  GEngineSampleRate: Cardinal = ENGINE_DEFAULT_SAMPLE_RATE;
  GEngineBufferSize: Cardinal = ENGINE_DEFAULT_BUFFER_SIZE;
  GEngineMasterVolume: Single = 0.7;
  GEngineVoices: array[0..ENGINE_MAX_VOICES-1] of TEngineVoice;
  GEngineActiveVoices: Integer = 0;
  GEngineSID: TSedaiSIDEvo = nil;
  GEngineAudioDevice: TSDL_AudioDeviceID = 0;

// ============================================================================
// INITIALIZATION
// ============================================================================

function InitAudio(AMaxVoices: Integer = ENGINE_MAX_VOICES): Boolean;
procedure ShutdownAudio;
function GetSampleRate: Cardinal;
function GetActiveVoices: Integer;
function GetMaxVoices: Integer;

// ============================================================================
// VOLUME CONTROL
// ============================================================================

procedure SetMasterVolume(AVolume: Single);
function GetMasterVolume: Single;

// ============================================================================
// VOICE MANAGEMENT
// ============================================================================

procedure StopAll;
procedure SmoothStopAll(AFadeTimeMs: Integer = 100);

// ============================================================================
// BASIC WAVEFORM PLAYBACK
// ============================================================================

procedure PlaySine(AFrequency: Single; AVelocity: Single = 0.8);
procedure PlaySquare(AFrequency: Single; AVelocity: Single = 0.8);
procedure PlaySaw(AFrequency: Single; AVelocity: Single = 0.8);
procedure PlayTriangle(AFrequency: Single; AVelocity: Single = 0.8);
procedure PlayPulse(AFrequency: Single; APulseWidth: Single = 0.5; AVelocity: Single = 0.8);

// ============================================================================
// PRESET PLAYBACK (Classic subtractive synthesis)
// ============================================================================

procedure PlayClassic(AFrequency: Single; const APreset: string);
procedure PlayLead(AFrequency: Single);
procedure PlayBass(AFrequency: Single);
procedure PlayPad(AFrequency: Single);

// ============================================================================
// FM SYNTHESIS
// ============================================================================

procedure PlayFM(AFrequency: Single; const APreset: string);
procedure PlayEPiano(AFrequency: Single);
procedure PlayFMBrass(AFrequency: Single);
procedure PlayFMBell(AFrequency: Single);
procedure PlayFMOrgan(AFrequency: Single);
procedure PlayFMLead(AFrequency: Single);
procedure PlayFMBass(AFrequency: Single);

// ============================================================================
// WAVETABLE SYNTHESIS
// ============================================================================

procedure PlayWavetable(AFrequency: Single; const APreset: string);
procedure PlaySerum(AFrequency: Single);
procedure PlayWasp(AFrequency: Single);
procedure PlayPPG(AFrequency: Single);

// ============================================================================
// CHORDS AND SCALES
// ============================================================================

procedure PlayChordClassic(ANote1, ANote2, ANote3: Single; const APreset: string);
procedure PlayChordFM(ANote1, ANote2, ANote3: Single; const APreset: string);
procedure PlayChordWavetable(ANote1, ANote2, ANote3: Single; const APreset: string);
procedure PlayScaleClassic(ABaseFreq: Single; const APreset: string);
procedure PlayScaleFM(ABaseFreq: Single; const APreset: string);
procedure PlayScaleWavetable(ABaseFreq: Single; const APreset: string);

// ============================================================================
// SID CHIP
// ============================================================================

function GetSID: TSedaiSIDEvo;

// ============================================================================
// STATUS
// ============================================================================

procedure PrintStatus;

// ============================================================================
// MIDI NOTE FREQUENCIES
// ============================================================================

const
  NOTE_C4 = 261.63;
  NOTE_D4 = 293.66;
  NOTE_E4 = 329.63;
  NOTE_F4 = 349.23;
  NOTE_G4 = 392.00;
  NOTE_A4 = 440.00;
  NOTE_B4 = 493.88;
  NOTE_C5 = 523.25;

implementation

var
  GAudioSpec: TSDL_AudioSpec;
  GVoiceLock: TRTLCriticalSection;

// ============================================================================
// SDL AUDIO CALLBACK
// ============================================================================

procedure AudioCallback(AUserdata: Pointer; AStream: PUInt8; ALen: Integer); cdecl;
var
  I, J: Integer;
  SampleCount: Integer;
  Buffer: PSingle;
  Sample: Single;
  MixBuffer: array[0..ENGINE_DEFAULT_BUFFER_SIZE*2-1] of Single;
  VoiceBuffer: array[0..ENGINE_DEFAULT_BUFFER_SIZE*2-1] of Single;
begin
  Buffer := PSingle(AStream);
  SampleCount := ALen div (SizeOf(Single) * 2);  // Stereo

  // Clear mix buffer
  FillChar(MixBuffer[0], SizeOf(MixBuffer), 0);

  EnterCriticalSection(GVoiceLock);
  try
    // Mix all active voices
    for I := 0 to ENGINE_MAX_VOICES - 1 do
    begin
      if GEngineVoices[I].Active and Assigned(GEngineVoices[I].Voice) then
      begin
        // Generate samples from voice using ProcessBlock
        FillChar(VoiceBuffer[0], SampleCount * 2 * SizeOf(Single), 0);
        GEngineVoices[I].Voice.ProcessBlock(nil, @VoiceBuffer[0], SampleCount);

        // Mix into main buffer
        for J := 0 to SampleCount * 2 - 1 do
          MixBuffer[J] := MixBuffer[J] + VoiceBuffer[J];

        // Check if voice finished using IsActive function
        if not GEngineVoices[I].Voice.IsActive then
        begin
          GEngineVoices[I].Voice.Free;
          GEngineVoices[I].Voice := nil;
          GEngineVoices[I].Active := False;
          Dec(GEngineActiveVoices);
        end;
      end;
    end;

    // Mix SID if available using GenerateSample
    if Assigned(GEngineSID) then
    begin
      for J := 0 to SampleCount - 1 do
      begin
        Sample := GEngineSID.GenerateSample;
        MixBuffer[J * 2] := MixBuffer[J * 2] + Sample;
        MixBuffer[J * 2 + 1] := MixBuffer[J * 2 + 1] + Sample;
      end;
    end;
  finally
    LeaveCriticalSection(GVoiceLock);
  end;

  // Apply master volume and copy to output
  for J := 0 to SampleCount * 2 - 1 do
  begin
    Sample := MixBuffer[J] * GEngineMasterVolume;
    // Soft clip
    if Sample > 1.0 then
      Sample := 1.0
    else if Sample < -1.0 then
      Sample := -1.0;
    Buffer[J] := Sample;
  end;
end;

// ============================================================================
// INITIALIZATION
// ============================================================================

function InitAudio(AMaxVoices: Integer): Boolean;
var
  DesiredSpec: TSDL_AudioSpec;
  I: Integer;
begin
  Result := False;

  if GEngineInitialized then
  begin
    Result := True;
    Exit;
  end;

  // Initialize critical section
  InitCriticalSection(GVoiceLock);

  // Initialize SDL audio
  if SDL_Init(SDL_INIT_AUDIO or SDL_INIT_TIMER) < 0 then
  begin
    WriteLn('SDL_Init error: ', SDL_GetError);
    Exit;
  end;

  // Setup audio spec
  FillChar(DesiredSpec, SizeOf(DesiredSpec), 0);
  DesiredSpec.freq := ENGINE_DEFAULT_SAMPLE_RATE;
  DesiredSpec.format := AUDIO_F32SYS;
  DesiredSpec.channels := 2;
  DesiredSpec.samples := ENGINE_DEFAULT_BUFFER_SIZE;
  DesiredSpec.callback := @AudioCallback;
  DesiredSpec.userdata := nil;

  // Open audio device
  GEngineAudioDevice := SDL_OpenAudioDevice(nil, 0, @DesiredSpec, @GAudioSpec, 0);
  if GEngineAudioDevice = 0 then
  begin
    WriteLn('SDL_OpenAudioDevice error: ', SDL_GetError);
    SDL_Quit;
    Exit;
  end;

  GEngineSampleRate := GAudioSpec.freq;
  GEngineBufferSize := GAudioSpec.samples;

  // Initialize voice slots
  for I := 0 to ENGINE_MAX_VOICES - 1 do
  begin
    GEngineVoices[I].Voice := nil;
    GEngineVoices[I].Active := False;
  end;
  GEngineActiveVoices := 0;

  // Create SID chip
  GEngineSID := TSedaiSIDEvo.Create;
  GEngineSID.SetSampleRate(GEngineSampleRate);

  // Start audio
  SDL_PauseAudioDevice(GEngineAudioDevice, 0);

  GEngineInitialized := True;
  Result := True;
end;

procedure ShutdownAudio;
var
  I: Integer;
begin
  if not GEngineInitialized then
    Exit;

  // Stop audio
  SDL_PauseAudioDevice(GEngineAudioDevice, 1);
  SDL_CloseAudioDevice(GEngineAudioDevice);

  // Free voices
  EnterCriticalSection(GVoiceLock);
  try
    for I := 0 to ENGINE_MAX_VOICES - 1 do
    begin
      if Assigned(GEngineVoices[I].Voice) then
      begin
        GEngineVoices[I].Voice.Free;
        GEngineVoices[I].Voice := nil;
      end;
      GEngineVoices[I].Active := False;
    end;
    GEngineActiveVoices := 0;

    // Free SID
    if Assigned(GEngineSID) then
    begin
      GEngineSID.Free;
      GEngineSID := nil;
    end;
  finally
    LeaveCriticalSection(GVoiceLock);
  end;

  DoneCriticalSection(GVoiceLock);
  SDL_Quit;

  GEngineInitialized := False;
end;

function GetSampleRate: Cardinal;
begin
  Result := GEngineSampleRate;
end;

function GetActiveVoices: Integer;
begin
  Result := GEngineActiveVoices;
end;

function GetMaxVoices: Integer;
begin
  Result := ENGINE_MAX_VOICES;
end;

// ============================================================================
// VOLUME CONTROL
// ============================================================================

procedure SetMasterVolume(AVolume: Single);
begin
  if AVolume < 0.0 then AVolume := 0.0;
  if AVolume > 1.0 then AVolume := 1.0;
  GEngineMasterVolume := AVolume;
end;

function GetMasterVolume: Single;
begin
  Result := GEngineMasterVolume;
end;

// ============================================================================
// VOICE MANAGEMENT
// ============================================================================

function FindFreeVoice: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ENGINE_MAX_VOICES - 1 do
  begin
    if not GEngineVoices[I].Active then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function AllocateVoice: TSedaiVoice;
var
  Slot: Integer;
begin
  Result := nil;

  EnterCriticalSection(GVoiceLock);
  try
    Slot := FindFreeVoice;
    if Slot < 0 then
      Exit;

    Result := TSedaiVoice.Create;
    Result.SetSampleRate(GEngineSampleRate);
    GEngineVoices[Slot].Voice := Result;
    GEngineVoices[Slot].Active := True;
    Inc(GEngineActiveVoices);
  finally
    LeaveCriticalSection(GVoiceLock);
  end;
end;

procedure StopAll;
var
  I: Integer;
begin
  EnterCriticalSection(GVoiceLock);
  try
    for I := 0 to ENGINE_MAX_VOICES - 1 do
    begin
      if GEngineVoices[I].Active and Assigned(GEngineVoices[I].Voice) then
      begin
        GEngineVoices[I].Voice.NoteOff;
      end;
    end;
  finally
    LeaveCriticalSection(GVoiceLock);
  end;
end;

procedure SmoothStopAll(AFadeTimeMs: Integer);
begin
  StopAll;
  SDL_Delay(AFadeTimeMs);
end;

// ============================================================================
// BASIC WAVEFORM PLAYBACK
// ============================================================================

procedure PlayNote(AWaveform: TWaveformType; AFrequency: Single; AVelocity: Single);
var
  Voice: TSedaiVoice;
  MidiNote: Byte;
begin
  Voice := AllocateVoice;
  if Voice = nil then Exit;

  // Use SetOscillatorWaveform method instead of direct property access
  Voice.SetOscillatorWaveform(0, AWaveform);
  Voice.SetOscillatorEnabled(0, True);

  // Set oscillator frequency via the indexed property
  Voice.Oscillators[0].Frequency := AFrequency;

  // Convert frequency to MIDI note (approximate)
  MidiNote := Round(69 + 12 * Log2(AFrequency / 440.0));
  if MidiNote > 127 then MidiNote := 127;

  Voice.NoteOn(MidiNote, AVelocity);
end;

procedure PlaySine(AFrequency: Single; AVelocity: Single);
begin
  PlayNote(wtSine, AFrequency, AVelocity);
end;

procedure PlaySquare(AFrequency: Single; AVelocity: Single);
begin
  PlayNote(wtSquare, AFrequency, AVelocity);
end;

procedure PlaySaw(AFrequency: Single; AVelocity: Single);
begin
  PlayNote(wtSawtooth, AFrequency, AVelocity);
end;

procedure PlayTriangle(AFrequency: Single; AVelocity: Single);
begin
  PlayNote(wtTriangle, AFrequency, AVelocity);
end;

procedure PlayPulse(AFrequency: Single; APulseWidth: Single; AVelocity: Single);
var
  Voice: TSedaiVoice;
  MidiNote: Byte;
begin
  Voice := AllocateVoice;
  if Voice = nil then Exit;

  Voice.SetOscillatorWaveform(0, wtPulse);
  Voice.SetOscillatorEnabled(0, True);
  Voice.SetOscillatorPulseWidth(0, APulseWidth);
  Voice.Oscillators[0].Frequency := AFrequency;

  MidiNote := Round(69 + 12 * Log2(AFrequency / 440.0));
  if MidiNote > 127 then MidiNote := 127;

  Voice.NoteOn(MidiNote, AVelocity);
end;

// ============================================================================
// PRESET PLAYBACK
// ============================================================================

procedure ApplyPreset(AVoice: TSedaiVoice; const APreset: string);
var
  PresetLower: string;
begin
  PresetLower := LowerCase(APreset);

  // Default settings using the proper API methods
  AVoice.SetOscillatorWaveform(0, wtSawtooth);
  AVoice.SetOscillatorEnabled(0, True);
  AVoice.SetFilterType(ftLowPass);
  AVoice.SetFilterCutoff(2000.0);
  AVoice.SetFilterResonance(0.2);
  AVoice.SetEnvelopeADSR(0, 0.01, 0.1, 0.7, 0.3);

  if PresetLower = 'strings' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSawtooth);
    AVoice.SetFilterCutoff(3000.0);
    AVoice.SetEnvelopeADSR(0, 0.3, 0.2, 0.8, 0.5);
  end
  else if PresetLower = 'brass' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSawtooth);
    AVoice.SetFilterCutoff(1500.0);
    AVoice.SetFilterResonance(0.5);
    AVoice.SetEnvelopeADSR(0, 0.05, 0.1, 0.9, 0.2);
  end
  else if PresetLower = 'organ' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSquare);
    AVoice.SetFilterCutoff(5000.0);
    AVoice.SetEnvelopeADSR(0, 0.005, 0.0, 1.0, 0.05);
  end
  else if PresetLower = 'pluck' then
  begin
    AVoice.SetOscillatorWaveform(0, wtTriangle);
    AVoice.SetFilterCutoff(4000.0);
    AVoice.SetEnvelopeADSR(0, 0.001, 0.3, 0.0, 0.1);
  end
  else if PresetLower = 'lead' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSawtooth);
    AVoice.SetFilterCutoff(2500.0);
    AVoice.SetFilterResonance(0.6);
    AVoice.SetEnvelopeADSR(0, 0.01, 0.1, 0.8, 0.2);
  end
  else if PresetLower = 'bass' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSquare);
    AVoice.SetFilterCutoff(800.0);
    AVoice.SetFilterResonance(0.5);
    AVoice.SetEnvelopeADSR(0, 0.01, 0.2, 0.6, 0.15);
  end
  else if PresetLower = 'pad' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSawtooth);
    AVoice.SetFilterCutoff(1500.0);
    AVoice.SetEnvelopeADSR(0, 0.5, 0.5, 0.7, 1.0);
  end
  else if PresetLower = 'sine' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSine);
    AVoice.SetFilterCutoff(10000.0);
  end;
end;

procedure PlayClassic(AFrequency: Single; const APreset: string);
var
  Voice: TSedaiVoice;
  MidiNote: Byte;
begin
  Voice := AllocateVoice;
  if Voice = nil then Exit;

  ApplyPreset(Voice, APreset);
  Voice.Oscillators[0].Frequency := AFrequency;

  MidiNote := Round(69 + 12 * Log2(AFrequency / 440.0));
  if MidiNote > 127 then MidiNote := 127;

  Voice.NoteOn(MidiNote, 0.8);
end;

procedure PlayLead(AFrequency: Single);
begin
  PlayClassic(AFrequency, 'lead');
end;

procedure PlayBass(AFrequency: Single);
begin
  PlayClassic(AFrequency, 'bass');
end;

procedure PlayPad(AFrequency: Single);
begin
  PlayClassic(AFrequency, 'pad');
end;

// ============================================================================
// FM SYNTHESIS (simplified - using basic oscillator with harmonics)
// ============================================================================

procedure PlayFM(AFrequency: Single; const APreset: string);
var
  Voice: TSedaiVoice;
  PresetLower: string;
  MidiNote: Byte;
begin
  Voice := AllocateVoice;
  if Voice = nil then Exit;

  PresetLower := LowerCase(APreset);

  // For now, use approximations with the oscillator
  // TODO: Implement proper FM synthesis with TSedaiFMOperator
  Voice.SetOscillatorEnabled(0, True);
  Voice.Oscillators[0].Frequency := AFrequency;

  if PresetLower = 'epiano' then
  begin
    Voice.SetOscillatorWaveform(0, wtSine);
    Voice.SetFilterCutoff(3000.0);
    Voice.SetEnvelopeADSR(0, 0.001, 0.8, 0.3, 0.5);
  end
  else if PresetLower = 'brass' then
  begin
    Voice.SetOscillatorWaveform(0, wtSawtooth);
    Voice.SetFilterCutoff(2000.0);
    Voice.SetEnvelopeADSR(0, 0.05, 0.1, 0.9, 0.2);
  end
  else if PresetLower = 'bell' then
  begin
    Voice.SetOscillatorWaveform(0, wtSine);
    Voice.SetFilterCutoff(5000.0);
    Voice.SetEnvelopeADSR(0, 0.001, 2.0, 0.0, 0.5);
  end
  else if PresetLower = 'organ' then
  begin
    Voice.SetOscillatorWaveform(0, wtSquare);
    Voice.SetFilterCutoff(4000.0);
    Voice.SetEnvelopeADSR(0, 0.005, 0.0, 1.0, 0.05);
  end
  else if PresetLower = 'lead' then
  begin
    Voice.SetOscillatorWaveform(0, wtSawtooth);
    Voice.SetFilterCutoff(3000.0);
    Voice.SetEnvelopeADSR(0, 0.01, 0.1, 0.8, 0.2);
  end
  else if PresetLower = 'bass' then
  begin
    Voice.SetOscillatorWaveform(0, wtSquare);
    Voice.SetFilterCutoff(600.0);
    Voice.SetEnvelopeADSR(0, 0.01, 0.2, 0.5, 0.1);
  end
  else if (PresetLower = 'choir') or (PresetLower = 'marimba') or (PresetLower = 'churchbell') then
  begin
    Voice.SetOscillatorWaveform(0, wtSine);
    Voice.SetFilterCutoff(4000.0);
    Voice.SetEnvelopeADSR(0, 0.01, 1.0, 0.4, 0.5);
  end
  else
  begin
    Voice.SetOscillatorWaveform(0, wtSine);
    Voice.SetEnvelopeADSR(0, 0.01, 0.5, 0.5, 0.3);
  end;

  MidiNote := Round(69 + 12 * Log2(AFrequency / 440.0));
  if MidiNote > 127 then MidiNote := 127;

  Voice.NoteOn(MidiNote, 0.8);
end;

procedure PlayEPiano(AFrequency: Single);
begin
  PlayFM(AFrequency, 'epiano');
end;

procedure PlayFMBrass(AFrequency: Single);
begin
  PlayFM(AFrequency, 'brass');
end;

procedure PlayFMBell(AFrequency: Single);
begin
  PlayFM(AFrequency, 'bell');
end;

procedure PlayFMOrgan(AFrequency: Single);
begin
  PlayFM(AFrequency, 'organ');
end;

procedure PlayFMLead(AFrequency: Single);
begin
  PlayFM(AFrequency, 'lead');
end;

procedure PlayFMBass(AFrequency: Single);
begin
  PlayFM(AFrequency, 'bass');
end;

// ============================================================================
// WAVETABLE SYNTHESIS (simplified)
// ============================================================================

procedure PlayWavetable(AFrequency: Single; const APreset: string);
var
  Voice: TSedaiVoice;
  PresetLower: string;
  MidiNote: Byte;
begin
  Voice := AllocateVoice;
  if Voice = nil then Exit;

  PresetLower := LowerCase(APreset);
  Voice.SetOscillatorEnabled(0, True);
  Voice.Oscillators[0].Frequency := AFrequency;

  // TODO: Implement proper wavetable synthesis with TSedaiWavetableGenerator
  if (PresetLower = 'serum') or (PresetLower = 'wasp') or (PresetLower = 'ppg') then
  begin
    Voice.SetOscillatorWaveform(0, wtSawtooth);
    Voice.SetFilterCutoff(3000.0);
    Voice.SetEnvelopeADSR(0, 0.01, 0.2, 0.7, 0.3);
  end
  else if PresetLower = 'vocal' then
  begin
    Voice.SetOscillatorWaveform(0, wtSawtooth);
    Voice.SetFilterType(ftBandPass);
    Voice.SetFilterCutoff(1500.0);
    Voice.SetFilterResonance(0.6);
  end
  else if PresetLower = 'metallic' then
  begin
    Voice.SetOscillatorWaveform(0, wtSquare);
    Voice.SetFilterCutoff(4000.0);
    Voice.SetEnvelopeADSR(0, 0.001, 1.0, 0.2, 0.3);
  end
  else if PresetLower = 'glass' then
  begin
    Voice.SetOscillatorWaveform(0, wtSine);
    Voice.SetFilterCutoff(6000.0);
    Voice.SetEnvelopeADSR(0, 0.001, 1.5, 0.0, 0.3);
  end
  else if PresetLower = 'organ' then
  begin
    Voice.SetOscillatorWaveform(0, wtSquare);
    Voice.SetFilterCutoff(5000.0);
    Voice.SetEnvelopeADSR(0, 0.005, 0.0, 1.0, 0.05);
  end
  else if PresetLower = 'evolving' then
  begin
    Voice.SetOscillatorWaveform(0, wtSawtooth);
    Voice.SetFilterCutoff(1500.0);
    Voice.SetEnvelopeADSR(0, 0.5, 0.5, 0.6, 1.0);
  end
  else if PresetLower = 'digitalchaos' then
  begin
    Voice.SetOscillatorWaveform(0, wtNoise);
    Voice.SetFilterCutoff(2000.0);
    Voice.SetFilterResonance(0.8);
  end
  else
  begin
    Voice.SetOscillatorWaveform(0, wtSawtooth);
  end;

  MidiNote := Round(69 + 12 * Log2(AFrequency / 440.0));
  if MidiNote > 127 then MidiNote := 127;

  Voice.NoteOn(MidiNote, 0.8);
end;

procedure PlaySerum(AFrequency: Single);
begin
  PlayWavetable(AFrequency, 'serum');
end;

procedure PlayWasp(AFrequency: Single);
begin
  PlayWavetable(AFrequency, 'wasp');
end;

procedure PlayPPG(AFrequency: Single);
begin
  PlayWavetable(AFrequency, 'ppg');
end;

// ============================================================================
// CHORDS AND SCALES
// ============================================================================

procedure PlayChordClassic(ANote1, ANote2, ANote3: Single; const APreset: string);
begin
  PlayClassic(ANote1, APreset);
  PlayClassic(ANote2, APreset);
  PlayClassic(ANote3, APreset);
end;

procedure PlayChordFM(ANote1, ANote2, ANote3: Single; const APreset: string);
begin
  PlayFM(ANote1, APreset);
  PlayFM(ANote2, APreset);
  PlayFM(ANote3, APreset);
end;

procedure PlayChordWavetable(ANote1, ANote2, ANote3: Single; const APreset: string);
begin
  PlayWavetable(ANote1, APreset);
  PlayWavetable(ANote2, APreset);
  PlayWavetable(ANote3, APreset);
end;

procedure PlayScaleClassic(ABaseFreq: Single; const APreset: string);
const
  MajorScale: array[0..7] of Single = (1.0, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8, 2.0);
var
  I: Integer;
begin
  for I := 0 to 7 do
  begin
    PlayClassic(ABaseFreq * MajorScale[I], APreset);
    SDL_Delay(300);
  end;
end;

procedure PlayScaleFM(ABaseFreq: Single; const APreset: string);
const
  MajorScale: array[0..7] of Single = (1.0, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8, 2.0);
var
  I: Integer;
begin
  for I := 0 to 7 do
  begin
    PlayFM(ABaseFreq * MajorScale[I], APreset);
    SDL_Delay(300);
  end;
end;

procedure PlayScaleWavetable(ABaseFreq: Single; const APreset: string);
const
  MajorScale: array[0..7] of Single = (1.0, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8, 2.0);
var
  I: Integer;
begin
  for I := 0 to 7 do
  begin
    PlayWavetable(ABaseFreq * MajorScale[I], APreset);
    SDL_Delay(300);
  end;
end;

// ============================================================================
// SID CHIP
// ============================================================================

function GetSID: TSedaiSIDEvo;
begin
  Result := GEngineSID;
end;

// ============================================================================
// STATUS
// ============================================================================

procedure PrintStatus;
begin
  WriteLn('Sedai Audio Engine Status');
  WriteLn('-------------------------');
  WriteLn('Initialized: ', GEngineInitialized);
  WriteLn('Sample Rate: ', GEngineSampleRate, ' Hz');
  WriteLn('Buffer Size: ', GEngineBufferSize, ' samples');
  WriteLn('Master Volume: ', GEngineMasterVolume:0:2);
  WriteLn('Active Voices: ', GEngineActiveVoices, '/', ENGINE_MAX_VOICES);
  WriteLn('SID Available: ', Assigned(GEngineSID));
end;

end.
