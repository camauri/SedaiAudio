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

unit SedaiAudioFoundation;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, ctypes, SDL2, Math,
  SedaiAudioTypes, SedaiAudioChip, SedaiWavetableLoader;

// NEW: MIDI Integration support
type
  TMIDIUpdateProc = procedure(ADeltaTimeSeconds: Single);

var
  MIDIUpdateProc: TMIDIUpdateProc = nil;

// Global audio chip instance
var
  GlobalAudioChip: TSedaiAudioChip = nil;

// ========================================================================
// CORE API
// ========================================================================

// System initialization
function InitAudio(AVoices: Integer = 32): Boolean;
procedure ShutdownAudio;

// Classic synthesis (subtractive)
procedure PlayClassic(AFreq: Single; const APreset: string = 'sine');
procedure PlaySine(AFreq: Single);
procedure PlaySquare(AFreq: Single);
procedure PlaySaw(AFreq: Single);
procedure PlayTriangle(AFreq: Single);
procedure PlayLead(AFreq: Single);
procedure PlayBass(AFreq: Single);
procedure PlayPad(AFreq: Single);

// FM synthesis
procedure PlayFM(AFreq: Single; const APreset: string = 'epiano');
procedure PlayEPiano(AFreq: Single);
procedure PlayFMBrass(AFreq: Single);
procedure PlayFMBell(AFreq: Single);
procedure PlayFMOrgan(AFreq: Single);
procedure PlayFMLead(AFreq: Single);
procedure PlayFMBass(AFreq: Single);

// Wavetable synthesis
procedure PlayWavetable(AFreq: Single; const APreset: string = 'serum');
procedure PlaySerum(AFreq: Single);
procedure PlayWasp(AFreq: Single);
procedure PlayPPG(AFreq: Single);
procedure PlayScaleWavetable(ABaseFreq: Single; const APreset: string = 'serum');

// ========================================================================
// WAVETABLE LOADING API
// ========================================================================

function LoadWavetableFile(const AFilename: string): Boolean;
function LoadWavetableDirectory(const APath: string): Integer;
procedure ScanWavetableDirectory(const APath: string);

function GetLoadedWavetables: TStringArray;
function IsWavetableLoaded(const AName: string): Boolean;
procedure ClearWavetableCache;
function GetWavetableCacheInfo: string;

procedure PlayLoadedWavetable(AFreq: Single; const AWavetableName: string);
function PlayLoadedWavetableAdv(AFreq: Single; const AWavetableName: string): Integer;
procedure PlayCustomWavetable(AFreq: Single; const ACustomWavetable: TWavetable);
function PlayCustomWavetableAdv(AFreq: Single; const ACustomWavetable: TWavetable): Integer;

procedure PrintWavetableInfo(const AFilename: string);
procedure ListLoadedWavetables;
function GetWavetableFormats: TStringArray;

// ========================================================================
// ADVANCED API - VOICE CONTROL
// ========================================================================

function PlayClassicAdv(AFreq: Single; const APreset: string = 'sine'): Integer;
function PlayFMAdv(AFreq: Single; const APreset: string = 'epiano'): Integer;
function PlayWavetableAdv(AFreq: Single; const APreset: string = 'serum'): Integer;

procedure NoteOff(AVoiceIndex: Integer);
procedure NoteRelease(AVoiceIndex: Integer);
procedure SetVoicePan(AVoiceIndex: Integer; APan: Single);

// ========================================================================
// MIDI VOICE MANAGEMENT API
// ========================================================================

// Voice allocation - returns voice index or -1 if no voice available
function MidiAllocateVoice: Integer;
function MidiIsVoiceActive(AVoiceIndex: Integer): Boolean;
function MidiGetFreeVoiceCount: Integer;

// Voice state control
procedure MidiVoiceOn(AVoiceIndex: Integer);
procedure MidiVoiceOff(AVoiceIndex: Integer);
procedure MidiReleaseVoice(AVoiceIndex: Integer);
procedure MidiReleaseAllVoices;

// Voice parameter configuration
procedure MidiSetVoiceFrequency(AVoiceIndex: Integer; AFrequency: Single);
procedure MidiSetVoiceAmplitude(AVoiceIndex: Integer; AAmplitude: Single);
procedure MidiSetVoiceWavetable(AVoiceIndex: Integer; const AWavetableType: string);
procedure MidiSetVoicePan(AVoiceIndex: Integer; APan: Single);

// Voice parameter queries
function MidiGetVoiceFrequency(AVoiceIndex: Integer): Single;
function MidiGetVoiceAmplitude(AVoiceIndex: Integer): Single;
function MidiGetVoiceWavetable(AVoiceIndex: Integer): string;
function MidiGetVoicePan(AVoiceIndex: Integer): Single;

// ========================================================================
// MIDI UTILITY FUNCTIONS
// ========================================================================

// Note/Frequency conversion (A4 = 440 Hz, MIDI note 69)
function MidiNoteToFreq(ANoteNumber: Byte): Single;
function MidiFreqToNote(AFrequency: Single): Byte;

// Velocity/Amplitude conversion with logarithmic curve for natural dynamics
function MidiVelocityToAmp(AVelocity: Byte): Single;
function MidiAmpToVelocity(AAmplitude: Single): Byte;

// Pan conversion: MIDI pan (0-127, 64=center) to Sedai pan (-1.0 to 1.0)
function MidiPanToSedai(APan: Byte): Single;
function SedaiPanToMidi(APan: Single): Byte;

// Note name utilities
function MidiNoteToName(ANoteNumber: Byte): string;
function MidiNoteToOctave(ANoteNumber: Byte): Integer;

// ========================================================================
// MIDI-OPTIMIZED PLAYBACK
// ========================================================================

// Optimized wavetable playback for MIDI - allocates voice, configures, and plays
function PlayWavetableMidi(AFrequency: Single; const AWavetableType: string;
                           AAmplitude: Single = 0.8): Integer;

// Play note by MIDI note number
function PlayMidiNote(ANoteNumber: Byte; AVelocity: Byte;
                      const AWavetableType: string = 'serum'): Integer;

// ========================================================================
// SYSTEM UTILITIES
// ========================================================================

procedure StopAll;
procedure SmoothStopAll(AFadeTimeMs: Integer = 300);
procedure SetMasterVolume(AVolume: Single);
function GetMasterVolume: Single;

function GetActiveVoices: Integer;
function GetMaxVoices: Integer;
function GetSampleRate: Cardinal;
procedure PrintStatus;

// ========================================================================
// MIDI INTEGRATION API
// ========================================================================

procedure RegisterMidiUpdateCallback(ACallback: TMIDIUpdateProc);
procedure UnregisterMidiUpdateCallback;

// ========================================================================
// MUSICAL HELPERS
// ========================================================================

const
  // Note frequencies (A4 = 440 Hz) - CamelCase format
  NoteC1  = 32.70;   NoteCs1 = 34.65;   NoteD1  = 36.71;   NoteDs1 = 38.89;
  NoteE1  = 41.20;   NoteF1  = 43.65;   NoteFs1 = 46.25;   NoteG1  = 49.00;
  NoteGs1 = 51.91;   NoteA1  = 55.00;   NoteAs1 = 58.27;   NoteB1  = 61.74;

  NoteC2  = 65.41;   NoteCs2 = 69.30;   NoteD2  = 73.42;   NoteDs2 = 77.78;
  NoteE2  = 82.41;   NoteF2  = 87.31;   NoteFs2 = 92.50;   NoteG2  = 98.00;
  NoteGs2 = 103.83;  NoteA2  = 110.00;  NoteAs2 = 116.54;  NoteB2  = 123.47;

  NoteC3  = 130.81;  NoteCs3 = 138.59;  NoteD3  = 146.83;  NoteDs3 = 155.56;
  NoteE3  = 164.81;  NoteF3  = 174.61;  NoteFs3 = 185.00;  NoteG3  = 196.00;
  NoteGs3 = 207.65;  NoteA3  = 220.00;  NoteAs3 = 233.08;  NoteB3  = 246.94;

  NoteC4  = 261.63;  NoteCs4 = 277.18;  NoteD4  = 293.66;  NoteDs4 = 311.13;
  NoteE4  = 329.63;  NoteF4  = 349.23;  NoteFs4 = 369.99;  NoteG4  = 392.00;
  NoteGs4 = 415.30;  NoteA4  = 440.00;  NoteAs4 = 466.16;  NoteB4  = 493.88;

  NoteC5  = 523.25;  NoteCs5 = 554.37;  NoteD5  = 587.33;  NoteDs5 = 622.25;
  NoteE5  = 659.25;  NoteF5  = 698.46;  NoteFs5 = 739.99;  NoteG5  = 783.99;
  NoteGs5 = 830.61;  NoteA5  = 880.00;  NoteAs5 = 932.33;  NoteB5  = 987.77;

  NoteC6  = 1046.50; NoteCs6 = 1108.73; NoteD6  = 1174.66; NoteDs6 = 1244.51;
  NoteE6  = 1318.51; NoteF6  = 1396.91; NoteFs6 = 1479.98; NoteG6  = 1567.98;
  NoteGs6 = 1661.22; NoteA6  = 1760.00; NoteAs6 = 1864.66; NoteB6  = 1975.53;

  // Alias with underscore format (backward compatibility)
  NOTE_C1  = NoteC1;   NOTE_CS1 = NoteCs1;  NOTE_D1  = NoteD1;   NOTE_DS1 = NoteDs1;
  NOTE_E1  = NoteE1;   NOTE_F1  = NoteF1;   NOTE_FS1 = NoteFs1;  NOTE_G1  = NoteG1;
  NOTE_GS1 = NoteGs1;  NOTE_A1  = NoteA1;   NOTE_AS1 = NoteAs1;  NOTE_B1  = NoteB1;

  NOTE_C2  = NoteC2;   NOTE_CS2 = NoteCs2;  NOTE_D2  = NoteD2;   NOTE_DS2 = NoteDs2;
  NOTE_E2  = NoteE2;   NOTE_F2  = NoteF2;   NOTE_FS2 = NoteFs2;  NOTE_G2  = NoteG2;
  NOTE_GS2 = NoteGs2;  NOTE_A2  = NoteA2;   NOTE_AS2 = NoteAs2;  NOTE_B2  = NoteB2;

  NOTE_C3  = NoteC3;   NOTE_CS3 = NoteCs3;  NOTE_D3  = NoteD3;   NOTE_DS3 = NoteDs3;
  NOTE_E3  = NoteE3;   NOTE_F3  = NoteF3;   NOTE_FS3 = NoteFs3;  NOTE_G3  = NoteG3;
  NOTE_GS3 = NoteGs3;  NOTE_A3  = NoteA3;   NOTE_AS3 = NoteAs3;  NOTE_B3  = NoteB3;

  NOTE_C4  = NoteC4;   NOTE_CS4 = NoteCs4;  NOTE_D4  = NoteD4;   NOTE_DS4 = NoteDs4;
  NOTE_E4  = NoteE4;   NOTE_F4  = NoteF4;   NOTE_FS4 = NoteFs4;  NOTE_G4  = NoteG4;
  NOTE_GS4 = NoteGs4;  NOTE_A4  = NoteA4;   NOTE_AS4 = NoteAs4;  NOTE_B4  = NoteB4;

  NOTE_C5  = NoteC5;   NOTE_CS5 = NoteCs5;  NOTE_D5  = NoteD5;   NOTE_DS5 = NoteDs5;
  NOTE_E5  = NoteE5;   NOTE_F5  = NoteF5;   NOTE_FS5 = NoteFs5;  NOTE_G5  = NoteG5;
  NOTE_GS5 = NoteGs5;  NOTE_A5  = NoteA5;   NOTE_AS5 = NoteAs5;  NOTE_B5  = NoteB5;

  NOTE_C6  = NoteC6;   NOTE_CS6 = NoteCs6;  NOTE_D6  = NoteD6;   NOTE_DS6 = NoteDs6;
  NOTE_E6  = NoteE6;   NOTE_F6  = NoteF6;   NOTE_FS6 = NoteFs6;  NOTE_G6  = NoteG6;
  NOTE_GS6 = NoteGs6;  NOTE_A6  = NoteA6;   NOTE_AS6 = NoteAs6;  NOTE_B6  = NoteB6;

procedure PlayChordClassic(ANote1, ANote2, ANote3: Single; const APreset: string = 'sine');
procedure PlayChordFM(ANote1, ANote2, ANote3: Single; const APreset: string = 'epiano');
procedure PlayChordWavetable(ANote1, ANote2, ANote3: Single; const APreset: string = 'serum');

procedure PlayScaleClassic(ABaseFreq: Single; const APreset: string = 'sine');
procedure PlayScaleFM(ABaseFreq: Single; const APreset: string = 'epiano');

// Backward compatibility
procedure Beep(AFreq: Single; ADurationMs: Integer; AAmplitude: Single = 0.3);
procedure PlayNote(AChannel: Integer; AFreq: Single; ADurationMs: Integer;
                   AWaveType: TWaveType; AEndMode: TEndMode);

implementation

const
  // MIDI voice pool configuration
  MIDI_MAX_VOICES = 128;

  // Note names for MIDI_NOTE_TO_NAME
  NOTE_NAMES: array[0..11] of string = (
    'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'
  );

type
  // MIDI voice state record - clean design
  TMIDIVoiceState = record
    IsAllocated: Boolean;      // Voice slot is in use
    IsPlaying: Boolean;        // Voice is currently sounding
    Frequency: Single;         // Current frequency in Hz
    Amplitude: Single;         // Current amplitude (0.0-1.0)
    Pan: Single;               // Pan position (-1.0 to 1.0)
    WavetableType: string;     // Current wavetable preset
    SynthVoiceIndex: Integer;  // Mapped synth engine voice index
    NoteNumber: Byte;          // Original MIDI note (for tracking)
    Velocity: Byte;            // Original velocity (for tracking)
  end;

var
  // MIDI voice pool
  MIDIVoicePool: array[0..MIDI_MAX_VOICES-1] of TMIDIVoiceState;
  MIDIVoicePoolInitialized: Boolean = False;

// ========================================================================
// MIDI VOICE POOL INITIALIZATION
// ========================================================================

procedure InitializeMIDIVoicePool;
var
  i: Integer;
begin
  if MIDIVoicePoolInitialized then Exit;

  for i := 0 to MIDI_MAX_VOICES - 1 do
  begin
    MIDIVoicePool[i].IsAllocated := False;
    MIDIVoicePool[i].IsPlaying := False;
    MIDIVoicePool[i].Frequency := 440.0;
    MIDIVoicePool[i].Amplitude := 0.8;
    MIDIVoicePool[i].Pan := 0.0;
    MIDIVoicePool[i].WavetableType := 'serum';
    MIDIVoicePool[i].SynthVoiceIndex := -1;
    MIDIVoicePool[i].NoteNumber := 69;
    MIDIVoicePool[i].Velocity := 100;
  end;

  MIDIVoicePoolInitialized := True;
end;

// ========================================================================
// MIDI UTILITY FUNCTIONS - NEW IMPLEMENTATION
// ========================================================================

function MidiNoteToFreq(ANoteNumber: Byte): Single;
begin
  // Standard formula: f = 440 * 2^((n-69)/12)
  // A4 (note 69) = 440 Hz
  Result := 440.0 * Power(2.0, (ANoteNumber - 69) / 12.0);
end;

function MidiFreqToNote(AFrequency: Single): Byte;
var
  ANoteFloat: Single;
begin
  // Inverse formula: n = 69 + 12 * log2(f/440)
  if AFrequency <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  ANoteFloat := 69.0 + 12.0 * (Ln(AFrequency / 440.0) / Ln(2.0));

  // Clamp to valid MIDI range
  if ANoteFloat < 0 then
    Result := 0
  else if ANoteFloat > 127 then
    Result := 127
  else
    Result := Round(ANoteFloat);
end;

function MidiVelocityToAmp(AVelocity: Byte): Single;
begin
  // Logarithmic curve for more natural dynamics
  // Using power curve: amp = (velocity/127)^0.5 for softer response
  // Or use ^0.7 for slightly harder response
  if AVelocity = 0 then
    Result := 0.0
  else
    Result := Power(AVelocity / 127.0, 0.5);
end;

function MidiAmpToVelocity(AAmplitude: Single): Byte;
var
  AVelocity: Single;
begin
  // Inverse of MidiVelocityToAmp
  if AAmplitude <= 0 then
    Result := 0
  else if AAmplitude >= 1.0 then
    Result := 127
  else
  begin
    // amp = (vel/127)^0.5, so vel = 127 * amp^2
    AVelocity := 127.0 * Power(AAmplitude, 2.0);
    Result := Round(AVelocity);
  end;
end;

function MidiPanToSedai(APan: Byte): Single;
begin
  // MIDI pan: 0=hard left, 64=center, 127=hard right
  // Sedai pan: -1.0=left, 0.0=center, 1.0=right
  Result := (APan - 64) / 64.0;

  // Clamp to valid range
  if Result < -1.0 then Result := -1.0;
  if Result > 1.0 then Result := 1.0;
end;

function SedaiPanToMidi(APan: Single): Byte;
var
  AMIDIPan: Single;
begin
  // Inverse conversion
  // Clamp input
  if APan < -1.0 then APan := -1.0;
  if APan > 1.0 then APan := 1.0;

  AMIDIPan := (APan * 64.0) + 64.0;

  if AMIDIPan < 0 then
    Result := 0
  else if AMIDIPan > 127 then
    Result := 127
  else
    Result := Round(AMIDIPan);
end;

function MidiNoteToName(ANoteNumber: Byte): string;
var
  ANoteName: string;
  AOctave: Integer;
begin
  ANoteName := NOTE_NAMES[ANoteNumber mod 12];
  AOctave := (ANoteNumber div 12) - 1;  // MIDI octave convention
  Result := ANoteName + IntToStr(AOctave);
end;

function MidiNoteToOctave(ANoteNumber: Byte): Integer;
begin
  Result := (ANoteNumber div 12) - 1;
end;

// ========================================================================
// MIDI VOICE MANAGEMENT - NEW IMPLEMENTATION
// ========================================================================

function MidiAllocateVoice: Integer;
var
  i: Integer;
begin
  Result := -1;

  // Ensure pool is initialized
  InitializeMIDIVoicePool;

  // Find first free slot
  for i := 0 to MIDI_MAX_VOICES - 1 do
  begin
    if not MIDIVoicePool[i].IsAllocated then
    begin
      MIDIVoicePool[i].IsAllocated := True;
      MIDIVoicePool[i].IsPlaying := False;
      MIDIVoicePool[i].Frequency := 440.0;
      MIDIVoicePool[i].Amplitude := 0.8;
      MIDIVoicePool[i].Pan := 0.0;
      MIDIVoicePool[i].WavetableType := 'serum';
      MIDIVoicePool[i].SynthVoiceIndex := -1;
      Result := i;
      Exit;
    end;
  end;

  // No free voice available
  WriteLn('WARNING: MIDI voice pool exhausted (', MIDI_MAX_VOICES, ' voices in use)');
end;

function MidiIsVoiceActive(AVoiceIndex: Integer): Boolean;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := False
  else
    Result := MIDIVoicePool[AVoiceIndex].IsAllocated and
              MIDIVoicePool[AVoiceIndex].IsPlaying;
end;

function MidiGetFreeVoiceCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  InitializeMIDIVoicePool;

  for i := 0 to MIDI_MAX_VOICES - 1 do
  begin
    if not MIDIVoicePool[i].IsAllocated then
      Inc(Result);
  end;
end;

procedure MidiVoiceOn(AVoiceIndex: Integer);
var
  ASynthVoice: Integer;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;
  if not MIDIVoicePool[AVoiceIndex].IsAllocated then Exit;

  // Allocate synth engine voice and start playing
  with MIDIVoicePool[AVoiceIndex] do
  begin
    ASynthVoice := PlayWavetableAdv(Frequency, WavetableType);
    if ASynthVoice >= 0 then
    begin
      SynthVoiceIndex := ASynthVoice;
      SetVoicePan(ASynthVoice, Pan);
      IsPlaying := True;
    end;
  end;
end;

procedure MidiVoiceOff(AVoiceIndex: Integer);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;

  with MIDIVoicePool[AVoiceIndex] do
  begin
    if IsPlaying and (SynthVoiceIndex >= 0) then
    begin
      NoteOff(SynthVoiceIndex);
      IsPlaying := False;
    end;
  end;
end;

procedure MidiReleaseVoice(AVoiceIndex: Integer);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;

  // Stop playing if active
  MidiVoiceOff(AVoiceIndex);

  // Release the slot
  MIDIVoicePool[AVoiceIndex].IsAllocated := False;
  MIDIVoicePool[AVoiceIndex].SynthVoiceIndex := -1;
end;

procedure MidiReleaseAllVoices;
var
  i: Integer;
begin
  for i := 0 to MIDI_MAX_VOICES - 1 do
    MidiReleaseVoice(i);
end;

procedure MidiSetVoiceFrequency(AVoiceIndex: Integer; AFrequency: Single);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;
  MIDIVoicePool[AVoiceIndex].Frequency := AFrequency;
end;

procedure MidiSetVoiceAmplitude(AVoiceIndex: Integer; AAmplitude: Single);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;

  // Clamp amplitude
  if AAmplitude < 0.0 then AAmplitude := 0.0;
  if AAmplitude > 1.0 then AAmplitude := 1.0;

  MIDIVoicePool[AVoiceIndex].Amplitude := AAmplitude;
end;

procedure MidiSetVoiceWavetable(AVoiceIndex: Integer; const AWavetableType: string);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;
  MIDIVoicePool[AVoiceIndex].WavetableType := AWavetableType;
end;

procedure MidiSetVoicePan(AVoiceIndex: Integer; APan: Single);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;

  // Clamp pan
  if APan < -1.0 then APan := -1.0;
  if APan > 1.0 then APan := 1.0;

  MIDIVoicePool[AVoiceIndex].Pan := APan;

  // Update synth voice if playing
  with MIDIVoicePool[AVoiceIndex] do
  begin
    if IsPlaying and (SynthVoiceIndex >= 0) then
      SetVoicePan(SynthVoiceIndex, APan);
  end;
end;

function MidiGetVoiceFrequency(AVoiceIndex: Integer): Single;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := 440.0
  else
    Result := MIDIVoicePool[AVoiceIndex].Frequency;
end;

function MidiGetVoiceAmplitude(AVoiceIndex: Integer): Single;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := 0.8
  else
    Result := MIDIVoicePool[AVoiceIndex].Amplitude;
end;

function MidiGetVoiceWavetable(AVoiceIndex: Integer): string;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := 'serum'
  else
    Result := MIDIVoicePool[AVoiceIndex].WavetableType;
end;

function MidiGetVoicePan(AVoiceIndex: Integer): Single;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := 0.0
  else
    Result := MIDIVoicePool[AVoiceIndex].Pan;
end;

// ========================================================================
// MIDI-OPTIMIZED PLAYBACK - NEW IMPLEMENTATION
// ========================================================================

function PlayWavetableMidi(AFrequency: Single; const AWavetableType: string;
                          AAmplitude: Single): Integer;
begin
  Result := MidiAllocateVoice;
  if Result < 0 then Exit;

  with MIDIVoicePool[Result] do
  begin
    Frequency := AFrequency;
    Amplitude := AAmplitude;
    WavetableType := AWavetableType;
    NoteNumber := MidiFreqToNote(AFrequency);
  end;

  MidiVoiceOn(Result);
end;

function PlayMidiNote(ANoteNumber: Byte; AVelocity: Byte;
                      const AWavetableType: string): Integer;
begin
  Result := MidiAllocateVoice;
  if Result < 0 then Exit;

  with MIDIVoicePool[Result] do
  begin
    NoteNumber := ANoteNumber;
    Velocity := AVelocity;
    Frequency := MidiNoteToFreq(ANoteNumber);
    Amplitude := MidiVelocityToAmp(AVelocity);
    WavetableType := AWavetableType;
  end;

  MidiVoiceOn(Result);
end;

// ========================================================================
// SYSTEM INITIALIZATION
// ========================================================================

function InitAudio(AVoices: Integer): Boolean;
begin
  Result := False;

  // Cleanup existing instance
  if Assigned(GlobalAudioChip) then
  begin
    GlobalAudioChip.Shutdown;
    FreeAndNil(GlobalAudioChip);
  end;

  try
    GlobalAudioChip := TSedaiAudioChip.Create(AVoices);
    Result := GlobalAudioChip.Initialize;

    if Result then
    begin
      WriteLn('Sedai Audio System Initialized with MIDI Integration');
      WriteLn('- Voices: ', AVoices);
      WriteLn('- Sample Rate: ', GlobalAudioChip.GetSampleRate, ' Hz');
      WriteLn('- Synthesis Engines: Classic, FM, Wavetable');
      WriteLn('- Wavetable Loader: Multi-format support');
      WriteLn('- MIDI Integration: Unified callback system');
      WriteLn('- Ready for modular synthesis + MIDI!');
    end
    else
    begin
      WriteLn('ERROR: Sedai Audio initialization failed');
      FreeAndNil(GlobalAudioChip);
    end;
  except
    on E: Exception do
    begin
      WriteLn('EXCEPTION during audio init: ', E.Message);
      if Assigned(GlobalAudioChip) then
        FreeAndNil(GlobalAudioChip);
      Result := False;
    end;
  end;
end;

procedure ShutdownAudio;
begin
  if Assigned(GlobalAudioChip) then
  begin
    GlobalAudioChip.Shutdown;
    FreeAndNil(GlobalAudioChip);
    WriteLn('Sedai Audio shutdown complete');
  end;
end;

// ========================================================================
// MIDI INTEGRATION FUNCTIONS
// ========================================================================

procedure RegisterMidiUpdateCallback(ACallback: TMIDIUpdateProc);
begin
  MIDIUpdateProc := ACallback;
  if Assigned(GlobalAudioChip) then
  begin
    GlobalAudioChip.SetMIDIUpdateCallback(ACallback);
    WriteLn('MIDI update callback registered with audio system');
  end
  else
    WriteLn('WARNING: Audio chip not initialized, callback stored for later');
end;

procedure UnregisterMidiUpdateCallback;
begin
  MIDIUpdateProc := nil;
  if Assigned(GlobalAudioChip) then
  begin
    GlobalAudioChip.ClearMIDIUpdateCallback;
    WriteLn('MIDI update callback unregistered');
  end;
end;

// ========================================================================
// CLASSIC SYNTHESIS
// ========================================================================

procedure PlayClassic(AFreq: Single; const APreset: string);
begin
  if Assigned(GlobalAudioChip) then
    GlobalAudioChip.PlayNote(AFreq, APreset)
  else
    WriteLn('ERROR: Audio not initialized - call InitAudio first');
end;

procedure PlaySine(AFreq: Single);
begin
  PlayClassic(AFreq, 'sine');
end;

procedure PlaySquare(AFreq: Single);
begin
  PlayClassic(AFreq, 'square');
end;

procedure PlaySaw(AFreq: Single);
begin
  PlayClassic(AFreq, 'saw');
end;

procedure PlayTriangle(AFreq: Single);
begin
  PlayClassic(AFreq, 'triangle');
end;

procedure PlayLead(AFreq: Single);
begin
  PlayClassic(AFreq, 'lead');
end;

procedure PlayBass(AFreq: Single);
begin
  PlayClassic(AFreq, 'bass');
end;

procedure PlayPad(AFreq: Single);
begin
  PlayClassic(AFreq, 'pad');
end;

// ========================================================================
// FM SYNTHESIS
// ========================================================================

procedure PlayFM(AFreq: Single; const APreset: string);
begin
  if Assigned(GlobalAudioChip) then
    GlobalAudioChip.PlayFMNote(AFreq, APreset)
  else
    WriteLn('ERROR: Audio not initialized - call InitAudio first');
end;

procedure PlayEPiano(AFreq: Single);
begin
  PlayFM(AFreq, 'epiano');
end;

procedure PlayFMBrass(AFreq: Single);
begin
  PlayFM(AFreq, 'brass');
end;

procedure PlayFMBell(AFreq: Single);
begin
  PlayFM(AFreq, 'bell');
end;

procedure PlayFMOrgan(AFreq: Single);
begin
  PlayFM(AFreq, 'organ');
end;

procedure PlayFMLead(AFreq: Single);
begin
  PlayFM(AFreq, 'lead');
end;

procedure PlayFMBass(AFreq: Single);
begin
  PlayFM(AFreq, 'bass');
end;

// ========================================================================
// WAVETABLE SYNTHESIS
// ========================================================================

procedure PlayWavetable(AFreq: Single; const APreset: string);
begin
  if Assigned(GlobalAudioChip) then
    GlobalAudioChip.PlayWavetableNote(AFreq, APreset)
  else
    WriteLn('ERROR: Audio not initialized - call InitAudio first');
end;

procedure PlaySerum(AFreq: Single);
begin
  PlayWavetable(AFreq, 'serum');
end;

procedure PlayWasp(AFreq: Single);
begin
  PlayWavetable(AFreq, 'wasp');
end;

procedure PlayPPG(AFreq: Single);
begin
  PlayWavetable(AFreq, 'ppg');
end;

// ========================================================================
// WAVETABLE LOADING IMPLEMENTATION
// ========================================================================

function LoadWavetableFile(const AFilename: string): Boolean;
var
  AWavetable: TWavetable;
begin
  Result := False;

  if not FileExists(AFilename) then
  begin
    WriteLn('ERROR: Wavetable file not found: ', AFilename);
    Exit;
  end;

  WriteLn('Loading wavetable file: ', ExtractFileName(AFilename));
  AWavetable := TSedaiWavetableLoader.LoadWavetable(AFilename);
  Result := AWavetable.IsLoaded;

  if Result then
    WriteLn('SUCCESS: Wavetable loaded: ', AWavetable.Name)
  else
    WriteLn('ERROR: Failed to load wavetable: ', AFilename);
end;

function LoadWavetableDirectory(const APath: string): Integer;
var
  ALoadedNames: TStringArray;
begin
  Result := 0;

  if not DirectoryExists(APath) then
  begin
    WriteLn('ERROR: Directory not found: ', APath);
    Exit;
  end;

  WriteLn('Loading wavetables from directory: ', APath);
  ALoadedNames := TSedaiWavetableLoader.LoadWavetableDirectory(APath);
  Result := Length(ALoadedNames);

  WriteLn('SUCCESS: Loaded ', Result, ' wavetables from directory');
end;

procedure ScanWavetableDirectory(const APath: string);
var
  AFiles: TStringArray;
  i: Integer;
  AInfo: TWavetableInfo;
begin
  if not DirectoryExists(APath) then
  begin
    WriteLn('ERROR: Directory not found: ', APath);
    Exit;
  end;

  WriteLn('Scanning wavetable directory: ', APath);
  AFiles := TSedaiWavetableLoader.ScanWavetableDirectory(APath);

  WriteLn('Found ', Length(AFiles), ' wavetable files:');
  for i := 0 to Length(AFiles) - 1 do
  begin
    AInfo := TSedaiWavetableLoader.GetWavetableInfo(AFiles[i]);
    WriteLn('  ', ExtractFileName(AFiles[i]), ' (',
            TSedaiWavetableLoader.FormatToString(AInfo.Format),
            ', ', AInfo.FrameCount, ' frames)');
  end;
end;

function GetLoadedWavetables: TStringArray;
begin
  SetLength(Result, 0);
  WriteLn('INFO: GetLoadedWavetables not yet fully implemented');
end;

function IsWavetableLoaded(const AName: string): Boolean;
begin
  Result := TSedaiWavetableLoader.IsCached(AName);
end;

procedure ClearWavetableCache;
begin
  TSedaiWavetableLoader.ClearCache;
  WriteLn('Wavetable cache cleared');
end;

function GetWavetableCacheInfo: string;
begin
  Result := TSedaiWavetableLoader.GetCacheInfo;
end;

procedure PlayLoadedWavetable(AFreq: Single; const AWavetableName: string);
var
  AWavetable: TWavetable;
begin
  if not Assigned(GlobalAudioChip) then
  begin
    WriteLn('ERROR: Audio not initialized');
    Exit;
  end;

  AWavetable := TSedaiWavetableLoader.FindWavetableByName(AWavetableName);
  if AWavetable.IsLoaded then
  begin
    WriteLn('   >>> Playing AKWF: ', AWavetableName, ' (', AWavetable.SampleLength, ' samples)');
    GlobalAudioChip.PlayCustomWavetableNote(AFreq, AWavetable);
  end
  else
  begin
    // NO FALLBACK! If wavetable not loaded, report error and do nothing
    WriteLn('ERROR: Wavetable "', AWavetableName, '" NOT FOUND in cache - NOT PLAYING');
  end;
end;

function PlayLoadedWavetableAdv(AFreq: Single; const AWavetableName: string): Integer;
var
  AWavetable: TWavetable;
begin
  Result := -1;
  if not Assigned(GlobalAudioChip) then
  begin
    WriteLn('ERROR: Audio not initialized');
    Exit;
  end;

  AWavetable := TSedaiWavetableLoader.FindWavetableByName(AWavetableName);
  if AWavetable.IsLoaded then
  begin
    WriteLn('   >>> Playing AKWF: ', AWavetableName, ' (', AWavetable.SampleLength, ' samples)');
    Result := GlobalAudioChip.PlayCustomWavetableNoteAdvanced(AFreq, AWavetable);
  end
  else
  begin
    // NO FALLBACK! If wavetable not loaded, report error and return -1
    WriteLn('ERROR: Wavetable "', AWavetableName, '" NOT FOUND in cache - NOT PLAYING');
  end;
end;

procedure PlayCustomWavetable(AFreq: Single; const ACustomWavetable: TWavetable);
begin
  if not Assigned(GlobalAudioChip) then
  begin
    WriteLn('ERROR: Audio not initialized');
    Exit;
  end;

  if ACustomWavetable.IsLoaded then
    GlobalAudioChip.PlayCustomWavetableNote(AFreq, ACustomWavetable)
  else
  begin
    WriteLn('WARNING: Custom wavetable not loaded, using built-in');
    PlayWavetable(AFreq, 'serum');
  end;
end;

function PlayCustomWavetableAdv(AFreq: Single; const ACustomWavetable: TWavetable): Integer;
begin
  Result := -1;
  if not Assigned(GlobalAudioChip) then
  begin
    WriteLn('ERROR: Audio not initialized');
    Exit;
  end;

  if ACustomWavetable.IsLoaded then
    Result := GlobalAudioChip.PlayCustomWavetableNoteAdvanced(AFreq, ACustomWavetable)
  else
  begin
    WriteLn('WARNING: Custom wavetable not loaded, using built-in');
    Result := PlayWavetableAdv(AFreq, 'serum');
  end;
end;

procedure PrintWavetableInfo(const AFilename: string);
var
  AInfo: TWavetableInfo;
begin
  if not FileExists(AFilename) then
  begin
    WriteLn('ERROR: File not found: ', AFilename);
    Exit;
  end;

  AInfo := TSedaiWavetableLoader.GetWavetableInfo(AFilename);

  WriteLn('=== Wavetable Information ===');
  WriteLn('File: ', ExtractFileName(AFilename));
  WriteLn('Name: ', AInfo.Name);
  WriteLn('Format: ', TSedaiWavetableLoader.FormatToString(AInfo.Format));
  WriteLn('Frame Count: ', AInfo.FrameCount);
  WriteLn('Sample Rate: ', AInfo.SampleRate, ' Hz');
  WriteLn('File Size: ', AInfo.FileSize, ' bytes');
  WriteLn('Valid: ', BoolToStr(AInfo.IsValid, True));
  WriteLn('============================');
end;

procedure ListLoadedWavetables;
begin
  WriteLn('=== Loaded Wavetables ===');
  WriteLn(GetWavetableCacheInfo);
  WriteLn('========================');
end;

function GetWavetableFormats: TStringArray;
begin
  Result := TSedaiWavetableLoader.GetSupportedExtensions;
end;

// ========================================================================
// ADVANCED API - VOICE CONTROL
// ========================================================================

function PlayClassicAdv(AFreq: Single; const APreset: string): Integer;
begin
  if Assigned(GlobalAudioChip) then
    Result := GlobalAudioChip.PlayNoteAdvanced(AFreq, APreset)
  else
  begin
    WriteLn('ERROR: Audio not initialized');
    Result := -1;
  end;
end;

function PlayFMAdv(AFreq: Single; const APreset: string): Integer;
begin
  if Assigned(GlobalAudioChip) then
    Result := GlobalAudioChip.PlayFMNoteAdvanced(AFreq, APreset)
  else
  begin
    WriteLn('ERROR: Audio not initialized');
    Result := -1;
  end;
end;

function PlayWavetableAdv(AFreq: Single; const APreset: string): Integer;
begin
  if Assigned(GlobalAudioChip) then
    Result := GlobalAudioChip.PlayWavetableNoteAdvanced(AFreq, APreset)
  else
  begin
    WriteLn('ERROR: Audio not initialized');
    Result := -1;
  end;
end;

procedure NoteOff(AVoiceIndex: Integer);
begin
  if Assigned(GlobalAudioChip) then
    GlobalAudioChip.NoteOff(AVoiceIndex)
  else
    WriteLn('ERROR: Audio not initialized');
end;

procedure NoteRelease(AVoiceIndex: Integer);
begin
  // NoteRelease triggers the ADSR release phase
  // For now, this behaves the same as NoteOff
  // Future: could implement different release behaviors
  NoteOff(AVoiceIndex);
end;

procedure SetVoicePan(AVoiceIndex: Integer; APan: Single);
begin
  if Assigned(GlobalAudioChip) then
    GlobalAudioChip.SetVoicePan(AVoiceIndex, APan)
  else
    WriteLn('ERROR: Audio not initialized');
end;

// ========================================================================
// SYSTEM UTILITIES
// ========================================================================

procedure StopAll;
begin
  if Assigned(GlobalAudioChip) then
    GlobalAudioChip.StopAll
  else
    WriteLn('ERROR: Audio not initialized');
end;

procedure SmoothStopAll(AFadeTimeMs: Integer = 300);
var
  i, ASteps: Integer;
  ACurrentVolume, AStep: Single;
begin
  if not Assigned(GlobalAudioChip) then Exit;

  for i := 0 to GetMaxVoices - 1 do
    NoteOff(i);

  ACurrentVolume := GetMasterVolume;
  ASteps := AFadeTimeMs div 20;
  if ASteps <= 0 then ASteps := 1;
  AStep := ACurrentVolume / ASteps;

  for i := ASteps downto 1 do
  begin
    SetMasterVolume(i * AStep);
    SDL_Delay(20);
  end;

  StopAll;
  SetMasterVolume(ACurrentVolume);
end;

procedure SetMasterVolume(AVolume: Single);
begin
  if Assigned(GlobalAudioChip) then
    GlobalAudioChip.SetMasterVolume(AVolume)
  else
    WriteLn('ERROR: Audio not initialized');
end;

function GetMasterVolume: Single;
begin
  if Assigned(GlobalAudioChip) then
    Result := GlobalAudioChip.GetMasterVolume
  else
  begin
    WriteLn('ERROR: Audio not initialized');
    Result := 0.0;
  end;
end;

function GetActiveVoices: Integer;
begin
  if Assigned(GlobalAudioChip) then
    Result := GlobalAudioChip.GetActiveVoices
  else
  begin
    WriteLn('ERROR: Audio not initialized');
    Result := 0;
  end;
end;

function GetMaxVoices: Integer;
begin
  if Assigned(GlobalAudioChip) then
    Result := GlobalAudioChip.GetMaxVoices
  else
  begin
    WriteLn('ERROR: Audio not initialized');
    Result := 0;
  end;
end;

function GetSampleRate: Cardinal;
begin
  if Assigned(GlobalAudioChip) then
    Result := GlobalAudioChip.GetSampleRate
  else
  begin
    WriteLn('ERROR: Audio not initialized');
    Result := 0;
  end;
end;

procedure PrintStatus;
begin
  if Assigned(GlobalAudioChip) then
    GlobalAudioChip.PrintVoiceStatus
  else
    WriteLn('ERROR: Audio not initialized - call InitAudio first');
end;

// ========================================================================
// MUSICAL HELPERS
// ========================================================================

procedure PlayChordClassic(ANote1, ANote2, ANote3: Single; const APreset: string);
begin
  PlayClassic(ANote1, APreset);
  SDL_Delay(10);
  PlayClassic(ANote2, APreset);
  SDL_Delay(10);
  PlayClassic(ANote3, APreset);
end;

procedure PlayChordFM(ANote1, ANote2, ANote3: Single; const APreset: string);
begin
  PlayFM(ANote1, APreset);
  SDL_Delay(10);
  PlayFM(ANote2, APreset);
  SDL_Delay(10);
  PlayFM(ANote3, APreset);
end;

procedure PlayChordWavetable(ANote1, ANote2, ANote3: Single; const APreset: string);
begin
  PlayWavetable(ANote1, APreset);
  SDL_Delay(10);
  PlayWavetable(ANote2, APreset);
  SDL_Delay(10);
  PlayWavetable(ANote3, APreset);
end;

procedure PlayScaleClassic(ABaseFreq: Single; const APreset: string);
var
  i: Integer;
  Ratios: array[0..7] of Single;
begin
  Ratios[0] := 1.0;      Ratios[1] := 9.0/8.0;  Ratios[2] := 5.0/4.0;  Ratios[3] := 4.0/3.0;
  Ratios[4] := 3.0/2.0;  Ratios[5] := 5.0/3.0;  Ratios[6] := 15.0/8.0; Ratios[7] := 2.0;

  for i := 0 to 7 do
  begin
    PlayClassic(ABaseFreq * Ratios[i], APreset);
    SDL_Delay(300);
  end;
end;

procedure PlayScaleFM(ABaseFreq: Single; const APreset: string);
var
  i: Integer;
  Ratios: array[0..7] of Single;
begin
  Ratios[0] := 1.0; Ratios[1] := 9.0/8.0; Ratios[2] := 5.0/4.0; Ratios[3] := 4.0/3.0;
  Ratios[4] := 3.0/2.0; Ratios[5] := 5.0/3.0; Ratios[6] := 15.0/8.0; Ratios[7] := 2.0;

  for i := 0 to 7 do
  begin
    PlayFM(ABaseFreq * Ratios[i], APreset);
    SDL_Delay(300);
  end;
end;

procedure PlayScaleWavetable(ABaseFreq: Single; const APreset: string);
var
  i: Integer;
  Ratios: array[0..7] of Single;
begin
  Ratios[0] := 1.0; Ratios[1] := 9.0/8.0; Ratios[2] := 5.0/4.0; Ratios[3] := 4.0/3.0;
  Ratios[4] := 3.0/2.0; Ratios[5] := 5.0/3.0; Ratios[6] := 15.0/8.0; Ratios[7] := 2.0;

  for i := 0 to 7 do
  begin
    PlayWavetable(ABaseFreq * Ratios[i], APreset);
    SDL_Delay(300);
  end;
end;

// ========================================================================
// BACKWARD COMPATIBILITY
// ========================================================================

procedure Beep(AFreq: Single; ADurationMs: Integer; AAmplitude: Single);
begin
  PlaySine(AFreq);
end;

procedure PlayNote(AChannel: Integer; AFreq: Single; ADurationMs: Integer;
  AWaveType: TWaveType; AEndMode: TEndMode);
begin
  case AWaveType of
    wtSine: PlaySine(AFreq);
    wtSquare: PlaySquare(AFreq);
    wtSawtooth: PlaySaw(AFreq);
    wtTriangle: PlayTriangle(AFreq);
    wtNoise: PlayClassic(AFreq, 'noise');
    wtFM: PlayEPiano(AFreq);
  end;
end;

// ========================================================================
// FINALIZATION
// ========================================================================

initialization
  // Audio system is initialized on demand via InitAudio

finalization
  ShutdownAudio;

end.
