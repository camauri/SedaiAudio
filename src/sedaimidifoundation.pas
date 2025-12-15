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

unit SedaiMIDIFoundation;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, SedaiMIDITypes, SedaiMIDISequencer, SedaiMIDIParser,
  SedaiAudioFoundation; // For PlaySerum, PlayChordWavetable, etc.

// Global MIDI sequencer instance
var
  GlobalMIDISequencer: TSedaiMIDISequencer = nil;

// ========================================================================
// MIDI SYSTEM INITIALIZATION
// ========================================================================

function INIT_MIDI: Boolean;
procedure SHUTDOWN_MIDI;
function IS_MIDI_INITIALIZED: Boolean;

// ========================================================================
// MIDI FILE OPERATIONS
// ========================================================================

function LOAD_MIDI_FILE(const AFilename: string): Boolean;
procedure UNLOAD_MIDI_FILE;
function VALIDATE_MIDI_FILE(const AFilename: string): Boolean;
function GET_MIDI_FILE_INFO(const AFilename: string): string;

// ========================================================================
// PLAYBACK CONTROL
// ========================================================================

procedure MIDI_PLAY;
procedure MIDI_STOP;
procedure MIDI_PAUSE;
procedure MIDI_SET_POSITION(ATick: Cardinal);
procedure MIDI_SET_POSITION_PERCENT(APercent: Single);

// ========================================================================
// PLAYBACK STATUS
// ========================================================================

function GET_MIDI_STATE: string;
function GET_MIDI_PROGRESS: Single;       // 0.0 to 100.0
function GET_MIDI_CURRENT_TICK: Cardinal;
function GET_MIDI_TOTAL_TICKS: Cardinal;
function GET_MIDI_ACTIVE_NOTES: Integer;
function IS_MIDI_PLAYING: Boolean;
function IS_MIDI_STOPPED: Boolean;
function IS_MIDI_PAUSED: Boolean;

// ========================================================================
// CHANNEL CONTROL
// ========================================================================

procedure SET_MIDI_CHANNEL_WAVETABLE(AChannel: Integer; const AWavetableType: string);
procedure SET_MIDI_CHANNEL_VOLUME(AChannel: Integer; AVolume: Single);
procedure SET_MIDI_CHANNEL_MUTE(AChannel: Integer; AMuted: Boolean);
function GET_MIDI_CHANNEL_INFO(AChannel: Integer): string;

// ========================================================================
// TEMPO AND TIMING
// ========================================================================

procedure SET_MIDI_TEMPO(ATempoPercent: Single);  // 100% = normal speed
function GET_MIDI_TEMPO: Cardinal;                // Microseconds per quarter
function GET_MIDI_BPM: Integer;                   // Beats per minute

// ========================================================================
// PRESET CONFIGURATIONS
// ========================================================================

procedure SETUP_MIDI_GENERAL_MIDI;               // Standard GM mapping
procedure SETUP_MIDI_WAVETABLE_SHOWCASE;         // Show off wavetables
procedure SETUP_MIDI_CUSTOM_MAPPING;             // Custom wavetable mapping

// ========================================================================
// SYSTEM STATUS AND DEBUG
// ========================================================================

procedure PRINT_MIDI_STATUS;
procedure PRINT_MIDI_CHANNELS;
procedure PRINT_MIDI_FILE_DETAILS;
function GET_MIDI_SYSTEM_INFO: string;

// ========================================================================
// UTILITIES
// ========================================================================

function MIDI_TICK_TO_SECONDS(ATick: Cardinal): Single;
function MIDI_SECONDS_TO_TICK(ASeconds: Single): Cardinal;
function MIDI_NOTE_TO_FREQUENCY(ANoteNumber: Byte): Single;
function MIDI_FREQUENCY_TO_NOTE(AFrequency: Single): Byte;

// ========================================================================
// BUILT-IN DEMO FILES
// ========================================================================

procedure CREATE_DEMO_MIDI_FILE(const AFilename: string; AType: Integer = 0);
procedure PLAY_MIDI_DEMO_SCALE;
procedure PLAY_MIDI_DEMO_CHORD_PROGRESSION;

// ========================================================================
// INTERNAL INTEGRATION - CRITICAL CALLBACK
// ========================================================================

procedure UPDATE_MIDI_SEQUENCER(ADeltaTimeSeconds: Single);
procedure MIDI_UPDATE_CALLBACK(ADeltaTimeSeconds: Single);

implementation

// ========================================================================
// SYSTEM INITIALIZATION - FIXED CALLBACK REGISTRATION
// ========================================================================

function INIT_MIDI: Boolean;
begin
  Result := False;

  if Assigned(GlobalMIDISequencer) then
  begin
    {$IFDEF SEDAI_DEBUG_MIDI}
    WriteLn('WARNING: MIDI already initialized');
    {$ENDIF}
    Result := True;
    Exit;
  end;

  try
    {$IFDEF SEDAI_DEBUG_MIDI}
    WriteLn('DEBUG: Creating MIDI Sequencer...');
    {$ENDIF}
    GlobalMIDISequencer := TSedaiMIDISequencer.Create;

    {$IFDEF SEDAI_DEBUG_MIDI}
    WriteLn('DEBUG: Registering MIDI callback with audio system...');
    {$ENDIF}
    // CRITICAL: Register callback with audio system
    RegisterMidiUpdateCallback(@MIDI_UPDATE_CALLBACK);

    Result := True;

    WriteLn('SEDAI MIDI System Initialized');
    {$IFDEF SEDAI_DEBUG_MIDI}
    WriteLn('- MIDI File Parser: Standard MIDI Format 0/1 support');
    WriteLn('- Sequencer: Real-time playback with wavetable synthesis');
    WriteLn('- Channels: 16-channel polyphonic playback');
    WriteLn('- Integration: Full SEDAI Audio System compatibility');
    WriteLn('- Audio Integration: Callback registered successfully');
    WriteLn('- Ready for MIDI file playback!');
    {$ENDIF}
  except
    on E: Exception do
    begin
      WriteLn('ERROR: MIDI initialization failed: ', E.Message);
      FreeAndNil(GlobalMIDISequencer);
      Result := False;
    end;
  end;
end;

procedure SHUTDOWN_MIDI;
begin
  if Assigned(GlobalMIDISequencer) then
  begin
    {$IFDEF SEDAI_DEBUG_MIDI}
    WriteLn('DEBUG: Stopping MIDI sequencer...');
    {$ENDIF}
    GlobalMIDISequencer.Stop;

    {$IFDEF SEDAI_DEBUG_MIDI}
    WriteLn('DEBUG: Unregistering MIDI callback...');
    {$ENDIF}
    // Unregister from audio system
    UnregisterMidiUpdateCallback;

    {$IFDEF SEDAI_DEBUG_MIDI}
    WriteLn('DEBUG: Destroying MIDI sequencer...');
    {$ENDIF}
    FreeAndNil(GlobalMIDISequencer);
    WriteLn('SEDAI MIDI System shutdown complete');
  end;
end;

function IS_MIDI_INITIALIZED: Boolean;
begin
  Result := Assigned(GlobalMIDISequencer);
end;

// ========================================================================
// FILE OPERATIONS
// ========================================================================

function LOAD_MIDI_FILE(const AFilename: string): Boolean;
begin
  Result := False;

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized - call INIT_MIDI first');
    Exit;
  end;

  WriteLn('Loading MIDI file: ', ExtractFileName(AFilename));
  Result := GlobalMIDISequencer.LoadMIDIFile(AFilename);

  if Result then
  begin
    WriteLn('MIDI file loaded successfully!');
    WriteLn('File info: ', GlobalMIDISequencer.GetMIDIFileInfo);
  end
  else
    WriteLn('ERROR: Failed to load MIDI file: ', AFilename);
end;

procedure UNLOAD_MIDI_FILE;
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.UnloadMIDIFile;
    WriteLn('MIDI file unloaded');
  end;
end;

function VALIDATE_MIDI_FILE(const AFilename: string): Boolean;
begin
  Result := TSedaiMIDIParser.ValidateMIDIFile(AFilename);
  if Result then
    WriteLn('MIDI file validation: PASSED - ', ExtractFileName(AFilename))
  else
    WriteLn('MIDI file validation: FAILED - ', ExtractFileName(AFilename));
end;

function GET_MIDI_FILE_INFO(const AFilename: string): string;
begin
  Result := TSedaiMIDIParser.GetMIDIFileInfo(AFilename);
end;

// ========================================================================
// PLAYBACK CONTROL
// ========================================================================

procedure MIDI_PLAY;
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.Play;
    WriteLn('MIDI playback started');
  end
  else
    WriteLn('ERROR: MIDI not initialized');
end;

procedure MIDI_STOP;
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.Stop;
    WriteLn('MIDI playback stopped');
  end;
end;

procedure MIDI_PAUSE;
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.Pause;
    WriteLn('MIDI playback paused');
  end;
end;

procedure MIDI_SET_POSITION(ATick: Cardinal);
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.SetPosition(ATick);
    WriteLn('MIDI position set to tick: ', ATick);
  end;
end;

procedure MIDI_SET_POSITION_PERCENT(APercent: Single);
var
  ATick: Cardinal;
begin
  if IS_MIDI_INITIALIZED then
  begin
    ATick := Round((APercent / 100.0) * GlobalMIDISequencer.GetTotalTicks);
    MIDI_SET_POSITION(ATick);
  end;
end;

// ========================================================================
// STATUS FUNCTIONS
// ========================================================================

function GET_MIDI_STATE: string;
begin
  if IS_MIDI_INITIALIZED then
  begin
    case GlobalMIDISequencer.GetPlaybackState of
      mpsStop: Result := 'Stopped';
      mpsPlay: Result := 'Playing';
      mpsPause: Result := 'Paused';
    else
      Result := 'Unknown';
    end;
  end
  else
    Result := 'Not Initialized';
end;

function GET_MIDI_PROGRESS: Single;
begin
  if IS_MIDI_INITIALIZED then
    Result := GlobalMIDISequencer.GetProgressPercent
  else
    Result := 0.0;
end;

function GET_MIDI_CURRENT_TICK: Cardinal;
begin
  if IS_MIDI_INITIALIZED then
    Result := GlobalMIDISequencer.GetCurrentTick
  else
    Result := 0;
end;

function GET_MIDI_TOTAL_TICKS: Cardinal;
begin
  if IS_MIDI_INITIALIZED then
    Result := GlobalMIDISequencer.GetTotalTicks
  else
    Result := 0;
end;

function GET_MIDI_ACTIVE_NOTES: Integer;
begin
  if IS_MIDI_INITIALIZED then
    Result := GlobalMIDISequencer.GetActiveNoteCount
  else
    Result := 0;
end;

function IS_MIDI_PLAYING: Boolean;
begin
  Result := IS_MIDI_INITIALIZED and (GlobalMIDISequencer.GetPlaybackState = mpsPlay);
end;

function IS_MIDI_STOPPED: Boolean;
begin
  Result := not IS_MIDI_INITIALIZED or (GlobalMIDISequencer.GetPlaybackState = mpsStop);
end;

function IS_MIDI_PAUSED: Boolean;
begin
  Result := IS_MIDI_INITIALIZED and (GlobalMIDISequencer.GetPlaybackState = mpsPause);
end;

// ========================================================================
// CHANNEL CONTROL
// ========================================================================

procedure SET_MIDI_CHANNEL_WAVETABLE(AChannel: Integer; const AWavetableType: string);
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.SetChannelWavetable(AChannel, AWavetableType);
    WriteLn('Channel ', AChannel, ' wavetable set to: ', AWavetableType);
  end
  else
    WriteLn('ERROR: MIDI not initialized');
end;

procedure SET_MIDI_CHANNEL_VOLUME(AChannel: Integer; AVolume: Single);
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.SetChannelVolume(AChannel, AVolume);
    WriteLn('Channel ', AChannel, ' volume set to: ', AVolume:0:2);
  end
  else
    WriteLn('ERROR: MIDI not initialized');
end;

procedure SET_MIDI_CHANNEL_MUTE(AChannel: Integer; AMuted: Boolean);
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.SetChannelMute(AChannel, AMuted);
    WriteLn('Channel ', AChannel, ' mute: ', BoolToStr(AMuted, True));
  end
  else
    WriteLn('ERROR: MIDI not initialized');
end;

function GET_MIDI_CHANNEL_INFO(AChannel: Integer): string;
begin
  if IS_MIDI_INITIALIZED then
    Result := GlobalMIDISequencer.GetChannelInfo(AChannel)
  else
    Result := 'MIDI not initialized';
end;

// ========================================================================
// TEMPO AND TIMING
// ========================================================================

procedure SET_MIDI_TEMPO(ATempoPercent: Single);
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.SetTempo(ATempoPercent);
    WriteLn('MIDI tempo set to ', ATempoPercent * 100:0:0, '%');
  end
  else
    WriteLn('ERROR: MIDI not initialized');
end;

function GET_MIDI_TEMPO: Cardinal;
begin
  if IS_MIDI_INITIALIZED then
    Result := GlobalMIDISequencer.GetCurrentTempo
  else
    Result := 500000; // Default 120 BPM
end;

function GET_MIDI_BPM: Integer;
begin
  Result := Round(60000000 / GET_MIDI_TEMPO);
end;

// ========================================================================
// PRESET CONFIGURATIONS
// ========================================================================

procedure SETUP_MIDI_GENERAL_MIDI;
var
  i: Integer;
begin
  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('Setting up General MIDI wavetable mapping...');

  // Standard GM mapping using available wavetables
  for i := 0 to 15 do
  begin
    case i of
      0..7:   SET_MIDI_CHANNEL_WAVETABLE(i, 'serum');  // Piano, Chromatic, Organ, Guitar
      8:      SET_MIDI_CHANNEL_WAVETABLE(i, 'wasp');   // Bass
      9:      SET_MIDI_CHANNEL_WAVETABLE(i, 'ppg');    // Drums (digital)
      10..15: SET_MIDI_CHANNEL_WAVETABLE(i, 'serum');  // Strings, Ensemble, Brass, etc.
    end;
  end;

  WriteLn('General MIDI setup complete!');
end;

procedure SETUP_MIDI_WAVETABLE_SHOWCASE;
var
  i: Integer;
  AWavetables: array[0..2] of string;
begin
  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('Setting up wavetable showcase mapping...');

  AWavetables[0] := 'serum';
  AWavetables[1] := 'wasp';
  AWavetables[2] := 'ppg';

  // Distribute wavetables across channels to showcase variety
  for i := 0 to 15 do
    SET_MIDI_CHANNEL_WAVETABLE(i, AWavetables[i mod 3]);

  WriteLn('Wavetable showcase setup complete!');
  WriteLn('  Channels 0,3,6,9,12,15: Serum wavetables');
  WriteLn('  Channels 1,4,7,10,13: Wasp wavetables');
  WriteLn('  Channels 2,5,8,11,14: PPG wavetables');
end;

procedure SETUP_MIDI_CUSTOM_MAPPING;
begin
  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('Setting up custom MIDI mapping...');

  // Custom mapping for specific musical styles
  SET_MIDI_CHANNEL_WAVETABLE(0, 'serum');   // Lead
  SET_MIDI_CHANNEL_WAVETABLE(1, 'wasp');    // Bass
  SET_MIDI_CHANNEL_WAVETABLE(2, 'ppg');     // Pad
  SET_MIDI_CHANNEL_WAVETABLE(3, 'serum');   // Strings
  SET_MIDI_CHANNEL_WAVETABLE(4, 'wasp');    // Brass
  SET_MIDI_CHANNEL_WAVETABLE(5, 'ppg');     // Synth effects

  // Set lower volume for pad and strings
  SET_MIDI_CHANNEL_VOLUME(2, 0.6);  // Pad
  SET_MIDI_CHANNEL_VOLUME(3, 0.7);  // Strings

  WriteLn('Custom mapping setup complete!');
end;

// ========================================================================
// STATUS AND DEBUG
// ========================================================================

procedure PRINT_MIDI_STATUS;
begin
  if IS_MIDI_INITIALIZED then
  begin
    GlobalMIDISequencer.PrintSequencerStatus;
  end
  else
  begin
    WriteLn('=== MIDI System Status ===');
    WriteLn('Status: Not Initialized');
    WriteLn('Call INIT_MIDI to initialize the system');
    WriteLn('========================');
  end;
end;

procedure PRINT_MIDI_CHANNELS;
var
  i: Integer;
begin
  if IS_MIDI_INITIALIZED then
  begin
    WriteLn('=== MIDI Channel Configuration ===');
    for i := 0 to 15 do
      WriteLn('  ', GET_MIDI_CHANNEL_INFO(i));
    WriteLn('================================');
  end
  else
    WriteLn('ERROR: MIDI not initialized');
end;

procedure PRINT_MIDI_FILE_DETAILS;
begin
  if IS_MIDI_INITIALIZED then
  begin
    WriteLn('=== MIDI File Details ===');
    WriteLn(GlobalMIDISequencer.GetMIDIFileInfo);
    WriteLn('Current State: ', GET_MIDI_STATE);
    WriteLn('Progress: ', GET_MIDI_PROGRESS:0:1, '%');
    WriteLn('Active Notes: ', GET_MIDI_ACTIVE_NOTES);
    WriteLn('Tempo: ', GET_MIDI_BPM, ' BPM');
    WriteLn('========================');
  end
  else
    WriteLn('ERROR: MIDI not initialized');
end;

function GET_MIDI_SYSTEM_INFO: string;
begin
  if IS_MIDI_INITIALIZED then
  begin
    Result := Format('MIDI System: %s, %s, %d active notes, %.1f%% progress',
      [GET_MIDI_STATE, GlobalMIDISequencer.GetMIDIFileInfo,
       GET_MIDI_ACTIVE_NOTES, GET_MIDI_PROGRESS]);
  end
  else
    Result := 'MIDI System: Not Initialized';
end;

// ========================================================================
// UTILITIES
// ========================================================================

function MIDI_TICK_TO_SECONDS(ATick: Cardinal): Single;
var
  ATempo: Cardinal;
  ADivision: Word;
begin
  Result := 0.0;
  if not IS_MIDI_INITIALIZED then Exit;

  ATempo := GET_MIDI_TEMPO;
  ADivision := 480; // Default division - would get from loaded file

  Result := (ATick / ADivision) * (ATempo / 1000000.0);
end;

function MIDI_SECONDS_TO_TICK(ASeconds: Single): Cardinal;
var
  ATempo: Cardinal;
  ADivision: Word;
begin
  Result := 0;
  if not IS_MIDI_INITIALIZED then Exit;

  ATempo := GET_MIDI_TEMPO;
  ADivision := 480; // Default division

  Result := Round(ASeconds * ADivision * (1000000.0 / ATempo));
end;

function MIDI_NOTE_TO_FREQUENCY(ANoteNumber: Byte): Single;
begin
  Result := MIDINoteToFrequency(ANoteNumber);
end;

function MIDI_FREQUENCY_TO_NOTE(AFrequency: Single): Byte;
begin
  Result := FrequencyToMIDINote(AFrequency);
end;

// ========================================================================
// DEMO FUNCTIONS
// ========================================================================

procedure CREATE_DEMO_MIDI_FILE(const AFilename: string; AType: Integer);
begin
  WriteLn('Creating demo MIDI file: ', AFilename);
  WriteLn('Demo type: ', AType);
  WriteLn('Note: Demo MIDI file creation not yet implemented');
  WriteLn('Use existing MIDI files for testing');
end;

procedure PLAY_MIDI_DEMO_SCALE;
begin
  WriteLn('Playing MIDI demo scale...');
  WriteLn('Note: This would play a C major scale using direct MIDI events');
  WriteLn('Use LOAD_MIDI_FILE with an actual MIDI file for full playback');

  // For now, play a scale using the regular audio system
  if IS_MIDI_INITIALIZED then
  begin
    PlaySerum(261.63); // C4
    WriteLn('Demo scale note: C4');
  end;
end;

procedure PLAY_MIDI_DEMO_CHORD_PROGRESSION;
begin
  WriteLn('Playing MIDI demo chord progression...');
  WriteLn('Note: This would play a chord progression using MIDI events');
  WriteLn('Use LOAD_MIDI_FILE with an actual MIDI file for full playback');

  if IS_MIDI_INITIALIZED then
  begin
    PlayChordWavetable(261.63, 329.63, 392.00, 'serum'); // C major
    WriteLn('Demo chord: C major (Serum wavetable)');
  end;
end;

// ========================================================================
// INTEGRATION WITH MAIN AUDIO SYSTEM - CRITICAL CALLBACK
// ========================================================================

// This function is called from the main audio update loop
procedure UPDATE_MIDI_SEQUENCER(ADeltaTimeSeconds: Single);
begin
  if IS_MIDI_INITIALIZED then
  begin
    try
      GlobalMIDISequencer.Update(ADeltaTimeSeconds);
    except
      on E: Exception do
      begin
        // Log error but don't crash audio callback
        WriteLn('ERROR in MIDI update: ', E.Message);
      end;
    end;
  end;
end;

// Export this function so it can be called from SedaiAudioFoundation
procedure MIDI_UPDATE_CALLBACK(ADeltaTimeSeconds: Single);
begin
  UPDATE_MIDI_SEQUENCER(ADeltaTimeSeconds);
end;

// ========================================================================
// FINALIZATION
// ========================================================================

initialization
  // MIDI system is initialized on demand via INIT_MIDI

finalization
  SHUTDOWN_MIDI;

end.
