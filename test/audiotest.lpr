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

program AudioTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils,
  SDL2, SedaiDateTimeUtils,
  SedaiAudioFoundation, SedaiAudioTypes,
  SedaiMIDIFoundation, SedaiMIDITypes; // NEW: MIDI Support

var
  AppPath: string = '';
  DataPath: string = '';  // Path to data/ directory (wavetables, midi, etc.)
  GTerminateRequested: Boolean = False;

// =========================================================================
// DATA PATH RESOLUTION
// =========================================================================
// Finds the data/ directory by searching:
// 1. AppPath/data/ (if running from bin/<platform>/)
// 2. AppPath/../../data/ (if exe is in bin/<platform>/, data is at root)
// 3. Current directory/data/

function FindDataPath: string;
var
  ATestPath: string;
begin
  // Try 1: data/ in same directory as executable
  ATestPath := ConcatPaths([AppPath, 'data']);
  if DirectoryExists(ATestPath) then
  begin
    Result := ATestPath + DirectorySeparator;
    Exit;
  end;

  // Try 2: data/ two levels up (exe in bin/<platform>/, data at project root)
  ATestPath := ConcatPaths([AppPath, '..', '..', 'data']);
  if DirectoryExists(ATestPath) then
  begin
    Result := ExpandFileName(ATestPath) + DirectorySeparator;
    Exit;
  end;

  // Try 3: data/ in current working directory
  ATestPath := ConcatPaths([GetCurrentDir, 'data']);
  if DirectoryExists(ATestPath) then
  begin
    Result := ATestPath + DirectorySeparator;
    Exit;
  end;

  // Fallback: use AppPath (old behavior)
  Result := AppPath;
end;

// =========================================================================
// CTRL+C HANDLER
// =========================================================================

{$IFDEF WINDOWS}
function ConsoleCtrlHandler(dwCtrlType: DWORD): BOOL; stdcall;
begin
  Result := True;
  case dwCtrlType of
    CTRL_C_EVENT, CTRL_BREAK_EVENT, CTRL_CLOSE_EVENT:
    begin
      WriteLn('');
      WriteLn('Interrupt received, shutting down...');
      GTerminateRequested := True;
      // Perform immediate cleanup
      SmoothStopAll(100);
      SHUTDOWN_MIDI;
      ShutdownAudio;
      Halt(0);
    end;
  else
    Result := False;
  end;
end;
{$ENDIF}

procedure SetupCtrlCHandler;
begin
  {$IFDEF WINDOWS}
  SetConsoleCtrlHandler(@ConsoleCtrlHandler, True);
  {$ENDIF}
end;

// =========================================================================
// INTERRUPTIBLE DELAY - Checks for CTRL+C during wait
// =========================================================================

procedure InterruptibleDelay(AMilliseconds: Cardinal);
var
  AEndTime: QWord;
  ARemaining: Cardinal;
begin
  AEndTime := GetTickCount64 + AMilliseconds;
  while (GetTickCount64 < AEndTime) and (not GTerminateRequested) do
  begin
    ARemaining := AEndTime - GetTickCount64;
    if ARemaining > 50 then
      SDL_Delay(50)  // Check every 50ms for termination
    else if ARemaining > 0 then
      SDL_Delay(ARemaining);
  end;

  // If termination was requested, clean up and exit
  if GTerminateRequested then
  begin
    WriteLn('');
    WriteLn('Demo interrupted by user.');
    SmoothStopAll(100);
    raise EAbort.Create('User interrupt');
  end;
end;

// =========================================================================
// NEW: MIDI DEMO FUNCTIONS
// =========================================================================

procedure TestMIDISystem;
begin
  WriteLn('=== MIDI System Test ===');

  if not INIT_MIDI then
  begin
    WriteLn('ERROR: Failed to initialize MIDI system');
    Exit;
  end;

  WriteLn('1. MIDI System Status');
  PRINT_MIDI_STATUS;

  WriteLn('2. Setting up General MIDI mapping');
  SETUP_MIDI_GENERAL_MIDI;

  WriteLn('3. Channel configuration');
  PRINT_MIDI_CHANNELS;

  WriteLn('MIDI system test complete!');
end;

procedure TestMIDIFileLoading;
var
  ATestFiles: array[0..4] of string;
  i: Integer;
  AFound: Boolean;
begin
  WriteLn('=== MIDI File Loading Test ===');

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('1. Looking for MIDI files...');
  ATestFiles[0] := ConcatPaths([DataPath, 'midi', 'test.mid']);
  ATestFiles[1] := ConcatPaths([DataPath, 'demo.mid']);
  ATestFiles[2] := ConcatPaths([DataPath, 'music.mid']);
  ATestFiles[3] := 'test.mid';
  ATestFiles[4] := 'demo.mid';

  AFound := False;
  for i := 0 to Length(ATestFiles) - 1 do
  begin
    WriteLn('  Checking: ', ATestFiles[i]);
    if FileExists(ATestFiles[i]) then
    begin
      WriteLn('  > Found MIDI file: ', ATestFiles[i]);

      if VALIDATE_MIDI_FILE(ATestFiles[i]) then
      begin
        WriteLn('  > File validation: PASSED');
        WriteLn('  > Info: ', GET_MIDI_FILE_INFO(ATestFiles[i]));

        if LOAD_MIDI_FILE(ATestFiles[i]) then
        begin
          WriteLn('  > Loading: SUCCESS');
          PRINT_MIDI_FILE_DETAILS;
          AFound := True;
          Break;
        end
        else
          WriteLn('  > Loading: FAILED');
      end
      else
        WriteLn('  > File validation: FAILED');
    end
    else
      WriteLn('    Not found');
  end;

  if not AFound then
  begin
    WriteLn('  No MIDI files found for testing');
    WriteLn('  To test MIDI playback:');
    WriteLn('    1. Place a .mid file in the application directory');
    WriteLn('    2. Or create a "midi" subdirectory with MIDI files');
    WriteLn('    3. Restart demo and select MIDI file loading test');
  end;

  WriteLn('MIDI file loading test complete!');
end;

// CORRECT PATCH for AudioTest.lpr - STRICT FREE PASCAL

procedure TestMIDIPlayback;
var
  AStartTime, ALastProgressTime: QWord;
  ATimer: THiResTimer;          // <- High resolution timer
  AElapsedSeconds: Double;
begin
  WriteLn('=== MIDI Playback Test ===');

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  if GET_MIDI_TOTAL_TICKS = 0 then
  begin
    WriteLn('ERROR: No MIDI file loaded');
    WriteLn('Load a MIDI file first using option 21');
    Exit;
  end;

  WriteLn('1. Starting MIDI playback...');

  // Optimized volume and tempo
  SetMasterVolume(0.5);  // Safe volume
  SET_MIDI_TEMPO(1.0);     // Normal tempo

  // Start precise timer
  ATimer := CreateHiResTimer;

  MIDI_PLAY;

  WriteLn('2. Playing for 45 seconds with high-resolution timing...');
  WriteLn('   (You should hear music through the wavetable synthesizers)');
  WriteLn('   Progress will be shown every 5 seconds...');

  AStartTime := GetTickCount64;
  ALastProgressTime := AStartTime;

  while (GET_MIDI_PROGRESS < 100.0) and (IS_MIDI_PLAYING) and (not GTerminateRequested) do
  begin
    SDL_Delay(100); // Check every 100ms for better responsiveness

    // Check for user interrupt
    if GTerminateRequested then
    begin
      WriteLn('   Playback interrupted by user');
      Break;
    end;

    AElapsedSeconds := ATimer.ElapsedSeconds;

    // Show progress every 5 seconds
    if (GetTickCount64 - ALastProgressTime) >= 5000 then
    begin
      WriteLn(Format('   Progress: %.1f%% | Active Notes: %d | State: %s | Time: %.1fs',
        [GET_MIDI_PROGRESS, GET_MIDI_ACTIVE_NOTES, GET_MIDI_STATE, AElapsedSeconds]));
      ALastProgressTime := GetTickCount64;
    end;

    // Stop dopo 45 secondi
    if AElapsedSeconds > 45.0 then
    begin
      WriteLn('   Demo time limit reached (45 seconds) - stopping playback');
      Break;
    end;
  end;

  WriteLn('3. Stopping playback...');
  WriteLn('   Total elapsed time: ', ATimer.ElapsedSeconds:0:2, ' seconds');

  MIDI_STOP;
  SetMasterVolume(0.7);

  WriteLn('MIDI playback test complete!');
end;

procedure TestMIDITiming;
var
  ATimer: THiResTimer;
  i: Integer;
  AInterval: Double;
begin
  WriteLn('=== MIDI Timing Precision Test ===');

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('Testing high-resolution timer precision...');

  ATimer := CreateHiResTimer;

  for i := 1 to 5 do
  begin
    InterruptibleDelay(100);  // 100ms delay
    AInterval := ATimer.ElapsedMilliseconds;
    WriteLn('  Iteration ', i, ': ', AInterval:0:3, ' ms (expected: ~', i*100, ' ms)');
  end;

  WriteLn('Timer precision: ', ATimer.ElapsedMicroseconds:0:0, ' microseconds total');
  WriteLn('Expected: ~500000 microseconds (500ms)');

  WriteLn('High-resolution timing test complete!');
end;

procedure TestMIDIChannelMapping;
begin
  WriteLn('=== MIDI Channel Mapping Test ===');

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('1. Testing different wavetable mappings...');

  WriteLn('   Setting up wavetable showcase...');
  SETUP_MIDI_WAVETABLE_SHOWCASE;

  WriteLn('2. Channel configuration after showcase setup:');
  PRINT_MIDI_CHANNELS;

  WriteLn('3. Testing custom mapping...');
  SETUP_MIDI_CUSTOM_MAPPING;
  PRINT_MIDI_CHANNELS;

  WriteLn('4. Testing individual channel controls...');
  SET_MIDI_CHANNEL_VOLUME(0, 0.5);     // Half volume on channel 0
  SET_MIDI_CHANNEL_MUTE(9, True);      // Mute drum channel
  SET_MIDI_CHANNEL_WAVETABLE(1, 'ppg'); // Change channel 1 to PPG

  WriteLn('   After individual changes:');
  WriteLn('   ', GET_MIDI_CHANNEL_INFO(0));
  WriteLn('   ', GET_MIDI_CHANNEL_INFO(1));
  WriteLn('   ', GET_MIDI_CHANNEL_INFO(9));

  WriteLn('MIDI channel mapping test complete!');
end;

procedure TestMIDITempoControl;
begin
  WriteLn('=== MIDI Tempo Control Test ===');

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('1. Current tempo: ', GET_MIDI_BPM, ' BPM');

  WriteLn('2. Testing tempo changes...');
  SET_MIDI_TEMPO(0.5);  // Half speed
  WriteLn('   Half speed: ', GET_MIDI_BPM, ' BPM');

  SET_MIDI_TEMPO(2.0);  // Double speed
  WriteLn('   Double speed: ', GET_MIDI_BPM, ' BPM');

  SET_MIDI_TEMPO(1.0);  // Normal speed
  WriteLn('   Normal speed: ', GET_MIDI_BPM, ' BPM');

  WriteLn('3. Tempo utilities test...');
  WriteLn('   Note 60 (C4) frequency: ', MIDI_NOTE_TO_FREQUENCY(60):0:2, ' Hz');
  WriteLn('   440 Hz note number: ', MIDI_FREQUENCY_TO_NOTE(440.0));

  WriteLn('MIDI tempo control test complete!');
end;

// PATCH for AudioTest.lpr
// Add this at the beginning of RunMIDIDemo:

procedure RunMIDIDemo;
begin
  WriteLn('=== SEDAI MIDI - COMPLETE DEMO ===');

  SetMasterVolume(0.5);
  WriteLn('Master volume set to 50% for clean MIDI demo');

  WriteLn('');
  WriteLn('MIDI SYSTEM INITIALIZATION');
  TestMIDISystem;

  WriteLn('');
  WriteLn('MIDI TIMING PRECISION TEST');
  TestMIDITiming;

  WriteLn('');
  WriteLn('MIDI FILE LOADING');
  TestMIDIFileLoading;

  WriteLn('');
  WriteLn('MIDI CHANNEL MAPPING');
  TestMIDIChannelMapping;

  WriteLn('');
  WriteLn('MIDI TEMPO CONTROL');
  TestMIDITempoControl;

  WriteLn('');
  WriteLn('MIDI PLAYBACK (if file loaded)');
  if GET_MIDI_TOTAL_TICKS > 0 then
    TestMIDIPlayback
  else
    WriteLn('Skipping playback test - no MIDI file loaded');

  WriteLn('');
  WriteLn('=== COMPLETE MIDI DEMO FINISHED ===');
  WriteLn('');
  WriteLn('>> MIDI Playback with High-Resolution Timing:');
  WriteLn('1. Microsecond-precision timing for smooth playback');
  WriteLn('2. Real-time tempo calculation based on actual elapsed time');
  WriteLn('3. Automatic protection against timing jumps');
  WriteLn('4. Cross-platform high-resolution timers');
  WriteLn('5. Anti-click ADSR envelopes for clean audio');

  SetMasterVolume(0.7);
end;

// =========================================================================
// CLASSIC SYNTHESIS TESTS
// =========================================================================

procedure TestClassicWaveforms;
begin
  WriteLn('=== Classic Synthesis - Basic Waveforms ===');
  WriteLn('Active voices before start: ', GetActiveVoices);

  WriteLn('1. Pure Sine Wave (440 Hz)');
  PlaySine(440.0);
  WriteLn('   Active voices after PlaySine: ', GetActiveVoices);
  InterruptibleDelay(1500);
  WriteLn('   Active voices after delay: ', GetActiveVoices);

  WriteLn('2. Square Wave (440 Hz)');
  PlaySquare(440.0);
  WriteLn('   Active voices after PlaySquare: ', GetActiveVoices);
  InterruptibleDelay(1500);
  WriteLn('   Active voices after delay: ', GetActiveVoices);

  WriteLn('3. Sawtooth Wave (440 Hz)');
  PlaySaw(440.0);
  WriteLn('   Active voices after PlaySaw: ', GetActiveVoices);
  InterruptibleDelay(1500);
  WriteLn('   Active voices after delay: ', GetActiveVoices);

  WriteLn('4. Triangle Wave (440 Hz)');
  PlayTriangle(440.0);
  WriteLn('   Active voices after PlayTriangle: ', GetActiveVoices);
  InterruptibleDelay(1500);
  WriteLn('   Active voices after delay: ', GetActiveVoices);

  SmoothStopAll(500);
  WriteLn('Basic waveforms test complete!');
end;

procedure TestClassicPresets;
begin
  WriteLn('=== Classic Synthesis - Analog Presets ===');
  WriteLn('1. Analog Lead (E4 - 329.63 Hz)');
  PlayLead(329.63);
  InterruptibleDelay(2500);

  WriteLn('2. Analog Bass (C2 - 65.41 Hz)');
  PlayBass(65.41);
  InterruptibleDelay(2500);

  WriteLn('3. Analog Pad (G3 - 196.00 Hz)');
  PlayPad(196.00);
  InterruptibleDelay(3000);

  SmoothStopAll(500);
  WriteLn('Analog presets test complete!');
end;

procedure TestClassicChords;
begin
  WriteLn('=== Classic Synthesis - Chord Progressions ===');
  WriteLn('1. C Major Chord (Sine waves)');
  PlayChordClassic(NOTE_C4, NOTE_E4, NOTE_G4, 'sine');
  InterruptibleDelay(2500);

  WriteLn('2. A Minor Chord (Lead preset)');
  PlayChordClassic(NOTE_A4, NOTE_C5, NOTE_E4 * 2, 'lead');
  InterruptibleDelay(2500);

  WriteLn('3. F Major Chord (Pad preset)');
  PlayChordClassic(NOTE_F4, NOTE_A4, NOTE_C5, 'pad');
  InterruptibleDelay(3000);

  SmoothStopAll(500);
  WriteLn('Classic chord progressions complete!');
end;

procedure TestClassicScale;
begin
  WriteLn('=== Classic Synthesis - Musical Scale ===');
  WriteLn('Playing C Major scale (C4 to C5)');
  PlayScaleClassic(NOTE_C4, 'lead');

  SmoothStopAll(500);
  WriteLn('Scale demonstration complete!');
end;

// =========================================================================
// FM SYNTHESIS TESTS
// =========================================================================

procedure TestFMBasics;
begin
  WriteLn('=== FM Synthesis - Basic Algorithms ===');
  WriteLn('1. Electric Piano (Rhodes-style)');
  PlayEPiano(261.63); // C4
  InterruptibleDelay(2500);

  WriteLn('2. FM Brass (Horn section)');
  PlayFMBrass(349.23); // F4
  InterruptibleDelay(2500);

  WriteLn('3. FM Bell (Chime/Vibraphone)');
  PlayFMBell(523.25); // C5
  InterruptibleDelay(3000);

  SmoothStopAll(500);
  WriteLn('FM basics test complete!');
end;

procedure TestFMAdvanced;
begin
  WriteLn('=== FM Synthesis - Advanced Presets ===');
  WriteLn('1. FM Organ (Hammond-style)');
  PlayFMOrgan(440.00); // A4
  InterruptibleDelay(2500);

  WriteLn('2. FM Synth Lead (Aggressive)');
  PlayFMLead(659.25); // E5
  InterruptibleDelay(2000);

  WriteLn('3. FM Synth Bass (Deep)');
  PlayFMBass(82.41); // E2
  InterruptibleDelay(2500);

  SmoothStopAll(500);
  WriteLn('Advanced FM presets complete!');
end;

procedure TestFMChords;
begin
  WriteLn('=== FM Synthesis - Polyphonic Chords ===');
  WriteLn('1. Electric Piano Chord (C Major 7th)');
  PlayChordFM(NOTE_C4, NOTE_E4, NOTE_G4, 'epiano');
  InterruptibleDelay(3000);

  WriteLn('2. FM Brass Section (F Major)');
  PlayChordFM(NOTE_F4, NOTE_A4, NOTE_C5, 'brass');
  InterruptibleDelay(3000);

  WriteLn('3. Bell Choir (Am7)');
  PlayChordFM(NOTE_A4, NOTE_C5, NOTE_E4 * 2, 'bell');
  InterruptibleDelay(4000);

  SmoothStopAll(500);
  WriteLn('FM chord progressions complete!');
end;

procedure TestFMScale;
begin
  WriteLn('=== FM Synthesis - Musical Expressions ===');
  WriteLn('Playing C Major scale with Electric Piano');
  PlayScaleFM(NOTE_C4, 'epiano');

  SmoothStopAll(500);
  WriteLn('FM scale demonstration complete!');
end;

// =========================================================================
// WAVETABLE SYNTHESIS TESTS
// =========================================================================

procedure TestWavetableBasics;
begin
  WriteLn('=== Wavetable Synthesis - Modern Wavetables ===');
  WriteLn('1. Serum-style Wavetable (Modern)');
  PlaySerum(440.0);
  InterruptibleDelay(2500);

  WriteLn('2. Wasp-style Wavetable (Analog)');
  PlayWasp(329.63);
  InterruptibleDelay(2500);

  WriteLn('3. PPG-style Wavetable (Digital)');
  PlayPPG(261.63);
  InterruptibleDelay(2500);

  SmoothStopAll(500);
  WriteLn('Wavetable basics test complete!');
end;

procedure TestWavetableAdvanced;
begin
  WriteLn('=== Wavetable Synthesis - Advanced Techniques ===');
  WriteLn('1. Serum Lead (High frequency)');
  PlaySerum(880.0); // A5
  InterruptibleDelay(2000);

  WriteLn('2. Wasp Bass (Low frequency)');
  PlayWasp(110.0); // A2
  InterruptibleDelay(2500);

  WriteLn('3. PPG Pad (Mid frequency)');
  PlayPPG(220.0); // A3
  InterruptibleDelay(3000);

  SmoothStopAll(500);
  WriteLn('Advanced wavetable techniques complete!');
end;

procedure TestWavetableChords;
begin
  WriteLn('=== Wavetable Synthesis - Harmonic Progressions ===');
  WriteLn('1. Serum Chord (C Major)');
  PlayChordWavetable(NOTE_C4, NOTE_E4, NOTE_G4, 'serum');
  InterruptibleDelay(3000);

  WriteLn('2. Wasp Chord (D Minor)');
  PlayChordWavetable(NOTE_D4, NOTE_F4, NOTE_A4, 'wasp');
  InterruptibleDelay(3000);

  WriteLn('3. PPG Chord (F Major)');
  PlayChordWavetable(NOTE_F4, NOTE_A4, NOTE_C5, 'ppg');
  InterruptibleDelay(3500);

  SmoothStopAll(500);
  WriteLn('Wavetable chord progressions complete!');
end;

procedure TestWavetableScale;
begin
  WriteLn('=== Wavetable Synthesis - Melodic Sequences ===');
  WriteLn('Playing C Major scale with Serum wavetable');
  PlayScaleWavetable(NOTE_C4, 'serum');

  SmoothStopAll(500);
  WriteLn('Wavetable scale demonstration complete!');
end;

// =========================================================================
// WAVETABLE LOADING TESTS
// =========================================================================

procedure TestWavetableLoading;
var
  AFormats: TStringArray;
  i: Integer;
  AWavetablePath: string;
  ATestFile: string;
begin
  WriteLn('=== Wavetable Loading System Tests ===');

  WriteLn('1. Testing Supported Formats Detection');
  AFormats := GetWavetableFormats;
  Write('Supported formats: ');
  for i := 0 to Length(AFormats) - 1 do
  begin
    Write(AFormats[i]);
    if i < Length(AFormats) - 1 then Write(', ');
  end;
  WriteLn;

  WriteLn('2. Testing Directory Scanning');
  AWavetablePath := ConcatPaths([DataPath, 'wavetables']);
  WriteLn('  Scanning: ', AWavetablePath);
  if DirectoryExists(AWavetablePath) then
    ScanWavetableDirectory(AWavetablePath)
  else
    WriteLn('    Directory not found');

  WriteLn('3. Testing Cache System');
  WriteLn('  ', GetWavetableCacheInfo);

  WriteLn('4. Testing File Loading');
  // Test loading a real Serum wavetable
  ATestFile := ConcatPaths([DataPath, 'wavetables', 'linear_vae_v1.07_dims24_b256_697_free.wav']);
  WriteLn('  Testing Serum WAV loading:');
  if FileExists(ATestFile) then
  begin
    if LoadWavetableFile(ATestFile) then
      WriteLn('  OK: Serum wavetable loaded!')
    else
      WriteLn('  ERROR: Failed to load wavetable');
  end
  else
    WriteLn('  INFO: Test file not found (run option 17 first)');

  WriteLn('  Testing error handling:');
  if not LoadWavetableFile('non_existent_file.wav') then
    WriteLn('  OK: Error handling working');

  WriteLn('Wavetable loading system test complete!');
end;

procedure TestWavetableFileFormats;
var
  TestFiles: array[0..2] of string;
  i: Integer;
  ACount: Integer;
  AFilename: string;
begin
  WriteLn('=== Wavetable File Format Tests ===');

  WriteLn('1. Testing Format Detection');
  TestFiles[0] := 'test.wav';
  TestFiles[1] := 'linear_vae_v1.07_dims24_b256_697_free.wav';
  TestFiles[2] := 'bez_vae_v1.07_dims24_b256_73_free.wav';

  for i := 0 to Length(TestFiles) - 1 do
  begin
    AFilename := ConcatPaths([DataPath, 'wavetables', TestFiles[i]]);
    WriteLn('  File: ', TestFiles[i]);
    if FileExists(AFilename) then
    begin
      WriteLn('    > File exists, analyzing...');
      PrintWavetableInfo(AFilename);
    end
    else
      WriteLn('    File not found (normal for demo)');
  end;

  WriteLn('2. Testing Bulk Directory Loading');
  WriteLn('  Would load all supported files from directory...');
  ACount := LoadWavetableDirectory(ConcatPaths([DataPath, 'wavetables']));
  WriteLn('  Result: ', ACount, ' wavetables loaded');

  WriteLn('3. Current Cache Status');
  ListLoadedWavetables;

  WriteLn('File format testing complete!');
end;

procedure TestSerumWavetableDemo;
var
  AWavetablePath: string;
  ALoadedCount: Integer;
  ALoadedNames: TStringArray;
  i: Integer;
begin
  WriteLn('=== Serum Wavetable Demo ===');
  WriteLn('');
  WriteLn('This demo loads real wavetables in Serum-compatible WAV format.');
  WriteLn('Format: 2048 samples per frame, multiple frames stacked vertically.');
  WriteLn('License: Free wavetables (marked with _free suffix).');
  WriteLn('');

  // Look for wavetable files in data/wavetables
  AWavetablePath := ConcatPaths([DataPath, 'wavetables']);

  WriteLn('1. Scanning wavetable directory...');
  WriteLn('   Path: ', AWavetablePath);

  if not DirectoryExists(AWavetablePath) then
  begin
    WriteLn('   ERROR: Directory not found!');
    WriteLn('   Expected path: ', AWavetablePath);
    Exit;
  end;

  // Scan and display wavetable files
  ScanWavetableDirectory(AWavetablePath);

  WriteLn('');
  WriteLn('2. Loading wavetables from directory...');
  ALoadedCount := LoadWavetableDirectory(AWavetablePath);
  WriteLn('   Loaded ', ALoadedCount, ' wavetables');

  WriteLn('');
  WriteLn('3. Cache status:');
  WriteLn('   ', GetWavetableCacheInfo);

  // Get list of loaded wavetables
  ALoadedNames := GetLoadedWavetables;
  if Length(ALoadedNames) > 0 then
  begin
    WriteLn('   First 5 loaded wavetables:');
    for i := 0 to Min(4, Length(ALoadedNames) - 1) do
      WriteLn('     - ', ALoadedNames[i]);
    if Length(ALoadedNames) > 5 then
      WriteLn('     ... and ', Length(ALoadedNames) - 5, ' more');
  end;

  WriteLn('');
  WriteLn('4. Playing demo sounds with loaded wavetables...');

  // Play demo sounds
  WriteLn('   Playing low bass note (C2)...');
  PlaySerum(NOTE_C4 / 4);  // C2
  InterruptibleDelay(2000);

  WriteLn('   Playing mid-range chord (C4-E4-G4)...');
  PlayChordWavetable(NOTE_C4, NOTE_E4, NOTE_G4, 'serum');
  InterruptibleDelay(2500);

  WriteLn('   Playing high lead (A5)...');
  PlaySerum(NOTE_A4 * 2);  // A5
  InterruptibleDelay(2000);

  WriteLn('   Playing different wavetable types...');
  PlayPPG(NOTE_C4);
  InterruptibleDelay(1500);
  PlayWasp(NOTE_C4);
  InterruptibleDelay(1500);

  SmoothStopAll(500);

  WriteLn('');
  WriteLn('=== Serum Wavetable Demo Complete ===');
  WriteLn('');
  WriteLn('Wavetables loaded from open format (.wav) files.');
  WriteLn('These wavetables can be used in any Serum-compatible synthesizer.');
end;

// =========================================================================
// MULTI-ENGINE TESTS
// =========================================================================

procedure TestSynthesisComparison;
begin
  WriteLn('=== Synthesis Engine Comparison ===');
  WriteLn('Same note (A4 = 440 Hz) with different synthesis methods');

  WriteLn('1. Classic Analog Lead');
  PlayLead(440.0);
  InterruptibleDelay(2000);

  WriteLn('2. FM Electric Piano');
  PlayEPiano(440.0);
  InterruptibleDelay(2000);

  WriteLn('3. Wavetable Serum');
  PlaySerum(440.0);
  InterruptibleDelay(2000);

  SmoothStopAll(500);
  WriteLn('Synthesis comparison complete!');
end;

procedure TestPolyrhythms;
begin
  WriteLn('=== Multi-Engine Polyrhythms ===');
  WriteLn('1. Classic Bass Line + FM Lead');
  PlayBass(NOTE_C4 / 2); // C3
  InterruptibleDelay(200);
  PlayFMLead(NOTE_C5);
  InterruptibleDelay(600);

  WriteLn('2. Wavetable Chord + Classic Melody');
  PlayChordWavetable(NOTE_F4, NOTE_A4, NOTE_C5, 'serum');
  InterruptibleDelay(100);
  PlayLead(NOTE_F4 * 2); // F5
  InterruptibleDelay(700);

  WriteLn('3. FM Bells + Wavetable Pad');
  PlayFMBell(NOTE_G5);
  InterruptibleDelay(100);
  PlayWasp(NOTE_G4);
  InterruptibleDelay(1500);

  SmoothStopAll(500);
  WriteLn('Multi-engine polyrhythms complete!');
end;

// =========================================================================
// ALL TESTS SEQUENCE
// =========================================================================

procedure RunAllTests;
begin
  WriteLn('=== SEDAI AUDIO - COMPLETE DEMO SUITE ===');

  WriteLn('CLASSIC SYNTHESIS ENGINE');
  TestClassicWaveforms;
  InterruptibleDelay(1000);
  TestClassicPresets;
  InterruptibleDelay(1000);
  TestClassicChords;
  InterruptibleDelay(1000);
  TestClassicScale;
  InterruptibleDelay(2000);

  WriteLn('FM SYNTHESIS ENGINE');
  TestFMBasics;
  InterruptibleDelay(1000);
  TestFMAdvanced;
  InterruptibleDelay(1000);
  TestFMChords;
  InterruptibleDelay(1000);
  TestFMScale;
  InterruptibleDelay(2000);

  WriteLn('WAVETABLE SYNTHESIS ENGINE');
  TestWavetableBasics;
  InterruptibleDelay(1000);
  TestWavetableAdvanced;
  InterruptibleDelay(1000);
  TestWavetableChords;
  InterruptibleDelay(1000);
  TestWavetableScale;
  InterruptibleDelay(2000);

  WriteLn('WAVETABLE LOADING SYSTEM');
  TestWavetableLoading;
  InterruptibleDelay(1000);
  TestWavetableFileFormats;
  InterruptibleDelay(1000);
  TestSerumWavetableDemo;
  InterruptibleDelay(2000);

  WriteLn('MULTI-ENGINE DEMONSTRATIONS');
  TestSynthesisComparison;
  InterruptibleDelay(1000);
  TestPolyrhythms;
  InterruptibleDelay(2000);

  SmoothStopAll(500);
  WriteLn('COMPLETE DEMO SUITE FINISHED');
end;

// =========================================================================
// ENHANCED MENU SYSTEM WITH MIDI
// =========================================================================

procedure ClearScreen;
begin
  {$IFDEF WINDOWS}
  System.Write(#27'[2J'#27'[H');
  {$ELSE}
  System.Write(#27'[2J'#27'[H');
  {$ENDIF}
end;

procedure ShowMainMenu;
begin
  ClearScreen;
  WriteLn('SEDAI AUDIO + MIDI SYSTEM - Comprehensive Demo');
  WriteLn('Active Voices: ', GetActiveVoices, '/', GetMaxVoices, ' | Sample Rate: ', GetSampleRate, ' Hz');
  WriteLn('MIDI System: ', GET_MIDI_SYSTEM_INFO);
  WriteLn('Wavetable Cache: ', GetWavetableCacheInfo);
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn('CLASSIC SYNTHESIS              | FM SYNTHESIS');
  WriteLn(' 1. Basic Waveforms            |  5. FM Basics');
  WriteLn(' 2. Analog Presets             |  6. Advanced FM');
  WriteLn(' 3. Chord Progressions         |  7. FM Chord Progressions');
  WriteLn(' 4. Musical Scale              |  8. FM Musical Scale');
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn('WAVETABLE SYNTHESIS            | WAVETABLE LOADING');
  WriteLn(' 9. Wavetable Basics           | 15. Test Wavetable Loading');
  WriteLn('10. Advanced Wavetables        | 16. Test File Format Support');
  WriteLn('11. Wavetable Chord Prog.      | 17. Serum Wavetable Demo');
  WriteLn('12. Wavetable Musical Scale    | 18. Clear Wavetable Cache');
  WriteLn('--------------------------------------------------------------------------------');
  WriteLn('MULTI-ENGINE                   | MIDI SYSTEM');
  WriteLn('13. Synthesis Comparison       | 20. Initialize MIDI System');
  WriteLn('14. Multi-Engine Polyrhythms   | 21. Load MIDI File');
  WriteLn('                               | 22. MIDI Playback Control');
  WriteLn('SYSTEM                         | 23. MIDI Channel Mapping');
  WriteLn('90. Run All Audio Demos        | 24. MIDI Tempo Control');
  WriteLn('98. System Status              | 25. Complete MIDI Demo');
  WriteLn('99. Exit                       |');
  WriteLn('--------------------------------------------------------------------------------');
  Write('Select option: ');
end;

// =========================================================================
// MAIN PROGRAM WITH MIDI INTEGRATION
// =========================================================================

var
  Choice: String;
  Continue: Boolean;

begin
  // Setup CTRL+C handler for clean shutdown
  SetupCtrlCHandler;

  AppPath := ExtractFilePath(ParamStr(0));
  DataPath := FindDataPath;

  WriteLn('Initializing SEDAI Audio System with MIDI Support...');
  WriteLn('(Press CTRL+C at any time to exit)');
  WriteLn('');
  WriteLn('Data path: ', DataPath);
  WriteLn('');
  if not InitAudio(64) then
  begin
    WriteLn('ERROR: Failed to initialize audio system');
    WriteLn('Press ENTER to exit...');
    ReadLn;
    Halt(1);
  end;

  Continue := True;
  while Continue do
  begin
    ShowMainMenu;
    ReadLn(Choice);
    WriteLn('');

    try
      case Choice of
        // Classic Synthesis (1-4)
        '1': TestClassicWaveforms;
        '2': TestClassicPresets;
        '3': TestClassicChords;
        '4': TestClassicScale;

        // FM Synthesis (5-8)
        '5': TestFMBasics;
        '6': TestFMAdvanced;
        '7': TestFMChords;
        '8': TestFMScale;

        // Wavetable Synthesis (9-12)
        '9': TestWavetableBasics;
        '10': TestWavetableAdvanced;
        '11': TestWavetableChords;
        '12': TestWavetableScale;

        // Multi-Engine (13-14)
        '13': TestSynthesisComparison;
        '14': TestPolyrhythms;

        // Wavetable Loading (15-18)
        '15': TestWavetableLoading;
        '16': TestWavetableFileFormats;
        '17': TestSerumWavetableDemo;
        '18': begin
          ClearWavetableCache;
          WriteLn('Wavetable cache cleared!');
        end;

        // MIDI System (20-25)
        '20': TestMIDISystem;
        '21': TestMIDIFileLoading;
        '22': TestMIDIPlayback;
        '23': TestMIDIChannelMapping;
        '24': TestMIDITempoControl;
        '25': RunMIDIDemo;

        // System (90, 98, 99)
        '90': begin
          RunAllTests;
          WriteLn('');
          WriteLn('Running MIDI demo...');
          RunMIDIDemo;
        end;

        '98': begin
          WriteLn('=== SEDAI Audio + MIDI System Status ===');
          PrintStatus;
          WriteLn('Architecture: Modular Synthesis Engine + MIDI Sequencer');
          WriteLn('- Classic Processor: Subtractive synthesis');
          WriteLn('- FM Processor: Digital frequency modulation');
          WriteLn('- Wavetable Processor: Modern wavetable synthesis');
          WriteLn('- Wavetable Loader: Multi-format support');
          WriteLn('  Supported: Serum, Vital, Surge, Generic WAV');
          WriteLn('  Cache: ', GetWavetableCacheInfo);
          WriteLn('- MIDI System: Real-time MIDI file playback');
          WriteLn('- Integration: MIDI -> Wavetable synthesis');
          if IS_MIDI_INITIALIZED then
          begin
            WriteLn('');
            PRINT_MIDI_STATUS;
          end
          else
            WriteLn('MIDI System: Not initialized (use option 20)');
        end;

        '99': Continue := False;

        else
        begin
          WriteLn('Invalid choice. Try again.');
          SDL_Delay(1000);
        end;
      end;
    except
      on EAbort do
      begin
        // User interrupted with CTRL+C - return to menu
        GTerminateRequested := False;  // Reset for next demo
        WriteLn('');
        WriteLn('Returned to menu.');
      end;
    end;

    if Continue and (Choice <> '99') then
    begin
      WriteLn('');
      WriteLn('Press ENTER to return to menu...');
      ReadLn;
      SmoothStopAll(500);
    end;
  end;

  WriteLn('Shutting down SEDAI Audio + MIDI System...');
  SHUTDOWN_MIDI;
  ShutdownAudio;
  WriteLn('Thank you for using SEDAI Audio with MIDI!');
end.
