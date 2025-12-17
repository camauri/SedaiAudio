{*
 * Sedai Audio Foundation - SID Evo Full Demo
 * Comprehensive demonstration of all SIDEvo capabilities
 * Including SIDDump file playback
 *}

program demo_sidevo_full;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes, Math, SDL2, SedaiSIDEvo, SedaiSIDDumpPlayer;

var
  GRunning: Boolean = True;
  GEscapePressed: Boolean = False;
  GQuitRequested: Boolean = False;
  GLastFrameTime: UInt32;

// ============================================================================
// WINDOWS CONSOLE KEYBOARD INPUT
// Using Windows console API for non-blocking key detection in console apps
// ============================================================================

{$IFDEF WINDOWS}
var
  GConsoleHandle: THandle;
  GConsoleMode: DWORD;
  GOriginalConsoleMode: DWORD;

procedure InitConsoleInput;
begin
  GConsoleHandle := GetStdHandle(STD_INPUT_HANDLE);
  if GConsoleHandle <> INVALID_HANDLE_VALUE then
  begin
    GetConsoleMode(GConsoleHandle, @GOriginalConsoleMode);
    // Disable line input and echo for non-blocking reads
    GConsoleMode := GOriginalConsoleMode and (not (ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT));
  end;
end;

procedure RestoreConsoleInput;
begin
  if GConsoleHandle <> INVALID_HANDLE_VALUE then
    SetConsoleMode(GConsoleHandle, GOriginalConsoleMode);
end;

// Check if a key is available in the console buffer (non-blocking)
function ConsoleKeyPressed: Boolean;
var
  ANumEvents: DWORD;
  AInputRec: INPUT_RECORD;
  AEventsRead: DWORD;
begin
  Result := False;
  if GConsoleHandle = INVALID_HANDLE_VALUE then Exit;

  if GetNumberOfConsoleInputEvents(GConsoleHandle, @ANumEvents) then
  begin
    while ANumEvents > 0 do
    begin
      if PeekConsoleInput(GConsoleHandle, @AInputRec, 1, @AEventsRead) then
      begin
        if (AEventsRead > 0) and (AInputRec.EventType = KEY_EVENT) and
           (AInputRec.Event.KeyEvent.bKeyDown) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          // Remove non-key events from buffer
          ReadConsoleInput(GConsoleHandle, @AInputRec, 1, @AEventsRead);
        end;
      end;
      Dec(ANumEvents);
    end;
  end;
end;

// Read a key from console (non-blocking, returns #0 if no key)
function ConsoleReadKey: Char;
var
  AInputRec: INPUT_RECORD;
  AEventsRead: DWORD;
  AVk: Word;
  ACtrl: Boolean;
begin
  Result := #0;
  if GConsoleHandle = INVALID_HANDLE_VALUE then Exit;

  if ConsoleKeyPressed then
  begin
    if ReadConsoleInput(GConsoleHandle, @AInputRec, 1, @AEventsRead) then
    begin
      if (AEventsRead > 0) and (AInputRec.EventType = KEY_EVENT) and
         (AInputRec.Event.KeyEvent.bKeyDown) then
      begin
        AVk := AInputRec.Event.KeyEvent.wVirtualKeyCode;
        ACtrl := (AInputRec.Event.KeyEvent.dwControlKeyState and
                  (LEFT_CTRL_PRESSED or RIGHT_CTRL_PRESSED)) <> 0;

        // CTRL+C
        if ACtrl and (AVk = Ord('C')) then
          Result := #3
        // ESC
        else if AVk = VK_ESCAPE then
          Result := #27
        // Regular character
        else if AInputRec.Event.KeyEvent.AsciiChar <> #0 then
          Result := AInputRec.Event.KeyEvent.AsciiChar;
      end;
    end;
  end;
end;
{$ELSE}
// Non-Windows: Stub implementations (would need termios on Unix)
procedure InitConsoleInput;
begin
end;

procedure RestoreConsoleInput;
begin
end;

function ConsoleKeyPressed: Boolean;
begin
  Result := False;
end;

function ConsoleReadKey: Char;
begin
  Result := #0;
end;
{$ENDIF}

// Immediate exit without confirmation
procedure ImmediateExit;
begin
  WriteLn('');
  WriteLn('Exiting...');
  SIDEvoStopAll;
  SDL_Delay(100);
  SIDEvoShutdown;
  {$IFDEF WINDOWS}
  RestoreConsoleInput;
  {$ENDIF}
  Halt(0);
end;

// Check for ESC or CTRL+C via console (non-blocking)
// Returns: 0 = nothing, 1 = ESC pressed, 2 = CTRL+C pressed
function CheckConsoleInput: Integer;
var
  AKey: Char;
begin
  Result := 0;
  AKey := ConsoleReadKey;
  case AKey of
    #27: Result := 1;  // ESC
    #3:  Result := 2;  // CTRL+C
  end;
end;

// Check for ESC or CTRL+C, returns True if should abort current demo
function CheckInput: Boolean;
var
  AResult: Integer;
begin
  Result := False;
  AResult := CheckConsoleInput;
  case AResult of
    1: // ESC
      begin
        GEscapePressed := True;
        Result := True;
      end;
    2: // CTRL+C
      begin
        GQuitRequested := True;
        ImmediateExit;
      end;
  end;
end;

// Wait with input checking - returns True if interrupted
function WaitMsInterruptible(AMs: Integer): Boolean;
var
  AStartTime, AElapsed: UInt32;
begin
  Result := False;
  AStartTime := SDL_GetTicks;

  while True do
  begin
    if CheckInput then
    begin
      Result := True;
      SIDEvoStopAll;
      Exit;
    end;

    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= UInt32(AMs) then
      Break;

    SDL_Delay(10); // Check every 10ms
  end;
end;

procedure WaitMs(AMs: Integer);
begin
  WaitMsInterruptible(AMs);
end;

// Forward declaration
procedure CreateTestDumpFile(const AFilename: string); forward;

// Check if demo should abort
function ShouldAbortDemo: Boolean;
begin
  Result := GEscapePressed or GQuitRequested;
end;

procedure WaitForKey;
begin
  WriteLn('');
  Write('Press ENTER to continue...');
  ReadLn;
  // Reset escape flag after menu interaction
  GEscapePressed := False;
end;

// Confirm quit dialog
function ConfirmQuit: Boolean;
var
  AResponse: string;
begin
  WriteLn('');
  Write('Do you want to quit? (Y/N) ');
  ReadLn(AResponse);
  AResponse := UpperCase(Trim(AResponse));
  Result := (AResponse = 'Y') or (AResponse = 'YES');
end;

procedure PrintHeader(const ATitle: string);
begin
  WriteLn('');
  WriteLn('================================================================');
  WriteLn('  ', ATitle);
  WriteLn('================================================================');
  WriteLn('');
end;

procedure PrintSubHeader(const ATitle: string);
begin
  WriteLn('');
  WriteLn('--- ', ATitle, ' ---');
  WriteLn('');
end;

// ============================================================================
// DEMO 1: Waveform Showcase
// ============================================================================
procedure Demo_Waveforms;
const
  DEMO_NOTE = 48; // C4
  DEMO_DURATION = 600;
begin
  PrintHeader('DEMO 1: WAVEFORM SHOWCASE');
  WriteLn('The SID Evo supports all classic SID waveforms plus Sine.');
  WriteLn('');

  // Triangle - The classic mellow SID sound
  WriteLn('  [1/5] TRIANGLE - Mellow, flute-like tone');
  SIDEvoWave(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvoEnvelope(0, 0.01, 0.2, 0.7, 0.3);
  SIDEvoPlay(0, DEMO_NOTE, 0.7);
  WaitMs(DEMO_DURATION);
  SIDEvoStop(0);
  WaitMs(200);

  // Sawtooth - Rich harmonics
  WriteLn('  [2/5] SAWTOOTH - Rich, brassy tone');
  SIDEvoWave(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoPlay(0, DEMO_NOTE, 0.7);
  WaitMs(DEMO_DURATION);
  SIDEvoStop(0);
  WaitMs(200);

  // Pulse/Square - Classic chiptune
  WriteLn('  [3/5] PULSE - Classic square wave');
  SIDEvoWave(0, SIDEVO_WAVE_PULSE);
  SIDEvoPlay(0, DEMO_NOTE, 0.7);
  WaitMs(DEMO_DURATION);
  SIDEvoStop(0);
  WaitMs(200);

  // Noise - For percussion/effects
  WriteLn('  [4/5] NOISE - White noise for drums/effects');
  SIDEvoWave(0, SIDEVO_WAVE_NOISE);
  SIDEvoPlay(0, DEMO_NOTE, 0.5);
  WaitMs(DEMO_DURATION);
  SIDEvoStop(0);
  WaitMs(200);

  // Sine - Extension (not in original SID)
  WriteLn('  [5/5] SINE - Pure tone (SIDEvo extension)');
  SIDEvoWave(0, SIDEVO_WAVE_SINE);
  SIDEvoPlay(0, DEMO_NOTE, 0.7);
  WaitMs(DEMO_DURATION);
  SIDEvoStop(0);
  WaitMs(300);

  WriteLn('');
  WriteLn('  Waveform demo complete!');
end;

// ============================================================================
// DEMO 2: ADSR Envelopes
// ============================================================================
procedure Demo_ADSR;
begin
  PrintHeader('DEMO 2: ADSR ENVELOPES');
  WriteLn('The SID uses Attack-Decay-Sustain-Release envelopes.');
  WriteLn('');

  SIDEvoWave(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoPan(0, 0.0);

  // Pluck/Staccato
  PrintSubHeader('Pluck - Fast Attack, No Sustain');
  SIDEvoEnvelope(0, 0.0, 0.15, 0.0, 0.1);
  SIDEvoPlay(0, 60, 0.8); WaitMs(300);
  SIDEvoPlay(0, 64, 0.8); WaitMs(300);
  SIDEvoPlay(0, 67, 0.8); WaitMs(300);
  SIDEvoPlay(0, 72, 0.8); WaitMs(500);
  SIDEvoStop(0);
  WaitMs(300);

  // Piano-like
  PrintSubHeader('Piano - Fast Attack, Long Decay');
  SIDEvoEnvelope(0, 0.0, 0.5, 0.3, 0.4);
  SIDEvoPlay(0, 48, 0.8);
  WaitMs(1500);
  SIDEvoStop(0);
  WaitMs(600);

  // Pad/Strings
  PrintSubHeader('Pad - Slow Attack, High Sustain');
  SIDEvoEnvelope(0, 0.7, 0.3, 0.8, 0.6);
  SIDEvoPlay(0, 55, 0.7);
  WaitMs(2500);
  SIDEvoStop(0);
  WaitMs(1000);

  // Organ
  PrintSubHeader('Organ - Instant Attack, Full Sustain');
  SIDEvoEnvelope(0, 0.0, 0.0, 1.0, 0.05);
  SIDEvoPlay(0, 60, 0.6);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(300);

  WriteLn('');
  WriteLn('  ADSR demo complete!');
end;

// ============================================================================
// DEMO 3: Stereo Panning
// ============================================================================
procedure Demo_Stereo;
var
  i: Integer;
  APan: Single;
begin
  PrintHeader('DEMO 3: STEREO PANNING');
  WriteLn('SIDEvo extends the mono SID to full stereo.');
  WriteLn('');

  SIDEvoWave(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoEnvelope(0, 0.01, 0.15, 0.6, 0.2);

  // Ping pong panning
  PrintSubHeader('Ping-Pong Panning');

  for i := 1 to 8 do
  begin
    if (i mod 2) = 1 then
      APan := -0.8
    else
      APan := 0.8;

    SIDEvoPan(0, APan);
    SIDEvoPlay(0, 55 + (i mod 4) * 2, 0.7);
    WaitMs(250);
  end;
  SIDEvoStop(0);
  WaitMs(300);

  // Smooth pan sweep
  PrintSubHeader('Smooth Pan Sweep');
  SIDEvoPlay(0, 60, 0.7);

  for i := 0 to 60 do
  begin
    APan := Sin(i * 0.2) * 0.9;
    SIDEvoPan(0, APan);
    WaitMs(40);
  end;

  SIDEvoStop(0);
  SIDEvoPan(0, 0.0);
  WaitMs(300);

  WriteLn('');
  WriteLn('  Stereo demo complete!');
end;

// ============================================================================
// DEMO 4: Polyphony (8 voices)
// ============================================================================
procedure Demo_Polyphony;
var
  i: Integer;
begin
  PrintHeader('DEMO 4: POLYPHONY (8 VOICES)');
  WriteLn('SIDEvo extends the 3-voice SID to 8 voices.');
  WriteLn('');

  // Setup voices with different waveforms and pans
  for i := 0 to 7 do
  begin
    case i mod 4 of
      0: SIDEvoWave(i, SIDEVO_WAVE_TRIANGLE);
      1: SIDEvoWave(i, SIDEVO_WAVE_SAWTOOTH);
      2: SIDEvoWave(i, SIDEVO_WAVE_PULSE);
      3: SIDEvoWave(i, SIDEVO_WAVE_SINE);
    end;
    SIDEvoEnvelope(i, 0.1, 0.2, 0.7, 0.4);
    // Spread across stereo field
    SIDEvoPan(i, -0.9 + (i * 0.25));
  end;

  // Play C major 9 chord (C-E-G-B-D)
  PrintSubHeader('8-Voice Chord');
  WriteLn('  Playing C major 9 chord spread across all voices...');

  SIDEvoPlay(0, 36, 0.5);  // C2
  WaitMs(100);
  SIDEvoPlay(1, 40, 0.5);  // E2
  WaitMs(100);
  SIDEvoPlay(2, 43, 0.5);  // G2
  WaitMs(100);
  SIDEvoPlay(3, 47, 0.5);  // B2
  WaitMs(100);
  SIDEvoPlay(4, 48, 0.6);  // C3
  WaitMs(100);
  SIDEvoPlay(5, 52, 0.5);  // E3
  WaitMs(100);
  SIDEvoPlay(6, 55, 0.5);  // G3
  WaitMs(100);
  SIDEvoPlay(7, 62, 0.4);  // D4

  WaitMs(3000);
  SIDEvoStopAll;
  WaitMs(500);

  WriteLn('');
  WriteLn('  Polyphony demo complete!');
end;

// ============================================================================
// DEMO 5: Filter Sweep
// ============================================================================
procedure Demo_Filter;
var
  i: Integer;
  ACutoff: Single;
begin
  PrintHeader('DEMO 5: FILTER SWEEP');
  WriteLn('SIDEvo supports per-voice filters (LP, HP, BP).');
  WriteLn('');

  SIDEvoWave(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoEnvelope(0, 0.01, 0.1, 0.9, 0.3);
  SIDEvoPan(0, 0.0);

  // Low-pass filter sweep
  PrintSubHeader('Low-Pass Filter Sweep');
  SIDEvoFilter(0, SIDEVO_FILTER_LOWPASS, 0.1, 0.6);
  SIDEvoPlay(0, 36, 0.8);

  for i := 0 to 80 do
  begin
    ACutoff := i / 80.0;
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);
    WaitMs(30);
  end;

  for i := 80 downto 0 do
  begin
    ACutoff := i / 80.0;
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);
    WaitMs(30);
  end;

  SIDEvoStop(0);
  WaitMs(300);

  // High-pass filter sweep
  PrintSubHeader('High-Pass Filter Sweep');
  SIDEvoFilter(0, SIDEVO_FILTER_HIGHPASS, 0.0, 0.5);
  SIDEvoPlay(0, 36, 0.8);

  for i := 0 to 60 do
  begin
    ACutoff := i / 80.0;
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);
    WaitMs(40);
  end;

  SIDEvoStop(0);
  SIDEvo.SetVoiceFilter(0, False);
  WaitMs(300);

  WriteLn('');
  WriteLn('  Filter demo complete!');
end;

// ============================================================================
// DEMO 6: Extended Range (11 Octaves - Full Audible Spectrum)
// ============================================================================
procedure Demo_Range;
var
  i: Integer;
begin
  PrintHeader('DEMO 6: EXTENDED RANGE (11 OCTAVES)');
  WriteLn('SIDEvo covers the full audible spectrum: C0 (16Hz) to B10 (20kHz).');
  WriteLn('');

  SIDEvoWave(0, SIDEVO_WAVE_SINE);
  SIDEvoEnvelope(0, 0.01, 0.1, 0.7, 0.15);
  SIDEvoPan(0, 0.0);

  // Play C in each octave (0-10 = 11 octaves)
  for i := 0 to 10 do
  begin
    WriteLn('  Octave ', i, ': ', SIDEvo.GetNoteName(i * 12),
            ' (', SIDEvo.NoteToFrequency(i * 12):0:1, ' Hz)');
    SIDEvoPlay(0, i * 12, 0.6);
    WaitMs(300);
    SIDEvoStop(0);
    WaitMs(50);
  end;

  WaitMs(300);
  WriteLn('');
  WriteLn('  Range demo complete! (16 Hz to ~20 kHz covered)');
end;

// ============================================================================
// DEMO 7: Classic SID Arpeggio
// ============================================================================
procedure Demo_Arpeggio;
var
  i, ANoteIndex: Integer;
  AChord: array[0..3] of Integer;
begin
  PrintHeader('DEMO 7: CLASSIC SID ARPEGGIO');
  WriteLn('The iconic SID sound - fast arpeggios!');
  WriteLn('');

  // C minor chord
  AChord[0] := 48;  // C
  AChord[1] := 51;  // Eb
  AChord[2] := 55;  // G
  AChord[3] := 60;  // C (octave)

  SIDEvoWave(0, SIDEVO_WAVE_PULSE);
  SIDEvoEnvelope(0, 0.0, 0.1, 0.5, 0.1);
  SIDEvoPan(0, 0.0);

  WriteLn('  Playing C minor arpeggio...');

  for i := 0 to 47 do
  begin
    ANoteIndex := i mod 4;
    SIDEvoPlay(0, AChord[ANoteIndex], 0.7);
    WaitMs(70);
  end;

  SIDEvoStop(0);
  WaitMs(300);

  // Faster arpeggio with different chord
  WriteLn('  Playing faster G major arpeggio...');

  AChord[0] := 43;  // G
  AChord[1] := 47;  // B
  AChord[2] := 50;  // D
  AChord[3] := 55;  // G

  for i := 0 to 63 do
  begin
    ANoteIndex := i mod 4;
    SIDEvoPlay(0, AChord[ANoteIndex], 0.7);
    WaitMs(50);
  end;

  SIDEvoStop(0);
  WaitMs(300);

  WriteLn('');
  WriteLn('  Arpeggio demo complete!');
end;

// ============================================================================
// DEMO 8: Music - Short Composition
// ============================================================================
procedure Demo_Music;
type
  TNote = record
    Voice: Integer;
    Note: Integer;
    Duration: Integer;
    Rest: Integer;
  end;
const
  // Simple melody with bass and lead
  MELODY_LENGTH = 32;
var
  i: Integer;
begin
  PrintHeader('DEMO 8: MUSICAL COMPOSITION');
  WriteLn('A short piece demonstrating multi-voice composition.');
  WriteLn('');

  // Setup voices
  // Voice 0: Bass (triangle)
  SIDEvoWave(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvoEnvelope(0, 0.01, 0.2, 0.6, 0.2);
  SIDEvoPan(0, -0.3);

  // Voice 1: Lead (pulse)
  SIDEvoWave(1, SIDEVO_WAVE_PULSE);
  SIDEvoEnvelope(1, 0.0, 0.15, 0.5, 0.15);
  SIDEvoPan(1, 0.3);

  // Voice 2: Arpeggio (sawtooth)
  SIDEvoWave(2, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoEnvelope(2, 0.0, 0.08, 0.4, 0.1);
  SIDEvoPan(2, 0.0);

  WriteLn('  Playing composition...');
  WriteLn('');

  // Bar 1-2: C minor
  // Bass
  SIDEvoPlay(0, 36, 0.6); // C2
  // Lead melody
  SIDEvoPlay(1, 60, 0.7); // C4
  WaitMs(400);
  SIDEvoPlay(1, 63, 0.7); // Eb4
  WaitMs(400);
  SIDEvoPlay(1, 67, 0.7); // G4
  WaitMs(400);
  SIDEvoPlay(1, 63, 0.6); // Eb4
  WaitMs(400);

  // Bar 3-4: Ab major
  SIDEvoPlay(0, 32, 0.6); // Ab1
  SIDEvoPlay(1, 60, 0.7); // C4
  WaitMs(400);
  SIDEvoPlay(1, 63, 0.7); // Eb4
  WaitMs(400);
  SIDEvoPlay(1, 68, 0.7); // Ab4
  WaitMs(400);
  SIDEvoPlay(1, 63, 0.6); // Eb4
  WaitMs(400);

  // Bar 5-6: Bb major
  SIDEvoPlay(0, 34, 0.6); // Bb1
  SIDEvoPlay(1, 62, 0.7); // D4
  WaitMs(400);
  SIDEvoPlay(1, 65, 0.7); // F4
  WaitMs(400);
  SIDEvoPlay(1, 70, 0.7); // Bb4
  WaitMs(400);
  SIDEvoPlay(1, 65, 0.6); // F4
  WaitMs(400);

  // Bar 7-8: G major -> C minor (cadence)
  SIDEvoPlay(0, 31, 0.6); // G1
  SIDEvoPlay(1, 67, 0.8); // G4
  WaitMs(600);
  SIDEvoPlay(1, 71, 0.8); // B4
  WaitMs(200);
  SIDEvoPlay(0, 36, 0.6); // C2
  SIDEvoPlay(1, 72, 0.9); // C5
  WaitMs(800);

  // Final chord
  SIDEvoPlay(2, 48, 0.5); // C3
  WaitMs(100);
  SIDEvoPlay(2, 51, 0.5); // Eb3
  WaitMs(100);
  SIDEvoPlay(2, 55, 0.5); // G3

  WaitMs(1500);

  SIDEvoStopAll;
  WaitMs(500);

  WriteLn('');
  WriteLn('  Composition demo complete!');
end;

// ============================================================================
// DEMO 9: Voice Groups (8-64 voices)
// ============================================================================
procedure Demo_VoiceGroups;
var
  i, g: Integer;
begin
  PrintHeader('DEMO 9: VOICE GROUPS (8-64 VOICES)');
  WriteLn('SIDEvo supports 8 to 64 voices in groups of 8.');
  WriteLn('Each group can have its own volume, pan, and transposition.');
  WriteLn('');

  // Start with 1 group (default)
  SIDEvoSetGroups(1);
  WriteLn('  Currently: 1 group (8 voices)');
  WaitMs(500);

  // Enable 2 groups
  SIDEvoSetGroups(2);
  WriteLn('  Expanded to: 2 groups (16 voices)');

  // Setup group 0: Left side, normal pitch
  SIDEvoGroupVol(0, 0.8);
  SIDEvoGroupPan(0, -0.6);
  SIDEvo.SetGroupTranspose(0, 0);

  // Setup group 1: Right side, octave up
  SIDEvoGroupVol(1, 0.7);
  SIDEvoGroupPan(1, 0.6);
  SIDEvo.SetGroupTranspose(1, 12);  // One octave up

  // Setup voices
  for i := 0 to 7 do
  begin
    SIDEvoWave(i, SIDEVO_WAVE_TRIANGLE);
    SIDEvoEnvelope(i, 0.05, 0.2, 0.6, 0.3);
  end;
  for i := 8 to 15 do
  begin
    SIDEvoWave(i, SIDEVO_WAVE_SAWTOOTH);
    SIDEvoEnvelope(i, 0.01, 0.15, 0.5, 0.2);
  end;

  WriteLn('');
  WriteLn('  Playing chord: Group 0 (left, base pitch) + Group 1 (right, +1 octave)');

  // Play C minor chord on both groups
  // Group 0 (voices 0-7)
  SIDEvoPlay(0, 48, 0.7);  // C3
  SIDEvoPlay(1, 51, 0.6);  // Eb3
  SIDEvoPlay(2, 55, 0.6);  // G3

  // Group 1 (voices 8-15) - will be transposed +12 automatically
  SIDEvoPlay(8, 48, 0.6);  // Becomes C4
  SIDEvoPlay(9, 51, 0.5);  // Becomes Eb4
  SIDEvoPlay(10, 55, 0.5); // Becomes G4

  WaitMs(2500);

  SIDEvoStopAll;
  WaitMs(300);

  // Demonstrate 4 groups
  WriteLn('');
  WriteLn('  Expanding to 4 groups (32 voices)...');
  SIDEvoSetGroups(4);

  // Setup 4 groups across stereo field
  SIDEvoGroupPan(0, -0.9);
  SIDEvoGroupPan(1, -0.3);
  SIDEvoGroupPan(2, 0.3);
  SIDEvoGroupPan(3, 0.9);

  // Different transpositions
  SIDEvo.SetGroupTranspose(0, -12);  // Octave down
  SIDEvo.SetGroupTranspose(1, 0);    // Normal
  SIDEvo.SetGroupTranspose(2, 7);    // Fifth up
  SIDEvo.SetGroupTranspose(3, 12);   // Octave up

  for g := 0 to 3 do
  begin
    SIDEvoWave(g * 8, SIDEVO_WAVE_SINE);
    SIDEvoEnvelope(g * 8, 0.1, 0.2, 0.7, 0.4);
  end;

  WriteLn('  Playing note across 4 groups with different transpositions...');

  // Play same note on first voice of each group
  SIDEvoPlay(0, 60, 0.5);   // Group 0: C4 - 1 octave = C3
  SIDEvoPlay(8, 60, 0.5);   // Group 1: C4
  SIDEvoPlay(16, 60, 0.5);  // Group 2: C4 + fifth = G4
  SIDEvoPlay(24, 60, 0.5);  // Group 3: C4 + octave = C5

  WaitMs(3000);

  SIDEvoStopAll;

  // Reset to 1 group
  SIDEvoSetGroups(1);
  WaitMs(300);

  WriteLn('');
  WriteLn('  Voice groups demo complete!');
end;

// ============================================================================
// DEMO 10: Extended Waveforms
// ============================================================================
procedure Demo_ExtendedWaveforms;
begin
  PrintHeader('DEMO 10: EXTENDED WAVEFORMS');
  WriteLn('SIDEvo adds modern waveforms beyond the original SID.');
  WriteLn('');

  SIDEvoEnvelope(0, 0.01, 0.3, 0.6, 0.3);
  SIDEvoPan(0, 0.0);

  // Classic SID waveforms
  WriteLn('  --- Classic SID Waveforms ---');
  WriteLn('');

  WriteLn('  [1] TRIANGLE - Mellow, flute-like');
  SIDEvoWave(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvoPlay(0, 60, 0.7);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [2] SAWTOOTH - Rich, brassy');
  SIDEvoWave(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoPlay(0, 60, 0.7);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [3] PULSE - Classic chiptune square');
  SIDEvoWave(0, SIDEVO_WAVE_PULSE);
  SIDEvoPlay(0, 60, 0.7);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [4] NOISE - Percussion/effects');
  SIDEvoWave(0, SIDEVO_WAVE_NOISE);
  SIDEvoPlay(0, 60, 0.5);
  WaitMs(600);
  SIDEvoStop(0);
  WaitMs(200);

  // Extended waveforms
  WriteLn('');
  WriteLn('  --- SIDEvo Extended Waveforms ---');
  WriteLn('');

  WriteLn('  [5] SINE - Pure fundamental tone');
  SIDEvoWave(0, SIDEVO_WAVE_SINE);
  SIDEvoPlay(0, 60, 0.7);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [6] SUPERSAW - Multiple detuned saws (trance/EDM)');
  SIDEvoWave(0, SIDEVO_WAVE_SUPERSAW);
  SIDEvo.SetSupersawDetune(0, 0.3);
  SIDEvo.SetSupersawMix(0, 0.5);
  SIDEvoPlay(0, 48, 0.7);
  WaitMs(1200);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [7] PWM - Auto-modulating pulse width');
  SIDEvoWave(0, SIDEVO_WAVE_PWM);
  SIDEvoPlay(0, 55, 0.7);
  WaitMs(1000);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [8] HALFSIN - Half-rectified sine (bass)');
  SIDEvoWave(0, SIDEVO_WAVE_HALFSIN);
  SIDEvoPlay(0, 36, 0.8);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [9] FULLSIN - Full-rectified sine (warm distortion)');
  SIDEvoWave(0, SIDEVO_WAVE_FULLSIN);
  SIDEvoPlay(0, 48, 0.7);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [10] FORMANT - Vocal-like resonant tone');
  SIDEvoWave(0, SIDEVO_WAVE_FORMANT);
  SIDEvoPlay(0, 55, 0.7);
  WaitMs(1000);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [11] METALLIC - Bell-like, inharmonic');
  SIDEvoWave(0, SIDEVO_WAVE_METALLIC);
  SIDEvoPlay(0, 60, 0.6);
  WaitMs(1200);
  SIDEvoStop(0);
  WaitMs(200);

  // Waveform combinations
  WriteLn('');
  WriteLn('  --- Waveform Combinations ---');
  WriteLn('');

  WriteLn('  [12] TRIANGLE + SAWTOOTH combined');
  SIDEvoWave(0, SIDEVO_WAVE_TRIANGLE or SIDEVO_WAVE_SAWTOOTH);
  SIDEvoPlay(0, 55, 0.6);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(150);

  WriteLn('  [13] PULSE + TRIANGLE combined');
  SIDEvoWave(0, SIDEVO_WAVE_PULSE or SIDEVO_WAVE_TRIANGLE);
  SIDEvoPlay(0, 55, 0.6);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(200);

  // Reset to triangle
  SIDEvoWave(0, SIDEVO_WAVE_TRIANGLE);

  WriteLn('');
  WriteLn('  Note: CUSTOM waveform requires user-defined wavetable.');
  WriteLn('');
  WriteLn('  Extended waveforms demo complete!');
end;

// ============================================================================
// DEMO 11: SIDDump File Playback
// ============================================================================
procedure Demo_SIDDump;
var
  APlayer: TSedaiSIDDumpPlayer;
  AFrameInterval: UInt32;
  ACurrentTime, ALastTime: UInt32;
  ADumpFile: string;
begin
  PrintHeader('DEMO 9: SIDDUMP FILE PLAYBACK');
  WriteLn('Playing SID register dumps - real C64 music!');
  WriteLn('');

  ADumpFile := 'test_music.dmp';

  // Check if dump file exists
  if not FileExists(ADumpFile) then
  begin
    WriteLn('  No dump file found. Creating a test dump...');
    WriteLn('');

    // Create a simple test dump programmatically
    CreateTestDumpFile(ADumpFile);
  end;

  if not FileExists(ADumpFile) then
  begin
    WriteLn('  ERROR: Could not create test dump file.');
    WriteLn('');
    WriteLn('  To play real SID music:');
    WriteLn('  1. Download SIDDump from: https://csdb.dk/release/?id=192079');
    WriteLn('  2. Convert a .SID file: siddump -o music.dmp music.sid');
    WriteLn('  3. Place the .dmp file in the program directory');
    Exit;
  end;

  // Create player (will create its own SIDEvo)
  APlayer := TSedaiSIDDumpPlayer.Create;
  try
    // Use PAL timing (50 Hz)
    APlayer.FrameRate := SID_FRAMERATE_PAL;
    APlayer.Looping := False;

    // Load the dump file
    if not APlayer.LoadFromFile(ADumpFile) then
    begin
      WriteLn('  ERROR: Failed to load dump file');
      Exit;
    end;

    WriteLn('  File: ', ADumpFile);
    WriteLn('  Frames: ', APlayer.FrameCount);
    WriteLn('  Duration: ', APlayer.GetDurationSeconds:0:2, ' seconds');
    WriteLn('');
    WriteLn('  Playing... (Press Ctrl+C to stop)');
    WriteLn('');

    // Start playback
    APlayer.Play;

    // Frame timing
    AFrameInterval := 1000 div APlayer.FrameRate; // ~20ms for PAL
    ALastTime := SDL_GetTicks;

    // Playback loop
    while APlayer.IsPlaying do
    begin
      ACurrentTime := SDL_GetTicks;

      // Check if it's time for next frame
      if (ACurrentTime - ALastTime) >= AFrameInterval then
      begin
        APlayer.Update;
        ALastTime := ACurrentTime;

        // Show progress every second
        if (APlayer.CurrentFrame mod APlayer.FrameRate) = 0 then
          Write('  Progress: ', (APlayer.GetProgress * 100):0:1, '%  ',
                'Time: ', APlayer.GetPositionSeconds:0:1, 's    '#13);
      end;

      SDL_Delay(1);
    end;

    WriteLn('');
    WriteLn('');
    WriteLn('  Playback complete!');

  finally
    APlayer.Free;
  end;
end;

// Create a simple test dump file programmatically
procedure CreateTestDumpFile(const AFilename: string);
var
  AStream: TFileStream;
  AFrame: array[0..24] of Byte;
  i, ANote, AOctave: Integer;
  AFreq: Word;

  procedure WriteFrame;
  begin
    AStream.Write(AFrame, 25);
  end;

  procedure SetVoice1(AFreqVal: Word; AWaveCtrl, AAD, ASR: Byte);
  begin
    AFrame[0] := AFreqVal and $FF;         // Freq lo
    AFrame[1] := (AFreqVal shr 8) and $FF; // Freq hi
    AFrame[2] := $00;                       // PW lo
    AFrame[3] := $08;                       // PW hi (50% duty)
    AFrame[4] := AWaveCtrl;                 // Control
    AFrame[5] := AAD;                       // Attack/Decay
    AFrame[6] := ASR;                       // Sustain/Release
  end;

  function NoteToSIDFreq(ANote: Integer): Word;
  var
    AHz: Single;
  begin
    // Convert note number to SID frequency register
    AHz := 440.0 * Power(2.0, (ANote - 69) / 12.0);
    Result := Round((AHz * 16777216.0) / 985248.0);
  end;

begin
  WriteLn('  Generating test dump file...');

  AStream := TFileStream.Create(AFilename, fmCreate);
  try
    // Initialize frame with silence
    FillChar(AFrame, 25, 0);
    AFrame[24] := $0F; // Volume = 15

    // Generate a simple melody (C major scale up and down)
    // Each note is 12 frames (about 240ms at 50Hz)

    // Scale up
    for i := 0 to 7 do
    begin
      case i of
        0: ANote := 60; // C4
        1: ANote := 62; // D4
        2: ANote := 64; // E4
        3: ANote := 65; // F4
        4: ANote := 67; // G4
        5: ANote := 69; // A4
        6: ANote := 71; // B4
        7: ANote := 72; // C5
      end;

      AFreq := NoteToSIDFreq(ANote);

      // Note on (gate=1, triangle wave)
      SetVoice1(AFreq, $11, $09, $00); // Gate + Triangle, A=0, D=9, S=0, R=0

      // Write 10 frames of note
      for AOctave := 0 to 9 do
        WriteFrame;

      // Note off (gate=0)
      AFrame[4] := $10; // Triangle, no gate

      // Write 2 frames of release
      WriteFrame;
      WriteFrame;
    end;

    // Scale down
    for i := 7 downto 0 do
    begin
      case i of
        0: ANote := 60;
        1: ANote := 62;
        2: ANote := 64;
        3: ANote := 65;
        4: ANote := 67;
        5: ANote := 69;
        6: ANote := 71;
        7: ANote := 72;
      end;

      AFreq := NoteToSIDFreq(ANote);
      SetVoice1(AFreq, $11, $09, $00);

      for AOctave := 0 to 9 do
        WriteFrame;

      AFrame[4] := $10;
      WriteFrame;
      WriteFrame;
    end;

    // Final long note
    AFreq := NoteToSIDFreq(60); // C4
    SetVoice1(AFreq, $11, $0A, $A0); // Longer sustain

    for i := 0 to 49 do
      WriteFrame;

    // Release
    AFrame[4] := $10;
    for i := 0 to 24 do
      WriteFrame;

    WriteLn('  Created test dump: ', AStream.Size, ' bytes, ',
            AStream.Size div 25, ' frames');

  finally
    AStream.Free;
  end;
end;

// ============================================================================
// MENU INPUT HANDLER (uses ReadLn for console input)
// ============================================================================
function GetMenuChoice: string;
begin
  Write('Choice: ');
  ReadLn(Result);
  Result := UpperCase(Trim(Result));
  // Reset escape flag when returning to menu
  GEscapePressed := False;
end;

procedure ShowMenu;
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('         SID EVO DEMO MENU');
  WriteLn('========================================');
  WriteLn('');
  WriteLn('  1. Waveform Showcase');
  WriteLn('  2. ADSR Envelopes');
  WriteLn('  3. Stereo Panning');
  WriteLn('  4. Polyphony (8 voices)');
  WriteLn('  5. Filter Sweep');
  WriteLn('  6. Extended Range (11 octaves)');
  WriteLn('  7. Classic SID Arpeggio');
  WriteLn('  8. Musical Composition');
  WriteLn('  9. Voice Groups (8-64 voices)');
  WriteLn('  0. Extended Waveforms');
  WriteLn('  S. SIDDump File Playback');
  WriteLn('  A. Run ALL demos');
  WriteLn('  Q. Quit');
  WriteLn('');
  WriteLn('  (ESC/CTRL+C during demo to interrupt)');
  WriteLn('');
end;

// ============================================================================
// MAIN PROGRAM
// ============================================================================
var
  AChoice: string;

begin
  WriteLn('');
  WriteLn('****************************************************************');
  WriteLn('*                                                              *');
  WriteLn('*   SEDAI SID EVO - FULL DEMONSTRATION                         *');
  WriteLn('*                                                              *');
  WriteLn('*   An evolved SID synthesizer powered by                      *');
  WriteLn('*   Sedai Audio Foundation                                     *');
  WriteLn('*                                                              *');
  WriteLn('*   Features:                                                  *');
  WriteLn('*   - 8 to 64 voices in groups of 8 (original SID: 3)          *');
  WriteLn('*   - 11 octaves C0-B10 / 16Hz-20kHz (original: 8)             *');
  WriteLn('*   - Stereo output with per-voice panning                     *');
  WriteLn('*   - Per-voice filters: LP, HP, BP, Notch, Peak, AllPass      *');
  WriteLn('*   - Filter slopes: 12/24/48 dB per octave                    *');
  WriteLn('*   - Extended waveforms: Sine, Supersaw, PWM, HalfSin...      *');
  WriteLn('*   - Voice groups with independent vol/pan/transpose          *');
  WriteLn('*   - SIDDump file playback for C64 music                      *');
  WriteLn('*   - 44.1kHz 32-bit audio quality                             *');
  WriteLn('*                                                              *');
  WriteLn('****************************************************************');
  WriteLn('');

  // Initialize console input for ESC/CTRL+C detection during demos
  {$IFDEF WINDOWS}
  InitConsoleInput;
  {$ENDIF}

  // Initialize SID Evo
  SIDEvoInit(8);
  SIDEvoVol(0.7);

  try
    repeat
      ShowMenu;
      AChoice := GetMenuChoice;

      // Handle menu choice
      if (AChoice = 'Q') or (AChoice = 'ESC') then
        Break
      else if AChoice = '1' then
        Demo_Waveforms
      else if AChoice = '2' then
        Demo_ADSR
      else if AChoice = '3' then
        Demo_Stereo
      else if AChoice = '4' then
        Demo_Polyphony
      else if AChoice = '5' then
        Demo_Filter
      else if AChoice = '6' then
        Demo_Range
      else if AChoice = '7' then
        Demo_Arpeggio
      else if AChoice = '8' then
        Demo_Music
      else if AChoice = '9' then
        Demo_VoiceGroups
      else if AChoice = '0' then
        Demo_ExtendedWaveforms
      else if AChoice = 'S' then
        Demo_SIDDump
      else if AChoice = 'A' then
      begin
        Demo_Waveforms;
        if not ShouldAbortDemo then begin WaitForKey; Demo_ADSR; end;
        if not ShouldAbortDemo then begin WaitForKey; Demo_Stereo; end;
        if not ShouldAbortDemo then begin WaitForKey; Demo_Polyphony; end;
        if not ShouldAbortDemo then begin WaitForKey; Demo_Filter; end;
        if not ShouldAbortDemo then begin WaitForKey; Demo_Range; end;
        if not ShouldAbortDemo then begin WaitForKey; Demo_Arpeggio; end;
        if not ShouldAbortDemo then begin WaitForKey; Demo_Music; end;
        if not ShouldAbortDemo then begin WaitForKey; Demo_VoiceGroups; end;
        if not ShouldAbortDemo then begin WaitForKey; Demo_ExtendedWaveforms; end;
        if not ShouldAbortDemo then begin WaitForKey; Demo_SIDDump; end;
      end;

      // Stop sounds and reset flags after each demo
      SIDEvoStopAll;
      GEscapePressed := False;

    until False;

  finally
    SIDEvoStopAll;
    SDL_Delay(300);
    SIDEvoShutdown;
    {$IFDEF WINDOWS}
    RestoreConsoleInput;
    {$ENDIF}
  end;

  WriteLn('');
  WriteLn('Thank you for trying SID Evo!');
  WriteLn('');
end.
