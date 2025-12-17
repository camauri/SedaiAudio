{*
 * Sedai Audio Foundation - SID Evo Demo
 * Demonstrates the evolved SID synthesizer capabilities
 *}

program demo_sidevo;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, SDL2, SedaiSIDEvo;

var
  GEscapePressed: Boolean = False;
  GQuitRequested: Boolean = False;

// ============================================================================
// WINDOWS CONSOLE KEYBOARD INPUT
// Using Windows console API for non-blocking key detection in console apps
// ============================================================================

{$IFDEF WINDOWS}
var
  GConsoleHandle: THandle;
  GOriginalConsoleMode: DWORD;

procedure InitConsoleInput;
begin
  GConsoleHandle := GetStdHandle(STD_INPUT_HANDLE);
  if GConsoleHandle <> INVALID_HANDLE_VALUE then
    GetConsoleMode(GConsoleHandle, @GOriginalConsoleMode);
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
// Non-Windows: Stub implementations
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

// Immediate exit
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

// Check for ESC or CTRL+C - returns True if demo should abort
function CheckInput: Boolean;
var
  AKey: Char;
begin
  Result := False;
  AKey := ConsoleReadKey;
  case AKey of
    #27: // ESC - abort demo
      begin
        GEscapePressed := True;
        Result := True;
      end;
    #3: // CTRL+C - exit immediately
      begin
        GQuitRequested := True;
        ImmediateExit;
      end;
  end;
end;

// Interruptible wait - returns True if interrupted
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

    SDL_Delay(10);
  end;
end;

procedure WaitMs(AMs: Integer);
begin
  WaitMsInterruptible(AMs);
end;

procedure Demo1_BasicWaveforms;
var
  i: Integer;
begin
  WriteLn('');
  WriteLn('=== Demo 1: Basic Waveforms ===');
  WriteLn('Playing each waveform on voice 0...');
  WriteLn('');

  // Triangle
  WriteLn('  Triangle wave (C4)');
  SIDEvoWave(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvoPlay(0, 48, 0.7); // C4
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(200);

  // Sawtooth
  WriteLn('  Sawtooth wave (D4)');
  SIDEvoWave(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoPlay(0, 50, 0.7); // D4
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(200);

  // Pulse/Square
  WriteLn('  Pulse wave (E4)');
  SIDEvoWave(0, SIDEVO_WAVE_PULSE);
  SIDEvoPlay(0, 52, 0.7); // E4
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(200);

  // Noise
  WriteLn('  Noise');
  SIDEvoWave(0, SIDEVO_WAVE_NOISE);
  SIDEvoPlay(0, 48, 0.5);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(200);

  // Sine (extension)
  WriteLn('  Sine wave (G4) - SIDEvo extension');
  SIDEvoWave(0, SIDEVO_WAVE_SINE);
  SIDEvoPlay(0, 55, 0.7); // G4
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(300);
end;

procedure Demo2_StereoAndPanning;
var
  i: Integer;
  APan: Single;
begin
  WriteLn('');
  WriteLn('=== Demo 2: Stereo Panning ===');
  WriteLn('Panning a note from left to right...');
  WriteLn('');

  SIDEvoWave(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoEnvelope(0, 0.01, 0.1, 0.8, 0.3);

  // Start note at center
  SIDEvoPlay(0, 60, 0.7); // C5

  // Pan from left to right
  for i := 0 to 40 do
  begin
    APan := -1.0 + (i / 20.0); // -1.0 to 1.0
    SIDEvoPan(0, APan);
    WaitMs(50);
  end;

  SIDEvoStop(0);
  WaitMs(300);

  // Reset pan to center
  SIDEvoPan(0, 0.0);
end;

procedure Demo3_Polyphony;
begin
  WriteLn('');
  WriteLn('=== Demo 3: Polyphony (8 voices) ===');
  WriteLn('Playing a C major 7th chord spread across stereo field...');
  WriteLn('');

  // Setup all voices with different pans
  SIDEvoWave(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvoWave(1, SIDEVO_WAVE_TRIANGLE);
  SIDEvoWave(2, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoWave(3, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoWave(4, SIDEVO_WAVE_PULSE);
  SIDEvoWave(5, SIDEVO_WAVE_PULSE);
  SIDEvoWave(6, SIDEVO_WAVE_SINE);
  SIDEvoWave(7, SIDEVO_WAVE_SINE);

  // Pan voices across stereo field
  SIDEvoPan(0, -0.8);
  SIDEvoPan(1, -0.5);
  SIDEvoPan(2, -0.2);
  SIDEvoPan(3, 0.0);
  SIDEvoPan(4, 0.2);
  SIDEvoPan(5, 0.5);
  SIDEvoPan(6, 0.8);
  SIDEvoPan(7, 0.0);

  // Play C major 7th (C-E-G-B) with octave doublings
  SIDEvoPlay(0, 36, 0.5); // C3
  WaitMs(100);
  SIDEvoPlay(1, 40, 0.5); // E3
  WaitMs(100);
  SIDEvoPlay(2, 43, 0.5); // G3
  WaitMs(100);
  SIDEvoPlay(3, 47, 0.5); // B3
  WaitMs(100);
  SIDEvoPlay(4, 48, 0.6); // C4
  WaitMs(100);
  SIDEvoPlay(5, 52, 0.5); // E4
  WaitMs(100);
  SIDEvoPlay(6, 55, 0.5); // G4
  WaitMs(100);
  SIDEvoPlay(7, 59, 0.4); // B4

  WriteLn('  Chord playing...');
  WaitMs(2500);

  // Fade out by stopping voices
  SIDEvoStopAll;
  WaitMs(500);
end;

procedure Demo4_ADSREnvelopes;
begin
  WriteLn('');
  WriteLn('=== Demo 4: ADSR Envelopes ===');
  WriteLn('');

  SIDEvoWave(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoPan(0, 0.0);

  // Plucky sound (fast attack, no sustain)
  WriteLn('  Pluck envelope (fast attack, no sustain)');
  SIDEvoEnvelope(0, 0.0, 0.2, 0.0, 0.1);
  SIDEvoPlay(0, 60, 0.8);
  WaitMs(400);
  SIDEvoPlay(0, 64, 0.8);
  WaitMs(400);
  SIDEvoPlay(0, 67, 0.8);
  WaitMs(600);
  SIDEvoStop(0);
  WaitMs(300);

  // Pad sound (slow attack, high sustain)
  WriteLn('  Pad envelope (slow attack, high sustain)');
  SIDEvoEnvelope(0, 0.6, 0.3, 0.8, 0.7);
  SIDEvoPlay(0, 48, 0.7);
  WaitMs(2000);
  SIDEvoStop(0);
  WaitMs(1000);

  // Organ (instant attack, full sustain)
  WriteLn('  Organ envelope (instant attack, full sustain)');
  SIDEvoEnvelope(0, 0.0, 0.0, 1.0, 0.1);
  SIDEvoPlay(0, 55, 0.6);
  WaitMs(800);
  SIDEvoStop(0);
  WaitMs(300);
end;

procedure Demo5_FilterSweep;
var
  i: Integer;
  ACutoff: Single;
begin
  WriteLn('');
  WriteLn('=== Demo 5: Filter Sweep (Per-voice filter) ===');
  WriteLn('Sweeping lowpass filter cutoff...');
  WriteLn('');

  SIDEvoWave(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvoEnvelope(0, 0.01, 0.1, 0.9, 0.3);
  SIDEvoPan(0, 0.0);

  // Enable lowpass filter
  SIDEvoFilter(0, SIDEVO_FILTER_LOWPASS, 0.1, 0.5);

  SIDEvoPlay(0, 36, 0.8); // C3 - low note to hear filter

  // Sweep cutoff up
  for i := 0 to 50 do
  begin
    ACutoff := i / 50.0;
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);
    WaitMs(40);
  end;

  // Sweep cutoff down
  for i := 50 downto 0 do
  begin
    ACutoff := i / 50.0;
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);
    WaitMs(40);
  end;

  SIDEvoStop(0);

  // Disable filter
  SIDEvo.SetVoiceFilter(0, False);
  WaitMs(300);
end;

procedure Demo6_Arpeggio;
var
  i, ANote: Integer;
  ANotes: array[0..3] of Integer = (48, 52, 55, 60); // C-E-G-C
begin
  WriteLn('');
  WriteLn('=== Demo 6: Fast Arpeggio ===');
  WriteLn('Classic SID-style arpeggio...');
  WriteLn('');

  SIDEvoWave(0, SIDEVO_WAVE_PULSE);
  SIDEvoEnvelope(0, 0.0, 0.1, 0.6, 0.1);
  SIDEvoPan(0, 0.0);

  // Play fast arpeggio
  for i := 0 to 31 do
  begin
    ANote := ANotes[i mod 4];
    SIDEvoPlay(0, ANote, 0.7);
    WaitMs(80);
  end;

  SIDEvoStop(0);
  WaitMs(300);
end;

procedure Demo7_ExtendedRange;
var
  i: Integer;
begin
  WriteLn('');
  WriteLn('=== Demo 7: Extended Range (11 Octaves) ===');
  WriteLn('Playing notes from C0 (16Hz) to C10 (16kHz)...');
  WriteLn('');

  SIDEvoWave(0, SIDEVO_WAVE_SINE);
  SIDEvoEnvelope(0, 0.01, 0.1, 0.7, 0.2);

  // Play C in each octave (0-10 = 11 octaves)
  for i := 0 to 10 do
  begin
    WriteLn('  Playing C', i, ' (', SIDEvo.NoteToFrequency(i * 12):0:1, ' Hz)');
    SIDEvoPlay(0, i * 12, 0.6); // C0, C1, C2... C10
    WaitMs(350);
    SIDEvoStop(0);
    WaitMs(100);
  end;

  WaitMs(300);
end;

procedure Demo8_SimpleMelody;
var
  ANotes: array[0..15] of Integer;
  ADurations: array[0..15] of Integer;
  i: Integer;
begin
  WriteLn('');
  WriteLn('=== Demo 8: Simple Melody ===');
  WriteLn('Playing a short tune...');
  WriteLn('');

  // "Twinkle Twinkle" first phrase
  ANotes[0] := 48;  ADurations[0] := 300;  // C
  ANotes[1] := 48;  ADurations[1] := 300;  // C
  ANotes[2] := 55;  ADurations[2] := 300;  // G
  ANotes[3] := 55;  ADurations[3] := 300;  // G
  ANotes[4] := 57;  ADurations[4] := 300;  // A
  ANotes[5] := 57;  ADurations[5] := 300;  // A
  ANotes[6] := 55;  ADurations[6] := 600;  // G (long)
  ANotes[7] := 53;  ADurations[7] := 300;  // F
  ANotes[8] := 53;  ADurations[8] := 300;  // F
  ANotes[9] := 52;  ADurations[9] := 300;  // E
  ANotes[10] := 52; ADurations[10] := 300; // E
  ANotes[11] := 50; ADurations[11] := 300; // D
  ANotes[12] := 50; ADurations[12] := 300; // D
  ANotes[13] := 48; ADurations[13] := 600; // C (long)

  SIDEvoWave(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvoEnvelope(0, 0.01, 0.15, 0.6, 0.2);
  SIDEvoPan(0, 0.0);

  for i := 0 to 13 do
  begin
    SIDEvoPlay(0, ANotes[i], 0.7);
    WaitMs(ADurations[i]);
    SIDEvoStop(0);
    WaitMs(50);
  end;

  WaitMs(500);
end;

// Main program
begin
  WriteLn('');
  WriteLn('****************************************************');
  WriteLn('*                                                  *');
  WriteLn('*   SEDAI SID EVO - Evolved SID Synthesizer Demo   *');
  WriteLn('*                                                  *');
  WriteLn('*   Powered by Sedai Audio Foundation              *');
  WriteLn('*                                                  *');
  WriteLn('****************************************************');
  WriteLn('');

  // Initialize console input for ESC/CTRL+C detection during demos
  {$IFDEF WINDOWS}
  InitConsoleInput;
  {$ENDIF}

  // Initialize SID Evo
  SIDEvoInit(8);
  SIDEvoVol(0.7);

  WriteLn('Press ENTER to start demos...');
  ReadLn;

  try
    Demo1_BasicWaveforms;
    Demo2_StereoAndPanning;
    Demo3_Polyphony;
    Demo4_ADSREnvelopes;
    Demo5_FilterSweep;
    Demo6_Arpeggio;
    Demo7_ExtendedRange;
    Demo8_SimpleMelody;

    WriteLn('');
    WriteLn('=== All demos complete! ===');
    WriteLn('');

    // Print final status
    SIDEvo.PrintStatus;

  finally
    // Cleanup
    SIDEvoStopAll;
    WaitMs(500);
    SIDEvoShutdown;
    {$IFDEF WINDOWS}
    RestoreConsoleInput;
    {$ENDIF}
  end;

  WriteLn('');
  WriteLn('Press ENTER to exit...');
  ReadLn;
end.
