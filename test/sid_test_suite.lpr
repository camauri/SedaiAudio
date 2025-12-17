{*
 * Sedai Audio Foundation - SID Test Suite
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * Comprehensive test suite for SID emulation validation.
 * Tests all aspects of SIDEvo: waveforms, envelopes, filters,
 * modulations, and musical effects.
 *}

program sid_test_suite;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, SDL2, SedaiSIDEvo, SedaiSIDTestPatterns, SedaiWaveformValidator;

var
  GQuitRequested: Boolean = False;
  GEscapePressed: Boolean = False;

// ============================================================================
// WINDOWS CONSOLE KEYBOARD INPUT
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
          ReadConsoleInput(GConsoleHandle, @AInputRec, 1, @AEventsRead);
      end;
      Dec(ANumEvents);
    end;
  end;
end;

function CheckConsoleKeys: Integer;
var
  AInputRec: INPUT_RECORD;
  AEventsRead: DWORD;
  AVk: Word;
  ACtrl: Boolean;
begin
  Result := 0;
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

        if ACtrl and (AVk = Ord('C')) then
        begin
          Result := 2;  // CTRL+C
          Exit;
        end;

        if AVk = VK_ESCAPE then
        begin
          Result := 1;  // ESC
          Exit;
        end;
      end;
    end;
  end;
end;
{$ELSE}
procedure InitConsoleInput;
begin
end;

procedure RestoreConsoleInput;
begin
end;

function CheckConsoleKeys: Integer;
begin
  Result := 0;
end;
{$ENDIF}

// ============================================================================
// ABORT CHECK CALLBACK
// ============================================================================

function AbortCheck: Boolean;
var
  AResult: Integer;
begin
  Result := GQuitRequested or GEscapePressed;
  if Result then Exit;

  AResult := CheckConsoleKeys;
  case AResult of
    1: begin  // ESC
         GEscapePressed := True;
         Result := True;
       end;
    2: begin  // CTRL+C
         GQuitRequested := True;
         Result := True;
       end;
  end;
end;

// ============================================================================
// MENU FUNCTIONS
// ============================================================================

procedure ClearScreen;
begin
  {$IFDEF WINDOWS}
  // Simple approach: just add some newlines
  WriteLn;
  WriteLn;
  {$ELSE}
  Write(#27'[2J'#27'[H');
  {$ENDIF}
end;

procedure ShowMainMenu;
begin
  ClearScreen;
  WriteLn('================================================================');
  WriteLn('       SID EVO TEST SUITE - Sedai Audio Foundation');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('  Test Categories:');
  WriteLn;
  WriteLn('    1. Waveform Tests (Triangle, Saw, Pulse, Noise)');
  WriteLn('    2. Sweep Tests (PWM, Frequency, Noise pitch)');
  WriteLn('    3. ADSR Envelope Tests (Attack, Decay, Release)');
  WriteLn('    4. Filter Tests (LP, HP, BP, Notch sweeps)');
  WriteLn('    5. Modulation Tests (Sync, Ring Mod)');
  WriteLn('    6. Musical Effect Tests (Arpeggio, Trill, Drums)');
  WriteLn('    7. Complex Pattern Tests (Combined effects)');
  WriteLn;
  WriteLn('    8. Run Full Test Suite (Audio)');
  WriteLn('    9. Waveform Validation (Mathematical - No Audio)');
  WriteLn;
  WriteLn('    0. Exit');
  WriteLn;
  WriteLn('----------------------------------------------------------------');
  WriteLn('  Controls: ESC = Abort current test, CTRL+C = Exit program');
  WriteLn('----------------------------------------------------------------');
  WriteLn;
  Write('  Select option (0-9): ');
end;

procedure ShowWaveformMenu;
begin
  ClearScreen;
  WriteLn('================================================================');
  WriteLn('       WAVEFORM TESTS');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('    1. Triangle (100, 440, 1000, 4000 Hz)');
  WriteLn('    2. Sawtooth (100, 440, 1000, 4000 Hz)');
  WriteLn('    3. Pulse with various duty cycles');
  WriteLn('    4. Noise (50, 200, 1000 Hz)');
  WriteLn('    5. Run all waveform tests');
  WriteLn;
  WriteLn('    0. Back to main menu');
  WriteLn;
  Write('  Select option: ');
end;

procedure ShowSweepMenu;
begin
  ClearScreen;
  WriteLn('================================================================');
  WriteLn('       SWEEP TESTS');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('    1. Pulse Width Sweep ($0400 -> $0F00)');
  WriteLn('    2. Frequency Sweep - Sawtooth (100 -> 4000 Hz)');
  WriteLn('    3. Frequency Sweep - Triangle (100 -> 4000 Hz)');
  WriteLn('    4. Noise Pitch Sweep (50 -> 2000 Hz)');
  WriteLn('    5. Run all sweep tests');
  WriteLn;
  WriteLn('    0. Back to main menu');
  WriteLn;
  Write('  Select option: ');
end;

procedure ShowADSRMenu;
begin
  ClearScreen;
  WriteLn('================================================================');
  WriteLn('       ADSR ENVELOPE TESTS');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('    1. Attack test (A = 0, 2, 4, 8, F)');
  WriteLn('    2. Decay test (D = 0, 4, 8, F with S=0)');
  WriteLn('    3. Release test (R = 0, 4, 8, F)');
  WriteLn('    4. Run all ADSR tests');
  WriteLn;
  WriteLn('    0. Back to main menu');
  WriteLn;
  Write('  Select option: ');
end;

procedure ShowFilterMenu;
begin
  ClearScreen;
  WriteLn('================================================================');
  WriteLn('       FILTER TESTS');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('    1. Lowpass sweep (resonance 0, 4, 8, F)');
  WriteLn('    2. Highpass sweep');
  WriteLn('    3. Bandpass sweep');
  WriteLn('    4. Notch sweep');
  WriteLn('    5. Run all filter tests');
  WriteLn;
  WriteLn('    0. Back to main menu');
  WriteLn;
  Write('  Select option: ');
end;

procedure ShowModulationMenu;
begin
  ClearScreen;
  WriteLn('================================================================');
  WriteLn('       MODULATION TESTS');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('    1. Hard Sync (Voice 0: 440Hz, Voice 1: 880Hz synced)');
  WriteLn('    2. Ring Modulation');
  WriteLn('    3. Sync with frequency sweep');
  WriteLn('    4. Run all modulation tests');
  WriteLn;
  WriteLn('    0. Back to main menu');
  WriteLn;
  Write('  Select option: ');
end;

procedure ShowEffectsMenu;
begin
  ClearScreen;
  WriteLn('================================================================');
  WriteLn('       MUSICAL EFFECT TESTS');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('    1. Arpeggio (C major: C-E-G)');
  WriteLn('    2. Arpeggio (C minor: C-Eb-G)');
  WriteLn('    3. Trill (A4 <-> A#4)');
  WriteLn('    4. Vibrato');
  WriteLn('    5. PWM Vibrato');
  WriteLn('    6. Noise Drum');
  WriteLn('    7. Kick Synth');
  WriteLn('    8. Snare Synth');
  WriteLn('    9. Run all effect tests');
  WriteLn;
  WriteLn('    0. Back to main menu');
  WriteLn;
  Write('  Select option: ');
end;

procedure ShowPatternMenu;
begin
  ClearScreen;
  WriteLn('================================================================');
  WriteLn('       COMPLEX PATTERN TESTS');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('    1. Fast Arpeggio (50 Hz frame rate)');
  WriteLn('    2. Slow Arpeggio (100ms per note)');
  WriteLn('    3. Combined Effects (PWM + Vibrato + Filter)');
  WriteLn('    4. Run all pattern tests');
  WriteLn;
  WriteLn('    0. Back to main menu');
  WriteLn;
  Write('  Select option: ');
end;

procedure WaitForKey;
begin
  WriteLn;
  Write('Press ENTER to continue...');
  ReadLn;
  GEscapePressed := False;  // Reset for next test
end;

function GetChoice: Char;
var
  AInput: string;
begin
  ReadLn(AInput);
  if Length(AInput) > 0 then
    Result := AInput[1]
  else
    Result := #0;
  GEscapePressed := False;  // Reset when returning to menu
end;

// ============================================================================
// SUB-MENUS
// ============================================================================

procedure RunWaveformSubmenu;
var
  AChoice: Char;
begin
  repeat
    ShowWaveformMenu;
    AChoice := GetChoice;

    case AChoice of
      '1': begin
             TestTriangle(100, 800);
             TestTriangle(440, 800);
             TestTriangle(1000, 800);
             TestTriangle(4000, 800);
             WaitForKey;
           end;
      '2': begin
             TestSaw(100, 800);
             TestSaw(440, 800);
             TestSaw(1000, 800);
             TestSaw(4000, 800);
             WaitForKey;
           end;
      '3': begin
             TestPulse(440, $0800, 800);
             TestPulse(440, $1000, 800);
             TestPulse(440, $1800, 800);
             TestPulse(440, $2000, 800);
             WaitForKey;
           end;
      '4': begin
             TestNoise(50, 500);
             TestNoise(200, 500);
             TestNoise(1000, 500);
             WaitForKey;
           end;
      '5': begin
             RunWaveformTests;
             WaitForKey;
           end;
      '0': Break;
    end;

    if GQuitRequested then Break;
  until False;
end;

procedure RunSweepSubmenu;
var
  AChoice: Char;
begin
  repeat
    ShowSweepMenu;
    AChoice := GetChoice;

    case AChoice of
      '1': begin
             TestPWMSweep($0400, $0F00, 1500);
             WaitForKey;
           end;
      '2': begin
             TestFreqSweep(100, 4000, SIDEVO_WAVE_SAWTOOTH, 1500);
             WaitForKey;
           end;
      '3': begin
             TestFreqSweep(100, 4000, SIDEVO_WAVE_TRIANGLE, 1500);
             WaitForKey;
           end;
      '4': begin
             TestNoiseSweep(50, 2000, 1000);
             WaitForKey;
           end;
      '5': begin
             RunSweepTests;
             WaitForKey;
           end;
      '0': Break;
    end;

    if GQuitRequested then Break;
  until False;
end;

procedure RunADSRSubmenu;
var
  AChoice: Char;
  i: Integer;
begin
  repeat
    ShowADSRMenu;
    AChoice := GetChoice;

    case AChoice of
      '1': begin
             for i := 0 to 4 do
               TestAttack(i * 4, 1500);
             WaitForKey;
           end;
      '2': begin
             for i := 0 to 3 do
               TestDecay(i * 5, 1500);
             WaitForKey;
           end;
      '3': begin
             for i := 0 to 3 do
               TestRelease(i * 5);
             WaitForKey;
           end;
      '4': begin
             RunADSRTests;
             WaitForKey;
           end;
      '0': Break;
    end;

    if GQuitRequested then Break;
  until False;
end;

procedure RunFilterSubmenu;
var
  AChoice: Char;
  i: Integer;
begin
  repeat
    ShowFilterMenu;
    AChoice := GetChoice;

    case AChoice of
      '1': begin
             for i := 0 to 3 do
               TestLowpassSweep(i * 5, 1500);
             WaitForKey;
           end;
      '2': begin
             TestHighpassSweep(8, 1500);
             WaitForKey;
           end;
      '3': begin
             TestBandpassSweep(8, 1500);
             WaitForKey;
           end;
      '4': begin
             TestNotchSweep(8, 1500);
             WaitForKey;
           end;
      '5': begin
             RunFilterTests;
             WaitForKey;
           end;
      '0': Break;
    end;

    if GQuitRequested then Break;
  until False;
end;

procedure RunModulationSubmenu;
var
  AChoice: Char;
begin
  repeat
    ShowModulationMenu;
    AChoice := GetChoice;

    case AChoice of
      '1': begin
             TestSync;
             WaitForKey;
           end;
      '2': begin
             TestRingMod;
             WaitForKey;
           end;
      '3': begin
             TestSyncSweep;
             WaitForKey;
           end;
      '4': begin
             RunModulationTests;
             WaitForKey;
           end;
      '0': Break;
    end;

    if GQuitRequested then Break;
  until False;
end;

procedure RunEffectsSubmenu;
var
  AChoice: Char;
begin
  repeat
    ShowEffectsMenu;
    AChoice := GetChoice;

    case AChoice of
      '1': begin
             TestArpeggio;
             WaitForKey;
           end;
      '2': begin
             TestArpeggioMinor;
             WaitForKey;
           end;
      '3': begin
             TestTrill;
             WaitForKey;
           end;
      '4': begin
             TestVibrato;
             WaitForKey;
           end;
      '5': begin
             TestPWMVibrato;
             WaitForKey;
           end;
      '6': begin
             TestNoiseDrum;
             WaitForKey;
           end;
      '7': begin
             TestKickSynth;
             WaitForKey;
           end;
      '8': begin
             TestSnareSynth;
             WaitForKey;
           end;
      '9': begin
             RunEffectTests;
             WaitForKey;
           end;
      '0': Break;
    end;

    if GQuitRequested then Break;
  until False;
end;

procedure RunPatternSubmenu;
var
  AChoice: Char;
begin
  repeat
    ShowPatternMenu;
    AChoice := GetChoice;

    case AChoice of
      '1': begin
             TestFastArpeggio;
             WaitForKey;
           end;
      '2': begin
             TestSlowArpeggio;
             WaitForKey;
           end;
      '3': begin
             TestCombinedEffects;
             WaitForKey;
           end;
      '4': begin
             RunPatternTests;
             WaitForKey;
           end;
      '0': Break;
    end;

    if GQuitRequested then Break;
  until False;
end;

// ============================================================================
// MAIN PROGRAM
// ============================================================================

var
  AChoice: Char;
begin
  WriteLn;
  WriteLn('************************************************************');
  WriteLn('*                                                          *');
  WriteLn('*   SID EVO TEST SUITE - Comprehensive SID Emulation Test  *');
  WriteLn('*                                                          *');
  WriteLn('*   Powered by Sedai Audio Foundation                      *');
  WriteLn('*                                                          *');
  WriteLn('************************************************************');
  WriteLn;

  // Initialize console input
  {$IFDEF WINDOWS}
  InitConsoleInput;
  {$ENDIF}

  // Initialize SDL
  if SDL_Init(SDL_INIT_AUDIO or SDL_INIT_TIMER) < 0 then
  begin
    WriteLn('ERROR: SDL initialization failed');
    {$IFDEF WINDOWS}
    RestoreConsoleInput;
    {$ENDIF}
    Halt(1);
  end;

  // Initialize SIDEvo
  SIDEvoInit(8);
  SIDEvoVol(0.7);

  // Set abort callback for test patterns
  GAbortCheck := @AbortCheck;

  try
    repeat
      ShowMainMenu;
      AChoice := GetChoice;

      case AChoice of
        '1': RunWaveformSubmenu;
        '2': RunSweepSubmenu;
        '3': RunADSRSubmenu;
        '4': RunFilterSubmenu;
        '5': RunModulationSubmenu;
        '6': RunEffectsSubmenu;
        '7': RunPatternSubmenu;
        '8': begin
               RunFullTestSuite;
               WaitForKey;
             end;
        '9': begin
               // Mathematical waveform validation (no audio)
               with TSedaiWaveformValidator.Create do
               try
                 RunAllValidations;
               finally
                 Free;
               end;
               WaitForKey;
             end;
        '0': GQuitRequested := True;
      end;

    until GQuitRequested;

  finally
    WriteLn;
    WriteLn('Shutting down...');
    SIDEvoStopAll;
    SDL_Delay(200);
    SIDEvoShutdown;
    {$IFDEF WINDOWS}
    RestoreConsoleInput;
    {$ENDIF}
  end;

  WriteLn;
  WriteLn('Thank you for using SID Evo Test Suite!');
end.
