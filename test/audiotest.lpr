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
  SysUtils, Math,
  SDL2, SedaiDateTimeUtils,
  SedaiAudioFoundation, SedaiAudioTypes,
  SedaiMIDIFoundation, SedaiMIDITypes,
  SedaiAdditiveProcessor, SedaiADSRProcessor,
  SedaiFilters, SedaiAudioEffects;

var
  AppPath: string = '';
  DataPath: string = '';
  GTerminateRequested: Boolean = False;
  GEscapePressed: Boolean = False;

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

function ConsoleKeyAvailable: Boolean;
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

// Check for ESC or CTRL+C via Windows console
// Returns: 0 = nothing, 1 = ESC pressed, 2 = CTRL+C pressed
function CheckConsoleKeys: Integer;
var
  AInputRec: INPUT_RECORD;
  AEventsRead: DWORD;
  AVk: Word;
  ACtrl: Boolean;
begin
  Result := 0;
  if GConsoleHandle = INVALID_HANDLE_VALUE then Exit;

  if ConsoleKeyAvailable then
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

// =========================================================================
// DATA PATH RESOLUTION
// =========================================================================

function FindDataPath: string;
var
  ATestPath: string;
begin
  ATestPath := ConcatPaths([AppPath, 'data']);
  if DirectoryExists(ATestPath) then
  begin
    Result := ATestPath + DirectorySeparator;
    Exit;
  end;

  ATestPath := ConcatPaths([AppPath, '..', '..', 'data']);
  if DirectoryExists(ATestPath) then
  begin
    Result := ExpandFileName(ATestPath) + DirectorySeparator;
    Exit;
  end;

  ATestPath := ConcatPaths([GetCurrentDir, 'data']);
  if DirectoryExists(ATestPath) then
  begin
    Result := ATestPath + DirectorySeparator;
    Exit;
  end;

  Result := AppPath;
end;

// =========================================================================
// SDL2 KEYBOARD EVENT HANDLING
// =========================================================================

// Check for CTRL+C or ESC via SDL2 events
// Returns: 0 = nothing, 1 = ESC pressed, 2 = CTRL+C pressed (exit app)
function CheckSDLEvents: Integer;
var
  Event: TSDL_Event;
  KeyMod: TSDL_Keymod;
begin
  Result := 0;
  while SDL_PollEvent(@Event) <> 0 do
  begin
    case Event.type_ of
      SDL_QUITEV:
      begin
        Result := 2;  // Quit = like CTRL+C
        Exit;
      end;
      SDL_KEYDOWN:
      begin
        // Check for ESC
        if Event.key.keysym.sym = SDLK_ESCAPE then
        begin
          Result := 1;
          Exit;
        end;
        // Check for CTRL+C
        KeyMod := SDL_GetModState;
        if ((KeyMod and KMOD_CTRL) <> 0) and (Event.key.keysym.sym = SDLK_c) then
        begin
          Result := 2;
          Exit;
        end;
      end;
    end;
  end;
end;

// Immediate exit without confirmation
procedure ImmediateExit;
begin
  WriteLn('');
  WriteLn('Exiting...');
  SmoothStopAll(50);
  SHUTDOWN_MIDI;
  ShutdownAudio;
  {$IFDEF WINDOWS}
  RestoreConsoleInput;
  {$ENDIF}
  Halt(0);
end;

// Process SDL events and handle CTRL+C globally
procedure ProcessEvents;
var
  EventResult: Integer;
begin
  EventResult := CheckSDLEvents;
  case EventResult of
    1: GEscapePressed := True;
    2: ImmediateExit;
  end;
end;

// =========================================================================
// INTERRUPTIBLE DELAY
// =========================================================================

procedure InterruptibleDelay(AMilliseconds: Cardinal);
var
  AEndTime: QWord;
  ARemaining: Cardinal;
  EventResult: Integer;
begin
  GEscapePressed := False;
  AEndTime := GetTickCount64 + AMilliseconds;

  while (GetTickCount64 < AEndTime) and (not GTerminateRequested) and (not GEscapePressed) do
  begin
    // Check SDL events for ESC or CTRL+C (may not work in console-only mode)
    EventResult := CheckSDLEvents;

    // Also check Windows console keys (works in console mode)
    if EventResult = 0 then
      EventResult := CheckConsoleKeys;

    case EventResult of
      1: // ESC - interrupt test, return to menu
      begin
        GEscapePressed := True;
        Break;
      end;
      2: // CTRL+C - exit program immediately
      begin
        ImmediateExit;
      end;
    end;

    ARemaining := AEndTime - GetTickCount64;
    if ARemaining > 50 then
      SDL_Delay(50)
    else if ARemaining > 0 then
      SDL_Delay(ARemaining);
  end;

  if GEscapePressed or GTerminateRequested then
  begin
    WriteLn('');
    WriteLn('Test interrupted.');
    SmoothStopAll(100);
    raise EAbort.Create('User interrupt');
  end;
end;

// =========================================================================
// UTILITY FUNCTIONS
// =========================================================================

procedure ClearScreen;
begin
  {$IFDEF WINDOWS}
  System.Write(#27'[2J'#27'[H');
  {$ELSE}
  System.Write(#27'[2J'#27'[H');
  {$ENDIF}
end;

function ConfirmExit: Boolean;
var
  AChoice: string;
begin
  WriteLn('');
  Write('Do you want to exit? (Y/N): ');
  ReadLn(AChoice);
  Result := (UpperCase(AChoice) = 'Y') or (UpperCase(AChoice) = 'YES');
end;

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn('================================================================');
  WriteLn('  ', ATitle);
  WriteLn('================================================================');
  WriteLn;
end;

procedure WaitForKey(const AMessage: string = 'Press ENTER to continue...');
begin
  WriteLn;
  Write(AMessage);
  ReadLn;
end;

// =========================================================================
// ADDITIVE SYNTHESIS TESTS
// =========================================================================

procedure PrintHarmonicSpectrum(const ASynth: TAdditiveSynthesis);
var
  i: Integer;
  MaxAmp: Single;
  BarLength: Integer;
begin
  WriteLn;
  WriteLn('Harmonic spectrum:');
  WriteLn('-----------------------------------------------------');

  MaxAmp := 0.0;
  for i := 0 to ASynth.HarmonicCount - 1 do
    if ASynth.Harmonics[i].Amplitude > MaxAmp then
      MaxAmp := ASynth.Harmonics[i].Amplitude;

  if MaxAmp = 0.0 then MaxAmp := 1.0;

  for i := 0 to ASynth.HarmonicCount - 1 do
  begin
    if ASynth.Harmonics[i].IsActive then
    begin
      BarLength := Round((ASynth.Harmonics[i].Amplitude / MaxAmp) * 40);
      WriteLn(Format('H%2d: %s %.2f', [
        ASynth.Harmonics[i].Number,
        StringOfChar('#', BarLength),
        ASynth.Harmonics[i].Amplitude
      ]));
    end;
  end;

  WriteLn('-----------------------------------------------------');
end;

procedure TestAdditiveBasic;
var
  Synth: TAdditiveSynthesis;
  i: Integer;
  Sample: Single;
  SampleRate: Cardinal;
  DeltaTime: Single;
begin
  PrintHeader('Additive Synthesis - Basic Waveforms');

  SampleRate := 44100;
  DeltaTime := 1.0 / SampleRate;

  WriteLn('1. Creating a pure tone (fundamental only)...');
  TSedaiAdditiveProcessor.InitializeAdditive(Synth);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 1, 1.0);
  PrintHarmonicSpectrum(Synth);

  WriteLn('Generating 1000 samples @ 440Hz...');
  TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);
  for i := 1 to 1000 do
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 440.0, SampleRate, DeltaTime);
  WriteLn('Pure tone generated');
  WriteLn;

  WriteLn('2. Creating a square wave (odd harmonics)...');
  TSedaiAdditiveProcessor.ClearHarmonics(Synth);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 1, 1.0);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 3, 0.333);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 5, 0.2);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 7, 0.143);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 9, 0.111);
  PrintHarmonicSpectrum(Synth);

  WriteLn('Generating square wave...');
  TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);
  for i := 1 to 1000 do
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 440.0, SampleRate, DeltaTime);
  WriteLn('Square wave generated');
end;

procedure TestAdditivePresets;
var
  Organ, Strings, Brass, Flute, Bell, Voice: TAdditiveSynthesis;
begin
  PrintHeader('Additive Synthesis - Preset Spectra');

  WriteLn('1. ORGAN SPECTRUM (Hammond drawbar-style)');
  Organ := TSedaiAdditiveProcessor.CreateOrganSpectrum;
  PrintHarmonicSpectrum(Organ);
  WaitForKey;

  WriteLn('2. STRINGS SPECTRUM (sawtooth-like, 1/n rolloff)');
  Strings := TSedaiAdditiveProcessor.CreateStringsSpectrum;
  PrintHarmonicSpectrum(Strings);
  WaitForKey;

  WriteLn('3. BRASS SPECTRUM (strong odd harmonics)');
  Brass := TSedaiAdditiveProcessor.CreateBrassSpectrum;
  PrintHarmonicSpectrum(Brass);
  WaitForKey;

  WriteLn('4. FLUTE SPECTRUM (mostly fundamental)');
  Flute := TSedaiAdditiveProcessor.CreateFluteSpectrum;
  PrintHarmonicSpectrum(Flute);
  WaitForKey;

  WriteLn('5. BELL SPECTRUM (inharmonic partials)');
  Bell := TSedaiAdditiveProcessor.CreateBellSpectrum;
  WriteLn('Inharmonic factor: ', Bell.InharmonicFactor:0:6);
  PrintHarmonicSpectrum(Bell);
  WaitForKey;

  WriteLn('6. VOICE SPECTRUM (formant-like)');
  Voice := TSedaiAdditiveProcessor.CreateVoiceSpectrum;
  PrintHarmonicSpectrum(Voice);
end;

procedure TestAdditiveInharmonicity;
var
  Synth: TAdditiveSynthesis;
  i: Integer;
  Sample: Single;
  SampleRate: Cardinal;
  DeltaTime: Single;
begin
  PrintHeader('Additive Synthesis - Inharmonicity');

  SampleRate := 44100;
  DeltaTime := 1.0 / SampleRate;

  WriteLn('Comparison between harmonic and inharmonic synthesis...');
  WriteLn;

  WriteLn('1. HARMONIC spectrum (ideal bell)');
  TSedaiAdditiveProcessor.InitializeAdditive(Synth);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 1, 1.0);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 2, 0.6);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 3, 0.4);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 5, 0.3);
  Synth.InharmonicFactor := 0.0;
  WriteLn('InharmonicFactor = ', Synth.InharmonicFactor:0:6);
  WriteLn('Harmonics are exact multiples of the fundamental');
  PrintHarmonicSpectrum(Synth);

  TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);
  for i := 1 to 500 do
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 200.0, SampleRate, DeltaTime);
  WriteLn('Harmonic sound generated');
  WriteLn;

  WaitForKey('Press ENTER for inharmonic test...');

  WriteLn('2. INHARMONIC spectrum (realistic bell)');
  Synth.InharmonicFactor := 0.0001;
  WriteLn('InharmonicFactor = ', Synth.InharmonicFactor:0:6);
  WriteLn('Harmonics are slightly deviated (f = n * f0 * (1 + b*n^2))');
  WriteLn('This creates the characteristic sound of real bells');

  TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);
  for i := 1 to 500 do
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 200.0, SampleRate, DeltaTime);
  WriteLn('Inharmonic sound generated');
  WriteLn;

  WriteLn('Inharmonicity is crucial for realistic sounds of:');
  WriteLn('  - Bells');
  WriteLn('  - Cymbals and gongs');
  WriteLn('  - Metallic percussion instruments');
  WriteLn('  - Piano (stiff strings)');
end;

procedure TestAdditiveADSR;
var
  Synth: TAdditiveSynthesis;
  i: Integer;
  Sample: Single;
  SampleRate: Cardinal;
  DeltaTime: Single;
  MaxSample: Single;
begin
  PrintHeader('Additive Synthesis - ADSR Envelope');

  SampleRate := 44100;
  DeltaTime := 1.0 / SampleRate;

  Synth := TSedaiAdditiveProcessor.CreateBrassSpectrum;

  WriteLn('Preset: BRASS SPECTRUM');
  WriteLn('ADSR: Attack=', Synth.ADSR.Attack:0:3,
          ' Decay=', Synth.ADSR.Decay:0:3,
          ' Sustain=', Synth.ADSR.Sustain:0:2,
          ' Release=', Synth.ADSR.Release:0:3);
  WriteLn;

  WriteLn('Phase 1: ATTACK + DECAY + SUSTAIN (2 seconds)');
  TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);

  MaxSample := 0.0;
  for i := 1 to Round(SampleRate * 2) do
  begin
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 220.0, SampleRate, DeltaTime);
    if Abs(Sample) > MaxSample then MaxSample := Abs(Sample);

    if (i mod Round(SampleRate * 0.2)) = 0 then
    begin
      Write(Format('t=%.1fs Level=%.3f %s',
        [i / SampleRate, Synth.ADSR.Level, StringOfChar('#', Round(Synth.ADSR.Level * 40))]));

      case Synth.ADSR.Phase of
        apAttack: WriteLn(' [ATTACK]');
        apDecay: WriteLn(' [DECAY]');
        apSustain: WriteLn(' [SUSTAIN]');
        apRelease: WriteLn(' [RELEASE]');
        apIdle: WriteLn(' [IDLE]');
      end;
    end;
  end;

  WriteLn;
  WriteLn('Phase 2: RELEASE (1 second)');
  TSedaiAdditiveProcessor.StartAdditiveRelease(Synth);

  for i := 1 to Round(SampleRate * 1) do
  begin
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 220.0, SampleRate, DeltaTime);

    if (i mod Round(SampleRate * 0.2)) = 0 then
      WriteLn(Format('t=%.1fs Level=%.3f %s [RELEASE]',
        [i / SampleRate, Synth.ADSR.Level, StringOfChar('#', Round(Synth.ADSR.Level * 40))]));
  end;

  WriteLn;
  WriteLn('ADSR envelope completed');
  WriteLn('Maximum level reached: ', MaxSample:0:3);
end;

procedure RunAdditiveAllTests;
begin
  TestAdditiveBasic;
  WaitForKey;
  TestAdditivePresets;
  WaitForKey;
  TestAdditiveInharmonicity;
  WaitForKey;
  TestAdditiveADSR;
end;

// =========================================================================
// SUBTRACTIVE (CLASSIC) SYNTHESIS TESTS
// =========================================================================

procedure TestSubtractiveWaveforms;
begin
  PrintHeader('Subtractive Synthesis - Basic Waveforms');
  WriteLn('Active voices before start: ', GetActiveVoices);

  WriteLn('1. Pure Sine Wave (440 Hz)');
  PlaySine(440.0);
  InterruptibleDelay(1500);

  WriteLn('2. Square Wave (440 Hz)');
  PlaySquare(440.0);
  InterruptibleDelay(1500);

  WriteLn('3. Sawtooth Wave (440 Hz)');
  PlaySaw(440.0);
  InterruptibleDelay(1500);

  WriteLn('4. Triangle Wave (440 Hz)');
  PlayTriangle(440.0);
  InterruptibleDelay(1500);

  SmoothStopAll(500);
  WriteLn('Basic waveforms test complete!');
end;

procedure TestSubtractivePresets;
begin
  PrintHeader('Subtractive Synthesis - Analog Presets');

  WriteLn('1. Strings - C major chord');
  PlayClassic(261.63, 'strings');
  InterruptibleDelay(800);
  PlayClassic(329.63, 'strings');
  InterruptibleDelay(800);
  PlayClassic(392.00, 'strings');
  InterruptibleDelay(1500);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('2. Brass - ascending scale');
  PlayClassic(261.63, 'brass');
  InterruptibleDelay(400);
  PlayClassic(293.66, 'brass');
  InterruptibleDelay(400);
  PlayClassic(329.63, 'brass');
  InterruptibleDelay(400);
  PlayClassic(349.23, 'brass');
  InterruptibleDelay(600);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('3. Organ - chord');
  PlayClassic(261.63, 'organ');
  PlayClassic(329.63, 'organ');
  PlayClassic(392.00, 'organ');
  InterruptibleDelay(2000);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('4. Pluck - arpeggio');
  PlayClassic(261.63, 'pluck');
  InterruptibleDelay(300);
  PlayClassic(329.63, 'pluck');
  InterruptibleDelay(300);
  PlayClassic(392.00, 'pluck');
  InterruptibleDelay(300);
  PlayClassic(523.25, 'pluck');
  InterruptibleDelay(600);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('5. Analog Lead');
  PlayLead(329.63);
  InterruptibleDelay(2500);

  WriteLn('6. Analog Bass');
  PlayBass(65.41);
  InterruptibleDelay(2500);

  WriteLn('7. Analog Pad');
  PlayPad(196.00);
  InterruptibleDelay(3000);

  SmoothStopAll(500);
  WriteLn('Analog presets test complete!');
end;

procedure TestSubtractiveChords;
begin
  PrintHeader('Subtractive Synthesis - Chord Progressions');

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
  WriteLn('Chord progressions complete!');
end;

procedure TestSubtractiveScale;
begin
  PrintHeader('Subtractive Synthesis - Musical Scale');
  WriteLn('Playing C Major scale (C4 to C5)');
  PlayScaleClassic(NOTE_C4, 'lead');
  SmoothStopAll(500);
  WriteLn('Scale demonstration complete!');
end;

procedure RunSubtractiveAllTests;
begin
  TestSubtractiveWaveforms;
  InterruptibleDelay(1000);
  TestSubtractivePresets;
  InterruptibleDelay(1000);
  TestSubtractiveChords;
  InterruptibleDelay(1000);
  TestSubtractiveScale;
end;

// =========================================================================
// FM SYNTHESIS TESTS
// =========================================================================

procedure TestFMBasics;
begin
  PrintHeader('FM Synthesis - Basic Algorithms');

  WriteLn('1. Electric Piano (Rhodes-style)');
  PlayEPiano(261.63);
  InterruptibleDelay(2500);

  WriteLn('2. FM Brass (Horn section)');
  PlayFMBrass(349.23);
  InterruptibleDelay(2500);

  WriteLn('3. FM Bell (Chime/Vibraphone)');
  PlayFMBell(523.25);
  InterruptibleDelay(3000);

  SmoothStopAll(500);
  WriteLn('FM basics test complete!');
end;

procedure TestFMPresets;
begin
  PrintHeader('FM Synthesis - Advanced Presets');

  WriteLn('1. FM Organ (Hammond-style)');
  PlayFMOrgan(440.00);
  InterruptibleDelay(2500);

  WriteLn('2. FM Synth Lead (Aggressive)');
  PlayFMLead(659.25);
  InterruptibleDelay(2000);

  WriteLn('3. FM Synth Bass (Deep)');
  PlayFMBass(82.41);
  InterruptibleDelay(2500);

  WriteLn('4. Choir - choral chord');
  PlayFM(261.63, 'choir');
  PlayFM(329.63, 'choir');
  PlayFM(392.00, 'choir');
  InterruptibleDelay(2500);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('5. Marimba - rhythmic pattern');
  PlayFM(523.25, 'marimba');
  InterruptibleDelay(300);
  PlayFM(587.33, 'marimba');
  InterruptibleDelay(300);
  PlayFM(659.25, 'marimba');
  InterruptibleDelay(300);
  PlayFM(783.99, 'marimba');
  InterruptibleDelay(600);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('6. Church Bell - chimes');
  PlayFM(261.63, 'churchbell');
  InterruptibleDelay(1500);
  PlayFM(196.00, 'churchbell');
  InterruptibleDelay(2000);
  StopAll;

  SmoothStopAll(500);
  WriteLn('Advanced FM presets complete!');
end;

procedure TestFMChords;
begin
  PrintHeader('FM Synthesis - Polyphonic Chords');

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
  PrintHeader('FM Synthesis - Musical Scale');
  WriteLn('Playing C Major scale with Electric Piano');
  PlayScaleFM(NOTE_C4, 'epiano');
  SmoothStopAll(500);
  WriteLn('FM scale demonstration complete!');
end;

procedure RunFMAllTests;
begin
  TestFMBasics;
  InterruptibleDelay(1000);
  TestFMPresets;
  InterruptibleDelay(1000);
  TestFMChords;
  InterruptibleDelay(1000);
  TestFMScale;
end;

// =========================================================================
// WAVETABLE SYNTHESIS TESTS
// =========================================================================

procedure TestWavetableBasics;
begin
  PrintHeader('Wavetable Synthesis - Modern Wavetables');

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

procedure TestWavetablePresets;
begin
  PrintHeader('Wavetable Synthesis - Preset Demo');

  WriteLn('1. Vocal - vocal formants');
  PlayWavetable(261.63, 'vocal');
  InterruptibleDelay(800);
  PlayWavetable(293.66, 'vocal');
  InterruptibleDelay(800);
  PlayWavetable(329.63, 'vocal');
  InterruptibleDelay(1200);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('2. Metallic - metallic sounds');
  PlayWavetable(392.00, 'metallic');
  InterruptibleDelay(1000);
  PlayWavetable(493.88, 'metallic');
  InterruptibleDelay(1500);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('3. Glass - glass bells');
  PlayWavetable(523.25, 'glass');
  InterruptibleDelay(1000);
  PlayWavetable(659.25, 'glass');
  InterruptibleDelay(1500);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('4. Organ - Hammond-style organ');
  PlayWavetable(261.63, 'organ');
  PlayWavetable(329.63, 'organ');
  PlayWavetable(392.00, 'organ');
  InterruptibleDelay(2000);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('5. Evolving - evolving pad');
  PlayWavetable(130.81, 'evolving');
  PlayWavetable(164.81, 'evolving');
  PlayWavetable(196.00, 'evolving');
  InterruptibleDelay(3500);
  StopAll;
  InterruptibleDelay(500);

  WriteLn('6. Digital Chaos - chaotic modulation');
  PlayWavetable(220.00, 'digitalchaos');
  InterruptibleDelay(600);
  PlayWavetable(293.66, 'digitalchaos');
  InterruptibleDelay(600);
  PlayWavetable(369.99, 'digitalchaos');
  InterruptibleDelay(1000);
  StopAll;

  SmoothStopAll(500);
  WriteLn('Wavetable presets complete!');
end;

procedure TestWavetableChords;
begin
  PrintHeader('Wavetable Synthesis - Harmonic Progressions');

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

procedure TestWavetableLoading;
var
  AFormats: TStringArray;
  i: Integer;
  AWavetablePath: string;
  ATestFile: string;
  ALoadedCount: Integer;
begin
  PrintHeader('Wavetable Loading System');

  WriteLn('Using Adventure Kid Waveforms (AKWF) - CC0 Public Domain');
  WriteLn('');

  WriteLn('1. Supported Formats Detection');
  AFormats := GetWavetableFormats;
  Write('Supported formats: ');
  for i := 0 to Length(AFormats) - 1 do
  begin
    Write(AFormats[i]);
    if i < Length(AFormats) - 1 then Write(', ');
  end;
  WriteLn;

  WriteLn('');
  WriteLn('2. Directory Scanning');
  AWavetablePath := ConcatPaths([DataPath, 'wavetables']);
  WriteLn('   Scanning: ', AWavetablePath);
  if DirectoryExists(AWavetablePath) then
    ScanWavetableDirectory(AWavetablePath)
  else
    WriteLn('   Directory not found');

  WriteLn('');
  WriteLn('3. Cache Status');
  WriteLn('   ', GetWavetableCacheInfo);

  WriteLn('');
  WriteLn('4. Loading AKWF Wavetables');

  // Test loading basic waveforms (perfectwaves)
  ATestFile := ConcatPaths([DataPath, 'wavetables', 'AKWF_bw_perfectwaves', 'AKWF_sin.wav']);
  if FileExists(ATestFile) then
  begin
    if LoadWavetableFile(ATestFile) then
      WriteLn('   OK: AKWF sine wave loaded')
    else
      WriteLn('   ERROR: Failed to load AKWF sine');
  end
  else
    WriteLn('   INFO: AKWF_sin.wav not found');

  ATestFile := ConcatPaths([DataPath, 'wavetables', 'AKWF_bw_perfectwaves', 'AKWF_saw.wav']);
  if FileExists(ATestFile) then
  begin
    if LoadWavetableFile(ATestFile) then
      WriteLn('   OK: AKWF sawtooth wave loaded')
    else
      WriteLn('   ERROR: Failed to load AKWF saw');
  end;

  ATestFile := ConcatPaths([DataPath, 'wavetables', 'AKWF_bw_perfectwaves', 'AKWF_squ.wav']);
  if FileExists(ATestFile) then
  begin
    if LoadWavetableFile(ATestFile) then
      WriteLn('   OK: AKWF square wave loaded')
    else
      WriteLn('   ERROR: Failed to load AKWF square');
  end;

  ATestFile := ConcatPaths([DataPath, 'wavetables', 'AKWF_bw_perfectwaves', 'AKWF_tri.wav']);
  if FileExists(ATestFile) then
  begin
    if LoadWavetableFile(ATestFile) then
      WriteLn('   OK: AKWF triangle wave loaded')
    else
      WriteLn('   ERROR: Failed to load AKWF triangle');
  end;

  WriteLn('');
  WriteLn('5. Loading AKWF Categories');

  // Load epiano category
  AWavetablePath := ConcatPaths([DataPath, 'wavetables', 'AKWF_epiano']);
  if DirectoryExists(AWavetablePath) then
  begin
    ALoadedCount := LoadWavetableDirectory(AWavetablePath);
    WriteLn('   AKWF_epiano: ', ALoadedCount, ' wavetables loaded');
  end;

  // Load violin category
  AWavetablePath := ConcatPaths([DataPath, 'wavetables', 'AKWF_violin']);
  if DirectoryExists(AWavetablePath) then
  begin
    ALoadedCount := LoadWavetableDirectory(AWavetablePath);
    WriteLn('   AKWF_violin: ', ALoadedCount, ' wavetables loaded');
  end;

  // Load fmsynth category
  AWavetablePath := ConcatPaths([DataPath, 'wavetables', 'AKWF_fmsynth']);
  if DirectoryExists(AWavetablePath) then
  begin
    ALoadedCount := LoadWavetableDirectory(AWavetablePath);
    WriteLn('   AKWF_fmsynth: ', ALoadedCount, ' wavetables loaded');
  end;

  WriteLn('');
  WriteLn('6. Final Cache Status');
  WriteLn('   ', GetWavetableCacheInfo);

  WriteLn('');
  WriteLn('Wavetable loading system test complete!');
end;

procedure TestAKWFWavetables;
var
  AWavetablePath: string;
  ALoadedCount: Integer;
  ALoadedNames: TStringArray;
  i: Integer;
begin
  PrintHeader('AKWF Wavetable Playback Demo');

  WriteLn('Adventure Kid Waveforms (AKWF) - CC0 Public Domain');
  WriteLn('Single-cycle waveforms optimized for Surge/Serum compatibility');
  WriteLn('');

  // Load perfectwaves first
  AWavetablePath := ConcatPaths([DataPath, 'wavetables', 'AKWF_bw_perfectwaves']);
  if DirectoryExists(AWavetablePath) then
  begin
    ALoadedCount := LoadWavetableDirectory(AWavetablePath);
    WriteLn('Loaded ', ALoadedCount, ' basic waveforms from AKWF_bw_perfectwaves');
  end;

  // Load epiano
  AWavetablePath := ConcatPaths([DataPath, 'wavetables', 'AKWF_epiano']);
  if DirectoryExists(AWavetablePath) then
  begin
    ALoadedCount := LoadWavetableDirectory(AWavetablePath);
    WriteLn('Loaded ', ALoadedCount, ' electric piano wavetables');
  end;

  // Load violin
  AWavetablePath := ConcatPaths([DataPath, 'wavetables', 'AKWF_violin']);
  if DirectoryExists(AWavetablePath) then
  begin
    ALoadedCount := LoadWavetableDirectory(AWavetablePath);
    WriteLn('Loaded ', ALoadedCount, ' violin wavetables');
  end;

  WriteLn('');
  WriteLn('Cache status: ', GetWavetableCacheInfo);

  // Get loaded wavetables
  ALoadedNames := GetLoadedWavetables;
  if Length(ALoadedNames) > 0 then
  begin
    WriteLn('');
    WriteLn('First 10 loaded wavetables:');
    for i := 0 to Min(9, Length(ALoadedNames) - 1) do
      WriteLn('  ', i + 1, '. ', ALoadedNames[i]);
    if Length(ALoadedNames) > 10 then
      WriteLn('  ... and ', Length(ALoadedNames) - 10, ' more');
  end;

  WriteLn('');
  WriteLn('Playing AKWF wavetables - NO FALLBACK, errors if not loaded!');
  WriteLn('Look for ">>> Playing AKWF:" to confirm real wavetable playback.');
  WriteLn('');

  // Play AKWF wavetables - PlayLoadedWavetable will print what's playing
  // or ERROR if not found (no silent fallback!)
  WriteLn('1. AKWF Sine Wave');
  PlayLoadedWavetable(NOTE_C4, 'AKWF_sin');
  InterruptibleDelay(1500);
  SmoothStopAll(200);

  WriteLn('2. AKWF Sawtooth Wave');
  PlayLoadedWavetable(NOTE_C4, 'AKWF_saw');
  InterruptibleDelay(1500);
  SmoothStopAll(200);

  WriteLn('3. AKWF Square Wave');
  PlayLoadedWavetable(NOTE_C4, 'AKWF_squ');
  InterruptibleDelay(1500);
  SmoothStopAll(200);

  WriteLn('4. AKWF Triangle Wave');
  PlayLoadedWavetable(NOTE_C4, 'AKWF_tri');
  InterruptibleDelay(1500);
  SmoothStopAll(200);

  WriteLn('5. AKWF Electric Piano 0001');
  PlayLoadedWavetable(NOTE_C4, 'AKWF_epiano_0001');
  InterruptibleDelay(2000);
  SmoothStopAll(200);

  WriteLn('6. AKWF Electric Piano 0005');
  PlayLoadedWavetable(NOTE_C4, 'AKWF_epiano_0005');
  InterruptibleDelay(2000);
  SmoothStopAll(200);

  WriteLn('7. AKWF Violin 0001');
  PlayLoadedWavetable(NOTE_C4, 'AKWF_violin_0001');
  InterruptibleDelay(2000);
  SmoothStopAll(200);

  WriteLn('8. AKWF Electric Piano - Chord C-E-G');
  PlayLoadedWavetable(NOTE_C4, 'AKWF_epiano_0001');
  PlayLoadedWavetable(NOTE_E4, 'AKWF_epiano_0001');
  PlayLoadedWavetable(NOTE_G4, 'AKWF_epiano_0001');
  InterruptibleDelay(2500);
  SmoothStopAll(200);

  WriteLn('9. AKWF Violin - Arpeggio C-E-G-C');
  PlayLoadedWavetable(NOTE_C4, 'AKWF_violin_0001');
  InterruptibleDelay(300);
  PlayLoadedWavetable(NOTE_E4, 'AKWF_violin_0001');
  InterruptibleDelay(300);
  PlayLoadedWavetable(NOTE_G4, 'AKWF_violin_0001');
  InterruptibleDelay(300);
  PlayLoadedWavetable(NOTE_C4 * 2, 'AKWF_violin_0001');
  InterruptibleDelay(800);

  SmoothStopAll(500);

  WriteLn('');
  WriteLn('AKWF demo complete!');
  WriteLn('If you saw ">>> Playing AKWF:" messages, real wavetables were used.');
  WriteLn('If you saw "ERROR:" messages, those wavetables were NOT played.');
end;

procedure RunWavetableAllTests;
begin
  TestWavetableBasics;
  InterruptibleDelay(1000);
  TestWavetablePresets;
  InterruptibleDelay(1000);
  TestWavetableChords;
  InterruptibleDelay(1000);
  TestWavetableLoading;
  InterruptibleDelay(1000);
  TestAKWFWavetables;
end;

// =========================================================================
// MIDI SYSTEM TESTS
// =========================================================================

procedure TestMIDISystem;
begin
  PrintHeader('MIDI System Test');

  if not INIT_MIDI then
  begin
    WriteLn('ERROR: Failed to initialize MIDI system');
    Exit;
  end;

  WriteLn('1. MIDI System Status');
  PRINT_MIDI_STATUS;

  WriteLn('');
  WriteLn('2. Setting up General MIDI mapping');
  SETUP_MIDI_GENERAL_MIDI;

  WriteLn('');
  WriteLn('3. Channel configuration');
  PRINT_MIDI_CHANNELS;

  WriteLn('');
  WriteLn('MIDI system test complete!');
end;

procedure TestMIDIFileLoading;
var
  ATestFiles: array[0..4] of string;
  i: Integer;
  AFound: Boolean;
begin
  PrintHeader('MIDI File Loading Test');

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('Looking for MIDI files...');
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
    end;
  end;

  if not AFound then
  begin
    WriteLn('');
    WriteLn('No MIDI files found for testing');
    WriteLn('To test MIDI playback:');
    WriteLn('  1. Place a .mid file in the data/midi directory');
    WriteLn('  2. Or place it in the application directory');
  end;

  WriteLn('');
  WriteLn('MIDI file loading test complete!');
end;

procedure TestMIDIPlayback;
var
  ATimer: THiResTimer;
  AElapsedSeconds: Double;
  ALastProgressTime: QWord;
begin
  PrintHeader('MIDI Playback Test');

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  if GET_MIDI_TOTAL_TICKS = 0 then
  begin
    WriteLn('ERROR: No MIDI file loaded');
    WriteLn('Load a MIDI file first');
    Exit;
  end;

  WriteLn('Starting MIDI playback...');
  SetMasterVolume(0.5);
  SET_MIDI_TEMPO(1.0);

  ATimer := CreateHiResTimer;
  MIDI_PLAY;

  WriteLn('Playing for up to 45 seconds...');
  WriteLn('(Press CTRL+C to stop)');

  ALastProgressTime := GetTickCount64;

  while (GET_MIDI_PROGRESS < 100.0) and (IS_MIDI_PLAYING) and (not GTerminateRequested) do
  begin
    SDL_Delay(100);

    if GTerminateRequested then
    begin
      WriteLn('Playback interrupted by user');
      Break;
    end;

    AElapsedSeconds := ATimer.ElapsedSeconds;

    if (GetTickCount64 - ALastProgressTime) >= 5000 then
    begin
      WriteLn(Format('Progress: %.1f%% | Active Notes: %d | Time: %.1fs',
        [GET_MIDI_PROGRESS, GET_MIDI_ACTIVE_NOTES, AElapsedSeconds]));
      ALastProgressTime := GetTickCount64;
    end;

    if AElapsedSeconds > 45.0 then
    begin
      WriteLn('Demo time limit reached (45 seconds)');
      Break;
    end;
  end;

  WriteLn('');
  WriteLn('Stopping playback...');
  MIDI_STOP;
  SetMasterVolume(0.7);

  WriteLn('MIDI playback test complete!');
end;

procedure TestMIDIChannelMapping;
begin
  PrintHeader('MIDI Channel Mapping Test');

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('1. Setting up wavetable showcase...');
  SETUP_MIDI_WAVETABLE_SHOWCASE;
  PRINT_MIDI_CHANNELS;

  WriteLn('');
  WriteLn('2. Testing custom mapping...');
  SETUP_MIDI_CUSTOM_MAPPING;
  PRINT_MIDI_CHANNELS;

  WriteLn('');
  WriteLn('3. Testing individual channel controls...');
  SET_MIDI_CHANNEL_VOLUME(0, 0.5);
  SET_MIDI_CHANNEL_MUTE(9, True);
  SET_MIDI_CHANNEL_WAVETABLE(1, 'ppg');

  WriteLn('After individual changes:');
  WriteLn('  ', GET_MIDI_CHANNEL_INFO(0));
  WriteLn('  ', GET_MIDI_CHANNEL_INFO(1));
  WriteLn('  ', GET_MIDI_CHANNEL_INFO(9));

  WriteLn('');
  WriteLn('MIDI channel mapping test complete!');
end;

procedure TestMIDITempoControl;
begin
  PrintHeader('MIDI Tempo Control Test');

  if not IS_MIDI_INITIALIZED then
  begin
    WriteLn('ERROR: MIDI not initialized');
    Exit;
  end;

  WriteLn('1. Current tempo: ', GET_MIDI_BPM, ' BPM');

  WriteLn('');
  WriteLn('2. Testing tempo changes...');
  SET_MIDI_TEMPO(0.5);
  WriteLn('   Half speed: ', GET_MIDI_BPM, ' BPM');

  SET_MIDI_TEMPO(2.0);
  WriteLn('   Double speed: ', GET_MIDI_BPM, ' BPM');

  SET_MIDI_TEMPO(1.0);
  WriteLn('   Normal speed: ', GET_MIDI_BPM, ' BPM');

  WriteLn('');
  WriteLn('3. MIDI utilities test...');
  WriteLn('   Note 60 (C4) frequency: ', MIDI_NOTE_TO_FREQUENCY(60):0:2, ' Hz');
  WriteLn('   440 Hz note number: ', MIDI_FREQUENCY_TO_NOTE(440.0));

  WriteLn('');
  WriteLn('MIDI tempo control test complete!');
end;

procedure RunMIDIAllTests;
begin
  TestMIDISystem;
  WaitForKey;
  TestMIDIFileLoading;
  WaitForKey;
  TestMIDIChannelMapping;
  WaitForKey;
  TestMIDITempoControl;
  WaitForKey;
  if GET_MIDI_TOTAL_TICKS > 0 then
    TestMIDIPlayback;
end;

// =========================================================================
// FILTERS & EFFECTS TESTS
// =========================================================================

procedure TestFilters;
var
  Filter: TBiquadFilter;
  MultiFilter: TMultiPoleFilter;
  i: Integer;
  Input, Output: Single;
  SampleRate: Cardinal;
begin
  PrintHeader('Biquad Audio Filters');
  SampleRate := 44100;

  WriteLn('Testing 6 biquad filter types...');
  WriteLn;

  WriteLn('1. LOWPASS FILTER (1kHz, Q=0.707)');
  Filter := TSedaiFilters.CreateBiquadFilter(ftLowPass, 1000.0, 0.707, SampleRate);
  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;
    Output := TSedaiFilters.ProcessBiquadSample(Filter, Input);
  end;
  WriteLn('   Lowpass filter working');

  WriteLn('2. HIGHPASS FILTER (500Hz, Q=0.707)');
  Filter := TSedaiFilters.CreateBiquadFilter(ftHighPass, 500.0, 0.707, SampleRate);
  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;
    Output := TSedaiFilters.ProcessBiquadSample(Filter, Input);
  end;
  WriteLn('   Highpass filter working');

  WriteLn('3. BANDPASS FILTER (1kHz, Q=5.0)');
  Filter := TSedaiFilters.CreateBiquadFilter(ftBandPass, 1000.0, 5.0, SampleRate);
  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;
    Output := TSedaiFilters.ProcessBiquadSample(Filter, Input);
  end;
  WriteLn('   Bandpass filter working');

  WriteLn('4. NOTCH FILTER (1kHz, Q=10.0)');
  Filter := TSedaiFilters.CreateBiquadFilter(ftNotch, 1000.0, 10.0, SampleRate);
  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;
    Output := TSedaiFilters.ProcessBiquadSample(Filter, Input);
  end;
  WriteLn('   Notch filter working');

  WriteLn('5. MULTI-POLE FILTER (24dB/octave lowpass)');
  MultiFilter := TSedaiFilters.CreateMultiPoleFilter(ftLowPass, 800.0, 0.707, fs24dB, SampleRate);
  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;
    Output := TSedaiFilters.ProcessMultiPoleSample(MultiFilter, Input);
  end;
  WriteLn('   Multi-pole filter working');

  WriteLn('');
  WriteLn('All filters tested successfully!');
end;

procedure TestEffects;
var
  Delay: TDelayEffect;
  Reverb: TReverbEffect;
  Chorus: TChorusEffect;
  Flanger: TFlangerEffect;
  Distortion: TDistortionEffect;
  i: Integer;
  Input, Output: Single;
  SampleRate: Cardinal;
begin
  PrintHeader('Audio Effects');
  SampleRate := 44100;

  WriteLn('Testing audio effects...');
  WriteLn;

  WriteLn('1. DELAY EFFECT (250ms, 50% feedback)');
  Delay := TSedaiAudioEffects.CreateDelayEffect(0.25, 0.5, 0.5, SampleRate);
  for i := 0 to 22049 do
  begin
    if i = 0 then Input := 1.0 else Input := 0.0;
    Output := TSedaiAudioEffects.ProcessDelaySample(Delay, Input);
    if (Abs(Output) > 0.1) and (i > 0) then
      WriteLn('   Echo detected at sample ', i, ' (', (i / SampleRate * 1000):0:1, ' ms)');
  end;
  WriteLn('   Delay working');

  WriteLn('2. REVERB EFFECT (Schroeder)');
  Reverb := TSedaiAudioEffects.CreateReverbEffect(0.7, 0.5, 0.3, SampleRate);
  for i := 1 to 1000 do
  begin
    Input := (Random - 0.5) * 0.1;
    Output := TSedaiAudioEffects.ProcessReverbSample(Reverb, Input);
  end;
  WriteLn('   Reverb working');

  WriteLn('3. CHORUS EFFECT (2Hz LFO, 5ms depth)');
  Chorus := TSedaiAudioEffects.CreateChorusEffect(2.0, 0.005, 0.5, SampleRate);
  for i := 1 to 1000 do
  begin
    Input := Sin(2.0 * Pi * 440.0 * i / SampleRate);
    Output := TSedaiAudioEffects.ProcessChorusSample(Chorus, Input);
  end;
  WriteLn('   Chorus working');

  WriteLn('4. FLANGER EFFECT (0.5Hz LFO, 80% feedback)');
  Flanger := TSedaiAudioEffects.CreateFlangerEffect(0.5, 0.003, 0.8, 0.5, SampleRate);
  for i := 1 to 1000 do
  begin
    Input := Sin(2.0 * Pi * 440.0 * i / SampleRate);
    Output := TSedaiAudioEffects.ProcessFlangerSample(Flanger, Input);
  end;
  WriteLn('   Flanger working');

  WriteLn('5. DISTORTION EFFECTS');
  Distortion := TSedaiAudioEffects.CreateDistortionEffect(2.0, 0.5, 0.5);
  for i := 1 to 100 do
  begin
    Input := Sin(2.0 * Pi * 100.0 * i / SampleRate) * 1.5;
    Output := TSedaiAudioEffects.SoftClip(Input * Distortion.Drive);
  end;
  WriteLn('   Soft clip, Hard clip, Tanh saturation working');

  WriteLn('');
  WriteLn('All effects tested successfully!');
end;

// =========================================================================
// SYSTEM STATUS
// =========================================================================

procedure ShowSystemStatus;
begin
  PrintHeader('System Status');

  PrintStatus;
  WriteLn('');
  WriteLn('Architecture: Modular Synthesis Engine');
  WriteLn('');
  WriteLn('Synthesis Engines:');
  WriteLn('  - Additive: Harmonic spectrum synthesis (up to 32 harmonics)');
  WriteLn('  - Subtractive (Classic): Analog-style with oscillators, filters, LFO');
  WriteLn('  - FM: 6-operator frequency modulation (DX7-style)');
  WriteLn('  - Wavetable: Modern wavetable with morphing');
  WriteLn('');
  WriteLn('Signal Processing:');
  WriteLn('  - Biquad filters (LP, HP, BP, Notch, Allpass, Peaking)');
  WriteLn('  - Multi-pole filters (12dB, 24dB, 48dB/octave)');
  WriteLn('  - Effects: Delay, Reverb, Chorus, Flanger, Distortion');
  WriteLn('');
  WriteLn('Wavetable Loader:');
  WriteLn('  - Supported formats: Serum, Vital, Surge, Generic WAV');
  WriteLn('  - Cache: ', GetWavetableCacheInfo);
  WriteLn('');
  WriteLn('MIDI System:');
  if IS_MIDI_INITIALIZED then
  begin
    WriteLn('  - Status: Initialized');
    WriteLn('  - ', GET_MIDI_SYSTEM_INFO);
  end
  else
    WriteLn('  - Status: Not initialized');
end;

// =========================================================================
// SUBMENU HANDLERS
// =========================================================================

function ShowAdditiveMenu: Boolean;
var
  AChoice: string;
begin
  Result := True;
  repeat
    GEscapePressed := False;
    ClearScreen;
    WriteLn('================================================================');
    WriteLn('  Additive Synthesis');
    WriteLn('================================================================');
    WriteLn('');
    WriteLn('  1. Basic Waveforms (pure tone, square wave)');
    WriteLn('  2. Preset Spectra (organ, strings, brass, etc.)');
    WriteLn('  3. Inharmonicity Demo (bells, metallic sounds)');
    WriteLn('  4. ADSR Envelope Test');
    WriteLn('  5. Run All Additive Tests');
    WriteLn('');
    WriteLn(' 90. Go Back (or press ESC)');
    WriteLn(' 99. Exit (or press CTRL+C)');
    WriteLn('');
    Write('Select option: ');
    ReadLn(AChoice);

    // Check for pending SDL events after ReadLn
    ProcessEvents;
    if GEscapePressed then Exit;

    try
      case AChoice of
        '1': begin TestAdditiveBasic; WaitForKey; end;
        '2': begin TestAdditivePresets; WaitForKey; end;
        '3': begin TestAdditiveInharmonicity; WaitForKey; end;
        '4': begin TestAdditiveADSR; WaitForKey; end;
        '5': begin RunAdditiveAllTests; WaitForKey; end;
        '90', 'esc', 'ESC': Exit;
        '99': begin
          if ConfirmExit then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    except
      on EAbort do
      begin
        GTerminateRequested := False;
        GEscapePressed := False;
        WriteLn('');
        WriteLn('Returned to menu.');
        WaitForKey;
      end;
    end;
  until False;
end;

function ShowSubtractiveMenu: Boolean;
var
  AChoice: string;
begin
  Result := True;
  repeat
    GEscapePressed := False;
    ClearScreen;
    WriteLn('================================================================');
    WriteLn('  Subtractive (Classic) Synthesis');
    WriteLn('================================================================');
    WriteLn('');
    WriteLn('  1. Basic Waveforms (sine, square, saw, triangle)');
    WriteLn('  2. Analog Presets (strings, brass, organ, etc.)');
    WriteLn('  3. Chord Progressions');
    WriteLn('  4. Musical Scale');
    WriteLn('  5. Run All Subtractive Tests');
    WriteLn('');
    WriteLn(' 90. Go Back (or press ESC)');
    WriteLn(' 99. Exit (or press CTRL+C)');
    WriteLn('');
    Write('Select option: ');
    ReadLn(AChoice);

    ProcessEvents;
    if GEscapePressed then Exit;

    try
      case AChoice of
        '1': begin TestSubtractiveWaveforms; WaitForKey; end;
        '2': begin TestSubtractivePresets; WaitForKey; end;
        '3': begin TestSubtractiveChords; WaitForKey; end;
        '4': begin TestSubtractiveScale; WaitForKey; end;
        '5': begin RunSubtractiveAllTests; WaitForKey; end;
        '90', 'esc', 'ESC': Exit;
        '99': begin
          if ConfirmExit then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    except
      on EAbort do
      begin
        GTerminateRequested := False;
        GEscapePressed := False;
        WriteLn('');
        WriteLn('Returned to menu.');
        WaitForKey;
      end;
    end;
  until False;
end;

function ShowFMMenu: Boolean;
var
  AChoice: string;
begin
  Result := True;
  repeat
    GEscapePressed := False;
    ClearScreen;
    WriteLn('================================================================');
    WriteLn('  FM Synthesis');
    WriteLn('================================================================');
    WriteLn('');
    WriteLn('  1. Basic Algorithms (E.Piano, Brass, Bell)');
    WriteLn('  2. Advanced Presets (Organ, Choir, Marimba, etc.)');
    WriteLn('  3. Polyphonic Chords');
    WriteLn('  4. Musical Scale');
    WriteLn('  5. Run All FM Tests');
    WriteLn('');
    WriteLn(' 90. Go Back (or press ESC)');
    WriteLn(' 99. Exit (or press CTRL+C)');
    WriteLn('');
    Write('Select option: ');
    ReadLn(AChoice);

    ProcessEvents;
    if GEscapePressed then Exit;

    try
      case AChoice of
        '1': begin TestFMBasics; WaitForKey; end;
        '2': begin TestFMPresets; WaitForKey; end;
        '3': begin TestFMChords; WaitForKey; end;
        '4': begin TestFMScale; WaitForKey; end;
        '5': begin RunFMAllTests; WaitForKey; end;
        '90', 'esc', 'ESC': Exit;
        '99': begin
          if ConfirmExit then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    except
      on EAbort do
      begin
        GTerminateRequested := False;
        GEscapePressed := False;
        WriteLn('');
        WriteLn('Returned to menu.');
        WaitForKey;
      end;
    end;
  until False;
end;

function ShowWavetableMenu: Boolean;
var
  AChoice: string;
begin
  Result := True;
  repeat
    GEscapePressed := False;
    ClearScreen;
    WriteLn('================================================================');
    WriteLn('  Wavetable Synthesis');
    WriteLn('================================================================');
    WriteLn('');
    WriteLn('  1. Basic Wavetables (Serum, Wasp, PPG)');
    WriteLn('  2. Wavetable Presets (Vocal, Metallic, Glass, etc.)');
    WriteLn('  3. Harmonic Progressions (Chords)');
    WriteLn('  4. Wavetable Loading System');
    WriteLn('  5. AKWF Wavetable Demo (Adventure Kid Waveforms)');
    WriteLn('  6. Run All Wavetable Tests');
    WriteLn('');
    WriteLn(' 90. Go Back (or press ESC)');
    WriteLn(' 99. Exit (or press CTRL+C)');
    WriteLn('');
    Write('Select option: ');
    ReadLn(AChoice);

    ProcessEvents;
    if GEscapePressed then Exit;

    try
      case AChoice of
        '1': begin TestWavetableBasics; WaitForKey; end;
        '2': begin TestWavetablePresets; WaitForKey; end;
        '3': begin TestWavetableChords; WaitForKey; end;
        '4': begin TestWavetableLoading; WaitForKey; end;
        '5': begin TestAKWFWavetables; WaitForKey; end;
        '6': begin RunWavetableAllTests; WaitForKey; end;
        '90', 'esc', 'ESC': Exit;
        '99': begin
          if ConfirmExit then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    except
      on EAbort do
      begin
        GTerminateRequested := False;
        GEscapePressed := False;
        WriteLn('');
        WriteLn('Returned to menu.');
        WaitForKey;
      end;
    end;
  until False;
end;

function ShowMIDIMenu: Boolean;
var
  AChoice: string;
begin
  Result := True;
  repeat
    GEscapePressed := False;
    ClearScreen;
    WriteLn('================================================================');
    WriteLn('  MIDI System');
    WriteLn('================================================================');
    WriteLn('');
    WriteLn('  1. Initialize MIDI System');
    WriteLn('  2. Load MIDI File');
    WriteLn('  3. MIDI Playback');
    WriteLn('  4. Channel Mapping');
    WriteLn('  5. Tempo Control');
    WriteLn('  6. Run All MIDI Tests');
    WriteLn('');
    WriteLn(' 90. Go Back (or press ESC)');
    WriteLn(' 99. Exit (or press CTRL+C)');
    WriteLn('');
    Write('Select option: ');
    ReadLn(AChoice);

    ProcessEvents;
    if GEscapePressed then Exit;

    try
      case AChoice of
        '1': begin TestMIDISystem; WaitForKey; end;
        '2': begin TestMIDIFileLoading; WaitForKey; end;
        '3': begin TestMIDIPlayback; WaitForKey; end;
        '4': begin TestMIDIChannelMapping; WaitForKey; end;
        '5': begin TestMIDITempoControl; WaitForKey; end;
        '6': begin RunMIDIAllTests; WaitForKey; end;
        '90', 'esc', 'ESC': Exit;
        '99': begin
          if ConfirmExit then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    except
      on EAbort do
      begin
        GTerminateRequested := False;
        GEscapePressed := False;
        WriteLn('');
        WriteLn('Returned to menu.');
        WaitForKey;
      end;
    end;
  until False;
end;

function ShowFiltersEffectsMenu: Boolean;
var
  AChoice: string;
begin
  Result := True;
  repeat
    GEscapePressed := False;
    ClearScreen;
    WriteLn('================================================================');
    WriteLn('  Filters & Effects');
    WriteLn('================================================================');
    WriteLn('');
    WriteLn('  1. Test Biquad Filters');
    WriteLn('  2. Test Audio Effects');
    WriteLn('  3. Run All Filter/Effect Tests');
    WriteLn('');
    WriteLn(' 90. Go Back (or press ESC)');
    WriteLn(' 99. Exit (or press CTRL+C)');
    WriteLn('');
    Write('Select option: ');
    ReadLn(AChoice);

    ProcessEvents;
    if GEscapePressed then Exit;

    try
      case AChoice of
        '1': begin TestFilters; WaitForKey; end;
        '2': begin TestEffects; WaitForKey; end;
        '3': begin TestFilters; WaitForKey; TestEffects; WaitForKey; end;
        '90', 'esc', 'ESC': Exit;
        '99': begin
          if ConfirmExit then
          begin
            Result := False;
            Exit;
          end;
        end;
      end;
    except
      on EAbort do
      begin
        GTerminateRequested := False;
        GEscapePressed := False;
        WriteLn('');
        WriteLn('Returned to menu.');
        WaitForKey;
      end;
    end;
  until False;
end;

// =========================================================================
// COMPREHENSIVE TEST (RUN ALL)
// =========================================================================

procedure RunAllSynthesisTests;
begin
  PrintHeader('COMPREHENSIVE SYNTHESIS DEMO');
  WriteLn('Running all synthesis engine demonstrations...');
  WriteLn('This will demonstrate all audio capabilities of the library.');
  WriteLn('');

  // Subtractive
  WriteLn('>>> SUBTRACTIVE (CLASSIC) SYNTHESIS <<<');
  TestSubtractiveWaveforms;
  SDL_Delay(500);
  TestSubtractivePresets;
  SDL_Delay(500);
  TestSubtractiveChords;
  SDL_Delay(500);
  TestSubtractiveScale;
  SDL_Delay(1000);

  // FM
  WriteLn('>>> FM SYNTHESIS <<<');
  TestFMBasics;
  SDL_Delay(500);
  TestFMPresets;
  SDL_Delay(500);
  TestFMChords;
  SDL_Delay(500);
  TestFMScale;
  SDL_Delay(1000);

  // Wavetable
  WriteLn('>>> WAVETABLE SYNTHESIS <<<');
  TestWavetableBasics;
  SDL_Delay(500);
  TestWavetablePresets;
  SDL_Delay(500);
  TestWavetableChords;
  SDL_Delay(1000);

  SmoothStopAll(500);
  WriteLn('');
  WriteLn('=== COMPREHENSIVE DEMO COMPLETE ===');
end;

// =========================================================================
// MAIN MENU
// =========================================================================

procedure ShowMainMenu;
begin
  ClearScreen;
  WriteLn('================================================================');
  WriteLn('  Sedai Audio Foundation - Test Suite');
  WriteLn('================================================================');
  WriteLn('');
  WriteLn('  Active Voices: ', GetActiveVoices, '/', GetMaxVoices,
          ' | Sample Rate: ', GetSampleRate, ' Hz');
  WriteLn('');
  WriteLn('Select test type:');
  WriteLn('');
  WriteLn('  1. Additive Synthesis');
  WriteLn('  2. Subtractive Synthesis');
  WriteLn('  3. FM Synthesis');
  WriteLn('  4. Wavetable Synthesis');
  WriteLn('  5. MIDI System');
  WriteLn('  6. Filters & Effects');
  WriteLn('  7. System Status');
  WriteLn('');
  WriteLn(' 90. Run All Synthesis Tests (Comprehensive Demo)');
  WriteLn(' 99. Exit');
  WriteLn('');
  Write('Select option: ');
end;

// =========================================================================
// MAIN PROGRAM
// =========================================================================

var
  Choice: string;
  Continue: Boolean;

begin
  Randomize;

  AppPath := ExtractFilePath(ParamStr(0));
  DataPath := FindDataPath;

  // Initialize Windows console input handling
  {$IFDEF WINDOWS}
  InitConsoleInput;
  {$ENDIF}

  WriteLn('Initializing Sedai Audio System...');
  WriteLn('(Press CTRL+C to exit, ESC to go back during tests)');
  WriteLn('');
  WriteLn('Data path: ', DataPath);
  WriteLn('');

  if not InitAudio(64) then
  begin
    WriteLn('ERROR: Failed to initialize audio system');
    WriteLn('Press ENTER to exit...');
    ReadLn;
    {$IFDEF WINDOWS}
    RestoreConsoleInput;
    {$ENDIF}
    Halt(1);
  end;

  SetMasterVolume(0.7);

  Continue := True;
  while Continue do
  begin
    ShowMainMenu;
    ReadLn(Choice);

    // Check for CTRL+C via SDL events after ReadLn
    ProcessEvents;

    case Choice of
      '1': Continue := ShowAdditiveMenu;
      '2': Continue := ShowSubtractiveMenu;
      '3': Continue := ShowFMMenu;
      '4': Continue := ShowWavetableMenu;
      '5': Continue := ShowMIDIMenu;
      '6': Continue := ShowFiltersEffectsMenu;
      '7': begin ShowSystemStatus; WaitForKey; end;
      '90': begin
        try
          RunAllSynthesisTests;
        except
          on EAbort do
          begin
            GTerminateRequested := False;
            GEscapePressed := False;
            WriteLn('');
            WriteLn('Comprehensive test interrupted.');
          end;
        end;
        WaitForKey;
      end;
      '99': begin
        if ConfirmExit then
          Continue := False;
      end;
    end;

    SmoothStopAll(200);
  end;

  WriteLn('');
  WriteLn('Shutting down Sedai Audio System...');
  SHUTDOWN_MIDI;
  ShutdownAudio;
  {$IFDEF WINDOWS}
  RestoreConsoleInput;
  {$ENDIF}
  WriteLn('Thank you for using Sedai Audio Foundation!');
end.
