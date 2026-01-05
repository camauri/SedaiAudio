{*
 * Sedai Audio Foundation - Audio Test Demo
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * Interactive demo showcasing synthesis capabilities using the new modular API.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

program AudioTest;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Math, Classes,
  SDL2,
  SedaiAudioTypes,
  SedaiOscillator,
  SedaiEnvelope,
  SedaiFilter,
  SedaiVoice,
  SedaiSIDEvo,
  SedaiDelay,
  SedaiReverb,
  SedaiChorus,
  SedaiMIDIPlayer,
  SedaiAdditiveGenerator,
  SedaiAudioFoundation,
  SedaiAudioEngine;

const
  SAMPLE_RATE = 44100;

var
  GTerminateRequested: Boolean = False;
  GEscapePressed: Boolean = False;

// ============================================================================
// CONSOLE INPUT (Windows)
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

function ReadConsoleKey: Char;
var
  AInputRec: INPUT_RECORD;
  AEventsRead: DWORD;
  AVk: Word;
begin
  Result := #0;
  if GConsoleHandle = INVALID_HANDLE_VALUE then Exit;

  if ConsoleKeyAvailable then
  begin
    if ReadConsoleInput(GConsoleHandle, @AInputRec, 1, @AEventsRead) then
    begin
      if (AEventsRead > 0) and (AInputRec.EventType = KEY_EVENT) and
         (AInputRec.Event.KeyEvent.bKeyDown) then
      begin
        AVk := AInputRec.Event.KeyEvent.wVirtualKeyCode;
        if AVk = VK_ESCAPE then
          Result := #27
        else if AVk = VK_RETURN then
          Result := #13
        else if (AVk >= Ord('A')) and (AVk <= Ord('Z')) then
          Result := Chr(AVk)
        else if (AVk >= Ord('0')) and (AVk <= Ord('9')) then
          Result := Chr(AVk)
        else
          Result := AInputRec.Event.KeyEvent.AsciiChar;
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

function ConsoleKeyAvailable: Boolean;
begin
  Result := False;
end;

function ReadConsoleKey: Char;
begin
  Result := #0;
end;
{$ENDIF}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

procedure ClearScreen;
{$IFDEF WINDOWS}
var
  StdOut: THandle;
  ConsoleInfo: CONSOLE_SCREEN_BUFFER_INFO;
  CellsWritten: DWORD;
  ConsoleSize: DWORD;
  Origin: COORD;
begin
  StdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if StdOut = INVALID_HANDLE_VALUE then Exit;

  if not GetConsoleScreenBufferInfo(StdOut, @ConsoleInfo) then Exit;

  ConsoleSize := ConsoleInfo.dwSize.X * ConsoleInfo.dwSize.Y;
  Origin.X := 0;
  Origin.Y := 0;
  FillConsoleOutputCharacter(StdOut, ' ', ConsoleSize, Origin, @CellsWritten);
  FillConsoleOutputAttribute(StdOut, ConsoleInfo.wAttributes, ConsoleSize, Origin, @CellsWritten);
  SetConsoleCursorPosition(StdOut, Origin);
end;
{$ELSE}
begin
  Write(#27'[2J'#27'[H');
end;
{$ENDIF}

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn(StringOfChar('=', 60));
  WriteLn('  ', ATitle);
  WriteLn(StringOfChar('=', 60));
  WriteLn;
end;

procedure WaitForKey(const AMessage: string = 'Press ENTER to continue...');
begin
  WriteLn;
  Write(AMessage);
  ReadLn;
end;

procedure InterruptibleDelay(AMilliseconds: Cardinal);
var
  StartTime: QWord;
  Key: Char;
begin
  StartTime := SDL_GetTicks;
  while (SDL_GetTicks - StartTime < AMilliseconds) and not GEscapePressed do
  begin
    Key := ReadConsoleKey;
    if Key = #27 then
    begin
      GEscapePressed := True;
      Break;
    end;
    SDL_Delay(10);
  end;
end;

// ============================================================================
// ADDITIVE SYNTHESIS TESTS
// ============================================================================

var
  GAdditiveGen: TSedaiAdditiveGenerator = nil;

procedure PrintHarmonicSpectrum(AGen: TSedaiAdditiveGenerator);
var
  I: Integer;
  MaxLevel: Single;
  BarLength: Integer;
begin
  WriteLn;
  WriteLn('Harmonic spectrum:');
  WriteLn('-----------------------------------------------------');

  MaxLevel := 0;
  for I := 0 to AGen.HarmonicCount - 1 do
    if AGen.GetHarmonicLevel(I) > MaxLevel then
      MaxLevel := AGen.GetHarmonicLevel(I);

  if MaxLevel = 0 then MaxLevel := 1.0;

  for I := 0 to Min(15, AGen.HarmonicCount - 1) do
  begin
    if AGen.GetHarmonicLevel(I) > 0.001 then
    begin
      BarLength := Round((AGen.GetHarmonicLevel(I) / MaxLevel) * 40);
      WriteLn(Format('H%2d: %s %.2f', [
        I + 1,
        StringOfChar('#', BarLength),
        AGen.GetHarmonicLevel(I)
      ]));
    end;
  end;

  WriteLn('-----------------------------------------------------');
end;

procedure TestAdditiveBasicWaveforms;
begin
  PrintHeader('Additive Synthesis - Basic Waveforms');

  // Create additive generator if not exists
  if GAdditiveGen = nil then
  begin
    GAdditiveGen := TSedaiAdditiveGenerator.Create;
    GAdditiveGen.SetSampleRate(SAMPLE_RATE);
  end;

  WriteLn('1. Pure Sine Wave (fundamental only)');
  GAdditiveGen.LoadSineWave;
  PrintHarmonicSpectrum(GAdditiveGen);
  WriteLn('Press ENTER to continue...');
  ReadLn;

  WriteLn('2. Square Wave (odd harmonics 1/n)');
  GAdditiveGen.LoadSquareWave;
  PrintHarmonicSpectrum(GAdditiveGen);
  WriteLn('Press ENTER to continue...');
  ReadLn;

  WriteLn('3. Sawtooth Wave (all harmonics 1/n)');
  GAdditiveGen.LoadSawWave;
  PrintHarmonicSpectrum(GAdditiveGen);
  WriteLn('Press ENTER to continue...');
  ReadLn;

  WriteLn('4. Triangle Wave (odd harmonics 1/n^2)');
  GAdditiveGen.LoadTriangleWave;
  PrintHarmonicSpectrum(GAdditiveGen);

  WriteLn;
  WriteLn('Basic waveforms test complete.');
end;

procedure TestAdditivePresets;
begin
  PrintHeader('Additive Synthesis - Preset Spectra');

  if GAdditiveGen = nil then
  begin
    GAdditiveGen := TSedaiAdditiveGenerator.Create;
    GAdditiveGen.SetSampleRate(SAMPLE_RATE);
  end;

  WriteLn('1. ORGAN (Hammond drawbar simulation)');
  GAdditiveGen.LoadOrganWave;
  PrintHarmonicSpectrum(GAdditiveGen);
  WriteLn('Press ENTER to continue...');
  ReadLn;

  WriteLn('2. STRINGS (rich harmonics with gradual rolloff)');
  GAdditiveGen.LoadStringsWave;
  PrintHarmonicSpectrum(GAdditiveGen);
  WriteLn('Press ENTER to continue...');
  ReadLn;

  WriteLn('3. BRASS (strong mid-harmonics)');
  GAdditiveGen.LoadBrassWave;
  PrintHarmonicSpectrum(GAdditiveGen);
  WriteLn('Press ENTER to continue...');
  ReadLn;

  WriteLn('4. FLUTE (mostly fundamental)');
  GAdditiveGen.LoadFluteWave;
  PrintHarmonicSpectrum(GAdditiveGen);
  WriteLn('Press ENTER to continue...');
  ReadLn;

  WriteLn('5. BELL (inharmonic partials)');
  GAdditiveGen.LoadBellWave;
  PrintHarmonicSpectrum(GAdditiveGen);
  WriteLn('Press ENTER to continue...');
  ReadLn;

  WriteLn('6. CHOIR (formant-like structure)');
  GAdditiveGen.LoadChoirWave;
  PrintHarmonicSpectrum(GAdditiveGen);

  WriteLn;
  WriteLn('Preset spectra test complete.');
end;

procedure TestAdditiveCustom;
var
  I: Integer;
begin
  PrintHeader('Additive Synthesis - Custom Harmonics');

  if GAdditiveGen = nil then
  begin
    GAdditiveGen := TSedaiAdditiveGenerator.Create;
    GAdditiveGen.SetSampleRate(SAMPLE_RATE);
  end;

  WriteLn('Creating custom spectrum with detuned harmonics...');
  WriteLn;

  GAdditiveGen.ClearAllHarmonics;

  // Set up custom harmonics with detuning
  GAdditiveGen.SetHarmonicLevel(0, 1.0);   // Fundamental
  GAdditiveGen.SetHarmonicLevel(2, 0.5);   // 3rd harmonic
  GAdditiveGen.SetHarmonicDetune(2, 5);    // Detune +5 cents
  GAdditiveGen.SetHarmonicLevel(4, 0.3);   // 5th harmonic
  GAdditiveGen.SetHarmonicDetune(4, -3);   // Detune -3 cents
  GAdditiveGen.SetHarmonicLevel(6, 0.2);   // 7th harmonic
  GAdditiveGen.SetHarmonicDetune(6, 8);    // Detune +8 cents
  GAdditiveGen.SetHarmonicLevel(9, 0.15);  // 10th harmonic

  PrintHarmonicSpectrum(GAdditiveGen);

  WriteLn;
  WriteLn('Harmonic detuning creates rich, chorus-like sounds.');
  WriteLn('This technique is used for realistic bells, metallic sounds,');
  WriteLn('and piano strings (where string stiffness causes inharmonicity).');

  WriteLn;
  WriteLn('Custom harmonics test complete.');
end;

procedure TestAdditiveInfo;
begin
  PrintHeader('Additive Synthesis - Information');

  WriteLn('What is Additive Synthesis?');
  WriteLn;
  WriteLn('Additive synthesis builds complex timbres by combining');
  WriteLn('multiple sine wave oscillators (partials/harmonics).');
  WriteLn;
  WriteLn('Key concepts:');
  WriteLn('  - Fundamental frequency (1st harmonic)');
  WriteLn('  - Harmonics: integer multiples of fundamental (2x, 3x, 4x...)');
  WriteLn('  - Each harmonic has its own amplitude');
  WriteLn('  - Optionally: per-harmonic envelopes and detuning');
  WriteLn;
  WriteLn('Classic waveform formulas:');
  WriteLn('  - Sine: Only fundamental (no harmonics)');
  WriteLn('  - Square: Odd harmonics (1,3,5,7...) at 1/n amplitude');
  WriteLn('  - Sawtooth: All harmonics at 1/n amplitude');
  WriteLn('  - Triangle: Odd harmonics at 1/n^2 amplitude');
  WriteLn;
  WriteLn('SAF Additive Generator features:');
  WriteLn('  - Up to 64 harmonics');
  WriteLn('  - Per-harmonic amplitude and detuning (in cents)');
  WriteLn('  - Optional per-harmonic ADSR envelopes');
  WriteLn('  - 10 preset waveforms (Sine, Saw, Square, Triangle,');
  WriteLn('    Organ, Bell, Strings, Choir, Brass, Flute)');
  WriteLn('  - Automatic Nyquist limiting for alias-free output');
end;

procedure RunAdditiveAllTests;
begin
  TestAdditiveBasicWaveforms;
  WaitForKey;
  TestAdditivePresets;
  WaitForKey;
  TestAdditiveCustom;
  WaitForKey;
  TestAdditiveInfo;
end;

// ============================================================================
// BASIC WAVEFORM TESTS
// ============================================================================

procedure TestBasicWaveforms;
begin
  PrintHeader('Basic Waveforms Test');

  WriteLn('Playing Sine wave (440 Hz)...');
  PlaySine(440.0, 0.8);
  InterruptibleDelay(1000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Square wave (440 Hz)...');
  PlaySquare(440.0, 0.8);
  InterruptibleDelay(1000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Sawtooth wave (440 Hz)...');
  PlaySaw(440.0, 0.8);
  InterruptibleDelay(1000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Triangle wave (440 Hz)...');
  PlayTriangle(440.0, 0.8);
  InterruptibleDelay(1000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Pulse wave (440 Hz, 25% width)...');
  PlayPulse(440.0, 0.25, 0.8);
  InterruptibleDelay(1000);
  StopAll;

  WriteLn;
  WriteLn('Basic waveforms test complete.');
end;

// ============================================================================
// CLASSIC SUBTRACTIVE SYNTHESIS TESTS
// ============================================================================

procedure TestClassicPresets;
begin
  PrintHeader('Classic Subtractive Synthesis Presets');

  WriteLn('Playing Lead preset (C4)...');
  PlayLead(NOTE_C4);
  InterruptibleDelay(1500);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Bass preset (C3)...');
  PlayBass(NOTE_C4 / 2);
  InterruptibleDelay(1500);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Pad preset (C4)...');
  PlayPad(NOTE_C4);
  InterruptibleDelay(2000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Strings preset...');
  PlayClassic(NOTE_C4, 'strings');
  InterruptibleDelay(2000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Brass preset...');
  PlayClassic(NOTE_C4, 'brass');
  InterruptibleDelay(1500);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Organ preset...');
  PlayClassic(NOTE_C4, 'organ');
  InterruptibleDelay(1500);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Pluck preset...');
  PlayClassic(NOTE_C4, 'pluck');
  InterruptibleDelay(1500);
  StopAll;

  WriteLn;
  WriteLn('Classic presets test complete.');
end;

// ============================================================================
// FM SYNTHESIS TESTS
// ============================================================================

procedure TestFMPresets;
begin
  PrintHeader('FM Synthesis Presets');

  WriteLn('Playing FM E-Piano...');
  PlayEPiano(NOTE_C4);
  InterruptibleDelay(2000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing FM Brass...');
  PlayFMBrass(NOTE_C4);
  InterruptibleDelay(1500);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing FM Bell...');
  PlayFMBell(NOTE_C4);
  InterruptibleDelay(2000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing FM Organ...');
  PlayFMOrgan(NOTE_C4);
  InterruptibleDelay(1500);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing FM Lead...');
  PlayFMLead(NOTE_C4);
  InterruptibleDelay(1500);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing FM Bass...');
  PlayFMBass(NOTE_C4 / 2);
  InterruptibleDelay(1500);
  StopAll;

  WriteLn;
  WriteLn('FM synthesis test complete.');
end;

// ============================================================================
// WAVETABLE SYNTHESIS TESTS
// ============================================================================

procedure TestWavetablePresets;
begin
  PrintHeader('Wavetable Synthesis Presets');

  WriteLn('Playing Serum-style preset...');
  PlaySerum(NOTE_C4);
  InterruptibleDelay(2000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing Wasp-style preset...');
  PlayWasp(NOTE_C4);
  InterruptibleDelay(2000);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing PPG-style preset...');
  PlayPPG(NOTE_C4);
  InterruptibleDelay(2000);
  StopAll;

  WriteLn;
  WriteLn('Wavetable synthesis test complete.');
end;

// ============================================================================
// CHORD TESTS
// ============================================================================

procedure TestChords;
begin
  PrintHeader('Chord Tests');

  WriteLn('Playing C Major chord (Classic)...');
  PlayChordClassic(NOTE_C4, NOTE_E4, NOTE_G4, 'strings');
  InterruptibleDelay(2500);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing C Major chord (FM E-Piano)...');
  PlayChordFM(NOTE_C4, NOTE_E4, NOTE_G4, 'epiano');
  InterruptibleDelay(2500);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing C Major chord (Wavetable)...');
  PlayChordWavetable(NOTE_C4, NOTE_E4, NOTE_G4, 'serum');
  InterruptibleDelay(2500);
  StopAll;

  WriteLn;
  WriteLn('Chord tests complete.');
end;

// ============================================================================
// SCALE TESTS
// ============================================================================

procedure TestScales;
begin
  PrintHeader('Scale Tests');

  WriteLn('Playing C Major scale (Classic)...');
  PlayScaleClassic(NOTE_C4, 'pluck');
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing C Major scale (FM)...');
  PlayScaleFM(NOTE_C4, 'epiano');
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Playing C Major scale (Wavetable)...');
  PlayScaleWavetable(NOTE_C4, 'serum');
  StopAll;

  WriteLn;
  WriteLn('Scale tests complete.');
end;

// ============================================================================
// SID CHIP TESTS
// ============================================================================

procedure TestSIDChip;
var
  SID: TSedaiSIDEvo;
begin
  PrintHeader('SID Chip Emulation Test');

  SID := GetSID;
  if SID = nil then
  begin
    WriteLn('Error: SID chip not available');
    Exit;
  end;

  WriteLn('SID Model: ', Ord(SID.Model));
  WriteLn('SID Clock: ', SID.Clock, ' Hz');
  WriteLn;

  // Play a simple note on voice 1
  WriteLn('Playing SID Voice 1 - Triangle wave...');

  // Set frequency (A4 = 440 Hz)
  // SID freq = (freq * 16777216) / clock
  // For PAL clock ~985248 Hz
  SID.WriteRegister($00, $A7);  // Freq low
  SID.WriteRegister($01, $1C);  // Freq high

  // ADSR: Attack=0, Decay=8, Sustain=15, Release=4
  SID.WriteRegister($05, $08);  // Attack/Decay
  SID.WriteRegister($06, $F4);  // Sustain/Release

  // Gate + Triangle waveform
  SID.WriteRegister($04, $11);

  // Master volume
  SID.WriteRegister($18, $0F);

  InterruptibleDelay(1500);

  // Gate off
  SID.WriteRegister($04, $10);
  InterruptibleDelay(500);

  if GEscapePressed then Exit;

  // Play pulse wave
  WriteLn('Playing SID Voice 1 - Pulse wave...');

  // Set pulse width (50%)
  SID.WriteRegister($02, $00);
  SID.WriteRegister($03, $08);

  // Gate + Pulse waveform
  SID.WriteRegister($04, $41);

  InterruptibleDelay(1500);

  // Gate off
  SID.WriteRegister($04, $40);
  InterruptibleDelay(500);

  if GEscapePressed then Exit;

  // Play sawtooth
  WriteLn('Playing SID Voice 1 - Sawtooth wave...');

  // Gate + Sawtooth waveform
  SID.WriteRegister($04, $21);

  InterruptibleDelay(1500);

  // Gate off
  SID.WriteRegister($04, $20);
  InterruptibleDelay(500);

  // Reset SID
  SID.Reset;

  WriteLn;
  WriteLn('SID chip test complete.');
end;

// ============================================================================
// MIDI UTILITIES TESTS
// ============================================================================

procedure TestMIDIUtilities;
begin
  PrintHeader('MIDI Utilities Test');

  WriteLn('MIDI Note Conversion:');
  WriteLn('  MIDI Note 60 (C4) = ', MIDINoteToFreq(60):0:2, ' Hz');
  WriteLn('  MIDI Note 69 (A4) = ', MIDINoteToFreq(69):0:2, ' Hz (expected: 440.00)');
  WriteLn('  MIDI Note 72 (C5) = ', MIDINoteToFreq(72):0:2, ' Hz');
  WriteLn;

  WriteLn('Frequency to MIDI Note:');
  WriteLn('  440.0 Hz = MIDI Note ', FreqToMIDINote(440.0), ' (expected: 69)');
  WriteLn('  261.63 Hz = MIDI Note ', FreqToMIDINote(261.63), ' (expected: 60)');
  WriteLn;

  WriteLn('Velocity Conversion:');
  WriteLn('  Velocity 127 = Amplitude ', MIDIVelocityToAmplitude(127):0:3);
  WriteLn('  Velocity 64 = Amplitude ', MIDIVelocityToAmplitude(64):0:3);
  WriteLn('  Velocity 32 = Amplitude ', MIDIVelocityToAmplitude(32):0:3);
  WriteLn;

  WriteLn('Amplitude to Velocity:');
  WriteLn('  Amplitude 1.0 = Velocity ', MIDIAmplitudeToVelocity(1.0));
  WriteLn('  Amplitude 0.5 = Velocity ', MIDIAmplitudeToVelocity(0.5));
  WriteLn;

  WriteLn('Pan Conversion:');
  WriteLn('  MIDI Pan 0 (left) = Sedai Pan ', MIDIPanToSedai(0):0:2);
  WriteLn('  MIDI Pan 64 (center) = Sedai Pan ', MIDIPanToSedai(64):0:2);
  WriteLn('  MIDI Pan 127 (right) = Sedai Pan ', MIDIPanToSedai(127):0:2);
  WriteLn;

  WriteLn('Note Names:');
  WriteLn('  MIDI 60 = ', MIDINoteToName(60), ' (expected: C4)');
  WriteLn('  MIDI 69 = ', MIDINoteToName(69), ' (expected: A4)');
  WriteLn('  MIDI 72 = ', MIDINoteToName(72), ' (expected: C5)');
  WriteLn;

  WriteLn('MIDI utilities test complete.');
end;

// ============================================================================
// UTILITY FUNCTIONS TESTS
// ============================================================================

procedure TestUtilityFunctions;
var
  VoiceIdx: Integer;
begin
  PrintHeader('Utility Functions Test');

  WriteLn('Testing Beep function...');
  Beep(880.0, 500, 0.3);
  InterruptibleDelay(600);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Testing PlayNote function...');
  PlayNote(0, 440.0, 800, 0.5, 'sine');
  InterruptibleDelay(900);
  StopAll;
  if GEscapePressed then Exit;

  WriteLn('Testing PlayOnVoice function (voice 0)...');
  PlayOnVoice(0, 523.25, 'square');
  InterruptibleDelay(800);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing ReleaseVoice function...');
  ReleaseVoice(0);
  InterruptibleDelay(500);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing multiple voices...');
  PlayOnVoice(0, NOTE_C4, 'sine');
  InterruptibleDelay(200);
  PlayOnVoice(1, NOTE_E4, 'sine');
  InterruptibleDelay(200);
  PlayOnVoice(2, NOTE_G4, 'sine');
  InterruptibleDelay(1000);

  WriteLn('Releasing all voices...');
  ReleaseVoice(0);
  ReleaseVoice(1);
  ReleaseVoice(2);
  InterruptibleDelay(500);

  StopAll;
  WriteLn;
  WriteLn('Utility functions test complete.');
end;

// ============================================================================
// MIDI VOICE MANAGEMENT TESTS
// ============================================================================

procedure TestMIDIVoiceManagement;
var
  Voice1, Voice2, Voice3: Integer;
  FreeCount: Integer;
begin
  PrintHeader('MIDI Voice Management Test');

  WriteLn('Initial free voice count: ', MIDIGetFreeVoiceCount);
  WriteLn;

  WriteLn('Allocating 3 MIDI voices...');
  Voice1 := MIDIAllocateVoice;
  Voice2 := MIDIAllocateVoice;
  Voice3 := MIDIAllocateVoice;
  WriteLn('  Voice 1 index: ', Voice1);
  WriteLn('  Voice 2 index: ', Voice2);
  WriteLn('  Voice 3 index: ', Voice3);
  WriteLn('  Free voices remaining: ', MIDIGetFreeVoiceCount);
  WriteLn;

  WriteLn('Configuring voices...');
  MIDISetVoiceFrequency(Voice1, NOTE_C4);
  MIDISetVoiceAmplitude(Voice1, 0.7);
  MIDISetVoiceWavetable(Voice1, 'basic');
  MIDISetVoicePan(Voice1, -0.5);

  MIDISetVoiceFrequency(Voice2, NOTE_E4);
  MIDISetVoiceAmplitude(Voice2, 0.6);
  MIDISetVoiceWavetable(Voice2, 'basic');
  MIDISetVoicePan(Voice2, 0.0);

  MIDISetVoiceFrequency(Voice3, NOTE_G4);
  MIDISetVoiceAmplitude(Voice3, 0.5);
  MIDISetVoiceWavetable(Voice3, 'basic');
  MIDISetVoicePan(Voice3, 0.5);
  WriteLn;

  WriteLn('Reading voice properties:');
  WriteLn('  Voice 1: Freq=', MIDIGetVoiceFrequency(Voice1):0:2,
          ' Hz, Amp=', MIDIGetVoiceAmplitude(Voice1):0:2,
          ', WT=', MIDIGetVoiceWavetable(Voice1),
          ', Pan=', MIDIGetVoicePan(Voice1):0:2);
  WriteLn('  Voice 2: Freq=', MIDIGetVoiceFrequency(Voice2):0:2,
          ' Hz, Amp=', MIDIGetVoiceAmplitude(Voice2):0:2);
  WriteLn('  Voice 3: Freq=', MIDIGetVoiceFrequency(Voice3):0:2,
          ' Hz, Amp=', MIDIGetVoiceAmplitude(Voice3):0:2);
  WriteLn;

  WriteLn('Activating voices...');
  MIDIVoiceOn(Voice1);
  InterruptibleDelay(300);
  if GEscapePressed then begin MIDIReleaseAllVoices; StopAll; Exit; end;

  MIDIVoiceOn(Voice2);
  InterruptibleDelay(300);
  if GEscapePressed then begin MIDIReleaseAllVoices; StopAll; Exit; end;

  MIDIVoiceOn(Voice3);
  InterruptibleDelay(1000);
  if GEscapePressed then begin MIDIReleaseAllVoices; StopAll; Exit; end;

  WriteLn('Releasing voice 2...');
  MIDIVoiceOff(Voice2);
  MIDIReleaseVoice(Voice2);
  InterruptibleDelay(500);
  if GEscapePressed then begin MIDIReleaseAllVoices; StopAll; Exit; end;

  WriteLn('Releasing all remaining voices...');
  MIDIReleaseAllVoices;
  InterruptibleDelay(500);

  WriteLn;
  WriteLn('Final free voice count: ', MIDIGetFreeVoiceCount);

  StopAll;
  WriteLn;
  WriteLn('MIDI voice management test complete.');
end;

// ============================================================================
// MIDI NOTE PLAYBACK TESTS
// ============================================================================

procedure TestMIDINotePlayback;
var
  V1, V2, V3: Integer;
begin
  PrintHeader('MIDI Note Playback Test');

  WriteLn('Testing MIDIPlayNote (MIDI-style note on)...');
  V1 := MIDIPlayNote(60, 100, 'basic');  // C4, velocity 100
  WriteLn('  Playing C4 (MIDI 60) on voice ', V1);
  InterruptibleDelay(800);
  if GEscapePressed then begin MIDIReleaseAllVoices; StopAll; Exit; end;

  V2 := MIDIPlayNote(64, 80, 'basic');   // E4, velocity 80
  WriteLn('  Playing E4 (MIDI 64) on voice ', V2);
  InterruptibleDelay(800);
  if GEscapePressed then begin MIDIReleaseAllVoices; StopAll; Exit; end;

  V3 := MIDIPlayNote(67, 90, 'basic');   // G4, velocity 90
  WriteLn('  Playing G4 (MIDI 67) on voice ', V3);
  InterruptibleDelay(1000);
  if GEscapePressed then begin MIDIReleaseAllVoices; StopAll; Exit; end;

  WriteLn('Releasing all notes...');
  MIDIReleaseAllVoices;
  InterruptibleDelay(500);

  WriteLn;
  WriteLn('Testing MIDIPlayNoteWithFreq...');
  V1 := MIDIPlayNoteWithFreq(440.0, 100, 'basic');
  WriteLn('  Playing 440 Hz (A4) on voice ', V1);
  InterruptibleDelay(1000);
  if GEscapePressed then begin MIDIReleaseAllVoices; StopAll; Exit; end;

  MIDIReleaseAllVoices;
  InterruptibleDelay(500);

  WriteLn;
  WriteLn('Testing PlayWavetableMIDI...');
  V1 := PlayWavetableMIDI(261.63, 'basic', 100, 64);
  WriteLn('  Playing C4 wavetable on voice ', V1);
  InterruptibleDelay(1000);
  if GEscapePressed then begin MIDIReleaseAllVoices; StopAll; Exit; end;

  MIDIReleaseAllVoices;
  StopAll;

  WriteLn;
  WriteLn('MIDI note playback test complete.');
end;

// ============================================================================
// ADVANCED VOICE CONTROL TESTS
// ============================================================================

procedure TestAdvancedVoiceControl;
var
  VoiceIdx: Integer;
begin
  PrintHeader('Advanced Voice Control Test');

  WriteLn('Testing voice allocation and advanced controls...');
  VoiceIdx := PlayClassicAdv(440.0, 'saw');
  if VoiceIdx < 0 then
  begin
    WriteLn('Error: Failed to allocate voice');
    Exit;
  end;
  WriteLn('  Allocated voice: ', VoiceIdx);
  InterruptibleDelay(500);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing SetVoicePan (left)...');
  SetVoicePan(VoiceIdx, -1.0);
  InterruptibleDelay(500);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing SetVoicePan (right)...');
  SetVoicePan(VoiceIdx, 1.0);
  InterruptibleDelay(500);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing SetVoicePan (center)...');
  SetVoicePan(VoiceIdx, 0.0);
  InterruptibleDelay(500);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing SetVoiceFrequency (pitch bend up)...');
  SetVoiceFrequency(VoiceIdx, 523.25);  // C5
  InterruptibleDelay(500);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing SetVoiceFrequency (pitch bend down)...');
  SetVoiceFrequency(VoiceIdx, 329.63);  // E4
  InterruptibleDelay(500);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing SetVoiceAmplitude (fade)...');
  SetVoiceAmplitude(VoiceIdx, 0.3);
  InterruptibleDelay(300);
  SetVoiceAmplitude(VoiceIdx, 0.6);
  InterruptibleDelay(300);
  SetVoiceAmplitude(VoiceIdx, 0.9);
  InterruptibleDelay(300);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing RetriggerVoice (soft retrigger)...');
  RetriggerVoice(VoiceIdx);
  InterruptibleDelay(800);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing RetriggerVoiceHard (SID-style hard retrigger)...');
  RetriggerVoiceHard(VoiceIdx);
  InterruptibleDelay(800);
  if GEscapePressed then begin StopAll; Exit; end;

  WriteLn('Testing NoteOff...');
  NoteOff(VoiceIdx);
  InterruptibleDelay(500);

  StopAll;
  WriteLn;
  WriteLn('Advanced voice control test complete.');
end;

// ============================================================================
// SYSTEM STATUS
// ============================================================================

procedure ShowSystemStatus;
begin
  PrintHeader('System Status');

  PrintStatus;

  WaitForKey;
end;

// ============================================================================
// RUN ALL TESTS
// ============================================================================

procedure RunAllTests;
begin
  GEscapePressed := False;

  WriteLn('>>> ADDITIVE SYNTHESIS <<<');
  RunAdditiveAllTests;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  WriteLn('>>> SUBTRACTIVE SYNTHESIS <<<');
  TestBasicWaveforms;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  TestClassicPresets;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  WriteLn('>>> FM SYNTHESIS <<<');
  TestFMPresets;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  WriteLn('>>> WAVETABLE SYNTHESIS <<<');
  TestWavetablePresets;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  WriteLn('>>> MUSICAL DEMOS <<<');
  TestChords;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  TestScales;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  WriteLn('>>> SID CHIP EMULATION <<<');
  TestSIDChip;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  WriteLn('>>> MIDI & UTILITIES <<<');
  TestMIDIUtilities;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  TestUtilityFunctions;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  TestMIDIVoiceManagement;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  TestMIDINotePlayback;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  TestAdvancedVoiceControl;
  if GEscapePressed then begin WriteLn('Interrupted.'); Exit; end;

  WriteLn;
  WriteLn('All tests complete!');
  WaitForKey;
end;

// ============================================================================
// MAIN MENU
// ============================================================================

procedure ShowMainMenu;
var
  Choice: string;
begin
  repeat
    ClearScreen;

    WriteLn;
    WriteLn('===========================================');
    WriteLn('  Sedai Audio Foundation - Comprehensive Demo');
    WriteLn('===========================================');
    WriteLn;
    WriteLn('  SYNTHESIS ENGINES:');
    WriteLn('    1. Additive Synthesis');
    WriteLn('    2. Subtractive Synthesis (Basic Waveforms)');
    WriteLn('    3. Subtractive Presets (Classic Analog)');
    WriteLn('    4. FM Synthesis');
    WriteLn('    5. Wavetable Synthesis');
    WriteLn('    6. SID Chip Emulation');
    WriteLn;
    WriteLn('  MUSICAL DEMOS:');
    WriteLn('    7. Chord Tests');
    WriteLn('    8. Scale Tests');
    WriteLn;
    WriteLn('  MIDI & UTILITIES:');
    WriteLn('    A. MIDI Utilities Test');
    WriteLn('    B. Utility Functions (Beep, PlayNote, etc.)');
    WriteLn('    C. MIDI Voice Management');
    WriteLn('    D. MIDI Note Playback');
    WriteLn('    E. Advanced Voice Control');
    WriteLn;
    WriteLn('  SYSTEM:');
    WriteLn('    S. System Status');
    WriteLn('    R. Run All Tests');
    WriteLn('    0. Exit');
    WriteLn;
    Write('Choose option: ');

    ReadLn(Choice);
    GEscapePressed := False;

    case UpCase(Choice[1]) of
      '1': begin RunAdditiveAllTests; WaitForKey; end;
      '2': begin TestBasicWaveforms; WaitForKey; end;
      '3': begin TestClassicPresets; WaitForKey; end;
      '4': begin TestFMPresets; WaitForKey; end;
      '5': begin TestWavetablePresets; WaitForKey; end;
      '6': begin TestSIDChip; WaitForKey; end;
      '7': begin TestChords; WaitForKey; end;
      '8': begin TestScales; WaitForKey; end;
      'A': begin TestMIDIUtilities; WaitForKey; end;
      'B': begin TestUtilityFunctions; WaitForKey; end;
      'C': begin TestMIDIVoiceManagement; WaitForKey; end;
      'D': begin TestMIDINotePlayback; WaitForKey; end;
      'E': begin TestAdvancedVoiceControl; WaitForKey; end;
      'S': ShowSystemStatus;
      'R': RunAllTests;
      '0', 'Q': GTerminateRequested := True;
    end;

    StopAll;

  until GTerminateRequested;
end;

// ============================================================================
// MAIN PROGRAM
// ============================================================================

begin
  WriteLn;
  WriteLn('Sedai Audio Foundation - Audio Test');
  WriteLn('====================================');
  WriteLn;

  // Initialize console
  InitConsoleInput;

  // Initialize audio engine
  WriteLn('Initializing audio...');
  if not InitAudio then
  begin
    WriteLn('Error: Failed to initialize audio engine');
    RestoreConsoleInput;
    Halt(1);
  end;

  WriteLn('Audio initialized at ', GetSampleRate, ' Hz');
  WriteLn('Max voices: ', GetMaxVoices);
  WriteLn;

  try
    ShowMainMenu;
  finally
    WriteLn('Shutting down...');
    if Assigned(GAdditiveGen) then
      GAdditiveGen.Free;
    ShutdownAudio;
    RestoreConsoleInput;
  end;

  WriteLn('Done.');
end.
