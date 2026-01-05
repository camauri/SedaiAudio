{*
 * Sedai Audio Foundation - Test Program
 *
 * Tests all synthesis modes: Classic, FM, and Wavetable.
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
program TestSAFMain;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Classes, SysUtils, SedaiAudioFoundation;

procedure TestClassicSynthesis;
begin
  WriteLn('=== Testing Classic Synthesis ===');

  // Test different waveforms
  WriteLn('Playing sine wave (A4 = 440 Hz)...');
  PlaySine(NoteA4, 0.4);
  Sleep(500);

  WriteLn('Playing square wave (C5 = 523 Hz)...');
  PlaySquare(NoteC5, 0.3);
  Sleep(500);

  WriteLn('Playing sawtooth wave (E4)...');
  PlaySaw(NoteE4, 0.3);
  Sleep(500);

  WriteLn('Playing triangle wave (G4)...');
  PlayTriangle(NoteG4, 0.3);
  Sleep(500);

  // Stop all voices
  SmoothStopAll(200);
  Sleep(300);

  WriteLn('Classic synthesis test completed.');
  WriteLn('');
end;

procedure TestFMSynthesis;
begin
  WriteLn('=== Testing FM Synthesis ===');

  // Test E-Piano
  WriteLn('Playing FM E-Piano...');
  PlayEPiano(NoteC4);
  Sleep(2000);
  SmoothStopAll(300);
  Sleep(500);

  WriteLn('Playing FM Brass...');
  PlayFMBrass(NoteG3);
  Sleep(2000);
  SmoothStopAll(300);
  Sleep(500);

  WriteLn('Playing FM Bell...');
  PlayFMBell(NoteA4);
  Sleep(2500);
  SmoothStopAll(300);
  Sleep(500);

  WriteLn('Playing FM Organ...');
  PlayFMOrgan(NoteC4);
  Sleep(2000);
  SmoothStopAll(300);
  Sleep(500);

  WriteLn('FM synthesis test completed.');
  WriteLn('');
end;

procedure TestWavetableSynthesis;
begin
  WriteLn('=== Testing Wavetable Synthesis ===');

  // Test basic wavetable
  WriteLn('Playing basic wavetable...');
  PlayWavetable(NoteC4, 'basic');
  Sleep(800);

  // Test SuperSaw
  WriteLn('Playing SuperSaw wavetable...');
  PlaySuperSaw(NoteA3);
  Sleep(800);

  // Test PWM
  WriteLn('Playing PWM wavetable...');
  PlayPWM(NoteE4);
  Sleep(800);

  // Stop all voices
  SmoothStopAll(300);
  Sleep(400);

  WriteLn('Wavetable synthesis test completed.');
  WriteLn('');
end;

procedure TestChords;
begin
  WriteLn('=== Testing Chord Playback ===');

  // C major chord with FM E-Piano
  WriteLn('Playing C major chord (FM E-Piano)...');
  PlayChordFM(NoteC4, NoteE4, NoteG4, 'epiano');
  Sleep(1500);

  SmoothStopAll(500);
  Sleep(600);

  // A minor chord with classic triangle
  WriteLn('Playing A minor chord (Classic Triangle)...');
  PlayChordClassic(NoteA3, NoteC4, NoteE4, 'triangle');
  Sleep(1500);

  SmoothStopAll(500);
  Sleep(600);

  WriteLn('Chord test completed.');
  WriteLn('');
end;

procedure TestMIDIUtilities;
var
  MIDINote: Byte;
  Freq: Single;
begin
  WriteLn('=== Testing MIDI Utilities ===');

  // MIDI note to frequency
  WriteLn('MIDI Note 69 (A4) = ', MIDINoteToFreq(69):0:2, ' Hz (expected: 440.00)');
  WriteLn('MIDI Note 60 (C4) = ', MIDINoteToFreq(60):0:2, ' Hz (expected: 261.63)');
  WriteLn('MIDI Note 72 (C5) = ', MIDINoteToFreq(72):0:2, ' Hz (expected: 523.25)');

  // Frequency to MIDI note
  WriteLn('Frequency 440 Hz = MIDI Note ', FreqToMIDINote(440.0), ' (expected: 69)');
  WriteLn('Frequency 261.63 Hz = MIDI Note ', FreqToMIDINote(261.63), ' (expected: 60)');

  // Velocity to amplitude
  WriteLn('Velocity 127 = Amplitude ', MIDIVelocityToAmplitude(127):0:3, ' (expected: 1.000)');
  WriteLn('Velocity 64 = Amplitude ', MIDIVelocityToAmplitude(64):0:3, ' (expected: ~0.71)');

  WriteLn('MIDI utilities test completed.');
  WriteLn('');
end;

var
  UserInput: string;

begin
  WriteLn('========================================');
  WriteLn('  Sedai Audio Foundation - Test Suite  ');
  WriteLn('========================================');
  WriteLn('');

  // Initialize audio
  if not InitAudio(32) then
  begin
    WriteLn('ERROR: Failed to initialize audio!');
    Exit;
  end;

  WriteLn('');
  SetMasterVolume(0.5);

  // Run tests
  TestMIDIUtilities;

  WriteLn('Press ENTER to start audio tests...');
  ReadLn(UserInput);

  TestClassicSynthesis;

  WriteLn('Press ENTER to test FM synthesis...');
  ReadLn(UserInput);

  TestFMSynthesis;

  WriteLn('Press ENTER to test Wavetable synthesis...');
  ReadLn(UserInput);

  TestWavetableSynthesis;

  WriteLn('Press ENTER to test chords...');
  ReadLn(UserInput);

  TestChords;

  // Final status
  WriteLn('');
  PrintStatus;

  WriteLn('');
  WriteLn('Press ENTER to exit...');
  ReadLn(UserInput);

  // Shutdown
  ShutdownAudio;

  WriteLn('Test suite completed.');
end.
