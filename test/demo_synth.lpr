{*
 * Sedai Audio Foundation - Synthesis Demo
 * Demonstrates the new modular architecture with real audio output
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
program demo_synth;

{$mode objfpc}{$H+}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Math, SDL2,
  SedaiAudioTypes,
  SedaiAudioEngine;

var
  GQuit: Boolean = False;

procedure ShowMenu;
begin
  WriteLn;
  WriteLn('=== Sedai Audio Foundation - Synthesis Demo ===');
  WriteLn;
  WriteLn('Basic Waveforms:');
  WriteLn('  1 - Sine Wave');
  WriteLn('  2 - Square Wave');
  WriteLn('  3 - Sawtooth Wave');
  WriteLn('  4 - Triangle Wave');
  WriteLn;
  WriteLn('Presets:');
  WriteLn('  5 - Strings');
  WriteLn('  6 - Brass');
  WriteLn('  7 - Lead');
  WriteLn('  8 - Bass');
  WriteLn('  9 - Pad');
  WriteLn;
  WriteLn('Chords:');
  WriteLn('  C - C Major Chord');
  WriteLn('  A - A Minor Chord');
  WriteLn;
  WriteLn('FM Synthesis:');
  WriteLn('  E - Electric Piano');
  WriteLn('  B - FM Bell');
  WriteLn;
  WriteLn('Controls:');
  WriteLn('  S - Stop All');
  WriteLn('  I - Show Status');
  WriteLn('  Q - Quit');
  WriteLn;
  Write('> ');
end;

procedure ProcessKey(AKey: Char);
begin
  case UpCase(AKey) of
    '1': begin
           WriteLn('Playing Sine Wave (440 Hz)');
           PlaySine(440.0);
         end;
    '2': begin
           WriteLn('Playing Square Wave (440 Hz)');
           PlaySquare(440.0);
         end;
    '3': begin
           WriteLn('Playing Sawtooth Wave (440 Hz)');
           PlaySaw(440.0);
         end;
    '4': begin
           WriteLn('Playing Triangle Wave (440 Hz)');
           PlayTriangle(440.0);
         end;
    '5': begin
           WriteLn('Playing Strings Preset');
           PlayClassic(NOTE_C4, 'strings');
         end;
    '6': begin
           WriteLn('Playing Brass Preset');
           PlayClassic(NOTE_C4, 'brass');
         end;
    '7': begin
           WriteLn('Playing Lead Preset');
           PlayLead(NOTE_A4);
         end;
    '8': begin
           WriteLn('Playing Bass Preset');
           PlayBass(NOTE_C4 / 2);  // One octave lower
         end;
    '9': begin
           WriteLn('Playing Pad Preset');
           PlayPad(NOTE_C4);
         end;
    'C': begin
           WriteLn('Playing C Major Chord');
           PlayChordClassic(NOTE_C4, NOTE_E4, NOTE_G4, 'strings');
         end;
    'A': begin
           WriteLn('Playing A Minor Chord');
           PlayChordClassic(NOTE_A4, NOTE_C5, NOTE_E4 * 2, 'pad');
         end;
    'E': begin
           WriteLn('Playing Electric Piano');
           PlayEPiano(NOTE_C4);
         end;
    'B': begin
           WriteLn('Playing FM Bell');
           PlayFMBell(NOTE_C5);
         end;
    'S': begin
           WriteLn('Stopping All Voices');
           SmoothStopAll(200);
         end;
    'I': begin
           PrintStatus;
         end;
    'Q': begin
           GQuit := True;
         end;
  end;
end;

var
  Input: string;

begin
  WriteLn('Sedai Audio Foundation - Synthesis Demo');
  WriteLn('========================================');
  WriteLn;

  // Initialize audio
  WriteLn('Initializing audio...');
  if not InitAudio(32) then
  begin
    WriteLn('ERROR: Failed to initialize audio!');
    Halt(1);
  end;

  SetMasterVolume(0.7);
  WriteLn('Audio initialized successfully!');
  WriteLn('Sample Rate: ', GetSampleRate, ' Hz');
  WriteLn;

  // Main loop
  while not GQuit do
  begin
    ShowMenu;
    ReadLn(Input);

    if Length(Input) > 0 then
      ProcessKey(Input[1]);
  end;

  // Cleanup
  WriteLn;
  WriteLn('Shutting down...');
  SmoothStopAll(200);
  ShutdownAudio;
  WriteLn('Goodbye!');
end.
