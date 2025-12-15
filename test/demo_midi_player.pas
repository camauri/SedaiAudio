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

program DemoMIDIPlayer;

{$mode objfpc}{$H+}

uses
  SysUtils, SedaiAudioFoundation, SedaiMIDIFoundation;

procedure PrintHeader;
begin
  WriteLn;
  WriteLn('╔════════════════════════════════════════════════════════════╗');
  WriteLn('║     SEDAI MIDI Player - Complete MIDI Player              ║');
  WriteLn('╚════════════════════════════════════════════════════════════╝');
  WriteLn;
end;

procedure PrintControls;
begin
  WriteLn;
  WriteLn('┌────────────────────────────────────────────────────────────┐');
  WriteLn('│ CONTROLS:                                                  │');
  WriteLn('│   P - Play/Pause          S - Stop                         │');
  WriteLn('│   + - Volume up           - - Volume down                  │');
  WriteLn('│   F - Faster              L - Slower                       │');
  WriteLn('│   I - File info           C - Channel info                 │');
  WriteLn('│   T - Playback status     M - Load another file            │');
  WriteLn('│   Q - Quit                                                 │');
  WriteLn('└────────────────────────────────────────────────────────────┘');
end;

procedure PrintStatus;
var
  State: string;
  Progress: Single;
  ActiveNotes: Integer;
  BPM: Integer;
begin
  State := GET_MIDI_STATE;
  Progress := GET_MIDI_PROGRESS;
  ActiveNotes := GET_MIDI_ACTIVE_NOTES;
  BPM := GET_MIDI_BPM;

  WriteLn;
  WriteLn('┌────────────── Playback Status ──────────────┐');
  WriteLn('│ State: ', State:20, '                    │');
  WriteLn('│ Progress: ', Progress:5:1, '%', '                            │');
  WriteLn('│ Active notes: ', ActiveNotes:3, '                             │');
  WriteLn('│ Tempo: ', BPM:3, ' BPM', '                               │');
  WriteLn('│ Tick: ', GET_MIDI_CURRENT_TICK:6, ' / ', GET_MIDI_TOTAL_TICKS:6, '              │');
  WriteLn('└────────────────────────────────────────────┘');
end;

procedure LoadMIDIFile;
var
  Filename: string;
begin
  WriteLn;
  WriteLn('Enter the MIDI file path:');
  Write('> ');
  ReadLn(Filename);

  if Filename = '' then
  begin
    WriteLn('No file specified.');
    Exit;
  end;

  if not FileExists(Filename) then
  begin
    WriteLn('ERROR: File not found: ', Filename);
    Exit;
  end;

  WriteLn;
  WriteLn('Loading MIDI file...');

  if LOAD_MIDI_FILE(Filename) then
  begin
    WriteLn('✓ File loaded successfully!');
    WriteLn;
    PRINT_MIDI_FILE_DETAILS;
    WriteLn;
    WriteLn('Configuring MIDI channels...');
    SETUP_MIDI_GENERAL_MIDI;
    WriteLn('✓ Ready for playback!');
  end
  else
  begin
    WriteLn('✗ Error loading MIDI file.');
  end;
end;

procedure InteractivePlayer;
var
  Command: Char;
  Volume: Single;
  Tempo: Single;
  Running: Boolean;
begin
  PrintControls;
  Running := True;
  Volume := 0.7;
  Tempo := 1.0;

  while Running do
  begin
    WriteLn;
    Write('Command (? for help): ');
    ReadLn(Command);
    Command := UpCase(Command);

    case Command of
      'P': begin
        if IS_MIDI_PLAYING then
        begin
          MIDI_PAUSE;
          WriteLn('⏸ Paused');
        end
        else if IS_MIDI_PAUSED then
        begin
          MIDI_PLAY;
          WriteLn('▶ Playback resumed');
        end
        else
        begin
          MIDI_PLAY;
          WriteLn('▶ Playback started');
        end;
      end;

      'S': begin
        MIDI_STOP;
        WriteLn('⏹ Stop');
      end;

      '+': begin
        Volume := Volume + 0.1;
        if Volume > 1.0 then Volume := 1.0;
        SetMasterVolume(Volume);
        WriteLn('🔊 Volume: ', (Volume * 100):3:0, '%');
      end;

      '-': begin
        Volume := Volume - 0.1;
        if Volume < 0.0 then Volume := 0.0;
        SetMasterVolume(Volume);
        WriteLn('🔉 Volume: ', (Volume * 100):3:0, '%');
      end;

      'F': begin
        Tempo := Tempo * 1.1;
        if Tempo > 2.0 then Tempo := 2.0;
        SET_MIDI_TEMPO(Tempo);
        WriteLn('⏩ Tempo: ', (Tempo * 100):3:0, '%');
      end;

      'L': begin
        Tempo := Tempo / 1.1;
        if Tempo < 0.5 then Tempo := 0.5;
        SET_MIDI_TEMPO(Tempo);
        WriteLn('⏪ Tempo: ', (Tempo * 100):3:0, '%');
      end;

      'I': begin
        PRINT_MIDI_FILE_DETAILS;
      end;

      'C': begin
        PRINT_MIDI_CHANNELS;
      end;

      'T': begin
        PrintStatus;
      end;

      'M': begin
        MIDI_STOP;
        LoadMIDIFile;
      end;

      '?': begin
        PrintControls;
      end;

      'Q': begin
        WriteLn('Exiting player...');
        Running := False;
      end;

      else
        WriteLn('Unknown command. Press ? for help.');
    end;
  end;
end;

procedure AutoPlayDemo;
var
  i: Integer;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════');
  WriteLn('  Automatic Demo - MIDI Playback       ');
  WriteLn('═══════════════════════════════════════');
  WriteLn;
  WriteLn('Starting playback...');

  MIDI_PLAY;

  WriteLn('Playback in progress. Showing status every 2 seconds...');
  WriteLn('(Demo will stop automatically after 30 seconds)');
  WriteLn;

  for i := 1 to 15 do
  begin
    Sleep(2000);
    PrintStatus;

    if IS_MIDI_STOPPED then
    begin
      WriteLn;
      WriteLn('✓ Playback completed!');
      Break;
    end;
  end;

  if IS_MIDI_PLAYING then
  begin
    WriteLn;
    WriteLn('Demo finished - stopping playback.');
    MIDI_STOP;
  end;
end;

procedure ShowMainMenu;
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════');
  WriteLn('  Main Menu                            ');
  WriteLn('═══════════════════════════════════════');
  WriteLn;
  WriteLn('1. Load MIDI file and use interactive player');
  WriteLn('2. Automatic demo (30 seconds)');
  WriteLn('3. MIDI system info');
  WriteLn('0. Exit');
  WriteLn;
  Write('Choice: ');
end;

var
  Choice: Char;
  DefaultFile: string;

begin
  PrintHeader;

  WriteLn('Initializing audio system...');
  if not InitAudio(128) then
  begin
    WriteLn('ERROR: Unable to initialize audio!');
    Exit;
  end;

  WriteLn('Initializing MIDI system...');
  if not INIT_MIDI then
  begin
    WriteLn('ERROR: Unable to initialize MIDI!');
    ShutdownAudio;
    Exit;
  end;

  SetMasterVolume(0.7);

  WriteLn;
  WriteLn('✓ System ready!');
  WriteLn;
  WriteLn('NOTE: To test the player, you will need a MIDI file.');
  WriteLn('      You can download MIDI files from:');
  WriteLn('      - https://freemidi.org');
  WriteLn('      - https://www.midiworld.com');
  WriteLn('      or use any .mid file you have.');

  repeat
    ShowMainMenu;
    ReadLn(Choice);

    case Choice of
      '1': begin
        LoadMIDIFile;
        WriteLn;
        Write('Do you want to start the interactive player? (Y/N): ');
        ReadLn(Choice);
        if UpCase(Choice) = 'Y' then
          InteractivePlayer;
      end;

      '2': begin
        LoadMIDIFile;
        AutoPlayDemo;
        WriteLn;
        WriteLn('Press ENTER to continue...');
        ReadLn;
      end;

      '3': begin
        PRINT_MIDI_STATUS;
        WriteLn;
        WriteLn('Press ENTER to continue...');
        ReadLn;
      end;

      '0': begin
        WriteLn('Exiting...');
      end;

      else
        WriteLn('Invalid choice!');
    end;

  until Choice = '0';

  WriteLn;
  WriteLn('Shutting down systems...');
  SHUTDOWN_MIDI;
  ShutdownAudio;
  WriteLn('✓ MIDI Player closed. Goodbye!');
end.
