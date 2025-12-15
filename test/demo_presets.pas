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

program DemoPresets;

{$mode objfpc}{$H+}

uses
  SysUtils, SedaiAudioFoundation;

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn('========================================');
  WriteLn('  ', ATitle);
  WriteLn('========================================');
end;

procedure WaitForKey(const AMessage: string = 'Press ENTER to continue...');
begin
  WriteLn;
  Write(AMessage);
  ReadLn;
end;

procedure DemoClassicPresets;
begin
  PrintHeader('DEMO: Classic Synthesis Presets');
  WriteLn('Listening to the new classic synthesis presets...');
  WriteLn;

  WriteLn('1. Strings - C major chord');
  PlayClassic(261.63, 'strings');
  Sleep(800);
  PlayClassic(329.63, 'strings');
  Sleep(800);
  PlayClassic(392.00, 'strings');
  Sleep(1500);
  StopAll;
  Sleep(500);

  WriteLn('2. Brass - ascending scale');
  PlayClassic(261.63, 'brass');
  Sleep(400);
  PlayClassic(293.66, 'brass');
  Sleep(400);
  PlayClassic(329.63, 'brass');
  Sleep(400);
  PlayClassic(349.23, 'brass');
  Sleep(600);
  StopAll;
  Sleep(500);

  WriteLn('3. Organ - chord');
  PlayClassic(261.63, 'organ');
  PlayClassic(329.63, 'organ');
  PlayClassic(392.00, 'organ');
  Sleep(2000);
  StopAll;
  Sleep(500);

  WriteLn('4. Pluck - arpeggio');
  PlayClassic(261.63, 'pluck');
  Sleep(300);
  PlayClassic(329.63, 'pluck');
  Sleep(300);
  PlayClassic(392.00, 'pluck');
  Sleep(300);
  PlayClassic(523.25, 'pluck');
  Sleep(600);
  StopAll;
  Sleep(500);

  WriteLn('5. SynthKeys - melody');
  PlayClassic(392.00, 'synthkeys');
  Sleep(500);
  PlayClassic(440.00, 'synthkeys');
  Sleep(500);
  PlayClassic(493.88, 'synthkeys');
  Sleep(1000);
  StopAll;
  Sleep(500);

  WriteLn('6. WarmBass - bass line');
  PlayClassic(130.81, 'warmbass');
  Sleep(600);
  PlayClassic(146.83, 'warmbass');
  Sleep(600);
  PlayClassic(164.81, 'warmbass');
  Sleep(1200);
  StopAll;
end;

procedure DemoFMPresets;
begin
  PrintHeader('DEMO: FM Synthesis Presets');
  WriteLn('Listening to the new FM synthesis presets...');
  WriteLn;

  WriteLn('1. Choir - choral chord');
  PlayFM(261.63, 'choir');
  PlayFM(329.63, 'choir');
  PlayFM(392.00, 'choir');
  Sleep(2500);
  StopAll;
  Sleep(500);

  WriteLn('2. FM Strings - melodic line');
  PlayFM(392.00, 'fmstrings');
  Sleep(800);
  PlayFM(440.00, 'fmstrings');
  Sleep(800);
  PlayFM(493.88, 'fmstrings');
  Sleep(1500);
  StopAll;
  Sleep(500);

  WriteLn('3. FM Pad - evolving atmosphere');
  PlayFM(130.81, 'fmpad');
  PlayFM(164.81, 'fmpad');
  PlayFM(196.00, 'fmpad');
  Sleep(3000);
  StopAll;
  Sleep(500);

  WriteLn('4. Marimba - rhythmic pattern');
  PlayFM(523.25, 'marimba');
  Sleep(300);
  PlayFM(587.33, 'marimba');
  Sleep(300);
  PlayFM(659.25, 'marimba');
  Sleep(300);
  PlayFM(783.99, 'marimba');
  Sleep(600);
  StopAll;
  Sleep(500);

  WriteLn('5. Flute - soft melody');
  PlayFM(523.25, 'flute');
  Sleep(600);
  PlayFM(587.33, 'flute');
  Sleep(600);
  PlayFM(659.25, 'flute');
  Sleep(1200);
  StopAll;
  Sleep(500);

  WriteLn('6. Church Bell - chimes');
  PlayFM(261.63, 'churchbell');
  Sleep(1500);
  PlayFM(196.00, 'churchbell');
  Sleep(2000);
  StopAll;
end;

procedure DemoWavetablePresets;
begin
  PrintHeader('DEMO: Wavetable Synthesis Presets');
  WriteLn('Listening to the new wavetable presets...');
  WriteLn;

  WriteLn('1. Vocal - vocal formants');
  PlayWavetable(261.63, 'vocal');
  Sleep(800);
  PlayWavetable(293.66, 'vocal');
  Sleep(800);
  PlayWavetable(329.63, 'vocal');
  Sleep(1200);
  StopAll;
  Sleep(500);

  WriteLn('2. Metallic - metallic sounds');
  PlayWavetable(392.00, 'metallic');
  Sleep(1000);
  PlayWavetable(493.88, 'metallic');
  Sleep(1500);
  StopAll;
  Sleep(500);

  WriteLn('3. Glass - glass bells');
  PlayWavetable(523.25, 'glass');
  Sleep(1000);
  PlayWavetable(659.25, 'glass');
  Sleep(1500);
  StopAll;
  Sleep(500);

  WriteLn('4. Organ - Hammond-style organ');
  PlayWavetable(261.63, 'organ');
  PlayWavetable(329.63, 'organ');
  PlayWavetable(392.00, 'organ');
  Sleep(2000);
  StopAll;
  Sleep(500);

  WriteLn('5. Evolving - evolving pad');
  PlayWavetable(130.81, 'evolving');
  PlayWavetable(164.81, 'evolving');
  PlayWavetable(196.00, 'evolving');
  Sleep(3500);
  StopAll;
  Sleep(500);

  WriteLn('6. Digital Chaos - chaotic modulation');
  PlayWavetable(220.00, 'digitalchaos');
  Sleep(600);
  PlayWavetable(293.66, 'digitalchaos');
  Sleep(600);
  PlayWavetable(369.99, 'digitalchaos');
  Sleep(1000);
  StopAll;
end;

procedure DemoComparison;
begin
  PrintHeader('DEMO: Synthesis Engine Comparison');
  WriteLn('Same note (A4 440Hz) with different engines...');
  WriteLn;

  WriteLn('Classic Synth (Sawtooth):');
  PlaySaw(440.00);
  Sleep(1500);
  StopAll;
  Sleep(300);

  WriteLn('FM Synth (Electric Piano):');
  PlayEPiano(440.00);
  Sleep(1500);
  StopAll;
  Sleep(300);

  WriteLn('Wavetable (Serum):');
  PlaySerum(440.00);
  Sleep(1500);
  StopAll;
  Sleep(300);

  WriteLn('Wavetable (PPG):');
  PlayPPG(440.00);
  Sleep(1500);
  StopAll;
end;

procedure ShowMenu;
begin
  PrintHeader('SEDAI Audio Foundation - Preset Demo');
  WriteLn('This program demonstrates all the new presets added to the library.');
  WriteLn;
  WriteLn('1. Classic Synthesis Presets Demo (6 presets)');
  WriteLn('2. FM Synthesis Presets Demo (6 presets)');
  WriteLn('3. Wavetable Presets Demo (6 presets)');
  WriteLn('4. Synthesis Engines Comparison');
  WriteLn('5. Full Demo (all presets)');
  WriteLn('0. Exit');
  WriteLn;
  Write('Choice: ');
end;

var
  Choice: Char;

begin
  WriteLn('SEDAI Audio Foundation - Preset Demo');
  WriteLn('Initializing audio...');
  WriteLn;

  if not InitAudio(64) then
  begin
    WriteLn('ERROR: Unable to initialize audio!');
    Exit;
  end;

  SetMasterVolume(0.6);

  repeat
    ShowMenu;
    ReadLn(Choice);

    case Choice of
      '1': begin
        DemoClassicPresets;
        WaitForKey;
      end;

      '2': begin
        DemoFMPresets;
        WaitForKey;
      end;

      '3': begin
        DemoWavetablePresets;
        WaitForKey;
      end;

      '4': begin
        DemoComparison;
        WaitForKey;
      end;

      '5': begin
        DemoClassicPresets;
        WaitForKey('Press ENTER to continue with FM...');
        DemoFMPresets;
        WaitForKey('Press ENTER to continue with Wavetable...');
        DemoWavetablePresets;
        WaitForKey('Press ENTER for the final comparison...');
        DemoComparison;
        WaitForKey;
      end;

      '0': begin
        WriteLn('Exiting...');
      end;

      else
        WriteLn('Invalid choice!');
    end;

  until Choice = '0';

  WriteLn('Shutting down audio system...');
  ShutdownAudio;
  WriteLn('Demo completed. Goodbye!');
end.
