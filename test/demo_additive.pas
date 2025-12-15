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

program DemoAdditive;

{$mode objfpc}{$H+}

uses
  SysUtils, Math,
  SedaiAudioTypes, SedaiAdditiveProcessor, SedaiADSRProcessor;

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn('════════════════════════════════════════════════════════');
  WriteLn('  ', ATitle);
  WriteLn('════════════════════════════════════════════════════════');
  WriteLn;
end;

procedure WaitForKey(const AMessage: string = 'Press ENTER to continue...');
begin
  WriteLn(AMessage);
  ReadLn;
end;

procedure PrintHarmonicSpectrum(const ASynth: TAdditiveSynthesis);
var
  i: Integer;
  MaxAmp: Single;
  BarLength: Integer;
begin
  WriteLn;
  WriteLn('Harmonic spectrum:');
  WriteLn('─────────────────────────────────────────────────────');

  MaxAmp := 0.0;
  for i := 0 to ASynth.HarmonicCount - 1 do
    if ASynth.Harmonics[i].Amplitude > MaxAmp then
      MaxAmp := ASynth.Harmonics[i].Amplitude;

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

  WriteLn('─────────────────────────────────────────────────────');
end;

procedure DemoBasicAdditive;
var
  Synth: TAdditiveSynthesis;
  i: Integer;
  Sample: Single;
  SampleRate: Cardinal;
  DeltaTime: Single;
begin
  PrintHeader('DEMO: Basic Additive Synthesis');

  SampleRate := 44100;
  DeltaTime := 1.0 / SampleRate;

  WriteLn('Creating a pure tone (fundamental only)...');
  TSedaiAdditiveProcessor.InitializeAdditive(Synth);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 1, 1.0);  // Fundamental only

  PrintHarmonicSpectrum(Synth);

  WriteLn;
  WriteLn('Generating 1000 samples @ 440Hz...');
  TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);

  for i := 1 to 1000 do
  begin
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 440.0, SampleRate, DeltaTime);
  end;

  WriteLn('✓ Pure tone generated');
  WriteLn;

  WriteLn('Creating a square wave (odd harmonics)...');
  TSedaiAdditiveProcessor.ClearHarmonics(Synth);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 1, 1.0);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 3, 0.333);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 5, 0.2);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 7, 0.143);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 9, 0.111);

  PrintHarmonicSpectrum(Synth);

  WriteLn;
  WriteLn('Generating square wave...');
  TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);

  for i := 1 to 1000 do
  begin
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 440.0, SampleRate, DeltaTime);
  end;

  WriteLn('✓ Square wave generated');
end;

procedure DemoPresets;
var
  Organ, Strings, Brass, Flute, Bell, Voice: TAdditiveSynthesis;
begin
  PrintHeader('DEMO: Additive Synthesis Presets');

  WriteLn('Testing 6 harmonic spectrum presets:');
  WriteLn;

  // Organ
  WriteLn('1. ORGAN SPECTRUM (Hammond drawbar-style)');
  Organ := TSedaiAdditiveProcessor.CreateOrganSpectrum;
  PrintHarmonicSpectrum(Organ);
  WaitForKey;

  // Strings
  WriteLn('2. STRINGS SPECTRUM (sawtooth-like, 1/n rolloff)');
  Strings := TSedaiAdditiveProcessor.CreateStringsSpectrum;
  PrintHarmonicSpectrum(Strings);
  WaitForKey;

  // Brass
  WriteLn('3. BRASS SPECTRUM (strong odd harmonics)');
  Brass := TSedaiAdditiveProcessor.CreateBrassSpectrum;
  PrintHarmonicSpectrum(Brass);
  WaitForKey;

  // Flute
  WriteLn('4. FLUTE SPECTRUM (mostly fundamental)');
  Flute := TSedaiAdditiveProcessor.CreateFluteSpectrum;
  PrintHarmonicSpectrum(Flute);
  WaitForKey;

  // Bell
  WriteLn('5. BELL SPECTRUM (inharmonic partials)');
  Bell := TSedaiAdditiveProcessor.CreateBellSpectrum;
  WriteLn('Inharmonic factor: ', Bell.InharmonicFactor:0:6);
  PrintHarmonicSpectrum(Bell);
  WaitForKey;

  // Voice
  WriteLn('6. VOICE SPECTRUM (formant-like)');
  Voice := TSedaiAdditiveProcessor.CreateVoiceSpectrum;
  PrintHarmonicSpectrum(Voice);
end;

procedure DemoInharmonicity;
var
  Synth: TAdditiveSynthesis;
  i: Integer;
  Sample: Single;
  SampleRate: Cardinal;
  DeltaTime: Single;
begin
  PrintHeader('DEMO: Inharmonicity');

  SampleRate := 44100;
  DeltaTime := 1.0 / SampleRate;

  WriteLn('Comparison between harmonic and inharmonic synthesis...');
  WriteLn;

  WriteLn('Test 1: HARMONIC spectrum (ideal bell)');
  TSedaiAdditiveProcessor.InitializeAdditive(Synth);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 1, 1.0);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 2, 0.6);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 3, 0.4);
  TSedaiAdditiveProcessor.SetHarmonic(Synth, 5, 0.3);
  Synth.InharmonicFactor := 0.0;  // Pure harmonic

  WriteLn('InharmonicFactor = ', Synth.InharmonicFactor:0:6);
  WriteLn('Harmonics are exact multiples of the fundamental');
  PrintHarmonicSpectrum(Synth);
  WriteLn;

  WriteLn('Generating 500 samples...');
  TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);
  for i := 1 to 500 do
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 200.0, SampleRate, DeltaTime);
  WriteLn('✓ Harmonic sound generated');
  WriteLn;

  WaitForKey('Press ENTER for inharmonic test...');

  WriteLn('Test 2: INHARMONIC spectrum (realistic bell)');
  Synth.InharmonicFactor := 0.0001;  // Slight inharmonicity

  WriteLn('InharmonicFactor = ', Synth.InharmonicFactor:0:6);
  WriteLn('Harmonics are slightly deviated (f = n * f0 * (1 + b*n²))');
  WriteLn('This creates the characteristic sound of real bells');
  WriteLn;

  WriteLn('Generating 500 samples...');
  TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);
  for i := 1 to 500 do
    Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 200.0, SampleRate, DeltaTime);
  WriteLn('✓ Inharmonic sound generated');
  WriteLn;

  WriteLn('Inharmonicity is crucial for realistic sounds of:');
  WriteLn('  • Bells');
  WriteLn('  • Cymbals and gongs');
  WriteLn('  • Metallic percussion instruments');
  WriteLn('  • Piano (stiff strings)');
end;

procedure DemoCustomSpectrum;
var
  Synth: TAdditiveSynthesis;
  i: Integer;
  HarmonicNum: Integer;
  Amplitude: Single;
  Sample: Single;
  SampleRate: Cardinal;
  DeltaTime: Single;
begin
  PrintHeader('DEMO: Custom Spectrum Creation');

  SampleRate := 44100;
  DeltaTime := 1.0 / SampleRate;

  WriteLn('Create your custom harmonic spectrum!');
  WriteLn;

  TSedaiAdditiveProcessor.InitializeAdditive(Synth);

  WriteLn('Add harmonics (0 to finish):');

  repeat
    Write('  Harmonic number (1-32, 0=done): ');
    ReadLn(HarmonicNum);

    if (HarmonicNum > 0) and (HarmonicNum <= 32) then
    begin
      Write('  Amplitude (0.0-1.0): ');
      ReadLn(Amplitude);

      if (Amplitude >= 0.0) and (Amplitude <= 1.0) then
      begin
        TSedaiAdditiveProcessor.SetHarmonic(Synth, HarmonicNum, Amplitude);
        WriteLn('  ✓ Harmonic ', HarmonicNum, ' added with amplitude ', Amplitude:0:2);
      end
      else
        WriteLn('  ✗ Invalid amplitude');
    end;

  until (HarmonicNum = 0) or (Synth.HarmonicCount >= 32);

  if Synth.HarmonicCount > 0 then
  begin
    WriteLn;
    WriteLn('Final spectrum:');
    PrintHarmonicSpectrum(Synth);

    WriteLn;
    WriteLn('Generating audio from custom spectrum...');
    TSedaiAdditiveProcessor.StartAdditiveAttack(Synth);

    for i := 1 to 2000 do
      Sample := TSedaiAdditiveProcessor.ProcessAdditive(Synth, 440.0, SampleRate, DeltaTime);

    WriteLn('✓ Sound generated with ', Synth.HarmonicCount, ' harmonics');
  end
  else
    WriteLn('No harmonics defined!');
end;

procedure DemoADSRWithAdditive;
var
  Synth: TAdditiveSynthesis;
  i: Integer;
  Sample: Single;
  SampleRate: Cardinal;
  DeltaTime: Single;
  MaxSample: Single;
begin
  PrintHeader('DEMO: ADSR with Additive Synthesis');

  SampleRate := 44100;
  DeltaTime := 1.0 / SampleRate;

  WriteLn('Testing ADSR envelope on additive synthesis...');
  WriteLn;

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

    // Show ADSR level every 0.1 seconds
    if (i mod Round(SampleRate * 0.1)) = 0 then
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

    if (i mod Round(SampleRate * 0.1)) = 0 then
    begin
      WriteLn(Format('t=%.1fs Level=%.3f %s [RELEASE]',
        [i / SampleRate, Synth.ADSR.Level, StringOfChar('#', Round(Synth.ADSR.Level * 40))]));
    end;
  end;

  WriteLn;
  WriteLn('✓ ADSR envelope completed');
  WriteLn('  Maximum level reached: ', MaxSample:0:3);
end;

procedure ShowMenu;
begin
  WriteLn;
  WriteLn('════════════════════════════════════════════════════════');
  WriteLn('  SEDAI Audio - Additive Synthesis Demo                 ');
  WriteLn('════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('1. Basic Demo (pure tones and waves)');
  WriteLn('2. Spectral Presets (6 presets)');
  WriteLn('3. Inharmonicity Demo');
  WriteLn('4. Create Custom Spectrum');
  WriteLn('5. ADSR Test with Additive');
  WriteLn('6. Full Demo');
  WriteLn('0. Exit');
  WriteLn;
  Write('Choice: ');
end;

var
  Choice: Char;

begin
  WriteLn('SEDAI Audio Foundation - Additive Synthesis Demo');
  WriteLn('Module: sedaiadditiveprocessor.pas');
  WriteLn;

  repeat
    ShowMenu;
    ReadLn(Choice);

    case Choice of
      '1': begin
        DemoBasicAdditive;
        WaitForKey;
      end;

      '2': begin
        DemoPresets;
        WaitForKey;
      end;

      '3': begin
        DemoInharmonicity;
        WaitForKey;
      end;

      '4': begin
        DemoCustomSpectrum;
        WaitForKey;
      end;

      '5': begin
        DemoADSRWithAdditive;
        WaitForKey;
      end;

      '6': begin
        DemoBasicAdditive;
        WaitForKey('Press ENTER for presets...');
        DemoPresets;
        WaitForKey('Press ENTER for inharmonicity...');
        DemoInharmonicity;
        WaitForKey('Press ENTER for ADSR test...');
        DemoADSRWithAdditive;
        WaitForKey;
      end;

      '0': begin
        WriteLn('Exiting...');
      end;

      else
        WriteLn('Invalid choice!');
    end;

  until Choice = '0';

  WriteLn('Demo completed. Goodbye!');
end.
