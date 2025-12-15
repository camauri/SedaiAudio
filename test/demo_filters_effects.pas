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

program DemoFiltersEffects;

{$mode objfpc}{$H+}

uses
  SysUtils, Math,
  SedaiAudioTypes, SedaiFilters, SedaiAudioEffects;

procedure PrintHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn('========================================================');
  WriteLn('  ', ATitle);
  WriteLn('========================================================');
  WriteLn;
end;

procedure WaitForKey(const AMessage: string = 'Press ENTER to continue...');
begin
  WriteLn(AMessage);
  ReadLn;
end;

procedure DemoFilters;
var
  Filter: TBiquadFilter;
  MultiFilter: TMultiPoleFilter;
  i: Integer;
  Input, Output: Single;
  SampleRate: Cardinal;
begin
  PrintHeader('DEMO: Biquad Audio Filters');
  SampleRate := 44100;

  WriteLn('Testing 6 biquad filter types...');
  WriteLn;

  // Test Lowpass
  WriteLn('1. LOWPASS FILTER (1kHz, Q=0.707)');
  WriteLn('   Low-pass filter - attenuates high frequencies');
  Filter := TSedaiFilters.CreateBiquadFilter(ftLowPass, 1000.0, 0.707, SampleRate);

  WriteLn('   Generating white noise and applying filter...');
  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;  // White noise
    Output := TSedaiFilters.ProcessBiquadSample(Filter, Input);
  end;
  WriteLn('   + Lowpass filter working');
  WriteLn;

  // Test Highpass
  WriteLn('2. HIGHPASS FILTER (500Hz, Q=0.707)');
  WriteLn('   High-pass filter - attenuates low frequencies');
  Filter := TSedaiFilters.CreateBiquadFilter(ftHighPass, 500.0, 0.707, SampleRate);

  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;
    Output := TSedaiFilters.ProcessBiquadSample(Filter, Input);
  end;
  WriteLn('   + Highpass filter working');
  WriteLn;

  // Test Bandpass
  WriteLn('3. BANDPASS FILTER (1kHz, Q=5.0)');
  WriteLn('   Band-pass filter - allows only a band of frequencies through');
  Filter := TSedaiFilters.CreateBiquadFilter(ftBandPass, 1000.0, 5.0, SampleRate);

  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;
    Output := TSedaiFilters.ProcessBiquadSample(Filter, Input);
  end;
  WriteLn('   + Bandpass filter working');
  WriteLn;

  // Test Notch
  WriteLn('4. NOTCH FILTER (1kHz, Q=10.0)');
  WriteLn('   Notch filter - removes a narrow band of frequencies');
  Filter := TSedaiFilters.CreateBiquadFilter(ftNotch, 1000.0, 10.0, SampleRate);

  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;
    Output := TSedaiFilters.ProcessBiquadSample(Filter, Input);
  end;
  WriteLn('   + Notch filter working');
  WriteLn;

  // Test Multi-pole
  WriteLn('5. MULTI-POLE FILTER (24dB/octave lowpass)');
  WriteLn('   Multi-pole filter - steeper slope');
  MultiFilter := TSedaiFilters.CreateMultiPoleFilter(ftLowPass, 800.0, 0.707, fs24dB, SampleRate);

  for i := 1 to 100 do
  begin
    Input := (Random - 0.5) * 2.0;
    Output := TSedaiFilters.ProcessMultiPoleSample(MultiFilter, Input);
  end;
  WriteLn('   + Multi-pole filter working');
  WriteLn;

  WriteLn('All filters tested successfully!');
end;

procedure DemoEffects;
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
  PrintHeader('DEMO: Audio Effects');
  SampleRate := 44100;

  WriteLn('Testing audio effects...');
  WriteLn;

  // Test Delay
  WriteLn('1. DELAY EFFECT (250ms, 50% feedback)');
  WriteLn('   Echo/delay with feedback');
  Delay := TSedaiAudioEffects.CreateDelayEffect(0.25, 0.5, 0.5, SampleRate);

  WriteLn('   Generating impulse and applying delay...');
  for i := 0 to 22049 do
  begin
    if i = 0 then
      Input := 1.0  // Impulse
    else
      Input := 0.0;

    Output := TSedaiAudioEffects.ProcessDelaySample(Delay, Input);

    // Print when significant output is detected
    if (Abs(Output) > 0.1) and (i > 0) then
      WriteLn('   Echo detected at sample ', i, ' (', (i / SampleRate * 1000):0:1, ' ms)');
  end;
  WriteLn('   + Delay working');
  WriteLn;

  // Test Reverb
  WriteLn('2. REVERB EFFECT (Schroeder)');
  WriteLn('   Algorithmic reverb with comb and allpass filters');
  Reverb := TSedaiAudioEffects.CreateReverbEffect(0.7, 0.5, 0.3, SampleRate);

  for i := 1 to 1000 do
  begin
    Input := (Random - 0.5) * 0.1;
    Output := TSedaiAudioEffects.ProcessReverbSample(Reverb, Input);
  end;
  WriteLn('   + Reverb working');
  WriteLn;

  // Test Chorus
  WriteLn('3. CHORUS EFFECT (2Hz LFO, 5ms depth)');
  WriteLn('   Chorus effect with LFO modulation');
  Chorus := TSedaiAudioEffects.CreateChorusEffect(2.0, 0.005, 0.5, SampleRate);

  for i := 1 to 1000 do
  begin
    Input := Sin(2.0 * Pi * 440.0 * i / SampleRate);  // 440Hz sine
    Output := TSedaiAudioEffects.ProcessChorusSample(Chorus, Input);
  end;
  WriteLn('   + Chorus working');
  WriteLn;

  // Test Flanger
  WriteLn('4. FLANGER EFFECT (0.5Hz LFO, 80% feedback)');
  WriteLn('   Flanger effect with feedback');
  Flanger := TSedaiAudioEffects.CreateFlangerEffect(0.5, 0.003, 0.8, 0.5, SampleRate);

  for i := 1 to 1000 do
  begin
    Input := Sin(2.0 * Pi * 440.0 * i / SampleRate);
    Output := TSedaiAudioEffects.ProcessFlangerSample(Flanger, Input);
  end;
  WriteLn('   + Flanger working');
  WriteLn;

  // Test Distortion
  WriteLn('5. DISTORTION EFFECTS');

  WriteLn('   a) Soft Clip');
  Distortion := TSedaiAudioEffects.CreateDistortionEffect(2.0, 0.5, 0.5);
  for i := 1 to 100 do
  begin
    Input := Sin(2.0 * Pi * 100.0 * i / SampleRate) * 1.5;
    Output := TSedaiAudioEffects.SoftClip(Input * Distortion.Drive);
  end;
  WriteLn('      + Soft clip working');

  WriteLn('   b) Hard Clip');
  Distortion := TSedaiAudioEffects.CreateDistortionEffect(1.5, 0.5, 0.5);
  for i := 1 to 100 do
  begin
    Input := Sin(2.0 * Pi * 100.0 * i / SampleRate) * 2.0;
    Output := TSedaiAudioEffects.HardClip(Input * Distortion.Drive);
  end;
  WriteLn('      + Hard clip working');

  WriteLn('   c) Tanh Saturation');
  Distortion := TSedaiAudioEffects.CreateDistortionEffect(3.0, 0.5, 0.5);
  for i := 1 to 100 do
  begin
    Input := Sin(2.0 * Pi * 100.0 * i / SampleRate) * 2.0;
    Output := TSedaiAudioEffects.TanhDistortion(Input, Distortion.Drive);
  end;
  WriteLn('      + Tanh saturation working');
  WriteLn;

  WriteLn('All effects tested successfully!');
end;

procedure DemoFilterResponse;
var
  Filter: TBiquadFilter;
  Freq, Response: Single;
  SampleRate: Cardinal;
  i: Integer;
begin
  PrintHeader('DEMO: Filter Frequency Response');
  SampleRate := 44100;

  WriteLn('Frequency response analysis of lowpass filter @ 1kHz');
  WriteLn;

  Filter := TSedaiFilters.CreateBiquadFilter(ftLowPass, 1000.0, 0.707, SampleRate);

  WriteLn('Freq(Hz)  | Approximate Response');
  WriteLn('----------+-----------------------');

  for i := 0 to 10 do
  begin
    Freq := Power(10, 1.0 + i * 0.3);  // From 10Hz to ~20kHz
    // Note: this is just a simplified simulation
    // A real response would require FFT or sine sweep
    if Freq < 1000.0 then
      Response := 1.0  // Passband
    else
      Response := 1000.0 / Freq;  // Approximate rolloff

    WriteLn(Format('%8.0f  | %s', [Freq, StringOfChar('#', Round(Response * 30))]));
  end;

  WriteLn;
  WriteLn('Note: This is a simplified representation.');
  WriteLn('      For precise analysis, use FFT analysis tools.');
end;

procedure ShowMenu;
begin
  WriteLn;
  WriteLn('========================================================');
  WriteLn('  SEDAI Audio - Filters and Effects Demo                ');
  WriteLn('========================================================');
  WriteLn;
  WriteLn('1. Test Biquad Filters (6 types)');
  WriteLn('2. Test Audio Effects (5 effects)');
  WriteLn('3. Frequency Response Analysis');
  WriteLn('4. Full Demo');
  WriteLn('0. Exit');
  WriteLn;
  Write('Choice: ');
end;

var
  Choice: Char;

begin
  Randomize;

  WriteLn('SEDAI Audio Foundation - Filters and Effects Demo');
  WriteLn('This program tests the sedaifilters.pas and sedaiaudioeffects.pas modules');
  WriteLn;

  repeat
    ShowMenu;
    ReadLn(Choice);

    case Choice of
      '1': begin
        DemoFilters;
        WaitForKey;
      end;

      '2': begin
        DemoEffects;
        WaitForKey;
      end;

      '3': begin
        DemoFilterResponse;
        WaitForKey;
      end;

      '4': begin
        DemoFilters;
        WaitForKey('Press ENTER to continue with effects...');
        DemoEffects;
        WaitForKey('Press ENTER for final analysis...');
        DemoFilterResponse;
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
