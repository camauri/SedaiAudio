{*
 * Sedai Audio Foundation - SID Test Patterns
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * Comprehensive test patterns for SID emulation validation.
 * Tests waveforms, envelopes, filters, modulations and musical effects.
 *}

unit SedaiSIDTestPatterns;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, SDL2, SedaiSIDEvo;

type
  // Callback for checking user abort (ESC/CTRL+C)
  TAbortCheckFunc = function: Boolean;

var
  // Global abort check function - set by main program
  GAbortCheck: TAbortCheckFunc = nil;

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

// Interruptible delay - returns True if aborted
function WaitMs(AMs: Integer): Boolean;

// Print test header
procedure PrintTestHeader(const ATitle: string);

// Print test parameters
procedure PrintParams(const AParams: string);

// ============================================================================
// 1. WAVEFORM TESTS (Static)
// ============================================================================

procedure TestTriangle(AFreqHz: Single; ADurationMs: Integer = 1000);
procedure TestSaw(AFreqHz: Single; ADurationMs: Integer = 1000);
procedure TestPulse(AFreqHz: Single; APulseWidthReg: Word; ADurationMs: Integer = 1000);
procedure TestNoise(AFreqHz: Single; ADurationMs: Integer = 1000);

// Run all static waveform tests
procedure RunWaveformTests;

// ============================================================================
// 2. DYNAMIC SWEEP TESTS
// ============================================================================

procedure TestPWMSweep(AFromPW, AToPW: Word; ADurationMs: Integer = 1000);
procedure TestFreqSweep(AFromHz, AToHz: Single; AWaveform: TSIDEvoWaveform; ADurationMs: Integer = 1000);
procedure TestNoiseSweep(AFromHz, AToHz: Single; ADurationMs: Integer = 500);

// Run all sweep tests
procedure RunSweepTests;

// ============================================================================
// 3. ADSR ENVELOPE TESTS
// ============================================================================

// Test individual ADSR parameters (value 0-15 as in SID)
procedure TestAttack(AAttackValue: Integer; ADurationMs: Integer = 2000);
procedure TestDecay(ADecayValue: Integer; ADurationMs: Integer = 2000);
procedure TestRelease(AReleaseValue: Integer);

// Run all ADSR tests
procedure RunADSRTests;

// ============================================================================
// 4. FILTER TESTS
// ============================================================================

procedure TestLowpassSweep(AResonance: Integer; ADurationMs: Integer = 2000);
procedure TestHighpassSweep(AResonance: Integer; ADurationMs: Integer = 2000);
procedure TestBandpassSweep(AResonance: Integer; ADurationMs: Integer = 2000);
procedure TestNotchSweep(AResonance: Integer; ADurationMs: Integer = 2000);

// Run all filter tests
procedure RunFilterTests;

// ============================================================================
// 5. MODULATION TESTS
// ============================================================================

procedure TestSync;
procedure TestRingMod;
procedure TestSyncSweep;

// Run all modulation tests
procedure RunModulationTests;

// ============================================================================
// 6. MUSICAL EFFECT TESTS
// ============================================================================

procedure TestArpeggio;           // Classic C-E-G arpeggio
procedure TestArpeggioMinor;      // C-Eb-G minor arpeggio
procedure TestTrill;              // Fast alternating notes
procedure TestVibrato;            // Pitch modulation
procedure TestPWMVibrato;         // Pulse width modulation
procedure TestNoiseDrum;          // Percussion hit
procedure TestKickSynth;          // Synthesized kick drum
procedure TestSnareSynth;         // Synthesized snare

// Run all effect tests
procedure RunEffectTests;

// ============================================================================
// 7. COMPLEX PATTERN TESTS
// ============================================================================

procedure TestFastArpeggio;       // 50 Hz frame rate arpeggio
procedure TestSlowArpeggio;       // 100ms per note
procedure TestCombinedEffects;    // PWM + vibrato + filter

// Run all pattern tests
procedure RunPatternTests;

// ============================================================================
// FULL TEST SUITE
// ============================================================================

procedure RunFullTestSuite;

implementation

// ============================================================================
// UTILITY IMPLEMENTATION
// ============================================================================

function WaitMs(AMs: Integer): Boolean;
var
  AStartTime, AElapsed: UInt32;
begin
  Result := False;
  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= Cardinal(AMs) then
      Break;

    // Check for abort
    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      Result := True;
      Break;
    end;

    SDL_Delay(1);
  end;
end;

procedure PrintTestHeader(const ATitle: string);
begin
  WriteLn;
  WriteLn('=== ', ATitle, ' ===');
end;

procedure PrintParams(const AParams: string);
begin
  WriteLn('    ', AParams);
end;

// ============================================================================
// 1. WAVEFORM TESTS
// ============================================================================

procedure TestTriangle(AFreqHz: Single; ADurationMs: Integer);
begin
  PrintTestHeader('TRIANGLE');
  PrintParams(Format('Freq: %.1f Hz, Duration: %d ms', [AFreqHz, ADurationMs]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvo.SetFrequencyHz(0, AFreqHz);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  SIDEvo.GateOn(0);

  if not WaitMs(ADurationMs) then
  begin
    SIDEvo.GateOff(0);
    WaitMs(300);
  end
  else
    SIDEvo.StopVoice(0);
end;

procedure TestSaw(AFreqHz: Single; ADurationMs: Integer);
begin
  PrintTestHeader('SAWTOOTH');
  PrintParams(Format('Freq: %.1f Hz, Duration: %d ms', [AFreqHz, ADurationMs]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, AFreqHz);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  SIDEvo.GateOn(0);

  if not WaitMs(ADurationMs) then
  begin
    SIDEvo.GateOff(0);
    WaitMs(300);
  end
  else
    SIDEvo.StopVoice(0);
end;

procedure TestPulse(AFreqHz: Single; APulseWidthReg: Word; ADurationMs: Integer);
var
  APW: Single;
begin
  PrintTestHeader('PULSE');
  APW := APulseWidthReg / $1000;  // Convert SID register to 0-1 range
  PrintParams(Format('Freq: %.1f Hz, PW: $%04X (%.1f%%), Duration: %d ms',
    [AFreqHz, APulseWidthReg, APW * 100, ADurationMs]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_PULSE);
  SIDEvo.SetFrequencyHz(0, AFreqHz);
  SIDEvo.SetPulseWidth(0, APW);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  SIDEvo.GateOn(0);

  if not WaitMs(ADurationMs) then
  begin
    SIDEvo.GateOff(0);
    WaitMs(300);
  end
  else
    SIDEvo.StopVoice(0);
end;

procedure TestNoise(AFreqHz: Single; ADurationMs: Integer);
begin
  PrintTestHeader('NOISE');
  PrintParams(Format('Freq: %.1f Hz, Duration: %d ms', [AFreqHz, ADurationMs]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_NOISE);
  SIDEvo.SetFrequencyHz(0, AFreqHz);
  SIDEvo.SetADSR(0, 0, 2/15, 15/15, 2/15);
  SIDEvo.GateOn(0);

  if not WaitMs(ADurationMs) then
  begin
    SIDEvo.GateOff(0);
    WaitMs(300);
  end
  else
    SIDEvo.StopVoice(0);
end;

procedure RunWaveformTests;
const
  Freqs: array[0..3] of Single = (100, 440, 1000, 4000);
  // SID pulse width: 12-bit value (0-$FFF), $800 = 50%
  PulseWidths: array[0..3] of Word = ($0200, $0800, $0C00, $0F00);  // 12.5%, 50%, 75%, ~94%
var
  i: Integer;
begin
  PrintTestHeader('WAVEFORM TESTS - TRIANGLE');
  for i := 0 to High(Freqs) do
  begin
    TestTriangle(Freqs[i], 800);
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;

  PrintTestHeader('WAVEFORM TESTS - SAWTOOTH');
  for i := 0 to High(Freqs) do
  begin
    TestSaw(Freqs[i], 800);
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;

  PrintTestHeader('WAVEFORM TESTS - PULSE (Various Duty Cycles)');
  for i := 0 to High(PulseWidths) do
  begin
    TestPulse(440, PulseWidths[i], 800);
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;

  PrintTestHeader('WAVEFORM TESTS - NOISE');
  TestNoise(50, 500);
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  TestNoise(200, 500);
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  TestNoise(1000, 500);
end;

// ============================================================================
// 2. SWEEP TESTS
// ============================================================================

procedure TestPWMSweep(AFromPW, AToPW: Word; ADurationMs: Integer);
var
  AStartTime, AElapsed: UInt32;
  AProgress: Single;
  ACurrentPW: Single;
begin
  PrintTestHeader('PULSE WIDTH SWEEP');
  PrintParams(Format('PW: $%04X -> $%04X, Duration: %d ms', [AFromPW, AToPW, ADurationMs]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_PULSE);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  SIDEvo.SetPulseWidth(0, AFromPW / $1000);
  SIDEvo.GateOn(0);

  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= Cardinal(ADurationMs) then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.StopVoice(0);
      Exit;
    end;

    AProgress := AElapsed / ADurationMs;
    ACurrentPW := (AFromPW + (AToPW - AFromPW) * AProgress) / $1000;
    SIDEvo.SetPulseWidth(0, ACurrentPW);
    SIDEvo.UpdateActiveVoice(0);

    SDL_Delay(20);  // ~50 Hz update rate
  end;

  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestFreqSweep(AFromHz, AToHz: Single; AWaveform: TSIDEvoWaveform; ADurationMs: Integer);
var
  AStartTime, AElapsed: UInt32;
  AProgress: Single;
  ACurrentFreq: Single;
  AWaveName: string;
begin
  case AWaveform of
    SIDEVO_WAVE_TRIANGLE: AWaveName := 'TRIANGLE';
    SIDEVO_WAVE_SAWTOOTH: AWaveName := 'SAWTOOTH';
    SIDEVO_WAVE_PULSE: AWaveName := 'PULSE';
    SIDEVO_WAVE_NOISE: AWaveName := 'NOISE';
    else AWaveName := 'UNKNOWN';
  end;

  PrintTestHeader('FREQUENCY SWEEP (' + AWaveName + ')');
  PrintParams(Format('Freq: %.1f Hz -> %.1f Hz, Duration: %d ms', [AFromHz, AToHz, ADurationMs]));

  SIDEvo.SetWaveform(0, AWaveform);
  SIDEvo.SetFrequencyHz(0, AFromHz);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  if AWaveform = SIDEVO_WAVE_PULSE then
    SIDEvo.SetPulseWidth(0, 0.5);
  SIDEvo.GateOn(0);

  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= Cardinal(ADurationMs) then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.StopVoice(0);
      Exit;
    end;

    AProgress := AElapsed / ADurationMs;
    // Logarithmic sweep for more natural sound
    ACurrentFreq := AFromHz * Power(AToHz / AFromHz, AProgress);
    SIDEvo.SetFrequencyHz(0, ACurrentFreq);
    SIDEvo.UpdateActiveVoice(0);

    SDL_Delay(20);
  end;

  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestNoiseSweep(AFromHz, AToHz: Single; ADurationMs: Integer);
begin
  TestFreqSweep(AFromHz, AToHz, SIDEVO_WAVE_NOISE, ADurationMs);
end;

procedure RunSweepTests;
begin
  TestPWMSweep($0400, $0F00, 1500);
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestFreqSweep(100, 4000, SIDEVO_WAVE_SAWTOOTH, 1500);
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestFreqSweep(100, 4000, SIDEVO_WAVE_TRIANGLE, 1500);
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestNoiseSweep(50, 2000, 1000);
end;

// ============================================================================
// 3. ADSR TESTS
// ============================================================================

procedure TestAttack(AAttackValue: Integer; ADurationMs: Integer);
const
  // SID attack times in milliseconds (from ADSR_ATTACK_TIMES array)
  AttackTimesMs: array[0..15] of Integer = (
    2, 8, 16, 24, 38, 56, 68, 80,
    100, 250, 500, 800, 1000, 3000, 5000, 8000
  );
begin
  PrintTestHeader('ATTACK TEST');
  PrintParams(Format('Attack: %d/15 = %dms, D=4, S=F, R=2', [AAttackValue, AttackTimesMs[AAttackValue]]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, AAttackValue/15, 4/15, 15/15, 2/15);
  SIDEvo.GateOn(0);

  if not WaitMs(ADurationMs) then
  begin
    SIDEvo.GateOff(0);
    WaitMs(500);
  end
  else
    SIDEvo.StopVoice(0);
end;

procedure TestDecay(ADecayValue: Integer; ADurationMs: Integer);
const
  // SID decay/release times in milliseconds (from ADSR_DECAY_RELEASE_TIMES array)
  DecayTimesMs: array[0..15] of Integer = (
    6, 24, 48, 72, 114, 168, 204, 240,
    300, 750, 1500, 2400, 3000, 9000, 15000, 24000
  );
var
  AActualDuration: Integer;
begin
  PrintTestHeader('DECAY TEST');
  PrintParams(Format('A=0, Decay: %d/15 = %dms, S=0, R=2', [ADecayValue, DecayTimesMs[ADecayValue]]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, ADecayValue/15, 0, 2/15);
  SIDEvo.GateOn(0);

  // Use actual decay time + margin, or ADurationMs if longer
  AActualDuration := DecayTimesMs[ADecayValue] + 500;  // decay time + 500ms margin
  if ADurationMs > AActualDuration then
    AActualDuration := ADurationMs;

  if not WaitMs(AActualDuration) then
  begin
    SIDEvo.GateOff(0);
    WaitMs(300);
  end
  else
    SIDEvo.StopVoice(0);
end;

procedure TestRelease(AReleaseValue: Integer);
const
  // SID decay/release times in milliseconds (from ADSR_DECAY_RELEASE_TIMES array)
  ReleaseTimesMs: array[0..15] of Integer = (
    6, 24, 48, 72, 114, 168, 204, 240,
    300, 750, 1500, 2400, 3000, 9000, 15000, 24000
  );
begin
  PrintTestHeader('RELEASE TEST');
  PrintParams(Format('A=0, D=4, S=F, Release: %d/15 = %dms', [AReleaseValue, ReleaseTimesMs[AReleaseValue]]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, AReleaseValue/15);
  SIDEvo.GateOn(0);

  if not WaitMs(500) then
  begin
    WriteLn('    Gate OFF - listening to release...');
    SIDEvo.GateOff(0);
    // Wait longer for slow releases
    WaitMs(Min(ReleaseTimesMs[AReleaseValue] + 500, 5000));
  end
  else
    SIDEvo.StopVoice(0);
end;

procedure RunADSRTests;
const
  // Attack values chosen for audible differences:
  // 0 = 2ms (instant), 9 = 250ms (quick), 11 = 800ms (medium), 13 = 3s (slow), 15 = 8s (very slow)
  AttackValues: array[0..4] of Integer = (0, 9, 11, 13, 15);
  // Decay values: 0 = 6ms, 10 = 1.5s, 12 = 3s, 14 = 15s
  DecayValues: array[0..3] of Integer = (0, 10, 12, 14);
  // Release values: 0 = 6ms, 10 = 1.5s, 12 = 3s, 14 = 15s
  ReleaseValues: array[0..3] of Integer = (0, 10, 12, 14);
var
  i: Integer;
begin
  PrintTestHeader('ADSR TESTS - ATTACK');
  WriteLn('    Testing attack times: 2ms, 250ms, 800ms, 3s, 8s');
  for i := 0 to High(AttackValues) do
  begin
    TestAttack(AttackValues[i], 2500);  // Longer duration to hear slow attacks
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;

  PrintTestHeader('ADSR TESTS - DECAY');
  WriteLn('    Testing decay times: 6ms, 1.5s, 3s, 15s');
  for i := 0 to High(DecayValues) do
  begin
    TestDecay(DecayValues[i], 4000);  // Longer duration for slow decays
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;

  PrintTestHeader('ADSR TESTS - RELEASE');
  WriteLn('    Testing release times: 6ms, 1.5s, 3s, 15s');
  for i := 0 to High(ReleaseValues) do
  begin
    TestRelease(ReleaseValues[i]);
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;
end;

// ============================================================================
// 4. FILTER TESTS
// ============================================================================

procedure TestLowpassSweep(AResonance: Integer; ADurationMs: Integer);
var
  AStartTime, AElapsed: UInt32;
  AProgress: Single;
  ACutoff: Single;
begin
  PrintTestHeader('LOWPASS FILTER SWEEP');
  PrintParams(Format('Resonance: %d/15, Duration: %d ms', [AResonance, ADurationMs]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);

  // Enable per-voice filter
  SIDEvo.SetVoiceFilter(0, True);
  SIDEvo.SetVoiceFilterType(0, SIDEVO_FILTER_LOWPASS);
  SIDEvo.SetVoiceFilterResonance(0, AResonance / 15);
  SIDEvo.SetVoiceFilterCutoff(0, 0);

  SIDEvo.GateOn(0);

  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= Cardinal(ADurationMs) then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.SetVoiceFilter(0, False);
      SIDEvo.StopVoice(0);
      Exit;
    end;

    AProgress := AElapsed / ADurationMs;
    ACutoff := AProgress;  // 0 to 1
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);

    SDL_Delay(20);
  end;

  SIDEvo.GateOff(0);
  SIDEvo.SetVoiceFilter(0, False);
  WaitMs(300);
end;

procedure TestHighpassSweep(AResonance: Integer; ADurationMs: Integer);
var
  AStartTime, AElapsed: UInt32;
  AProgress: Single;
  ACutoff: Single;
begin
  PrintTestHeader('HIGHPASS FILTER SWEEP');
  PrintParams(Format('Resonance: %d/15, Duration: %d ms', [AResonance, ADurationMs]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);

  SIDEvo.SetVoiceFilter(0, True);
  SIDEvo.SetVoiceFilterType(0, SIDEVO_FILTER_HIGHPASS);
  SIDEvo.SetVoiceFilterResonance(0, AResonance / 15);
  SIDEvo.SetVoiceFilterCutoff(0, 0);

  SIDEvo.GateOn(0);

  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= Cardinal(ADurationMs) then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.SetVoiceFilter(0, False);
      SIDEvo.StopVoice(0);
      Exit;
    end;

    AProgress := AElapsed / ADurationMs;
    ACutoff := AProgress;
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);

    SDL_Delay(20);
  end;

  SIDEvo.GateOff(0);
  SIDEvo.SetVoiceFilter(0, False);
  WaitMs(300);
end;

procedure TestBandpassSweep(AResonance: Integer; ADurationMs: Integer);
var
  AStartTime, AElapsed: UInt32;
  AProgress: Single;
  ACutoff: Single;
begin
  PrintTestHeader('BANDPASS FILTER SWEEP');
  PrintParams(Format('Resonance: %d/15, Duration: %d ms', [AResonance, ADurationMs]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);

  SIDEvo.SetVoiceFilter(0, True);
  SIDEvo.SetVoiceFilterType(0, SIDEVO_FILTER_BANDPASS);
  SIDEvo.SetVoiceFilterResonance(0, AResonance / 15);
  SIDEvo.SetVoiceFilterCutoff(0, 0);

  SIDEvo.GateOn(0);

  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= Cardinal(ADurationMs) then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.SetVoiceFilter(0, False);
      SIDEvo.StopVoice(0);
      Exit;
    end;

    AProgress := AElapsed / ADurationMs;
    ACutoff := AProgress;
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);

    SDL_Delay(20);
  end;

  SIDEvo.GateOff(0);
  SIDEvo.SetVoiceFilter(0, False);
  WaitMs(300);
end;

procedure TestNotchSweep(AResonance: Integer; ADurationMs: Integer);
var
  AStartTime, AElapsed: UInt32;
  AProgress: Single;
  ACutoff: Single;
begin
  PrintTestHeader('NOTCH FILTER SWEEP');
  PrintParams(Format('Resonance: %d/15, Duration: %d ms', [AResonance, ADurationMs]));

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);

  SIDEvo.SetVoiceFilter(0, True);
  SIDEvo.SetVoiceFilterType(0, SIDEVO_FILTER_NOTCH);
  SIDEvo.SetVoiceFilterResonance(0, AResonance / 15);
  SIDEvo.SetVoiceFilterCutoff(0, 0);

  SIDEvo.GateOn(0);

  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= Cardinal(ADurationMs) then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.SetVoiceFilter(0, False);
      SIDEvo.StopVoice(0);
      Exit;
    end;

    AProgress := AElapsed / ADurationMs;
    ACutoff := AProgress;
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);

    SDL_Delay(20);
  end;

  SIDEvo.GateOff(0);
  SIDEvo.SetVoiceFilter(0, False);
  WaitMs(300);
end;

procedure RunFilterTests;
const
  Resonances: array[0..3] of Integer = (0, 4, 8, 15);
var
  i: Integer;
begin
  PrintTestHeader('FILTER TESTS - LOWPASS');
  for i := 0 to High(Resonances) do
  begin
    TestLowpassSweep(Resonances[i], 1500);
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;

  PrintTestHeader('FILTER TESTS - HIGHPASS');
  for i := 0 to High(Resonances) do
  begin
    TestHighpassSweep(Resonances[i], 1500);
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;

  PrintTestHeader('FILTER TESTS - BANDPASS');
  for i := 0 to High(Resonances) do
  begin
    TestBandpassSweep(Resonances[i], 1500);
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;

  PrintTestHeader('FILTER TESTS - NOTCH');
  for i := 0 to High(Resonances) do
  begin
    TestNotchSweep(Resonances[i], 1500);
    if Assigned(GAbortCheck) and GAbortCheck() then Exit;
  end;
end;

// ============================================================================
// 5. MODULATION TESTS
// ============================================================================

procedure TestSync;
begin
  PrintTestHeader('HARD SYNC TEST');
  PrintParams('Voice 0: 440 Hz (master), Voice 1: 880 Hz (slave, synced)');

  // Master voice
  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  SIDEvo.SetVoiceVolume(0, 0.5);

  // Slave voice with sync
  SIDEvo.SetWaveform(1, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(1, 880);
  SIDEvo.SetADSR(1, 0, 4/15, 15/15, 2/15);
  SIDEvo.SetVoiceVolume(1, 0.5);
  SIDEvo.SetHardSync(1, True, 0);  // Sync to voice 0

  SIDEvo.GateOn(0);
  SIDEvo.GateOn(1);

  if not WaitMs(2000) then
  begin
    SIDEvo.GateOff(0);
    SIDEvo.GateOff(1);
    SIDEvo.SetHardSync(1, False);
    WaitMs(500);
  end
  else
  begin
    SIDEvo.SetHardSync(1, False);
    SIDEvo.StopVoice(0);
    SIDEvo.StopVoice(1);
  end;
end;

procedure TestRingMod;
begin
  PrintTestHeader('RING MODULATION TEST');
  PrintParams('Voice 0: 440 Hz (carrier), Voice 1: 660 Hz (modulator)');

  // Carrier (triangle for best ring mod sound)
  SIDEvo.SetWaveform(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  SIDEvo.SetVoiceVolume(0, 0.7);

  // Modulator
  SIDEvo.SetWaveform(1, SIDEVO_WAVE_TRIANGLE);
  SIDEvo.SetFrequencyHz(1, 660);
  SIDEvo.SetADSR(1, 0, 4/15, 15/15, 2/15);
  SIDEvo.SetVoiceVolume(1, 0);  // Silent, only for modulation
  SIDEvo.SetRingModulation(0, True, 1);  // Ring mod voice 0 with voice 1

  SIDEvo.GateOn(0);
  SIDEvo.GateOn(1);

  if not WaitMs(2000) then
  begin
    SIDEvo.GateOff(0);
    SIDEvo.GateOff(1);
    SIDEvo.SetRingModulation(0, False);
    WaitMs(500);
  end
  else
  begin
    SIDEvo.SetRingModulation(0, False);
    SIDEvo.StopVoice(0);
    SIDEvo.StopVoice(1);
  end;
end;

procedure TestSyncSweep;
var
  AStartTime, AElapsed: UInt32;
  AProgress: Single;
  ASlaveFreq: Single;
begin
  PrintTestHeader('SYNC SWEEP TEST');
  PrintParams('Voice 0: 440 Hz (master), Voice 1: 200->2000 Hz (slave, synced)');

  // Master voice
  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  SIDEvo.SetVoiceVolume(0, 0.5);

  // Slave voice with sync
  SIDEvo.SetWaveform(1, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(1, 200);
  SIDEvo.SetADSR(1, 0, 4/15, 15/15, 2/15);
  SIDEvo.SetVoiceVolume(1, 0.5);
  SIDEvo.SetHardSync(1, True, 0);

  SIDEvo.GateOn(0);
  SIDEvo.GateOn(1);

  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= 3000 then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.SetHardSync(1, False);
      SIDEvo.StopVoice(0);
      SIDEvo.StopVoice(1);
      Exit;
    end;

    AProgress := AElapsed / 3000;
    ASlaveFreq := 200 * Power(2000 / 200, AProgress);
    SIDEvo.SetFrequencyHz(1, ASlaveFreq);
    SIDEvo.UpdateActiveVoice(1);

    SDL_Delay(20);
  end;

  SIDEvo.GateOff(0);
  SIDEvo.GateOff(1);
  SIDEvo.SetHardSync(1, False);
  WaitMs(500);
end;

procedure RunModulationTests;
begin
  TestSync;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestRingMod;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestSyncSweep;
end;

// ============================================================================
// 6. MUSICAL EFFECT TESTS
// ============================================================================

procedure TestArpeggio;
const
  // C major: C4, E4, G4
  Notes: array[0..2] of Single = (261.63, 329.63, 392.00);
var
  i, j: Integer;
begin
  PrintTestHeader('ARPEGGIO (C Major)');
  PrintParams('C-E-G @ 20 Hz frame rate (50ms per note)');

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_PULSE);
  SIDEvo.SetPulseWidth(0, 0.5);
  SIDEvo.SetADSR(0, 0, 2/15, 12/15, 2/15);
  SIDEvo.SetFrequencyHz(0, Notes[0]);
  SIDEvo.GateOn(0);

  for j := 0 to 9 do  // 10 cycles
  begin
    for i := 0 to High(Notes) do
    begin
      if Assigned(GAbortCheck) and GAbortCheck() then
      begin
        SIDEvo.StopVoice(0);
        Exit;
      end;

      SIDEvo.SetFrequencyHz(0, Notes[i]);
      SIDEvo.UpdateActiveVoice(0);
      SDL_Delay(50);  // 20 Hz - slower to hear the difference
    end;
  end;

  SIDEvo.GateOff(0);
  WaitMs(500);  // Longer pause before next test
end;

procedure TestArpeggioMinor;
const
  // C minor: C4, Eb4, G4
  Notes: array[0..2] of Single = (261.63, 311.13, 392.00);
var
  i, j: Integer;
begin
  PrintTestHeader('ARPEGGIO (C Minor)');
  PrintParams('C-Eb-G @ 20 Hz frame rate (50ms per note)');

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_PULSE);
  SIDEvo.SetPulseWidth(0, 0.5);
  SIDEvo.SetADSR(0, 0, 2/15, 12/15, 2/15);
  SIDEvo.SetFrequencyHz(0, Notes[0]);
  SIDEvo.GateOn(0);

  for j := 0 to 9 do
  begin
    for i := 0 to High(Notes) do
    begin
      if Assigned(GAbortCheck) and GAbortCheck() then
      begin
        SIDEvo.StopVoice(0);
        Exit;
      end;

      SIDEvo.SetFrequencyHz(0, Notes[i]);
      SIDEvo.UpdateActiveVoice(0);
      SDL_Delay(50);  // 20 Hz - slower to hear the difference
    end;
  end;

  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestTrill;
const
  Freq1 = 440.0;   // A4
  Freq2 = 466.16;  // A#4
var
  i: Integer;
  ACurrentFreq: Single;
begin
  PrintTestHeader('TRILL');
  PrintParams('A4 <-> A#4 alternating every 40ms');

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetADSR(0, 0, 2/15, 15/15, 2/15);
  SIDEvo.SetFrequencyHz(0, Freq1);
  SIDEvo.GateOn(0);

  for i := 0 to 49 do  // 50 alternations = 2 seconds
  begin
    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.StopVoice(0);
      Exit;
    end;

    if (i mod 2) = 0 then
      ACurrentFreq := Freq1
    else
      ACurrentFreq := Freq2;

    SIDEvo.SetFrequencyHz(0, ACurrentFreq);
    SIDEvo.UpdateActiveVoice(0);
    SDL_Delay(40);
  end;

  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestVibrato;
var
  AStartTime, AElapsed: UInt32;
  AProgress: Single;
  ABaseFreq: Single;
  AVibratoFreq: Single;
  AVibratoDepth: Single;
  ALFOPhase: Single;
begin
  PrintTestHeader('VIBRATO');
  PrintParams('Base: 440 Hz, Depth: +/-10 Hz, Rate: 6 Hz');

  ABaseFreq := 440;
  AVibratoDepth := 10;  // +/- 10 Hz

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  SIDEvo.SetFrequencyHz(0, ABaseFreq);
  SIDEvo.GateOn(0);

  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= 3000 then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.StopVoice(0);
      Exit;
    end;

    // 6 Hz LFO
    ALFOPhase := (AElapsed / 1000) * 6 * 2 * Pi;
    AVibratoFreq := ABaseFreq + Sin(ALFOPhase) * AVibratoDepth;

    SIDEvo.SetFrequencyHz(0, AVibratoFreq);
    SIDEvo.UpdateActiveVoice(0);

    SDL_Delay(10);
  end;

  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestPWMVibrato;
var
  AStartTime, AElapsed: UInt32;
  ALFOPhase: Single;
  APulseWidth: Single;
begin
  PrintTestHeader('PWM VIBRATO');
  PrintParams('Freq: 440 Hz, PW oscillating 0.3-0.7 @ 5 Hz');

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_PULSE);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 2/15);
  SIDEvo.SetPulseWidth(0, 0.5);
  SIDEvo.GateOn(0);

  AStartTime := SDL_GetTicks;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= 3000 then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.StopVoice(0);
      Exit;
    end;

    // 5 Hz LFO for PWM
    ALFOPhase := (AElapsed / 1000) * 5 * 2 * Pi;
    APulseWidth := 0.5 + Sin(ALFOPhase) * 0.2;  // 0.3 to 0.7

    SIDEvo.SetPulseWidth(0, APulseWidth);
    SIDEvo.UpdateActiveVoice(0);

    SDL_Delay(10);
  end;

  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestNoiseDrum;
begin
  PrintTestHeader('NOISE DRUM');
  PrintParams('Short noise burst - percussion hit');

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_NOISE);
  SIDEvo.SetFrequencyHz(0, 200);
  SIDEvo.SetADSR(0, 0, 2/15, 0, 2/15);

  // Play 4 hits
  SIDEvo.GateOn(0);
  if WaitMs(50) then begin SIDEvo.StopVoice(0); Exit; end;
  SIDEvo.GateOff(0);
  if WaitMs(200) then Exit;

  SIDEvo.GateOn(0);
  if WaitMs(50) then begin SIDEvo.StopVoice(0); Exit; end;
  SIDEvo.GateOff(0);
  if WaitMs(200) then Exit;

  SIDEvo.GateOn(0);
  if WaitMs(50) then begin SIDEvo.StopVoice(0); Exit; end;
  SIDEvo.GateOff(0);
  if WaitMs(200) then Exit;

  SIDEvo.GateOn(0);
  if WaitMs(50) then begin SIDEvo.StopVoice(0); Exit; end;
  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestKickSynth;
var
  AStartTime, AElapsed: UInt32;
  AProgress: Single;
  AFreq: Single;
begin
  PrintTestHeader('KICK SYNTH');
  PrintParams('Triangle, freq sweep 80->20 Hz in 40ms');

  // Play 4 kicks
  SIDEvo.SetWaveform(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvo.SetADSR(0, 0, 4/15, 0, 2/15);

  // Kick 1
  SIDEvo.SetFrequencyHz(0, 80);
  SIDEvo.GateOn(0);
  AStartTime := SDL_GetTicks;
  while (SDL_GetTicks - AStartTime) < 40 do
  begin
    if Assigned(GAbortCheck) and GAbortCheck() then begin SIDEvo.StopVoice(0); Exit; end;
    AElapsed := SDL_GetTicks - AStartTime;
    AProgress := AElapsed / 40;
    AFreq := 80 - (60 * AProgress);  // 80 to 20
    SIDEvo.SetFrequencyHz(0, AFreq);
    SIDEvo.UpdateActiveVoice(0);
    SDL_Delay(2);
  end;
  SIDEvo.GateOff(0);
  if WaitMs(300) then Exit;

  // Kick 2
  SIDEvo.SetFrequencyHz(0, 80);
  SIDEvo.GateOn(0);
  AStartTime := SDL_GetTicks;
  while (SDL_GetTicks - AStartTime) < 40 do
  begin
    if Assigned(GAbortCheck) and GAbortCheck() then begin SIDEvo.StopVoice(0); Exit; end;
    AElapsed := SDL_GetTicks - AStartTime;
    AProgress := AElapsed / 40;
    AFreq := 80 - (60 * AProgress);
    SIDEvo.SetFrequencyHz(0, AFreq);
    SIDEvo.UpdateActiveVoice(0);
    SDL_Delay(2);
  end;
  SIDEvo.GateOff(0);
  if WaitMs(300) then Exit;

  // Kick 3
  SIDEvo.SetFrequencyHz(0, 80);
  SIDEvo.GateOn(0);
  AStartTime := SDL_GetTicks;
  while (SDL_GetTicks - AStartTime) < 40 do
  begin
    if Assigned(GAbortCheck) and GAbortCheck() then begin SIDEvo.StopVoice(0); Exit; end;
    AElapsed := SDL_GetTicks - AStartTime;
    AProgress := AElapsed / 40;
    AFreq := 80 - (60 * AProgress);
    SIDEvo.SetFrequencyHz(0, AFreq);
    SIDEvo.UpdateActiveVoice(0);
    SDL_Delay(2);
  end;
  SIDEvo.GateOff(0);
  if WaitMs(300) then Exit;

  // Kick 4
  SIDEvo.SetFrequencyHz(0, 80);
  SIDEvo.GateOn(0);
  AStartTime := SDL_GetTicks;
  while (SDL_GetTicks - AStartTime) < 40 do
  begin
    if Assigned(GAbortCheck) and GAbortCheck() then begin SIDEvo.StopVoice(0); Exit; end;
    AElapsed := SDL_GetTicks - AStartTime;
    AProgress := AElapsed / 40;
    AFreq := 80 - (60 * AProgress);
    SIDEvo.SetFrequencyHz(0, AFreq);
    SIDEvo.UpdateActiveVoice(0);
    SDL_Delay(2);
  end;
  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestSnareSynth;
begin
  PrintTestHeader('SNARE SYNTH');
  PrintParams('Noise + short saw burst');

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_NOISE);
  SIDEvo.SetFrequencyHz(0, 200);
  SIDEvo.SetADSR(0, 0, 2/15, 0, 2/15);
  SIDEvo.SetVoiceVolume(0, 0.6);

  SIDEvo.SetWaveform(1, SIDEVO_WAVE_SAWTOOTH);
  SIDEvo.SetFrequencyHz(1, 1000);
  SIDEvo.SetADSR(1, 0, 1/15, 0, 1/15);
  SIDEvo.SetVoiceVolume(1, 0.4);

  // Hit 1
  SIDEvo.GateOn(0);
  SIDEvo.GateOn(1);
  if WaitMs(10) then begin SIDEvo.StopVoice(0); SIDEvo.StopVoice(1); Exit; end;
  SIDEvo.GateOff(1);
  if WaitMs(50) then begin SIDEvo.StopVoice(0); Exit; end;
  SIDEvo.GateOff(0);
  if WaitMs(250) then Exit;

  // Hit 2
  SIDEvo.GateOn(0);
  SIDEvo.GateOn(1);
  if WaitMs(10) then begin SIDEvo.StopVoice(0); SIDEvo.StopVoice(1); Exit; end;
  SIDEvo.GateOff(1);
  if WaitMs(50) then begin SIDEvo.StopVoice(0); Exit; end;
  SIDEvo.GateOff(0);
  if WaitMs(250) then Exit;

  // Hit 3
  SIDEvo.GateOn(0);
  SIDEvo.GateOn(1);
  if WaitMs(10) then begin SIDEvo.StopVoice(0); SIDEvo.StopVoice(1); Exit; end;
  SIDEvo.GateOff(1);
  if WaitMs(50) then begin SIDEvo.StopVoice(0); Exit; end;
  SIDEvo.GateOff(0);
  if WaitMs(250) then Exit;

  // Hit 4
  SIDEvo.GateOn(0);
  SIDEvo.GateOn(1);
  if WaitMs(10) then begin SIDEvo.StopVoice(0); SIDEvo.StopVoice(1); Exit; end;
  SIDEvo.GateOff(1);
  if WaitMs(50) then begin SIDEvo.StopVoice(0); Exit; end;
  SIDEvo.GateOff(0);
  WaitMs(300);

  SIDEvo.SetVoiceVolume(0, 0.8);
  SIDEvo.SetVoiceVolume(1, 0.8);
end;

procedure RunEffectTests;
begin
  TestArpeggio;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestArpeggioMinor;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestTrill;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestVibrato;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestPWMVibrato;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestNoiseDrum;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestKickSynth;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestSnareSynth;
end;

// ============================================================================
// 7. COMPLEX PATTERN TESTS
// ============================================================================

procedure TestFastArpeggio;
const
  // C major 7th: C4, E4, G4, B4
  Notes: array[0..3] of Single = (261.63, 329.63, 392.00, 493.88);
var
  i, j: Integer;
begin
  PrintTestHeader('FAST ARPEGGIO (50 Hz)');
  PrintParams('C-E-G-B @ 50 Hz (SID-style frame rate)');

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_PULSE);
  SIDEvo.SetPulseWidth(0, 0.25);
  SIDEvo.SetADSR(0, 0, 1/15, 12/15, 1/15);
  SIDEvo.SetFrequencyHz(0, Notes[0]);
  SIDEvo.GateOn(0);

  for j := 0 to 24 do  // 25 cycles = 2 seconds
  begin
    for i := 0 to High(Notes) do
    begin
      if Assigned(GAbortCheck) and GAbortCheck() then
      begin
        SIDEvo.StopVoice(0);
        Exit;
      end;

      SIDEvo.SetFrequencyHz(0, Notes[i]);
      SIDEvo.UpdateActiveVoice(0);
      SDL_Delay(20);  // 50 Hz
    end;
  end;

  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestSlowArpeggio;
const
  Notes: array[0..2] of Single = (261.63, 329.63, 392.00);
var
  i, j: Integer;
begin
  PrintTestHeader('SLOW ARPEGGIO');
  PrintParams('C-E-G @ 100ms per note');

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_PULSE);
  SIDEvo.SetPulseWidth(0, 0.5);
  SIDEvo.SetADSR(0, 0, 4/15, 12/15, 4/15);
  SIDEvo.SetFrequencyHz(0, Notes[0]);
  SIDEvo.GateOn(0);

  for j := 0 to 5 do  // 6 cycles = ~1.8 seconds
  begin
    for i := 0 to High(Notes) do
    begin
      if Assigned(GAbortCheck) and GAbortCheck() then
      begin
        SIDEvo.StopVoice(0);
        Exit;
      end;

      SIDEvo.SetFrequencyHz(0, Notes[i]);
      SIDEvo.UpdateActiveVoice(0);
      if WaitMs(100) then
      begin
        SIDEvo.StopVoice(0);
        Exit;
      end;
    end;
  end;

  SIDEvo.GateOff(0);
  WaitMs(300);
end;

procedure TestCombinedEffects;
var
  AStartTime, AElapsed: UInt32;
  ALFOPhase: Single;
  APulseWidth: Single;
  AFreq: Single;
  AVibratoDepth: Single;
  ACutoff: Single;
begin
  PrintTestHeader('COMBINED EFFECTS');
  PrintParams('PWM + Vibrato + Filter sweep');

  SIDEvo.SetWaveform(0, SIDEVO_WAVE_PULSE);
  SIDEvo.SetFrequencyHz(0, 440);
  SIDEvo.SetADSR(0, 0, 4/15, 15/15, 4/15);
  SIDEvo.SetPulseWidth(0, 0.5);

  // Enable filter
  SIDEvo.SetVoiceFilter(0, True);
  SIDEvo.SetVoiceFilterType(0, SIDEVO_FILTER_LOWPASS);
  SIDEvo.SetVoiceFilterResonance(0, 8/15);
  SIDEvo.SetVoiceFilterCutoff(0, 0.2);

  SIDEvo.GateOn(0);

  AStartTime := SDL_GetTicks;
  AVibratoDepth := 10;

  while True do
  begin
    AElapsed := SDL_GetTicks - AStartTime;
    if AElapsed >= 5000 then Break;

    if Assigned(GAbortCheck) and GAbortCheck() then
    begin
      SIDEvo.SetVoiceFilter(0, False);
      SIDEvo.StopVoice(0);
      Exit;
    end;

    // PWM at 5 Hz
    ALFOPhase := (AElapsed / 1000) * 5 * 2 * Pi;
    APulseWidth := 0.5 + Sin(ALFOPhase) * 0.2;
    SIDEvo.SetPulseWidth(0, APulseWidth);

    // Vibrato at 6 Hz
    ALFOPhase := (AElapsed / 1000) * 6 * 2 * Pi;
    AFreq := 440 + Sin(ALFOPhase) * AVibratoDepth;
    SIDEvo.SetFrequencyHz(0, AFreq);

    // Filter sweep (slow, 0.2 Hz)
    ALFOPhase := (AElapsed / 1000) * 0.2 * 2 * Pi;
    ACutoff := 0.5 + Sin(ALFOPhase) * 0.3;  // 0.2 to 0.8
    SIDEvo.SetVoiceFilterCutoff(0, ACutoff);

    SIDEvo.UpdateActiveVoice(0);

    SDL_Delay(10);
  end;

  SIDEvo.GateOff(0);
  SIDEvo.SetVoiceFilter(0, False);
  WaitMs(500);
end;

procedure RunPatternTests;
begin
  TestFastArpeggio;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestSlowArpeggio;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  TestCombinedEffects;
end;

// ============================================================================
// FULL TEST SUITE
// ============================================================================

procedure RunFullTestSuite;
begin
  WriteLn;
  WriteLn('========================================');
  WriteLn('  SID EVO FULL TEST SUITE');
  WriteLn('========================================');
  WriteLn;

  WriteLn('>>> 1. WAVEFORM TESTS <<<');
  RunWaveformTests;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  WriteLn;
  WriteLn('>>> 2. SWEEP TESTS <<<');
  RunSweepTests;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  WriteLn;
  WriteLn('>>> 3. ADSR TESTS <<<');
  RunADSRTests;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  WriteLn;
  WriteLn('>>> 4. FILTER TESTS <<<');
  RunFilterTests;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  WriteLn;
  WriteLn('>>> 5. MODULATION TESTS <<<');
  RunModulationTests;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  WriteLn;
  WriteLn('>>> 6. EFFECT TESTS <<<');
  RunEffectTests;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  WriteLn;
  WriteLn('>>> 7. PATTERN TESTS <<<');
  RunPatternTests;
  if Assigned(GAbortCheck) and GAbortCheck() then Exit;

  WriteLn;
  WriteLn('========================================');
  WriteLn('  TEST SUITE COMPLETE');
  WriteLn('========================================');
end;

end.
