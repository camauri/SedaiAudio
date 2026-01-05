{*
 * SedaiSIDEvo Test
 *
 * This program generates output from SedaiSIDEvo that can be compared
 * with ReSID reference output to verify correctness.
 *}

program sedaisid_test;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes,
  SedaiSIDEvo;

var
  SID: TSedaiSIDEvo;

procedure PrintState(const ALabel: string);
var
  I: Integer;
begin
  WriteLn(ALabel, ':');
  Write('  Accumulators: [');
  for I := 0 to 2 do
  begin
    Write(Format('%06X', [SID.GetAccumulator(I)]));
    if I < 2 then Write(', ');
  end;
  WriteLn(']');

  Write('  Envelopes:    [');
  for I := 0 to 2 do
  begin
    Write(Format('%02X', [SID.GetEnvelopeCounter(I)]));
    if I < 2 then Write(', ');
  end;
  WriteLn(']');

  WriteLn('  Output: ', Round(SID.Output * 32768));
  WriteLn;
end;

procedure TestOscillatorBasic;
var
  I: Integer;
begin
  WriteLn('=== TEST 1: Basic Oscillator Test ===');
  WriteLn;

  SID.Reset;

  // Set up voice 0: frequency $1D34 (~440Hz), sawtooth
  SID.WriteRegister($00, $34);  // Freq lo
  SID.WriteRegister($01, $1D);  // Freq hi
  SID.WriteRegister($05, $00);  // AD = 0
  SID.WriteRegister($06, $F0);  // SR = F0 (sustain max)
  SID.WriteRegister($04, $21);  // Gate on, Sawtooth
  SID.WriteRegister($18, $0F);  // Volume max

  PrintState('Initial state');

  // Clock 100 cycles and show accumulator progression
  WriteLn('Accumulator progression (first 10 clocks):');
  for I := 1 to 10 do
  begin
    SID.ClockCycle;
    WriteLn(Format('  Cycle %d: acc=%06X', [I, SID.GetAccumulator(0)]));
  end;

  // Clock more cycles
  for I := 1 to 90 do
    SID.ClockCycle;

  PrintState('After 100 cycles');

  // Clock 1000 more cycles
  for I := 1 to 1000 do
    SID.ClockCycle;

  PrintState('After 1100 cycles');
end;

procedure TestEnvelopeADSR;
var
  Checkpoints: array[0..5] of Integer = (100, 500, 1000, 2000, 5000, 10000);
  ReleaseCheckpoints: array[0..5] of Integer = (100, 500, 1000, 5000, 10000, 20000);
  Total, I, J, Cycles: Integer;
begin
  WriteLn('=== TEST 2: Envelope ADSR Test ===');
  WriteLn;

  SID.Reset;

  // Set up voice 0 with envelope
  SID.WriteRegister($00, $00);  // Freq lo = 0
  SID.WriteRegister($01, $10);  // Freq hi = $10
  SID.WriteRegister($05, $11);  // Attack=1, Decay=1
  SID.WriteRegister($06, $84);  // Sustain=8, Release=4

  PrintState('Before gate on');

  // Gate on
  SID.WriteRegister($04, $21);  // Gate on, Sawtooth

  // Sample envelope at intervals
  WriteLn('Envelope progression:');
  Total := 0;
  for I := 0 to 5 do
  begin
    Cycles := Checkpoints[I] - Total;
    for J := 1 to Cycles do
      SID.ClockCycle;
    Total := Checkpoints[I];
    WriteLn(Format('  Cycle %5d: envelope=%02X', [Total, SID.GetEnvelopeCounter(0)]));
  end;

  // Gate off
  WriteLn;
  WriteLn('Gate off:');
  SID.WriteRegister($04, $20);  // Gate off, Sawtooth

  Total := 0;
  for I := 0 to 5 do
  begin
    Cycles := ReleaseCheckpoints[I] - Total;
    for J := 1 to Cycles do
      SID.ClockCycle;
    Total := ReleaseCheckpoints[I];
    WriteLn(Format('  Cycle %5d: envelope=%02X', [Total, SID.GetEnvelopeCounter(0)]));
  end;
end;

procedure TestWaveforms;
type
  TWaveformTest = record
    Name: string;
    Control: Byte;
  end;
var
  Tests: array[0..4] of TWaveformTest = (
    (Name: 'Triangle'; Control: $11),
    (Name: 'Sawtooth'; Control: $21),
    (Name: 'Pulse'; Control: $41),
    (Name: 'Noise'; Control: $81),
    (Name: 'Triangle+Sawtooth'; Control: $31)
  );
  T, I, J: Integer;
begin
  WriteLn('=== TEST 3: Waveform Output Test ===');
  WriteLn;

  for T := 0 to 4 do
  begin
    SID.Reset;

    // Set up voice 0
    SID.WriteRegister($00, $00);  // Freq lo
    SID.WriteRegister($01, $10);  // Freq hi = $10
    SID.WriteRegister($02, $00);  // PW lo = 0
    SID.WriteRegister($03, $08);  // PW hi = 8 (50% duty)
    SID.WriteRegister($05, $00);  // AD = 0
    SID.WriteRegister($06, $F0);  // SR = F0
    SID.WriteRegister($04, Tests[T].Control);  // Waveform + gate
    SID.WriteRegister($18, $0F);  // Volume max

    WriteLn(Tests[T].Name, ' waveform:');

    // Clock and sample output
    for I := 0 to 4 do
    begin
      for J := 1 to 256 do
        SID.ClockCycle;
      WriteLn(Format('  acc=%06X, output=%6d',
        [SID.GetAccumulator(0), Round(SID.Output * 32768)]));
    end;
    WriteLn;
  end;
end;

procedure TestClockTiming;
var
  I: Integer;
  PrevAcc, CurAcc, Delta: Cardinal;
begin
  WriteLn('=== TEST 4: Clock Timing Test ===');
  WriteLn;

  SID.Reset;

  // Set up voice 0 with known frequency
  // Freq = $1000 means accumulator increases by $1000 each clock
  SID.WriteRegister($00, $00);  // Freq lo = 0
  SID.WriteRegister($01, $10);  // Freq hi = $10 -> freq = $1000
  SID.WriteRegister($05, $00);  // AD = 0
  SID.WriteRegister($06, $F0);  // SR = F0
  SID.WriteRegister($04, $21);  // Gate on, Sawtooth

  WriteLn('Frequency $1000 - accumulator should increase by $1000 each cycle:');

  PrevAcc := SID.GetAccumulator(0);

  for I := 1 to 10 do
  begin
    SID.ClockCycle;
    CurAcc := SID.GetAccumulator(0);
    Delta := (CurAcc - PrevAcc) and $FFFFFF;
    Write(Format('  Cycle %d: acc=%06X, delta=%06X ', [I, CurAcc, Delta]));
    if Delta = $1000 then
      WriteLn('OK')
    else
      WriteLn('ERROR!');
    PrevAcc := CurAcc;
  end;
end;

procedure GenerateReferenceFile;
var
  F: TextFile;
  I, J: Integer;
begin
  WriteLn('=== Generating Reference Data File ===');
  WriteLn;

  AssignFile(F, 'sedaisid_reference.txt');
  Rewrite(F);

  SID.Reset;

  // Test sequence similar to resid-test format
  WriteLn(F, '# SedaiSIDEvo Reference Output');
  WriteLn(F, '# Format: cycle, acc0, acc1, acc2, env0, env1, env2, output');
  WriteLn(F);

  // Set up voice 0: freq=$1D34, sawtooth, ADSR
  SID.WriteRegister($00, $34);
  SID.WriteRegister($01, $1D);
  SID.WriteRegister($05, $11);  // A=1, D=1
  SID.WriteRegister($06, $84);  // S=8, R=4
  SID.WriteRegister($04, $21);  // Gate, Sawtooth
  SID.WriteRegister($18, $0F);  // Volume

  WriteLn(F, '# Voice 0: freq=$1D34, sawtooth, A=1 D=1 S=8 R=4, gate on');

  // Clock and log state at regular intervals
  for I := 0 to 9999 do
  begin
    if I mod 100 = 0 then
    begin
      WriteLn(F, Format('%6d, %06X, %06X, %06X, %02X, %02X, %02X, %6d',
        [I,
         SID.GetAccumulator(0), SID.GetAccumulator(1), SID.GetAccumulator(2),
         SID.GetEnvelopeCounter(0), SID.GetEnvelopeCounter(1), SID.GetEnvelopeCounter(2),
         Round(SID.Output * 32768)]));
    end;
    SID.ClockCycle;
  end;

  // Gate off at cycle 10000
  SID.WriteRegister($04, $20);  // Gate off
  WriteLn(F);
  WriteLn(F, '# Gate off at cycle 10000');

  for I := 10000 to 19999 do
  begin
    if I mod 100 = 0 then
    begin
      WriteLn(F, Format('%6d, %06X, %06X, %06X, %02X, %02X, %02X, %6d',
        [I,
         SID.GetAccumulator(0), SID.GetAccumulator(1), SID.GetAccumulator(2),
         SID.GetEnvelopeCounter(0), SID.GetEnvelopeCounter(1), SID.GetEnvelopeCounter(2),
         Round(SID.Output * 32768)]));
    end;
    SID.ClockCycle;
  end;

  CloseFile(F);
  WriteLn('Reference file ''sedaisid_reference.txt'' generated.');
end;

begin
  WriteLn('SedaiSIDEvo Test');
  WriteLn('================');
  WriteLn;

  SID := TSedaiSIDEvo.Create;
  try
    SID.Clock := 985248;  // PAL clock
    SID.SetSampleRate(44100);

    TestOscillatorBasic;
    TestEnvelopeADSR;
    TestWaveforms;
    TestClockTiming;
    GenerateReferenceFile;

    WriteLn;
    WriteLn('All tests complete.');
  finally
    SID.Free;
  end;
end.
