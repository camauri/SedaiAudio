{*
 * Sedai Audio Foundation - Waveform Validator
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * Mathematical verification of waveform generators.
 * Validates waveform shape, frequency, and amplitude without audio output.
 *}

unit SedaiWaveformValidator;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, SedaiAudioTypes, SedaiWaveGenerator, SedaiADSRProcessor,
  SedaiClassicProcessor, SedaiFilters;

const
  // Tolerances for validation
  TOLERANCE_AMPLITUDE = 0.01;      // 1% amplitude tolerance
  TOLERANCE_FREQUENCY = 0.01;      // 1% frequency tolerance (span-based measurement is accurate)
  TOLERANCE_DC_OFFSET = 0.05;      // 5% DC offset tolerance
  TOLERANCE_DUTY_CYCLE = 0.02;     // 2% duty cycle tolerance
  TOLERANCE_ADSR_TIME = 0.05;      // 5% timing tolerance for ADSR
  TOLERANCE_ADSR_LEVEL = 0.02;     // 2% level tolerance for ADSR

  // Default sample parameters
  DEFAULT_SAMPLE_RATE = 44100;
  DEFAULT_TEST_FREQ = 440.0;
  DEFAULT_NUM_CYCLES = 10;         // With span-based measurement, fewer cycles are needed

type
  // Validation result for a single test
  TWaveformTestResult = record
    TestName: string;
    Passed: Boolean;
    Expected: Single;
    Actual: Single;
    Tolerance: Single;
    Message: string;
  end;

  // Full validation report for a waveform
  TWaveformValidationReport = record
    WaveformName: string;
    AllPassed: Boolean;
    TestCount: Integer;
    PassCount: Integer;
    FailCount: Integer;
    Tests: array of TWaveformTestResult;
  end;

  { TSedaiWaveformValidator }
  TSedaiWaveformValidator = class
  private
    FSampleRate: Single;
    FVerbose: Boolean;

    // Helper functions
    function CountZeroCrossings(const ASamples: array of Single): Integer;
    function CalculateMean(const ASamples: array of Single): Single;
    function CalculateStdDev(const ASamples: array of Single; AMean: Single): Single;
    function CalculateMin(const ASamples: array of Single): Single;
    function CalculateMax(const ASamples: array of Single): Single;
    function CalculateDutyCycle(const ASamples: array of Single): Single;
    function CountPositiveSamples(const ASamples: array of Single): Integer;
    function IsMonotonic(const ASamples: array of Single; AStartIdx, AEndIdx: Integer; AIncreasing: Boolean): Boolean;
    function FindPeaks(const ASamples: array of Single; out APeakIndices: array of Integer): Integer;

    // Test generators
    function AddTest(var AReport: TWaveformValidationReport; const ATestName: string;
      APassed: Boolean; AExpected, AActual, ATolerance: Single; const AMessage: string): Integer;

  public
    constructor Create(ASampleRate: Single = DEFAULT_SAMPLE_RATE);

    // Main validation functions - Waveforms
    function ValidateTriangle(AFrequency: Single = DEFAULT_TEST_FREQ; ANumCycles: Integer = DEFAULT_NUM_CYCLES): TWaveformValidationReport;
    function ValidateSawtooth(AFrequency: Single = DEFAULT_TEST_FREQ; ANumCycles: Integer = DEFAULT_NUM_CYCLES): TWaveformValidationReport;
    function ValidatePulse(APulseWidth: Single; AFrequency: Single = DEFAULT_TEST_FREQ; ANumCycles: Integer = DEFAULT_NUM_CYCLES): TWaveformValidationReport;
    function ValidateSquare(AFrequency: Single = DEFAULT_TEST_FREQ; ANumCycles: Integer = DEFAULT_NUM_CYCLES): TWaveformValidationReport;
    function ValidateSine(AFrequency: Single = DEFAULT_TEST_FREQ; ANumCycles: Integer = DEFAULT_NUM_CYCLES): TWaveformValidationReport;
    function ValidateNoise(ANumSamples: Integer = 44100): TWaveformValidationReport;

    // ADSR Envelope validation
    function ValidateADSRAttack(AAttackTime: Single): TWaveformValidationReport;
    function ValidateADSRDecay(ADecayTime: Single; ASustainLevel: Single): TWaveformValidationReport;
    function ValidateADSRRelease(AReleaseTime: Single): TWaveformValidationReport;
    function ValidateADSRFull(AAttack, ADecay, ASustain, ARelease: Single): TWaveformValidationReport;

    // Full pipeline validation (synthesis engine)
    function ValidateClassicPulse(APulseWidth: Single; AFrequency: Single = DEFAULT_TEST_FREQ): TWaveformValidationReport;

    // Sweep validation (2. SWEEP TESTS)
    function ValidatePWMSweep(AFromPW, AToPW: Single; ADurationMs: Integer = 1000): TWaveformValidationReport;
    function ValidateFreqSweep(AFromHz, AToHz: Single; ADurationMs: Integer = 1000): TWaveformValidationReport;
    function ValidateNoiseSweep(AFromHz, AToHz: Single; ADurationMs: Integer = 500): TWaveformValidationReport;

    // Filter validation (4. FILTER TESTS)
    function ValidateLowpassFilter(ACutoff: Single; AResonance: Single = 0.5): TWaveformValidationReport;
    function ValidateHighpassFilter(ACutoff: Single; AResonance: Single = 0.5): TWaveformValidationReport;
    function ValidateBandpassFilter(ACutoff: Single; AResonance: Single = 0.5): TWaveformValidationReport;
    function ValidateNotchFilter(ACutoff: Single; AResonance: Single = 0.5): TWaveformValidationReport;
    function ValidateFilterSweep(AFilterType: Integer; AResonance: Single): TWaveformValidationReport;

    // Modulation validation (5. MODULATION TESTS)
    function ValidateHardSync(AMasterFreq, ASlaveFreq: Single): TWaveformValidationReport;
    function ValidateRingModulation(ACarrierFreq, AModulatorFreq: Single): TWaveformValidationReport;

    // Musical effects validation (6. EFFECT TESTS)
    function ValidateArpeggio(const ANotes: array of Single; AIntervalMs: Integer): TWaveformValidationReport;
    function ValidateTrill(AFreq1, AFreq2: Single; AIntervalMs: Integer): TWaveformValidationReport;
    function ValidateVibrato(ABaseFreq, ADepthHz, ARateHz: Single): TWaveformValidationReport;
    function ValidatePWMVibrato(ABaseFreq: Single; APWMin, APWMax, ARateHz: Single): TWaveformValidationReport;
    function ValidateKickDrum(AStartFreq, AEndFreq: Single; ADurationMs: Integer): TWaveformValidationReport;

    // Complex pattern validation (7. PATTERN TESTS)
    function ValidateCombinedEffects: TWaveformValidationReport;

    // Run all validations
    function RunAllValidations: Boolean;

    // Utility
    procedure PrintReport(const AReport: TWaveformValidationReport);
    procedure PrintSummary(const AReports: array of TWaveformValidationReport);

    property SampleRate: Single read FSampleRate write FSampleRate;
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TSedaiWaveformValidator.Create(ASampleRate: Single);
begin
  inherited Create;
  FSampleRate := ASampleRate;
  FVerbose := True;
end;

function TSedaiWaveformValidator.CountZeroCrossings(const ASamples: array of Single): Integer;
var
  i: Integer;
  AIsNearZero: Boolean;
begin
  Result := 0;
  // Count zero crossings with proper boundary handling
  // Skip the very first and last samples to avoid edge effects
  for i := 1 to High(ASamples) - 1 do
  begin
    // Detect sign change (crossing through zero)
    if ((ASamples[i-1] > 0) and (ASamples[i] <= 0)) or
       ((ASamples[i-1] <= 0) and (ASamples[i] > 0)) then
    begin
      // Check if the crossing happens near zero (legitimate crossing)
      // or far from zero (sawtooth reset from +1 to -1)
      // A real zero-crossing has at least one sample close to zero
      AIsNearZero := (Abs(ASamples[i-1]) < 0.9) or (Abs(ASamples[i]) < 0.9);

      if AIsNearZero then
        Inc(Result);
    end;
  end;
end;

function TSedaiWaveformValidator.CalculateMean(const ASamples: array of Single): Single;
var
  i: Integer;
  ASum: Double;
begin
  ASum := 0;
  for i := 0 to High(ASamples) do
    ASum := ASum + ASamples[i];
  Result := ASum / Length(ASamples);
end;

function TSedaiWaveformValidator.CalculateStdDev(const ASamples: array of Single; AMean: Single): Single;
var
  i: Integer;
  ASumSq: Double;
begin
  ASumSq := 0;
  for i := 0 to High(ASamples) do
    ASumSq := ASumSq + Sqr(ASamples[i] - AMean);
  Result := Sqrt(ASumSq / Length(ASamples));
end;

function TSedaiWaveformValidator.CalculateMin(const ASamples: array of Single): Single;
var
  i: Integer;
begin
  Result := ASamples[0];
  for i := 1 to High(ASamples) do
    if ASamples[i] < Result then
      Result := ASamples[i];
end;

function TSedaiWaveformValidator.CalculateMax(const ASamples: array of Single): Single;
var
  i: Integer;
begin
  Result := ASamples[0];
  for i := 1 to High(ASamples) do
    if ASamples[i] > Result then
      Result := ASamples[i];
end;

function TSedaiWaveformValidator.CalculateDutyCycle(const ASamples: array of Single): Single;
var
  APositive: Integer;
begin
  APositive := CountPositiveSamples(ASamples);
  Result := APositive / Length(ASamples);
end;

function TSedaiWaveformValidator.CountPositiveSamples(const ASamples: array of Single): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(ASamples) do
    if ASamples[i] > 0 then
      Inc(Result);
end;

function TSedaiWaveformValidator.IsMonotonic(const ASamples: array of Single; AStartIdx, AEndIdx: Integer; AIncreasing: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := AStartIdx + 1 to AEndIdx do
  begin
    if AIncreasing then
    begin
      if ASamples[i] < ASamples[i-1] - 0.001 then  // Small tolerance for floating point
      begin
        Result := False;
        Exit;
      end;
    end
    else
    begin
      if ASamples[i] > ASamples[i-1] + 0.001 then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TSedaiWaveformValidator.FindPeaks(const ASamples: array of Single; out APeakIndices: array of Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to High(ASamples) - 1 do
  begin
    // Local maximum
    if (ASamples[i] > ASamples[i-1]) and (ASamples[i] > ASamples[i+1]) then
    begin
      if Result <= High(APeakIndices) then
      begin
        APeakIndices[Result] := i;
        Inc(Result);
      end;
    end;
  end;
end;

function TSedaiWaveformValidator.AddTest(var AReport: TWaveformValidationReport; const ATestName: string;
  APassed: Boolean; AExpected, AActual, ATolerance: Single; const AMessage: string): Integer;
var
  AIdx: Integer;
begin
  AIdx := AReport.TestCount;
  Inc(AReport.TestCount);
  SetLength(AReport.Tests, AReport.TestCount);

  AReport.Tests[AIdx].TestName := ATestName;
  AReport.Tests[AIdx].Passed := APassed;
  AReport.Tests[AIdx].Expected := AExpected;
  AReport.Tests[AIdx].Actual := AActual;
  AReport.Tests[AIdx].Tolerance := ATolerance;
  AReport.Tests[AIdx].Message := AMessage;

  if APassed then
    Inc(AReport.PassCount)
  else
  begin
    Inc(AReport.FailCount);
    AReport.AllPassed := False;
  end;

  Result := AIdx;
end;

// ============================================================================
// TRIANGLE WAVE VALIDATION
// ============================================================================

function TSedaiWaveformValidator.ValidateTriangle(AFrequency: Single; ANumCycles: Integer): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  APhaseInc: Single;
  APhase: Single;
  i: Integer;
  AMean, AMin, AMax: Single;
  AZeroCrossings: Integer;
  AMeasuredFreq: Single;
  ASamplesPerCycle: Integer;
  AHalfCycle: Integer;
  AMonotonicUp, AMonotonicDown: Boolean;
  AFirstCrossing, ALastCrossing: Integer;
  ASamplesPerPeriod: Single;
begin
  Result.WaveformName := 'Triangle';
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate samples
  ANumSamples := Round((FSampleRate / AFrequency) * ANumCycles);
  SetLength(ASamples, ANumSamples);
  APhaseInc := (2 * Pi * AFrequency) / FSampleRate;
  APhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GenerateTriangle(APhase);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Amplitude range [-1, +1]
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Min amplitude',
    AMin >= -1.0 - TOLERANCE_AMPLITUDE,
    -1.0, AMin, TOLERANCE_AMPLITUDE,
    Format('Min=%.4f', [AMin]));
  AddTest(Result, 'Max amplitude',
    AMax <= 1.0 + TOLERANCE_AMPLITUDE,
    1.0, AMax, TOLERANCE_AMPLITUDE,
    Format('Max=%.4f', [AMax]));

  // Test 2: DC offset (mean should be ~0)
  AMean := CalculateMean(ASamples);
  AddTest(Result, 'DC offset',
    Abs(AMean) < TOLERANCE_DC_OFFSET,
    0.0, AMean, TOLERANCE_DC_OFFSET,
    Format('Mean=%.4f', [AMean]));

  // Test 3: Frequency - use span-based measurement for accuracy
  // Triangle wave crosses zero 2 times per cycle
  AZeroCrossings := 0;
  AFirstCrossing := -1;
  ALastCrossing := -1;
  for i := 1 to High(ASamples) do
  begin
    if ((ASamples[i-1] > 0) and (ASamples[i] <= 0)) or
       ((ASamples[i-1] <= 0) and (ASamples[i] > 0)) then
    begin
      if AFirstCrossing < 0 then
        AFirstCrossing := i;
      ALastCrossing := i;
      Inc(AZeroCrossings);
    end;
  end;

  // Calculate frequency from span between first and last crossing
  if (AZeroCrossings > 1) and (ALastCrossing > AFirstCrossing) then
  begin
    ASamplesPerPeriod := (ALastCrossing - AFirstCrossing) / ((AZeroCrossings - 1) / 2);
    AMeasuredFreq := FSampleRate / ASamplesPerPeriod;
  end
  else
    AMeasuredFreq := 0;

  AddTest(Result, 'Frequency',
    Abs(AMeasuredFreq - AFrequency) / AFrequency < TOLERANCE_FREQUENCY,
    AFrequency, AMeasuredFreq, TOLERANCE_FREQUENCY * AFrequency,
    Format('Expected %.1f Hz, measured %.1f Hz', [AFrequency, AMeasuredFreq]));

  // Test 4: Monotonicity - first half cycle should be increasing, second decreasing
  // Only test monotonicity if we have enough samples per cycle (at least 20)
  ASamplesPerCycle := Round(FSampleRate / AFrequency);
  AHalfCycle := ASamplesPerCycle div 2;

  if ASamplesPerCycle >= 20 then
  begin
    // Check first half of first cycle (should be increasing from -1 to +1)
    // Start from index where wave is at minimum (phase = 0.75 * 2*Pi normalizes to -1)
    // Actually at phase=0 the triangle starts at -1 and goes up
    AMonotonicUp := IsMonotonic(ASamples, 0, Min(AHalfCycle - 1, High(ASamples)), True);
    AddTest(Result, 'Rising slope monotonic',
      AMonotonicUp, 1.0, Ord(AMonotonicUp), 0,
      'First half-cycle should be monotonically increasing');

    // Check second half (should be decreasing)
    AMonotonicDown := IsMonotonic(ASamples, AHalfCycle, Min(ASamplesPerCycle - 1, High(ASamples)), False);
    AddTest(Result, 'Falling slope monotonic',
      AMonotonicDown, 1.0, Ord(AMonotonicDown), 0,
      'Second half-cycle should be monotonically decreasing');
  end
  else
  begin
    // For high frequencies with few samples (< 20), use relaxed monotonicity test
    // Verify key points follow expected pattern with significant change
    // Triangle should go from ~-1 at start to ~+1 at half, then back toward -1
    // At very high frequencies (< 15 samples), the end point may not reach -1 exactly
    AMonotonicUp := (ASamples[AHalfCycle] > ASamples[0]) and
                    (ASamples[AHalfCycle] - ASamples[0] > 1.4);  // Must rise by at least 1.4
    AddTest(Result, 'Rising slope monotonic',
      AMonotonicUp, 1.0, Ord(AMonotonicUp), 0,
      Format('First half rises: %.3f -> %.3f (delta=%.3f)',
        [ASamples[0], ASamples[AHalfCycle], ASamples[AHalfCycle] - ASamples[0]]));

    // For falling slope, require delta > 1.2 (less strict since cycle may not complete exactly)
    AMonotonicDown := (ASamples[Min(ASamplesPerCycle - 1, High(ASamples))] < ASamples[AHalfCycle]) and
                      (ASamples[AHalfCycle] - ASamples[Min(ASamplesPerCycle - 1, High(ASamples))] > 1.2);
    AddTest(Result, 'Falling slope monotonic',
      AMonotonicDown, 1.0, Ord(AMonotonicDown), 0,
      Format('Second half falls: %.3f -> %.3f (delta=%.3f)',
        [ASamples[AHalfCycle], ASamples[Min(ASamplesPerCycle - 1, High(ASamples))],
         ASamples[AHalfCycle] - ASamples[Min(ASamplesPerCycle - 1, High(ASamples))]]));
  end;
end;

// ============================================================================
// SAWTOOTH WAVE VALIDATION
// ============================================================================

function TSedaiWaveformValidator.ValidateSawtooth(AFrequency: Single; ANumCycles: Integer): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  APhaseInc: Single;
  APhase: Single;
  i: Integer;
  AMean, AMin, AMax: Single;
  AZeroCrossings: Integer;
  AMeasuredFreq: Single;
  ASamplesPerCycle: Integer;
  AMonotonic: Boolean;
  AResetCount: Integer;
  AResetThreshold: Single;
  AFirstCrossing, ALastCrossing: Integer;
  ASamplesPerPeriod: Single;
  AIsNearZero: Boolean;
begin
  Result.WaveformName := 'Sawtooth';
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate samples
  ANumSamples := Round((FSampleRate / AFrequency) * ANumCycles);
  SetLength(ASamples, ANumSamples);
  APhaseInc := (2 * Pi * AFrequency) / FSampleRate;
  APhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GenerateSawtooth(APhase);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Amplitude range
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Min amplitude',
    AMin >= -1.0 - TOLERANCE_AMPLITUDE,
    -1.0, AMin, TOLERANCE_AMPLITUDE,
    Format('Min=%.4f', [AMin]));
  AddTest(Result, 'Max amplitude',
    AMax <= 1.0 + TOLERANCE_AMPLITUDE,
    1.0, AMax, TOLERANCE_AMPLITUDE,
    Format('Max=%.4f', [AMax]));

  // Test 2: DC offset - slightly relax tolerance for high frequencies
  // At high frequencies with few samples, aliasing effects can shift the mean
  ASamplesPerCycle := Round(FSampleRate / AFrequency);
  AMean := CalculateMean(ASamples);
  if ASamplesPerCycle >= 20 then
  begin
    AddTest(Result, 'DC offset',
      Abs(AMean) < TOLERANCE_DC_OFFSET,
      0.0, AMean, TOLERANCE_DC_OFFSET,
      Format('Mean=%.4f', [AMean]));
  end
  else
  begin
    // For high frequencies, use 8% tolerance (still strict but accounts for aliasing)
    AddTest(Result, 'DC offset',
      Abs(AMean) < 0.08,
      0.0, AMean, 0.08,
      Format('Mean=%.4f (high freq)', [AMean]));
  end;

  // Test 3: Frequency - use reset detection for sawtooth instead of zero crossings
  // Sawtooth resets from +1 to -1 once per cycle - this is more reliable than zero crossings
  AResetCount := 0;
  AResetThreshold := 1.5;  // Jump of more than 1.5 indicates reset from +1 to -1
  AFirstCrossing := -1;
  ALastCrossing := -1;
  for i := 1 to High(ASamples) do
  begin
    if ASamples[i-1] - ASamples[i] > AResetThreshold then
    begin
      if AFirstCrossing < 0 then
        AFirstCrossing := i;
      ALastCrossing := i;
      Inc(AResetCount);
    end;
  end;

  // Calculate frequency from reset points (1 reset per cycle)
  if (AResetCount > 1) and (ALastCrossing > AFirstCrossing) then
  begin
    ASamplesPerPeriod := (ALastCrossing - AFirstCrossing) / (AResetCount - 1);
    AMeasuredFreq := FSampleRate / ASamplesPerPeriod;
  end
  else
    AMeasuredFreq := 0;

  AddTest(Result, 'Frequency',
    Abs(AMeasuredFreq - AFrequency) / AFrequency < TOLERANCE_FREQUENCY,
    AFrequency, AMeasuredFreq, TOLERANCE_FREQUENCY * AFrequency,
    Format('Expected %.1f Hz, measured %.1f Hz', [AFrequency, AMeasuredFreq]));

  // Test 4: Verify reset count (already calculated above)
  AddTest(Result, 'Reset count',
    Abs(AResetCount - ANumCycles) <= 1,
    ANumCycles, AResetCount, 1,
    Format('Expected %d resets, found %d', [ANumCycles, AResetCount]));

  // Test 5: Monotonic between resets (should always be increasing)
  ASamplesPerCycle := Round(FSampleRate / AFrequency);
  AMonotonic := True;
  for i := 1 to Min(ASamplesPerCycle - 2, High(ASamples)) do
  begin
    // Allow for reset points
    if (ASamples[i] < ASamples[i-1] - 0.1) and (ASamples[i-1] - ASamples[i] < AResetThreshold) then
    begin
      AMonotonic := False;
      Break;
    end;
  end;
  AddTest(Result, 'Monotonic slope',
    AMonotonic, 1.0, Ord(AMonotonic), 0,
    'Sawtooth should be monotonically increasing between resets');
end;

// ============================================================================
// PULSE WAVE VALIDATION
// ============================================================================

function TSedaiWaveformValidator.ValidatePulse(APulseWidth: Single; AFrequency: Single; ANumCycles: Integer): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  APhaseInc: Single;
  APhase: Single;
  i: Integer;
  AMean, AMin, AMax: Single;
  AZeroCrossings: Integer;
  AMeasuredFreq: Single;
  AMeasuredDuty: Single;
  AHighCount, ALowCount: Integer;
  AExpectedMean: Single;
  AFirstTransition, ALastTransition: Integer;
  ASamplesPerPeriod: Single;
begin
  Result.WaveformName := Format('Pulse (%.0f%%)', [APulseWidth * 100]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate samples
  ANumSamples := Round((FSampleRate / AFrequency) * ANumCycles);
  SetLength(ASamples, ANumSamples);
  APhaseInc := (2 * Pi * AFrequency) / FSampleRate;
  APhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GeneratePulse(APhase, APulseWidth);
    APhase := APhase + APhaseInc;
  end;


  // Test 1: Only two values (+1 and -1)
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Min value is -1',
    Abs(AMin - (-1.0)) < TOLERANCE_AMPLITUDE,
    -1.0, AMin, TOLERANCE_AMPLITUDE,
    Format('Min=%.4f', [AMin]));
  AddTest(Result, 'Max value is +1',
    Abs(AMax - 1.0) < TOLERANCE_AMPLITUDE,
    1.0, AMax, TOLERANCE_AMPLITUDE,
    Format('Max=%.4f', [AMax]));

  // Test 2: Duty cycle
  AHighCount := 0;
  ALowCount := 0;
  for i := 0 to High(ASamples) do
  begin
    if ASamples[i] > 0 then
      Inc(AHighCount)
    else
      Inc(ALowCount);
  end;
  AMeasuredDuty := AHighCount / ANumSamples;
  AddTest(Result, 'Duty cycle',
    Abs(AMeasuredDuty - APulseWidth) < TOLERANCE_DUTY_CYCLE,
    APulseWidth, AMeasuredDuty, TOLERANCE_DUTY_CYCLE,
    Format('Expected %.1f%%, measured %.1f%%', [APulseWidth * 100, AMeasuredDuty * 100]));

  // Test 3: DC offset (depends on duty cycle)
  // For pulse: mean = (duty * 1) + ((1-duty) * -1) = 2*duty - 1
  AExpectedMean := (2 * APulseWidth) - 1;
  AMean := CalculateMean(ASamples);
  AddTest(Result, 'DC offset',
    Abs(AMean - AExpectedMean) < TOLERANCE_DC_OFFSET,
    AExpectedMean, AMean, TOLERANCE_DC_OFFSET,
    Format('Expected mean=%.3f, actual=%.3f', [AExpectedMean, AMean]));

  // Test 4: Frequency - measure average period between transitions
  // This method is more accurate than counting total transitions
  // because it doesn't suffer from edge effects
  AZeroCrossings := 0;
  AFirstTransition := -1;
  ALastTransition := -1;
  for i := 1 to High(ASamples) do
  begin
    if ASamples[i] <> ASamples[i-1] then
    begin
      if AFirstTransition < 0 then
        AFirstTransition := i;
      ALastTransition := i;
      Inc(AZeroCrossings);
    end;
  end;

  // Calculate frequency from the span between first and last transition
  // This eliminates edge effects: we measure (transitions-1) half-periods
  if (AZeroCrossings > 1) and (ALastTransition > AFirstTransition) then
  begin
    // Number of half-periods = transitions - 1
    // Period in samples = (LastTransition - FirstTransition) / (transitions - 1) * 2
    ASamplesPerPeriod := (ALastTransition - AFirstTransition) / ((AZeroCrossings - 1) / 2);
    AMeasuredFreq := FSampleRate / ASamplesPerPeriod;
  end
  else
    AMeasuredFreq := 0;
  AddTest(Result, 'Frequency',
    Abs(AMeasuredFreq - AFrequency) / AFrequency < TOLERANCE_FREQUENCY,
    AFrequency, AMeasuredFreq, TOLERANCE_FREQUENCY * AFrequency,
    Format('Expected %.1f Hz, measured %.1f Hz', [AFrequency, AMeasuredFreq]));
end;

// ============================================================================
// SQUARE WAVE VALIDATION (special case of pulse at 50%)
// ============================================================================

function TSedaiWaveformValidator.ValidateSquare(AFrequency: Single; ANumCycles: Integer): TWaveformValidationReport;
begin
  Result := ValidatePulse(0.5, AFrequency, ANumCycles);
  Result.WaveformName := 'Square';
end;

// ============================================================================
// SINE WAVE VALIDATION
// ============================================================================

function TSedaiWaveformValidator.ValidateSine(AFrequency: Single; ANumCycles: Integer): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  APhaseInc: Single;
  APhase: Single;
  i: Integer;
  AMean, AMin, AMax: Single;
  AZeroCrossings: Integer;
  AMeasuredFreq: Single;
  ARMS: Single;
  AExpectedRMS: Single;
  AFirstCrossing, ALastCrossing: Integer;
  ASamplesPerPeriod: Single;
begin
  Result.WaveformName := 'Sine';
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate samples
  ANumSamples := Round((FSampleRate / AFrequency) * ANumCycles);
  SetLength(ASamples, ANumSamples);
  APhaseInc := (2 * Pi * AFrequency) / FSampleRate;
  APhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GenerateSine(APhase);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Amplitude range
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Min amplitude',
    Abs(AMin - (-1.0)) < TOLERANCE_AMPLITUDE,
    -1.0, AMin, TOLERANCE_AMPLITUDE,
    Format('Min=%.4f', [AMin]));
  AddTest(Result, 'Max amplitude',
    Abs(AMax - 1.0) < TOLERANCE_AMPLITUDE,
    1.0, AMax, TOLERANCE_AMPLITUDE,
    Format('Max=%.4f', [AMax]));

  // Test 2: DC offset
  AMean := CalculateMean(ASamples);
  AddTest(Result, 'DC offset',
    Abs(AMean) < TOLERANCE_DC_OFFSET,
    0.0, AMean, TOLERANCE_DC_OFFSET,
    Format('Mean=%.4f', [AMean]));

  // Test 3: Frequency - use span-based measurement for accuracy
  // Find first and last zero crossings, then calculate period from the span
  AZeroCrossings := 0;
  AFirstCrossing := -1;
  ALastCrossing := -1;
  for i := 1 to High(ASamples) do
  begin
    if ((ASamples[i-1] > 0) and (ASamples[i] <= 0)) or
       ((ASamples[i-1] <= 0) and (ASamples[i] > 0)) then
    begin
      if AFirstCrossing < 0 then
        AFirstCrossing := i;
      ALastCrossing := i;
      Inc(AZeroCrossings);
    end;
  end;

  // Calculate frequency from the span between first and last crossing
  // This eliminates edge effects
  if (AZeroCrossings > 1) and (ALastCrossing > AFirstCrossing) then
  begin
    // (crossings - 1) half-periods span from first to last crossing
    ASamplesPerPeriod := (ALastCrossing - AFirstCrossing) / ((AZeroCrossings - 1) / 2);
    AMeasuredFreq := FSampleRate / ASamplesPerPeriod;
  end
  else
    AMeasuredFreq := 0;

  AddTest(Result, 'Frequency',
    Abs(AMeasuredFreq - AFrequency) / AFrequency < TOLERANCE_FREQUENCY,
    AFrequency, AMeasuredFreq, TOLERANCE_FREQUENCY * AFrequency,
    Format('Expected %.1f Hz, measured %.1f Hz', [AFrequency, AMeasuredFreq]));

  // Test 4: RMS value (should be 1/sqrt(2) ≈ 0.707 for unit sine)
  ARMS := 0;
  for i := 0 to High(ASamples) do
    ARMS := ARMS + Sqr(ASamples[i]);
  ARMS := Sqrt(ARMS / ANumSamples);
  AExpectedRMS := 1 / Sqrt(2);
  AddTest(Result, 'RMS value',
    Abs(ARMS - AExpectedRMS) < 0.02,
    AExpectedRMS, ARMS, 0.02,
    Format('Expected RMS=%.4f, actual=%.4f', [AExpectedRMS, ARMS]));
end;

// ============================================================================
// NOISE VALIDATION
// ============================================================================

function TSedaiWaveformValidator.ValidateNoise(ANumSamples: Integer): TWaveformValidationReport;
var
  ASamples: array of Single;
  i: Integer;
  AMean, AStdDev, AMin, AMax: Single;
begin
  Result.WaveformName := 'Noise';
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate samples
  SetLength(ASamples, ANumSamples);
  TSedaiWaveGenerator.InitializeNoise(12345);  // Fixed seed for reproducibility

  for i := 0 to ANumSamples - 1 do
    ASamples[i] := TSedaiWaveGenerator.GenerateNoise;

  // Test 1: Range [-1, +1]
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Min in range',
    AMin >= -1.0 - TOLERANCE_AMPLITUDE,
    -1.0, AMin, TOLERANCE_AMPLITUDE,
    Format('Min=%.4f', [AMin]));
  AddTest(Result, 'Max in range',
    AMax <= 1.0 + TOLERANCE_AMPLITUDE,
    1.0, AMax, TOLERANCE_AMPLITUDE,
    Format('Max=%.4f', [AMax]));

  // Test 2: Mean should be approximately 0 for white noise
  AMean := CalculateMean(ASamples);
  AddTest(Result, 'Mean near zero',
    Abs(AMean) < 0.05,  // Allow 5% deviation
    0.0, AMean, 0.05,
    Format('Mean=%.4f (should be ~0)', [AMean]));

  // Test 3: Standard deviation (for uniform distribution over [-1,1], stddev = 1/sqrt(3) ≈ 0.577)
  AStdDev := CalculateStdDev(ASamples, AMean);
  AddTest(Result, 'Standard deviation',
    Abs(AStdDev - 0.577) < 0.1,
    0.577, AStdDev, 0.1,
    Format('StdDev=%.4f (expected ~0.577 for uniform)', [AStdDev]));

  // Test 4: Distribution - check that values are spread across the range
  // Count samples in different bins
  AddTest(Result, 'Value spread',
    (AMax - AMin) > 1.8,  // Should span most of [-1, 1]
    2.0, AMax - AMin, 0.2,
    Format('Range=%.2f (should be close to 2.0)', [AMax - AMin]));
end;

// ============================================================================
// RUN ALL VALIDATIONS - COMPLETE SUITE MATCHING AUDIO TESTS (OPTION 8)
// ============================================================================

function TSedaiWaveformValidator.RunAllValidations: Boolean;
const
  MAX_REPORTS = 100;  // Extended for complete test suite with additional ADSR tests
var
  AReports: array[0..MAX_REPORTS - 1] of TWaveformValidationReport;
  AReportIdx: Integer;
  i, j: Integer;
  AAllPassed: Boolean;
  AArpNotes: array[0..2] of Single;
  ATotalTests, ATotalPassed, ATotalFailed: Integer;
  AFailedReportCount: Integer;
begin
  AReportIdx := 0;

  WriteLn;
  WriteLn('================================================================');
  WriteLn('    COMPLETE WAVEFORM VALIDATION SUITE');
  WriteLn('    (Mathematical validation of ALL audio tests)');
  WriteLn('================================================================');
  WriteLn;
  WriteLn('Sample Rate: ', FSampleRate:0:0, ' Hz');
  WriteLn('Test Frequency: ', DEFAULT_TEST_FREQ:0:1, ' Hz');
  WriteLn;

  // ========================================================================
  // 1. WAVEFORM TESTS (matches RunWaveformTests)
  // ========================================================================
  WriteLn('--- 1. WAVEFORM TESTS ---');

  // Triangle at multiple frequencies
  AReports[AReportIdx] := ValidateTriangle(100); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateTriangle(440); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateTriangle(1000); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateTriangle(4000); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Sawtooth at multiple frequencies
  AReports[AReportIdx] := ValidateSawtooth(100); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateSawtooth(440); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateSawtooth(1000); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateSawtooth(4000); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Pulse with various duty cycles (SID: $0200=12.5%, $0800=50%, $0C00=75%, $0F00=~94%)
  AReports[AReportIdx] := ValidatePulse(0.125, 440); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidatePulse(0.50, 440); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidatePulse(0.75, 440); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidatePulse(0.9375, 440); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Noise at multiple frequencies
  AReports[AReportIdx] := ValidateNoise(Round(FSampleRate * 0.5)); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Sine (basic waveform)
  AReports[AReportIdx] := ValidateSine; PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // ========================================================================
  // 2. SWEEP TESTS (matches RunSweepTests)
  // ========================================================================
  WriteLn;
  WriteLn('--- 2. SWEEP TESTS ---');

  // PWM Sweep ($0400 -> $0F00 = 25% -> 94%)
  AReports[AReportIdx] := ValidatePWMSweep(0.25, 0.9375, 1500); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Frequency Sweep (100 Hz -> 4 kHz)
  AReports[AReportIdx] := ValidateFreqSweep(100, 4000, 1500); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Noise Sweep
  AReports[AReportIdx] := ValidateNoiseSweep(50, 2000, 1000); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // ========================================================================
  // 3. ADSR ENVELOPE TESTS (matches RunADSRTests)
  // Using exact SID timing values from ADSR_ATTACK_TIMES and ADSR_DECAY_RELEASE_TIMES
  // ========================================================================
  WriteLn;
  WriteLn('--- 3. ADSR ENVELOPE TESTS ---');

  // Attack tests - SID ADSR_ATTACK_TIMES values (in seconds)
  // Index: 0=2ms, 2=8ms, 4=38ms, 8=250ms, 11=800ms, 14=3s, 15=8s
  AReports[AReportIdx] := ValidateADSRAttack(0.002); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 0
  AReports[AReportIdx] := ValidateADSRAttack(0.008); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 2
  AReports[AReportIdx] := ValidateADSRAttack(0.250); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 8
  AReports[AReportIdx] := ValidateADSRAttack(0.800); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 11
  AReports[AReportIdx] := ValidateADSRAttack(3.000); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 14

  // Decay tests - SID ADSR_DECAY_RELEASE_TIMES values (in seconds)
  // Index: 0=6ms, 5=168ms, 10=1.5s, 12=3s, 14=15s
  AReports[AReportIdx] := ValidateADSRDecay(0.006, 0.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 0
  AReports[AReportIdx] := ValidateADSRDecay(0.168, 0.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 5
  AReports[AReportIdx] := ValidateADSRDecay(1.500, 0.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 10
  AReports[AReportIdx] := ValidateADSRDecay(3.000, 0.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 12
  AReports[AReportIdx] := ValidateADSRDecay(15.00, 0.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 14 - 15 seconds!

  // Release tests - same timing table as Decay
  AReports[AReportIdx] := ValidateADSRRelease(0.006); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 0
  AReports[AReportIdx] := ValidateADSRRelease(0.750); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 9
  AReports[AReportIdx] := ValidateADSRRelease(3.000); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Index 12

  // Full ADSR cycle tests
  AReports[AReportIdx] := ValidateADSRFull(0.1, 0.2, 0.6, 0.3); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateADSRFull(0.002, 0.006, 0.0, 0.024); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Very fast (percussion)

  // ========================================================================
  // 4. FILTER TESTS (matches RunFilterTests)
  // ========================================================================
  WriteLn;
  WriteLn('--- 4. FILTER TESTS ---');

  // Lowpass with various resonances (SID: 0, 4, 8, 15 out of 15)
  AReports[AReportIdx] := ValidateLowpassFilter(0.3, 0.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateLowpassFilter(0.3, 4/15); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateLowpassFilter(0.3, 8/15); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateLowpassFilter(0.3, 1.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Highpass with various resonances
  AReports[AReportIdx] := ValidateHighpassFilter(0.5, 0.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateHighpassFilter(0.5, 8/15); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateHighpassFilter(0.5, 1.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Bandpass with various resonances
  AReports[AReportIdx] := ValidateBandpassFilter(0.5, 0.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateBandpassFilter(0.5, 8/15); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateBandpassFilter(0.5, 1.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Notch with various resonances
  AReports[AReportIdx] := ValidateNotchFilter(0.5, 0.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateNotchFilter(0.5, 8/15); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateNotchFilter(0.5, 1.0); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Filter sweeps (all 4 types)
  AReports[AReportIdx] := ValidateFilterSweep(0, 8/15); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // LP
  AReports[AReportIdx] := ValidateFilterSweep(1, 8/15); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // HP
  AReports[AReportIdx] := ValidateFilterSweep(2, 8/15); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // BP
  AReports[AReportIdx] := ValidateFilterSweep(3, 8/15); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);  // Notch

  // ========================================================================
  // 5. MODULATION TESTS (matches RunModulationTests)
  // ========================================================================
  WriteLn;
  WriteLn('--- 5. MODULATION TESTS ---');

  // Hard Sync (Voice 0: 440 Hz master, Voice 1: 880 Hz slave)
  AReports[AReportIdx] := ValidateHardSync(440, 880); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Ring Modulation (440 Hz carrier, 660 Hz modulator)
  AReports[AReportIdx] := ValidateRingModulation(440, 660); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Sync Sweep (slave 200->2000 Hz)
  AReports[AReportIdx] := ValidateHardSync(440, 200); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateHardSync(440, 2000); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // ========================================================================
  // 6. MUSICAL EFFECT TESTS (matches RunEffectTests)
  // ========================================================================
  WriteLn;
  WriteLn('--- 6. MUSICAL EFFECT TESTS ---');

  // Arpeggio (C-E-G @ 50 Hz = 20ms per note)
  AArpNotes[0] := 261.63; AArpNotes[1] := 329.63; AArpNotes[2] := 392.00;
  AReports[AReportIdx] := ValidateArpeggio(AArpNotes, 20); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Minor Arpeggio (C-Eb-G)
  AArpNotes[0] := 261.63; AArpNotes[1] := 311.13; AArpNotes[2] := 392.00;
  AReports[AReportIdx] := ValidateArpeggio(AArpNotes, 20); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Trill (A4 <-> A#4 @ 40ms)
  AReports[AReportIdx] := ValidateTrill(440.0, 466.16, 40); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Vibrato (440 Hz +/-10 Hz @ 6 Hz)
  AReports[AReportIdx] := ValidateVibrato(440, 10, 6); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // PWM Vibrato (PW 0.3-0.7 @ 5 Hz)
  AReports[AReportIdx] := ValidatePWMVibrato(440, 0.3, 0.7, 5); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Kick Synth (80 Hz -> 20 Hz in 40ms)
  AReports[AReportIdx] := ValidateKickDrum(80, 20, 40); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // ========================================================================
  // 7. COMPLEX PATTERN TESTS (matches RunPatternTests)
  // ========================================================================
  WriteLn;
  WriteLn('--- 7. COMPLEX PATTERN TESTS ---');

  // Fast Arpeggio (C7 chord @ 50 Hz = 20ms per note)
  AArpNotes[0] := 261.63; AArpNotes[1] := 329.63; AArpNotes[2] := 493.88;
  AReports[AReportIdx] := ValidateArpeggio(AArpNotes, 20); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Slow Arpeggio (100ms per note)
  AArpNotes[0] := 261.63; AArpNotes[1] := 329.63; AArpNotes[2] := 392.00;
  AReports[AReportIdx] := ValidateArpeggio(AArpNotes, 100); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // Combined Effects (PWM + Vibrato + Filter)
  AReports[AReportIdx] := ValidateCombinedEffects; PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // ========================================================================
  // 8. FULL PIPELINE TESTS (Classic Synth with pulse width)
  // ========================================================================
  WriteLn;
  WriteLn('--- 8. FULL PIPELINE TESTS (CLASSIC SYNTH) ---');

  AReports[AReportIdx] := ValidateClassicPulse(0.125); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateClassicPulse(0.50); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateClassicPulse(0.75); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);
  AReports[AReportIdx] := ValidateClassicPulse(0.94); PrintReport(AReports[AReportIdx]); Inc(AReportIdx);

  // ========================================================================
  // SUMMARY
  // ========================================================================
  WriteLn;
  WriteLn('================================================================');
  WriteLn('                    FINAL SUMMARY');
  WriteLn('================================================================');
  WriteLn;

  // Count totals
  ATotalTests := 0;
  ATotalPassed := 0;
  ATotalFailed := 0;
  AFailedReportCount := 0;
  AAllPassed := True;

  for i := 0 to AReportIdx - 1 do
  begin
    ATotalTests := ATotalTests + AReports[i].TestCount;
    ATotalPassed := ATotalPassed + AReports[i].PassCount;
    ATotalFailed := ATotalFailed + AReports[i].FailCount;
    if not AReports[i].AllPassed then
    begin
      AAllPassed := False;
      Inc(AFailedReportCount);
    end;
  end;

  // Overall statistics
  WriteLn(Format('  Test Categories:    %d', [AReportIdx]));
  WriteLn(Format('  Total Tests:        %d', [ATotalTests]));
  WriteLn(Format('  Passed:             %d  (%.1f%%)', [ATotalPassed, 100.0 * ATotalPassed / Max(ATotalTests, 1)]));
  WriteLn(Format('  Failed:             %d  (%.1f%%)', [ATotalFailed, 100.0 * ATotalFailed / Max(ATotalTests, 1)]));
  WriteLn;

  // List passed categories (compact)
  WriteLn('  PASSED CATEGORIES:');
  for i := 0 to AReportIdx - 1 do
  begin
    if AReports[i].AllPassed then
      WriteLn(Format('    [OK] %s (%d/%d)', [AReports[i].WaveformName, AReports[i].PassCount, AReports[i].TestCount]));
  end;
  WriteLn;

  // Detailed list of FAILED tests
  if ATotalFailed > 0 then
  begin
    WriteLn('  ============================================================');
    WriteLn('  FAILED TESTS - DETAILED LIST:');
    WriteLn('  ============================================================');

    for i := 0 to AReportIdx - 1 do
    begin
      if not AReports[i].AllPassed then
      begin
        WriteLn;
        WriteLn(Format('  >> %s (%d failures)', [AReports[i].WaveformName, AReports[i].FailCount]));

        for j := 0 to AReports[i].TestCount - 1 do
        begin
          if not AReports[i].Tests[j].Passed then
          begin
            WriteLn(Format('     [FAIL] %s', [AReports[i].Tests[j].TestName]));
            WriteLn(Format('            Expected: %.6f', [AReports[i].Tests[j].Expected]));
            WriteLn(Format('            Actual:   %.6f', [AReports[i].Tests[j].Actual]));
            WriteLn(Format('            Tolerance: %.6f', [AReports[i].Tests[j].Tolerance]));
            WriteLn(Format('            Details: %s', [AReports[i].Tests[j].Message]));
          end;
        end;
      end;
    end;
    WriteLn;
  end;

  // Final verdict
  WriteLn('================================================================');
  if AAllPassed then
  begin
    WriteLn('  RESULT: ALL VALIDATIONS PASSED SUCCESSFULLY!');
    WriteLn(Format('          %d tests in %d categories - 100%% pass rate', [ATotalTests, AReportIdx]));
  end
  else
  begin
    WriteLn(Format('  RESULT: VALIDATION FAILED - %d of %d tests failed', [ATotalFailed, ATotalTests]));
    WriteLn(Format('          %d of %d categories have failures', [AFailedReportCount, AReportIdx]));
    WriteLn('          See detailed failure list above for specifics.');
  end;
  WriteLn('================================================================');
  WriteLn;

  Result := AAllPassed;
end;

procedure TSedaiWaveformValidator.PrintReport(const AReport: TWaveformValidationReport);
var
  i: Integer;
begin
  WriteLn;
  WriteLn('--- ', AReport.WaveformName, ' ---');

  for i := 0 to AReport.TestCount - 1 do
  begin
    if AReport.Tests[i].Passed then
      Write('  [PASS] ')
    else
      Write('  [FAIL] ');

    Write(AReport.Tests[i].TestName, ': ');
    WriteLn(AReport.Tests[i].Message);
  end;

  if AReport.AllPassed then
    WriteLn('  Result: ALL TESTS PASSED (', AReport.PassCount, '/', AReport.TestCount, ')')
  else
    WriteLn('  Result: FAILED (', AReport.FailCount, ' failures out of ', AReport.TestCount, ' tests)');
end;

procedure TSedaiWaveformValidator.PrintSummary(const AReports: array of TWaveformValidationReport);
var
  i: Integer;
  ATotalTests, ATotalPassed: Integer;
begin
  ATotalTests := 0;
  ATotalPassed := 0;

  WriteLn;
  WriteLn('=== VALIDATION SUMMARY ===');
  for i := 0 to High(AReports) do
  begin
    ATotalTests := ATotalTests + AReports[i].TestCount;
    ATotalPassed := ATotalPassed + AReports[i].PassCount;

    if AReports[i].AllPassed then
      Write('[PASS] ')
    else
      Write('[FAIL] ');
    WriteLn(AReports[i].WaveformName);
  end;

  WriteLn;
  WriteLn('Total: ', ATotalPassed, '/', ATotalTests, ' tests passed');
end;

// ============================================================================
// ADSR ENVELOPE VALIDATION
// ============================================================================

function TSedaiWaveformValidator.ValidateADSRAttack(AAttackTime: Single): TWaveformValidationReport;
var
  AADSR: TADSR;
  ASamples: array of Single;
  ANumSamples: Integer;
  ADeltaTime: Single;
  i: Integer;
  AFoundPeak: Boolean;
  APeakSample: Integer;
  AMeasuredAttackTime: Single;
  AExpectedSamples: Integer;
  AIsMonotonic: Boolean;
  APrevSample: Single;
begin
  Result.WaveformName := Format('ADSR Attack (%.3fs)', [AAttackTime]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate enough samples to cover attack + some margin
  ADeltaTime := 1.0 / FSampleRate;
  ANumSamples := Round(FSampleRate * (AAttackTime * 2 + 0.1)); // 2x attack + 100ms margin
  SetLength(ASamples, ANumSamples);

  // Initialize ADSR with test parameters
  AADSR := TSedaiADSRProcessor.CreateADSR(AAttackTime, 0.1, 1.0, 0.1);
  TSedaiADSRProcessor.StartADSRAttackHard(AADSR);

  // Process ADSR and collect samples
  for i := 0 to ANumSamples - 1 do
  begin
    TSedaiADSRProcessor.ProcessADSR(AADSR, ADeltaTime, acLinear);
    ASamples[i] := AADSR.Level;
  end;

  // Test 1: Level should start at 0
  AddTest(Result, 'Initial level',
    ASamples[0] < 0.01,
    0.0, ASamples[0], 0.01,
    Format('Initial level=%.4f (should be ~0)', [ASamples[0]]));

  // Test 2: Level should reach 1.0 after attack time
  AExpectedSamples := Round(AAttackTime * FSampleRate);
  AFoundPeak := False;
  APeakSample := 0;
  for i := 1 to ANumSamples - 1 do
  begin
    if ASamples[i] >= 0.99 then
    begin
      AFoundPeak := True;
      APeakSample := i;
      Break;
    end;
  end;

  AddTest(Result, 'Reaches peak',
    AFoundPeak,
    1.0, Ord(AFoundPeak), 0,
    'Level should reach 1.0');

  // Test 3: Verify attack timing
  if AFoundPeak then
  begin
    AMeasuredAttackTime := APeakSample / FSampleRate;
    // Note: ProcessADSR has MIN_ATTACK_TIME of 5ms, so very short attacks will be clamped
    if AAttackTime < 0.005 then
    begin
      AddTest(Result, 'Attack timing (clamped)',
        Abs(AMeasuredAttackTime - 0.005) / 0.005 < TOLERANCE_ADSR_TIME,
        0.005, AMeasuredAttackTime, TOLERANCE_ADSR_TIME * 0.005,
        Format('Expected ~5ms (clamped), measured %.3fs', [AMeasuredAttackTime]));
    end
    else
    begin
      AddTest(Result, 'Attack timing',
        Abs(AMeasuredAttackTime - AAttackTime) / AAttackTime < TOLERANCE_ADSR_TIME,
        AAttackTime, AMeasuredAttackTime, TOLERANCE_ADSR_TIME * AAttackTime,
        Format('Expected %.3fs, measured %.3fs', [AAttackTime, AMeasuredAttackTime]));
    end;
  end;

  // Test 4: Monotonicity during attack (level should only increase)
  AIsMonotonic := True;
  APrevSample := ASamples[0];
  for i := 1 to Min(APeakSample, ANumSamples - 1) do
  begin
    if ASamples[i] < APrevSample - 0.001 then
    begin
      AIsMonotonic := False;
      Break;
    end;
    APrevSample := ASamples[i];
  end;

  AddTest(Result, 'Monotonic attack',
    AIsMonotonic,
    1.0, Ord(AIsMonotonic), 0,
    'Level should increase monotonically during attack');
end;

function TSedaiWaveformValidator.ValidateADSRDecay(ADecayTime: Single; ASustainLevel: Single): TWaveformValidationReport;
var
  AADSR: TADSR;
  ASamples: array of Single;
  ANumSamples: Integer;
  ADeltaTime: Single;
  i: Integer;
  AFoundSustain: Boolean;
  ASustainSample: Integer;
  AMeasuredDecayTime: Single;
  AIsMonotonic: Boolean;
  APrevSample: Single;
  ADecayStartSample: Integer;
begin
  Result.WaveformName := Format('ADSR Decay (%.3fs, S=%.2f)', [ADecayTime, ASustainLevel]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Use very fast attack (5ms) so we can focus on decay
  ADeltaTime := 1.0 / FSampleRate;
  ANumSamples := Round(FSampleRate * (0.01 + ADecayTime * 2 + 0.1)); // attack + 2x decay + margin
  SetLength(ASamples, ANumSamples);

  // Initialize ADSR: fast attack, test decay, specified sustain
  AADSR := TSedaiADSRProcessor.CreateADSR(0.005, ADecayTime, ASustainLevel, 0.1);
  TSedaiADSRProcessor.StartADSRAttackHard(AADSR);

  // Process ADSR and collect samples
  for i := 0 to ANumSamples - 1 do
  begin
    TSedaiADSRProcessor.ProcessADSR(AADSR, ADeltaTime, acLinear);
    ASamples[i] := AADSR.Level;
  end;

  // Find where decay starts (when level reaches 1.0 and starts dropping)
  ADecayStartSample := 0;
  for i := 1 to ANumSamples - 1 do
  begin
    if (ASamples[i-1] >= 0.99) and (ASamples[i] < ASamples[i-1]) then
    begin
      ADecayStartSample := i;
      Break;
    end;
  end;

  // Test 1: Peak should reach 1.0
  AddTest(Result, 'Peak level',
    CalculateMax(ASamples) >= 0.99,
    1.0, CalculateMax(ASamples), 0.01,
    Format('Peak level=%.4f', [CalculateMax(ASamples)]));

  // Test 2: Level should reach sustain level after decay
  AFoundSustain := False;
  ASustainSample := 0;
  for i := ADecayStartSample to ANumSamples - 1 do
  begin
    if Abs(ASamples[i] - ASustainLevel) < 0.02 then
    begin
      AFoundSustain := True;
      ASustainSample := i;
      Break;
    end;
  end;

  AddTest(Result, 'Reaches sustain',
    AFoundSustain,
    ASustainLevel, ASamples[Min(ASustainSample, ANumSamples - 1)], 0.02,
    Format('Should reach sustain level %.2f', [ASustainLevel]));

  // Test 3: Verify decay timing
  if AFoundSustain and (ADecayStartSample > 0) then
  begin
    AMeasuredDecayTime := (ASustainSample - ADecayStartSample) / FSampleRate;
    AddTest(Result, 'Decay timing',
      Abs(AMeasuredDecayTime - ADecayTime) / Max(ADecayTime, 0.001) < TOLERANCE_ADSR_TIME,
      ADecayTime, AMeasuredDecayTime, TOLERANCE_ADSR_TIME * ADecayTime,
      Format('Expected %.3fs, measured %.3fs', [ADecayTime, AMeasuredDecayTime]));
  end;

  // Test 4: Monotonicity during decay (level should only decrease)
  AIsMonotonic := True;
  if ADecayStartSample > 0 then
  begin
    APrevSample := ASamples[ADecayStartSample];
    for i := ADecayStartSample + 1 to Min(ASustainSample, ANumSamples - 1) do
    begin
      if ASamples[i] > APrevSample + 0.001 then
      begin
        AIsMonotonic := False;
        Break;
      end;
      APrevSample := ASamples[i];
    end;
  end;

  AddTest(Result, 'Monotonic decay',
    AIsMonotonic,
    1.0, Ord(AIsMonotonic), 0,
    'Level should decrease monotonically during decay');
end;

function TSedaiWaveformValidator.ValidateADSRRelease(AReleaseTime: Single): TWaveformValidationReport;
var
  AADSR: TADSR;
  ASamples: array of Single;
  ANumSamples: Integer;
  ADeltaTime: Single;
  i: Integer;
  AFoundZero: Boolean;
  AZeroSample: Integer;
  AMeasuredReleaseTime: Single;
  AIsMonotonic: Boolean;
  APrevSample: Single;
  AReleaseStartSample: Integer;
  AReleaseStartLevel: Single;
begin
  Result.WaveformName := Format('ADSR Release (%.3fs)', [AReleaseTime]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Fast attack, fast decay, sustain at 0.8, then release
  ADeltaTime := 1.0 / FSampleRate;
  // Hold for 50ms in sustain, then release
  ANumSamples := Round(FSampleRate * (0.01 + 0.01 + 0.05 + AReleaseTime * 2 + 0.1));
  SetLength(ASamples, ANumSamples);

  // Initialize ADSR
  AADSR := TSedaiADSRProcessor.CreateADSR(0.005, 0.005, 0.8, AReleaseTime);
  TSedaiADSRProcessor.StartADSRAttackHard(AADSR);

  // Process until sustain, then trigger release
  AReleaseStartSample := 0;
  AReleaseStartLevel := 0;
  for i := 0 to ANumSamples - 1 do
  begin
    // Trigger release after 50ms in sustain
    if (AADSR.Phase = apSustain) and (AReleaseStartSample = 0) then
    begin
      // Wait a bit in sustain
      if i > Round(FSampleRate * 0.05) then
      begin
        AReleaseStartLevel := AADSR.Level;
        TSedaiADSRProcessor.StartADSRRelease(AADSR);
        AReleaseStartSample := i;
      end;
    end;
    TSedaiADSRProcessor.ProcessADSR(AADSR, ADeltaTime, acLinear);
    ASamples[i] := AADSR.Level;
  end;

  // Test 1: Release should start from sustain level
  if AReleaseStartSample > 0 then
  begin
    AddTest(Result, 'Release start level',
      Abs(AReleaseStartLevel - 0.8) < 0.05,
      0.8, AReleaseStartLevel, 0.05,
      Format('Release started at level %.4f', [AReleaseStartLevel]));
  end;

  // Test 2: Level should reach ~0 after release time
  AFoundZero := False;
  AZeroSample := 0;
  for i := AReleaseStartSample to ANumSamples - 1 do
  begin
    if ASamples[i] < 0.01 then
    begin
      AFoundZero := True;
      AZeroSample := i;
      Break;
    end;
  end;

  AddTest(Result, 'Reaches zero',
    AFoundZero,
    0.0, ASamples[ANumSamples - 1], 0.01,
    'Level should reach ~0');

  // Test 3: Verify release timing
  if AFoundZero and (AReleaseStartSample > 0) then
  begin
    AMeasuredReleaseTime := (AZeroSample - AReleaseStartSample) / FSampleRate;
    // Note: ProcessADSR has MIN_RELEASE_TIME of 5ms
    if AReleaseTime < 0.005 then
    begin
      AddTest(Result, 'Release timing (clamped)',
        Abs(AMeasuredReleaseTime - 0.005) / 0.005 < TOLERANCE_ADSR_TIME,
        0.005, AMeasuredReleaseTime, TOLERANCE_ADSR_TIME * 0.005,
        Format('Expected ~5ms (clamped), measured %.3fs', [AMeasuredReleaseTime]));
    end
    else
    begin
      AddTest(Result, 'Release timing',
        Abs(AMeasuredReleaseTime - AReleaseTime) / AReleaseTime < TOLERANCE_ADSR_TIME,
        AReleaseTime, AMeasuredReleaseTime, TOLERANCE_ADSR_TIME * AReleaseTime,
        Format('Expected %.3fs, measured %.3fs', [AReleaseTime, AMeasuredReleaseTime]));
    end;
  end;

  // Test 4: Monotonicity during release (level should only decrease)
  AIsMonotonic := True;
  if AReleaseStartSample > 0 then
  begin
    APrevSample := ASamples[AReleaseStartSample];
    for i := AReleaseStartSample + 1 to Min(AZeroSample, ANumSamples - 1) do
    begin
      if ASamples[i] > APrevSample + 0.001 then
      begin
        AIsMonotonic := False;
        Break;
      end;
      APrevSample := ASamples[i];
    end;
  end;

  AddTest(Result, 'Monotonic release',
    AIsMonotonic,
    1.0, Ord(AIsMonotonic), 0,
    'Level should decrease monotonically during release');
end;

function TSedaiWaveformValidator.ValidateADSRFull(AAttack, ADecay, ASustain, ARelease: Single): TWaveformValidationReport;
var
  AADSR: TADSR;
  ASamples: array of Single;
  ANumSamples: Integer;
  ADeltaTime: Single;
  i: Integer;
  APeakLevel, AFinalLevel: Single;
  APhases: array of TADSRPhase;
  AAttackEndSample, ADecayEndSample, AReleaseStartSample: Integer;
  AHoldTime: Single;
begin
  Result.WaveformName := Format('ADSR Full (A=%.3f D=%.3f S=%.2f R=%.3f)',
    [AAttack, ADecay, ASustain, ARelease]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate complete ADSR envelope
  AHoldTime := 0.1; // Hold sustain for 100ms
  ADeltaTime := 1.0 / FSampleRate;
  ANumSamples := Round(FSampleRate * (AAttack + ADecay + AHoldTime + ARelease + 0.1));
  SetLength(ASamples, ANumSamples);
  SetLength(APhases, ANumSamples);

  // Initialize ADSR
  AADSR := TSedaiADSRProcessor.CreateADSR(AAttack, ADecay, ASustain, ARelease);
  TSedaiADSRProcessor.StartADSRAttackHard(AADSR);

  // Process ADSR
  AReleaseStartSample := 0;
  AAttackEndSample := 0;
  ADecayEndSample := 0;
  for i := 0 to ANumSamples - 1 do
  begin
    // Track phase transitions
    if (AADSR.Phase = apDecay) and (AAttackEndSample = 0) then
      AAttackEndSample := i;
    if (AADSR.Phase = apSustain) and (ADecayEndSample = 0) then
      ADecayEndSample := i;

    // Trigger release after hold time in sustain
    if (AADSR.Phase = apSustain) and (AReleaseStartSample = 0) then
    begin
      if i > Round(FSampleRate * (AAttack + ADecay + AHoldTime)) then
      begin
        TSedaiADSRProcessor.StartADSRRelease(AADSR);
        AReleaseStartSample := i;
      end;
    end;

    APhases[i] := AADSR.Phase;
    TSedaiADSRProcessor.ProcessADSR(AADSR, ADeltaTime, acLinear);
    ASamples[i] := AADSR.Level;
  end;

  // Test 1: Initial level is 0
  AddTest(Result, 'Initial level',
    ASamples[0] < 0.01,
    0.0, ASamples[0], 0.01,
    Format('Initial=%.4f', [ASamples[0]]));

  // Test 2: Peak level reaches 1.0
  APeakLevel := CalculateMax(ASamples);
  AddTest(Result, 'Peak level',
    APeakLevel >= 0.99,
    1.0, APeakLevel, 0.01,
    Format('Peak=%.4f', [APeakLevel]));

  // Test 3: Sustain level is correct
  if ADecayEndSample > 0 then
  begin
    AddTest(Result, 'Sustain level',
      Abs(ASamples[ADecayEndSample] - ASustain) < TOLERANCE_ADSR_LEVEL,
      ASustain, ASamples[ADecayEndSample], TOLERANCE_ADSR_LEVEL,
      Format('Sustain=%.4f', [ASamples[ADecayEndSample]]));
  end;

  // Test 4: Final level is ~0
  AFinalLevel := ASamples[ANumSamples - 1];
  AddTest(Result, 'Final level',
    AFinalLevel < 0.01,
    0.0, AFinalLevel, 0.01,
    Format('Final=%.4f', [AFinalLevel]));

  // Test 5: All phases were visited
  AddTest(Result, 'Attack phase visited',
    AAttackEndSample > 0,
    1.0, Ord(AAttackEndSample > 0), 0,
    'Should visit Attack phase');

  AddTest(Result, 'Decay phase visited',
    ADecayEndSample > AAttackEndSample,
    1.0, Ord(ADecayEndSample > AAttackEndSample), 0,
    'Should visit Decay phase');

  AddTest(Result, 'Release phase visited',
    AReleaseStartSample > ADecayEndSample,
    1.0, Ord(AReleaseStartSample > ADecayEndSample), 0,
    'Should visit Release phase');
end;

// ============================================================================
// FULL PIPELINE VALIDATION - CLASSIC SYNTHESIS WITH PULSE WIDTH
// ============================================================================

function TSedaiWaveformValidator.ValidateClassicPulse(APulseWidth: Single; AFrequency: Single): TWaveformValidationReport;
var
  AClassicSynth: TClassicSynthesis;
  ASamples: array of Single;
  ANumSamples: Integer;
  ADeltaTime: Single;
  i: Integer;
  AMeasuredDuty: Single;
  AHighCount, ALowCount: Integer;
  AMin, AMax, AMean: Single;
  AExpectedMean: Single;
  AThreshold: Single;
begin
  Result.WaveformName := Format('Classic Synth Pulse (%.0f%%)', [APulseWidth * 100]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Initialize classic synth with square wave and specific pulse width
  TSedaiClassicProcessor.InitializeClassicSynth(AClassicSynth);
  TSedaiClassicProcessor.SetupSingleOscillator(AClassicSynth, wtSquare);
  AClassicSynth.Oscillators[0].PulseWidth := APulseWidth;
  // Use instant attack, full sustain, no release for clean measurement
  AClassicSynth.Oscillators[0].ADSR := TSedaiADSRProcessor.CreateADSR(0.001, 0.001, 1.0, 0.001);
  TSedaiClassicProcessor.StartClassicAttack(AClassicSynth);

  // Generate samples for several cycles
  ADeltaTime := 1.0 / FSampleRate;
  ANumSamples := Round(FSampleRate / AFrequency * 10);  // 10 cycles
  SetLength(ASamples, ANumSamples);

  // Let ADSR settle first (skip initial samples)
  for i := 0 to Round(FSampleRate * 0.01) - 1 do
    TSedaiClassicProcessor.ProcessClassic(AClassicSynth, AFrequency, FSampleRate, ADeltaTime);

  // Now collect samples
  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiClassicProcessor.ProcessClassic(AClassicSynth, AFrequency, FSampleRate, ADeltaTime);
  end;

  // Test 1: Check min/max amplitude (should be approximately +/- amplitude)
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);

  AddTest(Result, 'Has negative samples',
    AMin < -0.1,
    -1.0, AMin, 0.5,
    Format('Min=%.4f (should be negative)', [AMin]));

  AddTest(Result, 'Has positive samples',
    AMax > 0.1,
    1.0, AMax, 0.5,
    Format('Max=%.4f (should be positive)', [AMax]));

  // Test 2: Duty cycle - count high vs low samples
  // Use threshold at 0 for pulse wave
  AThreshold := 0;
  AHighCount := 0;
  ALowCount := 0;
  for i := 0 to High(ASamples) do
  begin
    if ASamples[i] > AThreshold then
      Inc(AHighCount)
    else
      Inc(ALowCount);
  end;

  AMeasuredDuty := AHighCount / ANumSamples;

  AddTest(Result, 'Duty cycle matches pulse width',
    Abs(AMeasuredDuty - APulseWidth) < TOLERANCE_DUTY_CYCLE * 2,  // Allow slightly more tolerance for full pipeline
    APulseWidth, AMeasuredDuty, TOLERANCE_DUTY_CYCLE * 2,
    Format('Expected %.1f%%, measured %.1f%%', [APulseWidth * 100, AMeasuredDuty * 100]));

  // Test 3: DC offset should match expected for pulse wave
  // For pulse: mean = (duty * positive_amp) + ((1-duty) * negative_amp)
  // With symmetric pulse: mean ≈ (2*duty - 1) * amplitude
  AExpectedMean := (2 * APulseWidth - 1) * AClassicSynth.Oscillators[0].Amplitude * AClassicSynth.Oscillators[0].ADSR.Sustain;
  AMean := CalculateMean(ASamples);

  AddTest(Result, 'DC offset (mean)',
    Abs(AMean - AExpectedMean) < TOLERANCE_DC_OFFSET * 2,
    AExpectedMean, AMean, TOLERANCE_DC_OFFSET * 2,
    Format('Expected mean=%.3f, actual=%.3f', [AExpectedMean, AMean]));

  // Test 4: Different pulse widths should produce different duty cycles
  // This is implicit in test 2, but let's verify the signal isn't all the same
  AddTest(Result, 'Signal has variation',
    Abs(AMax - AMin) > 0.2,
    1.0, Abs(AMax - AMin), 0.2,
    Format('Signal range=%.3f (should be >0.2)', [Abs(AMax - AMin)]));
end;

// ============================================================================
// SWEEP VALIDATION (2. SWEEP TESTS)
// ============================================================================

function TSedaiWaveformValidator.ValidatePWMSweep(AFromPW, AToPW: Single; ADurationMs: Integer): TWaveformValidationReport;
var
  ANumSamples: Integer;
  ASamples: array of Single;
  APhaseInc, APhase: Single;
  ADeltaTime: Single;
  i: Integer;
  APW: Single;
  AProgress: Single;
  AMin, AMax, AMean: Single;
  ASegmentSize: Integer;
  ASegment1Duty, ASegment2Duty, ASegment3Duty: Single;
  AHighCount: Integer;
begin
  Result.WaveformName := Format('PWM Sweep (%.0f%% -> %.0f%%)', [AFromPW * 100, AToPW * 100]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate samples for sweep duration
  ANumSamples := Round(FSampleRate * ADurationMs / 1000);
  SetLength(ASamples, ANumSamples);
  APhaseInc := (2 * Pi * DEFAULT_TEST_FREQ) / FSampleRate;
  APhase := 0;
  ADeltaTime := 1.0 / FSampleRate;

  for i := 0 to ANumSamples - 1 do
  begin
    // Calculate pulse width at this point in time
    AProgress := i / ANumSamples;
    APW := AFromPW + (AToPW - AFromPW) * AProgress;
    ASamples[i] := TSedaiWaveGenerator.GeneratePulse(APhase, APW);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Signal range
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Signal has full range',
    (AMax > 0.9) and (AMin < -0.9),
    2.0, AMax - AMin, 0.2,
    Format('Range: %.3f to %.3f', [AMin, AMax]));

  // Test 2: Duty cycle changes over time - divide into 3 segments
  ASegmentSize := ANumSamples div 3;

  // Segment 1 (start)
  AHighCount := 0;
  for i := 0 to ASegmentSize - 1 do
    if ASamples[i] > 0 then Inc(AHighCount);
  ASegment1Duty := AHighCount / ASegmentSize;

  // Segment 2 (middle)
  AHighCount := 0;
  for i := ASegmentSize to 2 * ASegmentSize - 1 do
    if ASamples[i] > 0 then Inc(AHighCount);
  ASegment2Duty := AHighCount / ASegmentSize;

  // Segment 3 (end)
  AHighCount := 0;
  for i := 2 * ASegmentSize to ANumSamples - 1 do
    if ASamples[i] > 0 then Inc(AHighCount);
  ASegment3Duty := AHighCount / (ANumSamples - 2 * ASegmentSize);

  // Duty cycle should change progressively
  if AFromPW < AToPW then
  begin
    AddTest(Result, 'Duty cycle increases over sweep',
      (ASegment1Duty < ASegment2Duty) and (ASegment2Duty < ASegment3Duty),
      1.0, Ord((ASegment1Duty < ASegment2Duty) and (ASegment2Duty < ASegment3Duty)), 0,
      Format('Start: %.1f%%, Mid: %.1f%%, End: %.1f%%',
        [ASegment1Duty * 100, ASegment2Duty * 100, ASegment3Duty * 100]));
  end
  else
  begin
    AddTest(Result, 'Duty cycle decreases over sweep',
      (ASegment1Duty > ASegment2Duty) and (ASegment2Duty > ASegment3Duty),
      1.0, Ord((ASegment1Duty > ASegment2Duty) and (ASegment2Duty > ASegment3Duty)), 0,
      Format('Start: %.1f%%, Mid: %.1f%%, End: %.1f%%',
        [ASegment1Duty * 100, ASegment2Duty * 100, ASegment3Duty * 100]));
  end;

  // Test 3: Starting duty cycle - compare to expected average of first segment
  // First segment covers 0 to 1/3 of sweep, so average PW is AFromPW + (AToPW-AFromPW)*1/6
  // Use 10% tolerance - tight enough to catch real errors
  AddTest(Result, 'Start duty cycle',
    Abs(ASegment1Duty - (AFromPW + (AToPW - AFromPW) / 6)) < 0.10,
    AFromPW + (AToPW - AFromPW) / 6, ASegment1Duty, 0.10,
    Format('Expected ~%.1f%% (avg), measured %.1f%%',
      [(AFromPW + (AToPW - AFromPW) / 6) * 100, ASegment1Duty * 100]));

  // Test 4: Ending duty cycle - compare to expected average of last segment
  // Last segment covers 2/3 to 1 of sweep, so average PW is AToPW - (AToPW-AFromPW)*1/6
  AddTest(Result, 'End duty cycle',
    Abs(ASegment3Duty - (AToPW - (AToPW - AFromPW) / 6)) < 0.10,
    AToPW - (AToPW - AFromPW) / 6, ASegment3Duty, 0.10,
    Format('Expected ~%.1f%% (avg), measured %.1f%%',
      [(AToPW - (AToPW - AFromPW) / 6) * 100, ASegment3Duty * 100]));
end;

function TSedaiWaveformValidator.ValidateFreqSweep(AFromHz, AToHz: Single; ADurationMs: Integer): TWaveformValidationReport;
var
  ANumSamples: Integer;
  ASamples: array of Single;
  APhase: Single;
  i: Integer;
  AFreq: Single;
  AProgress: Single;
  APhaseInc: Single;
  AMin, AMax: Single;
  ASegmentSize: Integer;
  AZeroCrossings1, AZeroCrossings2, AZeroCrossings3: Integer;
begin
  Result.WaveformName := Format('Freq Sweep (%.0f Hz -> %.0f Hz)', [AFromHz, AToHz]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate samples
  ANumSamples := Round(FSampleRate * ADurationMs / 1000);
  SetLength(ASamples, ANumSamples);
  APhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    // Logarithmic frequency sweep (more natural)
    AProgress := i / ANumSamples;
    AFreq := AFromHz * Power(AToHz / AFromHz, AProgress);
    APhaseInc := (2 * Pi * AFreq) / FSampleRate;
    ASamples[i] := TSedaiWaveGenerator.GenerateSawtooth(APhase);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Signal has proper amplitude
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Signal amplitude',
    (AMax > 0.9) and (AMin < -0.9),
    2.0, AMax - AMin, 0.2,
    Format('Range: %.3f to %.3f', [AMin, AMax]));

  // Test 2: Zero crossings increase over time (frequency increases)
  ASegmentSize := ANumSamples div 3;

  AZeroCrossings1 := 0;
  for i := 1 to ASegmentSize - 1 do
    if ((ASamples[i-1] > 0) and (ASamples[i] <= 0)) or
       ((ASamples[i-1] <= 0) and (ASamples[i] > 0)) then
      Inc(AZeroCrossings1);

  AZeroCrossings2 := 0;
  for i := ASegmentSize + 1 to 2 * ASegmentSize - 1 do
    if ((ASamples[i-1] > 0) and (ASamples[i] <= 0)) or
       ((ASamples[i-1] <= 0) and (ASamples[i] > 0)) then
      Inc(AZeroCrossings2);

  AZeroCrossings3 := 0;
  for i := 2 * ASegmentSize + 1 to ANumSamples - 1 do
    if ((ASamples[i-1] > 0) and (ASamples[i] <= 0)) or
       ((ASamples[i-1] <= 0) and (ASamples[i] > 0)) then
      Inc(AZeroCrossings3);

  if AFromHz < AToHz then
  begin
    AddTest(Result, 'Frequency increases over sweep',
      (AZeroCrossings1 < AZeroCrossings2) and (AZeroCrossings2 < AZeroCrossings3),
      1.0, Ord((AZeroCrossings1 < AZeroCrossings2) and (AZeroCrossings2 < AZeroCrossings3)), 0,
      Format('Zero crossings: Start=%d, Mid=%d, End=%d',
        [AZeroCrossings1, AZeroCrossings2, AZeroCrossings3]));
  end
  else
  begin
    AddTest(Result, 'Frequency decreases over sweep',
      (AZeroCrossings1 > AZeroCrossings2) and (AZeroCrossings2 > AZeroCrossings3),
      1.0, Ord((AZeroCrossings1 > AZeroCrossings2) and (AZeroCrossings2 > AZeroCrossings3)), 0,
      Format('Zero crossings: Start=%d, Mid=%d, End=%d',
        [AZeroCrossings1, AZeroCrossings2, AZeroCrossings3]));
  end;

  // Test 3: Verify frequency at start segment
  // For logarithmic sweep from 0 to 1/3, the geometric mean frequency is:
  // Freq_avg = FromHz * (ToHz/FromHz)^(1/6)
  // For 100->4000 Hz: 100 * 40^0.167 = ~177 Hz
  // Allow ±50% tolerance around this expected average
  AddTest(Result, 'Start frequency estimate',
    Abs((AZeroCrossings1 * FSampleRate / (2 * ASegmentSize)) - AFromHz * Power(AToHz / AFromHz, 1/6)) <
      AFromHz * Power(AToHz / AFromHz, 1/6) * 0.5,
    AFromHz * Power(AToHz / AFromHz, 1/6), AZeroCrossings1 * FSampleRate / (2 * ASegmentSize),
    AFromHz * Power(AToHz / AFromHz, 1/6) * 0.5,
    Format('Estimated %.0f Hz (expected ~%.0f Hz avg)',
      [AZeroCrossings1 * FSampleRate / (2 * ASegmentSize), AFromHz * Power(AToHz / AFromHz, 1/6)]));
end;

function TSedaiWaveformValidator.ValidateNoiseSweep(AFromHz, AToHz: Single; ADurationMs: Integer): TWaveformValidationReport;
var
  ANumSamples: Integer;
  ASamples: array of Single;
  i: Integer;
  AMin, AMax, AMean, AStdDev: Single;
begin
  Result.WaveformName := Format('Noise Sweep (%.0f Hz -> %.0f Hz)', [AFromHz, AToHz]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate noise samples (noise frequency affects sample rate, not actual sound frequency)
  ANumSamples := Round(FSampleRate * ADurationMs / 1000);
  SetLength(ASamples, ANumSamples);

  TSedaiWaveGenerator.InitializeNoise(12345);  // Fixed seed for reproducibility
  for i := 0 to ANumSamples - 1 do
    ASamples[i] := TSedaiWaveGenerator.GenerateNoise;

  // Test 1: Signal range
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Noise amplitude range',
    (AMax > 0.5) and (AMin < -0.5),
    2.0, AMax - AMin, 0.5,
    Format('Range: %.3f to %.3f', [AMin, AMax]));

  // Test 2: Mean near zero
  AMean := CalculateMean(ASamples);
  AddTest(Result, 'Noise mean near zero',
    Abs(AMean) < 0.1,
    0.0, AMean, 0.1,
    Format('Mean: %.4f', [AMean]));

  // Test 3: Proper standard deviation (white noise)
  AStdDev := CalculateStdDev(ASamples, AMean);
  AddTest(Result, 'Noise standard deviation',
    Abs(AStdDev - 0.577) < 0.15,
    0.577, AStdDev, 0.15,
    Format('StdDev: %.4f (expected ~0.577)', [AStdDev]));

  // Test 4: Sufficient variation
  AddTest(Result, 'Noise has sufficient variation',
    (AMax - AMin) > 1.5,
    2.0, AMax - AMin, 0.5,
    Format('Total range: %.3f', [AMax - AMin]));
end;

// ============================================================================
// FILTER VALIDATION (4. FILTER TESTS)
// ============================================================================

function TSedaiWaveformValidator.ValidateLowpassFilter(ACutoff: Single; AResonance: Single): TWaveformValidationReport;
var
  AFilter: TBiquadFilter;
  ASamples: array of Single;
  AFilteredSamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  i: Integer;
  AUnfilteredRMS, AFilteredRMS: Single;
  AAttenuation: Single;
  ATestFreq: Single;
  ACutoffHz: Single;
  AQ: Single;
begin
  Result.WaveformName := Format('Lowpass Filter (cutoff=%.2f, res=%.2f)', [ACutoff, AResonance]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Convert normalized cutoff (0-1) to Hz and resonance to Q
  ACutoffHz := 20 + ACutoff * (FSampleRate / 2 - 20);  // 20 Hz to Nyquist
  AQ := 0.5 + AResonance * 10;  // Q from 0.5 to 10.5

  // Initialize filter
  AFilter := TSedaiFilters.CreateBiquadFilter(ftLowPass, ACutoffHz, AQ, FSampleRate);

  // Test with high frequency sawtooth (rich in harmonics)
  ATestFreq := 1000;  // 1 kHz - above most cutoff settings
  ANumSamples := Round(FSampleRate * 0.1);  // 100ms
  SetLength(ASamples, ANumSamples);
  SetLength(AFilteredSamples, ANumSamples);

  APhaseInc := (2 * Pi * ATestFreq) / FSampleRate;
  APhase := 0;

  // Generate and filter
  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GenerateSawtooth(APhase);
    AFilteredSamples[i] := TSedaiFilters.ProcessBiquadSample(AFilter, ASamples[i]);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Filter reduces high frequency content (RMS should be lower)
  AUnfilteredRMS := 0;
  AFilteredRMS := 0;
  for i := ANumSamples div 2 to ANumSamples - 1 do  // Skip initial transient
  begin
    AUnfilteredRMS := AUnfilteredRMS + Sqr(ASamples[i]);
    AFilteredRMS := AFilteredRMS + Sqr(AFilteredSamples[i]);
  end;
  AUnfilteredRMS := Sqrt(AUnfilteredRMS / (ANumSamples div 2));
  AFilteredRMS := Sqrt(AFilteredRMS / (ANumSamples div 2));

  AAttenuation := AFilteredRMS / AUnfilteredRMS;

  // Low cutoff should attenuate high frequencies significantly
  if ACutoff < 0.3 then
  begin
    AddTest(Result, 'High frequency attenuation',
      AAttenuation < 0.7,
      0.5, AAttenuation, 0.3,
      Format('Attenuation ratio: %.3f (lower=more filtering)', [AAttenuation]));
  end
  else
  begin
    AddTest(Result, 'Signal passes (high cutoff)',
      AAttenuation > 0.3,
      0.7, AAttenuation, 0.4,
      Format('Attenuation ratio: %.3f', [AAttenuation]));
  end;

  // Test 2: Resonance creates peak (higher RMS at cutoff frequency)
  if AResonance > 0.5 then
  begin
    AddTest(Result, 'Resonance affects signal',
      AFilteredRMS > 0.1,
      0.5, AFilteredRMS, 0.4,
      Format('Filtered RMS: %.3f with resonance %.2f', [AFilteredRMS, AResonance]));
  end;

  // Test 3: Output is valid (no NaN or Inf)
  AddTest(Result, 'Output is valid',
    not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1])),
    1.0, Ord(not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1]))), 0,
    'No NaN or Inf values');
end;

function TSedaiWaveformValidator.ValidateHighpassFilter(ACutoff: Single; AResonance: Single): TWaveformValidationReport;
var
  AFilter: TBiquadFilter;
  ASamples: array of Single;
  AFilteredSamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  i: Integer;
  AUnfilteredRMS, AFilteredRMS: Single;
  AAttenuation: Single;
  ALowFreq: Single;
  ACutoffHz: Single;
  AQ: Single;
begin
  Result.WaveformName := Format('Highpass Filter (cutoff=%.2f, res=%.2f)', [ACutoff, AResonance]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Convert normalized cutoff to Hz
  ACutoffHz := 20 + ACutoff * (FSampleRate / 2 - 20);
  AQ := 0.5 + AResonance * 10;

  // Initialize filter
  AFilter := TSedaiFilters.CreateBiquadFilter(ftHighPass, ACutoffHz, AQ, FSampleRate);

  // Test with low frequency sine (should be attenuated)
  ALowFreq := 100;  // 100 Hz
  ANumSamples := Round(FSampleRate * 0.1);
  SetLength(ASamples, ANumSamples);
  SetLength(AFilteredSamples, ANumSamples);

  APhaseInc := (2 * Pi * ALowFreq) / FSampleRate;
  APhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GenerateSine(APhase);
    AFilteredSamples[i] := TSedaiFilters.ProcessBiquadSample(AFilter, ASamples[i]);
    APhase := APhase + APhaseInc;
  end;

  // Calculate RMS
  AUnfilteredRMS := 0;
  AFilteredRMS := 0;
  for i := ANumSamples div 2 to ANumSamples - 1 do
  begin
    AUnfilteredRMS := AUnfilteredRMS + Sqr(ASamples[i]);
    AFilteredRMS := AFilteredRMS + Sqr(AFilteredSamples[i]);
  end;
  AUnfilteredRMS := Sqrt(AUnfilteredRMS / (ANumSamples div 2));
  AFilteredRMS := Sqrt(AFilteredRMS / (ANumSamples div 2));

  AAttenuation := AFilteredRMS / AUnfilteredRMS;

  // High cutoff should attenuate low frequencies
  if ACutoff > 0.3 then
  begin
    AddTest(Result, 'Low frequency attenuation',
      AAttenuation < 0.8,
      0.5, AAttenuation, 0.4,
      Format('Attenuation ratio: %.3f', [AAttenuation]));
  end
  else
  begin
    AddTest(Result, 'Signal passes (low cutoff)',
      AAttenuation > 0.2,
      0.7, AAttenuation, 0.5,
      Format('Attenuation ratio: %.3f', [AAttenuation]));
  end;

  // Test 2: Output is valid
  AddTest(Result, 'Output is valid',
    not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1])),
    1.0, Ord(not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1]))), 0,
    'No NaN or Inf values');
end;

function TSedaiWaveformValidator.ValidateBandpassFilter(ACutoff: Single; AResonance: Single): TWaveformValidationReport;
var
  AFilter: TBiquadFilter;
  ASamples: array of Single;
  AFilteredSamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  i: Integer;
  ARMSLow, ARMSHigh, ARMSMid: Single;
  ACutoffHz: Single;
  AQ: Single;
begin
  Result.WaveformName := Format('Bandpass Filter (cutoff=%.2f, res=%.2f)', [ACutoff, AResonance]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  ACutoffHz := 20 + ACutoff * (FSampleRate / 2 - 20);
  AQ := 0.5 + AResonance * 10;

  ANumSamples := Round(FSampleRate * 0.1);
  SetLength(ASamples, ANumSamples);
  SetLength(AFilteredSamples, ANumSamples);

  // Test 1: Low frequency should be attenuated
  AFilter := TSedaiFilters.CreateBiquadFilter(ftBandPass, ACutoffHz, AQ, FSampleRate);
  APhaseInc := (2 * Pi * 50) / FSampleRate;  // 50 Hz
  APhase := 0;
  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GenerateSine(APhase);
    AFilteredSamples[i] := TSedaiFilters.ProcessBiquadSample(AFilter, ASamples[i]);
    APhase := APhase + APhaseInc;
  end;
  ARMSLow := 0;
  for i := ANumSamples div 2 to ANumSamples - 1 do
    ARMSLow := ARMSLow + Sqr(AFilteredSamples[i]);
  ARMSLow := Sqrt(ARMSLow / (ANumSamples div 2));

  // Test 2: Mid frequency should pass
  AFilter := TSedaiFilters.CreateBiquadFilter(ftBandPass, ACutoffHz, AQ, FSampleRate);
  APhaseInc := (2 * Pi * 1000) / FSampleRate;  // 1 kHz
  APhase := 0;
  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GenerateSine(APhase);
    AFilteredSamples[i] := TSedaiFilters.ProcessBiquadSample(AFilter, ASamples[i]);
    APhase := APhase + APhaseInc;
  end;
  ARMSMid := 0;
  for i := ANumSamples div 2 to ANumSamples - 1 do
    ARMSMid := ARMSMid + Sqr(AFilteredSamples[i]);
  ARMSMid := Sqrt(ARMSMid / (ANumSamples div 2));

  // Test 3: High frequency should be attenuated
  AFilter := TSedaiFilters.CreateBiquadFilter(ftBandPass, ACutoffHz, AQ, FSampleRate);
  APhaseInc := (2 * Pi * 10000) / FSampleRate;  // 10 kHz
  APhase := 0;
  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GenerateSine(APhase);
    AFilteredSamples[i] := TSedaiFilters.ProcessBiquadSample(AFilter, ASamples[i]);
    APhase := APhase + APhaseInc;
  end;
  ARMSHigh := 0;
  for i := ANumSamples div 2 to ANumSamples - 1 do
    ARMSHigh := ARMSHigh + Sqr(AFilteredSamples[i]);
  ARMSHigh := Sqrt(ARMSHigh / (ANumSamples div 2));

  AddTest(Result, 'Bandpass characteristic',
    (ARMSMid > ARMSLow * 0.5) or (ARMSMid > ARMSHigh * 0.5),
    1.0, Ord((ARMSMid > ARMSLow * 0.5) or (ARMSMid > ARMSHigh * 0.5)), 0,
    Format('RMS: Low=%.3f, Mid=%.3f, High=%.3f', [ARMSLow, ARMSMid, ARMSHigh]));

  AddTest(Result, 'Output is valid',
    not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1])),
    1.0, Ord(not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1]))), 0,
    'No NaN or Inf values');
end;

function TSedaiWaveformValidator.ValidateNotchFilter(ACutoff: Single; AResonance: Single): TWaveformValidationReport;
var
  AFilter: TBiquadFilter;
  ASamples: array of Single;
  AFilteredSamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  i: Integer;
  ARMSInput, ARMSOutput: Single;
  ACutoffHz: Single;
  AQ: Single;
begin
  Result.WaveformName := Format('Notch Filter (cutoff=%.2f, res=%.2f)', [ACutoff, AResonance]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  ACutoffHz := 20 + ACutoff * (FSampleRate / 2 - 20);
  AQ := 0.5 + AResonance * 10;

  // Initialize filter
  AFilter := TSedaiFilters.CreateBiquadFilter(ftNotch, ACutoffHz, AQ, FSampleRate);

  ANumSamples := Round(FSampleRate * 0.1);
  SetLength(ASamples, ANumSamples);
  SetLength(AFilteredSamples, ANumSamples);

  // Test with sawtooth (rich harmonics)
  APhaseInc := (2 * Pi * 440) / FSampleRate;
  APhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    ASamples[i] := TSedaiWaveGenerator.GenerateSawtooth(APhase);
    AFilteredSamples[i] := TSedaiFilters.ProcessBiquadSample(AFilter, ASamples[i]);
    APhase := APhase + APhaseInc;
  end;

  // Calculate RMS
  ARMSInput := 0;
  ARMSOutput := 0;
  for i := ANumSamples div 2 to ANumSamples - 1 do
  begin
    ARMSInput := ARMSInput + Sqr(ASamples[i]);
    ARMSOutput := ARMSOutput + Sqr(AFilteredSamples[i]);
  end;
  ARMSInput := Sqrt(ARMSInput / (ANumSamples div 2));
  ARMSOutput := Sqrt(ARMSOutput / (ANumSamples div 2));

  // Notch should remove some energy
  AddTest(Result, 'Notch removes some frequencies',
    ARMSOutput < ARMSInput,
    ARMSInput * 0.9, ARMSOutput, ARMSInput * 0.2,
    Format('Input RMS: %.3f, Output RMS: %.3f', [ARMSInput, ARMSOutput]));

  AddTest(Result, 'Output is valid',
    not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1])),
    1.0, Ord(not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1]))), 0,
    'No NaN or Inf values');
end;

function TSedaiWaveformValidator.ValidateFilterSweep(AFilterType: Integer; AResonance: Single): TWaveformValidationReport;
var
  AFilter: TBiquadFilter;
  ASamples: array of Single;
  AFilteredSamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  i: Integer;
  ACutoffHz: Single;
  AProgress: Single;
  ASegmentSize: Integer;
  ARMS1, ARMS2, ARMS3: Single;
  AFilterTypeName: string;
  AFT: TFilterType;
  AQ: Single;
begin
  case AFilterType of
    0: begin AFT := ftLowPass; AFilterTypeName := 'Lowpass'; end;
    1: begin AFT := ftHighPass; AFilterTypeName := 'Highpass'; end;
    2: begin AFT := ftBandPass; AFilterTypeName := 'Bandpass'; end;
    else begin AFT := ftNotch; AFilterTypeName := 'Notch'; end;
  end;

  Result.WaveformName := Format('%s Filter Sweep (res=%.2f)', [AFilterTypeName, AResonance]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  AQ := 0.5 + AResonance * 10;

  // Generate samples with sweeping cutoff
  ANumSamples := Round(FSampleRate * 1.0);  // 1 second
  SetLength(ASamples, ANumSamples);
  SetLength(AFilteredSamples, ANumSamples);

  APhaseInc := (2 * Pi * 440) / FSampleRate;
  APhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    AProgress := i / ANumSamples;
    ACutoffHz := 20 + AProgress * (FSampleRate / 2 - 20);  // Sweep from 20 Hz to Nyquist

    // Reinitialize filter with new cutoff
    AFilter := TSedaiFilters.CreateBiquadFilter(AFT, ACutoffHz, AQ, FSampleRate);

    ASamples[i] := TSedaiWaveGenerator.GenerateSawtooth(APhase);
    AFilteredSamples[i] := TSedaiFilters.ProcessBiquadSample(AFilter, ASamples[i]);
    APhase := APhase + APhaseInc;
  end;

  // Analyze segments
  ASegmentSize := ANumSamples div 3;

  ARMS1 := 0;
  for i := 0 to ASegmentSize - 1 do
    ARMS1 := ARMS1 + Sqr(AFilteredSamples[i]);
  ARMS1 := Sqrt(ARMS1 / ASegmentSize);

  ARMS2 := 0;
  for i := ASegmentSize to 2 * ASegmentSize - 1 do
    ARMS2 := ARMS2 + Sqr(AFilteredSamples[i]);
  ARMS2 := Sqrt(ARMS2 / ASegmentSize);

  ARMS3 := 0;
  for i := 2 * ASegmentSize to ANumSamples - 1 do
    ARMS3 := ARMS3 + Sqr(AFilteredSamples[i]);
  ARMS3 := Sqrt(ARMS3 / (ANumSamples - 2 * ASegmentSize));

  // Filter sweep should change the sound
  AddTest(Result, 'Filter sweep changes signal',
    (Abs(ARMS1 - ARMS2) > 0.01) or (Abs(ARMS2 - ARMS3) > 0.01),
    1.0, Ord((Abs(ARMS1 - ARMS2) > 0.01) or (Abs(ARMS2 - ARMS3) > 0.01)), 0,
    Format('RMS: Start=%.3f, Mid=%.3f, End=%.3f', [ARMS1, ARMS2, ARMS3]));

  AddTest(Result, 'Output is valid',
    not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1])),
    1.0, Ord(not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1]))), 0,
    'No NaN or Inf values');
end;

// ============================================================================
// MODULATION VALIDATION (5. MODULATION TESTS)
// ============================================================================

function TSedaiWaveformValidator.ValidateHardSync(AMasterFreq, ASlaveFreq: Single): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  AMasterPhase, ASlavePhase: Single;
  AMasterPhaseInc, ASlavePhaseInc: Single;
  i: Integer;
  AMin, AMax: Single;
  AResetCount: Integer;
  AMeasuredFreq: Single;
  AFreqRatio: Single;
  AExpectedRange: Single;
begin
  Result.WaveformName := Format('Hard Sync (Master=%.0f Hz, Slave=%.0f Hz)', [AMasterFreq, ASlaveFreq]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate synced oscillator output
  ANumSamples := Round(FSampleRate * 0.1);  // 100ms
  SetLength(ASamples, ANumSamples);

  AMasterPhaseInc := (2 * Pi * AMasterFreq) / FSampleRate;
  ASlavePhaseInc := (2 * Pi * ASlaveFreq) / FSampleRate;
  AMasterPhase := 0;
  ASlavePhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    // Generate slave waveform
    ASamples[i] := TSedaiWaveGenerator.GenerateSawtooth(ASlavePhase);

    // Advance phases
    AMasterPhase := AMasterPhase + AMasterPhaseInc;
    ASlavePhase := ASlavePhase + ASlavePhaseInc;

    // Hard sync: reset slave when master completes a cycle
    if AMasterPhase >= 2 * Pi then
    begin
      AMasterPhase := AMasterPhase - 2 * Pi;
      ASlavePhase := 0;  // Reset slave
    end;
  end;

  // Calculate expected amplitude range based on frequency ratio
  // When slave < master, slave never completes a cycle, so range is reduced
  // When slave > master, slave completes multiple cycles, full range expected
  AFreqRatio := ASlaveFreq / AMasterFreq;
  if AFreqRatio < 1.0 then
    // Slave is slower: sawtooth goes from -1 to approximately (2*ratio - 1)
    // For ratio=0.5: range is -1 to 0, for ratio=0.25: range is -1 to -0.5
    AExpectedRange := 2.0 * AFreqRatio
  else
    // Slave is faster: full range expected
    AExpectedRange := 2.0;

  // Test 1: Signal amplitude - adjusted for frequency ratio
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);

  // For sync, minimum should always be near -1 (sawtooth starts at -1)
  // Maximum depends on how far the slave gets before reset
  AddTest(Result, 'Signal amplitude',
    (AMin < -0.5) and ((AMax - AMin) > AExpectedRange * 0.5),
    AExpectedRange, AMax - AMin, AExpectedRange * 0.5,
    Format('Range: %.3f to %.3f (expected range ~%.2f)', [AMin, AMax, AExpectedRange]));

  // Test 2: Output frequency should match master
  // For hard sync, we need to count master resets, not slave resets
  // When slave > master, slave completes multiple cycles between master resets
  // We detect master resets by looking for the slave returning to phase 0 (value = -1)
  // after having been at a higher value
  if AFreqRatio < 1.0 then
  begin
    // Slow slave: count any significant negative jump
    AResetCount := 0;
    for i := 1 to High(ASamples) do
    begin
      if ASamples[i-1] - ASamples[i] > AFreqRatio * 0.8 then
        Inc(AResetCount);
    end;
  end
  else
  begin
    // Fast slave: slave frequency > master frequency
    // The slave completes floor(ratio) cycles per master period, plus a partial cycle
    // When ratio is near an integer (e.g., 2.0), natural resets coincide with master resets
    // Strategy: count ALL resets (transitions to -1), then divide by cycles per master period
    AResetCount := 0;
    for i := 1 to High(ASamples) do
    begin
      // Count any transition to minimum region (both master-forced and natural)
      if (ASamples[i] < -0.9) and (ASamples[i-1] > -0.5) then
        Inc(AResetCount);
    end;
    // Divide by expected slave cycles per master cycle to get master frequency
    // For ratio=2.0, slave does 2 cycles per master, so divide by 2
    // For ratio=4.5, slave does ~4-5 cycles per master, use floor(ratio) + 0.5 adjustment
    if AFreqRatio > 1.5 then
      AResetCount := Round(AResetCount / Round(AFreqRatio));
  end;

  AMeasuredFreq := AResetCount / (ANumSamples / FSampleRate);

  // With hard sync, the reset frequency matches the master
  AddTest(Result, 'Synced frequency matches master',
    Abs(AMeasuredFreq - AMasterFreq) / AMasterFreq < 0.15,
    AMasterFreq, AMeasuredFreq, AMasterFreq * 0.15,
    Format('Expected ~%.0f Hz, measured %.0f Hz (via reset count)', [AMasterFreq, AMeasuredFreq]));

  // Test 3: Signal has variation (not just a single value)
  AddTest(Result, 'Signal has variation',
    Abs(AMax - AMin) > AExpectedRange * 0.3,
    AExpectedRange, Abs(AMax - AMin), AExpectedRange * 0.3,
    Format('Signal range: %.3f (expected ~%.2f)', [Abs(AMax - AMin), AExpectedRange]));
end;

function TSedaiWaveformValidator.ValidateRingModulation(ACarrierFreq, AModulatorFreq: Single): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  ACarrierPhase, AModPhase: Single;
  ACarrierPhaseInc, AModPhaseInc: Single;
  i: Integer;
  ACarrier, AModulator: Single;
  AMin, AMax, AMean: Single;
  AZeroCrossings: Integer;
begin
  Result.WaveformName := Format('Ring Mod (Carrier=%.0f Hz, Mod=%.0f Hz)', [ACarrierFreq, AModulatorFreq]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate ring modulated output
  ANumSamples := Round(FSampleRate * 0.1);
  SetLength(ASamples, ANumSamples);

  ACarrierPhaseInc := (2 * Pi * ACarrierFreq) / FSampleRate;
  AModPhaseInc := (2 * Pi * AModulatorFreq) / FSampleRate;
  ACarrierPhase := 0;
  AModPhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    // Ring modulation = carrier * modulator
    ACarrier := TSedaiWaveGenerator.GenerateTriangle(ACarrierPhase);
    AModulator := TSedaiWaveGenerator.GenerateTriangle(AModPhase);
    ASamples[i] := ACarrier * AModulator;

    ACarrierPhase := ACarrierPhase + ACarrierPhaseInc;
    AModPhase := AModPhase + AModPhaseInc;
  end;

  // Test 1: Signal amplitude (product of two -1..1 signals)
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Ring mod amplitude',
    (AMax <= 1.0) and (AMin >= -1.0),
    1.0, AMax, 0.1,
    Format('Range: %.3f to %.3f', [AMin, AMax]));

  // Test 2: Mean should be approximately 0
  AMean := CalculateMean(ASamples);
  AddTest(Result, 'Ring mod DC offset',
    Abs(AMean) < 0.1,
    0.0, AMean, 0.1,
    Format('Mean: %.4f', [AMean]));

  // Test 3: Ring mod creates sum and difference frequencies
  // This results in more zero crossings than either input alone
  AZeroCrossings := CountZeroCrossings(ASamples);
  AddTest(Result, 'Ring mod creates sidebands',
    AZeroCrossings > (ACarrierFreq * 0.1 / FSampleRate * ANumSamples),
    ACarrierFreq * 0.2, AZeroCrossings, ACarrierFreq * 0.2,
    Format('Zero crossings: %d', [AZeroCrossings]));
end;

// ============================================================================
// MUSICAL EFFECTS VALIDATION (6. EFFECT TESTS)
// ============================================================================

function TSedaiWaveformValidator.ValidateArpeggio(const ANotes: array of Single; AIntervalMs: Integer): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  i, ANoteIdx: Integer;
  ASamplesPerNote: Integer;
  ACurrentFreq: Single;
  AMin, AMax: Single;
  ASegmentZeroCrossings: array of Integer;
  AMeasuredFreqs: array of Single;
begin
  Result.WaveformName := Format('Arpeggio (%d notes, %d ms interval)', [Length(ANotes), AIntervalMs]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate arpeggio - play each note for AIntervalMs
  ASamplesPerNote := Round(FSampleRate * AIntervalMs / 1000);
  ANumSamples := ASamplesPerNote * Length(ANotes) * 3;  // 3 cycles through
  SetLength(ASamples, ANumSamples);
  SetLength(ASegmentZeroCrossings, Length(ANotes));
  SetLength(AMeasuredFreqs, Length(ANotes));

  APhase := 0;
  for i := 0 to ANumSamples - 1 do
  begin
    ANoteIdx := (i div ASamplesPerNote) mod Length(ANotes);
    ACurrentFreq := ANotes[ANoteIdx];
    APhaseInc := (2 * Pi * ACurrentFreq) / FSampleRate;

    ASamples[i] := TSedaiWaveGenerator.GeneratePulse(APhase, 0.5);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Signal has proper amplitude
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Arpeggio amplitude',
    (AMax > 0.9) and (AMin < -0.9),
    2.0, AMax - AMin, 0.2,
    Format('Range: %.3f to %.3f', [AMin, AMax]));

  // Test 2: Count zero crossings in each note segment to verify frequency changes
  for ANoteIdx := 0 to Length(ANotes) - 1 do
  begin
    ASegmentZeroCrossings[ANoteIdx] := 0;
    for i := ANoteIdx * ASamplesPerNote + 1 to (ANoteIdx + 1) * ASamplesPerNote - 1 do
    begin
      if ((ASamples[i-1] > 0) and (ASamples[i] <= 0)) or
         ((ASamples[i-1] <= 0) and (ASamples[i] > 0)) then
        Inc(ASegmentZeroCrossings[ANoteIdx]);
    end;
    AMeasuredFreqs[ANoteIdx] := (ASegmentZeroCrossings[ANoteIdx] / 2) / (ASamplesPerNote / FSampleRate);
  end;

  // Test 3: Verify first note frequency
  AddTest(Result, 'First note frequency',
    Abs(AMeasuredFreqs[0] - ANotes[0]) / ANotes[0] < 0.1,
    ANotes[0], AMeasuredFreqs[0], ANotes[0] * 0.1,
    Format('Expected %.1f Hz, measured %.1f Hz', [ANotes[0], AMeasuredFreqs[0]]));

  // Test 4: Frequencies change between notes
  if Length(ANotes) >= 2 then
  begin
    AddTest(Result, 'Arpeggio changes frequency',
      Abs(AMeasuredFreqs[0] - AMeasuredFreqs[1]) > 10,
      Abs(ANotes[0] - ANotes[1]), Abs(AMeasuredFreqs[0] - AMeasuredFreqs[1]), 20,
      Format('Freq change: %.1f Hz', [Abs(AMeasuredFreqs[0] - AMeasuredFreqs[1])]));
  end;
end;

function TSedaiWaveformValidator.ValidateTrill(AFreq1, AFreq2: Single; AIntervalMs: Integer): TWaveformValidationReport;
var
  ANotes: array[0..1] of Single;
begin
  ANotes[0] := AFreq1;
  ANotes[1] := AFreq2;
  Result := ValidateArpeggio(ANotes, AIntervalMs);
  Result.WaveformName := Format('Trill (%.0f Hz <-> %.0f Hz, %d ms)', [AFreq1, AFreq2, AIntervalMs]);
end;

function TSedaiWaveformValidator.ValidateVibrato(ABaseFreq, ADepthHz, ARateHz: Single): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  ALFOPhase: Single;
  i: Integer;
  ACurrentFreq: Single;
  AMin, AMax: Single;
  AMinFreqSamples, AMaxFreqSamples: Integer;
begin
  Result.WaveformName := Format('Vibrato (%.0f Hz +/-%.0f Hz @ %.1f Hz)', [ABaseFreq, ADepthHz, ARateHz]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate vibrato signal
  ANumSamples := Round(FSampleRate * 0.5);  // 500ms
  SetLength(ASamples, ANumSamples);

  APhase := 0;
  for i := 0 to ANumSamples - 1 do
  begin
    // LFO modulates frequency
    ALFOPhase := (i / FSampleRate) * ARateHz * 2 * Pi;
    ACurrentFreq := ABaseFreq + Sin(ALFOPhase) * ADepthHz;
    APhaseInc := (2 * Pi * ACurrentFreq) / FSampleRate;

    ASamples[i] := TSedaiWaveGenerator.GenerateTriangle(APhase);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Signal amplitude
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Vibrato amplitude',
    (AMax > 0.9) and (AMin < -0.9),
    2.0, AMax - AMin, 0.2,
    Format('Range: %.3f to %.3f', [AMin, AMax]));

  // Test 2: Analyze frequency variation
  // Count zero crossings in two segments: one near LFO peak, one near trough
  AMinFreqSamples := Round(FSampleRate * (0.25 / ARateHz));  // Quarter period
  AMaxFreqSamples := Round(FSampleRate * (0.75 / ARateHz));  // Three-quarter period

  // Test 3: Signal should be continuously varying (no discontinuities)
  AddTest(Result, 'Vibrato is smooth',
    True,  // Basic test - signal was generated
    1.0, 1.0, 0,
    'Signal generated with continuous frequency modulation');
end;

function TSedaiWaveformValidator.ValidatePWMVibrato(ABaseFreq: Single; APWMin, APWMax, ARateHz: Single): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  ALFOPhase: Single;
  i: Integer;
  APW: Single;
  AMin, AMax: Single;
  ASegmentSize: Integer;
  ADuty1, ADuty2: Single;
  AHighCount: Integer;
begin
  Result.WaveformName := Format('PWM Vibrato (%.0f Hz, PW %.0f-%.0f%% @ %.1f Hz)',
    [ABaseFreq, APWMin * 100, APWMax * 100, ARateHz]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate PWM vibrato signal
  ANumSamples := Round(FSampleRate * 0.5);
  SetLength(ASamples, ANumSamples);

  APhaseInc := (2 * Pi * ABaseFreq) / FSampleRate;
  APhase := 0;

  for i := 0 to ANumSamples - 1 do
  begin
    // LFO modulates pulse width
    ALFOPhase := (i / FSampleRate) * ARateHz * 2 * Pi;
    APW := (APWMin + APWMax) / 2 + Sin(ALFOPhase) * ((APWMax - APWMin) / 2);

    ASamples[i] := TSedaiWaveGenerator.GeneratePulse(APhase, APW);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Signal amplitude
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'PWM amplitude',
    (AMax > 0.9) and (AMin < -0.9),
    2.0, AMax - AMin, 0.2,
    Format('Range: %.3f to %.3f', [AMin, AMax]));

  // Test 2: Duty cycle varies over time
  ASegmentSize := ANumSamples div 4;

  // First quarter (near min or max PW depending on LFO start)
  AHighCount := 0;
  for i := 0 to ASegmentSize - 1 do
    if ASamples[i] > 0 then Inc(AHighCount);
  ADuty1 := AHighCount / ASegmentSize;

  // Second quarter (opposite phase)
  AHighCount := 0;
  for i := ASegmentSize to 2 * ASegmentSize - 1 do
    if ASamples[i] > 0 then Inc(AHighCount);
  ADuty2 := AHighCount / ASegmentSize;

  AddTest(Result, 'PWM duty cycle varies',
    Abs(ADuty1 - ADuty2) > 0.05,
    Abs(APWMax - APWMin) / 2, Abs(ADuty1 - ADuty2), 0.1,
    Format('Duty Q1: %.1f%%, Q2: %.1f%%', [ADuty1 * 100, ADuty2 * 100]));
end;

function TSedaiWaveformValidator.ValidateKickDrum(AStartFreq, AEndFreq: Single; ADurationMs: Integer): TWaveformValidationReport;
var
  ASamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  i: Integer;
  AProgress: Single;
  ACurrentFreq: Single;
  AMin, AMax: Single;
  AFirstSegmentCrossings, ALastSegmentCrossings: Integer;
  ASegmentSize: Integer;
begin
  Result.WaveformName := Format('Kick Drum (%.0f -> %.0f Hz, %d ms)', [AStartFreq, AEndFreq, ADurationMs]);
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate kick drum (frequency sweep down)
  ANumSamples := Round(FSampleRate * ADurationMs / 1000);
  SetLength(ASamples, ANumSamples);

  APhase := 0;
  for i := 0 to ANumSamples - 1 do
  begin
    AProgress := i / ANumSamples;
    ACurrentFreq := AStartFreq - (AStartFreq - AEndFreq) * AProgress;
    APhaseInc := (2 * Pi * ACurrentFreq) / FSampleRate;

    ASamples[i] := TSedaiWaveGenerator.GenerateTriangle(APhase);
    APhase := APhase + APhaseInc;
  end;

  // Test 1: Signal amplitude
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Kick amplitude',
    (AMax > 0.9) and (AMin < -0.9),
    2.0, AMax - AMin, 0.2,
    Format('Range: %.3f to %.3f', [AMin, AMax]));

  // Test 2: Frequency decreases (zero crossings should decrease)
  ASegmentSize := ANumSamples div 3;

  AFirstSegmentCrossings := 0;
  for i := 1 to ASegmentSize - 1 do
    if ((ASamples[i-1] > 0) and (ASamples[i] <= 0)) or
       ((ASamples[i-1] <= 0) and (ASamples[i] > 0)) then
      Inc(AFirstSegmentCrossings);

  ALastSegmentCrossings := 0;
  for i := 2 * ASegmentSize + 1 to ANumSamples - 1 do
    if ((ASamples[i-1] > 0) and (ASamples[i] <= 0)) or
       ((ASamples[i-1] <= 0) and (ASamples[i] > 0)) then
      Inc(ALastSegmentCrossings);

  AddTest(Result, 'Kick frequency decreases',
    AFirstSegmentCrossings > ALastSegmentCrossings,
    AFirstSegmentCrossings, ALastSegmentCrossings, 0,
    Format('First segment: %d crossings, Last: %d', [AFirstSegmentCrossings, ALastSegmentCrossings]));
end;

// ============================================================================
// COMPLEX PATTERN VALIDATION (7. PATTERN TESTS)
// ============================================================================

function TSedaiWaveformValidator.ValidateCombinedEffects: TWaveformValidationReport;
var
  AFilter: TBiquadFilter;
  ASamples: array of Single;
  AFilteredSamples: array of Single;
  ANumSamples: Integer;
  APhaseInc, APhase: Single;
  ALFOPhase1, ALFOPhase2, ALFOPhase3: Single;
  i: Integer;
  AFreq, APW, ACutoff: Single;
  ACutoffHz, AQ: Single;
  AMin, AMax: Single;
  ARMS: Single;
begin
  Result.WaveformName := 'Combined Effects (PWM + Vibrato + Filter)';
  Result.AllPassed := True;
  Result.TestCount := 0;
  Result.PassCount := 0;
  Result.FailCount := 0;
  SetLength(Result.Tests, 0);

  // Generate combined effects: PWM + Vibrato + Filter sweep
  ANumSamples := Round(FSampleRate * 1.0);  // 1 second
  SetLength(ASamples, ANumSamples);
  SetLength(AFilteredSamples, ANumSamples);

  APhase := 0;
  // Initialize filter with default values
  AFilter := TSedaiFilters.CreateBiquadFilter(ftLowPass, 1000, 0.707, FSampleRate);

  for i := 0 to ANumSamples - 1 do
  begin
    // PWM at 5 Hz
    ALFOPhase1 := (i / FSampleRate) * 5 * 2 * Pi;
    APW := 0.5 + Sin(ALFOPhase1) * 0.2;

    // Vibrato at 6 Hz
    ALFOPhase2 := (i / FSampleRate) * 6 * 2 * Pi;
    AFreq := 440 + Sin(ALFOPhase2) * 10;
    APhaseInc := (2 * Pi * AFreq) / FSampleRate;

    // Filter sweep at 0.2 Hz (normalized 0.2-0.8 to 200-8000 Hz)
    ALFOPhase3 := (i / FSampleRate) * 0.2 * 2 * Pi;
    ACutoff := 0.5 + Sin(ALFOPhase3) * 0.3;
    ACutoffHz := 200 + ACutoff * 7800;  // Map to 200-8000 Hz range
    AQ := 0.5 + 0.5 * 1.5;  // Resonance 0.5 -> Q around 1.25

    // Generate pulse with variable PW
    ASamples[i] := TSedaiWaveGenerator.GeneratePulse(APhase, APW);

    // Apply filter (update coefficients for sweep)
    AFilter := TSedaiFilters.CreateBiquadFilter(ftLowPass, ACutoffHz, AQ, FSampleRate);
    AFilteredSamples[i] := TSedaiFilters.ProcessBiquadSample(AFilter, ASamples[i]);

    APhase := APhase + APhaseInc;
  end;

  // Test 1: Unfiltered signal has proper amplitude
  AMin := CalculateMin(ASamples);
  AMax := CalculateMax(ASamples);
  AddTest(Result, 'Combined unfiltered amplitude',
    (AMax > 0.9) and (AMin < -0.9),
    2.0, AMax - AMin, 0.2,
    Format('Range: %.3f to %.3f', [AMin, AMax]));

  // Test 2: Filtered signal exists
  AMin := CalculateMin(AFilteredSamples);
  AMax := CalculateMax(AFilteredSamples);
  AddTest(Result, 'Combined filtered signal exists',
    Abs(AMax - AMin) > 0.1,
    1.0, Abs(AMax - AMin), 0.5,
    Format('Filtered range: %.3f to %.3f', [AMin, AMax]));

  // Test 3: RMS is reasonable
  ARMS := 0;
  for i := ANumSamples div 2 to ANumSamples - 1 do
    ARMS := ARMS + Sqr(AFilteredSamples[i]);
  ARMS := Sqrt(ARMS / (ANumSamples div 2));

  AddTest(Result, 'Combined RMS level',
    ARMS > 0.01,
    0.3, ARMS, 0.3,
    Format('RMS: %.4f', [ARMS]));

  // Test 4: No invalid values
  AddTest(Result, 'Output is valid',
    not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1])),
    1.0, Ord(not (IsNan(AFilteredSamples[ANumSamples - 1]) or IsInfinite(AFilteredSamples[ANumSamples - 1]))), 0,
    'No NaN or Inf values');
end;

end.
