{*
 * Sedai Audio Foundation - FM Operator
 *
 * TSedaiFMOperator provides a complete FM synthesis operator
 * with sine oscillator, ADSR envelope, feedback, and multiple
 * modulation inputs. DX7-style 6-operator algorithms supported.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiFMOperator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiOscillator;

const
  MAX_FM_OPERATORS = 6;
  MAX_FM_MOD_INPUTS = 4;

type
  // Operator output destination
  TFMOutputMode = (
    fmOutCarrier,         // Output to audio
    fmOutModulator        // Output to modulation bus
  );

  // Envelope rate scaling
  TFMRateScaling = (
    fmrsOff,              // No rate scaling
    fmrsLinear,           // Linear scaling with key
    fmrsExponential       // Exponential scaling (DX7-like)
  );

  { TSedaiFMOperator }
  // Single FM synthesis operator
  TSedaiFMOperator = class(TSedaiSignalGenerator)
  private
    // Note: FPhase and FPhaseIncrement are inherited from TSedaiSignalGenerator

    // Frequency control
    FRatio: Double;              // Frequency ratio to base note
    FFixed: Boolean;             // Fixed frequency mode
    FFixedFreq: Single;          // Fixed frequency in Hz
    FDetune: Single;             // Fine detune in cents

    // Level
    FLevel: Single;              // Output level (0-1)
    FVelocitySens: Single;       // Velocity sensitivity (0-1)
    FCurrentLevel: Single;       // Level after velocity

    // Envelope (simplified ADSR)
    FAttackRate: Single;         // Attack rate (0-99 DX7 style)
    FDecay1Rate: Single;         // Decay 1 rate
    FDecay2Rate: Single;         // Decay 2 rate (sustain rate)
    FReleaseRate: Single;        // Release rate
    FAttackLevel: Single;        // Level after attack (usually 1.0)
    FDecay1Level: Single;        // Level after decay 1
    FSustainLevel: Single;       // Sustain level

    // Envelope state
    FEnvPhase: Integer;          // 0=idle, 1=attack, 2=decay1, 3=decay2, 4=release
    FEnvLevel: Single;           // Current envelope level
    FEnvRate: Single;            // Current rate (samples)

    // Rate scaling
    FRateScaling: TFMRateScaling;
    FRateScaleDepth: Single;

    // Level scaling (keyboard scaling)
    FLevelScaleLeft: Single;     // Left of break point
    FLevelScaleRight: Single;    // Right of break point
    FLevelScaleBreak: Integer;   // Break point (MIDI note)

    // Feedback
    FFeedbackLevel: Single;      // Feedback amount (0-1)
    FFeedbackSample: array[0..1] of Single;  // Feedback delay line

    // Modulation inputs
    FModInputs: array[0..MAX_FM_MOD_INPUTS-1] of Single;

    // Output mode
    FOutputMode: TFMOutputMode;

    // Current note
    FCurrentNote: Integer;
    FCurrentVelocity: Single;

    procedure CalculatePhaseIncrement;
    procedure UpdateEnvelopeRate;
    function GetScaledRate(ABaseRate: Single): Single;
    function GetScaledLevel: Single;

  public
    constructor Create; override;

    procedure Reset; override;

    // Note control
    procedure NoteOn(ANote: Integer; AVelocity: Single);
    procedure NoteOff;

    // Set modulation input (from other operators)
    procedure SetModInput(AIndex: Integer; AValue: Single);

    // Clear all modulation inputs
    procedure ClearModInputs;

    // Generate sample
    function GenerateSample: Single; override;

    // Properties - Frequency
    property Ratio: Double read FRatio write FRatio;
    property Fixed: Boolean read FFixed write FFixed;
    property FixedFreq: Single read FFixedFreq write FFixedFreq;
    property Detune: Single read FDetune write FDetune;

    // Properties - Level
    property Level: Single read FLevel write FLevel;
    property VelocitySensitivity: Single read FVelocitySens write FVelocitySens;

    // Properties - Envelope
    property AttackRate: Single read FAttackRate write FAttackRate;
    property Decay1Rate: Single read FDecay1Rate write FDecay1Rate;
    property Decay2Rate: Single read FDecay2Rate write FDecay2Rate;
    property ReleaseRate: Single read FReleaseRate write FReleaseRate;
    property AttackLevel: Single read FAttackLevel write FAttackLevel;
    property Decay1Level: Single read FDecay1Level write FDecay1Level;
    property SustainLevel: Single read FSustainLevel write FSustainLevel;
    property EnvelopeLevel: Single read FEnvLevel;
    property EnvelopePhase: Integer read FEnvPhase;

    // Properties - Scaling
    property RateScaling: TFMRateScaling read FRateScaling write FRateScaling;
    property RateScaleDepth: Single read FRateScaleDepth write FRateScaleDepth;
    property LevelScaleLeft: Single read FLevelScaleLeft write FLevelScaleLeft;
    property LevelScaleRight: Single read FLevelScaleRight write FLevelScaleRight;
    property LevelScaleBreak: Integer read FLevelScaleBreak write FLevelScaleBreak;

    // Properties - Feedback & Output
    property FeedbackLevel: Single read FFeedbackLevel write FFeedbackLevel;
    property OutputMode: TFMOutputMode read FOutputMode write FOutputMode;
  end;

  // DX7 Algorithm type
  TFMAlgorithm = 1..32;

  { TSedaiFMSynth }
  // Complete 6-operator FM synthesizer
  TSedaiFMSynth = class(TSedaiSignalGenerator)
  private
    FOperators: array[0..MAX_FM_OPERATORS-1] of TSedaiFMOperator;
    FAlgorithm: TFMAlgorithm;
    FFeedbackOp: Integer;        // Which operator has feedback
    FFeedbackLevel: Single;

    // Pitch envelope
    FPitchEnvLevel: Single;
    FPitchEnvRate: Single;

    // LFO
    FLFOPhase: Single;
    FLFORate: Single;
    FLFODelay: Single;
    FLFOPitchDepth: Single;
    FLFOAmpDepth: Single;

    procedure SetAlgorithm(AValue: TFMAlgorithm);
    procedure ProcessAlgorithm;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // Note control
    procedure NoteOn(ANote: Integer; AVelocity: Single);
    procedure NoteOff;

    // Generate sample
    function GenerateSample: Single; override;

    // Check if all envelopes have finished (voice can be released)
    function IsFinished: Boolean;

    // Access operators
    function GetOperator(AIndex: Integer): TSedaiFMOperator;

    // Properties
    property Algorithm: TFMAlgorithm read FAlgorithm write SetAlgorithm;
    property FeedbackOperator: Integer read FFeedbackOp write FFeedbackOp;
    property FeedbackLevel: Single read FFeedbackLevel write FFeedbackLevel;
    property LFORate: Single read FLFORate write FLFORate;
    property LFODelay: Single read FLFODelay write FLFODelay;
    property LFOPitchDepth: Single read FLFOPitchDepth write FLFOPitchDepth;
    property LFOAmpDepth: Single read FLFOAmpDepth write FLFOAmpDepth;
  end;

implementation

{ TSedaiFMOperator }

constructor TSedaiFMOperator.Create;
begin
  inherited Create;

  FPhase := 0.0;
  FPhaseIncrement := 0.0;

  FRatio := 1.0;
  FFixed := False;
  FFixedFreq := 440.0;
  FDetune := 0.0;

  FLevel := 1.0;
  FVelocitySens := 0.7;
  FCurrentLevel := 1.0;

  // Default envelope (quick attack, medium decay)
  FAttackRate := 90.0;
  FDecay1Rate := 70.0;
  FDecay2Rate := 50.0;
  FReleaseRate := 60.0;
  FAttackLevel := 1.0;
  FDecay1Level := 0.8;
  FSustainLevel := 0.7;

  FEnvPhase := 0;
  FEnvLevel := 0.0;
  FEnvRate := 0.0;

  FRateScaling := fmrsOff;
  FRateScaleDepth := 0.0;

  FLevelScaleLeft := 0.0;
  FLevelScaleRight := 0.0;
  FLevelScaleBreak := 60;

  FFeedbackLevel := 0.0;
  FFeedbackSample[0] := 0.0;
  FFeedbackSample[1] := 0.0;

  FillChar(FModInputs, SizeOf(FModInputs), 0);

  FOutputMode := fmOutCarrier;
  FCurrentNote := 60;
  FCurrentVelocity := 0.8;
end;

procedure TSedaiFMOperator.Reset;
var
  I: Integer;
begin
  inherited Reset;

  FPhase := 0.0;
  FEnvPhase := 0;
  FEnvLevel := 0.0;
  FFeedbackSample[0] := 0.0;
  FFeedbackSample[1] := 0.0;

  for I := 0 to MAX_FM_MOD_INPUTS - 1 do
    FModInputs[I] := 0.0;
end;

procedure TSedaiFMOperator.CalculatePhaseIncrement;
var
  Freq: Single;
  DetuneRatio: Single;
begin
  if FFixed then
    Freq := FFixedFreq
  else
    Freq := FFrequency * FRatio;

  // Apply detune (cents)
  DetuneRatio := Power(2.0, FDetune / 1200.0);
  Freq := Freq * DetuneRatio;

  if FSampleRate > 0 then
    FPhaseIncrement := Freq / FSampleRate
  else
    FPhaseIncrement := 0;
end;

procedure TSedaiFMOperator.UpdateEnvelopeRate;
var
  BaseRate: Single;
  Samples: Single;
begin
  case FEnvPhase of
    1: BaseRate := FAttackRate;
    2: BaseRate := FDecay1Rate;
    3: BaseRate := FDecay2Rate;
    4: BaseRate := FReleaseRate;
    else BaseRate := 0;
  end;

  // Scale rate
  BaseRate := GetScaledRate(BaseRate);

  // Convert DX7-style rate (0-99) to samples
  // Higher rate = faster = fewer samples
  if BaseRate > 0 then
  begin
    // Approximate DX7 timing: rate 99 = instant, rate 0 = ~40 seconds
    Samples := FSampleRate * Power(10.0, (99.0 - BaseRate) / 20.0) * 0.001;
    if Samples < 1 then Samples := 1;
    FEnvRate := 1.0 / Samples;
  end
  else
    FEnvRate := 0.0;
end;

function TSedaiFMOperator.GetScaledRate(ABaseRate: Single): Single;
var
  KeyOffset: Integer;
  Scale: Single;
begin
  Result := ABaseRate;

  if FRateScaling = fmrsOff then Exit;
  if FRateScaleDepth <= 0 then Exit;

  KeyOffset := FCurrentNote - 60;  // Scale from middle C

  case FRateScaling of
    fmrsLinear:
      Scale := KeyOffset * FRateScaleDepth * 0.01;

    fmrsExponential:
      Scale := KeyOffset * FRateScaleDepth * 0.02;  // Stronger effect

    else
      Scale := 0;
  end;

  Result := ABaseRate + Scale;
  if Result < 0 then Result := 0;
  if Result > 99 then Result := 99;
end;

function TSedaiFMOperator.GetScaledLevel: Single;
var
  KeyOffset: Integer;
  Scale: Single;
begin
  Result := FLevel;

  KeyOffset := FCurrentNote - FLevelScaleBreak;

  if KeyOffset < 0 then
    Scale := KeyOffset * FLevelScaleLeft * 0.01
  else
    Scale := -KeyOffset * FLevelScaleRight * 0.01;

  Result := Result + Scale;
  if Result < 0 then Result := 0;
  if Result > 1 then Result := 1;
end;

procedure TSedaiFMOperator.NoteOn(ANote: Integer; AVelocity: Single);
var
  VelFactor: Single;
begin
  FCurrentNote := ANote;
  FCurrentVelocity := AVelocity;

  // Calculate frequency from note
  FFrequency := MIDINoteToFrequency(ANote);
  CalculatePhaseIncrement;

  // Calculate velocity-scaled level
  VelFactor := 1.0 - FVelocitySens + (FVelocitySens * AVelocity);
  FCurrentLevel := GetScaledLevel * VelFactor;

  // Start envelope
  FEnvPhase := 1;  // Attack
  FEnvLevel := 0.0;
  UpdateEnvelopeRate;

  // Reset phase (optional - can set to free-running)
  FPhase := 0.0;
end;

procedure TSedaiFMOperator.NoteOff;
begin
  if FEnvPhase > 0 then
  begin
    FEnvPhase := 4;  // Release
    UpdateEnvelopeRate;
  end;
end;

procedure TSedaiFMOperator.SetModInput(AIndex: Integer; AValue: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_FM_MOD_INPUTS) then
    FModInputs[AIndex] := AValue;
end;

procedure TSedaiFMOperator.ClearModInputs;
var
  I: Integer;
begin
  for I := 0 to MAX_FM_MOD_INPUTS - 1 do
    FModInputs[I] := 0.0;
end;

function TSedaiFMOperator.GenerateSample: Single;
var
  ModSum: Single;
  FeedbackMod: Single;
  PhaseWithMod: Double;
  I: Integer;
  TargetLevel: Single;
begin
  if FEnvPhase = 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  // Sum modulation inputs
  ModSum := 0.0;
  for I := 0 to MAX_FM_MOD_INPUTS - 1 do
    ModSum := ModSum + FModInputs[I];

  // Add feedback
  if FFeedbackLevel > 0 then
  begin
    FeedbackMod := (FFeedbackSample[0] + FFeedbackSample[1]) * 0.5 * FFeedbackLevel;
    ModSum := ModSum + FeedbackMod;
  end;

  // Calculate phase with modulation
  // Modulation index scales the effect of modulators
  PhaseWithMod := FPhase + ModSum * 4.0;  // Scale factor for FM depth

  // Generate sine wave
  Result := Sin(PhaseWithMod * 2.0 * PI);

  // Apply envelope and level
  Result := Result * FEnvLevel * FCurrentLevel * FAmplitude;

  // Store for feedback
  FFeedbackSample[1] := FFeedbackSample[0];
  FFeedbackSample[0] := Result;

  // Advance phase
  FPhase := FPhase + FPhaseIncrement;
  if FPhase >= 1.0 then
    FPhase := FPhase - 1.0;

  // Process envelope
  case FEnvPhase of
    1:  // Attack
      begin
        FEnvLevel := FEnvLevel + FEnvRate;
        if FEnvLevel >= FAttackLevel then
        begin
          FEnvLevel := FAttackLevel;
          FEnvPhase := 2;  // Decay 1
          UpdateEnvelopeRate;
        end;
      end;

    2:  // Decay 1
      begin
        FEnvLevel := FEnvLevel - FEnvRate;
        if FEnvLevel <= FDecay1Level then
        begin
          FEnvLevel := FDecay1Level;
          FEnvPhase := 3;  // Decay 2 (sustain rate)
          UpdateEnvelopeRate;
        end;
      end;

    3:  // Decay 2 (slow decay during sustain)
      begin
        TargetLevel := FSustainLevel;
        if FEnvLevel > TargetLevel then
        begin
          FEnvLevel := FEnvLevel - FEnvRate;
          if FEnvLevel < TargetLevel then
            FEnvLevel := TargetLevel;
        end;
      end;

    4:  // Release
      begin
        FEnvLevel := FEnvLevel - FEnvRate;
        if FEnvLevel <= 0.0 then
        begin
          FEnvLevel := 0.0;
          FEnvPhase := 0;  // Idle
        end;
      end;
  end;
end;

{ TSedaiFMSynth }

constructor TSedaiFMSynth.Create;
var
  I: Integer;
begin
  inherited Create;

  for I := 0 to MAX_FM_OPERATORS - 1 do
    FOperators[I] := TSedaiFMOperator.Create;

  FAlgorithm := 1;
  FFeedbackOp := 5;  // Op 6 (0-indexed as 5)
  FFeedbackLevel := 0.0;

  FPitchEnvLevel := 0.0;
  FPitchEnvRate := 0.0;

  FLFOPhase := 0.0;
  FLFORate := 5.0;
  FLFODelay := 0.0;
  FLFOPitchDepth := 0.0;
  FLFOAmpDepth := 0.0;

  // Default ratios for a simple FM sound
  FOperators[0].Ratio := 1.0;    // Carrier
  FOperators[1].Ratio := 1.0;    // Modulator
  FOperators[2].Ratio := 2.0;
  FOperators[3].Ratio := 3.0;
  FOperators[4].Ratio := 4.0;
  FOperators[5].Ratio := 5.0;
end;

destructor TSedaiFMSynth.Destroy;
var
  I: Integer;
begin
  for I := 0 to MAX_FM_OPERATORS - 1 do
    FOperators[I].Free;

  inherited Destroy;
end;

procedure TSedaiFMSynth.Reset;
var
  I: Integer;
begin
  inherited Reset;

  for I := 0 to MAX_FM_OPERATORS - 1 do
    FOperators[I].Reset;

  FLFOPhase := 0.0;
end;

procedure TSedaiFMSynth.SetAlgorithm(AValue: TFMAlgorithm);
begin
  if AValue < 1 then AValue := 1;
  if AValue > 32 then AValue := 32;
  FAlgorithm := AValue;
end;

procedure TSedaiFMSynth.NoteOn(ANote: Integer; AVelocity: Single);
var
  I: Integer;
begin
  for I := 0 to MAX_FM_OPERATORS - 1 do
  begin
    FOperators[I].SetSampleRate(FSampleRate);
    FOperators[I].NoteOn(ANote, AVelocity);
  end;
end;

procedure TSedaiFMSynth.NoteOff;
var
  I: Integer;
begin
  for I := 0 to MAX_FM_OPERATORS - 1 do
    FOperators[I].NoteOff;
end;

function TSedaiFMSynth.GetOperator(AIndex: Integer): TSedaiFMOperator;
begin
  if (AIndex >= 0) and (AIndex < MAX_FM_OPERATORS) then
    Result := FOperators[AIndex]
  else
    Result := nil;
end;

function TSedaiFMSynth.GenerateSample: Single;
var
  I: Integer;
  Output: Single;
  Op: array[0..5] of Single;
  AllIdle: Boolean;
begin
  // Clear modulation inputs
  for I := 0 to MAX_FM_OPERATORS - 1 do
    FOperators[I].ClearModInputs;

  // Set feedback
  if (FFeedbackOp >= 0) and (FFeedbackOp < MAX_FM_OPERATORS) then
    FOperators[FFeedbackOp].FeedbackLevel := FFeedbackLevel;

  // Initialize output samples
  for I := 0 to 5 do
    Op[I] := 0.0;

  Output := 0.0;

  // DX7 Algorithm implementations
  // Each algorithm generates modulators first, sets up modulation, then generates carriers
  case FAlgorithm of
    1:  // 6->5->4->3->2->1 (serial chain, single carrier)
      begin
        Op[5] := FOperators[5].GenerateSample;
        FOperators[4].SetModInput(0, Op[5]);
        Op[4] := FOperators[4].GenerateSample;
        FOperators[3].SetModInput(0, Op[4]);
        Op[3] := FOperators[3].GenerateSample;
        FOperators[2].SetModInput(0, Op[3]);
        Op[2] := FOperators[2].GenerateSample;
        FOperators[1].SetModInput(0, Op[2]);
        Op[1] := FOperators[1].GenerateSample;
        FOperators[0].SetModInput(0, Op[1]);
        Output := FOperators[0].GenerateSample;
      end;

    2:  // Similar to 1 but with split modulation
      begin
        Op[5] := FOperators[5].GenerateSample;
        FOperators[4].SetModInput(0, Op[5]);
        Op[4] := FOperators[4].GenerateSample;
        FOperators[3].SetModInput(0, Op[4]);
        Op[3] := FOperators[3].GenerateSample;
        FOperators[2].SetModInput(0, Op[3]);
        FOperators[1].SetModInput(0, Op[3]);
        Op[2] := FOperators[2].GenerateSample;
        Op[1] := FOperators[1].GenerateSample;
        FOperators[0].SetModInput(0, Op[2]);
        FOperators[0].SetModInput(1, Op[1]);
        Output := FOperators[0].GenerateSample;
      end;

    5:  // 6->5, 4->3, 2 all to 1 (single carrier)
      begin
        Op[5] := FOperators[5].GenerateSample;
        FOperators[4].SetModInput(0, Op[5]);
        Op[4] := FOperators[4].GenerateSample;

        Op[3] := FOperators[3].GenerateSample;
        FOperators[2].SetModInput(0, Op[3]);
        Op[2] := FOperators[2].GenerateSample;

        Op[1] := FOperators[1].GenerateSample;

        FOperators[0].SetModInput(0, Op[4]);
        FOperators[0].SetModInput(1, Op[2]);
        FOperators[0].SetModInput(2, Op[1]);
        Output := FOperators[0].GenerateSample;
      end;

    7:  // 6->5, 4->3, 2->1 (three carriers: 1, 3, 5)
      begin
        Op[5] := FOperators[5].GenerateSample;
        FOperators[4].SetModInput(0, Op[5]);
        Op[4] := FOperators[4].GenerateSample;

        Op[3] := FOperators[3].GenerateSample;
        FOperators[2].SetModInput(0, Op[3]);
        Op[2] := FOperators[2].GenerateSample;

        Op[1] := FOperators[1].GenerateSample;
        FOperators[0].SetModInput(0, Op[1]);
        Op[0] := FOperators[0].GenerateSample;

        Output := (Op[0] + Op[2] + Op[4]) / 3.0;
      end;

    32: // All carriers (additive synthesis)
      begin
        for I := 0 to MAX_FM_OPERATORS - 1 do
          Op[I] := FOperators[I].GenerateSample;
        for I := 0 to MAX_FM_OPERATORS - 1 do
          Output := Output + Op[I];
        Output := Output / MAX_FM_OPERATORS;
      end;

    else
      // Default: simple 2->1 FM
      begin
        Op[1] := FOperators[1].GenerateSample;
        FOperators[0].SetModInput(0, Op[1]);
        Output := FOperators[0].GenerateSample;
      end;
  end;

  Result := Output * FAmplitude;

  // Advance LFO
  if FSampleRate > 0 then
  begin
    FLFOPhase := FLFOPhase + FLFORate / FSampleRate;
    if FLFOPhase >= 1.0 then
      FLFOPhase := FLFOPhase - 1.0;
  end;
end;

function TSedaiFMSynth.IsFinished: Boolean;
var
  I: Integer;
begin
  // Check if all operators have finished their envelopes
  for I := 0 to MAX_FM_OPERATORS - 1 do
  begin
    if FOperators[I].EnvelopePhase <> 0 then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TSedaiFMSynth.ProcessAlgorithm;
begin
  // This method is kept for compatibility but functionality moved to GenerateSample
end;

end.
