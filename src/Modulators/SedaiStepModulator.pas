{*
 * Sedai Audio Foundation - Step Modulator
 *
 * TSedaiStepModulator provides step sequencer modulation with
 * variable step count, glide, tempo sync, and per-step gates.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiStepModulator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject;

const
  MAX_STEPS = 64;
  DEFAULT_STEPS = 16;

type
  // Step data
  TStepData = record
    Value: Single;       // -1.0 to +1.0
    Gate: Boolean;       // Gate on/off for this step
    Glide: Single;       // Glide time (0 = instant, 1 = full step duration)
    Velocity: Single;    // Optional velocity (0-1)
  end;

  // Playback direction
  TStepDirection = (
    sdForward,           // 1, 2, 3, 4, ...
    sdBackward,          // ..., 4, 3, 2, 1
    sdPingPong,          // 1, 2, 3, 4, 3, 2, 1, 2, ...
    sdRandom,            // Random step each time
    sdRandomWalk         // Random +1 or -1 from current
  );

  // Sync mode
  TStepSyncMode = (
    ssmFree,             // Free running at Hz rate
    ssmTempo,            // Synced to tempo
    ssmGate              // Advance on external trigger
  );

  { TSedaiStepModulator }
  // Step sequencer for modulation
  TSedaiStepModulator = class(TSedaiAudioObject)
  private
    // Step data
    FSteps: array[0..MAX_STEPS-1] of TStepData;
    FStepCount: Integer;

    // Current state
    FCurrentStep: Integer;
    FNextStep: Integer;
    FPhase: Double;              // Position within current step (0-1)
    FDirection: Integer;         // 1 or -1 for ping-pong

    // Output
    FCurrentValue: Single;
    FTargetValue: Single;
    FGateOutput: Boolean;
    FVelocityOutput: Single;

    // Timing
    FRate: Single;               // Hz when free running
    FTempo: Single;              // BPM
    FBeatDivision: Integer;      // Steps per beat (1, 2, 4, 8, 16)
    FSyncMode: TStepSyncMode;
    FPlayDirection: TStepDirection;

    // Options
    FGlideEnabled: Boolean;
    FLoopEnabled: Boolean;
    FRunning: Boolean;
    FOneShot: Boolean;           // Play once then stop

    // Smoothing
    FGlideRate: Single;          // Calculated glide rate

    procedure CalculateStepDuration;
    procedure AdvanceStep;
    function GetNextStepIndex: Integer;
    procedure UpdateGlideRate;

  public
    constructor Create; override;

    procedure Reset; override;

    // Set step values
    procedure SetStep(AIndex: Integer; AValue: Single; AGate: Boolean = True;
                      AGlide: Single = 0.0; AVelocity: Single = 1.0);
    function GetStep(AIndex: Integer): TStepData;

    // Preset patterns
    procedure SetAllSteps(AValue: Single);
    procedure SetRamp(AStart, AEnd: Single);
    procedure SetSine(ACycles: Integer = 1);
    procedure SetRandom(AMin, AMax: Single);

    // Playback control
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure JumpToStep(AStep: Integer);
    procedure Trigger;  // For gate sync mode

    // Process one sample
    function Process: Single;

    // Properties - Configuration
    property StepCount: Integer read FStepCount write FStepCount;
    property Rate: Single read FRate write FRate;
    property Tempo: Single read FTempo write FTempo;
    property BeatDivision: Integer read FBeatDivision write FBeatDivision;
    property SyncMode: TStepSyncMode read FSyncMode write FSyncMode;
    property PlayDirection: TStepDirection read FPlayDirection write FPlayDirection;

    // Properties - Options
    property GlideEnabled: Boolean read FGlideEnabled write FGlideEnabled;
    property LoopEnabled: Boolean read FLoopEnabled write FLoopEnabled;
    property OneShot: Boolean read FOneShot write FOneShot;

    // Properties - State
    property CurrentStep: Integer read FCurrentStep;
    property CurrentValue: Single read FCurrentValue;
    property GateOutput: Boolean read FGateOutput;
    property VelocityOutput: Single read FVelocityOutput;
    property Running: Boolean read FRunning;
    property Phase: Double read FPhase;
  end;

implementation

{ TSedaiStepModulator }

constructor TSedaiStepModulator.Create;
var
  I: Integer;
begin
  inherited Create;

  FStepCount := DEFAULT_STEPS;

  // Initialize all steps
  for I := 0 to MAX_STEPS - 1 do
  begin
    FSteps[I].Value := 0.0;
    FSteps[I].Gate := True;
    FSteps[I].Glide := 0.0;
    FSteps[I].Velocity := 1.0;
  end;

  FCurrentStep := 0;
  FNextStep := 1;
  FPhase := 0.0;
  FDirection := 1;

  FCurrentValue := 0.0;
  FTargetValue := 0.0;
  FGateOutput := False;
  FVelocityOutput := 1.0;

  FRate := 4.0;          // 4 Hz default
  FTempo := 120.0;
  FBeatDivision := 4;    // Quarter notes
  FSyncMode := ssmFree;
  FPlayDirection := sdForward;

  FGlideEnabled := True;
  FLoopEnabled := True;
  FRunning := False;
  FOneShot := False;

  FGlideRate := 0.0;
end;

procedure TSedaiStepModulator.Reset;
begin
  inherited Reset;

  FCurrentStep := 0;
  FNextStep := 1;
  FPhase := 0.0;
  FDirection := 1;
  FCurrentValue := FSteps[0].Value;
  FTargetValue := FSteps[0].Value;
  FGateOutput := False;
  FRunning := False;
end;

procedure TSedaiStepModulator.CalculateStepDuration;
begin
  // Duration is handled in Process via phase increment
end;

procedure TSedaiStepModulator.AdvanceStep;
begin
  FCurrentStep := FNextStep;
  FNextStep := GetNextStepIndex;

  // Update target value
  FTargetValue := FSteps[FCurrentStep].Value;

  // Update gate and velocity
  FGateOutput := FSteps[FCurrentStep].Gate;
  FVelocityOutput := FSteps[FCurrentStep].Velocity;

  // Calculate glide rate for this step
  UpdateGlideRate;

  // Check for one-shot end
  if FOneShot and not FLoopEnabled then
  begin
    case FPlayDirection of
      sdForward:
        if FCurrentStep >= FStepCount - 1 then
          FRunning := False;
      sdBackward:
        if FCurrentStep <= 0 then
          FRunning := False;
    end;
  end;
end;

function TSedaiStepModulator.GetNextStepIndex: Integer;
begin
  case FPlayDirection of
    sdForward:
      begin
        Result := FCurrentStep + 1;
        if Result >= FStepCount then
        begin
          if FLoopEnabled then
            Result := 0
          else
            Result := FStepCount - 1;
        end;
      end;

    sdBackward:
      begin
        Result := FCurrentStep - 1;
        if Result < 0 then
        begin
          if FLoopEnabled then
            Result := FStepCount - 1
          else
            Result := 0;
        end;
      end;

    sdPingPong:
      begin
        Result := FCurrentStep + FDirection;
        if Result >= FStepCount then
        begin
          FDirection := -1;
          Result := FStepCount - 2;
          if Result < 0 then Result := 0;
        end
        else if Result < 0 then
        begin
          FDirection := 1;
          Result := 1;
          if Result >= FStepCount then Result := 0;
        end;
      end;

    sdRandom:
      Result := Random(FStepCount);

    sdRandomWalk:
      begin
        if Random(2) = 0 then
          Result := FCurrentStep + 1
        else
          Result := FCurrentStep - 1;

        if Result >= FStepCount then Result := 0;
        if Result < 0 then Result := FStepCount - 1;
      end;

    else
      Result := FCurrentStep;
  end;
end;

procedure TSedaiStepModulator.UpdateGlideRate;
var
  StepDuration: Single;
  GlideTime: Single;
  GlideSamples: Single;
begin
  if not FGlideEnabled then
  begin
    FGlideRate := 1.0;  // Instant
    Exit;
  end;

  // Calculate step duration in samples
  case FSyncMode of
    ssmFree:
      if FRate > 0 then
        StepDuration := FSampleRate / FRate
      else
        StepDuration := FSampleRate;

    ssmTempo:
      if (FTempo > 0) and (FBeatDivision > 0) then
        StepDuration := (60.0 / FTempo) * FSampleRate / FBeatDivision
      else
        StepDuration := FSampleRate;

    else
      StepDuration := FSampleRate;
  end;

  // Glide time as fraction of step duration
  GlideTime := FSteps[FCurrentStep].Glide * StepDuration;

  if GlideTime > 1 then
  begin
    GlideSamples := GlideTime;
    FGlideRate := 1.0 / GlideSamples;
  end
  else
    FGlideRate := 1.0;  // Instant
end;

procedure TSedaiStepModulator.SetStep(AIndex: Integer; AValue: Single;
  AGate: Boolean; AGlide: Single; AVelocity: Single);
begin
  if (AIndex < 0) or (AIndex >= MAX_STEPS) then Exit;

  FSteps[AIndex].Value := AValue;
  FSteps[AIndex].Gate := AGate;
  FSteps[AIndex].Glide := AGlide;
  FSteps[AIndex].Velocity := AVelocity;
end;

function TSedaiStepModulator.GetStep(AIndex: Integer): TStepData;
begin
  if (AIndex >= 0) and (AIndex < MAX_STEPS) then
    Result := FSteps[AIndex]
  else
  begin
    Result.Value := 0.0;
    Result.Gate := False;
    Result.Glide := 0.0;
    Result.Velocity := 0.0;
  end;
end;

procedure TSedaiStepModulator.SetAllSteps(AValue: Single);
var
  I: Integer;
begin
  for I := 0 to MAX_STEPS - 1 do
    FSteps[I].Value := AValue;
end;

procedure TSedaiStepModulator.SetRamp(AStart, AEnd: Single);
var
  I: Integer;
begin
  for I := 0 to FStepCount - 1 do
    FSteps[I].Value := AStart + (AEnd - AStart) * (I / (FStepCount - 1));
end;

procedure TSedaiStepModulator.SetSine(ACycles: Integer);
var
  I: Integer;
begin
  for I := 0 to FStepCount - 1 do
    FSteps[I].Value := Sin(2.0 * PI * ACycles * I / FStepCount);
end;

procedure TSedaiStepModulator.SetRandom(AMin, AMax: Single);
var
  I: Integer;
begin
  for I := 0 to FStepCount - 1 do
    FSteps[I].Value := AMin + Random * (AMax - AMin);
end;

procedure TSedaiStepModulator.Start;
begin
  FRunning := True;
  FGateOutput := FSteps[FCurrentStep].Gate;
  FVelocityOutput := FSteps[FCurrentStep].Velocity;
end;

procedure TSedaiStepModulator.Stop;
begin
  FRunning := False;
  FGateOutput := False;
  Reset;
end;

procedure TSedaiStepModulator.Pause;
begin
  FRunning := False;
  FGateOutput := False;
end;

procedure TSedaiStepModulator.JumpToStep(AStep: Integer);
begin
  if AStep < 0 then AStep := 0;
  if AStep >= FStepCount then AStep := FStepCount - 1;

  FCurrentStep := AStep;
  FNextStep := GetNextStepIndex;
  FPhase := 0.0;
  FTargetValue := FSteps[FCurrentStep].Value;
  FGateOutput := FSteps[FCurrentStep].Gate;
  FVelocityOutput := FSteps[FCurrentStep].Velocity;
  UpdateGlideRate;
end;

procedure TSedaiStepModulator.Trigger;
begin
  if FSyncMode = ssmGate then
    AdvanceStep;
end;

function TSedaiStepModulator.Process: Single;
var
  PhaseInc: Single;
  Diff: Single;
begin
  if not FRunning then
  begin
    Result := FCurrentValue;
    Exit;
  end;

  // Calculate phase increment based on sync mode
  case FSyncMode of
    ssmFree:
      if (FSampleRate > 0) and (FRate > 0) then
        PhaseInc := FRate / FSampleRate
      else
        PhaseInc := 0;

    ssmTempo:
      if (FSampleRate > 0) and (FTempo > 0) and (FBeatDivision > 0) then
        PhaseInc := (FTempo / 60.0) * FBeatDivision / FSampleRate
      else
        PhaseInc := 0;

    ssmGate:
      PhaseInc := 0;  // Advanced by Trigger method

    else
      PhaseInc := 0;
  end;

  // Advance phase
  FPhase := FPhase + PhaseInc;

  // Check for step change
  if FPhase >= 1.0 then
  begin
    FPhase := FPhase - 1.0;
    AdvanceStep;
  end;

  // Apply glide to current value
  if FGlideEnabled and (FGlideRate < 1.0) then
  begin
    Diff := FTargetValue - FCurrentValue;
    if Abs(Diff) > 0.0001 then
      FCurrentValue := FCurrentValue + Diff * FGlideRate
    else
      FCurrentValue := FTargetValue;
  end
  else
    FCurrentValue := FTargetValue;

  Result := FCurrentValue;
end;

end.
