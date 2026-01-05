{*
 * Sedai Audio Foundation - Phaser
 *
 * TSedaiPhaser provides classic phaser effect using cascaded
 * allpass filters with LFO modulation.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiPhaser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiEffect;

const
  MAX_PHASER_STAGES = 12;

type
  { TSedaiPhaser }
  // Multi-stage phaser effect
  TSedaiPhaser = class(TSedaiEffect)
  private
    FStages: Integer;             // Number of allpass stages (2-12, even)
    FRate: Single;                // LFO rate in Hz
    FDepth: Single;               // Modulation depth (0-1)
    FFreqMin: Single;             // Minimum frequency in Hz
    FFreqMax: Single;             // Maximum frequency in Hz
    FFeedback: Single;            // Feedback amount (-1 to +1)
    FMix: Single;                 // Dry/wet mix
    FStereoPhase: Single;         // Stereo LFO phase offset (0-180)

    // Allpass filter state (stereo)
    FAllpassState: array[0..1, 0..MAX_PHASER_STAGES-1] of Single;

    // LFO state
    FLFOPhase: array[0..1] of Single;

    // Feedback state
    FFeedbackSample: array[0..1] of Single;

    procedure SetStages(AValue: Integer);
    procedure SetRate(AValue: Single);
    procedure SetFreqMin(AValue: Single);
    procedure SetFreqMax(AValue: Single);
    procedure SetStereoPhase(AValue: Single);

    function ProcessAllpass(AInput, ACoeff: Single; AChannel, AStage: Integer): Single;

  public
    constructor Create; override;

    procedure Reset; override;

    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    property Stages: Integer read FStages write SetStages;
    property Rate: Single read FRate write SetRate;
    property Depth: Single read FDepth write FDepth;
    property FreqMin: Single read FFreqMin write SetFreqMin;
    property FreqMax: Single read FFreqMax write SetFreqMax;
    property Feedback: Single read FFeedback write FFeedback;
    property Mix: Single read FMix write FMix;
    property StereoPhase: Single read FStereoPhase write SetStereoPhase;
  end;

implementation

{ TSedaiPhaser }

constructor TSedaiPhaser.Create;
var
  I, J: Integer;
begin
  inherited Create;

  FStages := 6;
  FRate := 0.5;
  FDepth := 0.7;
  FFreqMin := 200.0;
  FFreqMax := 3000.0;
  FFeedback := 0.5;
  FMix := 0.5;
  FStereoPhase := 90.0;

  FLFOPhase[0] := 0.0;
  FLFOPhase[1] := 0.25;

  FFeedbackSample[0] := 0.0;
  FFeedbackSample[1] := 0.0;

  // Clear allpass state
  for I := 0 to 1 do
    for J := 0 to MAX_PHASER_STAGES - 1 do
      FAllpassState[I, J] := 0.0;
end;

procedure TSedaiPhaser.Reset;
var
  I, J: Integer;
begin
  inherited Reset;

  FLFOPhase[0] := 0.0;
  FLFOPhase[1] := FStereoPhase / 360.0;

  FFeedbackSample[0] := 0.0;
  FFeedbackSample[1] := 0.0;

  for I := 0 to 1 do
    for J := 0 to MAX_PHASER_STAGES - 1 do
      FAllpassState[I, J] := 0.0;
end;

procedure TSedaiPhaser.SetStages(AValue: Integer);
begin
  // Force even number of stages
  if AValue < 2 then AValue := 2;
  if AValue > MAX_PHASER_STAGES then AValue := MAX_PHASER_STAGES;
  FStages := (AValue div 2) * 2;  // Make even
end;

procedure TSedaiPhaser.SetRate(AValue: Single);
begin
  if AValue < 0.01 then AValue := 0.01;
  if AValue > 10.0 then AValue := 10.0;
  FRate := AValue;
end;

procedure TSedaiPhaser.SetFreqMin(AValue: Single);
begin
  if AValue < 20.0 then AValue := 20.0;
  if AValue > 5000.0 then AValue := 5000.0;
  FFreqMin := AValue;
end;

procedure TSedaiPhaser.SetFreqMax(AValue: Single);
begin
  if AValue < 100.0 then AValue := 100.0;
  if AValue > 20000.0 then AValue := 20000.0;
  FFreqMax := AValue;
end;

procedure TSedaiPhaser.SetStereoPhase(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 180.0 then AValue := 180.0;
  FStereoPhase := AValue;
end;

function TSedaiPhaser.ProcessAllpass(AInput, ACoeff: Single; AChannel, AStage: Integer): Single;
var
  Output: Single;
begin
  // First-order allpass filter
  // y[n] = -x[n] + x[n-1] + coeff * y[n-1]
  // Simplified: y[n] = coeff * (y[n-1] - x[n]) + x[n-1]

  Output := ACoeff * (FAllpassState[AChannel, AStage] - AInput) + AInput;
  FAllpassState[AChannel, AStage] := Output;
  Result := Output;
end;

procedure TSedaiPhaser.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I, S: Integer;
  Left, Right: Single;
  PhasedL, PhasedR: Single;
  LFOValueL, LFOValueR: Single;
  FreqL, FreqR: Single;
  CoeffL, CoeffR: Single;
  PhaseInc: Single;
  FreqRange: Single;
begin
  PhaseInc := FRate / FSampleRate;
  FreqRange := FFreqMax - FFreqMin;

  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2];
    Right := AInput[I * 2 + 1];

    // Calculate LFO values (sine wave for smooth sweep)
    LFOValueL := Sin(FLFOPhase[0] * 2.0 * PI);
    LFOValueR := Sin(FLFOPhase[1] * 2.0 * PI);

    // Map to unipolar (0-1)
    LFOValueL := (LFOValueL + 1.0) * 0.5;
    LFOValueR := (LFOValueR + 1.0) * 0.5;

    // Apply depth
    LFOValueL := LFOValueL * FDepth;
    LFOValueR := LFOValueR * FDepth;

    // Calculate modulated frequency
    FreqL := FFreqMin + LFOValueL * FreqRange;
    FreqR := FFreqMin + LFOValueR * FreqRange;

    // Calculate allpass coefficients
    // coeff = (tan(pi * freq / sr) - 1) / (tan(pi * freq / sr) + 1)
    CoeffL := (Tan(PI * FreqL / FSampleRate) - 1.0) /
              (Tan(PI * FreqL / FSampleRate) + 1.0);
    CoeffR := (Tan(PI * FreqR / FSampleRate) - 1.0) /
              (Tan(PI * FreqR / FSampleRate) + 1.0);

    // Add feedback
    PhasedL := Left + FFeedbackSample[0] * FFeedback;
    PhasedR := Right + FFeedbackSample[1] * FFeedback;

    // Process through allpass cascade
    for S := 0 to FStages - 1 do
    begin
      PhasedL := ProcessAllpass(PhasedL, CoeffL, 0, S);
      PhasedR := ProcessAllpass(PhasedR, CoeffR, 1, S);
    end;

    // Store feedback
    FFeedbackSample[0] := PhasedL;
    FFeedbackSample[1] := PhasedR;

    // Mix dry/wet (sum creates notches)
    AOutput[I * 2] := Left * (1.0 - FMix) + (Left + PhasedL) * FMix * 0.5;
    AOutput[I * 2 + 1] := Right * (1.0 - FMix) + (Right + PhasedR) * FMix * 0.5;

    // Advance LFO phases
    FLFOPhase[0] := FLFOPhase[0] + PhaseInc;
    if FLFOPhase[0] >= 1.0 then FLFOPhase[0] := FLFOPhase[0] - 1.0;

    FLFOPhase[1] := FLFOPhase[1] + PhaseInc;
    if FLFOPhase[1] >= 1.0 then FLFOPhase[1] := FLFOPhase[1] - 1.0;
  end;
end;

end.
