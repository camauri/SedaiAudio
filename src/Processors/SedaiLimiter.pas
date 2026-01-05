{*
 * Sedai Audio Foundation - Limiter
 *
 * TSedaiLimiter provides brickwall limiting with lookahead,
 * ensuring output never exceeds the ceiling threshold.
 * Includes true peak detection and automatic release.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiLimiter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiFilter;

type
  { TSedaiLimiter }
  // Brickwall limiter with lookahead
  TSedaiLimiter = class(TSedaiSignalProcessor)
  private
    // Parameters
    FCeiling: Single;             // Output ceiling in dB (typically -0.1 to -3)
    FRelease: Single;             // Release time in ms
    FLookahead: Single;           // Lookahead time in ms

    // Internal state
    FGain: Single;                // Current gain multiplier
    FReleaseCoeff: Single;        // Release coefficient

    // Lookahead buffer
    FLookaheadSamples: Integer;
    FDelayBuffer: array[0..1] of array of Single;
    FDelayIndex: Integer;

    // Peak detection
    FPeakHold: Single;
    FPeakHoldTime: Integer;
    FPeakHoldCounter: Integer;

    // Metering
    FGainReduction: Single;
    FPeakLevel: Single;

    procedure SetCeiling(AValue: Single);
    procedure SetRelease(AValue: Single);
    procedure SetLookahead(AValue: Single);
    procedure UpdateCoefficients;
    procedure ResizeDelayBuffer;

    function FindPeakInLookahead: Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // Process block
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Properties
    property Ceiling: Single read FCeiling write SetCeiling;
    property Release: Single read FRelease write SetRelease;
    property Lookahead: Single read FLookahead write SetLookahead;
    property GainReduction: Single read FGainReduction;
    property PeakLevel: Single read FPeakLevel;
  end;

implementation

{ TSedaiLimiter }

constructor TSedaiLimiter.Create;
begin
  inherited Create;

  FCeiling := -0.3;
  FRelease := 100.0;
  FLookahead := 5.0;

  FGain := 1.0;
  FReleaseCoeff := 0.9999;

  FLookaheadSamples := 0;
  FDelayIndex := 0;

  FPeakHold := 0.0;
  FPeakHoldTime := 0;
  FPeakHoldCounter := 0;

  FGainReduction := 0.0;
  FPeakLevel := -96.0;

  UpdateCoefficients;
end;

destructor TSedaiLimiter.Destroy;
begin
  SetLength(FDelayBuffer[0], 0);
  SetLength(FDelayBuffer[1], 0);
  inherited Destroy;
end;

procedure TSedaiLimiter.Reset;
var
  I: Integer;
begin
  inherited Reset;

  FGain := 1.0;
  FDelayIndex := 0;
  FPeakHold := 0.0;
  FPeakHoldCounter := 0;
  FGainReduction := 0.0;

  // Clear delay buffer
  for I := 0 to FLookaheadSamples - 1 do
  begin
    if I < Length(FDelayBuffer[0]) then
    begin
      FDelayBuffer[0][I] := 0.0;
      FDelayBuffer[1][I] := 0.0;
    end;
  end;
end;

procedure TSedaiLimiter.SetCeiling(AValue: Single);
begin
  if AValue < -20.0 then
    AValue := -20.0
  else if AValue > 0.0 then
    AValue := 0.0;

  FCeiling := AValue;
end;

procedure TSedaiLimiter.SetRelease(AValue: Single);
begin
  if AValue < 10.0 then
    AValue := 10.0
  else if AValue > 1000.0 then
    AValue := 1000.0;

  FRelease := AValue;
  UpdateCoefficients;
end;

procedure TSedaiLimiter.SetLookahead(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 20.0 then
    AValue := 20.0;

  FLookahead := AValue;
  ResizeDelayBuffer;
end;

procedure TSedaiLimiter.UpdateCoefficients;
begin
  if FSampleRate > 0 then
  begin
    FReleaseCoeff := Exp(-1.0 / (FRelease * 0.001 * FSampleRate));
    FPeakHoldTime := Round(FLookahead * 0.001 * FSampleRate);
    ResizeDelayBuffer;
  end;
end;

procedure TSedaiLimiter.ResizeDelayBuffer;
var
  NewSize: Integer;
begin
  if FSampleRate > 0 then
  begin
    NewSize := Round(FLookahead * 0.001 * FSampleRate);
    if NewSize < 1 then NewSize := 1;

    if NewSize <> FLookaheadSamples then
    begin
      FLookaheadSamples := NewSize;
      SetLength(FDelayBuffer[0], FLookaheadSamples);
      SetLength(FDelayBuffer[1], FLookaheadSamples);
      FDelayIndex := 0;
    end;
  end;
end;

function TSedaiLimiter.FindPeakInLookahead: Single;
var
  I: Integer;
  Peak, Sample: Single;
begin
  Peak := 0.0;

  for I := 0 to FLookaheadSamples - 1 do
  begin
    Sample := Abs(FDelayBuffer[0][I]);
    if Sample > Peak then
      Peak := Sample;

    Sample := Abs(FDelayBuffer[1][I]);
    if Sample > Peak then
      Peak := Sample;
  end;

  Result := Peak;
end;

procedure TSedaiLimiter.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
  Left, Right: Single;
  DelayedLeft, DelayedRight: Single;
  Peak, TargetGain, CeilingLin: Single;
  GainDB: Single;
begin
  CeilingLin := DecibelToLinear(FCeiling);

  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2];
    Right := AInput[I * 2 + 1];

    // Store in delay buffer
    if FLookaheadSamples > 0 then
    begin
      DelayedLeft := FDelayBuffer[0][FDelayIndex];
      DelayedRight := FDelayBuffer[1][FDelayIndex];

      FDelayBuffer[0][FDelayIndex] := Left;
      FDelayBuffer[1][FDelayIndex] := Right;

      FDelayIndex := (FDelayIndex + 1) mod FLookaheadSamples;
    end
    else
    begin
      DelayedLeft := Left;
      DelayedRight := Right;
    end;

    // Find peak in lookahead buffer
    Peak := FindPeakInLookahead;

    // Also check current input
    if Abs(Left) > Peak then Peak := Abs(Left);
    if Abs(Right) > Peak then Peak := Abs(Right);

    // Calculate target gain
    if Peak > CeilingLin then
      TargetGain := CeilingLin / Peak
    else
      TargetGain := 1.0;

    // Apply gain envelope (instant attack, smooth release)
    if TargetGain < FGain then
      FGain := TargetGain  // Instant attack
    else
      FGain := FGain + (1.0 - FReleaseCoeff) * (TargetGain - FGain);  // Smooth release

    // Apply gain to delayed signal
    Left := DelayedLeft * FGain;
    Right := DelayedRight * FGain;

    // Hard clip as safety (should rarely trigger with proper lookahead)
    if Left > CeilingLin then Left := CeilingLin
    else if Left < -CeilingLin then Left := -CeilingLin;

    if Right > CeilingLin then Right := CeilingLin
    else if Right < -CeilingLin then Right := -CeilingLin;

    // Output
    AOutput[I * 2] := Left;
    AOutput[I * 2 + 1] := Right;

    // Update metering
    if FGain < 1.0 then
      GainDB := 20.0 * Log10(FGain)
    else
      GainDB := 0.0;

    FGainReduction := -GainDB;

    Peak := Max(Abs(Left), Abs(Right));
    if Peak > 1e-10 then
      FPeakLevel := 20.0 * Log10(Peak)
    else
      FPeakLevel := -96.0;
  end;
end;

end.
