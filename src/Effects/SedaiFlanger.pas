{*
 * Sedai Audio Foundation - Flanger
 *
 * TSedaiFlanger provides classic flanging effect with LFO modulation,
 * feedback, and stereo enhancement. Based on short modulated delay.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiFlanger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiEffect;

const
  MAX_FLANGER_DELAY = 20;         // Maximum delay in ms

type
  { TSedaiFlanger }
  // Flanger effect
  TSedaiFlanger = class(TSedaiEffect)
  private
    FRate: Single;                // LFO rate in Hz
    FDepth: Single;               // Modulation depth (0-1)
    FDelay: Single;               // Center delay in ms
    FFeedback: Single;            // Feedback amount (-1 to +1)
    FMix: Single;                 // Dry/wet mix
    FStereoPhase: Single;         // Stereo phase offset (0-180 degrees)

    // LFO state
    FLFOPhase: array[0..1] of Single;

    // Delay buffer
    FBufferSize: Integer;
    FBuffer: array[0..1] of array of Single;
    FWriteIndex: Integer;

    // Feedback state
    FFeedbackSample: array[0..1] of Single;

    procedure SetRate(AValue: Single);
    procedure SetDepth(AValue: Single);
    procedure SetDelay(AValue: Single);
    procedure SetStereoPhase(AValue: Single);
    procedure AllocateBuffer;

    function ReadDelayInterpolated(AChannel: Integer; ADelaySamples: Single): Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    property Rate: Single read FRate write SetRate;
    property Depth: Single read FDepth write SetDepth;
    property Delay: Single read FDelay write SetDelay;
    property Feedback: Single read FFeedback write FFeedback;
    property Mix: Single read FMix write FMix;
    property StereoPhase: Single read FStereoPhase write SetStereoPhase;
  end;

implementation

{ TSedaiFlanger }

constructor TSedaiFlanger.Create;
begin
  inherited Create;

  FRate := 0.5;
  FDepth := 0.7;
  FDelay := 5.0;
  FFeedback := 0.5;
  FMix := 0.5;
  FStereoPhase := 90.0;

  FLFOPhase[0] := 0.0;
  FLFOPhase[1] := 0.25;  // 90 degree offset

  FBufferSize := 0;
  FWriteIndex := 0;

  FFeedbackSample[0] := 0.0;
  FFeedbackSample[1] := 0.0;

  AllocateBuffer;
end;

destructor TSedaiFlanger.Destroy;
begin
  SetLength(FBuffer[0], 0);
  SetLength(FBuffer[1], 0);
  inherited Destroy;
end;

procedure TSedaiFlanger.Reset;
var
  I: Integer;
begin
  inherited Reset;

  FWriteIndex := 0;
  FLFOPhase[0] := 0.0;
  FLFOPhase[1] := FStereoPhase / 360.0;

  FFeedbackSample[0] := 0.0;
  FFeedbackSample[1] := 0.0;

  for I := 0 to FBufferSize - 1 do
  begin
    if I < Length(FBuffer[0]) then FBuffer[0][I] := 0.0;
    if I < Length(FBuffer[1]) then FBuffer[1][I] := 0.0;
  end;
end;

procedure TSedaiFlanger.SetRate(AValue: Single);
begin
  if AValue < 0.01 then AValue := 0.01;
  if AValue > 10.0 then AValue := 10.0;
  FRate := AValue;
end;

procedure TSedaiFlanger.SetDepth(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 1.0 then AValue := 1.0;
  FDepth := AValue;
end;

procedure TSedaiFlanger.SetDelay(AValue: Single);
begin
  if AValue < 0.5 then AValue := 0.5;
  if AValue > MAX_FLANGER_DELAY then AValue := MAX_FLANGER_DELAY;
  FDelay := AValue;
end;

procedure TSedaiFlanger.SetStereoPhase(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 180.0 then AValue := 180.0;
  FStereoPhase := AValue;
  FLFOPhase[1] := FLFOPhase[0] + FStereoPhase / 360.0;
  if FLFOPhase[1] >= 1.0 then
    FLFOPhase[1] := FLFOPhase[1] - 1.0;
end;

procedure TSedaiFlanger.AllocateBuffer;
var
  NewSize: Integer;
begin
  if FSampleRate > 0 then
  begin
    NewSize := Round(MAX_FLANGER_DELAY * 0.001 * FSampleRate * 2) + 256;

    if NewSize <> FBufferSize then
    begin
      FBufferSize := NewSize;
      SetLength(FBuffer[0], FBufferSize);
      SetLength(FBuffer[1], FBufferSize);
      Reset;
    end;
  end;
end;

function TSedaiFlanger.ReadDelayInterpolated(AChannel: Integer; ADelaySamples: Single): Single;
var
  ReadPos: Single;
  Index0, Index1, Index2, Index3: Integer;
  Frac: Single;
  Y0, Y1, Y2, Y3: Single;
begin
  ReadPos := FWriteIndex - ADelaySamples;
  while ReadPos < 0 do
    ReadPos := ReadPos + FBufferSize;

  Index1 := Trunc(ReadPos) mod FBufferSize;
  Index0 := (Index1 - 1 + FBufferSize) mod FBufferSize;
  Index2 := (Index1 + 1) mod FBufferSize;
  Index3 := (Index1 + 2) mod FBufferSize;
  Frac := ReadPos - Trunc(ReadPos);

  Y0 := FBuffer[AChannel][Index0];
  Y1 := FBuffer[AChannel][Index1];
  Y2 := FBuffer[AChannel][Index2];
  Y3 := FBuffer[AChannel][Index3];

  // Cubic interpolation
  Result := CubicInterpolate(Y0, Y1, Y2, Y3, Frac);
end;

procedure TSedaiFlanger.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
  Left, Right: Single;
  FlangedL, FlangedR: Single;
  LFOValueL, LFOValueR: Single;
  DelaySamplesL, DelaySamplesR: Single;
  MinDelay, MaxDelay: Single;
  PhaseInc: Single;
begin
  if FBufferSize = 0 then
    AllocateBuffer;

  PhaseInc := FRate / FSampleRate;

  // Calculate delay range in samples
  MinDelay := 1;
  MaxDelay := FDelay * 0.001 * FSampleRate;

  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2];
    Right := AInput[I * 2 + 1];

    // Calculate LFO values (triangle wave)
    if FLFOPhase[0] < 0.5 then
      LFOValueL := FLFOPhase[0] * 4.0 - 1.0
    else
      LFOValueL := 3.0 - FLFOPhase[0] * 4.0;

    if FLFOPhase[1] < 0.5 then
      LFOValueR := FLFOPhase[1] * 4.0 - 1.0
    else
      LFOValueR := 3.0 - FLFOPhase[1] * 4.0;

    // Map LFO to delay time (unipolar)
    LFOValueL := (LFOValueL + 1.0) * 0.5;
    LFOValueR := (LFOValueR + 1.0) * 0.5;

    // Calculate modulated delay
    DelaySamplesL := MinDelay + LFOValueL * FDepth * (MaxDelay - MinDelay);
    DelaySamplesR := MinDelay + LFOValueR * FDepth * (MaxDelay - MinDelay);

    // Read from delay line
    FlangedL := ReadDelayInterpolated(0, DelaySamplesL);
    FlangedR := ReadDelayInterpolated(1, DelaySamplesR);

    // Write to delay line with feedback
    FBuffer[0][FWriteIndex] := Left + FFeedbackSample[0] * FFeedback;
    FBuffer[1][FWriteIndex] := Right + FFeedbackSample[1] * FFeedback;

    // Store feedback
    FFeedbackSample[0] := FlangedL;
    FFeedbackSample[1] := FlangedR;

    // Mix dry/wet (negative feedback inverts phase for classic flanger)
    AOutput[I * 2] := Left * (1.0 - FMix) + FlangedL * FMix;
    AOutput[I * 2 + 1] := Right * (1.0 - FMix) + FlangedR * FMix;

    // Advance write index
    FWriteIndex := (FWriteIndex + 1) mod FBufferSize;

    // Advance LFO phases
    FLFOPhase[0] := FLFOPhase[0] + PhaseInc;
    if FLFOPhase[0] >= 1.0 then FLFOPhase[0] := FLFOPhase[0] - 1.0;

    FLFOPhase[1] := FLFOPhase[1] + PhaseInc;
    if FLFOPhase[1] >= 1.0 then FLFOPhase[1] := FLFOPhase[1] - 1.0;
  end;
end;

end.
