{*
 * Sedai Audio Foundation - Amplifier
 *
 * TSedaiAmplifier provides gain stage with multiple saturation modes,
 * soft/hard clipping, and tube-style warmth. Includes DC offset removal.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAmplifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiFilter;

type
  // Saturation mode
  TSaturationMode = (
    smNone,           // Clean (no saturation)
    smSoft,           // Soft clipping (tanh)
    smHard,           // Hard clipping
    smTube,           // Tube-style warmth
    smTape,           // Tape saturation
    smTransistor      // Transistor-style
  );

  { TSedaiAmplifier }
  // Gain stage with saturation and DC blocking
  TSedaiAmplifier = class(TSedaiSignalProcessor)
  private
    FGain: Single;                // Linear gain
    FGainDB: Single;              // Gain in dB
    FSaturationMode: TSaturationMode;
    FSaturationAmount: Single;    // 0.0 - 1.0
    FOutputLevel: Single;         // Output trim

    // DC blocking filter state
    FDCBlockEnabled: Boolean;
    FDCBlockCoeff: Single;
    FDCBlockState: array[0..1] of Single;
    FDCBlockPrev: array[0..1] of Single;

    // Tube simulation state
    FTubeBias: Single;
    FTubeAsymmetry: Single;

    procedure SetGain(AValue: Single);
    procedure SetGainDB(AValue: Single);
    procedure SetSaturationMode(AValue: TSaturationMode);
    procedure UpdateDCBlockCoeff;

    function ApplySaturation(AInput: Single): Single;
    function SoftClip(AInput: Single): Single;
    function HardClip(AInput: Single): Single;
    function TubeSaturate(AInput: Single): Single;
    function TapeSaturate(AInput: Single): Single;
    function TransistorSaturate(AInput: Single): Single;
    function DCBlock(AInput: Single; AChannel: Integer): Single;

  public
    constructor Create; override;

    procedure Reset; override;

    // Process single sample
    function ProcessSample(AInput: Single; AChannel: Integer): Single;

    // Process block
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Properties
    property Gain: Single read FGain write SetGain;
    property GainDB: Single read FGainDB write SetGainDB;
    property SaturationMode: TSaturationMode read FSaturationMode write SetSaturationMode;
    property SaturationAmount: Single read FSaturationAmount write FSaturationAmount;
    property OutputLevel: Single read FOutputLevel write FOutputLevel;
    property DCBlockEnabled: Boolean read FDCBlockEnabled write FDCBlockEnabled;
    property TubeBias: Single read FTubeBias write FTubeBias;
    property TubeAsymmetry: Single read FTubeAsymmetry write FTubeAsymmetry;
  end;

implementation

{ TSedaiAmplifier }

constructor TSedaiAmplifier.Create;
begin
  inherited Create;

  FGain := 1.0;
  FGainDB := 0.0;
  FSaturationMode := smNone;
  FSaturationAmount := 0.5;
  FOutputLevel := 1.0;

  FDCBlockEnabled := True;
  FDCBlockCoeff := 0.995;
  FDCBlockState[0] := 0.0;
  FDCBlockState[1] := 0.0;
  FDCBlockPrev[0] := 0.0;
  FDCBlockPrev[1] := 0.0;

  FTubeBias := 0.0;
  FTubeAsymmetry := 0.2;
end;

procedure TSedaiAmplifier.Reset;
begin
  inherited Reset;

  FDCBlockState[0] := 0.0;
  FDCBlockState[1] := 0.0;
  FDCBlockPrev[0] := 0.0;
  FDCBlockPrev[1] := 0.0;
end;

procedure TSedaiAmplifier.SetGain(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 100.0 then
    AValue := 100.0;

  FGain := AValue;
  FGainDB := LinearToDecibel(AValue);
end;

procedure TSedaiAmplifier.SetGainDB(AValue: Single);
begin
  if AValue < -96.0 then
    AValue := -96.0
  else if AValue > 40.0 then
    AValue := 40.0;

  FGainDB := AValue;
  FGain := DecibelToLinear(AValue);
end;

procedure TSedaiAmplifier.SetSaturationMode(AValue: TSaturationMode);
begin
  FSaturationMode := AValue;
end;

procedure TSedaiAmplifier.UpdateDCBlockCoeff;
begin
  // Higher sample rate needs higher coefficient
  if FSampleRate > 0 then
    FDCBlockCoeff := 1.0 - (20.0 / FSampleRate)
  else
    FDCBlockCoeff := 0.995;
end;

function TSedaiAmplifier.SoftClip(AInput: Single): Single;
var
  Threshold: Single;
begin
  // Soft clipping using tanh
  Threshold := 1.0 - FSaturationAmount * 0.5;

  if Abs(AInput) < Threshold then
    Result := AInput
  else
    Result := Threshold * Tanh(AInput / Threshold);
end;

function TSedaiAmplifier.HardClip(AInput: Single): Single;
var
  Threshold: Single;
begin
  Threshold := 1.0 - FSaturationAmount * 0.3;

  if AInput > Threshold then
    Result := Threshold
  else if AInput < -Threshold then
    Result := -Threshold
  else
    Result := AInput;
end;

function TSedaiAmplifier.TubeSaturate(AInput: Single): Single;
var
  Biased, Saturated: Single;
begin
  // Add bias for asymmetric clipping
  Biased := AInput + FTubeBias;

  // Asymmetric soft clipping (more on positive side)
  if Biased >= 0 then
    Saturated := Tanh(Biased * (1.0 + FTubeAsymmetry))
  else
    Saturated := Tanh(Biased * (1.0 - FTubeAsymmetry * 0.5));

  // Blend based on saturation amount
  Result := AInput * (1.0 - FSaturationAmount) + Saturated * FSaturationAmount;
end;

function TSedaiAmplifier.TapeSaturate(AInput: Single): Single;
var
  Compressed: Single;
begin
  // Tape-style soft compression with gentle saturation
  Compressed := AInput / (1.0 + Abs(AInput) * FSaturationAmount);

  // Add slight even harmonics
  Result := Compressed + Compressed * Compressed * 0.1 * FSaturationAmount;
end;

function TSedaiAmplifier.TransistorSaturate(AInput: Single): Single;
var
  Squared: Single;
begin
  // Transistor-style clipping (harder than tube)
  Squared := AInput * AInput;

  if AInput >= 0 then
    Result := AInput / (1.0 + Squared * FSaturationAmount)
  else
    Result := AInput / (1.0 + Squared * FSaturationAmount * 1.2);
end;

function TSedaiAmplifier.ApplySaturation(AInput: Single): Single;
begin
  case FSaturationMode of
    smNone:
      Result := AInput;
    smSoft:
      Result := SoftClip(AInput);
    smHard:
      Result := HardClip(AInput);
    smTube:
      Result := TubeSaturate(AInput);
    smTape:
      Result := TapeSaturate(AInput);
    smTransistor:
      Result := TransistorSaturate(AInput);
    else
      Result := AInput;
  end;
end;

function TSedaiAmplifier.DCBlock(AInput: Single; AChannel: Integer): Single;
begin
  // High-pass filter to remove DC offset
  // y[n] = x[n] - x[n-1] + R * y[n-1]
  Result := AInput - FDCBlockPrev[AChannel] + FDCBlockCoeff * FDCBlockState[AChannel];
  FDCBlockPrev[AChannel] := AInput;
  FDCBlockState[AChannel] := Result;
end;

function TSedaiAmplifier.ProcessSample(AInput: Single; AChannel: Integer): Single;
begin
  // Apply gain
  Result := AInput * FGain;

  // Apply saturation
  if FSaturationMode <> smNone then
    Result := ApplySaturation(Result);

  // Apply output level
  Result := Result * FOutputLevel;

  // DC blocking
  if FDCBlockEnabled then
    Result := DCBlock(Result, AChannel);
end;

procedure TSedaiAmplifier.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
begin
  for I := 0 to AFrameCount - 1 do
  begin
    AOutput[I * 2] := ProcessSample(AInput[I * 2], 0);
    AOutput[I * 2 + 1] := ProcessSample(AInput[I * 2 + 1], 1);
  end;
end;

end.
