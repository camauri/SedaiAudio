{*
 * Sedai Audio Foundation - Distortion
 *
 * TSedaiDistortion provides multiple distortion algorithms including
 * soft/hard clipping, foldback, bitcrush, tube, and tape emulation.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiDistortion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiFilter;

type
  // Distortion type
  TDistortionType = (
    dtSoftClip,       // Smooth saturation (tanh)
    dtHardClip,       // Hard clipping
    dtFoldback,       // Wavefolding
    dtBitcrush,       // Bit reduction + sample rate reduction
    dtTube,           // Tube amp simulation
    dtTape,           // Tape saturation
    dtRectify,        // Full/half wave rectification
    dtAsymmetric      // Asymmetric clipping
  );

  { TSedaiDistortion }
  // Multi-mode distortion effect
  TSedaiDistortion = class(TSedaiSignalProcessor)
  private
    FDistortionType: TDistortionType;
    FDrive: Single;               // Input drive (1.0 - 100.0)
    FTone: Single;                // Post-distortion tone (0.0 - 1.0)
    FMix: Single;                 // Dry/wet mix (0.0 - 1.0)
    FOutputGain: Single;          // Output gain compensation

    // Bitcrush parameters
    FBitDepth: Integer;           // Bit depth (1-16)
    FDownsampleFactor: Integer;   // Sample rate reduction
    FHoldSample: array[0..1] of Single;
    FHoldCounter: Integer;

    // Foldback parameters
    FFoldThreshold: Single;

    // Tone filter (simple one-pole lowpass)
    FToneCoeff: Single;
    FToneState: array[0..1] of Single;

    procedure SetDrive(AValue: Single);
    procedure SetTone(AValue: Single);
    procedure SetBitDepth(AValue: Integer);
    procedure UpdateToneCoeff;

    function ProcessSoftClip(AInput: Single): Single;
    function ProcessHardClip(AInput: Single): Single;
    function ProcessFoldback(AInput: Single): Single;
    function ProcessBitcrush(AInput: Single; AChannel: Integer): Single;
    function ProcessTube(AInput: Single): Single;
    function ProcessTape(AInput: Single): Single;
    function ProcessRectify(AInput: Single): Single;
    function ProcessAsymmetric(AInput: Single): Single;
    function ApplyToneFilter(AInput: Single; AChannel: Integer): Single;

  public
    constructor Create; override;

    procedure Reset; override;

    // Process single sample
    function ProcessSample(AInput: Single; AChannel: Integer): Single;

    // Process block
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Properties
    property DistortionType: TDistortionType read FDistortionType write FDistortionType;
    property Drive: Single read FDrive write SetDrive;
    property Tone: Single read FTone write SetTone;
    property Mix: Single read FMix write FMix;
    property OutputGain: Single read FOutputGain write FOutputGain;
    property BitDepth: Integer read FBitDepth write SetBitDepth;
    property DownsampleFactor: Integer read FDownsampleFactor write FDownsampleFactor;
    property FoldThreshold: Single read FFoldThreshold write FFoldThreshold;
  end;

implementation

{ TSedaiDistortion }

constructor TSedaiDistortion.Create;
begin
  inherited Create;

  FDistortionType := dtSoftClip;
  FDrive := 1.0;
  FTone := 0.5;
  FMix := 1.0;
  FOutputGain := 1.0;

  FBitDepth := 8;
  FDownsampleFactor := 1;
  FHoldSample[0] := 0.0;
  FHoldSample[1] := 0.0;
  FHoldCounter := 0;

  FFoldThreshold := 0.8;

  FToneCoeff := 0.5;
  FToneState[0] := 0.0;
  FToneState[1] := 0.0;

  UpdateToneCoeff;
end;

procedure TSedaiDistortion.Reset;
begin
  inherited Reset;

  FHoldSample[0] := 0.0;
  FHoldSample[1] := 0.0;
  FHoldCounter := 0;
  FToneState[0] := 0.0;
  FToneState[1] := 0.0;
end;

procedure TSedaiDistortion.SetDrive(AValue: Single);
begin
  if AValue < 1.0 then
    AValue := 1.0
  else if AValue > 100.0 then
    AValue := 100.0;

  FDrive := AValue;
end;

procedure TSedaiDistortion.SetTone(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;

  FTone := AValue;
  UpdateToneCoeff;
end;

procedure TSedaiDistortion.SetBitDepth(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1
  else if AValue > 16 then
    AValue := 16;

  FBitDepth := AValue;
end;

procedure TSedaiDistortion.UpdateToneCoeff;
begin
  // Map tone (0-1) to filter coefficient
  // 0 = dark, 1 = bright
  FToneCoeff := 0.1 + FTone * 0.89;
end;

function TSedaiDistortion.ProcessSoftClip(AInput: Single): Single;
begin
  // Tanh soft clipping
  Result := Tanh(AInput * FDrive) / Tanh(FDrive);
end;

function TSedaiDistortion.ProcessHardClip(AInput: Single): Single;
var
  Driven: Single;
begin
  Driven := AInput * FDrive;

  if Driven > 1.0 then
    Result := 1.0
  else if Driven < -1.0 then
    Result := -1.0
  else
    Result := Driven;
end;

function TSedaiDistortion.ProcessFoldback(AInput: Single): Single;
var
  Driven: Single;
begin
  Driven := AInput * FDrive;

  // Foldback distortion - fold signal back when it exceeds threshold
  while Abs(Driven) > FFoldThreshold do
  begin
    if Driven > FFoldThreshold then
      Driven := 2.0 * FFoldThreshold - Driven
    else if Driven < -FFoldThreshold then
      Driven := -2.0 * FFoldThreshold - Driven;
  end;

  Result := Driven / FFoldThreshold;
end;

function TSedaiDistortion.ProcessBitcrush(AInput: Single; AChannel: Integer): Single;
var
  Quantized: Single;
  Steps: Integer;
begin
  // Sample rate reduction
  Inc(FHoldCounter);
  if FHoldCounter >= FDownsampleFactor then
  begin
    FHoldCounter := 0;

    // Bit depth reduction
    Steps := 1 shl FBitDepth;
    Quantized := AInput * FDrive;

    // Quantize
    Quantized := Round(Quantized * Steps) / Steps;

    FHoldSample[AChannel] := Quantized;
  end;

  Result := FHoldSample[AChannel];
end;

function TSedaiDistortion.ProcessTube(AInput: Single): Single;
var
  Driven, Positive, Negative: Single;
begin
  Driven := AInput * FDrive;

  // Asymmetric tube-style clipping
  // Softer on positive, harder on negative
  if Driven >= 0 then
  begin
    Positive := 1.0 - Exp(-Driven);
    Result := Positive;
  end
  else
  begin
    Negative := -1.0 + Exp(Driven);
    Result := Negative * 0.9;  // Slight asymmetry
  end;
end;

function TSedaiDistortion.ProcessTape(AInput: Single): Single;
var
  Driven: Single;
begin
  Driven := AInput * FDrive;

  // Tape-style saturation with soft knee
  Result := Driven / (1.0 + Abs(Driven));

  // Add slight even harmonics
  Result := Result + 0.05 * Driven * Driven * Sign(Driven);
end;

function TSedaiDistortion.ProcessRectify(AInput: Single): Single;
var
  Driven: Single;
begin
  Driven := AInput * FDrive;

  // Full-wave rectification
  Result := Abs(Driven);

  // Normalize
  if Result > 1.0 then
    Result := 1.0;
end;

function TSedaiDistortion.ProcessAsymmetric(AInput: Single): Single;
var
  Driven: Single;
begin
  Driven := AInput * FDrive;

  // Strong clipping on positive, soft on negative
  if Driven > 0.3 then
    Result := 0.3 + (Driven - 0.3) * 0.2
  else if Driven < -0.7 then
    Result := -0.7 - Tanh((Driven + 0.7) * 2.0) * 0.3
  else
    Result := Driven;
end;

function TSedaiDistortion.ApplyToneFilter(AInput: Single; AChannel: Integer): Single;
begin
  // Simple one-pole lowpass for tone control
  FToneState[AChannel] := FToneState[AChannel] + FToneCoeff * (AInput - FToneState[AChannel]);
  Result := FToneState[AChannel];
end;

function TSedaiDistortion.ProcessSample(AInput: Single; AChannel: Integer): Single;
var
  Processed, Filtered: Single;
begin
  // Apply distortion based on type
  case FDistortionType of
    dtSoftClip:    Processed := ProcessSoftClip(AInput);
    dtHardClip:    Processed := ProcessHardClip(AInput);
    dtFoldback:    Processed := ProcessFoldback(AInput);
    dtBitcrush:    Processed := ProcessBitcrush(AInput, AChannel);
    dtTube:        Processed := ProcessTube(AInput);
    dtTape:        Processed := ProcessTape(AInput);
    dtRectify:     Processed := ProcessRectify(AInput);
    dtAsymmetric:  Processed := ProcessAsymmetric(AInput);
    else           Processed := AInput;
  end;

  // Apply tone filter
  Filtered := ApplyToneFilter(Processed, AChannel);

  // Apply output gain
  Filtered := Filtered * FOutputGain;

  // Mix dry/wet
  Result := AInput * (1.0 - FMix) + Filtered * FMix;
end;

procedure TSedaiDistortion.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
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
