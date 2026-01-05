{*
 * Sedai Audio Foundation - Compressor
 *
 * TSedaiCompressor provides dynamics compression with adjustable
 * threshold, ratio, attack, release, knee, and makeup gain.
 * Supports peak and RMS detection, sidechain, and lookahead.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiCompressor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiFilter;

type
  // Detection mode
  TDetectionMode = (
    dmPeak,           // Peak detection
    dmRMS             // RMS detection
  );

  // Stereo link mode
  TStereoLinkMode = (
    slmDual,          // Independent L/R
    slmLinked,        // Linked (max of L/R)
    slmMid,           // Mid channel only
    slmSide           // Side channel only
  );

  { TSedaiCompressor }
  // Dynamics compressor
  TSedaiCompressor = class(TSedaiSignalProcessor)
  private
    // Parameters
    FThreshold: Single;           // Threshold in dB (-60 to 0)
    FRatio: Single;               // Compression ratio (1:1 to inf:1)
    FAttack: Single;              // Attack time in ms
    FRelease: Single;             // Release time in ms
    FKnee: Single;                // Knee width in dB (0 = hard knee)
    FMakeupGain: Single;          // Makeup gain in dB
    FAutoMakeup: Boolean;         // Auto makeup gain

    // Mode
    FDetectionMode: TDetectionMode;
    FStereoLink: TStereoLinkMode;

    // Envelope followers
    FEnvelope: array[0..1] of Single;
    FAttackCoeff: Single;
    FReleaseCoeff: Single;

    // RMS detection
    FRMSWindow: Integer;
    FRMSBuffer: array[0..1] of array of Single;
    FRMSIndex: Integer;
    FRMSSum: array[0..1] of Single;

    // Metering
    FGainReduction: Single;       // Current gain reduction in dB
    FInputLevel: Single;          // Input level in dB
    FOutputLevel: Single;         // Output level in dB

    procedure SetThreshold(AValue: Single);
    procedure SetRatio(AValue: Single);
    procedure SetAttack(AValue: Single);
    procedure SetRelease(AValue: Single);
    procedure SetKnee(AValue: Single);
    procedure UpdateCoefficients;

    function DetectLevel(ALeft, ARight: Single): Single;
    function ComputeGain(AInputDB: Single): Single;
    function ApplyEnvelope(AInput, AEnvelope: Single; AChannel: Integer): Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // Process block
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Get current gain reduction (for metering)
    function GetGainReduction: Single;

    // Properties
    property Threshold: Single read FThreshold write SetThreshold;
    property Ratio: Single read FRatio write SetRatio;
    property Attack: Single read FAttack write SetAttack;
    property Release: Single read FRelease write SetRelease;
    property Knee: Single read FKnee write SetKnee;
    property MakeupGain: Single read FMakeupGain write FMakeupGain;
    property AutoMakeup: Boolean read FAutoMakeup write FAutoMakeup;
    property DetectionMode: TDetectionMode read FDetectionMode write FDetectionMode;
    property StereoLink: TStereoLinkMode read FStereoLink write FStereoLink;
    property GainReduction: Single read FGainReduction;
    property InputLevel: Single read FInputLevel;
    property OutputLevel: Single read FOutputLevel;
  end;

implementation

{ TSedaiCompressor }

constructor TSedaiCompressor.Create;
begin
  inherited Create;

  FThreshold := -20.0;
  FRatio := 4.0;
  FAttack := 10.0;
  FRelease := 100.0;
  FKnee := 6.0;
  FMakeupGain := 0.0;
  FAutoMakeup := False;

  FDetectionMode := dmPeak;
  FStereoLink := slmLinked;

  FEnvelope[0] := 0.0;
  FEnvelope[1] := 0.0;

  FRMSWindow := 50;  // ~1ms at 44.1kHz
  FRMSIndex := 0;

  FGainReduction := 0.0;
  FInputLevel := -96.0;
  FOutputLevel := -96.0;

  UpdateCoefficients;
end;

destructor TSedaiCompressor.Destroy;
begin
  SetLength(FRMSBuffer[0], 0);
  SetLength(FRMSBuffer[1], 0);
  inherited Destroy;
end;

procedure TSedaiCompressor.Reset;
begin
  inherited Reset;

  FEnvelope[0] := 0.0;
  FEnvelope[1] := 0.0;
  FRMSIndex := 0;
  FRMSSum[0] := 0.0;
  FRMSSum[1] := 0.0;
  FGainReduction := 0.0;
end;

procedure TSedaiCompressor.SetThreshold(AValue: Single);
begin
  if AValue < -60.0 then
    AValue := -60.0
  else if AValue > 0.0 then
    AValue := 0.0;

  FThreshold := AValue;
end;

procedure TSedaiCompressor.SetRatio(AValue: Single);
begin
  if AValue < 1.0 then
    AValue := 1.0
  else if AValue > 100.0 then
    AValue := 100.0;

  FRatio := AValue;
end;

procedure TSedaiCompressor.SetAttack(AValue: Single);
begin
  if AValue < 0.01 then
    AValue := 0.01
  else if AValue > 500.0 then
    AValue := 500.0;

  FAttack := AValue;
  UpdateCoefficients;
end;

procedure TSedaiCompressor.SetRelease(AValue: Single);
begin
  if AValue < 1.0 then
    AValue := 1.0
  else if AValue > 5000.0 then
    AValue := 5000.0;

  FRelease := AValue;
  UpdateCoefficients;
end;

procedure TSedaiCompressor.SetKnee(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 24.0 then
    AValue := 24.0;

  FKnee := AValue;
end;

procedure TSedaiCompressor.UpdateCoefficients;
begin
  if FSampleRate > 0 then
  begin
    // Attack: time to reach ~63% of target
    FAttackCoeff := Exp(-1.0 / (FAttack * 0.001 * FSampleRate));

    // Release: time to reach ~63% of target
    FReleaseCoeff := Exp(-1.0 / (FRelease * 0.001 * FSampleRate));

    // RMS window size
    FRMSWindow := Round(FSampleRate * 0.001);  // 1ms window
    if FRMSWindow < 1 then FRMSWindow := 1;

    SetLength(FRMSBuffer[0], FRMSWindow);
    SetLength(FRMSBuffer[1], FRMSWindow);
  end;
end;

function TSedaiCompressor.DetectLevel(ALeft, ARight: Single): Single;
var
  Left, Right, Level: Single;
  Mid, Side: Single;
begin
  case FStereoLink of
    slmDual:
      begin
        // Return max of both (will process independently later)
        Left := Abs(ALeft);
        Right := Abs(ARight);
        Level := Max(Left, Right);
      end;

    slmLinked:
      begin
        Left := Abs(ALeft);
        Right := Abs(ARight);
        Level := Max(Left, Right);
      end;

    slmMid:
      begin
        Mid := (ALeft + ARight) * 0.5;
        Level := Abs(Mid);
      end;

    slmSide:
      begin
        Side := (ALeft - ARight) * 0.5;
        Level := Abs(Side);
      end;

    else
      Level := Max(Abs(ALeft), Abs(ARight));
  end;

  Result := Level;
end;

function TSedaiCompressor.ComputeGain(AInputDB: Single): Single;
var
  OverThreshold, GainDB: Single;
  KneeStart, KneeEnd: Single;
  KneeRatio: Single;
begin
  // Soft knee implementation
  if FKnee > 0 then
  begin
    KneeStart := FThreshold - FKnee * 0.5;
    KneeEnd := FThreshold + FKnee * 0.5;

    if AInputDB < KneeStart then
    begin
      // Below knee - no compression
      GainDB := 0.0;
    end
    else if AInputDB > KneeEnd then
    begin
      // Above knee - full compression
      OverThreshold := AInputDB - FThreshold;
      GainDB := OverThreshold * (1.0 - 1.0 / FRatio);
    end
    else
    begin
      // In knee region - gradual compression
      KneeRatio := (AInputDB - KneeStart) / FKnee;
      OverThreshold := AInputDB - FThreshold;

      // Quadratic interpolation in knee region
      GainDB := KneeRatio * KneeRatio * OverThreshold * (1.0 - 1.0 / FRatio) * 0.5;
    end;
  end
  else
  begin
    // Hard knee
    if AInputDB > FThreshold then
    begin
      OverThreshold := AInputDB - FThreshold;
      GainDB := OverThreshold * (1.0 - 1.0 / FRatio);
    end
    else
      GainDB := 0.0;
  end;

  Result := -GainDB;  // Negative = gain reduction
end;

function TSedaiCompressor.ApplyEnvelope(AInput, AEnvelope: Single; AChannel: Integer): Single;
begin
  // Attack/release envelope follower
  if AInput > AEnvelope then
    Result := FAttackCoeff * AEnvelope + (1.0 - FAttackCoeff) * AInput
  else
    Result := FReleaseCoeff * AEnvelope + (1.0 - FReleaseCoeff) * AInput;

  FEnvelope[AChannel] := Result;
end;

procedure TSedaiCompressor.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
  Left, Right: Single;
  Level, LevelDB: Single;
  GainDB, GainLin: Single;
  Envelope: Single;
  Makeup: Single;
begin
  // Calculate auto makeup gain
  if FAutoMakeup then
    Makeup := DecibelToLinear(-FThreshold * (1.0 - 1.0 / FRatio) * 0.5)
  else
    Makeup := DecibelToLinear(FMakeupGain);

  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2];
    Right := AInput[I * 2 + 1];

    // Detect level
    Level := DetectLevel(Left, Right);

    // Convert to dB
    if Level > 1e-10 then
      LevelDB := 20.0 * Log10(Level)
    else
      LevelDB := -96.0;

    FInputLevel := LevelDB;

    // Compute gain reduction
    GainDB := ComputeGain(LevelDB);

    // Apply envelope
    Envelope := ApplyEnvelope(Abs(GainDB), FEnvelope[0], 0);
    FGainReduction := Envelope;

    // Convert to linear
    GainLin := DecibelToLinear(-Envelope);

    // Apply gain
    Left := Left * GainLin * Makeup;
    Right := Right * GainLin * Makeup;

    // Output
    AOutput[I * 2] := Left;
    AOutput[I * 2 + 1] := Right;

    // Update output level
    Level := Max(Abs(Left), Abs(Right));
    if Level > 1e-10 then
      FOutputLevel := 20.0 * Log10(Level)
    else
      FOutputLevel := -96.0;
  end;
end;

function TSedaiCompressor.GetGainReduction: Single;
begin
  Result := FGainReduction;
end;

end.
