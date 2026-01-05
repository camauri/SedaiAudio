{*
 * Sedai Audio Foundation - Envelope Generator
 *
 * TSedaiEnvelope provides ADSR envelope with multiple curve types
 * (linear, exponential, logarithmic, S-curve). Supports retrigger,
 * legato, and SID-authentic mode with rate counter emulation.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiEnvelope;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject;

type
  { TSedaiModulator }
  // Abstract base class for all modulators (envelopes, LFOs, etc.)
  TSedaiModulator = class(TSedaiAudioObject)
  protected
    FOutput: Single;              // Current output value (0.0 - 1.0)

  public
    constructor Create; override;

    // Process one sample and return output
    function Process: Single; virtual; abstract;

    // Process a block of samples
    procedure ProcessBlock(AOutput: PSingle; AFrameCount: Integer); virtual;

    // Current output value
    property Output: Single read FOutput;
  end;

  { TSedaiEnvelope }
  // ADSR envelope generator with multiple curve types
  TSedaiEnvelope = class(TSedaiModulator)
  private
    // ADSR times (in seconds)
    FAttackTime: Single;
    FDecayTime: Single;
    FSustainLevel: Single;        // 0.0 - 1.0
    FReleaseTime: Single;

    // Curve types for each stage
    FAttackCurve: TEnvelopeCurve;
    FDecayCurve: TEnvelopeCurve;
    FReleaseCurve: TEnvelopeCurve;

    // State
    FState: TEnvelopeState;
    FLevel: Single;               // Current envelope level
    FTargetLevel: Single;         // Target level for current stage
    FReleaseLevel: Single;        // Level when release started

    // Internal coefficients (calculated from times)
    FAttackCoeff: Single;
    FDecayCoeff: Single;
    FReleaseCoeff: Single;

    // Curve parameters
    FAttackTCO: Single;           // Time constant offset for attack curve
    FDecayTCO: Single;            // Time constant offset for decay curve
    FReleaseTCO: Single;          // Time constant offset for release curve

    // Mode options
    FRetrigger: Boolean;          // Reset level on retrigger
    FLegato: Boolean;             // Continue from current level on retrigger

    // SID-authentic mode
    FSIDMode: Boolean;
    FSIDRateCounter: Integer;     // SID envelope rate counter
    FSIDExponentialCounter: Byte; // SID exponential counter
    FSIDADSRBits: Byte;           // Raw 4-bit ADSR values (SID style)

    procedure SetAttackTime(AValue: Single);
    procedure SetDecayTime(AValue: Single);
    procedure SetSustainLevel(AValue: Single);
    procedure SetReleaseTime(AValue: Single);
    procedure SetAttackCurve(AValue: TEnvelopeCurve);
    procedure SetDecayCurve(AValue: TEnvelopeCurve);
    procedure SetReleaseCurve(AValue: TEnvelopeCurve);

    procedure CalculateCoefficients;
    function CalculateCoeff(ATimeSeconds: Single; ATargetRatio: Single): Single;

    // Apply curve to linear progress
    function ApplyCurve(AProgress: Single; ACurve: TEnvelopeCurve; AIsDecay: Boolean): Single;

    // SID envelope processing
    function ProcessSIDEnvelope: Single;

  public
    constructor Create; override;

    // Reset envelope to idle
    procedure Reset; override;

    // Trigger envelope (note on)
    procedure Trigger;

    // Release envelope (note off)
    procedure Release;

    // Force release with custom time
    procedure ForceRelease(AReleaseTime: Single);

    // Process one sample
    function Process: Single; override;

    // Check if envelope is finished (idle after release)
    function IsFinished: Boolean;

    // Check if envelope is active (not idle)
    function IsActive: Boolean;

    // Get current state
    function GetState: TEnvelopeState;

    // Set ADSR from SID-style 4-bit values (0-15 each)
    procedure SetSIDADSR(AAttack, ADecay, ASustain, ARelease: Byte);

    // Properties - Times
    property AttackTime: Single read FAttackTime write SetAttackTime;
    property DecayTime: Single read FDecayTime write SetDecayTime;
    property SustainLevel: Single read FSustainLevel write SetSustainLevel;
    property ReleaseTime: Single read FReleaseTime write SetReleaseTime;

    // Properties - Curves
    property AttackCurve: TEnvelopeCurve read FAttackCurve write SetAttackCurve;
    property DecayCurve: TEnvelopeCurve read FDecayCurve write SetDecayCurve;
    property ReleaseCurve: TEnvelopeCurve read FReleaseCurve write SetReleaseCurve;

    // Properties - Mode
    property Retrigger: Boolean read FRetrigger write FRetrigger;
    property Legato: Boolean read FLegato write FLegato;
    property SIDMode: Boolean read FSIDMode write FSIDMode;

    // Properties - State
    property State: TEnvelopeState read FState;
    property Level: Single read FLevel;
  end;

implementation

const
  // SID envelope rate table (PAL values in clock cycles)
  SID_RATE_TABLE: array[0..15] of Integer = (
    9,       // 0: 2ms
    32,      // 1: 8ms
    63,      // 2: 16ms
    95,      // 3: 24ms
    149,     // 4: 38ms
    220,     // 5: 56ms
    267,     // 6: 68ms
    313,     // 7: 80ms
    392,     // 8: 100ms
    977,     // 9: 250ms
    1954,    // 10: 500ms
    3126,    // 11: 800ms
    3907,    // 12: 1s
    11720,   // 13: 3s
    19532,   // 14: 5s
    31251    // 15: 8s
  );

  // SID exponential counter periods
  SID_EXP_PERIODS: array[0..255] of Byte = (
    1, 30, 30, 30, 30, 30, 30, 16,
    16, 16, 16, 16, 16, 16, 16, 8,
    8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 4,
    4, 4, 4, 4, 4, 4, 4, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 1
  );

{ TSedaiModulator }

constructor TSedaiModulator.Create;
begin
  inherited Create;
  FOutput := 0.0;
end;

procedure TSedaiModulator.ProcessBlock(AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
begin
  for I := 0 to AFrameCount - 1 do
    AOutput[I] := Process;
end;

{ TSedaiEnvelope }

constructor TSedaiEnvelope.Create;
begin
  inherited Create;

  // Default ADSR values
  FAttackTime := 0.01;     // 10ms
  FDecayTime := 0.1;       // 100ms
  FSustainLevel := 0.7;    // 70%
  FReleaseTime := 0.3;     // 300ms

  // Default curves
  FAttackCurve := ecLinear;
  FDecayCurve := ecExponential;
  FReleaseCurve := ecExponential;

  // State
  FState := esIdle;
  FLevel := 0.0;
  FTargetLevel := 0.0;
  FReleaseLevel := 0.0;

  // Mode
  FRetrigger := True;
  FLegato := False;

  // SID mode
  FSIDMode := False;
  FSIDRateCounter := 0;
  FSIDExponentialCounter := 0;
  FSIDADSRBits := 0;

  // Time constant offsets for analog-style curves
  FAttackTCO := 0.99999;
  FDecayTCO := 0.36788;    // e^-1
  FReleaseTCO := 0.36788;

  CalculateCoefficients;
end;

procedure TSedaiEnvelope.Reset;
begin
  inherited Reset;

  FState := esIdle;
  FLevel := 0.0;
  FOutput := 0.0;
  FSIDRateCounter := 0;
  FSIDExponentialCounter := 0;
end;

procedure TSedaiEnvelope.SetAttackTime(AValue: Single);
begin
  if AValue < 0.0001 then
    AValue := 0.0001
  else if AValue > 30.0 then
    AValue := 30.0;

  FAttackTime := AValue;
  CalculateCoefficients;
end;

procedure TSedaiEnvelope.SetDecayTime(AValue: Single);
begin
  if AValue < 0.0001 then
    AValue := 0.0001
  else if AValue > 30.0 then
    AValue := 30.0;

  FDecayTime := AValue;
  CalculateCoefficients;
end;

procedure TSedaiEnvelope.SetSustainLevel(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;

  FSustainLevel := AValue;
end;

procedure TSedaiEnvelope.SetReleaseTime(AValue: Single);
begin
  if AValue < 0.0001 then
    AValue := 0.0001
  else if AValue > 30.0 then
    AValue := 30.0;

  FReleaseTime := AValue;
  CalculateCoefficients;
end;

procedure TSedaiEnvelope.SetAttackCurve(AValue: TEnvelopeCurve);
begin
  FAttackCurve := AValue;
end;

procedure TSedaiEnvelope.SetDecayCurve(AValue: TEnvelopeCurve);
begin
  FDecayCurve := AValue;
end;

procedure TSedaiEnvelope.SetReleaseCurve(AValue: TEnvelopeCurve);
begin
  FReleaseCurve := AValue;
end;

procedure TSedaiEnvelope.CalculateCoefficients;
begin
  if FSampleRate > 0 then
  begin
    FAttackCoeff := CalculateCoeff(FAttackTime, FAttackTCO);
    FDecayCoeff := CalculateCoeff(FDecayTime, FDecayTCO);
    FReleaseCoeff := CalculateCoeff(FReleaseTime, FReleaseTCO);
  end;
end;

function TSedaiEnvelope.CalculateCoeff(ATimeSeconds: Single; ATargetRatio: Single): Single;
begin
  // Calculate coefficient for exponential decay to reach target ratio in given time
  if (ATimeSeconds > 0) and (FSampleRate > 0) then
    Result := Exp(-Ln((1.0 + ATargetRatio) / ATargetRatio) / (ATimeSeconds * FSampleRate))
  else
    Result := 0.0;
end;

function TSedaiEnvelope.ApplyCurve(AProgress: Single; ACurve: TEnvelopeCurve;
                                   AIsDecay: Boolean): Single;
begin
  case ACurve of
    ecLinear:
      Result := AProgress;

    ecExponential:
      begin
        // Exponential curve (fast start, slow end for attack; opposite for decay)
        if AIsDecay then
          Result := 1.0 - Power(1.0 - AProgress, 3.0)
        else
          Result := Power(AProgress, 3.0);
      end;

    ecLogarithmic:
      begin
        // Logarithmic curve (slow start, fast end for attack; opposite for decay)
        if AIsDecay then
          Result := Power(AProgress, 0.33)
        else
          Result := 1.0 - Power(1.0 - AProgress, 0.33);
      end;

    ecSCurve:
      begin
        // S-curve (smooth start and end)
        Result := AProgress * AProgress * (3.0 - 2.0 * AProgress);
      end;

    else
      Result := AProgress;
  end;
end;

function TSedaiEnvelope.ProcessSIDEnvelope: Single;
var
  Rate: Integer;
  EnvLevel: Byte;
begin
  EnvLevel := Round(FLevel * 255);

  case FState of
    esAttack:
      begin
        Rate := SID_RATE_TABLE[FSIDADSRBits shr 4];
        Inc(FSIDRateCounter);

        if FSIDRateCounter >= Rate then
        begin
          FSIDRateCounter := 0;
          Inc(EnvLevel);

          if EnvLevel >= 255 then
          begin
            EnvLevel := 255;
            FState := esDecay;
          end;
        end;
      end;

    esDecay:
      begin
        Rate := SID_RATE_TABLE[(FSIDADSRBits shr 4) and $0F];
        Inc(FSIDRateCounter);

        if FSIDRateCounter >= Rate then
        begin
          FSIDRateCounter := 0;
          Inc(FSIDExponentialCounter);

          if FSIDExponentialCounter >= SID_EXP_PERIODS[EnvLevel] then
          begin
            FSIDExponentialCounter := 0;

            if EnvLevel > 0 then
              Dec(EnvLevel);

            // Check if we've reached sustain level
            if EnvLevel <= Round(FSustainLevel * 255) then
            begin
              FState := esSustain;
              EnvLevel := Round(FSustainLevel * 255);
            end;
          end;
        end;
      end;

    esSustain:
      begin
        // Hold at sustain level
        EnvLevel := Round(FSustainLevel * 255);
      end;

    esRelease:
      begin
        Rate := SID_RATE_TABLE[FSIDADSRBits and $0F];
        Inc(FSIDRateCounter);

        if FSIDRateCounter >= Rate then
        begin
          FSIDRateCounter := 0;
          Inc(FSIDExponentialCounter);

          if FSIDExponentialCounter >= SID_EXP_PERIODS[EnvLevel] then
          begin
            FSIDExponentialCounter := 0;

            if EnvLevel > 0 then
              Dec(EnvLevel)
            else
              FState := esIdle;
          end;
        end;
      end;

    esIdle:
      EnvLevel := 0;
  end;

  FLevel := EnvLevel / 255.0;
  Result := FLevel;
end;

function TSedaiEnvelope.Process: Single;
var
  Delta: Single;
begin
  // SID mode uses different processing
  if FSIDMode then
  begin
    FOutput := ProcessSIDEnvelope;
    Result := FOutput;
    Exit;
  end;

  // Standard envelope processing
  case FState of
    esIdle:
      FLevel := 0.0;

    esAttack:
      begin
        // Attack: rise from current level to 1.0
        FLevel := FLevel + FAttackCoeff * (1.0 + FAttackTCO - FLevel);

        if FLevel >= 1.0 then
        begin
          FLevel := 1.0;
          FState := esDecay;
        end;
      end;

    esDecay:
      begin
        // Decay: fall from 1.0 to sustain level
        FLevel := FLevel + FDecayCoeff * (FSustainLevel - FDecayTCO - FLevel);

        if FLevel <= FSustainLevel then
        begin
          FLevel := FSustainLevel;
          FState := esSustain;
        end;
      end;

    esSustain:
      begin
        // Sustain: hold at sustain level
        FLevel := FSustainLevel;
      end;

    esRelease:
      begin
        // Release: fall from release level to 0
        FLevel := FLevel + FReleaseCoeff * (-FReleaseTCO - FLevel);

        if FLevel <= 0.001 then
        begin
          FLevel := 0.0;
          FState := esIdle;
        end;
      end;
  end;

  // Clamp output
  if FLevel < 0.0 then
    FLevel := 0.0
  else if FLevel > 1.0 then
    FLevel := 1.0;

  FOutput := FLevel;
  Result := FOutput;
end;

procedure TSedaiEnvelope.Trigger;
begin
  if FRetrigger or (FState = esIdle) then
  begin
    // Start from zero or current level based on legato
    if FLegato and (FLevel > 0) then
      // Continue from current level
    else
      FLevel := 0.0;

    FState := esAttack;
    FSIDRateCounter := 0;
    FSIDExponentialCounter := 0;
  end;
end;

procedure TSedaiEnvelope.Release;
begin
  if FState <> esIdle then
  begin
    FReleaseLevel := FLevel;
    FState := esRelease;
    FSIDRateCounter := 0;
    FSIDExponentialCounter := 0;
  end;
end;

procedure TSedaiEnvelope.ForceRelease(AReleaseTime: Single);
begin
  FReleaseTime := AReleaseTime;
  CalculateCoefficients;
  Release;
end;

function TSedaiEnvelope.IsFinished: Boolean;
begin
  Result := FState = esIdle;
end;

function TSedaiEnvelope.IsActive: Boolean;
begin
  Result := FState <> esIdle;
end;

function TSedaiEnvelope.GetState: TEnvelopeState;
begin
  Result := FState;
end;

procedure TSedaiEnvelope.SetSIDADSR(AAttack, ADecay, ASustain, ARelease: Byte);
begin
  // Store raw 4-bit values (clamped to 0-15)
  if AAttack > 15 then AAttack := 15;
  if ADecay > 15 then ADecay := 15;
  if ASustain > 15 then ASustain := 15;
  if ARelease > 15 then ARelease := 15;

  FSIDADSRBits := (AAttack shl 4) or ADecay;

  // Set sustain level (0-15 maps to 0-1)
  FSustainLevel := ASustain / 15.0;

  // Store release in lower nibble for SID processing
  FSIDADSRBits := (FSIDADSRBits and $F0) or ARelease;

  FSIDMode := True;
end;

end.
