{*
 * Sedai Audio Foundation - Bus Classes
 *
 * TSedaiAuxBus, TSedaiGroupBus, and TSedaiMasterBus provide
 * audio routing and summing for the mixing console.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiBus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiFilter, SedaiEffect;

const
  MAX_BUS_INSERTS = 8;

type
  // Bus type
  TBusType = (
    btAux,                // Aux/effects return bus
    btGroup,              // Group/submix bus
    btMaster              // Master output bus
  );

  { TSedaiBus }
  // Base class for all bus types
  TSedaiBus = class(TSedaiSignalProcessor)
  private
    FBusType: TBusType;
    FName: string;
    FIndex: Integer;

    // Gain staging
    FVolume: Single;              // dB
    FPan: Single;                 // -1 to +1 (for stereo balance)

    // Mute/Solo
    FMuted: Boolean;
    FSoloed: Boolean;

    // Insert effects
    FInserts: array[0..MAX_BUS_INSERTS-1] of TSedaiEffect;
    FInsertBypass: array[0..MAX_BUS_INSERTS-1] of Boolean;
    FInsertCount: Integer;

    // Metering
    FPeakL, FPeakR: Single;
    FMeterDecay: Single;

    // Summing buffer
    FSumBuffer: array of Single;
    FSumFrameCount: Integer;
    FHasInput: Boolean;

    // Smoothing
    FVolumeSmooth: Single;
    FSmoothCoeff: Single;

    procedure SetVolume(AValue: Single);
    procedure UpdateMeters(ALeft, ARight: Single);

  protected
    procedure ProcessInserts(ABuffer: PSingle; AFrameCount: Integer);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure SampleRateChanged; override;

    // Summing interface
    procedure ClearSum(AFrameCount: Integer);
    procedure AddToSum(AInput: PSingle; AFrameCount: Integer);

    // Insert management
    function AddInsert(AEffect: TSedaiEffect): Integer;
    procedure RemoveInsert(AIndex: Integer);
    procedure ClearInserts;
    procedure SetInsertBypass(AIndex: Integer; ABypass: Boolean);
    function GetInsert(AIndex: Integer): TSedaiEffect;

    // Process summed audio
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Metering
    procedure ResetMeters;
    procedure GetMeterLevels(out ALeft, ARight: Single);

    // Properties
    property BusType: TBusType read FBusType write FBusType;
    property Name: string read FName write FName;
    property Index: Integer read FIndex write FIndex;
    property Volume: Single read FVolume write SetVolume;
    property Pan: Single read FPan write FPan;
    property Muted: Boolean read FMuted write FMuted;
    property Soloed: Boolean read FSoloed write FSoloed;
    property InsertCount: Integer read FInsertCount;
    property PeakL: Single read FPeakL;
    property PeakR: Single read FPeakR;
  end;

  { TSedaiAuxBus }
  // Auxiliary/effects return bus
  TSedaiAuxBus = class(TSedaiBus)
  private
    FReturnLevel: Single;         // Return level (0-1)
    FPreFader: Boolean;           // Pre/post fader return

  public
    constructor Create; override;

    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    property ReturnLevel: Single read FReturnLevel write FReturnLevel;
    property PreFader: Boolean read FPreFader write FPreFader;
  end;

  { TSedaiGroupBus }
  // Group/submix bus
  TSedaiGroupBus = class(TSedaiBus)
  private
    FOutputBus: Integer;          // Destination bus (-1 = master)

  public
    constructor Create; override;

    property OutputBus: Integer read FOutputBus write FOutputBus;
  end;

  { TSedaiMasterBus }
  // Master output bus with limiting
  TSedaiMasterBus = class(TSedaiBus)
  private
    FLimiterEnabled: Boolean;
    FLimiterThreshold: Single;    // dB
    FLimiterRelease: Single;      // ms

    // Limiter state
    FGainReduction: Single;
    FGRAttack: Single;
    FGRRelease: Single;

    // Dithering
    FDitherEnabled: Boolean;
    FDitherBits: Integer;

    procedure ApplyLimiter(var ALeft, ARight: Single);
    function GenerateDither: Single;

  public
    constructor Create; override;

    procedure Reset; override;
    procedure SampleRateChanged; override;

    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Properties
    property LimiterEnabled: Boolean read FLimiterEnabled write FLimiterEnabled;
    property LimiterThreshold: Single read FLimiterThreshold write FLimiterThreshold;
    property LimiterRelease: Single read FLimiterRelease write FLimiterRelease;
    property GainReduction: Single read FGainReduction;
    property DitherEnabled: Boolean read FDitherEnabled write FDitherEnabled;
    property DitherBits: Integer read FDitherBits write FDitherBits;
  end;

implementation

{ TSedaiBus }

constructor TSedaiBus.Create;
var
  I: Integer;
begin
  inherited Create;

  FBusType := btGroup;
  FName := 'Bus';
  FIndex := 0;

  FVolume := 0.0;
  FPan := 0.0;
  FMuted := False;
  FSoloed := False;

  FInsertCount := 0;
  for I := 0 to MAX_BUS_INSERTS - 1 do
  begin
    FInserts[I] := nil;
    FInsertBypass[I] := False;
  end;

  FPeakL := 0.0;
  FPeakR := 0.0;
  FMeterDecay := 0.9995;

  FSumFrameCount := 0;
  FHasInput := False;

  FVolumeSmooth := 1.0;
  FSmoothCoeff := 0.999;
end;

destructor TSedaiBus.Destroy;
begin
  ClearInserts;
  SetLength(FSumBuffer, 0);
  inherited Destroy;
end;

procedure TSedaiBus.Reset;
begin
  inherited Reset;

  FVolumeSmooth := DecibelToLinear(FVolume);
  ResetMeters;
  FHasInput := False;
end;

procedure TSedaiBus.SampleRateChanged;
var
  I: Integer;
  Rate: Cardinal;
begin
  inherited SampleRateChanged;
  Rate := SampleRate;

  for I := 0 to MAX_BUS_INSERTS - 1 do
    if Assigned(FInserts[I]) then
      FInserts[I].SetSampleRate(Rate);

  if Rate > 0 then
  begin
    FSmoothCoeff := Exp(-1.0 / (0.01 * Rate));
    FMeterDecay := Exp(-1.0 / (0.3 * Rate));
  end;
end;

procedure TSedaiBus.SetVolume(AValue: Single);
begin
  if AValue < -96.0 then AValue := -96.0;
  if AValue > 12.0 then AValue := 12.0;
  FVolume := AValue;
end;

procedure TSedaiBus.UpdateMeters(ALeft, ARight: Single);
var
  AbsL, AbsR: Single;
begin
  AbsL := Abs(ALeft);
  AbsR := Abs(ARight);

  if AbsL > FPeakL then
    FPeakL := AbsL
  else
    FPeakL := FPeakL * FMeterDecay;

  if AbsR > FPeakR then
    FPeakR := AbsR
  else
    FPeakR := FPeakR * FMeterDecay;
end;

procedure TSedaiBus.ProcessInserts(ABuffer: PSingle; AFrameCount: Integer);
var
  I, J: Integer;
  TempOut: array of Single;
begin
  SetLength(TempOut, AFrameCount * 2);

  for J := 0 to MAX_BUS_INSERTS - 1 do
  begin
    if Assigned(FInserts[J]) and not FInsertBypass[J] then
    begin
      FInserts[J].ProcessBlock(ABuffer, @TempOut[0], AFrameCount);

      for I := 0 to AFrameCount * 2 - 1 do
        ABuffer[I] := TempOut[I];
    end;
  end;
end;

procedure TSedaiBus.ClearSum(AFrameCount: Integer);
var
  I: Integer;
begin
  if Length(FSumBuffer) < AFrameCount * 2 then
    SetLength(FSumBuffer, AFrameCount * 2);

  for I := 0 to AFrameCount * 2 - 1 do
    FSumBuffer[I] := 0.0;

  FSumFrameCount := AFrameCount;
  FHasInput := False;
end;

procedure TSedaiBus.AddToSum(AInput: PSingle; AFrameCount: Integer);
var
  I: Integer;
begin
  if AFrameCount <> FSumFrameCount then Exit;

  for I := 0 to AFrameCount * 2 - 1 do
    FSumBuffer[I] := FSumBuffer[I] + AInput[I];

  FHasInput := True;
end;

function TSedaiBus.AddInsert(AEffect: TSedaiEffect): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to MAX_BUS_INSERTS - 1 do
  begin
    if FInserts[I] = nil then
    begin
      FInserts[I] := AEffect;
      FInserts[I].SetSampleRate(FSampleRate);
      FInsertBypass[I] := False;
      Inc(FInsertCount);
      Result := I;
      Exit;
    end;
  end;
end;

procedure TSedaiBus.RemoveInsert(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= MAX_BUS_INSERTS) then Exit;
  if FInserts[AIndex] = nil then Exit;

  FInserts[AIndex] := nil;
  FInsertBypass[AIndex] := False;
  Dec(FInsertCount);
end;

procedure TSedaiBus.ClearInserts;
var
  I: Integer;
begin
  for I := 0 to MAX_BUS_INSERTS - 1 do
  begin
    FInserts[I] := nil;
    FInsertBypass[I] := False;
  end;
  FInsertCount := 0;
end;

procedure TSedaiBus.SetInsertBypass(AIndex: Integer; ABypass: Boolean);
begin
  if (AIndex >= 0) and (AIndex < MAX_BUS_INSERTS) then
    FInsertBypass[AIndex] := ABypass;
end;

function TSedaiBus.GetInsert(AIndex: Integer): TSedaiEffect;
begin
  if (AIndex >= 0) and (AIndex < MAX_BUS_INSERTS) then
    Result := FInserts[AIndex]
  else
    Result := nil;
end;

procedure TSedaiBus.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
  Left, Right, Vol: Single;
begin
  // If we have summed input, use that; otherwise use AInput
  if FHasInput then
  begin
    for I := 0 to AFrameCount * 2 - 1 do
      AOutput[I] := FSumBuffer[I];
  end
  else if AInput <> nil then
  begin
    for I := 0 to AFrameCount * 2 - 1 do
      AOutput[I] := AInput[I];
  end
  else
  begin
    for I := 0 to AFrameCount * 2 - 1 do
      AOutput[I] := 0.0;
    Exit;
  end;

  // Process inserts
  ProcessInserts(AOutput, AFrameCount);

  // Apply volume and pan
  for I := 0 to AFrameCount - 1 do
  begin
    // Smooth volume
    Vol := DecibelToLinear(FVolume);
    FVolumeSmooth := FSmoothCoeff * FVolumeSmooth + (1.0 - FSmoothCoeff) * Vol;

    Left := AOutput[I * 2];
    Right := AOutput[I * 2 + 1];

    if FMuted then
    begin
      Left := 0.0;
      Right := 0.0;
    end
    else
    begin
      Left := Left * FVolumeSmooth;
      Right := Right * FVolumeSmooth;
    end;

    AOutput[I * 2] := Left;
    AOutput[I * 2 + 1] := Right;

    UpdateMeters(Left, Right);
  end;
end;

procedure TSedaiBus.ResetMeters;
begin
  FPeakL := 0.0;
  FPeakR := 0.0;
end;

procedure TSedaiBus.GetMeterLevels(out ALeft, ARight: Single);
begin
  ALeft := FPeakL;
  ARight := FPeakR;
end;

{ TSedaiAuxBus }

constructor TSedaiAuxBus.Create;
begin
  inherited Create;

  FBusType := btAux;
  FName := 'Aux';
  FReturnLevel := 1.0;
  FPreFader := False;
end;

procedure TSedaiAuxBus.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
begin
  // First do standard bus processing
  inherited ProcessBlock(AInput, AOutput, AFrameCount);

  // Apply return level
  for I := 0 to AFrameCount * 2 - 1 do
    AOutput[I] := AOutput[I] * FReturnLevel;
end;

{ TSedaiGroupBus }

constructor TSedaiGroupBus.Create;
begin
  inherited Create;

  FBusType := btGroup;
  FName := 'Group';
  FOutputBus := -1;  // Master
end;

{ TSedaiMasterBus }

constructor TSedaiMasterBus.Create;
begin
  inherited Create;

  FBusType := btMaster;
  FName := 'Master';

  FLimiterEnabled := True;
  FLimiterThreshold := -0.3;  // -0.3 dB
  FLimiterRelease := 100.0;   // 100 ms

  FGainReduction := 1.0;
  FGRAttack := 0.0;
  FGRRelease := 0.0;

  FDitherEnabled := False;
  FDitherBits := 16;
end;

procedure TSedaiMasterBus.Reset;
begin
  inherited Reset;

  FGainReduction := 1.0;
end;

procedure TSedaiMasterBus.SampleRateChanged;
var
  Rate: Cardinal;
begin
  inherited SampleRateChanged;
  Rate := SampleRate;

  if Rate > 0 then
  begin
    // Fast attack (0.1ms), variable release
    FGRAttack := Exp(-1.0 / (0.0001 * Rate));
    FGRRelease := Exp(-1.0 / (FLimiterRelease * 0.001 * Rate));
  end;
end;

procedure TSedaiMasterBus.ApplyLimiter(var ALeft, ARight: Single);
var
  Peak, Threshold, Target: Single;
begin
  if not FLimiterEnabled then Exit;

  Peak := Max(Abs(ALeft), Abs(ARight));
  Threshold := DecibelToLinear(FLimiterThreshold);

  if Peak > Threshold then
  begin
    Target := Threshold / Peak;
    // Fast attack
    if Target < FGainReduction then
      FGainReduction := FGRAttack * FGainReduction + (1.0 - FGRAttack) * Target;
  end
  else
  begin
    // Slow release
    if FGainReduction < 1.0 then
      FGainReduction := FGRRelease * FGainReduction + (1.0 - FGRRelease) * 1.0;
  end;

  ALeft := ALeft * FGainReduction;
  ARight := ARight * FGainReduction;
end;

function TSedaiMasterBus.GenerateDither: Single;
var
  Scale: Single;
begin
  // Triangular PDF dither
  Scale := 1.0 / (1 shl (FDitherBits - 1));
  Result := (Random - 0.5 + Random - 0.5) * Scale;
end;

procedure TSedaiMasterBus.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
  Left, Right: Single;
begin
  // First do standard bus processing
  inherited ProcessBlock(AInput, AOutput, AFrameCount);

  // Apply limiter and dither
  for I := 0 to AFrameCount - 1 do
  begin
    Left := AOutput[I * 2];
    Right := AOutput[I * 2 + 1];

    // Limiter
    ApplyLimiter(Left, Right);

    // Dither
    if FDitherEnabled then
    begin
      Left := Left + GenerateDither;
      Right := Right + GenerateDither;
    end;

    // Final clip (safety)
    if Left > 1.0 then Left := 1.0
    else if Left < -1.0 then Left := -1.0;

    if Right > 1.0 then Right := 1.0
    else if Right < -1.0 then Right := -1.0;

    AOutput[I * 2] := Left;
    AOutput[I * 2 + 1] := Right;
  end;
end;

end.
