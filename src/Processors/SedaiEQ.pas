{*
 * Sedai Audio Foundation - Equalizer
 *
 * TSedaiEQ provides parametric equalizer with multiple bands.
 * Each band supports low shelf, high shelf, peaking, low cut,
 * and high cut filter types.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiEQ;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiFilter;

const
  MAX_EQ_BANDS = 8;

type
  // EQ band type
  TEQBandType = (
    eqLowCut,         // High-pass filter
    eqLowShelf,       // Low shelf
    eqPeaking,        // Parametric peaking
    eqHighShelf,      // High shelf
    eqHighCut         // Low-pass filter
  );

  { TEQBand }
  // Single EQ band
  TEQBand = record
    Enabled: Boolean;
    BandType: TEQBandType;
    Frequency: Single;            // Center/corner frequency
    Gain: Single;                 // Gain in dB (for shelf/peaking)
    Q: Single;                    // Q factor / bandwidth

    // Biquad coefficients
    A0, A1, A2: Single;
    B1, B2: Single;

    // Filter state (stereo)
    X1, X2: array[0..1] of Single;
    Y1, Y2: array[0..1] of Single;
  end;

  { TSedaiEQ }
  // Multi-band parametric equalizer
  TSedaiEQ = class(TSedaiSignalProcessor)
  private
    FBands: array[0..MAX_EQ_BANDS-1] of TEQBand;
    FBandCount: Integer;
    FOutputGain: Single;

    procedure CalculateBandCoefficients(var ABand: TEQBand);
    function ProcessBand(var ABand: TEQBand; AInput: Single; AChannel: Integer): Single;

  public
    constructor Create; override;

    procedure Reset; override;

    // Band configuration
    procedure SetBandEnabled(AIndex: Integer; AEnabled: Boolean);
    procedure SetBandType(AIndex: Integer; AType: TEQBandType);
    procedure SetBandFrequency(AIndex: Integer; AFreq: Single);
    procedure SetBandGain(AIndex: Integer; AGain: Single);
    procedure SetBandQ(AIndex: Integer; AQ: Single);

    // Configure band with all parameters
    procedure ConfigureBand(AIndex: Integer; AType: TEQBandType;
                           AFreq, AGain, AQ: Single; AEnabled: Boolean = True);

    // Get band info
    function GetBandEnabled(AIndex: Integer): Boolean;
    function GetBandFrequency(AIndex: Integer): Single;
    function GetBandGain(AIndex: Integer): Single;
    function GetBandQ(AIndex: Integer): Single;

    // Process
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Presets
    procedure LoadPresetFlat;
    procedure LoadPresetVocal;
    procedure LoadPresetBass;
    procedure LoadPresetBrightness;

    // Properties
    property BandCount: Integer read FBandCount;
    property OutputGain: Single read FOutputGain write FOutputGain;
  end;

implementation

{ TSedaiEQ }

constructor TSedaiEQ.Create;
var
  I, J: Integer;
begin
  inherited Create;

  FBandCount := MAX_EQ_BANDS;
  FOutputGain := 1.0;

  // Initialize all bands
  for I := 0 to MAX_EQ_BANDS - 1 do
  begin
    FBands[I].Enabled := False;
    FBands[I].BandType := eqPeaking;
    FBands[I].Frequency := 1000.0;
    FBands[I].Gain := 0.0;
    FBands[I].Q := 1.0;

    FBands[I].A0 := 1.0;
    FBands[I].A1 := 0.0;
    FBands[I].A2 := 0.0;
    FBands[I].B1 := 0.0;
    FBands[I].B2 := 0.0;

    for J := 0 to 1 do
    begin
      FBands[I].X1[J] := 0.0;
      FBands[I].X2[J] := 0.0;
      FBands[I].Y1[J] := 0.0;
      FBands[I].Y2[J] := 0.0;
    end;
  end;

  // Default setup: common 4-band EQ
  ConfigureBand(0, eqLowCut, 80.0, 0.0, 0.707, False);
  ConfigureBand(1, eqLowShelf, 200.0, 0.0, 0.707, False);
  ConfigureBand(2, eqPeaking, 1000.0, 0.0, 1.0, False);
  ConfigureBand(3, eqPeaking, 3000.0, 0.0, 1.0, False);
  ConfigureBand(4, eqHighShelf, 8000.0, 0.0, 0.707, False);
  ConfigureBand(5, eqHighCut, 16000.0, 0.0, 0.707, False);
end;

procedure TSedaiEQ.Reset;
var
  I, J: Integer;
begin
  inherited Reset;

  for I := 0 to MAX_EQ_BANDS - 1 do
  begin
    for J := 0 to 1 do
    begin
      FBands[I].X1[J] := 0.0;
      FBands[I].X2[J] := 0.0;
      FBands[I].Y1[J] := 0.0;
      FBands[I].Y2[J] := 0.0;
    end;
  end;
end;

procedure TSedaiEQ.CalculateBandCoefficients(var ABand: TEQBand);
var
  W0, CosW0, SinW0, Alpha: Single;
  A, Sqrt2A: Single;
begin
  if FSampleRate = 0 then Exit;

  // Clamp frequency
  if ABand.Frequency < 20.0 then ABand.Frequency := 20.0;
  if ABand.Frequency > FSampleRate * 0.45 then ABand.Frequency := FSampleRate * 0.45;

  W0 := 2.0 * PI * ABand.Frequency / FSampleRate;
  CosW0 := Cos(W0);
  SinW0 := Sin(W0);

  // Gain factor for shelf/peaking
  A := Power(10.0, ABand.Gain / 40.0);
  Sqrt2A := Sqrt(2.0 * A);

  // Alpha based on Q
  if ABand.Q < 0.1 then ABand.Q := 0.1;
  Alpha := SinW0 / (2.0 * ABand.Q);

  case ABand.BandType of
    eqLowCut:
      begin
        // High-pass filter
        ABand.A0 := (1.0 + CosW0) / 2.0 / (1.0 + Alpha);
        ABand.A1 := -(1.0 + CosW0) / (1.0 + Alpha);
        ABand.A2 := ABand.A0;
        ABand.B1 := -2.0 * CosW0 / (1.0 + Alpha);
        ABand.B2 := (1.0 - Alpha) / (1.0 + Alpha);
      end;

    eqLowShelf:
      begin
        ABand.A0 := A * ((A + 1.0) - (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.A1 := 2.0 * A * ((A - 1.0) - (A + 1.0) * CosW0);
        ABand.A2 := A * ((A + 1.0) - (A - 1.0) * CosW0 - Sqrt2A * SinW0);
        ABand.B1 := -2.0 * ((A - 1.0) + (A + 1.0) * CosW0);
        ABand.B2 := (A + 1.0) + (A - 1.0) * CosW0 - Sqrt2A * SinW0;

        // Normalize
        ABand.A0 := ABand.A0 / ((A + 1.0) + (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.A1 := ABand.A1 / ((A + 1.0) + (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.A2 := ABand.A2 / ((A + 1.0) + (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.B1 := ABand.B1 / ((A + 1.0) + (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.B2 := ABand.B2 / ((A + 1.0) + (A - 1.0) * CosW0 + Sqrt2A * SinW0);
      end;

    eqPeaking:
      begin
        ABand.A0 := (1.0 + Alpha * A) / (1.0 + Alpha / A);
        ABand.A1 := -2.0 * CosW0 / (1.0 + Alpha / A);
        ABand.A2 := (1.0 - Alpha * A) / (1.0 + Alpha / A);
        ABand.B1 := ABand.A1;
        ABand.B2 := (1.0 - Alpha / A) / (1.0 + Alpha / A);
      end;

    eqHighShelf:
      begin
        ABand.A0 := A * ((A + 1.0) + (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.A1 := -2.0 * A * ((A - 1.0) + (A + 1.0) * CosW0);
        ABand.A2 := A * ((A + 1.0) + (A - 1.0) * CosW0 - Sqrt2A * SinW0);
        ABand.B1 := 2.0 * ((A - 1.0) - (A + 1.0) * CosW0);
        ABand.B2 := (A + 1.0) - (A - 1.0) * CosW0 - Sqrt2A * SinW0;

        // Normalize
        ABand.A0 := ABand.A0 / ((A + 1.0) - (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.A1 := ABand.A1 / ((A + 1.0) - (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.A2 := ABand.A2 / ((A + 1.0) - (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.B1 := ABand.B1 / ((A + 1.0) - (A - 1.0) * CosW0 + Sqrt2A * SinW0);
        ABand.B2 := ABand.B2 / ((A + 1.0) - (A - 1.0) * CosW0 + Sqrt2A * SinW0);
      end;

    eqHighCut:
      begin
        // Low-pass filter
        ABand.A0 := (1.0 - CosW0) / 2.0 / (1.0 + Alpha);
        ABand.A1 := (1.0 - CosW0) / (1.0 + Alpha);
        ABand.A2 := ABand.A0;
        ABand.B1 := -2.0 * CosW0 / (1.0 + Alpha);
        ABand.B2 := (1.0 - Alpha) / (1.0 + Alpha);
      end;
  end;
end;

function TSedaiEQ.ProcessBand(var ABand: TEQBand; AInput: Single; AChannel: Integer): Single;
begin
  // Biquad filter processing
  Result := ABand.A0 * AInput + ABand.A1 * ABand.X1[AChannel] + ABand.A2 * ABand.X2[AChannel]
            - ABand.B1 * ABand.Y1[AChannel] - ABand.B2 * ABand.Y2[AChannel];

  // Update state
  ABand.X2[AChannel] := ABand.X1[AChannel];
  ABand.X1[AChannel] := AInput;
  ABand.Y2[AChannel] := ABand.Y1[AChannel];
  ABand.Y1[AChannel] := Result;

  // Denormal prevention
  if Abs(Result) < 1e-20 then
    Result := 0.0;
end;

procedure TSedaiEQ.SetBandEnabled(AIndex: Integer; AEnabled: Boolean);
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
    FBands[AIndex].Enabled := AEnabled;
end;

procedure TSedaiEQ.SetBandType(AIndex: Integer; AType: TEQBandType);
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
  begin
    FBands[AIndex].BandType := AType;
    CalculateBandCoefficients(FBands[AIndex]);
  end;
end;

procedure TSedaiEQ.SetBandFrequency(AIndex: Integer; AFreq: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
  begin
    FBands[AIndex].Frequency := AFreq;
    CalculateBandCoefficients(FBands[AIndex]);
  end;
end;

procedure TSedaiEQ.SetBandGain(AIndex: Integer; AGain: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
  begin
    if AGain < -24.0 then AGain := -24.0;
    if AGain > 24.0 then AGain := 24.0;
    FBands[AIndex].Gain := AGain;
    CalculateBandCoefficients(FBands[AIndex]);
  end;
end;

procedure TSedaiEQ.SetBandQ(AIndex: Integer; AQ: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
  begin
    if AQ < 0.1 then AQ := 0.1;
    if AQ > 20.0 then AQ := 20.0;
    FBands[AIndex].Q := AQ;
    CalculateBandCoefficients(FBands[AIndex]);
  end;
end;

procedure TSedaiEQ.ConfigureBand(AIndex: Integer; AType: TEQBandType;
                                 AFreq, AGain, AQ: Single; AEnabled: Boolean);
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
  begin
    FBands[AIndex].Enabled := AEnabled;
    FBands[AIndex].BandType := AType;
    FBands[AIndex].Frequency := AFreq;
    FBands[AIndex].Gain := AGain;
    FBands[AIndex].Q := AQ;
    CalculateBandCoefficients(FBands[AIndex]);
  end;
end;

function TSedaiEQ.GetBandEnabled(AIndex: Integer): Boolean;
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
    Result := FBands[AIndex].Enabled
  else
    Result := False;
end;

function TSedaiEQ.GetBandFrequency(AIndex: Integer): Single;
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
    Result := FBands[AIndex].Frequency
  else
    Result := 1000.0;
end;

function TSedaiEQ.GetBandGain(AIndex: Integer): Single;
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
    Result := FBands[AIndex].Gain
  else
    Result := 0.0;
end;

function TSedaiEQ.GetBandQ(AIndex: Integer): Single;
begin
  if (AIndex >= 0) and (AIndex < MAX_EQ_BANDS) then
    Result := FBands[AIndex].Q
  else
    Result := 1.0;
end;

procedure TSedaiEQ.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I, B: Integer;
  Left, Right: Single;
begin
  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2];
    Right := AInput[I * 2 + 1];

    // Process through all enabled bands
    for B := 0 to MAX_EQ_BANDS - 1 do
    begin
      if FBands[B].Enabled then
      begin
        Left := ProcessBand(FBands[B], Left, 0);
        Right := ProcessBand(FBands[B], Right, 1);
      end;
    end;

    // Apply output gain
    AOutput[I * 2] := Left * FOutputGain;
    AOutput[I * 2 + 1] := Right * FOutputGain;
  end;
end;

procedure TSedaiEQ.LoadPresetFlat;
var
  I: Integer;
begin
  for I := 0 to MAX_EQ_BANDS - 1 do
  begin
    FBands[I].Enabled := False;
    FBands[I].Gain := 0.0;
    CalculateBandCoefficients(FBands[I]);
  end;
end;

procedure TSedaiEQ.LoadPresetVocal;
begin
  LoadPresetFlat;
  ConfigureBand(0, eqLowCut, 100.0, 0.0, 0.707, True);
  ConfigureBand(1, eqPeaking, 250.0, -3.0, 1.5, True);
  ConfigureBand(2, eqPeaking, 3000.0, 3.0, 1.0, True);
  ConfigureBand(3, eqHighShelf, 10000.0, 2.0, 0.707, True);
end;

procedure TSedaiEQ.LoadPresetBass;
begin
  LoadPresetFlat;
  ConfigureBand(0, eqLowShelf, 80.0, 4.0, 0.707, True);
  ConfigureBand(1, eqPeaking, 400.0, -2.0, 1.0, True);
  ConfigureBand(2, eqPeaking, 2500.0, 2.0, 1.5, True);
end;

procedure TSedaiEQ.LoadPresetBrightness;
begin
  LoadPresetFlat;
  ConfigureBand(0, eqPeaking, 2000.0, 2.0, 1.0, True);
  ConfigureBand(1, eqPeaking, 5000.0, 3.0, 1.0, True);
  ConfigureBand(2, eqHighShelf, 10000.0, 4.0, 0.707, True);
end;

end.
