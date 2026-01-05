{*
 * Sedai Audio Foundation - Parameter Port
 *
 * TSedaiParameterPort provides modulatable parameters with support for
 * base values, modulation sources, value curves (linear/log/exp),
 * and smoothing for click-free parameter changes.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiParameterPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes;

type
  // Forward declaration for modulation sources
  TSedaiParameterPort = class;

  { TSedaiModulationSource }
  // Represents a single modulation source connected to a parameter
  TSedaiModulationSource = record
    Source: Pointer;        // Pointer to the modulation source (envelope, LFO, etc.)
    Amount: Single;         // Modulation amount (-1.0 to 1.0)
    Bipolar: Boolean;       // True = -1..+1, False = 0..+1
  end;

  TSedaiModulationSourceArray = array of TSedaiModulationSource;

  { TSedaiParameterPort }
  // Modulatable parameter with value mapping and smoothing
  TSedaiParameterPort = class
  private
    // Value storage
    FBaseValue: Single;           // User-set value (0.0 to 1.0 normalized)
    FModulatedValue: Single;      // Value after modulation
    FDisplayValue: Single;        // Value in display units
    FSmoothedValue: Single;       // Value after smoothing

    // Range and mapping
    FMin: Single;                 // Minimum value in display units
    FMax: Single;                 // Maximum value in display units
    FDefault: Single;             // Default value in display units
    FCurve: TParameterCurve;      // Mapping curve (Linear, Log, Exp)
    FSkewFactor: Single;          // Curve skew (for fine-tuning)

    // Modulation
    FModulationSources: TSedaiModulationSourceArray;
    FModulationDepth: Single;     // Total modulation amount

    // Smoothing
    FSmoothingEnabled: Boolean;
    FSmoothingCoeff: Single;      // Smoothing coefficient (0.0 = instant, 0.999 = slow)
    FSampleRate: Cardinal;

    // Metadata
    FName: string;
    FUnitLabel: string;           // Display unit (Hz, dB, %, ms, etc.)
    FSteps: Integer;              // Number of discrete steps (0 = continuous)

    procedure SetBaseValue(AValue: Single);
    procedure SetMin(AValue: Single);
    procedure SetMax(AValue: Single);
    procedure SetCurve(AValue: TParameterCurve);
    procedure SetSmoothingTime(ATimeMs: Single);

    function NormalizedToDisplay(ANormalized: Single): Single;
    function DisplayToNormalized(ADisplay: Single): Single;
    procedure UpdateDisplayValue;

  public
    constructor Create;
    destructor Destroy; override;

    // ========================================================================
    // VALUE ACCESS
    // ========================================================================

    // Set base value (normalized 0-1)
    procedure SetNormalized(AValue: Single);

    // Set base value (display units)
    procedure SetDisplay(AValue: Single);

    // Get current value (with modulation, in display units)
    function GetValue: Single;

    // Get current value (normalized 0-1)
    function GetNormalizedValue: Single;

    // Get smoothed value (for audio processing)
    function GetSmoothedValue: Single;

    // Process smoothing for one sample
    procedure ProcessSmoothing;

    // Process smoothing for a block
    procedure ProcessSmoothingBlock(AFrameCount: Integer);

    // ========================================================================
    // MODULATION
    // ========================================================================

    // Add a modulation source
    function AddModulation(ASource: Pointer; AAmount: Single; ABipolar: Boolean = True): Integer;

    // Remove a modulation source
    procedure RemoveModulation(ASource: Pointer);

    // Remove all modulation sources
    procedure ClearModulation;

    // Update modulated value from all sources
    // ASourceValues is array of current modulation source values (0-1 or -1 to +1)
    procedure UpdateModulation(const ASourceValues: array of Single);

    // Get modulation source count
    function GetModulationSourceCount: Integer;

    // ========================================================================
    // RESET AND INITIALIZATION
    // ========================================================================

    // Reset to default value
    procedure Reset;

    // Reset smoothing (jump to target immediately)
    procedure ResetSmoothing;

    // Set sample rate (for smoothing calculations)
    procedure SetSampleRate(AValue: Cardinal);

    // ========================================================================
    // DISPLAY AND FORMATTING
    // ========================================================================

    // Get formatted display string (value + unit)
    function GetDisplayString: string;

    // Get formatted display string with specified precision
    function GetDisplayStringPrecision(ADecimals: Integer): string;

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    // Base value (normalized 0-1, before modulation)
    property BaseValue: Single read FBaseValue write SetBaseValue;

    // Modulated value (normalized 0-1)
    property ModulatedValue: Single read FModulatedValue;

    // Display value (in display units)
    property DisplayValue: Single read FDisplayValue;

    // Smoothed value (for click-free audio)
    property SmoothedValue: Single read FSmoothedValue;

    // Minimum value (display units)
    property Min: Single read FMin write SetMin;

    // Maximum value (display units)
    property Max: Single read FMax write SetMax;

    // Default value (display units)
    property Default: Single read FDefault write FDefault;

    // Value curve mapping
    property Curve: TParameterCurve read FCurve write SetCurve;

    // Curve skew factor (default 1.0)
    property SkewFactor: Single read FSkewFactor write FSkewFactor;

    // Parameter name
    property Name: string read FName write FName;

    // Display unit label
    property UnitLabel: string read FUnitLabel write FUnitLabel;

    // Number of discrete steps (0 = continuous)
    property Steps: Integer read FSteps write FSteps;

    // Smoothing enabled
    property SmoothingEnabled: Boolean read FSmoothingEnabled write FSmoothingEnabled;

    // Smoothing coefficient
    property SmoothingCoeff: Single read FSmoothingCoeff;

    // Sample rate
    property SampleRate: Cardinal read FSampleRate write SetSampleRate;
  end;

  { TSedaiParameterPortArray }
  TSedaiParameterPortArray = array of TSedaiParameterPort;

implementation

{ TSedaiParameterPort }

constructor TSedaiParameterPort.Create;
begin
  inherited Create;

  FBaseValue := 0.0;
  FModulatedValue := 0.0;
  FDisplayValue := 0.0;
  FSmoothedValue := 0.0;

  FMin := 0.0;
  FMax := 1.0;
  FDefault := 0.0;
  FCurve := pcLinear;
  FSkewFactor := 1.0;

  SetLength(FModulationSources, 0);
  FModulationDepth := 0.0;

  FSmoothingEnabled := False;
  FSmoothingCoeff := 0.0;
  FSampleRate := SEDAI_DEFAULT_SAMPLE_RATE;

  FName := '';
  FUnitLabel := '';
  FSteps := 0;
end;

destructor TSedaiParameterPort.Destroy;
begin
  SetLength(FModulationSources, 0);
  inherited Destroy;
end;

procedure TSedaiParameterPort.SetBaseValue(AValue: Single);
begin
  // Clamp to 0-1 range
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;

  // Quantize if steps defined
  if FSteps > 1 then
    AValue := Round(AValue * (FSteps - 1)) / (FSteps - 1);

  FBaseValue := AValue;
  FModulatedValue := AValue;  // Reset modulation
  UpdateDisplayValue;
end;

procedure TSedaiParameterPort.SetMin(AValue: Single);
begin
  FMin := AValue;
  UpdateDisplayValue;
end;

procedure TSedaiParameterPort.SetMax(AValue: Single);
begin
  FMax := AValue;
  UpdateDisplayValue;
end;

procedure TSedaiParameterPort.SetCurve(AValue: TParameterCurve);
begin
  FCurve := AValue;
  UpdateDisplayValue;
end;

procedure TSedaiParameterPort.SetSmoothingTime(ATimeMs: Single);
begin
  if (ATimeMs <= 0) or (FSampleRate = 0) then
  begin
    FSmoothingCoeff := 0.0;
    FSmoothingEnabled := False;
  end
  else
  begin
    // Calculate coefficient for RC-style smoothing
    // coefficient = exp(-1 / (time_in_samples))
    FSmoothingCoeff := Exp(-1.0 / (ATimeMs * 0.001 * FSampleRate));
    FSmoothingEnabled := True;
  end;
end;

function TSedaiParameterPort.NormalizedToDisplay(ANormalized: Single): Single;
var
  Mapped: Single;
begin
  // Apply curve mapping
  case FCurve of
    pcLinear:
      Mapped := ANormalized;

    pcLogarithmic:
      begin
        // Log curve for frequency-like parameters
        if ANormalized <= 0 then
          Mapped := 0
        else
          Mapped := Power(ANormalized, 1.0 / FSkewFactor);
      end;

    pcExponential:
      begin
        // Exp curve for amplitude-like parameters
        Mapped := Power(ANormalized, FSkewFactor);
      end;

    else
      Mapped := ANormalized;
  end;

  // Scale to display range
  Result := FMin + Mapped * (FMax - FMin);
end;

function TSedaiParameterPort.DisplayToNormalized(ADisplay: Single): Single;
var
  Scaled: Single;
begin
  // Scale from display range to 0-1
  if Abs(FMax - FMin) < 1e-10 then
    Result := 0.0
  else
  begin
    Scaled := (ADisplay - FMin) / (FMax - FMin);

    // Clamp
    if Scaled < 0 then Scaled := 0;
    if Scaled > 1 then Scaled := 1;

    // Apply inverse curve mapping
    case FCurve of
      pcLinear:
        Result := Scaled;

      pcLogarithmic:
        begin
          if Scaled <= 0 then
            Result := 0
          else
            Result := Power(Scaled, FSkewFactor);
        end;

      pcExponential:
        begin
          if Scaled <= 0 then
            Result := 0
          else
            Result := Power(Scaled, 1.0 / FSkewFactor);
        end;

      else
        Result := Scaled;
    end;
  end;
end;

procedure TSedaiParameterPort.UpdateDisplayValue;
begin
  FDisplayValue := NormalizedToDisplay(FModulatedValue);
end;

procedure TSedaiParameterPort.SetNormalized(AValue: Single);
begin
  SetBaseValue(AValue);
end;

procedure TSedaiParameterPort.SetDisplay(AValue: Single);
begin
  SetBaseValue(DisplayToNormalized(AValue));
end;

function TSedaiParameterPort.GetValue: Single;
begin
  Result := FDisplayValue;
end;

function TSedaiParameterPort.GetNormalizedValue: Single;
begin
  Result := FModulatedValue;
end;

function TSedaiParameterPort.GetSmoothedValue: Single;
begin
  if FSmoothingEnabled then
    Result := NormalizedToDisplay(FSmoothedValue)
  else
    Result := FDisplayValue;
end;

procedure TSedaiParameterPort.ProcessSmoothing;
begin
  if FSmoothingEnabled then
  begin
    // RC-style lowpass smoothing
    FSmoothedValue := FSmoothedValue * FSmoothingCoeff +
                      FModulatedValue * (1.0 - FSmoothingCoeff);
  end
  else
    FSmoothedValue := FModulatedValue;
end;

procedure TSedaiParameterPort.ProcessSmoothingBlock(AFrameCount: Integer);
var
  I: Integer;
begin
  if FSmoothingEnabled then
  begin
    for I := 0 to AFrameCount - 1 do
    begin
      FSmoothedValue := FSmoothedValue * FSmoothingCoeff +
                        FModulatedValue * (1.0 - FSmoothingCoeff);
    end;
  end
  else
    FSmoothedValue := FModulatedValue;
end;

function TSedaiParameterPort.AddModulation(ASource: Pointer; AAmount: Single;
                                           ABipolar: Boolean): Integer;
var
  NewSource: TSedaiModulationSource;
begin
  NewSource.Source := ASource;
  NewSource.Amount := AAmount;
  NewSource.Bipolar := ABipolar;

  SetLength(FModulationSources, Length(FModulationSources) + 1);
  FModulationSources[High(FModulationSources)] := NewSource;

  Result := High(FModulationSources);
end;

procedure TSedaiParameterPort.RemoveModulation(ASource: Pointer);
var
  I, J: Integer;
begin
  for I := High(FModulationSources) downto 0 do
  begin
    if FModulationSources[I].Source = ASource then
    begin
      // Shift remaining elements
      for J := I to High(FModulationSources) - 1 do
        FModulationSources[J] := FModulationSources[J + 1];
      SetLength(FModulationSources, Length(FModulationSources) - 1);
    end;
  end;
end;

procedure TSedaiParameterPort.ClearModulation;
begin
  SetLength(FModulationSources, 0);
  FModulationDepth := 0.0;
  FModulatedValue := FBaseValue;
  UpdateDisplayValue;
end;

procedure TSedaiParameterPort.UpdateModulation(const ASourceValues: array of Single);
var
  I: Integer;
  ModSum: Single;
  SourceValue: Single;
begin
  if Length(FModulationSources) = 0 then
  begin
    FModulatedValue := FBaseValue;
    UpdateDisplayValue;
    Exit;
  end;

  // Calculate total modulation
  ModSum := 0.0;
  for I := 0 to High(FModulationSources) do
  begin
    if I <= High(ASourceValues) then
    begin
      SourceValue := ASourceValues[I];

      // Convert unipolar to bipolar if needed
      if not FModulationSources[I].Bipolar then
        SourceValue := SourceValue * 2.0 - 1.0;

      ModSum := ModSum + SourceValue * FModulationSources[I].Amount;
    end;
  end;

  FModulationDepth := ModSum;

  // Apply modulation to base value
  FModulatedValue := FBaseValue + ModSum;

  // Clamp to valid range
  if FModulatedValue < 0.0 then
    FModulatedValue := 0.0
  else if FModulatedValue > 1.0 then
    FModulatedValue := 1.0;

  UpdateDisplayValue;
end;

function TSedaiParameterPort.GetModulationSourceCount: Integer;
begin
  Result := Length(FModulationSources);
end;

procedure TSedaiParameterPort.Reset;
begin
  SetDisplay(FDefault);
  ResetSmoothing;
end;

procedure TSedaiParameterPort.ResetSmoothing;
begin
  FSmoothedValue := FModulatedValue;
end;

procedure TSedaiParameterPort.SetSampleRate(AValue: Cardinal);
var
  OldTimeMs: Single;
begin
  // Preserve smoothing time when sample rate changes
  if (FSampleRate > 0) and FSmoothingEnabled and (FSmoothingCoeff > 0) then
  begin
    // Calculate old time
    OldTimeMs := -1000.0 / (FSampleRate * Ln(FSmoothingCoeff));
    FSampleRate := AValue;
    // Recalculate coefficient for new sample rate
    SetSmoothingTime(OldTimeMs);
  end
  else
    FSampleRate := AValue;
end;

function TSedaiParameterPort.GetDisplayString: string;
begin
  if FUnitLabel <> '' then
    Result := Format('%.2f %s', [FDisplayValue, FUnitLabel])
  else
    Result := Format('%.2f', [FDisplayValue]);
end;

function TSedaiParameterPort.GetDisplayStringPrecision(ADecimals: Integer): string;
var
  FormatStr: string;
begin
  FormatStr := '%.' + IntToStr(ADecimals) + 'f';

  if FUnitLabel <> '' then
    Result := Format(FormatStr + ' %s', [FDisplayValue, FUnitLabel])
  else
    Result := Format(FormatStr, [FDisplayValue]);
end;

end.
