{*
 * Sedai Audio Foundation - Multi-Mode Filter
 *
 * TSedaiFilter provides multi-mode filter with support for low-pass,
 * high-pass, band-pass, notch, all-pass, and peaking responses.
 * Supports 12dB, 24dB, and 48dB slopes. Includes SID filter emulation.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode;

type
  { TSedaiSignalProcessor }
  // Abstract base class for all signal processors
  TSedaiSignalProcessor = class(TSedaiSignalNode)
  protected
    FDryWet: Single;              // Dry/wet mix (0.0 = dry, 1.0 = wet)

  public
    constructor Create; override;

    // Mix dry and wet signals
    function MixDryWet(ADry, AWet: Single): Single; inline;

    // Properties
    property DryWet: Single read FDryWet write FDryWet;
  end;

  { TSedaiFilter }
  // Multi-mode resonant filter
  TSedaiFilter = class(TSedaiSignalProcessor)
  private
    // Filter parameters
    FFilterType: TFilterType;
    FSlope: TFilterSlope;
    FCutoff: Single;              // Cutoff frequency in Hz
    FResonance: Single;           // Resonance (0.0 - 1.0, maps to Q)
    FGain: Single;                // Gain for peaking filter (dB)

    // Internal filter coefficients (biquad)
    FA0, FA1, FA2: Single;        // Feedforward coefficients
    FB1, FB2: Single;             // Feedback coefficients

    // Filter state (per channel, supports stereo)
    FX1, FX2: array[0..1] of Single;  // Input history
    FY1, FY2: array[0..1] of Single;  // Output history

    // Second stage for 24dB
    FX1_2, FX2_2: array[0..1] of Single;
    FY1_2, FY2_2: array[0..1] of Single;

    // Third stage for 48dB
    FX1_3, FX2_3: array[0..1] of Single;
    FY1_3, FY2_3: array[0..1] of Single;

    // Fourth stage for 48dB
    FX1_4, FX2_4: array[0..1] of Single;
    FY1_4, FY2_4: array[0..1] of Single;

    // SID filter emulation
    FSIDMode: Boolean;
    FSIDModel: TSIDModel;
    FSIDVoiceMask: Byte;          // Which voices pass through filter

    // SID filter state (state variable)
    FSIDLowPass: Single;
    FSIDBandPass: Single;
    FSIDHighPass: Single;

    procedure SetFilterType(AValue: TFilterType);
    procedure SetSlope(AValue: TFilterSlope);
    procedure SetCutoff(AValue: Single);
    procedure SetResonance(AValue: Single);
    procedure SetGain(AValue: Single);

    procedure CalculateCoefficients;
    procedure CalculateLowPass(AW0, AAlpha: Single);
    procedure CalculateHighPass(AW0, AAlpha: Single);
    procedure CalculateBandPass(AW0, AAlpha: Single);
    procedure CalculateNotch(AW0, AAlpha: Single);
    procedure CalculateAllPass(AW0, AAlpha: Single);
    procedure CalculatePeaking(AW0, AAlpha, AGainLinear: Single);

    // Process single biquad stage
    function ProcessBiquad(AInput: Single; AChannel: Integer;
      var X1, X2, Y1, Y2: Single): Single;

    // SID filter processing
    function ProcessSIDFilter(AInput: Single): Single;

  public
    constructor Create; override;

    // Reset filter state
    procedure Reset; override;

    // Process single sample (mono)
    function ProcessSample(AInput: Single; AChannel: Integer = 0): Single;

    // Process block (stereo interleaved)
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Set SID filter mode
    procedure SetSIDMode(AEnable: Boolean; AModel: TSIDModel = smMOS6581);

    // Set SID voice mask (bits 0-2 for voices 1-3)
    procedure SetSIDVoiceMask(AMask: Byte);

    // Properties
    property FilterType: TFilterType read FFilterType write SetFilterType;
    property Slope: TFilterSlope read FSlope write SetSlope;
    property Cutoff: Single read FCutoff write SetCutoff;
    property Resonance: Single read FResonance write SetResonance;
    property Gain: Single read FGain write SetGain;
    property SIDMode: Boolean read FSIDMode;
    property SIDModel: TSIDModel read FSIDModel;
  end;

implementation

const
  // Minimum Q to prevent divide by zero
  MIN_Q = 0.5;
  MAX_Q = 20.0;

  // SID filter constants
  SID_6581_CUTOFF_SCALE = 0.0002;   // Approximate scale for 6581
  SID_8580_CUTOFF_SCALE = 0.00022;  // Approximate scale for 8580

{ TSedaiSignalProcessor }

constructor TSedaiSignalProcessor.Create;
begin
  inherited Create;
  FDryWet := 1.0;  // Full wet by default
end;

function TSedaiSignalProcessor.MixDryWet(ADry, AWet: Single): Single;
begin
  Result := ADry * (1.0 - FDryWet) + AWet * FDryWet;
end;

{ TSedaiFilter }

constructor TSedaiFilter.Create;
var
  I: Integer;
begin
  inherited Create;

  FFilterType := ftLowPass;
  FSlope := fs12dB;
  FCutoff := 1000.0;
  FResonance := 0.0;
  FGain := 0.0;

  // Initialize coefficients
  FA0 := 1.0;
  FA1 := 0.0;
  FA2 := 0.0;
  FB1 := 0.0;
  FB2 := 0.0;

  // Clear state
  for I := 0 to 1 do
  begin
    FX1[I] := 0.0;
    FX2[I] := 0.0;
    FY1[I] := 0.0;
    FY2[I] := 0.0;

    FX1_2[I] := 0.0;
    FX2_2[I] := 0.0;
    FY1_2[I] := 0.0;
    FY2_2[I] := 0.0;

    FX1_3[I] := 0.0;
    FX2_3[I] := 0.0;
    FY1_3[I] := 0.0;
    FY2_3[I] := 0.0;

    FX1_4[I] := 0.0;
    FX2_4[I] := 0.0;
    FY1_4[I] := 0.0;
    FY2_4[I] := 0.0;
  end;

  // SID mode off by default
  FSIDMode := False;
  FSIDModel := smMOS6581;
  FSIDVoiceMask := $07;  // All voices through filter

  FSIDLowPass := 0.0;
  FSIDBandPass := 0.0;
  FSIDHighPass := 0.0;

  CalculateCoefficients;
end;

procedure TSedaiFilter.Reset;
var
  I: Integer;
begin
  inherited Reset;

  for I := 0 to 1 do
  begin
    FX1[I] := 0.0;
    FX2[I] := 0.0;
    FY1[I] := 0.0;
    FY2[I] := 0.0;

    FX1_2[I] := 0.0;
    FX2_2[I] := 0.0;
    FY1_2[I] := 0.0;
    FY2_2[I] := 0.0;

    FX1_3[I] := 0.0;
    FX2_3[I] := 0.0;
    FY1_3[I] := 0.0;
    FY2_3[I] := 0.0;

    FX1_4[I] := 0.0;
    FX2_4[I] := 0.0;
    FY1_4[I] := 0.0;
    FY2_4[I] := 0.0;
  end;

  FSIDLowPass := 0.0;
  FSIDBandPass := 0.0;
  FSIDHighPass := 0.0;
end;

procedure TSedaiFilter.SetFilterType(AValue: TFilterType);
begin
  FFilterType := AValue;
  CalculateCoefficients;
end;

procedure TSedaiFilter.SetSlope(AValue: TFilterSlope);
begin
  FSlope := AValue;
  CalculateCoefficients;
end;

procedure TSedaiFilter.SetCutoff(AValue: Single);
begin
  // Clamp to valid range
  if AValue < 20.0 then
    AValue := 20.0
  else if AValue > FSampleRate * 0.45 then
    AValue := FSampleRate * 0.45;

  FCutoff := AValue;
  CalculateCoefficients;
end;

procedure TSedaiFilter.SetResonance(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;

  FResonance := AValue;
  CalculateCoefficients;
end;

procedure TSedaiFilter.SetGain(AValue: Single);
begin
  if AValue < -24.0 then
    AValue := -24.0
  else if AValue > 24.0 then
    AValue := 24.0;

  FGain := AValue;
  CalculateCoefficients;
end;

procedure TSedaiFilter.CalculateCoefficients;
var
  W0, Alpha, Q: Single;
  GainLinear: Single;
begin
  if FSampleRate = 0 then
    Exit;

  // Calculate normalized angular frequency
  W0 := 2.0 * PI * FCutoff / FSampleRate;

  // Map resonance (0-1) to Q (0.5 - 20)
  Q := MIN_Q + FResonance * (MAX_Q - MIN_Q);

  // Calculate alpha (bandwidth coefficient)
  Alpha := Sin(W0) / (2.0 * Q);

  // Calculate gain for peaking filter
  GainLinear := Power(10.0, FGain / 40.0);  // dB to linear (half for biquad)

  // Calculate coefficients based on filter type
  case FFilterType of
    ftLowPass:   CalculateLowPass(W0, Alpha);
    ftHighPass:  CalculateHighPass(W0, Alpha);
    ftBandPass:  CalculateBandPass(W0, Alpha);
    ftNotch:     CalculateNotch(W0, Alpha);
    ftAllPass:   CalculateAllPass(W0, Alpha);
    ftPeaking:   CalculatePeaking(W0, Alpha, GainLinear);
  end;
end;

procedure TSedaiFilter.CalculateLowPass(AW0, AAlpha: Single);
var
  CosW0, A0Inv: Single;
begin
  CosW0 := Cos(AW0);

  // LPF: H(s) = 1 / (s^2 + s/Q + 1)
  A0Inv := 1.0 / (1.0 + AAlpha);

  FA0 := ((1.0 - CosW0) / 2.0) * A0Inv;
  FA1 := (1.0 - CosW0) * A0Inv;
  FA2 := FA0;
  FB1 := (-2.0 * CosW0) * A0Inv;
  FB2 := (1.0 - AAlpha) * A0Inv;
end;

procedure TSedaiFilter.CalculateHighPass(AW0, AAlpha: Single);
var
  CosW0, A0Inv: Single;
begin
  CosW0 := Cos(AW0);

  // HPF: H(s) = s^2 / (s^2 + s/Q + 1)
  A0Inv := 1.0 / (1.0 + AAlpha);

  FA0 := ((1.0 + CosW0) / 2.0) * A0Inv;
  FA1 := -(1.0 + CosW0) * A0Inv;
  FA2 := FA0;
  FB1 := (-2.0 * CosW0) * A0Inv;
  FB2 := (1.0 - AAlpha) * A0Inv;
end;

procedure TSedaiFilter.CalculateBandPass(AW0, AAlpha: Single);
var
  CosW0, A0Inv: Single;
begin
  CosW0 := Cos(AW0);

  // BPF: H(s) = s / (s^2 + s/Q + 1) (constant skirt gain)
  A0Inv := 1.0 / (1.0 + AAlpha);

  FA0 := AAlpha * A0Inv;
  FA1 := 0.0;
  FA2 := -AAlpha * A0Inv;
  FB1 := (-2.0 * CosW0) * A0Inv;
  FB2 := (1.0 - AAlpha) * A0Inv;
end;

procedure TSedaiFilter.CalculateNotch(AW0, AAlpha: Single);
var
  CosW0, A0Inv: Single;
begin
  CosW0 := Cos(AW0);

  // Notch: H(s) = (s^2 + 1) / (s^2 + s/Q + 1)
  A0Inv := 1.0 / (1.0 + AAlpha);

  FA0 := 1.0 * A0Inv;
  FA1 := (-2.0 * CosW0) * A0Inv;
  FA2 := 1.0 * A0Inv;
  FB1 := FA1;
  FB2 := (1.0 - AAlpha) * A0Inv;
end;

procedure TSedaiFilter.CalculateAllPass(AW0, AAlpha: Single);
var
  CosW0, A0Inv: Single;
begin
  CosW0 := Cos(AW0);

  // APF: H(s) = (s^2 - s/Q + 1) / (s^2 + s/Q + 1)
  A0Inv := 1.0 / (1.0 + AAlpha);

  FA0 := (1.0 - AAlpha) * A0Inv;
  FA1 := (-2.0 * CosW0) * A0Inv;
  FA2 := (1.0 + AAlpha) * A0Inv;
  FB1 := FA1;
  FB2 := FA0;
end;

procedure TSedaiFilter.CalculatePeaking(AW0, AAlpha, AGainLinear: Single);
var
  CosW0, A0Inv: Single;
  AlphaA, AlphaOverA: Single;
begin
  CosW0 := Cos(AW0);
  AlphaA := AAlpha * AGainLinear;
  AlphaOverA := AAlpha / AGainLinear;

  // Peaking EQ
  A0Inv := 1.0 / (1.0 + AlphaOverA);

  FA0 := (1.0 + AlphaA) * A0Inv;
  FA1 := (-2.0 * CosW0) * A0Inv;
  FA2 := (1.0 - AlphaA) * A0Inv;
  FB1 := FA1;
  FB2 := (1.0 - AlphaOverA) * A0Inv;
end;

function TSedaiFilter.ProcessBiquad(AInput: Single; AChannel: Integer;
  var X1, X2, Y1, Y2: Single): Single;
begin
  // Direct Form I biquad
  Result := FA0 * AInput + FA1 * X1 + FA2 * X2 - FB1 * Y1 - FB2 * Y2;

  // Update state
  X2 := X1;
  X1 := AInput;
  Y2 := Y1;
  Y1 := Result;

  // Denormal prevention
  if Abs(Result) < 1e-20 then
    Result := 0.0;
end;

function TSedaiFilter.ProcessSIDFilter(AInput: Single): Single;
var
  CutoffScale: Single;
  FilterCutoff: Single;
  ResValue: Single;
begin
  // Select model-specific parameters
  if FSIDModel = smMOS6581 then
    CutoffScale := SID_6581_CUTOFF_SCALE
  else
    CutoffScale := SID_8580_CUTOFF_SCALE;

  // Scale cutoff for SID range (approximately 30Hz to 12kHz)
  FilterCutoff := FCutoff * CutoffScale;
  if FilterCutoff > 0.45 then
    FilterCutoff := 0.45;  // Stability limit

  // Map resonance (SID uses 4-bit value, 0-15)
  // Higher values = more resonance (self-oscillation at max)
  ResValue := 1.0 - FResonance * 0.9;  // Prevent full self-oscillation
  if ResValue < 0.1 then
    ResValue := 0.1;

  // State variable filter (SID style)
  // This is a simplified model of the SID's analog filter

  // Calculate filter outputs
  FSIDHighPass := AInput - FSIDLowPass - FSIDBandPass * ResValue;
  FSIDBandPass := FSIDBandPass + FilterCutoff * FSIDHighPass;
  FSIDLowPass := FSIDLowPass + FilterCutoff * FSIDBandPass;

  // Select output based on filter type
  case FFilterType of
    ftLowPass:   Result := FSIDLowPass;
    ftHighPass:  Result := FSIDHighPass;
    ftBandPass:  Result := FSIDBandPass;
    else         Result := FSIDLowPass;  // Default to lowpass
  end;

  // Apply SID distortion characteristics (6581 has more distortion)
  if FSIDModel = smMOS6581 then
  begin
    // Simple soft clipping for 6581 character
    if Result > 0.8 then
      Result := 0.8 + (Result - 0.8) * 0.5
    else if Result < -0.8 then
      Result := -0.8 + (Result + 0.8) * 0.5;
  end;
end;

function TSedaiFilter.ProcessSample(AInput: Single; AChannel: Integer): Single;
var
  Output: Single;
begin
  // SID mode uses state variable filter
  if FSIDMode then
  begin
    Result := ProcessSIDFilter(AInput);
    Exit;
  end;

  // Standard biquad processing
  Output := ProcessBiquad(AInput, AChannel,
    FX1[AChannel], FX2[AChannel], FY1[AChannel], FY2[AChannel]);

  // Additional stages for higher slopes
  case FSlope of
    fs24dB:
      Output := ProcessBiquad(Output, AChannel,
        FX1_2[AChannel], FX2_2[AChannel], FY1_2[AChannel], FY2_2[AChannel]);

    fs48dB:
      begin
        Output := ProcessBiquad(Output, AChannel,
          FX1_2[AChannel], FX2_2[AChannel], FY1_2[AChannel], FY2_2[AChannel]);
        Output := ProcessBiquad(Output, AChannel,
          FX1_3[AChannel], FX2_3[AChannel], FY1_3[AChannel], FY2_3[AChannel]);
        Output := ProcessBiquad(Output, AChannel,
          FX1_4[AChannel], FX2_4[AChannel], FY1_4[AChannel], FY2_4[AChannel]);
      end;
  end;

  // Apply dry/wet mix
  Result := MixDryWet(AInput, Output);
end;

procedure TSedaiFilter.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
begin
  // Process stereo interleaved
  for I := 0 to AFrameCount - 1 do
  begin
    AOutput[I * 2] := ProcessSample(AInput[I * 2], 0);         // Left
    AOutput[I * 2 + 1] := ProcessSample(AInput[I * 2 + 1], 1); // Right
  end;
end;

procedure TSedaiFilter.SetSIDMode(AEnable: Boolean; AModel: TSIDModel);
begin
  FSIDMode := AEnable;
  FSIDModel := AModel;

  // Reset SID filter state
  FSIDLowPass := 0.0;
  FSIDBandPass := 0.0;
  FSIDHighPass := 0.0;
end;

procedure TSedaiFilter.SetSIDVoiceMask(AMask: Byte);
begin
  FSIDVoiceMask := AMask and $07;  // Only bits 0-2 valid
end;

end.
