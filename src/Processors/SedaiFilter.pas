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

  // One 2nd-order section's biquad coefficients (Direct Form I), double
  // precision to avoid single-precision cancellation at low cutoffs.
  TBiquadCoeff = record
    A0, A1, A2: Double;   // feedforward
    B1, B2: Double;       // feedback
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

    // Per-stage biquad coefficients. The number of cascaded 2nd-order stages
    // depends on the slope: 12dB=1, 24dB=2, 48dB=4. 24/48dB low/high-pass use
    // Butterworth pole-Q per stage for a flat passband and a clean slope.
    FCoeff: array[0..3] of TBiquadCoeff;
    FStageCount: Integer;

    // Per-stage, per-channel (stereo) filter state, double precision.
    FState: array[0..3, 0..1] of record
      X1, X2, Y1, Y2: Double;
    end;

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
    // Fill one 2nd-order section's coefficients for the given type / Q / gain.
    procedure CalcStage(AType: TFilterType; AW0, AQ, AGainLinear: Double;
      out C: TBiquadCoeff);

    // Process single biquad stage (double-precision state, Single I/O).
    function ProcessBiquad(AInput: Single; const C: TBiquadCoeff;
      var X1, X2, Y1, Y2: Double): Single;

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
begin
  inherited Create;

  FFilterType := ftLowPass;
  FSlope := fs12dB;
  FCutoff := 1000.0;
  FResonance := 0.0;
  FGain := 0.0;

  FStageCount := 1;
  FillChar(FCoeff, SizeOf(FCoeff), 0);
  FCoeff[0].A0 := 1.0;            // unity pass-through until first calc
  FillChar(FState, SizeOf(FState), 0);

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
begin
  inherited Reset;

  FillChar(FState, SizeOf(FState), 0);

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
const
  // Butterworth pole-Q for cascaded 2nd-order sections (maximally-flat passband).
  BW_Q4: array[0..1] of Double = (0.54119610, 1.30656296);          // order 4 (24dB)
  BW_Q8: array[0..3] of Double =                                    // order 8 (48dB)
    (0.50979558, 0.60134489, 0.89997622, 2.56291545);
var
  W0, ResQ, GainLinear, StageQ, BaseQ: Double;
  I: Integer;
  ButterworthCascade: Boolean;
begin
  if FSampleRate = 0 then
    Exit;

  W0 := 2.0 * PI * FCutoff / FSampleRate;
  ResQ := MIN_Q + FResonance * (MAX_Q - MIN_Q);   // resonance -> Q (0.5 .. 20)
  GainLinear := Power(10.0, FGain / 40.0);         // dB to linear (half for biquad)

  case FSlope of
    fs24dB: FStageCount := 2;
    fs48dB: FStageCount := 4;
  else      FStageCount := 1;
  end;

  // Per-stage Butterworth Q only applies to low/high-pass cascades; for a single
  // stage (12dB) or the other responses we keep the resonance Q on every stage
  // (the historical behaviour), so 12dB and BP/notch/allpass/peaking are unchanged.
  ButterworthCascade := (FStageCount > 1) and
                        (FFilterType in [ftLowPass, ftHighPass]);

  for I := 0 to FStageCount - 1 do
  begin
    if ButterworthCascade then
    begin
      if FStageCount = 2 then BaseQ := BW_Q4[I] else BaseQ := BW_Q8[I];
      // Resonance peaks the LAST stage only: the earlier stages keep the flat
      // Butterworth shape, the final one rises from BaseQ (res=0) up to MAX_Q.
      if I = FStageCount - 1 then
        StageQ := BaseQ + FResonance * (MAX_Q - BaseQ)
      else
        StageQ := BaseQ;
    end
    else
      StageQ := ResQ;

    CalcStage(FFilterType, W0, StageQ, GainLinear, FCoeff[I]);
  end;
end;

procedure TSedaiFilter.CalcStage(AType: TFilterType; AW0, AQ, AGainLinear: Double;
  out C: TBiquadCoeff);
var
  CosW0, SinW0, Alpha, A0Inv, AlphaA, AlphaOverA: Double;
begin
  CosW0 := Cos(AW0);
  SinW0 := Sin(AW0);
  Alpha := SinW0 / (2.0 * AQ);

  case AType of
    ftLowPass:
      begin
        A0Inv := 1.0 / (1.0 + Alpha);
        C.A0 := ((1.0 - CosW0) / 2.0) * A0Inv;
        C.A1 := (1.0 - CosW0) * A0Inv;
        C.A2 := C.A0;
        C.B1 := (-2.0 * CosW0) * A0Inv;
        C.B2 := (1.0 - Alpha) * A0Inv;
      end;
    ftHighPass:
      begin
        A0Inv := 1.0 / (1.0 + Alpha);
        C.A0 := ((1.0 + CosW0) / 2.0) * A0Inv;
        C.A1 := -(1.0 + CosW0) * A0Inv;
        C.A2 := C.A0;
        C.B1 := (-2.0 * CosW0) * A0Inv;
        C.B2 := (1.0 - Alpha) * A0Inv;
      end;
    ftBandPass:
      begin
        A0Inv := 1.0 / (1.0 + Alpha);
        C.A0 := Alpha * A0Inv;
        C.A1 := 0.0;
        C.A2 := -Alpha * A0Inv;
        C.B1 := (-2.0 * CosW0) * A0Inv;
        C.B2 := (1.0 - Alpha) * A0Inv;
      end;
    ftNotch:
      begin
        A0Inv := 1.0 / (1.0 + Alpha);
        C.A0 := A0Inv;
        C.A1 := (-2.0 * CosW0) * A0Inv;
        C.A2 := A0Inv;
        C.B1 := C.A1;
        C.B2 := (1.0 - Alpha) * A0Inv;
      end;
    ftAllPass:
      begin
        A0Inv := 1.0 / (1.0 + Alpha);
        C.A0 := (1.0 - Alpha) * A0Inv;
        C.A1 := (-2.0 * CosW0) * A0Inv;
        C.A2 := (1.0 + Alpha) * A0Inv;
        C.B1 := C.A1;
        C.B2 := C.A0;
      end;
    ftPeaking:
      begin
        AlphaA := Alpha * AGainLinear;
        AlphaOverA := Alpha / AGainLinear;
        A0Inv := 1.0 / (1.0 + AlphaOverA);
        C.A0 := (1.0 + AlphaA) * A0Inv;
        C.A1 := (-2.0 * CosW0) * A0Inv;
        C.A2 := (1.0 - AlphaA) * A0Inv;
        C.B1 := C.A1;
        C.B2 := (1.0 - AlphaOverA) * A0Inv;
      end;
  else
    begin
      C.A0 := 1.0; C.A1 := 0.0; C.A2 := 0.0; C.B1 := 0.0; C.B2 := 0.0;
    end;
  end;
end;

function TSedaiFilter.ProcessBiquad(AInput: Single; const C: TBiquadCoeff;
  var X1, X2, Y1, Y2: Double): Single;
var
  Acc: Double;
begin
  // Direct Form I biquad, double-precision accumulation and state.
  Acc := C.A0 * AInput + C.A1 * X1 + C.A2 * X2 - C.B1 * Y1 - C.B2 * Y2;

  // Denormal prevention (applied to the stored state too).
  if Abs(Acc) < 1e-20 then
    Acc := 0.0;

  X2 := X1;
  X1 := AInput;
  Y2 := Y1;
  Y1 := Acc;

  Result := Acc;
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
  I: Integer;
begin
  // SID mode uses state variable filter
  if FSIDMode then
  begin
    Result := ProcessSIDFilter(AInput);
    Exit;
  end;

  if (AChannel < 0) or (AChannel > 1) then AChannel := 0;

  // Cascade the active 2nd-order sections (1 / 2 / 4 for 12 / 24 / 48 dB).
  Output := AInput;
  for I := 0 to FStageCount - 1 do
    Output := ProcessBiquad(Output, FCoeff[I],
      FState[I, AChannel].X1, FState[I, AChannel].X2,
      FState[I, AChannel].Y1, FState[I, AChannel].Y2);

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
