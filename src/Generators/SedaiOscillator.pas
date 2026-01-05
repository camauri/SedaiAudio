{*
 * Sedai Audio Foundation - Oscillator
 *
 * TSedaiOscillator provides multi-waveform oscillator with support for
 * sine, sawtooth, square, pulse (with variable width), triangle, and
 * noise waveforms. Includes SID-authentic combined waveform mode.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiOscillator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode;

type
  { TSedaiSignalGenerator }
  // Abstract base class for all signal generators
  TSedaiSignalGenerator = class(TSedaiSignalNode)
  protected
    FFrequency: Single;           // Frequency in Hz
    FPhase: Single;               // Current phase (0.0 - 1.0)
    FPhaseIncrement: Single;      // Phase increment per sample
    FAmplitude: Single;           // Output amplitude (0.0 - 1.0)

    procedure UpdatePhaseIncrement; virtual;
    procedure SetFrequency(AValue: Single); virtual;
    procedure SetAmplitude(AValue: Single); virtual;

  public
    constructor Create; override;

    // Reset phase to zero
    procedure ResetPhase; virtual;

    // Set phase (0.0 - 1.0)
    procedure SetPhase(AValue: Single); virtual;

    // Generate next sample
    function GenerateSample: Single; virtual; abstract;

    // Generate a block of samples
    procedure GenerateBlock(AOutput: PSingle; AFrameCount: Integer); virtual;

    // Properties
    property Frequency: Single read FFrequency write SetFrequency;
    property Phase: Single read FPhase write SetPhase;
    property Amplitude: Single read FAmplitude write SetAmplitude;
  end;

  { TSedaiOscillator }
  // Multi-waveform oscillator with SID-compatible mode
  TSedaiOscillator = class(TSedaiSignalGenerator)
  private
    FWaveform: TWaveformType;
    FPulseWidth: Single;          // Pulse width (0.0 - 1.0, default 0.5)
    FDetune: Single;              // Detune in cents
    FHardSync: Boolean;           // Hard sync enabled
    FSyncSource: TSedaiOscillator; // Sync source oscillator

    // SID-authentic mode
    FSIDMode: Boolean;            // Enable SID-authentic waveforms
    FSIDAccumulator: Cardinal;    // 24-bit phase accumulator (SID style)
    FSIDWaveform: Byte;           // Combined waveform bits (SID style)

    // Noise generator state
    FNoiseShiftReg: Cardinal;     // LFSR for noise generation
    FNoiseSample: Single;         // Cached noise sample
    FNoiseCounter: Integer;       // Counter for noise rate

    // Anti-aliasing
    FBandlimited: Boolean;        // Enable bandlimited waveforms

    // SuperSaw state (7 detuned saws)
    FSuperSawPhases: array[0..6] of Single;  // Phases for 7 saws
    FSuperSawDetune: Single;                  // Detune amount (0.0-1.0)
    FSuperSawMix: Single;                     // Mix between center and sides

    // PWM state
    FPWMPhase: Single;            // LFO phase for auto-PWM
    FPWMRate: Single;             // PWM modulation rate (Hz)
    FPWMDepth: Single;            // PWM modulation depth

    // Formant state
    FFormantFreq: Single;         // Formant frequency
    FFormantQ: Single;            // Formant resonance

    // Combined waveform state
    FCombinedMode: Boolean;                 // True when using combined waveforms
    FCombinedWaveforms: TWaveformSet;       // Set of waveforms to combine
    FCombineMode: TWaveformCombineMode;     // How to combine waveforms

    procedure SetWaveform(AValue: TWaveformType);
    procedure SetPulseWidth(AValue: Single);
    procedure SetDetune(AValue: Single);
    procedure UpdatePhaseIncrement; override;

    // Waveform generators
    function GenerateSine: Single;
    function GenerateSawtooth: Single;
    function GenerateSquare: Single;
    function GeneratePulse: Single;
    function GenerateTriangle: Single;
    function GenerateNoise: Single;

    // Bandlimited waveform generators (PolyBLEP)
    function GenerateBandlimitedSaw: Single;
    function GenerateBandlimitedSquare: Single;
    function GenerateBandlimitedPulse: Single;

    // SID-authentic waveform generators
    function GenerateSIDSaw: Single;
    function GenerateSIDTriangle: Single;
    function GenerateSIDPulse: Single;
    function GenerateSIDNoise: Single;
    function GenerateSIDCombined: Single;

    // Extended waveform generators
    function GenerateSuperSaw: Single;
    function GeneratePWM: Single;
    function GenerateHalfSine: Single;
    function GenerateFullSine: Single;
    function GenerateFormant: Single;
    function GenerateMetallic: Single;

    // Combined waveform generator
    function GenerateCombinedWaveform: Single;
    function GenerateWaveformByType(AWaveform: TWaveformType): Single;

    // PolyBLEP correction
    function PolyBLEP(APhase, APhaseInc: Single): Single;

  public
    constructor Create; override;

    // Reset oscillator state
    procedure Reset; override;

    // Generate next sample
    function GenerateSample: Single; override;

    // Process block (for signal graph)
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Set combined waveform (SID style: bits for Tri/Saw/Pulse/Noise)
    procedure SetSIDWaveform(ATriangle, ASawtooth, APulse, ANoise: Boolean);

    // Hard sync to another oscillator
    procedure SyncTo(ASource: TSedaiOscillator);

    // Combined waveform mode - allows mixing multiple waveforms
    procedure SetCombinedWaveforms(AWaveforms: TWaveformSet; AMode: TWaveformCombineMode = wcmMix);
    procedure ClearCombinedWaveforms;
    procedure AddWaveform(AWaveform: TWaveformType);
    procedure RemoveWaveform(AWaveform: TWaveformType);

    // Properties
    property Waveform: TWaveformType read FWaveform write SetWaveform;
    property PulseWidth: Single read FPulseWidth write SetPulseWidth;
    property Detune: Single read FDetune write SetDetune;
    property HardSync: Boolean read FHardSync write FHardSync;
    property SyncSource: TSedaiOscillator read FSyncSource write FSyncSource;
    property SIDMode: Boolean read FSIDMode write FSIDMode;
    property SIDWaveform: Byte read FSIDWaveform;
    property Bandlimited: Boolean read FBandlimited write FBandlimited;

    // Combined waveform properties
    property CombinedMode: Boolean read FCombinedMode;
    property CombinedWaveforms: TWaveformSet read FCombinedWaveforms;
    property CombineMode: TWaveformCombineMode read FCombineMode write FCombineMode;
  end;

implementation

const
  // SID constants
  SID_CLOCK_PAL = 985248;
  SID_ACCUMULATOR_BITS = 24;
  SID_ACCUMULATOR_MAX = 1 shl SID_ACCUMULATOR_BITS;  // 16777216

{ TSedaiSignalGenerator }

constructor TSedaiSignalGenerator.Create;
begin
  inherited Create;

  FFrequency := 440.0;
  FPhase := 0.0;
  FPhaseIncrement := 0.0;
  FAmplitude := 1.0;

  UpdatePhaseIncrement;
end;

procedure TSedaiSignalGenerator.UpdatePhaseIncrement;
begin
  if FSampleRate > 0 then
    FPhaseIncrement := FFrequency / FSampleRate
  else
    FPhaseIncrement := 0.0;
end;

procedure TSedaiSignalGenerator.SetFrequency(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > FSampleRate / 2 then
    AValue := FSampleRate / 2;

  FFrequency := AValue;
  UpdatePhaseIncrement;
end;

procedure TSedaiSignalGenerator.SetAmplitude(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;

  FAmplitude := AValue;
end;

procedure TSedaiSignalGenerator.ResetPhase;
begin
  FPhase := 0.0;
end;

procedure TSedaiSignalGenerator.SetPhase(AValue: Single);
begin
  // Wrap to 0-1 range
  FPhase := AValue - Floor(AValue);
end;

procedure TSedaiSignalGenerator.GenerateBlock(AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
begin
  for I := 0 to AFrameCount - 1 do
    AOutput[I] := GenerateSample;
end;

{ TSedaiOscillator }

constructor TSedaiOscillator.Create;
begin
  inherited Create;

  FWaveform := wtSine;
  FPulseWidth := 0.5;
  FDetune := 0.0;
  FHardSync := False;
  FSyncSource := nil;

  FSIDMode := False;
  FSIDAccumulator := 0;
  FSIDWaveform := 0;

  // Initialize LFSR with non-zero value
  FNoiseShiftReg := $7FFFF8;
  FNoiseSample := 0.0;
  FNoiseCounter := 0;

  FBandlimited := True;

  // Initialize SuperSaw state
  FillChar(FSuperSawPhases, SizeOf(FSuperSawPhases), 0);
  FSuperSawDetune := 0.3;  // Default detune amount
  FSuperSawMix := 0.75;    // Default mix (favor detuned saws)

  // Initialize PWM state
  FPWMPhase := 0.0;
  FPWMRate := 0.5;         // Default 0.5 Hz
  FPWMDepth := 0.4;        // 40% modulation depth

  // Initialize Formant state
  FFormantFreq := 800.0;   // Default formant freq
  FFormantQ := 5.0;        // Default Q

  // Initialize Combined waveform state
  FCombinedMode := False;
  FCombinedWaveforms := [];
  FCombineMode := wcmMix;
end;

procedure TSedaiOscillator.Reset;
begin
  inherited Reset;

  FPhase := 0.0;
  FSIDAccumulator := 0;
  FNoiseShiftReg := $7FFFF8;
  FNoiseSample := 0.0;
  FNoiseCounter := 0;
end;

procedure TSedaiOscillator.SetWaveform(AValue: TWaveformType);
begin
  FWaveform := AValue;

  // Update SID waveform bits
  case AValue of
    wtTriangle: FSIDWaveform := $10;
    wtSawtooth: FSIDWaveform := $20;
    wtPulse, wtSquare: FSIDWaveform := $40;
    wtNoise: FSIDWaveform := $80;
    else FSIDWaveform := 0;
  end;
end;

procedure TSedaiOscillator.SetPulseWidth(AValue: Single);
begin
  if AValue < 0.01 then
    AValue := 0.01
  else if AValue > 0.99 then
    AValue := 0.99;

  FPulseWidth := AValue;
end;

procedure TSedaiOscillator.SetDetune(AValue: Single);
begin
  FDetune := AValue;
  UpdatePhaseIncrement;
end;

procedure TSedaiOscillator.UpdatePhaseIncrement;
var
  DetuneMultiplier: Single;
begin
  // Apply detune (cents to frequency multiplier)
  if Abs(FDetune) > 0.001 then
    DetuneMultiplier := Power(2.0, FDetune / 1200.0)
  else
    DetuneMultiplier := 1.0;

  if FSampleRate > 0 then
    FPhaseIncrement := (FFrequency * DetuneMultiplier) / FSampleRate
  else
    FPhaseIncrement := 0.0;
end;

function TSedaiOscillator.GenerateSine: Single;
begin
  Result := Sin(FPhase * 2.0 * PI);
end;

function TSedaiOscillator.GenerateSawtooth: Single;
begin
  // Naive sawtooth: -1 to +1 ramp
  Result := 2.0 * FPhase - 1.0;
end;

function TSedaiOscillator.GenerateSquare: Single;
begin
  if FPhase < 0.5 then
    Result := 1.0
  else
    Result := -1.0;
end;

function TSedaiOscillator.GeneratePulse: Single;
begin
  if FPhase < FPulseWidth then
    Result := 1.0
  else
    Result := -1.0;
end;

function TSedaiOscillator.GenerateTriangle: Single;
begin
  if FPhase < 0.5 then
    Result := 4.0 * FPhase - 1.0
  else
    Result := 3.0 - 4.0 * FPhase;
end;

function TSedaiOscillator.GenerateNoise: Single;
var
  Bit: Cardinal;
begin
  // Clock the LFSR at a rate proportional to frequency
  Inc(FNoiseCounter);
  if FNoiseCounter >= Round(FSampleRate / (FFrequency * 16)) then
  begin
    FNoiseCounter := 0;

    // SID-style LFSR: feedback from bits 17 and 22
    Bit := ((FNoiseShiftReg shr 17) xor (FNoiseShiftReg shr 22)) and 1;
    FNoiseShiftReg := (FNoiseShiftReg shl 1) or Bit;

    // Convert LFSR bits to sample (use upper 8 bits for value)
    FNoiseSample := ((FNoiseShiftReg shr 15) and $FF) / 127.5 - 1.0;
  end;

  Result := FNoiseSample;
end;

function TSedaiOscillator.PolyBLEP(APhase, APhaseInc: Single): Single;
var
  T: Single;
begin
  // PolyBLEP (polynomial band-limited step)
  // Reduces aliasing at discontinuities

  if APhase < APhaseInc then
  begin
    // Rising edge
    T := APhase / APhaseInc;
    Result := T + T - T * T - 1.0;
  end
  else if APhase > 1.0 - APhaseInc then
  begin
    // Falling edge
    T := (APhase - 1.0) / APhaseInc;
    Result := T * T + T + T + 1.0;
  end
  else
    Result := 0.0;
end;

function TSedaiOscillator.GenerateBandlimitedSaw: Single;
begin
  // Naive sawtooth with PolyBLEP correction
  Result := GenerateSawtooth - PolyBLEP(FPhase, FPhaseIncrement);
end;

function TSedaiOscillator.GenerateBandlimitedSquare: Single;
var
  Phase2: Single;
begin
  Result := GenerateSquare;

  // Apply PolyBLEP at both transitions
  Result := Result - PolyBLEP(FPhase, FPhaseIncrement);

  Phase2 := FPhase + 0.5;
  if Phase2 >= 1.0 then
    Phase2 := Phase2 - 1.0;

  Result := Result + PolyBLEP(Phase2, FPhaseIncrement);
end;

function TSedaiOscillator.GenerateBandlimitedPulse: Single;
var
  Phase2: Single;
begin
  Result := GeneratePulse;

  // Apply PolyBLEP at both transitions
  Result := Result - PolyBLEP(FPhase, FPhaseIncrement);

  Phase2 := FPhase + (1.0 - FPulseWidth);
  if Phase2 >= 1.0 then
    Phase2 := Phase2 - 1.0;

  Result := Result + PolyBLEP(Phase2, FPhaseIncrement);
end;

function TSedaiOscillator.GenerateSIDSaw: Single;
begin
  // SID sawtooth: upper 12 bits of 24-bit accumulator
  Result := ((FSIDAccumulator shr 12) and $FFF) / 2047.5 - 1.0;
end;

function TSedaiOscillator.GenerateSIDTriangle: Single;
var
  TriValue: Cardinal;
begin
  // SID triangle: XOR fold of accumulator bit 23
  TriValue := FSIDAccumulator shr 12;
  if (FSIDAccumulator and $800000) <> 0 then
    TriValue := TriValue xor $FFF;

  Result := (TriValue and $FFF) / 2047.5 - 1.0;
end;

function TSedaiOscillator.GenerateSIDPulse: Single;
var
  PulseThreshold: Cardinal;
begin
  // SID pulse: compare accumulator with 12-bit pulse width
  PulseThreshold := Round(FPulseWidth * $FFF);

  if (FSIDAccumulator shr 12) >= PulseThreshold then
    Result := 1.0
  else
    Result := -1.0;
end;

function TSedaiOscillator.GenerateSIDNoise: Single;
begin
  // SID noise uses LFSR clocked by bit 19 of accumulator
  Result := GenerateNoise;
end;

function TSedaiOscillator.GenerateSIDCombined: Single;
var
  SawVal, TriVal, PulseVal: Cardinal;
  Combined: Cardinal;
begin
  // SID combined waveforms use bitwise AND of digital values
  Combined := $FFF;

  if (FSIDWaveform and $20) <> 0 then
  begin
    // Sawtooth
    SawVal := (FSIDAccumulator shr 12) and $FFF;
    Combined := Combined and SawVal;
  end;

  if (FSIDWaveform and $10) <> 0 then
  begin
    // Triangle
    TriVal := FSIDAccumulator shr 12;
    if (FSIDAccumulator and $800000) <> 0 then
      TriVal := TriVal xor $FFF;
    TriVal := TriVal and $FFF;
    Combined := Combined and TriVal;
  end;

  if (FSIDWaveform and $40) <> 0 then
  begin
    // Pulse
    if (FSIDAccumulator shr 12) >= Round(FPulseWidth * $FFF) then
      PulseVal := $FFF
    else
      PulseVal := 0;
    Combined := Combined and PulseVal;
  end;

  Result := Combined / 2047.5 - 1.0;

  // Add noise if enabled (additive, not AND)
  if (FSIDWaveform and $80) <> 0 then
    Result := Result * 0.5 + GenerateSIDNoise * 0.5;
end;

// ============================================================================
// EXTENDED WAVEFORM GENERATORS
// ============================================================================

function TSedaiOscillator.GenerateSuperSaw: Single;
const
  // Detune offsets for 7 saws (in semitones, normalized)
  DetuneOffsets: array[0..6] of Single = (-0.11002313, -0.06288439, -0.01952356,
                                           0.0,
                                           0.01991221, 0.06216538, 0.10745242);
var
  I: Integer;
  DetuneFactor, PhaseInc, SawPhase, Sample: Single;
  CenterWeight, SideWeight: Single;
begin
  Result := 0.0;

  // Calculate weights (center saw vs detuned saws)
  CenterWeight := 1.0 - FSuperSawMix;
  SideWeight := FSuperSawMix / 6.0;  // Distribute among 6 side saws

  for I := 0 to 6 do
  begin
    // Calculate detuned phase increment
    DetuneFactor := DetuneOffsets[I] * FSuperSawDetune;
    PhaseInc := FPhaseIncrement * Power(2.0, DetuneFactor);

    // Advance phase for this saw
    FSuperSawPhases[I] := FSuperSawPhases[I] + PhaseInc;
    if FSuperSawPhases[I] >= 1.0 then
      FSuperSawPhases[I] := FSuperSawPhases[I] - 1.0;

    // Generate saw value
    SawPhase := FSuperSawPhases[I];
    Sample := 2.0 * SawPhase - 1.0;

    // Apply weight
    if I = 3 then
      Result := Result + Sample * CenterWeight
    else
      Result := Result + Sample * SideWeight;
  end;

  // Normalize output
  Result := Result * 0.7;  // Reduce level to avoid clipping
end;

function TSedaiOscillator.GeneratePWM: Single;
var
  ModulatedPW: Single;
begin
  // Auto-modulating pulse width
  // Advance PWM LFO phase
  FPWMPhase := FPWMPhase + (FPWMRate / FSampleRate);
  if FPWMPhase >= 1.0 then
    FPWMPhase := FPWMPhase - 1.0;

  // Calculate modulated pulse width (0.1 to 0.9 range)
  ModulatedPW := 0.5 + Sin(FPWMPhase * 2.0 * Pi) * FPWMDepth * 0.4;

  // Generate pulse with modulated width
  if FPhase < ModulatedPW then
    Result := 1.0
  else
    Result := -1.0;
end;

function TSedaiOscillator.GenerateHalfSine: Single;
var
  SineVal: Single;
begin
  // Half-wave rectified sine (only positive half)
  SineVal := Sin(FPhase * 2.0 * Pi);
  if SineVal > 0 then
    Result := SineVal
  else
    Result := 0.0;

  // Shift to bipolar (-1 to 1) range
  Result := Result * 2.0 - 1.0;
end;

function TSedaiOscillator.GenerateFullSine: Single;
begin
  // Full-wave rectified sine (absolute value)
  Result := Abs(Sin(FPhase * 2.0 * Pi));

  // Shift to bipolar range
  Result := Result * 2.0 - 1.0;
end;

function TSedaiOscillator.GenerateFormant: Single;
var
  Carrier, Formant: Single;
  FormantPhase: Single;
begin
  // Simple formant synthesis using carrier * formant
  // Carrier is a pulse train, formant is a resonant sine burst

  // Carrier: impulse train (sync pulse)
  if FPhase < 0.1 then
    Carrier := 1.0
  else
    Carrier := 0.0;

  // Formant: decaying sine at formant frequency
  FormantPhase := FPhase * (FFormantFreq / FFrequency);
  Formant := Sin(FormantPhase * 2.0 * Pi) * Exp(-FPhase * FFormantQ);

  // Mix carrier envelope with formant
  Result := Carrier * Formant * 4.0;  // Boost level

  // Clamp output
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

function TSedaiOscillator.GenerateMetallic: Single;
var
  Ratio1, Ratio2: Single;
  Osc1, Osc2: Single;
begin
  // Metallic sound using inharmonic ratios (ring mod style)
  // Use golden ratio and sqrt(2) for bell-like inharmonic tones
  Ratio1 := 1.618033988749895;  // Golden ratio
  Ratio2 := 2.414213562373095;  // 1 + sqrt(2)

  Osc1 := Sin(FPhase * Ratio1 * 2.0 * Pi);
  Osc2 := Sin(FPhase * Ratio2 * 2.0 * Pi);

  // Ring modulation style mixing
  Result := (Osc1 * Osc2 + Osc1 * 0.3 + Osc2 * 0.2) * 0.7;
end;

function TSedaiOscillator.GenerateSample: Single;
var
  OldPhase: Single;
begin
  // Check for hard sync
  if FHardSync and Assigned(FSyncSource) then
  begin
    OldPhase := FSyncSource.Phase;
    // Sync when source crosses zero
    if (OldPhase < FSyncSource.FPhaseIncrement) and (OldPhase >= 0) then
      FPhase := 0.0;
  end;

  // Generate waveform
  if FCombinedMode and (FCombinedWaveforms <> []) then
  begin
    // Combined waveform mode - mix/combine multiple waveforms
    Result := GenerateCombinedWaveform;
  end
  else if FSIDMode and (FSIDWaveform <> 0) then
  begin
    // SID-authentic mode
    if (FSIDWaveform and (FSIDWaveform - 1)) <> 0 then
      Result := GenerateSIDCombined  // Multiple waveforms combined
    else
    begin
      case FSIDWaveform of
        $10: Result := GenerateSIDTriangle;
        $20: Result := GenerateSIDSaw;
        $40: Result := GenerateSIDPulse;
        $80: Result := GenerateSIDNoise;
        else Result := 0.0;
      end;
    end;

    // Update SID accumulator
    FSIDAccumulator := (FSIDAccumulator + Round(FPhaseIncrement * SID_ACCUMULATOR_MAX))
                       and (SID_ACCUMULATOR_MAX - 1);
  end
  else
  begin
    // Standard waveforms
    case FWaveform of
      wtSine:
        Result := GenerateSine;

      wtSawtooth:
        if FBandlimited then
          Result := GenerateBandlimitedSaw
        else
          Result := GenerateSawtooth;

      wtSquare:
        if FBandlimited then
          Result := GenerateBandlimitedSquare
        else
          Result := GenerateSquare;

      wtPulse:
        if FBandlimited then
          Result := GenerateBandlimitedPulse
        else
          Result := GeneratePulse;

      wtTriangle:
        Result := GenerateTriangle;  // Triangle is already bandlimited

      wtNoise:
        Result := GenerateNoise;

      // Extended waveforms
      wtSuperSaw:
        Result := GenerateSuperSaw;

      wtPWM:
        Result := GeneratePWM;

      wtHalfSine:
        Result := GenerateHalfSine;

      wtFullSine:
        Result := GenerateFullSine;

      wtFormant:
        Result := GenerateFormant;

      wtMetallic:
        Result := GenerateMetallic;

      wtCustom:
        Result := 0.0;  // Custom wavetable needs external handling

      else
        Result := 0.0;
    end;
  end;

  // Apply amplitude
  Result := Result * FAmplitude;

  // Advance phase
  FPhase := FPhase + FPhaseIncrement;
  if FPhase >= 1.0 then
    FPhase := FPhase - 1.0;
end;

procedure TSedaiOscillator.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
begin
  // Oscillator ignores input and generates output
  for I := 0 to AFrameCount - 1 do
  begin
    // Output mono to stereo (interleaved)
    AOutput[I * 2] := GenerateSample;      // Left
    AOutput[I * 2 + 1] := AOutput[I * 2];  // Right (duplicate)
  end;
end;

procedure TSedaiOscillator.SetSIDWaveform(ATriangle, ASawtooth, APulse, ANoise: Boolean);
begin
  FSIDWaveform := 0;

  if ATriangle then FSIDWaveform := FSIDWaveform or $10;
  if ASawtooth then FSIDWaveform := FSIDWaveform or $20;
  if APulse then FSIDWaveform := FSIDWaveform or $40;
  if ANoise then FSIDWaveform := FSIDWaveform or $80;

  FSIDMode := True;
end;

procedure TSedaiOscillator.SyncTo(ASource: TSedaiOscillator);
begin
  FSyncSource := ASource;
  FHardSync := Assigned(ASource);
end;

// ============================================================================
// COMBINED WAVEFORM METHODS
// ============================================================================

procedure TSedaiOscillator.SetCombinedWaveforms(AWaveforms: TWaveformSet; AMode: TWaveformCombineMode);
begin
  FCombinedWaveforms := AWaveforms;
  FCombineMode := AMode;
  FCombinedMode := AWaveforms <> [];
end;

procedure TSedaiOscillator.ClearCombinedWaveforms;
begin
  FCombinedWaveforms := [];
  FCombinedMode := False;
end;

procedure TSedaiOscillator.AddWaveform(AWaveform: TWaveformType);
begin
  Include(FCombinedWaveforms, AWaveform);
  FCombinedMode := True;
end;

procedure TSedaiOscillator.RemoveWaveform(AWaveform: TWaveformType);
begin
  Exclude(FCombinedWaveforms, AWaveform);
  FCombinedMode := FCombinedWaveforms <> [];
end;

function TSedaiOscillator.GenerateWaveformByType(AWaveform: TWaveformType): Single;
begin
  case AWaveform of
    wtSine:
      Result := GenerateSine;
    wtSawtooth:
      if FBandlimited then
        Result := GenerateBandlimitedSaw
      else
        Result := GenerateSawtooth;
    wtSquare:
      if FBandlimited then
        Result := GenerateBandlimitedSquare
      else
        Result := GenerateSquare;
    wtPulse:
      if FBandlimited then
        Result := GenerateBandlimitedPulse
      else
        Result := GeneratePulse;
    wtTriangle:
      Result := GenerateTriangle;
    wtNoise:
      Result := GenerateNoise;
    wtSuperSaw:
      Result := GenerateSuperSaw;
    wtPWM:
      Result := GeneratePWM;
    wtHalfSine:
      Result := GenerateHalfSine;
    wtFullSine:
      Result := GenerateFullSine;
    wtFormant:
      Result := GenerateFormant;
    wtMetallic:
      Result := GenerateMetallic;
    else
      Result := 0.0;
  end;
end;

function TSedaiOscillator.GenerateCombinedWaveform: Single;
var
  WT: TWaveformType;
  Sample: Single;
  Count: Integer;
  DigitalVal, CombinedDigital: Integer;
begin
  Result := 0.0;
  Count := 0;

  case FCombineMode of
    wcmMix:
      begin
        // Additive mixing - sum all waveforms and normalize
        for WT := Low(TWaveformType) to High(TWaveformType) do
        begin
          if WT in FCombinedWaveforms then
          begin
            Result := Result + GenerateWaveformByType(WT);
            Inc(Count);
          end;
        end;
        // Normalize by number of waveforms
        if Count > 1 then
          Result := Result / Count;
      end;

    wcmAND:
      begin
        // SID-style AND combination - convert to 12-bit digital, AND together
        CombinedDigital := $FFF;  // Start with all bits set
        for WT := Low(TWaveformType) to High(TWaveformType) do
        begin
          if WT in FCombinedWaveforms then
          begin
            Sample := GenerateWaveformByType(WT);
            // Convert -1..+1 to 0..4095 (12-bit)
            DigitalVal := Round((Sample + 1.0) * 2047.5);
            if DigitalVal < 0 then DigitalVal := 0;
            if DigitalVal > $FFF then DigitalVal := $FFF;
            // AND with accumulated value
            CombinedDigital := CombinedDigital and DigitalVal;
            Inc(Count);
          end;
        end;
        // Convert back to -1..+1
        if Count > 0 then
          Result := (CombinedDigital / 2047.5) - 1.0
        else
          Result := 0.0;
      end;

    wcmMultiply:
      begin
        // Ring modulation style - multiply all waveforms together
        Result := 1.0;
        for WT := Low(TWaveformType) to High(TWaveformType) do
        begin
          if WT in FCombinedWaveforms then
          begin
            Result := Result * GenerateWaveformByType(WT);
            Inc(Count);
          end;
        end;
        if Count = 0 then
          Result := 0.0;
      end;

    wcmMin:
      begin
        // Minimum of all waveforms
        Result := 1.0;  // Start high
        for WT := Low(TWaveformType) to High(TWaveformType) do
        begin
          if WT in FCombinedWaveforms then
          begin
            Sample := GenerateWaveformByType(WT);
            if (Count = 0) or (Sample < Result) then
              Result := Sample;
            Inc(Count);
          end;
        end;
        if Count = 0 then
          Result := 0.0;
      end;

    wcmMax:
      begin
        // Maximum of all waveforms
        Result := -1.0;  // Start low
        for WT := Low(TWaveformType) to High(TWaveformType) do
        begin
          if WT in FCombinedWaveforms then
          begin
            Sample := GenerateWaveformByType(WT);
            if (Count = 0) or (Sample > Result) then
              Result := Sample;
            Inc(Count);
          end;
        end;
        if Count = 0 then
          Result := 0.0;
      end;
  end;
end;

end.
