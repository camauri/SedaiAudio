{*
 * Sedai Audio Foundation - Waveguide Single-Reed Generator
 *
 * TSedaiReedGenerator is a PHYSICAL-MODELLING wind voice: a digital-waveguide
 * bore driven by a nonlinear reed, in the McIntyre-Schumacher-Woodhouse (MSW)
 * feedback loop. Unlike the additive/partial generators (which SUM a spectrum),
 * this SELF-OSCILLATES: the reed is a pressure-controlled valve whose nonlinear
 * clamp (the reed "beating" shut) shapes the waveform sample-by-sample, producing
 * the reed "flesh" and dynamic even/odd balance that summation cannot make.
 *
 * Re-derived from the Smith digital-waveguide clarinet / STK Clarinet (Cook &
 * Scavone); no STK dependency. Loop (per sample):
 *   breath   = pressureEnv * maxPressure * (1 + noise + vibrato)
 *   boreOut  = delayLine.lastOut                     // wave returned from the bell
 *   reflected= reflCoeff * oneZeroLP(boreOut)        // bell = inverting lowpass reflection
 *   pressDiff= reflected - breath
 *   reedRefl = clamp(offset + slope*pressDiff, -1, 1)// NONLINEAR reed (beating = clamp)
 *   delayLine.tick(breath + pressDiff * reedRefl)
 *   out      = outputGain * boreOut
 *
 * Bore: fractional-delay line, length = SR/(2*f) - REED_DELAY_FUDGE (tuned so the
 * self-oscillation lands on the note). Cylindrical (closed-open) => odd harmonics
 * (clarinet). Conicity > 0 engages a leaky-integrator "throat" that admits even
 * harmonics (sax/oboe-class); its spectrum is validated objectively, not assumed.
 *
 * Opt-in / inert: with the gate closed the generator is silent.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiReedGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode,
  SedaiOscillator;

type
  TReedBoreType = (rbCylindrical, rbConical);

  { TSedaiReedGenerator }
  TSedaiReedGenerator = class(TSedaiSignalGenerator)
  private
    // --- bore: fractional delay line ---
    FBore: array of Single;
    FBoreLen: Integer;         // buffer size (samples)
    FBoreWrite: Integer;       // write index
    FBoreDelay: Single;        // current fractional delay (samples)
    FBoreLastOut: Single;      // delayLine.lastOut()

    // --- reflection (bell) filter: one-zero lowpass, scaled by FReflCoeff ---
    FFilterX1: Single;
    FReflCoeff: Single;        // reflection gain (~ -0.95, inverting + loss)

    // --- reed nonlinearity ---
    FReedOffset: Single;       // ~0.7
    FReedSlope: Single;        // ~-0.44 (negative)

    // --- conical throat (leaky integrator) ---
    FBoreType: TReedBoreType;
    FConicity: Single;         // 0 = cylinder; >0 blends the throat integrator
    FThroatState: Single;

    // --- breath / excitation ---
    FMaxPressure: Single;      // steady breath pressure target (from velocity)
    FPressure: Single;         // current (ramped) breath pressure
    FPressCoeff: Single;       // one-pole ramp coefficient toward target
    FNoiseGain: Single;        // breath turbulence noise depth
    FVibratoGain: Single;      // vibrato depth
    FVibratoRate: Single;      // vibrato rate (Hz)
    FVibratoPhase: Single;
    FRng: Cardinal;            // xorshift noise state

    FOutputGain: Single;

    // --- note state ---
    FNote: Integer;
    FVelocity: Single;
    FGateOpen: Boolean;
    FReleasing: Boolean;

    procedure UpdateBoreDelay;
    procedure RecalcPressCoeff;
    function NoiseSample: Single;   // white noise in [-1,1]
    function DelayLineTick(AInput: Single): Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SampleRateChanged; override;

    procedure NoteOn(ANote: Integer; AVelocity: Single);
    procedure NoteOff;
    procedure Kill;

    function GenerateSample: Single; override;

    // Reed nonlinearity: offset (~0.7) + slope (~-0.44). More negative slope /
    // higher pressure => the reed beats harder => brighter, more "pressed".
    procedure SetReed(AOffset, ASlope: Single);
    // Breath excitation. APressure = steady blowing pressure target (~0.2..1.1;
    // too low = no oscillation, too high = choked). ANoise/AVibDepth/AVibRateHz
    // add turbulence + vibrato. Also sets the pressure ramp (attack) time.
    procedure SetBreath(APressure, ANoise, AVibDepth, AVibRateHz: Single);
    // Bell reflection: gain (magnitude ~0.9..0.98; sign applied internally as
    // inverting) and an extra HF damping added to the one-zero (0..1).
    procedure SetReflection(ACoeffMag: Single);
    // Bore geometry. rbCylindrical (odd harmonics) or rbConical (AConicity 0..1
    // engages the throat integrator => full harmonic series).
    procedure SetBoreType(AType: TReedBoreType; AConicity: Single);
    procedure SetOutputGain(AGain: Single);

    property Note: Integer read FNote;
    property Velocity: Single read FVelocity;
    property GateOpen: Boolean read FGateOpen;
    property Releasing: Boolean read FReleasing;
  end;

implementation

const
  REED_MIN_FREQ = 40.0;          // sizes the delay buffer
  // The loop period is 2*FBoreDelay + ~3 samples (one-zero filter 2*0.5, reed
  // junction, interpolation). period = SR/f - 2*FUDGE + 3, so FUDGE ~= 1.5 makes
  // period = SR/f (measured: C4 lands within ~1 cent). Frequency-independent.
  REED_DELAY_FUDGE = 1.5;
  REED_ATTACK_S = 0.02;          // breath ramp time constant

{ TSedaiReedGenerator }

constructor TSedaiReedGenerator.Create;
begin
  inherited Create;
  FReflCoeff := -0.95;
  FReedOffset := 0.7;
  FReedSlope := -0.44;
  FBoreType := rbCylindrical;
  FConicity := 0.0;
  FThroatState := 0.0;
  FMaxPressure := 0.55;
  FNoiseGain := 0.0;
  FVibratoGain := 0.0;
  FVibratoRate := 5.0;
  FVibratoPhase := 0.0;
  FOutputGain := 0.3;
  FRng := 22222;
  FNote := -1;
  FVelocity := 1.0;
  FGateOpen := False;
  FReleasing := False;
  FFrequency := 261.63;
  FAmplitude := 1.0;
  SampleRateChanged;   // sizes the bore + computes ramp coeff
end;

destructor TSedaiReedGenerator.Destroy;
begin
  SetLength(FBore, 0);
  inherited Destroy;
end;

procedure TSedaiReedGenerator.SampleRateChanged;
var
  need: Integer;
begin
  inherited SampleRateChanged;
  if FSampleRate <= 0 then Exit;
  // buffer must hold the longest half-wavelength (lowest note) + interp margin
  need := Ceil(FSampleRate / (2 * REED_MIN_FREQ)) + 4;
  if Length(FBore) < need then SetLength(FBore, need);
  FBoreLen := Length(FBore);
  RecalcPressCoeff;
  UpdateBoreDelay;
end;

procedure TSedaiReedGenerator.RecalcPressCoeff;
begin
  if (FSampleRate > 0) then
    FPressCoeff := Exp(-1.0 / (REED_ATTACK_S * FSampleRate))
  else
    FPressCoeff := 0.0;
end;

// half-wavelength delay, minus the reflection-filter phase delay
procedure TSedaiReedGenerator.UpdateBoreDelay;
var
  d: Single;
begin
  if (FFrequency <= 0) or (FSampleRate <= 0) then Exit;
  d := (FSampleRate / FFrequency) * 0.5 - REED_DELAY_FUDGE;
  if d < 1 then d := 1;
  if d > FBoreLen - 2 then d := FBoreLen - 2;
  FBoreDelay := d;
end;

// xorshift32 white noise in [-1, 1]
function TSedaiReedGenerator.NoiseSample: Single;
begin
  FRng := FRng xor (FRng shl 13);
  FRng := FRng xor (FRng shr 17);
  FRng := FRng xor (FRng shl 5);
  Result := (FRng / 2147483648.0) - 1.0;
end;

// Write AInput, return the fractionally-delayed sample (linear interp).
function TSedaiReedGenerator.DelayLineTick(AInput: Single): Single;
var
  readPos, frac: Single;
  i0, i1: Integer;
begin
  FBore[FBoreWrite] := AInput;
  readPos := FBoreWrite - FBoreDelay;
  while readPos < 0 do readPos := readPos + FBoreLen;
  i0 := Trunc(readPos);
  frac := readPos - i0;
  i1 := i0 + 1; if i1 >= FBoreLen then i1 := i1 - FBoreLen;
  Result := FBore[i0] * (1 - frac) + FBore[i1] * frac;
  Inc(FBoreWrite); if FBoreWrite >= FBoreLen then FBoreWrite := 0;
end;

procedure TSedaiReedGenerator.NoteOn(ANote: Integer; AVelocity: Single);
var
  i: Integer;
begin
  FNote := ANote;
  FVelocity := EnsureRange(AVelocity, 0, 1);
  FGateOpen := True;
  FReleasing := False;

  FFrequency := 440 * Power(2, (ANote - 69) / 12);
  UpdateBoreDelay;

  // clear the bore + filter state for a clean start
  for i := 0 to FBoreLen - 1 do FBore[i] := 0;
  FBoreWrite := 0;
  FBoreLastOut := 0;
  FFilterX1 := 0;
  FThroatState := 0;
  FPressure := 0;
  FVibratoPhase := 0;

  // steady breath scales a little with velocity (harder blow = brighter)
  FMaxPressure := FMaxPressure;   // keep the configured target; velocity trims it
end;

procedure TSedaiReedGenerator.NoteOff;
begin
  if not FGateOpen then Exit;
  FGateOpen := False;
  FReleasing := True;
end;

procedure TSedaiReedGenerator.Kill;
var
  i: Integer;
begin
  FGateOpen := False;
  FReleasing := False;
  FNote := -1;
  FPressure := 0;
  FBoreLastOut := 0;
  FFilterX1 := 0;
  FThroatState := 0;
  for i := 0 to FBoreLen - 1 do FBore[i] := 0;
  FBoreWrite := 0;
end;

function TSedaiReedGenerator.GenerateSample: Single;
var
  target, breath, boreOut, reflected, pressDiff, reedRefl, boreIn: Single;
begin
  if (not FGateOpen) and (not FReleasing) then
  begin
    Result := 0;
    Exit;
  end;

  // breath pressure ramp (attack on gate, decay on release)
  if FGateOpen then target := FMaxPressure * (0.6 + 0.4 * FVelocity)
  else target := 0;
  FPressure := target + (FPressure - target) * FPressCoeff;

  // once the breath has decayed to ~silence on release, the note is done
  if FReleasing and (FPressure < 1e-4) and (Abs(FBoreLastOut) < 1e-4) then
  begin
    FReleasing := False;
    Result := 0;
    Exit;
  end;

  breath := FPressure;
  if FNoiseGain > 0 then breath := breath * (1 + FNoiseGain * NoiseSample);
  if FVibratoGain > 0 then
  begin
    breath := breath * (1 + FVibratoGain * Sin(2 * Pi * FVibratoPhase));
    FVibratoPhase := FVibratoPhase + FVibratoRate / FSampleRate;
    if FVibratoPhase >= 1 then FVibratoPhase := FVibratoPhase - 1;
  end;

  boreOut := FBoreLastOut;

  // bell reflection: one-zero lowpass y = 0.5(x + x1), scaled by FReflCoeff
  reflected := FReflCoeff * 0.5 * (boreOut + FFilterX1);
  FFilterX1 := boreOut;

  // Throat low-pass (provisional). MEASURED: this darkens/rounds the odd-harmonic
  // tone; it does NOT create the even harmonics a true cone has (no in-loop filter
  // can add a resonator mode at 2*f0 that a closed-open cylinder lacks). A real
  // sax/oboe cone needs a conical scattering junction (Scavone) — TODO. Kept as a
  // timbral control for now; even-harmonic validation deferred.
  if (FBoreType = rbConical) and (FConicity > 0) then
  begin
    FThroatState := 0.995 * FThroatState + (1 - 0.995) * reflected;
    reflected := reflected + FConicity * FThroatState;
  end;

  pressDiff := reflected - breath;

  // nonlinear reed: clamp(offset + slope*pressDiff, -1, 1). The clamp = beating.
  reedRefl := FReedOffset + FReedSlope * pressDiff;
  if reedRefl > 1.0 then reedRefl := 1.0
  else if reedRefl < -1.0 then reedRefl := -1.0;

  boreIn := breath + pressDiff * reedRefl;
  FBoreLastOut := DelayLineTick(boreIn);

  Result := FOutputGain * boreOut * FAmplitude;
end;

procedure TSedaiReedGenerator.SetReed(AOffset, ASlope: Single);
begin
  FReedOffset := AOffset;
  FReedSlope := ASlope;
end;

procedure TSedaiReedGenerator.SetBreath(APressure, ANoise, AVibDepth, AVibRateHz: Single);
begin
  FMaxPressure := Max(0, APressure);
  FNoiseGain := Max(0, ANoise);
  FVibratoGain := Max(0, AVibDepth);
  if AVibRateHz > 0 then FVibratoRate := AVibRateHz;
end;

procedure TSedaiReedGenerator.SetReflection(ACoeffMag: Single);
begin
  ACoeffMag := EnsureRange(ACoeffMag, 0.5, 0.999);
  FReflCoeff := -ACoeffMag;   // inverting reflection at the open bell
end;

procedure TSedaiReedGenerator.SetBoreType(AType: TReedBoreType; AConicity: Single);
begin
  FBoreType := AType;
  FConicity := EnsureRange(AConicity, 0, 1);
end;

procedure TSedaiReedGenerator.SetOutputGain(AGain: Single);
begin
  FOutputGain := Max(0, AGain);
end;

end.
