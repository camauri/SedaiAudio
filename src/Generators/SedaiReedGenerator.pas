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
 * Two bore models (re-derived from the Smith/STK clarinet and STK Saxofony;
 * Cook & Scavone; no STK dependency):
 *
 *  rbCylindrical (clarinet): ONE delay line, HALF-period, inverting reflection =>
 *    modes at (2n-1)f0 (odd harmonics). Validated to ~0.4 cents across MIDI 48..72.
 *
 *  rbConical (faux cone / sax-class): TWO delay lines summing to a FULL period,
 *    with the reed injected at an interior "blow position" between them (one end
 *    rigid, the far end a lossy inverting bell). Exciting the bore off-end fills
 *    in the even harmonics a single-end clarinet lacks => a fuller, sax-like tone.
 *    (STK Saxofony's "faux conical bore"; not the exact conical scattering
 *    junction, but a validated all-harmonic reed model.)
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
  SedaiOscillator, SedaiFormantBody;

type
  TReedBoreType = (rbCylindrical, rbConical);

  // A fractional-delay line (linear interpolation).
  TReedDelay = record
    buf: array of Single;
    len: Integer;
    wr: Integer;
    delay: Single;
    last: Single;      // value returned by the previous tick (STK lastOut())
  end;

  { TSedaiReedGenerator }
  TSedaiReedGenerator = class(TSedaiSignalGenerator)
  private
    FD0, FD1: TReedDelay;      // bore delay lines (FD1 used only for the cone)
    FFilterX1: Single;         // one-zero reflection-filter memory (bell)
    FReflCoeff: Single;        // reflection gain (~ -0.95, inverting + loss)

    FReedOffset: Single;       // ~0.7
    FReedSlope: Single;        // ~-0.44 (negative)

    FBoreType: TReedBoreType;
    FBlowPosition: Single;     // cone: reed position along the bore (0..1)

    FMaxPressure: Single;
    FPressure: Single;
    FPressCoeff: Single;
    FAttackTime: Single;       // breath ramp time constant (s), rise + release
    FVelGain: Single;          // velocity -> output loudness (reed self-osc level
                               // is ~pressure-set, so velocity scales the output)
    FToneState: Single;        // dynamic-brightness one-pole low-pass state
    FToneCoeff: Single;        // one-pole coeff (velocity -> cutoff): soft=darker
    FBrightness: Single;       // 0..1 depth of the velocity->tone effect
    FBody: TSedaiFormantBody;  // instrument body colour (sax/clarinet formants)
    FNoiseGain: Single;
    FVibratoGain: Single;
    FVibratoRate: Single;
    FVibratoPhase: Single;
    FRng: Cardinal;

    FOutputGain: Single;

    FNote: Integer;
    FVelocity: Single;
    FGateOpen: Boolean;
    FReleasing: Boolean;

  protected
    // Driving the inherited Frequency property retunes the bore (so the voice's
    // glide / explicit-Hz path repitches the reed like the other generators).
    procedure SetFrequency(AValue: Single); override;

  private
    procedure UpdateBoreDelay;
    procedure RecalcPressCoeff;
    procedure UpdateToneCoeff;
    procedure ClearDelays;
    function NoiseSample: Single;
    function DelayTick(var AD: TReedDelay; AInput: Single): Single;
    function BreathNow: Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SampleRateChanged; override;

    procedure NoteOn(ANote: Integer; AVelocity: Single);
    procedure NoteOff;
    procedure Kill;

    function GenerateSample: Single; override;

    procedure SetReed(AOffset, ASlope: Single);
    procedure SetBreath(APressure, ANoise, AVibDepth, AVibRateHz: Single);
    // Breath ramp time (s): a slow blow-in (soft attack) vs a quick chiff. Governs
    // both the rise on note-on and the fall on note-off. Default ~0.02 s.
    procedure SetAttack(ASeconds: Single);
    procedure SetReflection(ACoeffMag: Single);
    // Dynamic brightness: how much the played velocity opens the tone (0 = off /
    // flat, 1 = soft notes darkened by a ~1.8 kHz low-pass, loud notes fully open).
    procedure SetBrightness(ADepth: Single);
    // Instrument body colour (formant filter) on the reed output: sax/clarinet/none.
    procedure SetBody(AKind: TFormantBodyKind; AMix: Single);
    // Bore geometry. rbCylindrical (odd harmonics) or rbConical (faux cone; the
    // ABlowPosition 0..1 = the reed's position along the bore, which fills in the
    // even harmonics; ~0.2 is sax-like, avoid 0 and 0.5).
    procedure SetBoreType(AType: TReedBoreType; ABlowPosition: Single);
    procedure SetOutputGain(AGain: Single);

    property Note: Integer read FNote;
    property Velocity: Single read FVelocity;
    property GateOpen: Boolean read FGateOpen;
    property Releasing: Boolean read FReleasing;
  end;

implementation

const
  REED_MIN_FREQ = 40.0;
  // Loop period = 2*delay + ~3 samples (one-zero filter, reed junction, interp).
  // FUDGE ~= 1.5 makes the self-oscillation land on the note (measured within a
  // cent). Same constant works for the cone (total = SR/f - FUDGE).
  REED_DELAY_FUDGE = 1.5;
  // The two-delay-line cone loop carries ~2.6 samples of extra latency (junction
  // + both lastOut stages) vs the single clarinet line; tuned so the sax lands on
  // the note (measured C4 within ~1 cent at the sax operating point).
  REED_CONE_FUDGE = 2.6;
  REED_ATTACK_S = 0.02;

{ TSedaiReedGenerator }

constructor TSedaiReedGenerator.Create;
begin
  inherited Create;
  FReflCoeff := -0.95;
  FReedOffset := 0.7;
  FReedSlope := -0.44;
  FBoreType := rbCylindrical;
  FBlowPosition := 0.2;
  FMaxPressure := 0.55;
  FAttackTime := REED_ATTACK_S;
  FVelGain := 1.0;
  FBrightness := 0.0;
  FToneState := 0.0;
  FToneCoeff := 0.0;
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
  FBody := TSedaiFormantBody.Create;
  SampleRateChanged;
end;

destructor TSedaiReedGenerator.Destroy;
begin
  FBody.Free;
  SetLength(FD0.buf, 0);
  SetLength(FD1.buf, 0);
  inherited Destroy;
end;

procedure TSedaiReedGenerator.SampleRateChanged;
var
  need: Integer;
begin
  inherited SampleRateChanged;
  if FSampleRate <= 0 then Exit;
  // longest full-period loop (lowest note) + interp margin
  need := Ceil(FSampleRate / REED_MIN_FREQ) + 4;
  if Length(FD0.buf) < need then SetLength(FD0.buf, need);
  if Length(FD1.buf) < need then SetLength(FD1.buf, need);
  FD0.len := Length(FD0.buf);
  FD1.len := Length(FD1.buf);
  RecalcPressCoeff;
  UpdateBoreDelay;
  if Assigned(FBody) then FBody.SetSampleRate(FSampleRate);
end;

procedure TSedaiReedGenerator.SetFrequency(AValue: Single);
begin
  inherited SetFrequency(AValue);
  UpdateBoreDelay;
end;

// One-pole low-pass coeff from velocity: cutoff opens with velocity, scaled by
// FBrightness. cutoff = 16 kHz - FBrightness*(1-vel)*(16k-1.8k). a = exp(-2*pi*fc/SR).
procedure TSedaiReedGenerator.UpdateToneCoeff;
var fc: Single;
begin
  if (FSampleRate <= 0) or (FBrightness <= 0) then begin FToneCoeff := 0; Exit; end;
  fc := 16000 - FBrightness * (1 - FVelocity) * (16000 - 1800);
  if fc < 200 then fc := 200;
  if fc > FSampleRate * 0.49 then fc := FSampleRate * 0.49;
  FToneCoeff := Exp(-2 * Pi * fc / FSampleRate);
end;

procedure TSedaiReedGenerator.RecalcPressCoeff;
begin
  if (FSampleRate > 0) and (FAttackTime > 0) then
    FPressCoeff := Exp(-1.0 / (FAttackTime * FSampleRate))
  else
    FPressCoeff := 0.0;
end;

procedure TSedaiReedGenerator.UpdateBoreDelay;
var
  total, d: Single;
begin
  if (FFrequency <= 0) or (FSampleRate <= 0) then Exit;
  if FBoreType = rbConical then
  begin
    // full-period loop split by the blow position between the two delay lines
    total := (FSampleRate / FFrequency) - REED_CONE_FUDGE;
    if total < 2 then total := 2;
    FD0.delay := (1 - FBlowPosition) * total;
    FD1.delay := FBlowPosition * total;
    if FD0.delay < 1 then FD0.delay := 1;
    if FD1.delay < 1 then FD1.delay := 1;
    if FD0.delay > FD0.len - 2 then FD0.delay := FD0.len - 2;
    if FD1.delay > FD1.len - 2 then FD1.delay := FD1.len - 2;
  end
  else
  begin
    // half-period, single line (clarinet)
    d := (FSampleRate / FFrequency) * 0.5 - REED_DELAY_FUDGE;
    if d < 1 then d := 1;
    if d > FD0.len - 2 then d := FD0.len - 2;
    FD0.delay := d;
    FD1.delay := 1;
  end;
end;

procedure TSedaiReedGenerator.ClearDelays;
var i: Integer;
begin
  for i := 0 to FD0.len - 1 do FD0.buf[i] := 0;
  for i := 0 to FD1.len - 1 do FD1.buf[i] := 0;
  FD0.wr := 0; FD1.wr := 0;
  FD0.last := 0; FD1.last := 0;
  FFilterX1 := 0;
  FToneState := 0;
  if Assigned(FBody) then FBody.Reset;
end;

function TSedaiReedGenerator.NoiseSample: Single;
begin
  FRng := FRng xor (FRng shl 13);
  FRng := FRng xor (FRng shr 17);
  FRng := FRng xor (FRng shl 5);
  Result := (FRng / 2147483648.0) - 1.0;
end;

// Write AInput, return + store the fractionally-delayed sample (linear interp).
function TSedaiReedGenerator.DelayTick(var AD: TReedDelay; AInput: Single): Single;
var
  readPos, frac: Single;
  i0, i1: Integer;
begin
  AD.buf[AD.wr] := AInput;
  readPos := AD.wr - AD.delay;
  while readPos < 0 do readPos := readPos + AD.len;
  i0 := Trunc(readPos);
  frac := readPos - i0;
  i1 := i0 + 1; if i1 >= AD.len then i1 := i1 - AD.len;
  Result := AD.buf[i0] * (1 - frac) + AD.buf[i1] * frac;
  Inc(AD.wr); if AD.wr >= AD.len then AD.wr := 0;
  AD.last := Result;
end;

function TSedaiReedGenerator.BreathNow: Single;
var target, b: Single;
begin
  // Breath pressure is held at the (robust, well-above-threshold) configured
  // value so every velocity oscillates cleanly; velocity controls LOUDNESS via
  // FVelGain on the output (the reed self-osc level is ~constant above threshold,
  // so scaling pressure near the edge would just make soft notes drop out).
  if FGateOpen then target := FMaxPressure
  else target := 0;
  FPressure := target + (FPressure - target) * FPressCoeff;
  b := FPressure;
  if FNoiseGain > 0 then b := b * (1 + FNoiseGain * NoiseSample);
  if FVibratoGain > 0 then
  begin
    b := b * (1 + FVibratoGain * Sin(2 * Pi * FVibratoPhase));
    FVibratoPhase := FVibratoPhase + FVibratoRate / FSampleRate;
    if FVibratoPhase >= 1 then FVibratoPhase := FVibratoPhase - 1;
  end;
  Result := b;
end;

procedure TSedaiReedGenerator.NoteOn(ANote: Integer; AVelocity: Single);
begin
  FNote := ANote;
  FVelocity := EnsureRange(AVelocity, 0, 1);
  FVelGain := 0.3 + 0.7 * FVelocity;   // velocity -> output loudness
  // velocity -> brightness: soft notes get a lower low-pass cutoff (darker),
  // loud notes open up. cutoff 1.8 kHz (pp) .. ~14 kHz (ff); FBrightness scales
  // the effect (0 = off, flat response).
  UpdateToneCoeff;
  FGateOpen := True;
  FReleasing := False;
  FFrequency := 440 * Power(2, (ANote - 69) / 12);
  UpdateBoreDelay;
  ClearDelays;
  FPressure := 0;
  FVibratoPhase := 0;
end;

procedure TSedaiReedGenerator.NoteOff;
begin
  if not FGateOpen then Exit;
  FGateOpen := False;
  FReleasing := True;
end;

procedure TSedaiReedGenerator.Kill;
begin
  FGateOpen := False;
  FReleasing := False;
  FNote := -1;
  FPressure := 0;
  ClearDelays;
end;

function TSedaiReedGenerator.GenerateSample: Single;
var
  breath, boreOut, reflected, pressDiff, reedRefl, temp, junction: Single;
begin
  if (not FGateOpen) and (not FReleasing) then
  begin
    Result := 0;
    Exit;
  end;

  breath := BreathNow;

  // note finished once breath + bore have decayed on release
  if FReleasing and (FPressure < 1e-4) and
     (Abs(FD0.last) < 1e-4) and (Abs(FD1.last) < 1e-4) then
  begin
    FReleasing := False;
    Result := 0;
    Exit;
  end;

  if FBoreType = rbConical then
  begin
    // STK Saxofony: reed injected between two delay lines (faux cone).
    temp := FReflCoeff * 0.5 * (FD0.last + FFilterX1);   // bell reflection (LP, inverting)
    FFilterX1 := FD0.last;
    junction := temp - FD1.last;                         // pressure at the blow point
    pressDiff := breath - junction;
    reedRefl := FReedOffset + FReedSlope * pressDiff;
    if reedRefl > 1.0 then reedRefl := 1.0
    else if reedRefl < -1.0 then reedRefl := -1.0;
    DelayTick(FD1, temp);
    DelayTick(FD0, breath - pressDiff * reedRefl - temp);
    Result := FOutputGain * FBody.ProcessSample(junction) * FAmplitude * FVelGain;
  end
  else
  begin
    // Clarinet: single delay line, reed at the closed end.
    boreOut := FD0.last;
    reflected := FReflCoeff * 0.5 * (boreOut + FFilterX1);
    FFilterX1 := boreOut;
    pressDiff := reflected - breath;
    reedRefl := FReedOffset + FReedSlope * pressDiff;
    if reedRefl > 1.0 then reedRefl := 1.0
    else if reedRefl < -1.0 then reedRefl := -1.0;
    DelayTick(FD0, breath + pressDiff * reedRefl);
    Result := FOutputGain * FBody.ProcessSample(boreOut) * FAmplitude * FVelGain;
  end;

  // dynamic-brightness one-pole low-pass (velocity-opened tone; off when depth 0)
  if FToneCoeff > 0 then
  begin
    FToneState := (1 - FToneCoeff) * Result + FToneCoeff * FToneState;
    Result := FToneState;
  end;
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

procedure TSedaiReedGenerator.SetAttack(ASeconds: Single);
begin
  FAttackTime := Max(0.0, ASeconds);
  RecalcPressCoeff;
end;

procedure TSedaiReedGenerator.SetReflection(ACoeffMag: Single);
begin
  ACoeffMag := EnsureRange(ACoeffMag, 0.5, 0.999);
  FReflCoeff := -ACoeffMag;
end;

procedure TSedaiReedGenerator.SetBrightness(ADepth: Single);
begin
  FBrightness := EnsureRange(ADepth, 0, 1);
  UpdateToneCoeff;
end;

procedure TSedaiReedGenerator.SetBody(AKind: TFormantBodyKind; AMix: Single);
begin
  FBody.SetBody(AKind);
  FBody.SetMix(AMix);
end;

procedure TSedaiReedGenerator.SetBoreType(AType: TReedBoreType; ABlowPosition: Single);
begin
  FBoreType := AType;
  FBlowPosition := EnsureRange(ABlowPosition, 0.05, 0.95);
  UpdateBoreDelay;
end;

procedure TSedaiReedGenerator.SetOutputGain(AGain: Single);
begin
  FOutputGain := Max(0, AGain);
end;

end.
