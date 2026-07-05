{*
 * Sedai Audio Foundation - Bowed-String Generator
 *
 * TSedaiBowedGenerator is a PHYSICAL-MODELLING bowed string (violin/cello class):
 * a digital-waveguide string driven by a nonlinear bow-friction junction, in the
 * McIntyre-Schumacher-Woodhouse loop — the string is the resonator, the bow the
 * exciter. Re-derived from the Smith / STK Bowed model (Cook & Scavone); no STK
 * dependency. Like the reed, it SELF-OSCILLATES: the stick-slip friction curve
 * pumps energy into the string, producing Helmholtz (sawtooth-like) motion with a
 * full harmonic series.
 *
 * Loop (per sample):
 *   bowVelocity     = maxVelocity * envelope
 *   bridgeRefl      = -stringFilter( bridgeDelay.lastOut )   // bridge = lossy, inverting
 *   nutRefl         = -neckDelay.lastOut                     // nut = inverting
 *   stringVel       = bridgeRefl + nutRefl                   // string velocity at the bow
 *   deltaV          = bowVelocity - stringVel                // bow-string relative velocity
 *   newVel          = deltaV * bowTable(deltaV)              // stick-slip friction
 *   neckDelay.tick( bridgeRefl + newVel )
 *   bridgeDelay.tick( nutRefl + newVel )
 *   out             = bridgeDelay.lastOut
 *
 * bowTable(x) = clamp((|x*slope + offset| + 0.75)^-4, 0, 1): ~1 when the relative
 * velocity is small (the string sticks to the bow) and falling off as it grows
 * (slip). The string is fixed at both ends (both reflections inverting) -> a full
 * harmonic series. A body/formant colour can be added downstream (SedaiBodyResonator
 * / SedaiTubeResonator); this generator outputs the raw string signal.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiBowedGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode,
  SedaiOscillator, SedaiFormantBody;

type
  TBowDelay = record
    buf: array of Single;
    len, wr: Integer;
    delay: Single;
    last: Single;
  end;

  { TSedaiBowedGenerator }
  TSedaiBowedGenerator = class(TSedaiSignalGenerator)
  private
    FNeck, FBridge: TBowDelay;     // the two string sections either side of the bow
    FStrPole: Single;              // string (bridge) one-pole lowpass coeff
    FStrY1: Single;                // its state
    FLoopGain: Single;             // per-reflection loss (<1) so the string damps
                                   // out when unbowed and stays bounded

    FBowSlope: Single;             // bow-table slope (bow force / grip)
    FBowOffset: Single;

    FBowPosition: Single;          // 0..1 along the string (betaRatio)
    FMaxVelocity: Single;          // steady bow velocity target
    FVelocity: Single;             // current (ramped) bow velocity
    FVelCoeff: Single;             // one-pole ramp toward target
    FAttackTime: Single;

    FVibratoGain, FVibratoRate, FVibratoPhase: Single;

    FBody: TSedaiFormantBody;      // instrument body colour (violin/cello formants)

    FOutputGain: Single;

    FNote: Integer;
    FNoteVel: Single;
    FGateOpen, FReleasing: Boolean;

    procedure UpdateDelays;
    procedure RecalcVelCoeff;
    procedure UpdateStringFilter;
    procedure ClearString;
    function DelayTick(var AD: TBowDelay; AInput: Single): Single;
    function BowTable(AIn: Single): Single;

  protected
    procedure SetFrequency(AValue: Single); override;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SampleRateChanged; override;

    procedure NoteOn(ANote: Integer; AVelocity: Single);
    procedure NoteOff;
    procedure Kill;
    function GenerateSample: Single; override;

    // Bow velocity (loudness/brightness; ~0.15..0.4), bow position along the string
    // (0.05..0.4; avoid 0 and simple fractions), and bow "force"/grip (table slope,
    // ~2..5; higher = grippier/brighter).
    procedure SetBow(AMaxVelocity, APosition, AForce: Single);
    // Instrument body colour: a modal formant filter on the string output. Kind =
    // violin/viola/cello/none; mix = dry..wet (0 = raw string). Without a body the
    // raw Helmholtz sawtooth sounds synthetic.
    procedure SetBody(AKind: TFormantBodyKind; AMix: Single);
    procedure SetVibrato(ADepth, ARateHz: Single);
    procedure SetAttack(ASeconds: Single);
    procedure SetOutputGain(AGain: Single);

    property Note: Integer read FNote;
    property GateOpen: Boolean read FGateOpen;
    property Releasing: Boolean read FReleasing;
  end;

implementation

const
  BOW_MIN_FREQ = 40.0;
  BOW_DELAY_FUDGE = 4.0;     // STK: baseDelay = SR/f - 4
  BOW_ATTACK_S = 0.05;

{ TSedaiBowedGenerator }

constructor TSedaiBowedGenerator.Create;
begin
  inherited Create;
  FBowSlope := 3.0;
  FBowOffset := 0.001;
  FBowPosition := 0.127;
  FMaxVelocity := 0.25;
  FLoopGain := 0.995;
  FAttackTime := BOW_ATTACK_S;
  FVibratoGain := 0.0;
  FVibratoRate := 6.0;
  FVibratoPhase := 0.0;
  FOutputGain := 0.15;
  FNote := -1;
  FNoteVel := 1.0;
  FGateOpen := False;
  FReleasing := False;
  FFrequency := 220.0;
  FAmplitude := 1.0;
  FBody := TSedaiFormantBody.Create;
  SampleRateChanged;
end;

destructor TSedaiBowedGenerator.Destroy;
begin
  FBody.Free;
  SetLength(FNeck.buf, 0);
  SetLength(FBridge.buf, 0);
  inherited Destroy;
end;

procedure TSedaiBowedGenerator.SampleRateChanged;
var need: Integer;
begin
  inherited SampleRateChanged;
  if FSampleRate <= 0 then Exit;
  need := Ceil(FSampleRate / BOW_MIN_FREQ) + 8;
  if Length(FNeck.buf) < need then SetLength(FNeck.buf, need);
  if Length(FBridge.buf) < need then SetLength(FBridge.buf, need);
  FNeck.len := Length(FNeck.buf);
  FBridge.len := Length(FBridge.buf);
  RecalcVelCoeff;
  UpdateStringFilter;
  UpdateDelays;
  if Assigned(FBody) then FBody.SetSampleRate(FSampleRate);
end;

procedure TSedaiBowedGenerator.RecalcVelCoeff;
begin
  if (FSampleRate > 0) and (FAttackTime > 0) then
    FVelCoeff := Exp(-1.0 / (FAttackTime * FSampleRate))
  else
    FVelCoeff := 0.0;
end;

// STK: stringFilter one-pole, pole = 0.75 - 0.2*22050/SR (lowpass loss at the bridge)
procedure TSedaiBowedGenerator.UpdateStringFilter;
begin
  if FSampleRate > 0 then
    FStrPole := 0.75 - (0.2 * 22050.0 / FSampleRate)
  else
    FStrPole := 0.66;
  if FStrPole < 0 then FStrPole := 0 else if FStrPole > 0.98 then FStrPole := 0.98;
end;

procedure TSedaiBowedGenerator.UpdateDelays;
var baseDelay, b, nk: Single;
begin
  if (FFrequency <= 0) or (FSampleRate <= 0) then Exit;
  baseDelay := FSampleRate / FFrequency - BOW_DELAY_FUDGE;
  if baseDelay < 2 then baseDelay := 2;
  b := FBowPosition * baseDelay;          // bridge side
  nk := (1 - FBowPosition) * baseDelay;   // neck side
  if b < 1 then b := 1;
  if nk < 1 then nk := 1;
  if b > FBridge.len - 2 then b := FBridge.len - 2;
  if nk > FNeck.len - 2 then nk := FNeck.len - 2;
  FBridge.delay := b;
  FNeck.delay := nk;
end;

procedure TSedaiBowedGenerator.ClearString;
var i: Integer;
begin
  for i := 0 to FNeck.len - 1 do FNeck.buf[i] := 0;
  for i := 0 to FBridge.len - 1 do FBridge.buf[i] := 0;
  FNeck.wr := 0; FBridge.wr := 0;
  FNeck.last := 0; FBridge.last := 0;
  FStrY1 := 0;
  if Assigned(FBody) then FBody.Reset;
end;

function TSedaiBowedGenerator.DelayTick(var AD: TBowDelay; AInput: Single): Single;
var readPos, frac: Single; i0, i1: Integer;
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

// Stick-slip friction curve, clamped to [0,1].
function TSedaiBowedGenerator.BowTable(AIn: Single): Single;
var s: Single;
begin
  s := Abs(AIn * FBowSlope + FBowOffset) + 0.75;
  Result := Power(s, -4.0);
  if Result > 1.0 then Result := 1.0
  else if Result < 0.0 then Result := 0.0;
end;

procedure TSedaiBowedGenerator.SetFrequency(AValue: Single);
begin
  inherited SetFrequency(AValue);
  UpdateDelays;
end;

procedure TSedaiBowedGenerator.NoteOn(ANote: Integer; AVelocity: Single);
begin
  FNote := ANote;
  FNoteVel := EnsureRange(AVelocity, 0, 1);
  FGateOpen := True;
  FReleasing := False;
  FFrequency := 440 * Power(2, (ANote - 69) / 12);
  UpdateDelays;
  ClearString;
  FVelocity := 0;
  FVibratoPhase := 0;
end;

procedure TSedaiBowedGenerator.NoteOff;
begin
  if not FGateOpen then Exit;
  FGateOpen := False;
  FReleasing := True;
end;

procedure TSedaiBowedGenerator.Kill;
begin
  FGateOpen := False;
  FReleasing := False;
  FNote := -1;
  FVelocity := 0;
  ClearString;
end;

function TSedaiBowedGenerator.GenerateSample: Single;
var
  target, bowVel, bridgeRefl, nutRefl, stringVel, deltaV, newVel, strFilt: Single;
begin
  if (not FGateOpen) and (not FReleasing) then
  begin
    Result := 0;
    Exit;
  end;

  // bow velocity ramp (attack on note, decay on release), scaled by note velocity
  if FGateOpen then target := FMaxVelocity * (0.7 + 0.3 * FNoteVel)
  else target := 0;
  FVelocity := target + (FVelocity - target) * FVelCoeff;

  if FReleasing and (FVelocity < 1e-4) and
     (Abs(FBridge.last) < 1e-4) and (Abs(FNeck.last) < 1e-4) then
  begin
    FReleasing := False;
    Result := 0;
    Exit;
  end;

  bowVel := FVelocity;
  if FVibratoGain > 0 then
  begin
    bowVel := bowVel * (1 + FVibratoGain * Sin(2 * Pi * FVibratoPhase));
    FVibratoPhase := FVibratoPhase + FVibratoRate / FSampleRate;
    if FVibratoPhase >= 1 then FVibratoPhase := FVibratoPhase - 1;
  end;

  // bridge reflection = -lowpass(bridge delay out); nut reflection = -neck delay out
  strFilt := (1 - FStrPole) * FBridge.last + FStrPole * FStrY1;
  FStrY1 := strFilt;
  bridgeRefl := -FLoopGain * strFilt;      // loop loss => string damps when unbowed
  nutRefl := -FLoopGain * FNeck.last;

  stringVel := bridgeRefl + nutRefl;
  deltaV := bowVel - stringVel;
  newVel := deltaV * BowTable(deltaV);

  DelayTick(FNeck, bridgeRefl + newVel);
  DelayTick(FBridge, nutRefl + newVel);

  // colour the raw string with the instrument body (bypass when body = none)
  Result := FOutputGain * FBody.ProcessSample(FBridge.last) * FAmplitude;
end;

procedure TSedaiBowedGenerator.SetBow(AMaxVelocity, APosition, AForce: Single);
begin
  FMaxVelocity := Max(0.01, AMaxVelocity);
  FBowPosition := EnsureRange(APosition, 0.02, 0.5);
  FBowSlope := Max(0.5, AForce);
  UpdateDelays;
end;

procedure TSedaiBowedGenerator.SetBody(AKind: TFormantBodyKind; AMix: Single);
begin
  FBody.SetBody(AKind);
  FBody.SetMix(AMix);
end;

procedure TSedaiBowedGenerator.SetVibrato(ADepth, ARateHz: Single);
begin
  FVibratoGain := Max(0, ADepth);
  if ARateHz > 0 then FVibratoRate := ARateHz;
end;

procedure TSedaiBowedGenerator.SetAttack(ASeconds: Single);
begin
  FAttackTime := Max(0.0, ASeconds);
  RecalcVelCoeff;
end;

procedure TSedaiBowedGenerator.SetOutputGain(AGain: Single);
begin
  FOutputGain := Max(0, AGain);
end;

end.
