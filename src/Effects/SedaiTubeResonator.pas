{*
 * Sedai Audio Foundation - Tube Resonator (commuted "il tubo" body filter)
 *
 * TSedaiTubeResonator is a note-tuned waveguide-comb resonator that gives a
 * spectrally-faithful-but-bodyless source (e.g. the free-partial engine's
 * sum-of-sines output) the resonant "corpo" a real instrument bore radiates.
 * This is commuted synthesis (J.O. Smith; SAF research doc section 2b/6C):
 * partials = excitation, the tube = the body filter. Applying the tube to an
 * LTI additive/partial output is exact (no reed-loop approximation).
 *
 * It is a per-voice component (tuned to the played note), NOT a TSedaiEffect
 * insert: a polyphonic Part plays many notes, so the resonator lives in the
 * voice and is retuned per note-on.
 *
 * Modes:
 *   tmFull (all harmonics): full-period delay + non-inverting reflection ->
 *     comb peaks at k*f0. Adds the full-bodied resonance the ear preferred.
 *   tmOdd  (clarinet-like): half-period delay + inverting reflection -> peaks at
 *     (2k-1)f0. A hollow, "wooden tube" body.
 *
 * Resonance (0..0.98) = how much body/ring; Mix (0..1) = dry..wet.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiTubeResonator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TTubeMode = (tmFull, tmOdd);

  { TSedaiTubeResonator }
  TSedaiTubeResonator = class
  private
    FSampleRate: Single;
    FBuf: array of Single;
    FLen: Integer;
    FWr: Integer;
    FDelay: Single;
    FFrequency: Single;
    FMode: TTubeMode;
    FResonance: Single;   // feedback gain g (0..0.98)
    FMix: Single;         // dry..wet (0..1)
    FReflSign: Single;
    FX1: Single;          // one-zero reflection-filter memory
    procedure UpdateDelay;
  public
    constructor Create;
    procedure SetSampleRate(AValue: Single);
    procedure SetFrequency(AHz: Single);
    procedure SetMode(AMode: TTubeMode);
    procedure SetResonance(AG: Single);
    procedure SetMix(AMix: Single);
    procedure Reset;
    function ProcessSample(AIn: Single): Single;

    property Frequency: Single read FFrequency;
    property Mode: TTubeMode read FMode;
    property Resonance: Single read FResonance;
    property Mix: Single read FMix;
  end;

implementation

const
  TUBE_MIN_FREQ = 40.0;
  TUBE_MAX_G = 0.98;

constructor TSedaiTubeResonator.Create;
begin
  inherited Create;
  FSampleRate := 48000;
  FFrequency := 261.63;
  FMode := tmFull;
  FResonance := 0.0;      // 0 = inert (pass-through with mix, no ring)
  FMix := 0.0;
  FReflSign := 1.0;
  FX1 := 0;
  SetSampleRate(48000);
end;

procedure TSedaiTubeResonator.SetSampleRate(AValue: Single);
var need: Integer;
begin
  if AValue <= 0 then Exit;
  FSampleRate := AValue;
  need := Ceil(FSampleRate / TUBE_MIN_FREQ) + 8;   // longest full-period + margin
  if Length(FBuf) < need then SetLength(FBuf, need);
  FLen := Length(FBuf);
  UpdateDelay;
  Reset;
end;

// tmFull: full period, non-inverting -> all harmonics. tmOdd: half period,
// inverting -> odd harmonics (hollow). -1 fudge for the one-zero filter delay.
procedure TSedaiTubeResonator.UpdateDelay;
var d: Single;
begin
  if (FFrequency <= 0) or (FSampleRate <= 0) or (FLen < 4) then Exit;
  if FMode = tmOdd then begin d := FSampleRate / FFrequency * 0.5 - 1.0; FReflSign := -1.0; end
  else begin d := FSampleRate / FFrequency - 1.0; FReflSign := 1.0; end;
  if d < 1 then d := 1;
  if d > FLen - 2 then d := FLen - 2;
  FDelay := d;
end;

procedure TSedaiTubeResonator.SetFrequency(AHz: Single);
begin
  if AHz > 0 then begin FFrequency := AHz; UpdateDelay; end;
end;

procedure TSedaiTubeResonator.SetMode(AMode: TTubeMode);
begin
  FMode := AMode;
  UpdateDelay;
end;

procedure TSedaiTubeResonator.SetResonance(AG: Single);
begin
  FResonance := EnsureRange(AG, 0, TUBE_MAX_G);
end;

procedure TSedaiTubeResonator.SetMix(AMix: Single);
begin
  FMix := EnsureRange(AMix, 0, 1);
end;

procedure TSedaiTubeResonator.Reset;
var i: Integer;
begin
  for i := 0 to FLen - 1 do FBuf[i] := 0;
  FWr := 0;
  FX1 := 0;
end;

// y = delayed tube sample; reflected = reflSign*g*onezeroLP(y); write in+reflected.
// Output = dry*(1-mix) + wet*mix, wet = the resonating tube (delayed) sample.
function TSedaiTubeResonator.ProcessSample(AIn: Single): Single;
var
  readPos, frac, y, refl: Single;
  i0, i1: Integer;
begin
  if (FResonance <= 0) or (FMix <= 0) then Exit(AIn);   // inert
  readPos := FWr - FDelay;
  while readPos < 0 do readPos := readPos + FLen;
  i0 := Trunc(readPos);
  frac := readPos - i0;
  i1 := i0 + 1; if i1 >= FLen then i1 := i1 - FLen;
  y := FBuf[i0] * (1 - frac) + FBuf[i1] * frac;
  refl := FReflSign * FResonance * 0.5 * (y + FX1);
  FX1 := y;
  FBuf[FWr] := AIn + refl;
  Inc(FWr); if FWr >= FLen then FWr := 0;
  Result := (1 - FMix) * AIn + FMix * y;
end;

end.
