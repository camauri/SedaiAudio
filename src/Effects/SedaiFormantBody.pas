{*
 * Sedai Audio Foundation - Formant Body (instrument body / radiation colour)
 *
 * TSedaiFormantBody is a per-voice modal body filter: a parallel bank of resonant
 * band-pass biquads tuned to an instrument's signature body modes. The physical
 * exciter+resonator generators (bowed string, reed) produce the raw source signal
 * (a Helmholtz sawtooth, a reed tone); on their own they sound synthetic because a
 * real instrument's character lives largely in its RESONATING BODY. This filter
 * lends that body — the violin's A0 air mode / B1 wood twins / bridge hill, the
 * cello's scaled equivalents, etc. (modal radiation; research doc section 2b).
 *
 * It is a mono, per-voice component (not a stereo TSedaiEffect insert): the body
 * is part of the instrument, so it lives in the voice next to the generator.
 *
 * A few biquads are an APPROXIMATION of a real body's many modes / measured IR —
 * a solid step up from the raw string, not a substitute for a captured body IR
 * (which can be convolved via SedaiConvolver when authenticity matters).
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiFormantBody;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  TFormantBodyKind = (fbNone, fbViolin, fbViola, fbCello);

  { TSedaiFormantBody }
  TSedaiFormantBody = class
  private
    FSampleRate: Single;
    FKind: TFormantBodyKind;
    FMix: Single;                 // dry..wet (0 = bypass)
    FModes: array of record f, q, gain: Single; end;
    FBq: array of record
      b0, b1, b2, a1, a2, x1, x2, y1, y2: Single;
    end;
    procedure BuildModes;
    procedure RebuildFilters;
  public
    constructor Create;
    procedure SetSampleRate(AValue: Single);
    procedure SetBody(AKind: TFormantBodyKind);
    procedure SetMix(AMix: Single);
    procedure Reset;
    function ProcessSample(AIn: Single): Single;

    property Kind: TFormantBodyKind read FKind;
    property Mix: Single read FMix;
  end;

implementation

constructor TSedaiFormantBody.Create;
begin
  inherited Create;
  FSampleRate := 48000;
  FKind := fbNone;
  FMix := 0.0;
  BuildModes;
end;

procedure TSedaiFormantBody.SetSampleRate(AValue: Single);
begin
  if AValue <= 0 then Exit;
  FSampleRate := AValue;
  RebuildFilters;
end;

// Signature body modes (freq Hz, Q, gain). Q kept moderate so the body colours
// the tone without ringing metallically after the note stops.
procedure TSedaiFormantBody.BuildModes;
  procedure M(idx: Integer; f, q, g: Single);
  begin FModes[idx].f := f; FModes[idx].q := q; FModes[idx].gain := g; end;
begin
  case FKind of
    fbViolin:
      begin
        SetLength(FModes, 7);
        M(0, 280, 5.0, 1.00);   // A0 air
        M(1, 460, 6.5, 0.90);   // B1-
        M(2, 540, 6.5, 0.85);   // B1+
        M(3, 700, 5.0, 0.60);
        M(4, 1000, 3.5, 0.50);
        M(5, 2500, 2.2, 0.80);  // bridge hill (broad)
        M(6, 3500, 2.5, 0.35);
      end;
    fbViola:
      begin
        SetLength(FModes, 7);
        M(0, 220, 5.0, 1.00);
        M(1, 350, 6.5, 0.90);
        M(2, 420, 6.5, 0.85);
        M(3, 600, 5.0, 0.60);
        M(4, 900, 3.5, 0.50);
        M(5, 2000, 2.2, 0.80);
        M(6, 3000, 2.5, 0.35);
      end;
    fbCello:
      begin
        SetLength(FModes, 7);
        M(0, 105, 4.5, 1.00);   // air
        M(1, 175, 6.0, 0.90);
        M(2, 220, 6.0, 0.80);
        M(3, 330, 4.5, 0.60);
        M(4, 500, 3.5, 0.50);
        M(5, 1200, 2.2, 0.65);  // broad
        M(6, 2000, 2.5, 0.30);
      end;
  else
    SetLength(FModes, 0);
  end;
  RebuildFilters;
end;

// RBJ constant-0dB band-pass per mode.
procedure TSedaiFormantBody.RebuildFilters;
var i: Integer; w0, cw, sw, alpha, a0: Single;
begin
  SetLength(FBq, Length(FModes));
  for i := 0 to High(FModes) do
  begin
    if (FModes[i].f <= 0) or (FModes[i].f >= FSampleRate * 0.49) then
    begin
      FBq[i].b0 := 0; FBq[i].b1 := 0; FBq[i].b2 := 0; FBq[i].a1 := 0; FBq[i].a2 := 0;
      Continue;
    end;
    w0 := 2 * Pi * FModes[i].f / FSampleRate; cw := Cos(w0); sw := Sin(w0);
    alpha := sw / (2 * FModes[i].q); a0 := 1 + alpha;
    FBq[i].b0 := alpha / a0; FBq[i].b1 := 0; FBq[i].b2 := -alpha / a0;
    FBq[i].a1 := (-2 * cw) / a0; FBq[i].a2 := (1 - alpha) / a0;
  end;
  Reset;
end;

procedure TSedaiFormantBody.SetBody(AKind: TFormantBodyKind);
begin
  if FKind = AKind then Exit;
  FKind := AKind;
  BuildModes;
end;

procedure TSedaiFormantBody.SetMix(AMix: Single);
begin
  FMix := EnsureRange(AMix, 0, 1);
end;

procedure TSedaiFormantBody.Reset;
var i: Integer;
begin
  for i := 0 to High(FBq) do
  begin
    FBq[i].x1 := 0; FBq[i].x2 := 0; FBq[i].y1 := 0; FBq[i].y2 := 0;
  end;
end;

// out = dry*(1-mix) + mix * mean(gain_i * bandpass_i(in))
function TSedaiFormantBody.ProcessSample(AIn: Single): Single;
var i: Integer; body, y: Single;
begin
  if (FKind = fbNone) or (FMix <= 0) or (Length(FBq) = 0) then Exit(AIn);
  body := 0;
  for i := 0 to High(FBq) do
  begin
    y := FBq[i].b0*AIn + FBq[i].b1*FBq[i].x1 + FBq[i].b2*FBq[i].x2
         - FBq[i].a1*FBq[i].y1 - FBq[i].a2*FBq[i].y2;
    FBq[i].x2 := FBq[i].x1; FBq[i].x1 := AIn; FBq[i].y2 := FBq[i].y1; FBq[i].y1 := y;
    body := body + FModes[i].gain * y;
  end;
  // Only ~1-2 modes are active at any frequency, so normalise by ~sqrt(N), not N
  // (dividing by N over-attenuates and the body gets inaudibly quiet).
  body := body / Max(2.0, Sqrt(Length(FBq)));
  Result := (1 - FMix) * AIn + FMix * body;
end;

end.
