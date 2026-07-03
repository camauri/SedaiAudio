{*
 * Sedai Audio Foundation - Body Resonator ("il tubo": body/radiation modelling)
 *
 * TSedaiBodyResonator adds the instrument-specific BODY / RADIATION character that
 * pure additive partials lack. Session 13 established that the on-axis spectrum is
 * already matched by the partials, so a mono body IR that re-EQs would be
 * redundant. What the partials are genuinely missing is (i) the spatial radiation
 * (directivity -> interaural decorrelation -> width) and (ii) the body's resonant
 * ring. This effect supplies both while leaving the on-axis (mono) spectrum
 * UNTOUCHED, by construction.
 *
 * Method: it is the auto-space (Lauridsen) structure with the generic decorrelating
 * allpass REPLACED by the instrument's signature modal bank:
 *   mid = 0.5*(L+R)
 *   d   = FNorm * Sum_m  Gain_m * bandpass_m(mid)     (the body-coloured, ringing
 *                                                       decorrelation signal)
 *   Lout = mid + Width*d,  Rout = mid - Width*d        (the body lives in the SIDE)
 * Because the body's contribution is pure side, the mono sum stays Lout+Rout = 2*mid
 * = L+R -> the on-axis magnitude is EXACTLY the dry partials (perfect
 * complementarity, guaranteed -- not an approximation). The width is coloured by the
 * body's resonances and RINGS on transients (high-Q modes), giving instrument-
 * specific spatial character rather than the generic width of a plain allpass.
 *
 * Family-specific (the physics differs): resonant-body strings (violin/guitar) get a
 * modal bank (A0/B1/bridge-hill; top-plate + Helmholtz); winds (sax/bell) have no
 * resonant body -- "the tube" is the bore (already in the partials) + the bell's
 * frequency-dependent radiation (HF beams -> spaced mics decorrelate HF more), so
 * their "body" is a set of HF-weighted modes = HF-weighted width.
 *
 * Belongs to the MIX (a per-Part insert, applied once to the Part sum -- the
 * commuted-synthesis efficiency win on an LTI additive output), NOT the .safinst.
 * Note: models the body as coloured stereo RADIATION; the on-axis body ring is
 * absent by design (it is already carried by the partials' amplitude tracks). A v2
 * could add a mid (mono) resonant path and/or a measured SOFA-derived IR.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiBodyResonator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiEffect;

const
  MAX_BODY_MODES = 12;

type
  { instrument family whose signature modes model "the body/tube" }
  TBodyKind = (bodyNone, bodySax, bodyViolin, bodyGuitar);

  { one signature mode = an RBJ band-pass resonator (Freq, Q) at level Gain }
  TBodyMode = record
    Freq, Q, Gain: Single;
    b0, b1, b2, a1, a2: Single;   // RBJ band-pass coeffs (a0-normalized)
    x1, x2, y1, y2: Single;       // biquad state (Direct Form I)
  end;

  { TSedaiBodyResonator }
  // Instrument-specific body/radiation: a modal decorrelator -> coloured, ringing
  // stereo width, mono-safe (on-axis spectrum untouched by construction).
  TSedaiBodyResonator = class(TSedaiEffect)
  private
    FModes: array[0..MAX_BODY_MODES-1] of TBodyMode;
    FModeCount: Integer;
    FKind: TBodyKind;
    FWidth: Single;   // 0..1 side amount (radiation width)
    FMix: Single;     // 0..1 dry(input)->processed crossfade
    FNorm: Single;    // level normalization for the modal sum

    procedure SetWidth(AValue: Single);
    procedure RecalcCoeffs;
    procedure UpdateNorm;
    procedure ResetStates;
    function ProcessMode(var AM: TBodyMode; AIn: Single): Single;

  protected
    procedure SampleRateChanged; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Load a family's signature modes (literature values). bodyNone = passthrough.
    procedure LoadBody(AKind: TBodyKind);
    // Manual authoring: set a mode (Freq Hz, Q, Gain) and the active count.
    procedure SetModeCount(ACount: Integer);
    procedure SetMode(AIndex: Integer; AFreq, AQ, AGain: Single);

    property Kind: TBodyKind read FKind;
    property ModeCount: Integer read FModeCount;
    property Width: Single read FWidth write SetWidth;
    property Mix: Single read FMix write FMix;
  end;

implementation

{ TSedaiBodyResonator }

constructor TSedaiBodyResonator.Create;
begin
  inherited Create;
  FModeCount := 0;
  FKind := bodyNone;
  FWidth := 0.5;
  FMix := 1.0;
  FNorm := 1.0;
  ResetStates;
end;

destructor TSedaiBodyResonator.Destroy;
begin
  inherited Destroy;
end;

procedure TSedaiBodyResonator.SetWidth(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0
  else if AValue > 1.0 then AValue := 1.0;
  FWidth := AValue;
end;

procedure TSedaiBodyResonator.SetModeCount(ACount: Integer);
begin
  if ACount < 0 then ACount := 0
  else if ACount > MAX_BODY_MODES then ACount := MAX_BODY_MODES;
  FModeCount := ACount;
  RecalcCoeffs;
  UpdateNorm;
end;

procedure TSedaiBodyResonator.SetMode(AIndex: Integer; AFreq, AQ, AGain: Single);
begin
  if (AIndex < 0) or (AIndex >= MAX_BODY_MODES) then Exit;
  FModes[AIndex].Freq := AFreq;
  FModes[AIndex].Q := AQ;
  FModes[AIndex].Gain := AGain;
  RecalcCoeffs;
  UpdateNorm;
end;

// Signature modes per family (literature-derived: freq Hz, Q, gain). Resonant-body
// strings get their real modes; winds get HF-weighted "radiation" modes.
procedure TSedaiBodyResonator.LoadBody(AKind: TBodyKind);

  procedure M(i: Integer; f, q, g: Single);
  begin
    FModes[i].Freq := f; FModes[i].Q := q; FModes[i].Gain := g;
  end;

begin
  FKind := AKind;
  case AKind of
    bodySax:
      begin
        // No resonant body: the bell radiates HF directionally -> HF-weighted width.
        M(0, 700,  1.5, 0.25);
        M(1, 1500, 2.0, 0.45);
        M(2, 3000, 2.5, 0.70);
        M(3, 5500, 2.5, 0.60);
        FModeCount := 4;
      end;
    bodyViolin:
      begin
        // A0 (air/Helmholtz), B1-/B1+ (wood twins), bridge hill (broad ~2-3 kHz).
        M(0, 280,  8.0, 0.50);
        M(1, 460,  7.0, 0.40);
        M(2, 540,  7.0, 0.45);
        M(3, 2500, 1.5, 0.60);
        FModeCount := 4;
      end;
    bodyGuitar:
      begin
        // Helmholtz air + top-plate T(1,1)1/T(1,1)2 + a broad upper body region.
        M(0, 100,  6.0, 0.50);
        M(1, 200,  8.0, 0.60);
        M(2, 400,  6.0, 0.40);
        M(3, 2700, 1.5, 0.45);
        FModeCount := 4;
      end;
    else
      FModeCount := 0;   // bodyNone -> passthrough
  end;
  RecalcCoeffs;
  UpdateNorm;
  ResetStates;
end;

// RBJ band-pass (constant 0 dB peak gain) coefficients for each active mode.
procedure TSedaiBodyResonator.RecalcCoeffs;
var
  i: Integer;
  w0, cw, sw, alpha, a0: Single;
begin
  for i := 0 to FModeCount - 1 do
  begin
    if (FSampleRate > 0) and (FModes[i].Freq > 0) and (FModes[i].Freq < FSampleRate * 0.45)
       and (FModes[i].Q > 0) then
    begin
      w0 := 2.0 * Pi * FModes[i].Freq / FSampleRate;
      cw := Cos(w0); sw := Sin(w0);
      alpha := sw / (2.0 * FModes[i].Q);
      a0 := 1.0 + alpha;
      FModes[i].b0 :=  alpha / a0;
      FModes[i].b1 :=  0.0;
      FModes[i].b2 := -alpha / a0;
      FModes[i].a1 := (-2.0 * cw) / a0;
      FModes[i].a2 := (1.0 - alpha) / a0;
    end
    else
    begin
      FModes[i].b0 := 0; FModes[i].b1 := 0; FModes[i].b2 := 0;
      FModes[i].a1 := 0; FModes[i].a2 := 0;
    end;
  end;
end;

// Level normalization: a BPF has ~unit peak gain, so the modal sum peaks near
// Sum|Gain|. Divide by that (>=1) so the side signal stays bounded and Width is a
// meaningful 0..1 knob.
procedure TSedaiBodyResonator.UpdateNorm;
var
  i: Integer;
  s: Single;
begin
  s := 0;
  for i := 0 to FModeCount - 1 do s := s + Abs(FModes[i].Gain);
  if s < 1.0 then s := 1.0;
  FNorm := 1.0 / s;
end;

procedure TSedaiBodyResonator.ResetStates;
var
  i: Integer;
begin
  for i := 0 to MAX_BODY_MODES - 1 do
  begin
    FModes[i].x1 := 0; FModes[i].x2 := 0;
    FModes[i].y1 := 0; FModes[i].y2 := 0;
  end;
end;

procedure TSedaiBodyResonator.SampleRateChanged;
begin
  inherited SampleRateChanged;
  RecalcCoeffs;
end;

procedure TSedaiBodyResonator.Reset;
begin
  inherited Reset;
  ResetStates;
end;

function TSedaiBodyResonator.ProcessMode(var AM: TBodyMode; AIn: Single): Single;
begin
  Result := AM.b0 * AIn + AM.b1 * AM.x1 + AM.b2 * AM.x2 - AM.a1 * AM.y1 - AM.a2 * AM.y2;
  AM.x2 := AM.x1; AM.x1 := AIn;
  AM.y2 := AM.y1; AM.y1 := Result;
end;

procedure TSedaiBodyResonator.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  i, m: Integer;
  L, R, mid, d, pL, pR: Single;
begin
  for i := 0 to AFrameCount - 1 do
  begin
    L := AInput[i * 2];
    R := AInput[i * 2 + 1];
    mid := 0.5 * (L + R);

    // Body-coloured, ringing decorrelation signal from the modal bank.
    d := 0;
    for m := 0 to FModeCount - 1 do
      d := d + FModes[m].Gain * ProcessMode(FModes[m], mid);
    d := d * FNorm;

    // Lauridsen: the body lives in the side -> mono-safe (on-axis untouched).
    pL := mid + FWidth * d;
    pR := mid - FWidth * d;
    AOutput[i * 2]     := L * (1.0 - FMix) + pL * FMix;
    AOutput[i * 2 + 1] := R * (1.0 - FMix) + pR * FMix;
  end;
end;

end.
