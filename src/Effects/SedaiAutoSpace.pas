{*
 * Sedai Audio Foundation - Auto-Space (stereo decorrelation / widener)
 *
 * TSedaiAutoSpace turns a mono / dual-mono source into a decorrelated stereo
 * image WITHOUT a reverb tail. It targets the "boxed-in" character of a dry
 * mono solo instrument: the width of a real recording comes from the SPATIAL
 * radiation of the instrument captured by spaced microphones (decorrelation
 * between the ears), not from a reverb decay. A solo additive voice, whose
 * on-axis spectrum already matches the source, is missing exactly this.
 *
 * Method (Kendall 1995 / Lauridsen decorrelator — mono-safe, no coloration):
 *   M = 0.5*(L+R)                     (mid)
 *   D = allpass-chain(M)              (frequency-dependent phase scramble;
 *                                      an allpass preserves magnitude, so it
 *                                      decorrelates without colouring)
 *   Lout = M + Width*D
 *   Rout = M - Width*D                (the added part is pure SIDE)
 * Because the widening lives entirely in the side, the mono sum stays
 * Lout+Rout = 2*M -> perfectly mono-compatible for ANY Width/Mix (this is the
 * property the earlier independent-per-channel jitter experiment lacked, which
 * measured phasey/anti-phase for a single instrument). Width sets the early
 * interaural cross-correlation (IACC): 0 = mono, 1 = decorrelated = wide.
 *
 * Space belongs to the MIX, not the preset: instantiate this as a mixer-channel
 * or bus insert (AddInsert). It is deliberately NOT serialised into a .safinst.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAutoSpace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiEffect;

const
  SPACE_ALLPASS_COUNT = 3;
  // Base Schroeder-allpass delays (ms) at Size=0.5. Mutually prime-ish so the
  // chain sounds diffuse rather than a single metallic comb.
  SPACE_AP_DELAY_MS: array[0..SPACE_ALLPASS_COUNT-1] of Single = (7.3, 13.9, 22.1);
  SPACE_AP_FEEDBACK = 0.7;
  SPACE_SIZE_MAX = 1.5;   // Size=1 stretches the base delays to 1.5x

  // Lateral early reflections: a few delayed taps of the mid, added to the SIDE
  // (opposite sign L/R -> still mono-safe). They sit at LONGER delays (>1 ms) than
  // the near-field allpass, so they raise the side energy / sideMid WITHOUT moving
  // the +-1ms interaural correlation (IACC) much -> they decouple "how wide"
  // (side energy) from "how decorrelated near-field" (IACC), letting us match a
  // real spaced-mic recording that has BOTH high IACC and high side energy.
  SPACE_REFLECT_TAPS = 3;
  SPACE_REFLECT_MS:   array[0..SPACE_REFLECT_TAPS-1] of Single = (11.0, 23.0, 37.0);
  SPACE_REFLECT_SIGN: array[0..SPACE_REFLECT_TAPS-1] of Single = (1.0, -1.0, 1.0);
  SPACE_REFLECT_GAIN: array[0..SPACE_REFLECT_TAPS-1] of Single = (0.7, 0.5, 0.35);

type
  { one Schroeder allpass section (ring buffer) }
  TSpaceAllpass = record
    Buffer: array of Single;
    Size:   Integer;      // buffer length
    Index:  Integer;      // write cursor
    Delay:  Integer;      // current delay (samples)
  end;

  { TSedaiAutoSpace }
  // Mono/dual-mono -> decorrelated stereo widener (no reverb tail).
  TSedaiAutoSpace = class(TSedaiEffect)
  private
    FWidth: Single;       // 0..1 side amount (decorrelation / IACC)
    FSize:  Single;       // 0..1 scales the allpass delays (close..roomy)
    FMix:   Single;       // 0..1 dry(input)->wet(widened) crossfade
    FReflect: Single;     // 0..1 lateral early-reflection amount (raises side energy)
    FAP: array[0..SPACE_ALLPASS_COUNT-1] of TSpaceAllpass;
    // mid-history delay line for the lateral reflection taps
    FRefBuf: array of Single;
    FRefSize, FRefIdx: Integer;
    FRefTap: array[0..SPACE_REFLECT_TAPS-1] of Integer;  // current tap offsets (samples)
    FRefNorm: Single;     // normalization so FReflect is a meaningful 0..1

    procedure SetWidth(AValue: Single);
    procedure SetSize(AValue: Single);
    procedure SetReflect(AValue: Single);
    procedure AllocateBuffers;
    procedure UpdateDelays;
    function ProcessAllpass(var AAP: TSpaceAllpass; AIn: Single): Single;

  protected
    procedure SampleRateChanged; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Width: 0 = mono (bypass image), 1 = maximally decorrelated (near-field IACC).
    property Width: Single read FWidth write SetWidth;
    // Size: perceived source/room scale (stretches the decorrelation delays).
    property Size: Single read FSize write SetSize;
    // Reflect: lateral early-reflection amount. Raises side energy (sideMid) at
    // long delays without much IACC change -> matches spaced-mic width. 0 = off.
    property Reflect: Single read FReflect write SetReflect;
    // Mix: crossfade between the untouched input and the widened signal.
    property Mix: Single read FMix write FMix;
  end;

implementation

{ TSedaiAutoSpace }

constructor TSedaiAutoSpace.Create;
begin
  inherited Create;

  FWidth := 0.6;
  FSize  := 0.5;
  FMix   := 1.0;
  FReflect := 0.0;   // off by default -> identical to the allpass-only widener

  AllocateBuffers;
end;

destructor TSedaiAutoSpace.Destroy;
var
  I: Integer;
begin
  for I := 0 to SPACE_ALLPASS_COUNT - 1 do
    SetLength(FAP[I].Buffer, 0);
  SetLength(FRefBuf, 0);
  inherited Destroy;
end;

procedure TSedaiAutoSpace.SetWidth(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0
  else if AValue > 1.0 then AValue := 1.0;
  FWidth := AValue;
end;

procedure TSedaiAutoSpace.SetSize(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0
  else if AValue > 1.0 then AValue := 1.0;
  FSize := AValue;
  UpdateDelays;
end;

procedure TSedaiAutoSpace.SetReflect(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0
  else if AValue > 1.0 then AValue := 1.0;
  FReflect := AValue;
end;

procedure TSedaiAutoSpace.AllocateBuffers;
var
  I, sz: Integer;
  sr: Single;
begin
  if FSampleRate > 0 then sr := FSampleRate else sr := 44100.0;
  for I := 0 to SPACE_ALLPASS_COUNT - 1 do
  begin
    // size for the largest possible delay (Size=1 -> SPACE_SIZE_MAX)
    sz := Round(SPACE_AP_DELAY_MS[I] * SPACE_SIZE_MAX * 0.001 * sr) + 4;
    if sz < 4 then sz := 4;
    SetLength(FAP[I].Buffer, sz);
    FAP[I].Size := sz;
    FAP[I].Index := 0;
  end;
  // reflection delay line: sized for the longest tap at max Size
  sz := Round(SPACE_REFLECT_MS[SPACE_REFLECT_TAPS-1] * SPACE_SIZE_MAX * 0.001 * sr) + 4;
  if sz < 4 then sz := 4;
  SetLength(FRefBuf, sz);
  FRefSize := sz;
  FRefIdx := 0;
  // normalize so FReflect=1 keeps the reflection sum ~unit-scaled
  FRefNorm := 0;
  for I := 0 to SPACE_REFLECT_TAPS - 1 do FRefNorm := FRefNorm + Abs(SPACE_REFLECT_GAIN[I]);
  if FRefNorm > 0 then FRefNorm := 1.0 / FRefNorm else FRefNorm := 1.0;
  UpdateDelays;
  Reset;
end;

procedure TSedaiAutoSpace.UpdateDelays;
var
  I, d: Integer;
  scale, sr: Single;
begin
  if FSampleRate > 0 then sr := FSampleRate else sr := 44100.0;
  scale := 0.5 + FSize;               // Size 0..1 -> 0.5x..1.5x the base delay
  for I := 0 to SPACE_ALLPASS_COUNT - 1 do
  begin
    d := Round(SPACE_AP_DELAY_MS[I] * scale * 0.001 * sr);
    if d < 1 then d := 1;
    if (FAP[I].Size > 0) and (d >= FAP[I].Size) then d := FAP[I].Size - 1;
    FAP[I].Delay := d;
  end;
  for I := 0 to SPACE_REFLECT_TAPS - 1 do
  begin
    d := Round(SPACE_REFLECT_MS[I] * scale * 0.001 * sr);
    if d < 1 then d := 1;
    if (FRefSize > 0) and (d >= FRefSize) then d := FRefSize - 1;
    FRefTap[I] := d;
  end;
end;

procedure TSedaiAutoSpace.SampleRateChanged;
begin
  inherited SampleRateChanged;
  AllocateBuffers;
end;

procedure TSedaiAutoSpace.Reset;
var
  I, J: Integer;
begin
  inherited Reset;
  for I := 0 to SPACE_ALLPASS_COUNT - 1 do
  begin
    FAP[I].Index := 0;
    for J := 0 to FAP[I].Size - 1 do
      FAP[I].Buffer[J] := 0.0;
  end;
  FRefIdx := 0;
  for J := 0 to FRefSize - 1 do FRefBuf[J] := 0.0;
end;

// Schroeder allpass (true unit-magnitude form): with the internal comb state
//   w[n] = x[n] + g*w[n-D],  the output is  y[n] = -g*w[n] + w[n-D].
// (Using -g*x[n] instead of -g*w[n] is NOT all-pass: it has ~2.6x gain at the
//  comb resonances, which cascades and clips -- the bug this replaces.)
function TSedaiAutoSpace.ProcessAllpass(var AAP: TSpaceAllpass; AIn: Single): Single;
var
  readIdx: Integer;
  bufOut, w: Single;
begin
  readIdx := AAP.Index - AAP.Delay;
  if readIdx < 0 then readIdx := readIdx + AAP.Size;
  bufOut := AAP.Buffer[readIdx];              // w[n-D]
  w := AIn + SPACE_AP_FEEDBACK * bufOut;       // w[n]
  Result := bufOut - SPACE_AP_FEEDBACK * w;    // y[n] = -g*w[n] + w[n-D]
  AAP.Buffer[AAP.Index] := w;
  AAP.Index := AAP.Index + 1;
  if AAP.Index >= AAP.Size then AAP.Index := 0;
end;

procedure TSedaiAutoSpace.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I, K, ridx: Integer;
  L, R, M, D, S, refl, WL, WR: Single;
begin
  if FAP[0].Size = 0 then AllocateBuffers;

  for I := 0 to AFrameCount - 1 do
  begin
    L := AInput[I * 2];
    R := AInput[I * 2 + 1];
    M := 0.5 * (L + R);

    // Decorrelate the mid through the allpass chain (magnitude-preserving).
    D := M;
    for K := 0 to SPACE_ALLPASS_COUNT - 1 do
      D := ProcessAllpass(FAP[K], D);

    // Lateral early reflections: delayed taps of the mid, summed with alternating
    // sign, added to the SIDE. Long delays (>1 ms) -> raise side energy without
    // moving the +-1ms IACC much. Opt-in (FReflect 0 = off).
    refl := 0;
    if FReflect > 0 then
    begin
      for K := 0 to SPACE_REFLECT_TAPS - 1 do
      begin
        ridx := FRefIdx - FRefTap[K];
        if ridx < 0 then ridx := ridx + FRefSize;
        refl := refl + SPACE_REFLECT_SIGN[K] * SPACE_REFLECT_GAIN[K] * FRefBuf[ridx];
      end;
      refl := refl * FRefNorm * FReflect;
    end;
    FRefBuf[FRefIdx] := M;
    FRefIdx := FRefIdx + 1;
    if FRefIdx >= FRefSize then FRefIdx := 0;

    // Lauridsen: the widening lives entirely in the side -> mono-safe.
    S := FWidth * D + refl;
    WL := M + S;
    WR := M - S;

    // Crossfade the untouched input with the widened signal. Mono-safe for any
    // Mix: (L+R)*(1-Mix) + (WL+WR)*Mix = (L+R)*(1-Mix) + 2M*Mix = L+R.
    AOutput[I * 2]     := L * (1.0 - FMix) + WL * FMix;
    AOutput[I * 2 + 1] := R * (1.0 - FMix) + WR * FMix;
  end;
end;

end.
