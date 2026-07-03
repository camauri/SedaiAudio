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
    FAP: array[0..SPACE_ALLPASS_COUNT-1] of TSpaceAllpass;

    procedure SetWidth(AValue: Single);
    procedure SetSize(AValue: Single);
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

    // Width: 0 = mono (bypass image), 1 = maximally decorrelated.
    property Width: Single read FWidth write SetWidth;
    // Size: perceived source/room scale (stretches the decorrelation delays).
    property Size: Single read FSize write SetSize;
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

  AllocateBuffers;
end;

destructor TSedaiAutoSpace.Destroy;
var
  I: Integer;
begin
  for I := 0 to SPACE_ALLPASS_COUNT - 1 do
    SetLength(FAP[I].Buffer, 0);
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
  I, K: Integer;
  L, R, M, D, WL, WR: Single;
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

    // Lauridsen: the widening lives entirely in the side -> mono-safe.
    WL := M + FWidth * D;
    WR := M - FWidth * D;

    // Crossfade the untouched input with the widened signal. Mono-safe for any
    // Mix: (L+R)*(1-Mix) + (WL+WR)*Mix = (L+R)*(1-Mix) + 2M*Mix = L+R.
    AOutput[I * 2]     := L * (1.0 - FMix) + WL * FMix;
    AOutput[I * 2 + 1] := R * (1.0 - FMix) + WR * FMix;
  end;
end;

end.
