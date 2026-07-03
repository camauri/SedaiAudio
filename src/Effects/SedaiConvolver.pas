{*
 * Sedai Audio Foundation - Convolver (short stereo FIR: body/radiation IR)
 *
 * TSedaiConvolver convolves the signal with a short stereo impulse response — the
 * "measured" path for "il tubo" (C v2): a body/radiation IR captured by sweep
 * (Farina) or derived from a SOFA directivity database applies the instrument's
 * real body colour + spatial radiation to the additive partials (commuted synthesis
 * on the LTI additive output). The parametric body bank (TSedaiBodyResonator, C v1)
 * and this measured convolver coexist.
 *
 * Each channel is convolved with its own IR (out L = L * hL, out R = R * hR). For a
 * mono/dual-mono source (L=R=mid) two DIFFERENT IRs give decorrelated stereo
 * radiation; a single mono IR (hL=hR) just colours. Direct time-domain convolution
 * (O(N*M)) — fine for a SHORT IR (body/radiation is tens of ms, <= a few thousand
 * taps); a long room IR would want partitioned FFT (a future upgrade).
 *
 * Normalize the IR on-axis before loading so the convolver adds body/radiation
 * without re-EQ-ing the spectrum the partials already match (see SAF_TUBO_DESIGN).
 *
 * A per-Part / bus insert (like the other spatial effects), NOT serialised into a
 * .safinst. Load an IR via LoadIR (arrays) or LoadIRFromFile (WAV/AIFF/FLAC via the
 * SAF reader). No IR loaded -> passthrough.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiConvolver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiEffect,
  SedaiAudioBuffer, SedaiAudioFileReader;

const
  MAX_IR_TAPS = 8192;   // ~170 ms at 48 kHz — body/radiation, not room

type
  { TSedaiConvolver }
  TSedaiConvolver = class(TSedaiEffect)
  private
    FIRL, FIRR: array of Single;   // per-channel impulse responses
    FLen: Integer;                 // active IR length (max of the two)
    FRingL, FRingR: array of Single;  // input history ring buffers
    FRingSize, FRingIdx: Integer;
    FMix: Single;                  // dry/wet

    procedure AllocRing;
    function ConvOne(const IR, Ring: array of Single; RingIdx: Integer): Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Load per-channel IRs from arrays (truncated to MAX_IR_TAPS). Empty -> clears.
    procedure LoadIR(const AL, AR: array of Single);
    // Load a stereo (or mono -> both channels) IR from an audio file via the SAF
    // reader. Returns True on success. Optional ANormalize scales the IR so its peak
    // magnitude is 1 (keeps the level sane; do on-axis normalization upstream).
    function LoadIRFromFile(const APath: string; ANormalize: Boolean = True): Boolean;
    procedure ClearIR;

    function IRLength: Integer;
    property Mix: Single read FMix write FMix;
  end;

implementation

constructor TSedaiConvolver.Create;
begin
  inherited Create;
  FLen := 0;
  FRingSize := 0;
  FRingIdx := 0;
  FMix := 1.0;
end;

destructor TSedaiConvolver.Destroy;
begin
  SetLength(FIRL, 0); SetLength(FIRR, 0);
  SetLength(FRingL, 0); SetLength(FRingR, 0);
  inherited Destroy;
end;

procedure TSedaiConvolver.AllocRing;
var i: Integer;
begin
  FRingSize := FLen;
  if FRingSize < 1 then FRingSize := 1;
  SetLength(FRingL, FRingSize);
  SetLength(FRingR, FRingSize);
  for i := 0 to FRingSize - 1 do begin FRingL[i] := 0; FRingR[i] := 0; end;
  FRingIdx := 0;
end;

procedure TSedaiConvolver.ClearIR;
begin
  SetLength(FIRL, 0); SetLength(FIRR, 0);
  FLen := 0;
  AllocRing;
end;

procedure TSedaiConvolver.LoadIR(const AL, AR: array of Single);
var
  nL, nR, i: Integer;
begin
  nL := Length(AL); if nL > MAX_IR_TAPS then nL := MAX_IR_TAPS;
  nR := Length(AR); if nR > MAX_IR_TAPS then nR := MAX_IR_TAPS;
  FLen := Max(nL, nR);
  if FLen <= 0 then begin ClearIR; Exit; end;
  SetLength(FIRL, FLen); SetLength(FIRR, FLen);
  for i := 0 to FLen - 1 do
  begin
    if i < nL then FIRL[i] := AL[i] else FIRL[i] := 0;
    if i < nR then FIRR[i] := AR[i] else FIRR[i] := 0;
  end;
  AllocRing;
end;

function TSedaiConvolver.LoadIRFromFile(const APath: string; ANormalize: Boolean): Boolean;
var
  rd: TSedaiAudioFileReader;
  buf: TSedaiAudioBuffer;
  n, i, taps: Integer;
  hL, hR: array of Single;
  peak: Single;
begin
  Result := False;
  buf := nil;
  rd := TSedaiAudioFileReader.Create;
  try
    if not (rd.OpenFile(APath) and rd.ReadAll(buf) and (buf <> nil)) then Exit;
    n := buf.SampleCount;
    taps := n; if taps > MAX_IR_TAPS then taps := MAX_IR_TAPS;
    if taps <= 0 then Exit;
    SetLength(hL, taps); SetLength(hR, taps);
    peak := 1e-12;
    for i := 0 to taps - 1 do
    begin
      hL[i] := buf.GetSample(0, i);
      if buf.Channels > 1 then hR[i] := buf.GetSample(1, i) else hR[i] := hL[i];
      if Abs(hL[i]) > peak then peak := Abs(hL[i]);
      if Abs(hR[i]) > peak then peak := Abs(hR[i]);
    end;
    if ANormalize and (peak > 0) then
      for i := 0 to taps - 1 do begin hL[i] := hL[i] / peak; hR[i] := hR[i] / peak; end;
    LoadIR(hL, hR);
    Result := True;
  finally
    if buf <> nil then buf.Free;
    rd.Free;
  end;
end;

procedure TSedaiConvolver.Reset;
var i: Integer;
begin
  inherited Reset;
  for i := 0 to FRingSize - 1 do begin FRingL[i] := 0; FRingR[i] := 0; end;
  FRingIdx := 0;
end;

// y[n] = sum_j IR[j] * x[n-j].  Ring holds x with the newest sample at RingIdx-1
// (RingIdx points at the slot that was just written +1). We walk back j samples.
function TSedaiConvolver.ConvOne(const IR, Ring: array of Single; RingIdx: Integer): Single;
var
  j, idx: Integer;
  acc: Single;
begin
  acc := 0;
  // newest sample is at (RingIdx-1); IR[0] multiplies the newest (x[n]).
  for j := 0 to FLen - 1 do
  begin
    idx := RingIdx - 1 - j;
    while idx < 0 do idx := idx + FRingSize;
    acc := acc + IR[j] * Ring[idx];
  end;
  Result := acc;
end;

procedure TSedaiConvolver.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  i: Integer;
  L, R, wL, wR: Single;
begin
  if FLen <= 0 then
  begin
    // passthrough
    for i := 0 to AFrameCount * 2 - 1 do AOutput[i] := AInput[i];
    Exit;
  end;
  for i := 0 to AFrameCount - 1 do
  begin
    L := AInput[i * 2];
    R := AInput[i * 2 + 1];
    // write newest input into the ring
    FRingL[FRingIdx] := L;
    FRingR[FRingIdx] := R;
    FRingIdx := FRingIdx + 1;
    if FRingIdx >= FRingSize then FRingIdx := 0;
    // convolve (newest sample is at FRingIdx-1)
    wL := ConvOne(FIRL, FRingL, FRingIdx);
    wR := ConvOne(FIRR, FRingR, FRingIdx);
    AOutput[i * 2]     := L * (1.0 - FMix) + wL * FMix;
    AOutput[i * 2 + 1] := R * (1.0 - FMix) + wR * FMix;
  end;
end;

function TSedaiConvolver.IRLength: Integer;
begin
  Result := FLen;
end;

end.
