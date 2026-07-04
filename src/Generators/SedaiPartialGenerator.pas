{*
 * Sedai Audio Foundation - Free-Partial Generator
 *
 * TSedaiPartialGenerator is a "second-generation" additive engine: instead of
 * harmonics locked to k*f0 (see TSedaiAdditiveGenerator), it renders N FREE
 * partials, each with its own breakpoint track of (time, frequency, amplitude).
 * Frequencies are arbitrary and time-varying -> it reproduces the inharmonic
 * "flesh" (reed noise, air, drifting HF) that a pure harmonic additive discards.
 *
 * This is the McAulay-Quatieri sinusoidal model: each partial is one oscillator
 * with continuous phase, integrating 2*pi*freq(t)/SR, with freq/amp interpolated
 * linearly between breakpoints. A partial is "born" at its first breakpoint and
 * "dies" at its last (silent outside that range) -> partials appear/disappear in
 * time like SPEAR's tracks. The working reference is tmp/saf_spear_resynth.lpr.
 *
 * Transposition: partial frequencies are stored as measured at analysis time
 * (FAnalysisF0). At NoteOn every partial frequency is multiplied by the same
 * ratio FFrequency/FAnalysisF0, so playing a different note transposes the whole
 * cluster while preserving inharmonic relationships. Set FAnalysisF0 = 0 to play
 * at the recorded pitch unchanged (ratio 1).
 *
 * Clean release: on NoteOff the whole output is faded out (exponential, ~120 ms
 * default) instead of truncated -> avoids the "metallic tail" where stray HF
 * partials hang. Amplitude lives in the partial tracks themselves (no ADSR on
 * top); loudness is matched upstream (analysis/preset).
 *
 * Opt-in / inert: with 0 partials the generator is silent.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiPartialGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode,
  SedaiOscillator;

const
  PARTIAL_DEFAULT_MAX = 512;   // typical cap (e.g. SPEAR exports a few hundred)
  PARTIAL_TWO_PI = 2 * Pi;

type
  { TPartialBreak }
  // One breakpoint of a partial's trajectory: absolute time (s), frequency (Hz,
  // as measured at analysis), and linear amplitude.
  TPartialBreak = record
    t, freq, amp: Single;
  end;

  { TPartialData }
  TPartialData = record
    Points: array of TPartialBreak;   // ascending in t; a segment needs >= 2
    Phase: Double;                    // continuous phase, in turns (0..1)
    Cursor: Integer;                  // segment cursor, advances with FNoteTime
    TFirst, TLast: Single;            // cached born/death times (Points[0]/[n-1])
  end;

  { TSedaiPartialGenerator }
  TSedaiPartialGenerator = class(TSedaiSignalGenerator)
  private
    FPartials: array of TPartialData;
    FAnalysisF0: Single;      // reference f0 for transposition; 0 = play as recorded
    FMaxDur: Single;          // latest breakpoint time across all partials (s)

    FNote: Integer;
    FVelocity: Single;
    FGateOpen: Boolean;
    FReleasing: Boolean;
    FNoteTime: Double;        // seconds since NoteOn (drives the tracks)

    // Clean release: exponential fade applied to the whole output on NoteOff.
    FReleaseTime: Single;     // seconds to ~-80 dB
    FReleaseGain: Single;     // current fade multiplier (1 while sustaining)
    FReleaseCoeff: Single;    // per-sample decay factor

    FNyquistLimit: Single;

    function CalculateSample: Single;
    procedure RecalcReleaseCoeff;
    procedure UpdateNyquistLimit;
    procedure UpdateMaxDur;

  public
    constructor Create; override;
    destructor Destroy; override;

    // From TSedaiAudioObject
    procedure SampleRateChanged; override;

    // Note control
    procedure NoteOn(ANote: Integer; AVelocity: Single);
    procedure NoteOff;
    procedure Kill;

    // Generate samples (GenerateBlock inherited: calls GenerateSample per frame)
    function GenerateSample: Single; override;

    // Partial control. SetPartialCount pre-sizes the (empty) partial array;
    // SetPartial fills one partial's breakpoint track from parallel arrays
    // (times ascending from 0). ClearPartials empties everything.
    procedure SetPartialCount(ACount: Integer);
    procedure SetPartial(AIndex: Integer; const ATimes, AFreqs, AAmps: array of Single);
    procedure ClearPartials;

    // Reference f0 for transposition (Hz). 0 = play partials at their recorded
    // absolute frequencies (ratio 1). When > 0, NoteOn scales all partial
    // frequencies by FFrequency/FAnalysisF0.
    procedure SetAnalysisF0(AHz: Single);

    // Release fade time (seconds). 0 = immediate cut.
    procedure SetRelease(ASeconds: Single);

    function GetPartialCount: Integer;
    function GetPartialBreakCount(AIndex: Integer): Integer;

    // Properties
    property AnalysisF0: Single read FAnalysisF0 write SetAnalysisF0;
    property ReleaseTime: Single read FReleaseTime write SetRelease;
    property MaxDuration: Single read FMaxDur;
    property Note: Integer read FNote;
    property Velocity: Single read FVelocity;
    property GateOpen: Boolean read FGateOpen;
    property Releasing: Boolean read FReleasing;
  end;

implementation

{ TSedaiPartialGenerator }

constructor TSedaiPartialGenerator.Create;
begin
  inherited Create;

  SetLength(FPartials, 0);
  FAnalysisF0 := 0;
  FMaxDur := 0;
  FNote := -1;
  FVelocity := 1.0;
  FGateOpen := False;
  FReleasing := False;
  FNoteTime := 0;
  FReleaseTime := 0.12;
  FReleaseGain := 1.0;
  UpdateNyquistLimit;
  RecalcReleaseCoeff;
end;

destructor TSedaiPartialGenerator.Destroy;
begin
  SetLength(FPartials, 0);
  inherited Destroy;
end;

procedure TSedaiPartialGenerator.SampleRateChanged;
begin
  inherited SampleRateChanged;
  UpdateNyquistLimit;
  RecalcReleaseCoeff;
end;

procedure TSedaiPartialGenerator.UpdateNyquistLimit;
begin
  FNyquistLimit := FSampleRate * 0.5;
end;

// Exponential fade reaching ~-80 dB (1e-4) at FReleaseTime. FReleaseTime <= 0
// => coeff 0 (immediate cut on the next sample).
procedure TSedaiPartialGenerator.RecalcReleaseCoeff;
begin
  if (FReleaseTime > 0) and (FSampleRate > 0) then
    FReleaseCoeff := Exp(Ln(1e-4) / (FReleaseTime * FSampleRate))
  else
    FReleaseCoeff := 0.0;
end;

procedure TSedaiPartialGenerator.UpdateMaxDur;
var
  i, n: Integer;
begin
  FMaxDur := 0;
  for i := 0 to High(FPartials) do
  begin
    n := Length(FPartials[i].Points);
    if (n > 0) and (FPartials[i].Points[n - 1].t > FMaxDur) then
      FMaxDur := FPartials[i].Points[n - 1].t;
  end;
end;

procedure TSedaiPartialGenerator.NoteOn(ANote: Integer; AVelocity: Single);
var
  i: Integer;
begin
  FNote := ANote;
  FVelocity := EnsureRange(AVelocity, 0, 1);
  FGateOpen := True;
  FReleasing := False;
  FReleaseGain := 1.0;
  FNoteTime := 0;

  // Frequency from MIDI note (set directly, as the additive generator does)
  FFrequency := 440 * Power(2, (ANote - 69) / 12);

  // Reset per-partial phase + cursor so playback starts clean
  for i := 0 to High(FPartials) do
  begin
    FPartials[i].Phase := 0;
    FPartials[i].Cursor := 0;
  end;
end;

procedure TSedaiPartialGenerator.NoteOff;
begin
  if not FGateOpen then Exit;
  FGateOpen := False;
  FReleasing := True;
  FReleaseGain := 1.0;
end;

procedure TSedaiPartialGenerator.Kill;
var
  i: Integer;
begin
  FGateOpen := False;
  FReleasing := False;
  FNote := -1;
  FNoteTime := 0;
  FReleaseGain := 1.0;
  for i := 0 to High(FPartials) do
  begin
    FPartials[i].Phase := 0;
    FPartials[i].Cursor := 0;
  end;
end;

// Sum every currently-active partial. A partial is active only between its first
// and last breakpoint (born/death); outside that range it contributes nothing.
function TSedaiPartialGenerator.CalculateSample: Single;
var
  i, n, c: Integer;
  ratio, t0, t1, frac, f, a, fp: Single;
begin
  Result := 0;

  if FAnalysisF0 > 0 then
    ratio := FFrequency / FAnalysisF0
  else
    ratio := 1.0;

  for i := 0 to High(FPartials) do
  begin
    with FPartials[i] do
    begin
      n := Length(Points);
      if n < 2 then Continue;                 // need a segment to interpolate
      if FNoteTime < TFirst then Continue;     // not born yet
      if FNoteTime > TLast then Continue;      // died

      // advance cursor to the segment containing FNoteTime (monotonic clock)
      c := Cursor;
      if c < 0 then c := 0
      else if c > n - 2 then c := n - 2;
      while (c < n - 2) and (FNoteTime > Points[c + 1].t) do Inc(c);
      Cursor := c;

      t0 := Points[c].t;
      t1 := Points[c + 1].t;
      if t1 <= t0 then
      begin
        f := Points[c].freq;
        a := Points[c].amp;
      end
      else
      begin
        frac := (FNoteTime - t0) / (t1 - t0);
        if frac < 0 then frac := 0 else if frac > 1 then frac := 1;
        f := Points[c].freq + (Points[c + 1].freq - Points[c].freq) * frac;
        a := Points[c].amp  + (Points[c + 1].amp  - Points[c].amp)  * frac;
      end;

      fp := f * ratio;
      if (fp > 0) and (fp < FNyquistLimit) then
      begin
        Result := Result + a * Sin(PARTIAL_TWO_PI * Phase);
        Phase := Phase + fp / FSampleRate;
        if Phase >= 1.0 then Phase := Phase - 1.0;
      end;
    end;
  end;
end;

function TSedaiPartialGenerator.GenerateSample: Single;
begin
  if (not FGateOpen) and (not FReleasing) then
  begin
    Result := 0;
    Exit;
  end;

  Result := CalculateSample * FVelocity * FAmplitude;

  // Clean release: fade the whole output out, don't truncate it
  if FReleasing then
  begin
    Result := Result * FReleaseGain;
    FReleaseGain := FReleaseGain * FReleaseCoeff;
    if FReleaseGain < 1e-4 then
    begin
      FReleaseGain := 0;
      FReleasing := False;
    end;
  end;

  // Advance the per-note clock that drives the partial tracks
  if FSampleRate > 0 then
    FNoteTime := FNoteTime + 1.0 / FSampleRate;
end;

procedure TSedaiPartialGenerator.SetPartialCount(ACount: Integer);
var
  old, i: Integer;
begin
  if ACount < 0 then ACount := 0;
  old := Length(FPartials);
  SetLength(FPartials, ACount);
  // initialize any newly-added partials to empty/clean state
  for i := old to ACount - 1 do
  begin
    SetLength(FPartials[i].Points, 0);
    FPartials[i].Phase := 0;
    FPartials[i].Cursor := 0;
    FPartials[i].TFirst := 0;
    FPartials[i].TLast := 0;
  end;
  UpdateMaxDur;
end;

procedure TSedaiPartialGenerator.SetPartial(AIndex: Integer;
  const ATimes, AFreqs, AAmps: array of Single);
var
  n, i: Integer;
begin
  if (AIndex < 0) or (AIndex >= Length(FPartials)) then Exit;
  n := Length(ATimes);
  if Length(AFreqs) < n then n := Length(AFreqs);
  if Length(AAmps) < n then n := Length(AAmps);

  SetLength(FPartials[AIndex].Points, n);
  for i := 0 to n - 1 do
  begin
    FPartials[AIndex].Points[i].t := ATimes[i];
    FPartials[AIndex].Points[i].freq := AFreqs[i];
    FPartials[AIndex].Points[i].amp := AAmps[i];
  end;
  FPartials[AIndex].Cursor := 0;
  FPartials[AIndex].Phase := 0;
  if n > 0 then
  begin
    FPartials[AIndex].TFirst := ATimes[0];
    FPartials[AIndex].TLast := ATimes[n - 1];
  end
  else
  begin
    FPartials[AIndex].TFirst := 0;
    FPartials[AIndex].TLast := 0;
  end;
  UpdateMaxDur;
end;

procedure TSedaiPartialGenerator.ClearPartials;
begin
  SetLength(FPartials, 0);
  FMaxDur := 0;
end;

procedure TSedaiPartialGenerator.SetAnalysisF0(AHz: Single);
begin
  if AHz < 0 then AHz := 0;
  FAnalysisF0 := AHz;
end;

procedure TSedaiPartialGenerator.SetRelease(ASeconds: Single);
begin
  if ASeconds < 0 then ASeconds := 0;
  FReleaseTime := ASeconds;
  RecalcReleaseCoeff;
end;

function TSedaiPartialGenerator.GetPartialCount: Integer;
begin
  Result := Length(FPartials);
end;

function TSedaiPartialGenerator.GetPartialBreakCount(AIndex: Integer): Integer;
begin
  if (AIndex >= 0) and (AIndex < Length(FPartials)) then
    Result := Length(FPartials[AIndex].Points)
  else
    Result := 0;
end;

end.
