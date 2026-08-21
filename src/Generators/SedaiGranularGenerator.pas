// ============================================================================
// SedaiGranularGenerator — sound as a cloud of very short grains.
//
// A grain is a few tens of milliseconds of a recording, faded in and out, and
// dropped somewhere in time. Play a few of them a second and you hear the
// separate fragments; play a few hundred and they fuse into a continuous
// texture whose pitch and whose SPEED are no longer the same knob. That is the
// whole reason the technique exists: a sampler can only play a recording faster
// to make it shorter, and granular can hold it still.
//
// WHAT MAKES IT SOUND LIKE A TEXTURE RATHER THAN A STUTTER is the spread. Every
// grain takes its position, its pitch and its place in the stereo image from
// the same numbers plus a little randomness, and without that randomness a
// cloud is just a delay line with a bad window. The randomness is this object's
// OWN (see SedaiRandom): two granular voices in one mix must not draw from the
// same stream, or they stop being two clouds.
//
// EXACT RECONSTRUCTION IS THE TEST. With a Hann window, grains half a grain
// apart, natural speed and no spread at all, the overlap-add of the grains is
// the source again — because a periodic Hann at 50% overlap sums to exactly 1.
// Anything wrong with the scheduler, the windowing or the read position shows
// up there as a number, long before anyone has to judge a texture by ear.
//
// REAL-TIME SAFE. A fixed pool of grains, allocated once. When they are all
// busy a new grain is REFUSED AND COUNTED rather than allocated: a click in the
// audio callback is worse than a grain that never played, and a count nobody
// can see is worse than both.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiGranularGenerator;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, SedaiAudioTypes, SedaiAudioBuffer, SedaiAudioFileReader,
  SedaiOscillator, SedaiRandom;

const
  SEDAI_GRAIN_POOL = 128;      // how many may sound at once

type
  // The fade a grain lives inside.
  TSedaiGrainWindow = (
    gwHann,        // the default; the only one that reconstructs exactly
    gwTriangle,    // cheaper, and audibly harder at the edges
    gwTukey        // flat in the middle with cosine skirts: more body per grain
  );

  TSedaiGranularGenerator = class(TSedaiSignalGenerator)
  private
    FSource: TSedaiAudioBuffer;
    FOwnsSource: Boolean;
    // FSampleRate e SetSampleRate arrivano dalla base (Cardinal).

    FGrainMs: Single;          // length of one grain
    FDensity: Single;          // grains per second
    FSpeed: Single;            // how fast the head walks the source, 1 = natural
    FPitch: Single;            // playback rate of a grain, 1 = natural
    FPosition: Single;         // 0..1, where the head is right now
    FPosSpread: Single;        // seconds, either side of the head
    FPitchSpread: Single;      // cents, either side of the pitch
    FPanSpread: Single;        // 0..1
    FWindow: TSedaiGrainWindow;
    FTukeySkirt: Single;       // 0..0.5, how much of a Tukey grain fades

    FHead: Double;             // read head, in source samples
    FSinceLast: Double;        // samples since the last grain was launched
    FRandom: TSedaiRandom;
    FRefused: QWord;           // grains the pool had no room for

    FGrains: array[0..SEDAI_GRAIN_POOL - 1] of record
      Active: Boolean;
      Pos: Double;             // read position in the source
      Step: Double;            // per-sample increment
      Age: Integer;
      Len: Integer;
      Amp: Single;
      PanL, PanR: Single;
    end;
    FLastL, FLastR: Single;

    function SourceAt(APos: Double; AChannel: Integer): Single;
    function WindowAt(AAge, ALen: Integer): Single;
    procedure Launch;
    procedure RenderOne(out AMono, ALeft, ARight: Single);
    function FreeGrain: Integer;
    procedure SetGrainMs(AValue: Single);
    procedure SetDensity(AValue: Single);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure LoadSample(ASample: TSedaiAudioBuffer; ATakeOwnership: Boolean = False);
    // Read the cloud's source from any file SedaiAudioFileReader handles. The
    // buffer becomes ours, because a grain that outlives its recording is a
    // read into freed memory.
    function LoadSampleFromFile(const APath: string): Boolean;
    procedure ClearSample;
    function HasSample: Boolean;

    // Mono: the two channels summed. Stereo is what the spread is for, so a
    // host that can take two channels should ask for them.
    function GenerateSample: Single; override;
    procedure GenerateStereo(out ALeft, ARight: Single);

    // Make the cloud reproducible. Leave it and each generator still gets its
    // own stream, the same on every run.
    procedure SetSeed(ASeed: QWord);

    function ActiveGrains: Integer;

    property GrainMs: Single read FGrainMs write SetGrainMs;
    property Density: Single read FDensity write SetDensity;
    // 1.0 walks the source at its natural speed; 0.0 freezes it, which is the
    // thing a sampler cannot do.
    property Speed: Single read FSpeed write FSpeed;
    property Pitch: Single read FPitch write FPitch;
    property Position: Single read FPosition write FPosition;
    property PositionSpread: Single read FPosSpread write FPosSpread;
    property PitchSpread: Single read FPitchSpread write FPitchSpread;
    property PanSpread: Single read FPanSpread write FPanSpread;
    property Window: TSedaiGrainWindow read FWindow write FWindow;
    property TukeySkirt: Single read FTukeySkirt write FTukeySkirt;
    // Grains the pool had no room for. Print it: a cloud quietly thinning out
    // because the pool is full sounds like a bad parameter, not like a limit.
    property Refused: QWord read FRefused;
  end;

implementation

constructor TSedaiGranularGenerator.Create;
begin
  inherited Create;
  FSource := nil;
  FOwnsSource := False;
  FGrainMs := 50.0;
  FDensity := 40.0;
  FSpeed := 1.0;
  FPitch := 1.0;
  FPosition := 0.0;
  FPosSpread := 0.0;
  FPitchSpread := 0.0;
  FPanSpread := 0.0;
  FWindow := gwHann;
  FTukeySkirt := 0.25;
  FRandom.Seed(SedaiNextSeed);
  Reset;
end;

destructor TSedaiGranularGenerator.Destroy;
begin
  ClearSample;
  inherited Destroy;
end;

procedure TSedaiGranularGenerator.Reset;
var
  I: Integer;
begin
  inherited Reset;
  for I := 0 to SEDAI_GRAIN_POOL - 1 do FGrains[I].Active := False;
  FHead := 0.0;
  // Large, so the FIRST grain launches on the very first sample rather than one
  // interval late — a cloud that starts with a hole in it is a bug you hear.
  // It is CLAMPED to one interval in the render, which matters: left unclamped
  // this would ask the launcher for 1e30 grains on the first sample.
  FSinceLast := 1.0e9;
  FRefused := 0;
  FLastL := 0.0; FLastR := 0.0;
end;

procedure TSedaiGranularGenerator.SetSeed(ASeed: QWord);
begin
  FRandom.Seed(ASeed);
end;

procedure TSedaiGranularGenerator.SetGrainMs(AValue: Single);
begin
  if AValue < 1.0 then AValue := 1.0;
  if AValue > 2000.0 then AValue := 2000.0;
  FGrainMs := AValue;
end;

procedure TSedaiGranularGenerator.SetDensity(AValue: Single);
begin
  if AValue < 0.1 then AValue := 0.1;
  FDensity := AValue;
end;

procedure TSedaiGranularGenerator.LoadSample(ASample: TSedaiAudioBuffer;
  ATakeOwnership: Boolean);
begin
  ClearSample;
  FSource := ASample;
  FOwnsSource := ATakeOwnership;
  Reset;
end;

function TSedaiGranularGenerator.LoadSampleFromFile(const APath: string): Boolean;
var
  Rd: TSedaiAudioFileReader;
  Buf: TSedaiAudioBuffer;
begin
  Result := False;
  Buf := nil;
  Rd := TSedaiAudioFileReader.Create;
  try
    if not (Rd.OpenFile(APath) and Rd.ReadAll(Buf) and (Buf <> nil)) then Exit;
    if Buf.SampleCount < 2 then begin Buf.Free; Exit; end;
    LoadSample(Buf, True);
    Result := True;
  finally
    Rd.Free;
  end;
end;

procedure TSedaiGranularGenerator.ClearSample;
begin
  if FOwnsSource and Assigned(FSource) then FSource.Free;
  FSource := nil;
  FOwnsSource := False;
end;

function TSedaiGranularGenerator.HasSample: Boolean;
begin
  Result := Assigned(FSource) and (FSource.SampleCount > 0);
end;

function TSedaiGranularGenerator.ActiveGrains: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SEDAI_GRAIN_POOL - 1 do
    if FGrains[I].Active then Inc(Result);
end;

// Linear interpolation, and reading past either end gives silence rather than
// wrapping: a grain that ran off the end should fade out, not stitch the
// beginning of the recording onto its tail.
function TSedaiGranularGenerator.SourceAt(APos: Double; AChannel: Integer): Single;
var
  I0, I1, N, Ch: Integer;
  F: Single;
begin
  Result := 0.0;
  if not HasSample then Exit;
  N := FSource.SampleCount;
  if (APos < 0) or (APos >= N - 1) then Exit;
  Ch := AChannel;
  if Ch >= FSource.Channels then Ch := FSource.Channels - 1;
  I0 := Trunc(APos);
  I1 := I0 + 1;
  F := APos - I0;
  Result := FSource.GetSample(Ch, I0) * (1.0 - F) + FSource.GetSample(Ch, I1) * F;
end;

function TSedaiGranularGenerator.WindowAt(AAge, ALen: Integer): Single;
var
  T, Skirt: Single;
begin
  if ALen <= 1 then Exit(0.0);
  // PERIODIC Hann — divided by ALen, not ALen-1. That is what makes two of them
  // half a grain apart sum to exactly 1, and it is the whole reconstruction
  // test. The symmetric form is off by one sample and never quite closes.
  T := AAge / ALen;
  case FWindow of
    gwTriangle:
      if T < 0.5 then Result := 2.0 * T else Result := 2.0 * (1.0 - T);
    gwTukey:
      begin
        Skirt := FTukeySkirt;
        if Skirt <= 0.0 then Exit(1.0);
        if Skirt > 0.5 then Skirt := 0.5;
        if T < Skirt then
          Result := 0.5 * (1.0 - Cos(Pi * T / Skirt))
        else if T > 1.0 - Skirt then
          Result := 0.5 * (1.0 - Cos(Pi * (1.0 - T) / Skirt))
        else
          Result := 1.0;
      end;
  else
    Result := 0.5 * (1.0 - Cos(2.0 * Pi * T));
  end;
end;

function TSedaiGranularGenerator.FreeGrain: Integer;
var
  I: Integer;
begin
  for I := 0 to SEDAI_GRAIN_POOL - 1 do
    if not FGrains[I].Active then Exit(I);
  Result := -1;
end;

procedure TSedaiGranularGenerator.Launch;
var
  G, Len: Integer;
  P, Cents, Pan: Single;
begin
  if not HasSample then Exit;
  G := FreeGrain;
  if G < 0 then
  begin
    Inc(FRefused);
    Exit;
  end;
  Len := Round(FGrainMs * 0.001 * FSampleRate);
  if Len < 2 then Len := 2;

  // Where this grain reads from: the walking head, plus the knob, plus a little
  // randomness. With the knob at zero and no spread it is the head exactly,
  // which is what the reconstruction test needs.
  P := FPosition * FSource.SampleCount;
  if FPosSpread > 0.0 then
    P := P + FRandom.NextBipolar * FPosSpread * FSampleRate;

  Cents := 0.0;
  if FPitchSpread > 0.0 then Cents := FRandom.NextBipolar * FPitchSpread;

  FGrains[G].Active := True;
  FGrains[G].Pos := FHead + P;
  FGrains[G].Step := FPitch * Power(2.0, Cents / 1200.0);
  FGrains[G].Age := 0;
  FGrains[G].Len := Len;
  FGrains[G].Amp := 1.0;

  // Constant power across the image, so widening the spread does not also make
  // the cloud louder in the middle.
  Pan := 0.0;
  if FPanSpread > 0.0 then Pan := FRandom.NextBipolar * FPanSpread;
  FGrains[G].PanL := Cos((Pan + 1.0) * Pi * 0.25);
  FGrains[G].PanR := Sin((Pan + 1.0) * Pi * 0.25);
end;

// The one place a sample is made. Mono is NOT left plus right: constant-power
// panning puts 0.707 in each channel for a centred grain, so summing them would
// give 1.414 — which is the correct answer for two channels and the wrong one
// for a mono output. Mono takes the grain before it is panned.
procedure TSedaiGranularGenerator.RenderOne(out AMono, ALeft, ARight: Single);
var
  I: Integer;
  W, S, SR_, Interval: Single;
begin
  AMono := 0.0; ALeft := 0.0; ARight := 0.0;
  if not HasSample then Exit;

  // Launch whatever is due. A loop, not an `if`, because at high density more
  // than one grain can fall inside a single sample.
  Interval := FSampleRate / FDensity;
  if Interval < 1.0 then Interval := 1.0;
  // Never more than one interval of backlog. This is what makes the first
  // sample launch exactly one grain, and it also stops a density that has just
  // been raised from firing a burst to catch up.
  if FSinceLast > Interval then FSinceLast := Interval;
  FSinceLast := FSinceLast + 1.0;
  while FSinceLast >= Interval do
  begin
    Launch;
    FSinceLast := FSinceLast - Interval;
  end;

  for I := 0 to SEDAI_GRAIN_POOL - 1 do
    if FGrains[I].Active then
    begin
      W := WindowAt(FGrains[I].Age, FGrains[I].Len) * FGrains[I].Amp;
      S := SourceAt(FGrains[I].Pos, 0) * W;
      if FSource.Channels > 1 then SR_ := SourceAt(FGrains[I].Pos, 1) * W
                              else SR_ := S;
      ALeft  := ALeft  + S   * FGrains[I].PanL;
      ARight := ARight + SR_ * FGrains[I].PanR;
      // A stereo source folds down by averaging, not by adding: a grain read
      // from both channels must not be twice as loud in mono.
      if FSource.Channels > 1 then AMono := AMono + (S + SR_) * 0.5
                              else AMono := AMono + S;

      FGrains[I].Pos := FGrains[I].Pos + FGrains[I].Step;
      Inc(FGrains[I].Age);
      if FGrains[I].Age >= FGrains[I].Len then FGrains[I].Active := False;
    end;

  AMono := AMono * FAmplitude;
  ALeft := ALeft * FAmplitude;
  ARight := ARight * FAmplitude;
  FHead := FHead + FSpeed;
  // The head walks the source and comes back to the start. Position is a knob
  // on top of it, so a host can scrub without fighting the walk.
  if HasSample and (FSource.SampleCount > 1) then
  begin
    while FHead >= FSource.SampleCount do FHead := FHead - FSource.SampleCount;
    while FHead < 0 do FHead := FHead + FSource.SampleCount;
  end;
  FLastL := ALeft; FLastR := ARight;
end;

procedure TSedaiGranularGenerator.GenerateStereo(out ALeft, ARight: Single);
var
  M: Single;
begin
  RenderOne(M, ALeft, ARight);
end;

function TSedaiGranularGenerator.GenerateSample: Single;
var
  L, R: Single;
begin
  RenderOne(Result, L, R);
end;

end.
