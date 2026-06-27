{*
 * Sedai Audio Foundation - Karplus-Strong Generator
 *
 * TSedaiKarplusGenerator is a plucked-string physical model: a delay line
 * excited with noise and fed back through an averaging low-pass, which yields a
 * bright attack that decays into a pitched, string-like tone. Damping controls
 * the sustain; blend morphs from a clean string toward a percussive/drum tone.
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiKarplusGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiOscillator;

type
  { TSedaiKarplusGenerator }
  // Plucked-string (Karplus-Strong) generator.
  TSedaiKarplusGenerator = class(TSedaiSignalGenerator)
  private
    FDelayLine: array of Single;
    FDelayLength: Integer;
    FPos: Integer;
    FDamping: Single;       // feedback gain (0.90..0.9995); higher = longer sustain
    FBlend: Single;         // averaging weight (0.5 = pure string)
    FEnergy: Single;        // running level estimate, for end-of-note detection
    FGate: Boolean;

    procedure RebuildDelay;
    procedure Excite(AVelocity: Single);

  public
    constructor Create; override;

    procedure Reset; override;

    // Re-excite the current string (pluck) without changing pitch.
    procedure Pluck(AVelocity: Single = 1.0);

    // Set frequency + pluck (note on) / release (note off).
    procedure NoteOn(AFreq, AVelocity: Single);
    procedure NoteOff;

    function GenerateSample: Single; override;

    // True while the string still carries audible energy.
    function IsRinging: Boolean;

    property Damping: Single read FDamping write FDamping;
    property Blend: Single read FBlend write FBlend;
    property Gate: Boolean read FGate;
  end;

implementation

{ TSedaiKarplusGenerator }

constructor TSedaiKarplusGenerator.Create;
begin
  inherited Create;

  FDelayLength := 0;
  FPos := 0;
  FDamping := 0.996;
  FBlend := 0.5;
  FEnergy := 0.0;
  FGate := False;
  SetLength(FDelayLine, 0);
end;

procedure TSedaiKarplusGenerator.Reset;
var
  I: Integer;
begin
  inherited Reset;

  for I := 0 to High(FDelayLine) do
    FDelayLine[I] := 0.0;
  FPos := 0;
  FEnergy := 0.0;
  FGate := False;
end;

procedure TSedaiKarplusGenerator.RebuildDelay;
var
  Len: Integer;
begin
  if (FFrequency <= 0.0) or (FSampleRate = 0) then
    Len := 0
  else
    Len := Round(FSampleRate / FFrequency);
  if Len < 2 then Len := 2;

  if Len <> FDelayLength then
  begin
    FDelayLength := Len;
    SetLength(FDelayLine, FDelayLength);
  end;
  FPos := 0;
end;

procedure TSedaiKarplusGenerator.Excite(AVelocity: Single);
var
  I: Integer;
begin
  // Fill the delay line with white noise scaled by velocity (the "pluck").
  for I := 0 to FDelayLength - 1 do
    FDelayLine[I] := (Random * 2.0 - 1.0) * AVelocity;
  FPos := 0;
  FEnergy := AVelocity;
end;

procedure TSedaiKarplusGenerator.Pluck(AVelocity: Single);
begin
  if FDelayLength < 2 then
    RebuildDelay;
  if FDelayLength >= 2 then
    Excite(AVelocity);
end;

procedure TSedaiKarplusGenerator.NoteOn(AFreq, AVelocity: Single);
begin
  FFrequency := AFreq;
  RebuildDelay;
  Excite(AVelocity);
  FGate := True;
end;

procedure TSedaiKarplusGenerator.NoteOff;
begin
  FGate := False;
  // The string keeps ringing; the host voice envelope shapes the release.
end;

function TSedaiKarplusGenerator.GenerateSample: Single;
var
  Cur: Single;
  NextPos: Integer;
begin
  if FDelayLength < 2 then
  begin
    Result := 0.0;
    Exit;
  end;

  Cur := FDelayLine[FPos];

  NextPos := FPos + 1;
  if NextPos >= FDelayLength then NextPos := 0;

  // Karplus-Strong update: averaging low-pass + damping feedback.
  FDelayLine[FPos] := FDamping * (FBlend * Cur + (1.0 - FBlend) * FDelayLine[NextPos]);
  FPos := NextPos;

  // Cheap one-pole energy follower for end-of-note detection.
  FEnergy := FEnergy * 0.99995;
  if Abs(Cur) > FEnergy then FEnergy := Abs(Cur);

  Result := Cur * FAmplitude;
end;

function TSedaiKarplusGenerator.IsRinging: Boolean;
begin
  Result := FEnergy > 0.0008;
end;

end.
