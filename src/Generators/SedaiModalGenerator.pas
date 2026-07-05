{*
 * Sedai Audio Foundation - Modal (struck-percussion) Generator
 *
 * TSedaiModalGenerator synthesises struck/idiophone percussion (bells, mallets,
 * blocks) by MODAL SYNTHESIS: y(t) = sum aₙ·e^(−dₙt)·sin(2π fₙ t). Each mode is a
 * two-pole resonator (poles at radius rₙ = decay, angle 2π fₙ/SR); a strike feeds
 * an impulse into the bank and the modes ring and decay at their own rates. The
 * mode set is the instrument's signature (a bell's inharmonic hum/prime/tierce/
 * nominal; a marimba bar's 1 : 3.9 : 9.5; a woodblock's short high pair). Mode
 * frequencies are ratios of the played note, so a preset is pitched + transposable.
 *
 * Opt-in / inert: with no modes and no strike it is silent.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiModalGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode,
  SedaiOscillator;

type
  TModalKind = (mkBell, mkMarimba, mkTubular, mkWoodblock, mkTom);

  { TSedaiModalGenerator }
  TSedaiModalGenerator = class(TSedaiSignalGenerator)
  private
    FModes: array of record ratio, gain, decay: Single; end;  // decay = seconds to ~-60 dB
    FRes: array of record a1, a2, b0, y1, y2: Single; end;
    FKind: TModalKind;

    FStruckPending: Boolean;
    FStrikeAmp: Single;
    FDamp: Single;               // extra per-sample damping on note-off (1 = none)

    FNote: Integer;
    FGateOpen, FReleasing: Boolean;
    FOutputGain: Single;

    procedure BuildModes;
    procedure UpdateResonators;

  protected
    procedure SetFrequency(AValue: Single); override;

  public
    constructor Create; override;
    procedure SampleRateChanged; override;

    procedure NoteOn(ANote: Integer; AVelocity: Single);
    procedure NoteOff;
    procedure Kill;
    function GenerateSample: Single; override;

    procedure SetKind(AKind: TModalKind);
    procedure SetOutputGain(AGain: Single);

    property Kind: TModalKind read FKind;
    property Note: Integer read FNote;
    property GateOpen: Boolean read FGateOpen;   // True while still ringing
    property Releasing: Boolean read FReleasing;
  end;

implementation

{ TSedaiModalGenerator }

constructor TSedaiModalGenerator.Create;
begin
  inherited Create;
  FKind := mkBell;
  FOutputGain := 0.2;
  FDamp := 1.0;
  FNote := -1;
  FGateOpen := False;
  FReleasing := False;
  FFrequency := 220.0;
  FAmplitude := 1.0;
  BuildModes;
end;

procedure TSedaiModalGenerator.SampleRateChanged;
begin
  inherited SampleRateChanged;
  UpdateResonators;
end;

// signature modes (ratio to the played note, gain, decay seconds)
procedure TSedaiModalGenerator.BuildModes;
  procedure M(idx: Integer; r, g, d: Single);
  begin FModes[idx].ratio := r; FModes[idx].gain := g; FModes[idx].decay := d; end;
begin
  case FKind of
    mkBell:                        // inharmonic bell (minor tierce 1.2 -> "sad" bell)
      begin
        SetLength(FModes, 6);
        M(0, 0.50, 0.40, 3.0);     // hum
        M(1, 1.00, 1.00, 2.6);     // prime
        M(2, 1.19, 0.80, 2.2);     // tierce (minor third)
        M(3, 1.50, 0.55, 1.8);     // quint
        M(4, 2.00, 0.45, 1.4);     // nominal
        M(5, 2.66, 0.30, 1.0);
      end;
    mkMarimba:                     // wooden bar: 1 : ~3.9 : ~9.5, quick decay
      begin
        SetLength(FModes, 3);
        M(0, 1.00, 1.00, 0.55);
        M(1, 3.93, 0.35, 0.30);
        M(2, 9.55, 0.12, 0.16);
      end;
    mkTubular:                     // tubular bell / chime
      begin
        SetLength(FModes, 5);
        M(0, 1.00, 0.90, 4.0);
        M(1, 2.71, 1.00, 3.4);     // the strike-tone partial
        M(2, 5.15, 0.55, 2.6);
        M(3, 8.43, 0.35, 1.8);
        M(4, 12.4, 0.20, 1.2);
      end;
    mkWoodblock:                   // short, high, dry
      begin
        SetLength(FModes, 2);
        M(0, 1.00, 1.00, 0.10);
        M(1, 2.72, 0.55, 0.06);
      end;
    mkTom:                         // membrane-ish low tom
      begin
        SetLength(FModes, 4);
        M(0, 1.00, 1.00, 0.45);
        M(1, 1.59, 0.55, 0.35);
        M(2, 2.14, 0.35, 0.25);
        M(3, 2.30, 0.20, 0.18);
      end;
  else
    SetLength(FModes, 0);
  end;
  UpdateResonators;
end;

// two-pole resonator per mode: poles at radius r=exp(-1/(decay*SR)*6.9) angle w.
procedure TSedaiModalGenerator.UpdateResonators;
var i: Integer; w, r, f: Single;
begin
  SetLength(FRes, Length(FModes));
  for i := 0 to High(FModes) do
  begin
    f := FFrequency * FModes[i].ratio;
    if (f <= 0) or (f >= FSampleRate * 0.49) or (FModes[i].decay <= 0) then
    begin
      FRes[i].a1 := 0; FRes[i].a2 := 0; FRes[i].b0 := 0;
      Continue;
    end;
    w := 2 * Pi * f / FSampleRate;
    // decay = time to -60 dB (1e-3): r^(decay*SR) = 1e-3 -> r = exp(ln(1e-3)/(decay*SR))
    r := Exp(Ln(1e-3) / (FModes[i].decay * FSampleRate));
    if r > 0.99999 then r := 0.99999;
    FRes[i].a1 := 2 * r * Cos(w);
    FRes[i].a2 := -(r * r);
    FRes[i].b0 := FModes[i].gain * Sin(w);   // sine-excited impulse response
  end;
end;

procedure TSedaiModalGenerator.SetFrequency(AValue: Single);
begin
  inherited SetFrequency(AValue);
  UpdateResonators;
end;

procedure TSedaiModalGenerator.NoteOn(ANote: Integer; AVelocity: Single);
var i: Integer;
begin
  FNote := ANote;
  FFrequency := 440 * Power(2, (ANote - 69) / 12);
  UpdateResonators;
  for i := 0 to High(FRes) do begin FRes[i].y1 := 0; FRes[i].y2 := 0; end;
  FStruckPending := True;
  FStrikeAmp := EnsureRange(AVelocity, 0, 1);
  FDamp := 1.0;
  FGateOpen := True;
  FReleasing := False;
end;

// A key release damps the ring (like a hand on a bell) but does not cut it dead.
procedure TSedaiModalGenerator.NoteOff;
begin
  if not FGateOpen then Exit;
  FReleasing := True;
  if (FSampleRate > 0) then FDamp := Exp(-1.0 / (0.4 * FSampleRate)) else FDamp := 1.0;
end;

procedure TSedaiModalGenerator.Kill;
var i: Integer;
begin
  FGateOpen := False;
  FReleasing := False;
  FNote := -1;
  FStruckPending := False;
  for i := 0 to High(FRes) do begin FRes[i].y1 := 0; FRes[i].y2 := 0; end;
end;

function TSedaiModalGenerator.GenerateSample: Single;
var
  i: Integer;
  x, y, sum, peak: Single;
begin
  if not FGateOpen then begin Result := 0; Exit; end;

  if FStruckPending then begin x := FStrikeAmp; FStruckPending := False; end
  else x := 0;

  sum := 0; peak := 0;
  for i := 0 to High(FRes) do
  begin
    y := FRes[i].b0 * x + FRes[i].a1 * FRes[i].y1 + FRes[i].a2 * FRes[i].y2;
    FRes[i].y2 := FRes[i].y1; FRes[i].y1 := y;
    if FReleasing then
    begin
      FRes[i].y1 := FRes[i].y1 * FDamp; FRes[i].y2 := FRes[i].y2 * FDamp;
    end;
    sum := sum + y;
    if Abs(FRes[i].y1) > peak then peak := Abs(FRes[i].y1);
  end;

  // the voice is done once every mode has rung out
  if (not FStruckPending) and (peak < 2e-5) then FGateOpen := False;

  Result := FOutputGain * sum * FAmplitude;
end;

procedure TSedaiModalGenerator.SetKind(AKind: TModalKind);
begin
  FKind := AKind;
  BuildModes;
end;

procedure TSedaiModalGenerator.SetOutputGain(AGain: Single);
begin
  FOutputGain := Max(0, AGain);
end;

end.
