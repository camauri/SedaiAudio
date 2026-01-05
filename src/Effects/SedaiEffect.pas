{*
 * Sedai Audio Foundation - Effect Base Class
 *
 * TSedaiEffect is the abstract base class for all audio effects.
 * Provides common functionality like dry/wet mix, bypass, and
 * tempo sync support.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiEffect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiFilter;

type
  { TSedaiEffect }
  // Abstract base class for effects
  TSedaiEffect = class(TSedaiSignalProcessor)
  private
    FTempo: Single;               // Current tempo in BPM
    FTempoSync: Boolean;          // Enable tempo synchronization
    FPreDelay: Single;            // Pre-delay in ms

  protected
    // Helper: calculate delay in samples from ms
    function MsToSamples(AMs: Single): Integer;

    // Helper: calculate delay in samples from beat division
    function BeatToSamples(ABeats: Single): Integer;

    // Helper: linear interpolation
    function Lerp(A, B, T: Single): Single; inline;

    // Helper: cubic interpolation for delay lines
    function CubicInterpolate(Y0, Y1, Y2, Y3, T: Single): Single;

  public
    constructor Create; override;

    // Set tempo (for sync effects)
    procedure SetTempo(ABPM: Single);

    // Properties
    property Tempo: Single read FTempo write SetTempo;
    property TempoSync: Boolean read FTempoSync write FTempoSync;
    property PreDelay: Single read FPreDelay write FPreDelay;
  end;

implementation

{ TSedaiEffect }

constructor TSedaiEffect.Create;
begin
  inherited Create;

  FTempo := 120.0;
  FTempoSync := False;
  FPreDelay := 0.0;
end;

procedure TSedaiEffect.SetTempo(ABPM: Single);
begin
  if ABPM < 20.0 then
    ABPM := 20.0
  else if ABPM > 300.0 then
    ABPM := 300.0;

  FTempo := ABPM;
end;

function TSedaiEffect.MsToSamples(AMs: Single): Integer;
begin
  if FSampleRate > 0 then
    Result := Round(AMs * 0.001 * FSampleRate)
  else
    Result := 0;
end;

function TSedaiEffect.BeatToSamples(ABeats: Single): Integer;
var
  SecondsPerBeat: Single;
begin
  if (FSampleRate > 0) and (FTempo > 0) then
  begin
    SecondsPerBeat := 60.0 / FTempo;
    Result := Round(ABeats * SecondsPerBeat * FSampleRate);
  end
  else
    Result := 0;
end;

function TSedaiEffect.Lerp(A, B, T: Single): Single;
begin
  Result := A + (B - A) * T;
end;

function TSedaiEffect.CubicInterpolate(Y0, Y1, Y2, Y3, T: Single): Single;
var
  A0, A1, A2, A3: Single;
  T2: Single;
begin
  T2 := T * T;

  A0 := Y3 - Y2 - Y0 + Y1;
  A1 := Y0 - Y1 - A0;
  A2 := Y2 - Y0;
  A3 := Y1;

  Result := A0 * T * T2 + A1 * T2 + A2 * T + A3;
end;

end.
