{*
 * Sedai Audio Foundation - Low Frequency Oscillator
 *
 * TSedaiLFO provides tempo-syncable low frequency oscillator with
 * multiple waveforms, phase offset, and smooth fade-in option.
 * Output range is configurable (unipolar or bipolar).
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiLFO;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiEnvelope;

type
  // LFO sync mode
  TLFOSyncMode = (
    lsmFree,          // Free-running (Hz-based rate)
    lsmTempo,         // Tempo-synced (beat divisions)
    lsmKeySync        // Reset phase on key trigger
  );

  // Beat division for tempo sync
  TLFOBeatDivision = (
    lbd4Bars,         // 4 bars (16 beats)
    lbd2Bars,         // 2 bars (8 beats)
    lbd1Bar,          // 1 bar (4 beats)
    lbd1_2,           // Half note (2 beats)
    lbd1_4,           // Quarter note (1 beat)
    lbd1_8,           // Eighth note (1/2 beat)
    lbd1_16,          // Sixteenth note (1/4 beat)
    lbd1_32,          // Thirty-second note (1/8 beat)
    lbd1_4T,          // Quarter triplet (2/3 beat)
    lbd1_8T,          // Eighth triplet (1/3 beat)
    lbd1_16T,         // Sixteenth triplet (1/6 beat)
    lbd1_4D,          // Dotted quarter (1.5 beats)
    lbd1_8D,          // Dotted eighth (0.75 beats)
    lbd1_16D          // Dotted sixteenth (0.375 beats)
  );

  { TSedaiLFO }
  // Low frequency oscillator for modulation
  TSedaiLFO = class(TSedaiModulator)
  private
    // Waveform and rate
    FWaveform: TWaveformType;
    FRate: Single;                // Rate in Hz (free mode) or beat division (tempo mode)
    FBeatDivision: TLFOBeatDivision;

    // Phase
    FPhase: Single;               // Current phase (0.0 - 1.0)
    FPhaseOffset: Single;         // Phase offset (0.0 - 1.0)
    FPhaseIncrement: Single;      // Phase increment per sample

    // Sync
    FSyncMode: TLFOSyncMode;
    FTempo: Single;               // Current tempo in BPM (for tempo sync)

    // Output range
    FBipolar: Boolean;            // True = -1..+1, False = 0..+1
    FAmplitude: Single;           // Output amplitude (0.0 - 1.0)

    // Fade-in
    FFadeInEnabled: Boolean;
    FFadeInTime: Single;          // Fade-in time in seconds
    FFadeInLevel: Single;         // Current fade-in level (0-1)
    FFadeInIncrement: Single;     // Fade-in increment per sample

    // Sample & Hold state
    FSampleHoldValue: Single;     // Held value for S&H waveform
    FPreviousPhase: Single;       // For detecting phase wrap

    procedure SetRate(AValue: Single);
    procedure SetBeatDivision(AValue: TLFOBeatDivision);
    procedure SetTempo(AValue: Single);
    procedure SetWaveform(AValue: TWaveformType);
    procedure SetFadeInTime(AValue: Single);

    procedure UpdatePhaseIncrement;
    function BeatDivisionToBeats(ADivision: TLFOBeatDivision): Single;

    // Waveform generators
    function GenerateSine: Single;
    function GenerateSawtooth: Single;
    function GenerateSquare: Single;
    function GenerateTriangle: Single;
    function GenerateSampleHold: Single;

  public
    constructor Create; override;

    // Reset LFO state
    procedure Reset; override;

    // Reset phase (for key sync)
    procedure ResetPhase;

    // Trigger (for key sync mode)
    procedure Trigger;

    // Process one sample
    function Process: Single; override;

    // Get current output without advancing
    function GetOutput: Single;

    // Properties - Waveform and Rate
    property Waveform: TWaveformType read FWaveform write SetWaveform;
    property Rate: Single read FRate write SetRate;
    property BeatDivision: TLFOBeatDivision read FBeatDivision write SetBeatDivision;

    // Properties - Phase
    property Phase: Single read FPhase write FPhase;
    property PhaseOffset: Single read FPhaseOffset write FPhaseOffset;

    // Properties - Sync
    property SyncMode: TLFOSyncMode read FSyncMode write FSyncMode;
    property Tempo: Single read FTempo write SetTempo;

    // Properties - Output
    property Bipolar: Boolean read FBipolar write FBipolar;
    property Amplitude: Single read FAmplitude write FAmplitude;

    // Properties - Fade-in
    property FadeInEnabled: Boolean read FFadeInEnabled write FFadeInEnabled;
    property FadeInTime: Single read FFadeInTime write SetFadeInTime;
  end;

implementation

{ TSedaiLFO }

constructor TSedaiLFO.Create;
begin
  inherited Create;

  FWaveform := wtSine;
  FRate := 1.0;                   // 1 Hz default
  FBeatDivision := lbd1_4;        // Quarter note

  FPhase := 0.0;
  FPhaseOffset := 0.0;
  FPhaseIncrement := 0.0;

  FSyncMode := lsmFree;
  FTempo := 120.0;

  FBipolar := True;
  FAmplitude := 1.0;

  FFadeInEnabled := False;
  FFadeInTime := 0.5;
  FFadeInLevel := 1.0;
  FFadeInIncrement := 0.0;

  FSampleHoldValue := 0.0;
  FPreviousPhase := 0.0;

  UpdatePhaseIncrement;
end;

procedure TSedaiLFO.Reset;
begin
  inherited Reset;

  FPhase := 0.0;
  FFadeInLevel := 0.0;
  FSampleHoldValue := 0.0;
  FPreviousPhase := 0.0;
end;

procedure TSedaiLFO.SetRate(AValue: Single);
begin
  if AValue < 0.01 then
    AValue := 0.01
  else if AValue > 100.0 then
    AValue := 100.0;

  FRate := AValue;
  UpdatePhaseIncrement;
end;

procedure TSedaiLFO.SetBeatDivision(AValue: TLFOBeatDivision);
begin
  FBeatDivision := AValue;
  UpdatePhaseIncrement;
end;

procedure TSedaiLFO.SetTempo(AValue: Single);
begin
  if AValue < 20.0 then
    AValue := 20.0
  else if AValue > 300.0 then
    AValue := 300.0;

  FTempo := AValue;
  UpdatePhaseIncrement;
end;

procedure TSedaiLFO.SetWaveform(AValue: TWaveformType);
begin
  FWaveform := AValue;
end;

procedure TSedaiLFO.SetFadeInTime(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 10.0 then
    AValue := 10.0;

  FFadeInTime := AValue;

  // Calculate fade-in increment
  if (FFadeInTime > 0) and (FSampleRate > 0) then
    FFadeInIncrement := 1.0 / (FFadeInTime * FSampleRate)
  else
    FFadeInIncrement := 1.0;  // Instant
end;

procedure TSedaiLFO.UpdatePhaseIncrement;
var
  EffectiveRate: Single;
begin
  if FSampleRate = 0 then
  begin
    FPhaseIncrement := 0.0;
    Exit;
  end;

  case FSyncMode of
    lsmFree, lsmKeySync:
      EffectiveRate := FRate;

    lsmTempo:
      begin
        // Calculate rate from tempo and beat division
        // Rate = Tempo / 60 / Beats (cycles per second)
        EffectiveRate := (FTempo / 60.0) / BeatDivisionToBeats(FBeatDivision);
      end;

    else
      EffectiveRate := FRate;
  end;

  FPhaseIncrement := EffectiveRate / FSampleRate;
end;

function TSedaiLFO.BeatDivisionToBeats(ADivision: TLFOBeatDivision): Single;
begin
  case ADivision of
    lbd4Bars:  Result := 16.0;
    lbd2Bars:  Result := 8.0;
    lbd1Bar:   Result := 4.0;
    lbd1_2:    Result := 2.0;
    lbd1_4:    Result := 1.0;
    lbd1_8:    Result := 0.5;
    lbd1_16:   Result := 0.25;
    lbd1_32:   Result := 0.125;
    lbd1_4T:   Result := 2.0 / 3.0;
    lbd1_8T:   Result := 1.0 / 3.0;
    lbd1_16T:  Result := 1.0 / 6.0;
    lbd1_4D:   Result := 1.5;
    lbd1_8D:   Result := 0.75;
    lbd1_16D:  Result := 0.375;
    else       Result := 1.0;
  end;
end;

function TSedaiLFO.GenerateSine: Single;
var
  EffectivePhase: Single;
begin
  EffectivePhase := FPhase + FPhaseOffset;
  if EffectivePhase >= 1.0 then
    EffectivePhase := EffectivePhase - 1.0;

  Result := Sin(EffectivePhase * 2.0 * PI);
end;

function TSedaiLFO.GenerateSawtooth: Single;
var
  EffectivePhase: Single;
begin
  EffectivePhase := FPhase + FPhaseOffset;
  if EffectivePhase >= 1.0 then
    EffectivePhase := EffectivePhase - 1.0;

  // Rising sawtooth: -1 to +1
  Result := 2.0 * EffectivePhase - 1.0;
end;

function TSedaiLFO.GenerateSquare: Single;
var
  EffectivePhase: Single;
begin
  EffectivePhase := FPhase + FPhaseOffset;
  if EffectivePhase >= 1.0 then
    EffectivePhase := EffectivePhase - 1.0;

  if EffectivePhase < 0.5 then
    Result := 1.0
  else
    Result := -1.0;
end;

function TSedaiLFO.GenerateTriangle: Single;
var
  EffectivePhase: Single;
begin
  EffectivePhase := FPhase + FPhaseOffset;
  if EffectivePhase >= 1.0 then
    EffectivePhase := EffectivePhase - 1.0;

  if EffectivePhase < 0.5 then
    Result := 4.0 * EffectivePhase - 1.0
  else
    Result := 3.0 - 4.0 * EffectivePhase;
end;

function TSedaiLFO.GenerateSampleHold: Single;
var
  EffectivePhase: Single;
begin
  EffectivePhase := FPhase + FPhaseOffset;
  if EffectivePhase >= 1.0 then
    EffectivePhase := EffectivePhase - 1.0;

  // Sample new random value when phase wraps
  if EffectivePhase < FPreviousPhase then
    FSampleHoldValue := Random * 2.0 - 1.0;

  FPreviousPhase := EffectivePhase;
  Result := FSampleHoldValue;
end;

procedure TSedaiLFO.ResetPhase;
begin
  FPhase := 0.0;
  FPreviousPhase := 0.0;
end;

procedure TSedaiLFO.Trigger;
begin
  if FSyncMode = lsmKeySync then
    ResetPhase;

  // Reset fade-in
  if FFadeInEnabled then
    FFadeInLevel := 0.0;
end;

function TSedaiLFO.Process: Single;
var
  RawOutput: Single;
begin
  // Generate waveform
  case FWaveform of
    wtSine:      RawOutput := GenerateSine;
    wtSawtooth:  RawOutput := GenerateSawtooth;
    wtSquare:    RawOutput := GenerateSquare;
    wtTriangle:  RawOutput := GenerateTriangle;
    wtNoise:     RawOutput := GenerateSampleHold;  // S&H for LFO
    else         RawOutput := 0.0;
  end;

  // Apply amplitude
  RawOutput := RawOutput * FAmplitude;

  // Apply fade-in
  if FFadeInEnabled and (FFadeInLevel < 1.0) then
  begin
    RawOutput := RawOutput * FFadeInLevel;
    FFadeInLevel := FFadeInLevel + FFadeInIncrement;
    if FFadeInLevel > 1.0 then
      FFadeInLevel := 1.0;
  end;

  // Convert to unipolar if needed
  if not FBipolar then
    RawOutput := (RawOutput + 1.0) * 0.5;

  FOutput := RawOutput;

  // Advance phase
  FPhase := FPhase + FPhaseIncrement;
  if FPhase >= 1.0 then
    FPhase := FPhase - 1.0;

  Result := FOutput;
end;

function TSedaiLFO.GetOutput: Single;
begin
  Result := FOutput;
end;

end.
