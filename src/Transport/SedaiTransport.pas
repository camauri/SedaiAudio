{*
 * Sedai Audio Foundation - Transport
 *
 * TSedaiTransport provides timeline-based transport control
 * with tempo, time signature, locators, and loop points.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiTransport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject;

type
  // Transport state
  TTransportState = (
    tsStopped,
    tsPlaying,
    tsRecording,
    tsPaused
  );

  // Time display format
  TTimeFormat = (
    tfBarsBeats,          // Bars:Beats:Ticks
    tfMinSec,             // Minutes:Seconds:Milliseconds
    tfSamples,            // Sample position
    tfSMPTE                // SMPTE timecode
  );

  // SMPTE frame rate
  TSMPTEFrameRate = (
    smpte24,              // 24 fps (film)
    smpte25,              // 25 fps (PAL)
    smpte2997,            // 29.97 fps (NTSC drop-frame)
    smpte2997nd,          // 29.97 fps (NTSC non-drop)
    smpte30               // 30 fps
  );

  // Locator/marker
  TTransportLocator = record
    Position: Int64;      // Sample position
    Name: string;
    Color: Cardinal;
  end;

  // Time signature
  TTimeSignature = record
    Numerator: Integer;   // Beats per bar
    Denominator: Integer; // Beat unit (4 = quarter, 8 = eighth)
  end;

  // Tempo change event
  TTempoChange = record
    Position: Int64;      // Sample position
    Tempo: Single;        // BPM
    Curve: Single;        // Ramp curve (0 = instant, 1 = linear)
  end;

  { TSedaiTransport }
  // Timeline transport control
  TSedaiTransport = class(TSedaiAudioObject)
  private
    // Transport state
    FState: TTransportState;
    FPosition: Int64;             // Current position in samples
    FLength: Int64;               // Project length in samples

    // Tempo
    FTempo: Single;               // Current tempo in BPM
    FBaseTempo: Single;           // Base tempo (before automation)

    // Time signature
    FTimeSignature: TTimeSignature;
    FTicksPerBeat: Integer;       // Resolution (PPQ)

    // Loop
    FLoopEnabled: Boolean;
    FLoopStart: Int64;
    FLoopEnd: Int64;

    // Punch in/out
    FPunchEnabled: Boolean;
    FPunchIn: Int64;
    FPunchOut: Int64;

    // Pre-roll/Post-roll
    FPreRollBars: Integer;
    FPostRollBars: Integer;

    // Locators
    FLocators: array of TTransportLocator;
    FLocatorCount: Integer;

    // Tempo map
    FTempoChanges: array of TTempoChange;
    FTempoChangeCount: Integer;

    // Metronome
    FMetronomeEnabled: Boolean;
    FMetronomeVolume: Single;
    FCountIn: Boolean;            // Count-in before record
    FCountInBars: Integer;

    // SMPTE
    FSMPTEOffset: Int64;          // SMPTE start offset
    FSMPTEFrameRate: TSMPTEFrameRate;

    // Display
    FTimeFormat: TTimeFormat;

    // Callbacks
    FOnPositionChange: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnTempoChange: TNotifyEvent;

    procedure SetPosition(AValue: Int64);
    procedure SetTempo(AValue: Single);
    procedure SetLoopStart(AValue: Int64);
    procedure SetLoopEnd(AValue: Int64);
    function GetSamplesPerBeat: Double;
    function GetSamplesPerBar: Double;
    function GetSamplesPerTick: Double;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // ========================================================================
    // TRANSPORT CONTROL
    // ========================================================================

    procedure Play;
    procedure Stop;
    procedure Pause;
    procedure Record_;
    procedure TogglePlay;
    procedure Rewind;               // Go to start
    procedure FastForward;          // Go to end

    // Move by musical units
    procedure MoveByBars(ABars: Integer);
    procedure MoveByBeats(ABeats: Integer);
    procedure MoveToBar(ABar: Integer);
    procedure MoveToNextLocator;
    procedure MoveToPrevLocator;

    // ========================================================================
    // TEMPO AND TIME
    // ========================================================================

    // Add tempo change at position
    procedure AddTempoChange(APosition: Int64; ATempo: Single; ACurve: Single = 0);

    // Remove tempo change
    procedure RemoveTempoChange(AIndex: Integer);

    // Get tempo at position
    function GetTempoAtPosition(APosition: Int64): Single;

    // Set time signature
    procedure SetTimeSignature(ANumerator, ADenominator: Integer);

    // ========================================================================
    // POSITION CONVERSION
    // ========================================================================

    // Sample to bars/beats/ticks
    procedure SampleToBarsBeatsTicks(ASample: Int64; out ABar, ABeat, ATick: Integer);

    // Bars/beats/ticks to sample
    function BarsBeatsTicksToSample(ABar, ABeat, ATick: Integer): Int64;

    // Sample to time (seconds)
    function SampleToSeconds(ASample: Int64): Double;

    // Time to sample
    function SecondsToSample(ASeconds: Double): Int64;

    // Sample to SMPTE
    procedure SampleToSMPTE(ASample: Int64; out AHours, AMinutes, ASeconds, AFrames: Integer);

    // SMPTE to sample
    function SMPTEToSample(AHours, AMinutes, ASeconds, AFrames: Integer): Int64;

    // Get position string
    function GetPositionString: string;
    function GetPositionStringAt(ASample: Int64): string;

    // ========================================================================
    // LOCATORS
    // ========================================================================

    function AddLocator(APosition: Int64; AName: string; AColor: Cardinal = 0): Integer;
    procedure RemoveLocator(AIndex: Integer);
    procedure ClearLocators;
    function GetLocator(AIndex: Integer): TTransportLocator;
    function FindNearestLocator(APosition: Int64): Integer;

    // ========================================================================
    // PROCESSING
    // ========================================================================

    // Advance position by samples (call from audio callback)
    procedure AdvanceSamples(ASamples: Integer);

    // Check if position is in loop region
    function IsInLoop(APosition: Int64): Boolean;

    // Check if recording is active at position
    function IsRecordingAt(APosition: Int64): Boolean;

    // Get next transport event (loop point, punch point, etc.)
    function GetNextEventPosition: Int64;

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    property State: TTransportState read FState;
    property Position: Int64 read FPosition write SetPosition;
    property Length: Int64 read FLength write FLength;

    property Tempo: Single read FTempo write SetTempo;
    property BaseTempo: Single read FBaseTempo write FBaseTempo;
    property TimeSignature: TTimeSignature read FTimeSignature;
    property TicksPerBeat: Integer read FTicksPerBeat write FTicksPerBeat;

    property LoopEnabled: Boolean read FLoopEnabled write FLoopEnabled;
    property LoopStart: Int64 read FLoopStart write SetLoopStart;
    property LoopEnd: Int64 read FLoopEnd write SetLoopEnd;

    property PunchEnabled: Boolean read FPunchEnabled write FPunchEnabled;
    property PunchIn: Int64 read FPunchIn write FPunchIn;
    property PunchOut: Int64 read FPunchOut write FPunchOut;

    property PreRollBars: Integer read FPreRollBars write FPreRollBars;
    property PostRollBars: Integer read FPostRollBars write FPostRollBars;

    property MetronomeEnabled: Boolean read FMetronomeEnabled write FMetronomeEnabled;
    property MetronomeVolume: Single read FMetronomeVolume write FMetronomeVolume;
    property CountIn: Boolean read FCountIn write FCountIn;
    property CountInBars: Integer read FCountInBars write FCountInBars;

    property SMPTEOffset: Int64 read FSMPTEOffset write FSMPTEOffset;
    property SMPTEFrameRate: TSMPTEFrameRate read FSMPTEFrameRate write FSMPTEFrameRate;

    property TimeFormat: TTimeFormat read FTimeFormat write FTimeFormat;
    property LocatorCount: Integer read FLocatorCount;
    property TempoChangeCount: Integer read FTempoChangeCount;

    property OnPositionChange: TNotifyEvent read FOnPositionChange write FOnPositionChange;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnTempoChange: TNotifyEvent read FOnTempoChange write FOnTempoChange;
  end;

implementation

{ TSedaiTransport }

constructor TSedaiTransport.Create;
begin
  inherited Create;

  FState := tsStopped;
  FPosition := 0;
  FLength := Round(FSampleRate * 300);  // 5 minutes default

  FTempo := 120.0;
  FBaseTempo := 120.0;

  FTimeSignature.Numerator := 4;
  FTimeSignature.Denominator := 4;
  FTicksPerBeat := 960;  // High resolution

  FLoopEnabled := False;
  FLoopStart := 0;
  FLoopEnd := Round(FSampleRate * 8);  // 8 seconds

  FPunchEnabled := False;
  FPunchIn := 0;
  FPunchOut := Round(FSampleRate * 8);

  FPreRollBars := 1;
  FPostRollBars := 0;

  FLocatorCount := 0;
  FTempoChangeCount := 0;

  FMetronomeEnabled := False;
  FMetronomeVolume := 0.7;
  FCountIn := False;
  FCountInBars := 1;

  FSMPTEOffset := 0;
  FSMPTEFrameRate := smpte25;

  FTimeFormat := tfBarsBeats;
end;

destructor TSedaiTransport.Destroy;
begin
  SetLength(FLocators, 0);
  SetLength(FTempoChanges, 0);
  inherited Destroy;
end;

procedure TSedaiTransport.Reset;
begin
  inherited Reset;

  FState := tsStopped;
  FPosition := 0;
end;

procedure TSedaiTransport.SetPosition(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if AValue > FLength then AValue := FLength;

  if FPosition <> AValue then
  begin
    FPosition := AValue;
    if Assigned(FOnPositionChange) then
      FOnPositionChange(Self);
  end;
end;

procedure TSedaiTransport.SetTempo(AValue: Single);
begin
  if AValue < 20.0 then AValue := 20.0;
  if AValue > 300.0 then AValue := 300.0;

  if FTempo <> AValue then
  begin
    FTempo := AValue;
    if Assigned(FOnTempoChange) then
      FOnTempoChange(Self);
  end;
end;

procedure TSedaiTransport.SetLoopStart(AValue: Int64);
begin
  if AValue < 0 then AValue := 0;
  if AValue >= FLoopEnd then AValue := FLoopEnd - 1;
  FLoopStart := AValue;
end;

procedure TSedaiTransport.SetLoopEnd(AValue: Int64);
begin
  if AValue <= FLoopStart then AValue := FLoopStart + 1;
  if AValue > FLength then AValue := FLength;
  FLoopEnd := AValue;
end;

function TSedaiTransport.GetSamplesPerBeat: Double;
begin
  if FTempo > 0 then
    Result := (60.0 / FTempo) * FSampleRate
  else
    Result := FSampleRate;
end;

function TSedaiTransport.GetSamplesPerBar: Double;
begin
  Result := GetSamplesPerBeat * FTimeSignature.Numerator;
end;

function TSedaiTransport.GetSamplesPerTick: Double;
begin
  if FTicksPerBeat > 0 then
    Result := GetSamplesPerBeat / FTicksPerBeat
  else
    Result := GetSamplesPerBeat / 960;
end;

procedure TSedaiTransport.Play;
begin
  if FState = tsStopped then
    FPosition := 0;

  FState := tsPlaying;

  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TSedaiTransport.Stop;
begin
  FState := tsStopped;
  FPosition := 0;

  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TSedaiTransport.Pause;
begin
  if FState = tsPlaying then
    FState := tsPaused
  else if FState = tsPaused then
    FState := tsPlaying;

  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TSedaiTransport.Record_;
begin
  FState := tsRecording;

  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TSedaiTransport.TogglePlay;
begin
  if FState = tsPlaying then
    Pause
  else
    Play;
end;

procedure TSedaiTransport.Rewind;
begin
  SetPosition(0);
end;

procedure TSedaiTransport.FastForward;
begin
  SetPosition(FLength);
end;

procedure TSedaiTransport.MoveByBars(ABars: Integer);
var
  NewPos: Int64;
begin
  NewPos := FPosition + Round(ABars * GetSamplesPerBar);
  SetPosition(NewPos);
end;

procedure TSedaiTransport.MoveByBeats(ABeats: Integer);
var
  NewPos: Int64;
begin
  NewPos := FPosition + Round(ABeats * GetSamplesPerBeat);
  SetPosition(NewPos);
end;

procedure TSedaiTransport.MoveToBar(ABar: Integer);
begin
  if ABar < 1 then ABar := 1;
  SetPosition(Round((ABar - 1) * GetSamplesPerBar));
end;

procedure TSedaiTransport.MoveToNextLocator;
var
  I: Integer;
  NearestPos: Int64;
begin
  NearestPos := FLength;

  for I := 0 to FLocatorCount - 1 do
  begin
    if (FLocators[I].Position > FPosition) and
       (FLocators[I].Position < NearestPos) then
      NearestPos := FLocators[I].Position;
  end;

  SetPosition(NearestPos);
end;

procedure TSedaiTransport.MoveToPrevLocator;
var
  I: Integer;
  NearestPos: Int64;
begin
  NearestPos := 0;

  for I := 0 to FLocatorCount - 1 do
  begin
    if (FLocators[I].Position < FPosition) and
       (FLocators[I].Position > NearestPos) then
      NearestPos := FLocators[I].Position;
  end;

  SetPosition(NearestPos);
end;

procedure TSedaiTransport.AddTempoChange(APosition: Int64; ATempo: Single; ACurve: Single);
begin
  Inc(FTempoChangeCount);
  SetLength(FTempoChanges, FTempoChangeCount);

  FTempoChanges[FTempoChangeCount - 1].Position := APosition;
  FTempoChanges[FTempoChangeCount - 1].Tempo := ATempo;
  FTempoChanges[FTempoChangeCount - 1].Curve := ACurve;
end;

procedure TSedaiTransport.RemoveTempoChange(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FTempoChangeCount) then Exit;

  for I := AIndex to FTempoChangeCount - 2 do
    FTempoChanges[I] := FTempoChanges[I + 1];

  Dec(FTempoChangeCount);
  SetLength(FTempoChanges, FTempoChangeCount);
end;

function TSedaiTransport.GetTempoAtPosition(APosition: Int64): Single;
var
  I: Integer;
  LastTempo: Single;
begin
  LastTempo := FBaseTempo;

  for I := 0 to FTempoChangeCount - 1 do
  begin
    if FTempoChanges[I].Position <= APosition then
      LastTempo := FTempoChanges[I].Tempo
    else
      Break;
  end;

  Result := LastTempo;
end;

procedure TSedaiTransport.SetTimeSignature(ANumerator, ADenominator: Integer);
begin
  if ANumerator < 1 then ANumerator := 1;
  if ANumerator > 32 then ANumerator := 32;

  // Denominator must be power of 2
  if ADenominator < 2 then ADenominator := 2;
  if ADenominator > 32 then ADenominator := 32;

  FTimeSignature.Numerator := ANumerator;
  FTimeSignature.Denominator := ADenominator;
end;

procedure TSedaiTransport.SampleToBarsBeatsTicks(ASample: Int64;
  out ABar, ABeat, ATick: Integer);
var
  SamplesPerBar, SamplesPerBeat, SamplesPerTick: Double;
  TotalTicks, TicksPerBar: Int64;
begin
  SamplesPerBeat := GetSamplesPerBeat;
  SamplesPerBar := SamplesPerBeat * FTimeSignature.Numerator;
  SamplesPerTick := SamplesPerBeat / FTicksPerBeat;

  if SamplesPerTick > 0 then
  begin
    TotalTicks := Round(ASample / SamplesPerTick);
    TicksPerBar := FTicksPerBeat * FTimeSignature.Numerator;

    ABar := TotalTicks div TicksPerBar + 1;
    ABeat := (TotalTicks mod TicksPerBar) div FTicksPerBeat + 1;
    ATick := TotalTicks mod FTicksPerBeat;
  end
  else
  begin
    ABar := 1;
    ABeat := 1;
    ATick := 0;
  end;
end;

function TSedaiTransport.BarsBeatsTicksToSample(ABar, ABeat, ATick: Integer): Int64;
var
  TotalTicks: Int64;
  SamplesPerTick: Double;
begin
  TotalTicks := (ABar - 1) * FTimeSignature.Numerator * FTicksPerBeat +
                (ABeat - 1) * FTicksPerBeat + ATick;

  SamplesPerTick := GetSamplesPerTick;
  Result := Round(TotalTicks * SamplesPerTick);
end;

function TSedaiTransport.SampleToSeconds(ASample: Int64): Double;
begin
  if FSampleRate > 0 then
    Result := ASample / FSampleRate
  else
    Result := 0;
end;

function TSedaiTransport.SecondsToSample(ASeconds: Double): Int64;
begin
  Result := Round(ASeconds * FSampleRate);
end;

procedure TSedaiTransport.SampleToSMPTE(ASample: Int64;
  out AHours, AMinutes, ASeconds, AFrames: Integer);
var
  FrameRate: Double;
  TotalFrames: Int64;
  SMPTESample: Int64;
begin
  SMPTESample := ASample + FSMPTEOffset;

  case FSMPTEFrameRate of
    smpte24: FrameRate := 24.0;
    smpte25: FrameRate := 25.0;
    smpte2997, smpte2997nd: FrameRate := 29.97;
    smpte30: FrameRate := 30.0;
    else FrameRate := 25.0;
  end;

  TotalFrames := Round(SMPTESample / FSampleRate * FrameRate);

  AFrames := TotalFrames mod Round(FrameRate);
  TotalFrames := TotalFrames div Round(FrameRate);

  ASeconds := TotalFrames mod 60;
  TotalFrames := TotalFrames div 60;

  AMinutes := TotalFrames mod 60;
  AHours := TotalFrames div 60;
end;

function TSedaiTransport.SMPTEToSample(AHours, AMinutes, ASeconds, AFrames: Integer): Int64;
var
  FrameRate: Double;
  TotalFrames: Int64;
begin
  case FSMPTEFrameRate of
    smpte24: FrameRate := 24.0;
    smpte25: FrameRate := 25.0;
    smpte2997, smpte2997nd: FrameRate := 29.97;
    smpte30: FrameRate := 30.0;
    else FrameRate := 25.0;
  end;

  TotalFrames := AFrames +
                 ASeconds * Round(FrameRate) +
                 AMinutes * 60 * Round(FrameRate) +
                 AHours * 3600 * Round(FrameRate);

  Result := Round(TotalFrames / FrameRate * FSampleRate) - FSMPTEOffset;
end;

function TSedaiTransport.GetPositionString: string;
begin
  Result := GetPositionStringAt(FPosition);
end;

function TSedaiTransport.GetPositionStringAt(ASample: Int64): string;
var
  Bar, Beat, Tick: Integer;
  Hours, Minutes, Secs, Frames: Integer;
  TotalSeconds: Double;
begin
  case FTimeFormat of
    tfBarsBeats:
      begin
        SampleToBarsBeatsTicks(ASample, Bar, Beat, Tick);
        Result := Format('%d.%d.%03d', [Bar, Beat, Tick]);
      end;

    tfMinSec:
      begin
        TotalSeconds := SampleToSeconds(ASample);
        Minutes := Trunc(TotalSeconds) div 60;
        Secs := Trunc(TotalSeconds) mod 60;
        Result := Format('%d:%02d.%03d', [Minutes, Secs,
                         Round((TotalSeconds - Trunc(TotalSeconds)) * 1000)]);
      end;

    tfSamples:
      Result := IntToStr(ASample);

    tfSMPTE:
      begin
        SampleToSMPTE(ASample, Hours, Minutes, Secs, Frames);
        Result := Format('%02d:%02d:%02d:%02d', [Hours, Minutes, Secs, Frames]);
      end;

    else
      Result := IntToStr(ASample);
  end;
end;

function TSedaiTransport.AddLocator(APosition: Int64; AName: string;
  AColor: Cardinal): Integer;
begin
  Result := FLocatorCount;
  Inc(FLocatorCount);
  SetLength(FLocators, FLocatorCount);

  FLocators[Result].Position := APosition;
  FLocators[Result].Name := AName;
  FLocators[Result].Color := AColor;
end;

procedure TSedaiTransport.RemoveLocator(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FLocatorCount) then Exit;

  for I := AIndex to FLocatorCount - 2 do
    FLocators[I] := FLocators[I + 1];

  Dec(FLocatorCount);
  SetLength(FLocators, FLocatorCount);
end;

procedure TSedaiTransport.ClearLocators;
begin
  FLocatorCount := 0;
  SetLength(FLocators, 0);
end;

function TSedaiTransport.GetLocator(AIndex: Integer): TTransportLocator;
begin
  if (AIndex >= 0) and (AIndex < FLocatorCount) then
    Result := FLocators[AIndex]
  else
  begin
    Result.Position := 0;
    Result.Name := '';
    Result.Color := 0;
  end;
end;

function TSedaiTransport.FindNearestLocator(APosition: Int64): Integer;
var
  I: Integer;
  MinDist, Dist: Int64;
begin
  Result := -1;
  MinDist := High(Int64);

  for I := 0 to FLocatorCount - 1 do
  begin
    Dist := Abs(FLocators[I].Position - APosition);
    if Dist < MinDist then
    begin
      MinDist := Dist;
      Result := I;
    end;
  end;
end;

procedure TSedaiTransport.AdvanceSamples(ASamples: Integer);
var
  NewPos: Int64;
begin
  if FState <> tsPlaying then Exit;

  NewPos := FPosition + ASamples;

  // Handle loop
  if FLoopEnabled and (NewPos >= FLoopEnd) then
    NewPos := FLoopStart + (NewPos - FLoopEnd);

  // Clamp to project length
  if NewPos > FLength then
  begin
    NewPos := FLength;
    Stop;
  end;

  FPosition := NewPos;

  // Update tempo from map
  FTempo := GetTempoAtPosition(FPosition);
end;

function TSedaiTransport.IsInLoop(APosition: Int64): Boolean;
begin
  Result := FLoopEnabled and (APosition >= FLoopStart) and (APosition < FLoopEnd);
end;

function TSedaiTransport.IsRecordingAt(APosition: Int64): Boolean;
begin
  Result := (FState = tsRecording);

  if FPunchEnabled then
    Result := Result and (APosition >= FPunchIn) and (APosition < FPunchOut);
end;

function TSedaiTransport.GetNextEventPosition: Int64;
begin
  Result := FLength;

  // Check loop end
  if FLoopEnabled and (FLoopEnd > FPosition) and (FLoopEnd < Result) then
    Result := FLoopEnd;

  // Check punch points
  if FPunchEnabled then
  begin
    if (FPunchIn > FPosition) and (FPunchIn < Result) then
      Result := FPunchIn;
    if (FPunchOut > FPosition) and (FPunchOut < Result) then
      Result := FPunchOut;
  end;
end;

end.
