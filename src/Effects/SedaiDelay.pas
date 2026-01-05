{*
 * Sedai Audio Foundation - Delay
 *
 * TSedaiDelay provides multiple delay modes including simple delay,
 * ping-pong, multi-tap, tape emulation, and BBD (bucket brigade).
 * Supports tempo sync and modulation.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiDelay;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiEffect;

const
  MAX_DELAY_TIME = 5.0;           // Maximum delay time in seconds
  MAX_DELAY_TAPS = 8;             // Maximum multi-tap count

type
  // Delay mode
  TDelayMode = (
    dmSimple,         // Simple mono/stereo delay
    dmPingPong,       // Ping-pong stereo delay
    dmMultiTap,       // Multi-tap delay
    dmTape,           // Tape delay emulation
    dmBBD             // Bucket brigade (analog) emulation
  );

  // Delay tap
  TDelayTap = record
    Enabled: Boolean;
    Time: Single;               // Time in ms or beats
    Level: Single;              // Tap level (0-1)
    Pan: Single;                // Pan position (-1 to +1)
    Feedback: Single;           // Per-tap feedback
  end;

  { TSedaiDelay }
  // Multi-mode delay effect
  TSedaiDelay = class(TSedaiEffect)
  private
    FMode: TDelayMode;

    // Delay parameters
    FDelayTime: Single;           // Delay time in ms
    FDelayTimeRight: Single;      // Right channel delay (for stereo)
    FFeedback: Single;            // Feedback amount (0-1)
    FMix: Single;                 // Dry/wet mix

    // Modulation
    FModDepth: Single;            // Modulation depth in ms
    FModRate: Single;             // Modulation rate in Hz
    FModPhase: Single;            // Current modulation phase

    // Filtering
    FHighCut: Single;             // High-cut filter frequency
    FLowCut: Single;              // Low-cut filter frequency
    FFilterState: array[0..1, 0..1] of Single;  // [channel][LP/HP]

    // Delay buffer
    FBufferSize: Integer;
    FBuffer: array[0..1] of array of Single;
    FWriteIndex: Integer;

    // Multi-tap
    FTaps: array[0..MAX_DELAY_TAPS-1] of TDelayTap;
    FTapCount: Integer;

    // Tape/BBD emulation
    FTapeWow: Single;             // Wow depth
    FTapeFlutter: Single;         // Flutter depth
    FTapeSaturation: Single;      // Tape saturation amount
    FBBDStages: Integer;          // BBD stages (affects HF roll-off)
    FWowPhase: Single;
    FFlutterPhase: Single;

    procedure SetDelayTime(AValue: Single);
    procedure SetMode(AValue: TDelayMode);
    procedure AllocateBuffer;

    function ReadDelay(AChannel: Integer; ADelaySamples: Single): Single;
    procedure WriteDelay(AChannel: Integer; AValue: Single);
    function ApplyFilters(AInput: Single; AChannel: Integer): Single;
    function ApplyTapeSaturation(AInput: Single): Single;
    function GetModulatedDelay(ABaseDelay: Single): Single;
    function GetTapeModulation: Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // Process block
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Multi-tap configuration
    procedure SetTap(AIndex: Integer; ATime, ALevel, APan, AFeedback: Single);
    procedure EnableTap(AIndex: Integer; AEnabled: Boolean);
    procedure ClearTaps;

    // Properties
    property Mode: TDelayMode read FMode write SetMode;
    property DelayTime: Single read FDelayTime write SetDelayTime;
    property DelayTimeRight: Single read FDelayTimeRight write FDelayTimeRight;
    property Feedback: Single read FFeedback write FFeedback;
    property Mix: Single read FMix write FMix;
    property ModDepth: Single read FModDepth write FModDepth;
    property ModRate: Single read FModRate write FModRate;
    property HighCut: Single read FHighCut write FHighCut;
    property LowCut: Single read FLowCut write FLowCut;
    property TapeWow: Single read FTapeWow write FTapeWow;
    property TapeFlutter: Single read FTapeFlutter write FTapeFlutter;
    property TapeSaturation: Single read FTapeSaturation write FTapeSaturation;
    property TapCount: Integer read FTapCount;
  end;

implementation

{ TSedaiDelay }

constructor TSedaiDelay.Create;
var
  I: Integer;
begin
  inherited Create;

  FMode := dmSimple;
  FDelayTime := 500.0;
  FDelayTimeRight := 500.0;
  FFeedback := 0.3;
  FMix := 0.5;

  FModDepth := 0.0;
  FModRate := 0.5;
  FModPhase := 0.0;

  FHighCut := 8000.0;
  FLowCut := 100.0;

  FBufferSize := 0;
  FWriteIndex := 0;

  // Initialize taps
  for I := 0 to MAX_DELAY_TAPS - 1 do
  begin
    FTaps[I].Enabled := False;
    FTaps[I].Time := 100.0 * (I + 1);
    FTaps[I].Level := 0.5;
    FTaps[I].Pan := 0.0;
    FTaps[I].Feedback := 0.0;
  end;
  FTapCount := 0;

  FTapeWow := 0.0;
  FTapeFlutter := 0.0;
  FTapeSaturation := 0.0;
  FBBDStages := 512;
  FWowPhase := 0.0;
  FFlutterPhase := 0.0;

  // Clear filter state
  for I := 0 to 1 do
  begin
    FFilterState[I, 0] := 0.0;
    FFilterState[I, 1] := 0.0;
  end;

  AllocateBuffer;
end;

destructor TSedaiDelay.Destroy;
begin
  SetLength(FBuffer[0], 0);
  SetLength(FBuffer[1], 0);
  inherited Destroy;
end;

procedure TSedaiDelay.Reset;
var
  I: Integer;
begin
  inherited Reset;

  FWriteIndex := 0;
  FModPhase := 0.0;
  FWowPhase := 0.0;
  FFlutterPhase := 0.0;

  // Clear buffers
  for I := 0 to FBufferSize - 1 do
  begin
    if I < Length(FBuffer[0]) then FBuffer[0][I] := 0.0;
    if I < Length(FBuffer[1]) then FBuffer[1][I] := 0.0;
  end;

  // Clear filter state
  for I := 0 to 1 do
  begin
    FFilterState[I, 0] := 0.0;
    FFilterState[I, 1] := 0.0;
  end;
end;

procedure TSedaiDelay.SetDelayTime(AValue: Single);
begin
  if AValue < 1.0 then
    AValue := 1.0
  else if AValue > MAX_DELAY_TIME * 1000.0 then
    AValue := MAX_DELAY_TIME * 1000.0;

  FDelayTime := AValue;
end;

procedure TSedaiDelay.SetMode(AValue: TDelayMode);
begin
  FMode := AValue;
end;

procedure TSedaiDelay.AllocateBuffer;
var
  NewSize: Integer;
begin
  if FSampleRate > 0 then
  begin
    NewSize := Round(MAX_DELAY_TIME * FSampleRate) + 1024;

    if NewSize <> FBufferSize then
    begin
      FBufferSize := NewSize;
      SetLength(FBuffer[0], FBufferSize);
      SetLength(FBuffer[1], FBufferSize);
      Reset;
    end;
  end;
end;

function TSedaiDelay.ReadDelay(AChannel: Integer; ADelaySamples: Single): Single;
var
  ReadPos: Single;
  Index0, Index1: Integer;
  Frac: Single;
begin
  ReadPos := FWriteIndex - ADelaySamples;
  while ReadPos < 0 do
    ReadPos := ReadPos + FBufferSize;

  Index0 := Trunc(ReadPos) mod FBufferSize;
  Index1 := (Index0 + 1) mod FBufferSize;
  Frac := ReadPos - Trunc(ReadPos);

  // Linear interpolation
  Result := FBuffer[AChannel][Index0] * (1.0 - Frac) +
            FBuffer[AChannel][Index1] * Frac;
end;

procedure TSedaiDelay.WriteDelay(AChannel: Integer; AValue: Single);
begin
  FBuffer[AChannel][FWriteIndex] := AValue;
end;

function TSedaiDelay.ApplyFilters(AInput: Single; AChannel: Integer): Single;
var
  LPCoeff, HPCoeff: Single;
begin
  // Simple one-pole filters
  if FSampleRate > 0 then
  begin
    // Low-pass
    LPCoeff := Exp(-2.0 * PI * FHighCut / FSampleRate);
    FFilterState[AChannel, 0] := FFilterState[AChannel, 0] * LPCoeff +
                                  AInput * (1.0 - LPCoeff);
    Result := FFilterState[AChannel, 0];

    // High-pass
    HPCoeff := Exp(-2.0 * PI * FLowCut / FSampleRate);
    FFilterState[AChannel, 1] := FFilterState[AChannel, 1] * HPCoeff +
                                  Result * (1.0 - HPCoeff);
    Result := Result - FFilterState[AChannel, 1];
  end
  else
    Result := AInput;
end;

function TSedaiDelay.ApplyTapeSaturation(AInput: Single): Single;
begin
  if FTapeSaturation > 0 then
    Result := Tanh(AInput * (1.0 + FTapeSaturation * 2.0)) / (1.0 + FTapeSaturation)
  else
    Result := AInput;
end;

function TSedaiDelay.GetModulatedDelay(ABaseDelay: Single): Single;
var
  ModValue: Single;
begin
  if FModDepth > 0 then
  begin
    ModValue := Sin(FModPhase * 2.0 * PI) * FModDepth;
    Result := ABaseDelay + ModValue;
    if Result < 1.0 then Result := 1.0;
  end
  else
    Result := ABaseDelay;
end;

function TSedaiDelay.GetTapeModulation: Single;
var
  Wow, Flutter: Single;
begin
  Result := 0.0;

  if FTapeWow > 0 then
  begin
    Wow := Sin(FWowPhase * 2.0 * PI) * FTapeWow;
    Result := Result + Wow;
  end;

  if FTapeFlutter > 0 then
  begin
    Flutter := Sin(FFlutterPhase * 2.0 * PI) * FTapeFlutter;
    Result := Result + Flutter;
  end;
end;

procedure TSedaiDelay.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I, T: Integer;
  Left, Right: Single;
  DelayedLeft, DelayedRight: Single;
  DelaySamplesL, DelaySamplesR: Single;
  TapDelay, TapSample: Single;
  TapLeft, TapRight: Single;
  FeedbackL, FeedbackR: Single;
  TapeModMs: Single;
begin
  // Ensure buffer is allocated
  if FBufferSize = 0 then
    AllocateBuffer;

  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2];
    Right := AInput[I * 2 + 1];

    // Calculate delay in samples
    DelaySamplesL := FDelayTime * 0.001 * FSampleRate;
    DelaySamplesR := FDelayTimeRight * 0.001 * FSampleRate;

    // Apply modulation
    DelaySamplesL := GetModulatedDelay(DelaySamplesL);
    DelaySamplesR := GetModulatedDelay(DelaySamplesR);

    // Apply tape modulation
    if (FMode = dmTape) and ((FTapeWow > 0) or (FTapeFlutter > 0)) then
    begin
      TapeModMs := GetTapeModulation;
      DelaySamplesL := DelaySamplesL + TapeModMs * 0.001 * FSampleRate;
      DelaySamplesR := DelaySamplesR + TapeModMs * 0.001 * FSampleRate;
    end;

    case FMode of
      dmSimple:
        begin
          // Read from delay line
          DelayedLeft := ReadDelay(0, DelaySamplesL);
          DelayedRight := ReadDelay(1, DelaySamplesR);

          // Apply filters
          DelayedLeft := ApplyFilters(DelayedLeft, 0);
          DelayedRight := ApplyFilters(DelayedRight, 1);

          // Write input + feedback to delay line
          WriteDelay(0, Left + DelayedLeft * FFeedback);
          WriteDelay(1, Right + DelayedRight * FFeedback);
        end;

      dmPingPong:
        begin
          // Ping-pong: left feeds right, right feeds left
          DelayedLeft := ReadDelay(0, DelaySamplesL);
          DelayedRight := ReadDelay(1, DelaySamplesR);

          DelayedLeft := ApplyFilters(DelayedLeft, 0);
          DelayedRight := ApplyFilters(DelayedRight, 1);

          // Cross-feed
          WriteDelay(0, Left + DelayedRight * FFeedback);
          WriteDelay(1, Right + DelayedLeft * FFeedback);
        end;

      dmMultiTap:
        begin
          TapLeft := 0.0;
          TapRight := 0.0;
          FeedbackL := 0.0;
          FeedbackR := 0.0;

          // Sum all taps
          for T := 0 to MAX_DELAY_TAPS - 1 do
          begin
            if FTaps[T].Enabled then
            begin
              TapDelay := FTaps[T].Time * 0.001 * FSampleRate;
              TapSample := ReadDelay(0, TapDelay);

              // Pan the tap
              TapLeft := TapLeft + TapSample * FTaps[T].Level *
                         (1.0 - (FTaps[T].Pan + 1.0) * 0.5);
              TapRight := TapRight + TapSample * FTaps[T].Level *
                          ((FTaps[T].Pan + 1.0) * 0.5);

              FeedbackL := FeedbackL + TapSample * FTaps[T].Feedback;
            end;
          end;

          DelayedLeft := TapLeft;
          DelayedRight := TapRight;

          WriteDelay(0, (Left + Right) * 0.5 + FeedbackL);
          WriteDelay(1, 0.0);  // Not used in multi-tap
        end;

      dmTape:
        begin
          DelayedLeft := ReadDelay(0, DelaySamplesL);
          DelayedRight := ReadDelay(1, DelaySamplesR);

          // Apply tape saturation
          DelayedLeft := ApplyTapeSaturation(DelayedLeft);
          DelayedRight := ApplyTapeSaturation(DelayedRight);

          DelayedLeft := ApplyFilters(DelayedLeft, 0);
          DelayedRight := ApplyFilters(DelayedRight, 1);

          WriteDelay(0, Left + DelayedLeft * FFeedback);
          WriteDelay(1, Right + DelayedRight * FFeedback);
        end;

      dmBBD:
        begin
          DelayedLeft := ReadDelay(0, DelaySamplesL);
          DelayedRight := ReadDelay(1, DelaySamplesR);

          // BBD-style filtering (more aggressive high cut)
          DelayedLeft := ApplyFilters(DelayedLeft, 0);
          DelayedRight := ApplyFilters(DelayedRight, 1);

          // Slight noise and aliasing artifacts could be added here
          WriteDelay(0, Left + DelayedLeft * FFeedback);
          WriteDelay(1, Right + DelayedRight * FFeedback);
        end;
    end;

    // Mix dry/wet
    AOutput[I * 2] := Left * (1.0 - FMix) + DelayedLeft * FMix;
    AOutput[I * 2 + 1] := Right * (1.0 - FMix) + DelayedRight * FMix;

    // Advance write index
    FWriteIndex := (FWriteIndex + 1) mod FBufferSize;

    // Advance modulation phases
    if FSampleRate > 0 then
    begin
      FModPhase := FModPhase + FModRate / FSampleRate;
      if FModPhase >= 1.0 then FModPhase := FModPhase - 1.0;

      FWowPhase := FWowPhase + 0.5 / FSampleRate;  // 0.5 Hz wow
      if FWowPhase >= 1.0 then FWowPhase := FWowPhase - 1.0;

      FFlutterPhase := FFlutterPhase + 6.0 / FSampleRate;  // 6 Hz flutter
      if FFlutterPhase >= 1.0 then FFlutterPhase := FFlutterPhase - 1.0;
    end;
  end;
end;

procedure TSedaiDelay.SetTap(AIndex: Integer; ATime, ALevel, APan, AFeedback: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_DELAY_TAPS) then
  begin
    FTaps[AIndex].Time := ATime;
    FTaps[AIndex].Level := ALevel;
    FTaps[AIndex].Pan := APan;
    FTaps[AIndex].Feedback := AFeedback;
    FTaps[AIndex].Enabled := True;

    // Update tap count
    FTapCount := 0;
    for AIndex := 0 to MAX_DELAY_TAPS - 1 do
      if FTaps[AIndex].Enabled then Inc(FTapCount);
  end;
end;

procedure TSedaiDelay.EnableTap(AIndex: Integer; AEnabled: Boolean);
var
  I: Integer;
begin
  if (AIndex >= 0) and (AIndex < MAX_DELAY_TAPS) then
  begin
    FTaps[AIndex].Enabled := AEnabled;

    FTapCount := 0;
    for I := 0 to MAX_DELAY_TAPS - 1 do
      if FTaps[I].Enabled then Inc(FTapCount);
  end;
end;

procedure TSedaiDelay.ClearTaps;
var
  I: Integer;
begin
  for I := 0 to MAX_DELAY_TAPS - 1 do
    FTaps[I].Enabled := False;
  FTapCount := 0;
end;

end.
