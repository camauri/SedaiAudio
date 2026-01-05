{*
 * Sedai Audio Foundation - Chorus
 *
 * TSedaiChorus provides classic chorus effect with multiple voices,
 * LFO modulation, and stereo spread. Includes flanger mode.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiChorus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiEffect;

const
  MAX_CHORUS_VOICES = 4;
  MAX_CHORUS_DELAY = 50;          // Maximum delay in ms

type
  { TSedaiChorus }
  // Multi-voice chorus effect
  TSedaiChorus = class(TSedaiEffect)
  private
    // Parameters
    FVoices: Integer;             // Number of chorus voices (1-4)
    FRate: Single;                // LFO rate in Hz
    FDepth: Single;               // Modulation depth in ms
    FDelay: Single;               // Base delay in ms
    FFeedback: Single;            // Feedback amount (-1 to +1)
    FMix: Single;                 // Dry/wet mix
    FStereoSpread: Single;        // Stereo spread (0-1)

    // LFO state per voice
    FLFOPhase: array[0..MAX_CHORUS_VOICES-1] of Single;
    FLFOPhaseOffset: array[0..MAX_CHORUS_VOICES-1] of Single;

    // Delay buffer
    FBufferSize: Integer;
    FBuffer: array[0..1] of array of Single;
    FWriteIndex: Integer;

    // High-pass filter for feedback (remove DC)
    FHPState: array[0..1] of Single;

    procedure SetVoices(AValue: Integer);
    procedure SetRate(AValue: Single);
    procedure SetDepth(AValue: Single);
    procedure SetDelay(AValue: Single);
    procedure AllocateBuffer;

    function ReadDelay(AChannel: Integer; ADelaySamples: Single): Single;
    procedure WriteDelay(AChannel: Integer; AValue: Single);
    function GetLFOValue(AVoice: Integer): Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // Process block
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Properties
    property Voices: Integer read FVoices write SetVoices;
    property Rate: Single read FRate write SetRate;
    property Depth: Single read FDepth write SetDepth;
    property Delay: Single read FDelay write SetDelay;
    property Feedback: Single read FFeedback write FFeedback;
    property Mix: Single read FMix write FMix;
    property StereoSpread: Single read FStereoSpread write FStereoSpread;
  end;

implementation

{ TSedaiChorus }

constructor TSedaiChorus.Create;
var
  I: Integer;
begin
  inherited Create;

  FVoices := 2;
  FRate := 0.8;
  FDepth := 3.0;
  FDelay := 7.0;
  FFeedback := 0.0;
  FMix := 0.5;
  FStereoSpread := 1.0;

  FBufferSize := 0;
  FWriteIndex := 0;

  // Initialize LFO phases with spread
  for I := 0 to MAX_CHORUS_VOICES - 1 do
  begin
    FLFOPhase[I] := 0.0;
    FLFOPhaseOffset[I] := I / MAX_CHORUS_VOICES;
  end;

  FHPState[0] := 0.0;
  FHPState[1] := 0.0;

  AllocateBuffer;
end;

destructor TSedaiChorus.Destroy;
begin
  SetLength(FBuffer[0], 0);
  SetLength(FBuffer[1], 0);
  inherited Destroy;
end;

procedure TSedaiChorus.Reset;
var
  I: Integer;
begin
  inherited Reset;

  FWriteIndex := 0;

  for I := 0 to MAX_CHORUS_VOICES - 1 do
    FLFOPhase[I] := FLFOPhaseOffset[I];

  FHPState[0] := 0.0;
  FHPState[1] := 0.0;

  // Clear buffers
  for I := 0 to FBufferSize - 1 do
  begin
    if I < Length(FBuffer[0]) then FBuffer[0][I] := 0.0;
    if I < Length(FBuffer[1]) then FBuffer[1][I] := 0.0;
  end;
end;

procedure TSedaiChorus.SetVoices(AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
  if AValue > MAX_CHORUS_VOICES then AValue := MAX_CHORUS_VOICES;
  FVoices := AValue;
end;

procedure TSedaiChorus.SetRate(AValue: Single);
begin
  if AValue < 0.01 then AValue := 0.01;
  if AValue > 10.0 then AValue := 10.0;
  FRate := AValue;
end;

procedure TSedaiChorus.SetDepth(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0;
  if AValue > MAX_CHORUS_DELAY then AValue := MAX_CHORUS_DELAY;
  FDepth := AValue;
end;

procedure TSedaiChorus.SetDelay(AValue: Single);
begin
  if AValue < 1.0 then AValue := 1.0;
  if AValue > MAX_CHORUS_DELAY then AValue := MAX_CHORUS_DELAY;
  FDelay := AValue;
end;

procedure TSedaiChorus.AllocateBuffer;
var
  NewSize: Integer;
begin
  if FSampleRate > 0 then
  begin
    NewSize := Round((MAX_CHORUS_DELAY * 2) * 0.001 * FSampleRate) + 1024;

    if NewSize <> FBufferSize then
    begin
      FBufferSize := NewSize;
      SetLength(FBuffer[0], FBufferSize);
      SetLength(FBuffer[1], FBufferSize);
      Reset;
    end;
  end;
end;

function TSedaiChorus.ReadDelay(AChannel: Integer; ADelaySamples: Single): Single;
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

procedure TSedaiChorus.WriteDelay(AChannel: Integer; AValue: Single);
begin
  FBuffer[AChannel][FWriteIndex] := AValue;
end;

function TSedaiChorus.GetLFOValue(AVoice: Integer): Single;
begin
  // Triangle LFO for smoother modulation
  if FLFOPhase[AVoice] < 0.5 then
    Result := FLFOPhase[AVoice] * 4.0 - 1.0
  else
    Result := 3.0 - FLFOPhase[AVoice] * 4.0;
end;

procedure TSedaiChorus.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I, V: Integer;
  Left, Right: Single;
  ChorusL, ChorusR: Single;
  VoiceL, VoiceR: Single;
  DelaySamples, LFOValue: Single;
  VoicePan: Single;
  FeedbackL, FeedbackR: Single;
  PhaseInc: Single;
  HPCoeff: Single;
begin
  if FBufferSize = 0 then
    AllocateBuffer;

  PhaseInc := FRate / FSampleRate;

  // High-pass coefficient for DC removal in feedback
  HPCoeff := 0.995;

  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2];
    Right := AInput[I * 2 + 1];

    ChorusL := 0.0;
    ChorusR := 0.0;

    // Process each voice
    for V := 0 to FVoices - 1 do
    begin
      // Get LFO modulation
      LFOValue := GetLFOValue(V);

      // Calculate modulated delay
      DelaySamples := (FDelay + LFOValue * FDepth) * 0.001 * FSampleRate;
      if DelaySamples < 1 then DelaySamples := 1;

      // Read from delay line
      VoiceL := ReadDelay(0, DelaySamples);
      VoiceR := ReadDelay(1, DelaySamples);

      // Calculate pan for stereo spread
      if FVoices > 1 then
        VoicePan := -FStereoSpread + (V / (FVoices - 1)) * 2.0 * FStereoSpread
      else
        VoicePan := 0.0;

      // Apply panning
      ChorusL := ChorusL + VoiceL * (1.0 - Max(0, VoicePan));
      ChorusR := ChorusR + VoiceR * (1.0 + Min(0, VoicePan));

      // Advance LFO phase
      FLFOPhase[V] := FLFOPhase[V] + PhaseInc;
      if FLFOPhase[V] >= 1.0 then
        FLFOPhase[V] := FLFOPhase[V] - 1.0;
    end;

    // Normalize by voice count
    if FVoices > 1 then
    begin
      ChorusL := ChorusL / FVoices;
      ChorusR := ChorusR / FVoices;
    end;

    // Calculate feedback with high-pass to remove DC
    if Abs(FFeedback) > 0.01 then
    begin
      FeedbackL := ChorusL * FFeedback;
      FeedbackR := ChorusR * FFeedback;

      // High-pass filter
      FHPState[0] := HPCoeff * FHPState[0] + (1.0 - HPCoeff) * FeedbackL;
      FHPState[1] := HPCoeff * FHPState[1] + (1.0 - HPCoeff) * FeedbackR;
      FeedbackL := FeedbackL - FHPState[0];
      FeedbackR := FeedbackR - FHPState[1];

      // Write to delay with feedback
      WriteDelay(0, Left + FeedbackL);
      WriteDelay(1, Right + FeedbackR);
    end
    else
    begin
      WriteDelay(0, Left);
      WriteDelay(1, Right);
    end;

    // Mix dry/wet
    AOutput[I * 2] := Left * (1.0 - FMix) + ChorusL * FMix;
    AOutput[I * 2 + 1] := Right * (1.0 - FMix) + ChorusR * FMix;

    // Advance write index
    FWriteIndex := (FWriteIndex + 1) mod FBufferSize;
  end;
end;

end.
