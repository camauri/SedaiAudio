{*
 * Sedai Audio Foundation - Reverb
 *
 * TSedaiReverb implements Freeverb algorithm with additional controls
 * for pre-delay, damping, room size, and stereo width.
 *
 * Based on Freeverb by Jezar at Dreampoint
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiReverb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiEffect;

const
  // Freeverb constants
  NUM_COMBS = 8;
  NUM_ALLPASSES = 4;

  // Comb filter tunings (in samples at 44100 Hz)
  COMB_TUNING: array[0..NUM_COMBS-1] of Integer = (
    1116, 1188, 1277, 1356, 1422, 1491, 1557, 1617
  );

  // Allpass filter tunings
  ALLPASS_TUNING: array[0..NUM_ALLPASSES-1] of Integer = (
    556, 441, 341, 225
  );

  // Stereo spread (samples)
  STEREO_SPREAD = 23;

type
  { TCombFilter }
  // Comb filter for reverb
  TCombFilter = record
    Buffer: array of Single;
    BufferSize: Integer;
    Index: Integer;
    FilterStore: Single;
    Damp1: Single;
    Damp2: Single;
    Feedback: Single;
  end;

  { TAllpassFilter }
  // Allpass filter for diffusion
  TAllpassFilter = record
    Buffer: array of Single;
    BufferSize: Integer;
    Index: Integer;
    Feedback: Single;
  end;

  { TSedaiReverb }
  // Freeverb-based reverb effect
  TSedaiReverb = class(TSedaiEffect)
  private
    // Parameters
    FRoomSize: Single;            // Room size (0-1)
    FDamping: Single;             // High frequency damping (0-1)
    FWidth: Single;               // Stereo width (0-1)
    FMix: Single;                 // Dry/wet mix (0-1)
    FPreDelayMs: Single;          // Pre-delay in ms

    // Internal parameters
    FGain: Single;
    FRoomSize1: Single;
    FDamp1: Single;
    FDamp2: Single;

    // Comb filters (stereo)
    FCombL: array[0..NUM_COMBS-1] of TCombFilter;
    FCombR: array[0..NUM_COMBS-1] of TCombFilter;

    // Allpass filters (stereo)
    FAllpassL: array[0..NUM_ALLPASSES-1] of TAllpassFilter;
    FAllpassR: array[0..NUM_ALLPASSES-1] of TAllpassFilter;

    // Pre-delay buffer
    FPreDelayBuffer: array[0..1] of array of Single;
    FPreDelaySize: Integer;
    FPreDelayIndex: Integer;

    procedure SetRoomSize(AValue: Single);
    procedure SetDamping(AValue: Single);
    procedure SetWidth(AValue: Single);
    procedure SetPreDelayMs(AValue: Single);

    procedure UpdateParameters;
    procedure AllocateBuffers;

    function ProcessComb(var AComb: TCombFilter; AInput: Single): Single;
    function ProcessAllpass(var AAllpass: TAllpassFilter; AInput: Single): Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // Process block
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Presets
    procedure LoadPresetSmallRoom;
    procedure LoadPresetMediumHall;
    procedure LoadPresetLargeHall;
    procedure LoadPresetPlate;

    // Properties
    property RoomSize: Single read FRoomSize write SetRoomSize;
    property Damping: Single read FDamping write SetDamping;
    property Width: Single read FWidth write SetWidth;
    property Mix: Single read FMix write FMix;
    property PreDelayMs: Single read FPreDelayMs write SetPreDelayMs;
  end;

implementation

const
  INITIAL_ROOM = 0.5;
  INITIAL_DAMP = 0.5;
  INITIAL_WIDTH = 1.0;
  INITIAL_MIX = 0.33;
  FIXED_GAIN = 0.015;
  SCALE_ROOM = 0.28;
  OFFSET_ROOM = 0.7;
  SCALE_DAMP = 0.4;
  ALLPASS_FEEDBACK = 0.5;

{ TSedaiReverb }

constructor TSedaiReverb.Create;
begin
  inherited Create;

  FRoomSize := INITIAL_ROOM;
  FDamping := INITIAL_DAMP;
  FWidth := INITIAL_WIDTH;
  FMix := INITIAL_MIX;
  FPreDelayMs := 0.0;
  FGain := FIXED_GAIN;

  FPreDelaySize := 0;
  FPreDelayIndex := 0;

  AllocateBuffers;
  UpdateParameters;
end;

destructor TSedaiReverb.Destroy;
var
  I: Integer;
begin
  for I := 0 to NUM_COMBS - 1 do
  begin
    SetLength(FCombL[I].Buffer, 0);
    SetLength(FCombR[I].Buffer, 0);
  end;

  for I := 0 to NUM_ALLPASSES - 1 do
  begin
    SetLength(FAllpassL[I].Buffer, 0);
    SetLength(FAllpassR[I].Buffer, 0);
  end;

  SetLength(FPreDelayBuffer[0], 0);
  SetLength(FPreDelayBuffer[1], 0);

  inherited Destroy;
end;

procedure TSedaiReverb.AllocateBuffers;
var
  I: Integer;
  SampleRateScale: Single;
  Size: Integer;
begin
  if FSampleRate = 0 then
    SampleRateScale := 1.0
  else
    SampleRateScale := FSampleRate / 44100.0;

  // Allocate comb filters
  for I := 0 to NUM_COMBS - 1 do
  begin
    Size := Round(COMB_TUNING[I] * SampleRateScale);
    SetLength(FCombL[I].Buffer, Size);
    FCombL[I].BufferSize := Size;
    FCombL[I].Index := 0;
    FCombL[I].FilterStore := 0.0;

    Size := Round((COMB_TUNING[I] + STEREO_SPREAD) * SampleRateScale);
    SetLength(FCombR[I].Buffer, Size);
    FCombR[I].BufferSize := Size;
    FCombR[I].Index := 0;
    FCombR[I].FilterStore := 0.0;
  end;

  // Allocate allpass filters
  for I := 0 to NUM_ALLPASSES - 1 do
  begin
    Size := Round(ALLPASS_TUNING[I] * SampleRateScale);
    SetLength(FAllpassL[I].Buffer, Size);
    FAllpassL[I].BufferSize := Size;
    FAllpassL[I].Index := 0;
    FAllpassL[I].Feedback := ALLPASS_FEEDBACK;

    Size := Round((ALLPASS_TUNING[I] + STEREO_SPREAD) * SampleRateScale);
    SetLength(FAllpassR[I].Buffer, Size);
    FAllpassR[I].BufferSize := Size;
    FAllpassR[I].Index := 0;
    FAllpassR[I].Feedback := ALLPASS_FEEDBACK;
  end;

  // Pre-delay buffer (max 500ms)
  FPreDelaySize := Round(0.5 * FSampleRate);
  if FPreDelaySize < 1 then FPreDelaySize := 1;
  SetLength(FPreDelayBuffer[0], FPreDelaySize);
  SetLength(FPreDelayBuffer[1], FPreDelaySize);
end;

procedure TSedaiReverb.Reset;
var
  I, J: Integer;
begin
  inherited Reset;

  // Clear comb filters
  for I := 0 to NUM_COMBS - 1 do
  begin
    for J := 0 to FCombL[I].BufferSize - 1 do
      FCombL[I].Buffer[J] := 0.0;
    FCombL[I].FilterStore := 0.0;
    FCombL[I].Index := 0;

    for J := 0 to FCombR[I].BufferSize - 1 do
      FCombR[I].Buffer[J] := 0.0;
    FCombR[I].FilterStore := 0.0;
    FCombR[I].Index := 0;
  end;

  // Clear allpass filters
  for I := 0 to NUM_ALLPASSES - 1 do
  begin
    for J := 0 to FAllpassL[I].BufferSize - 1 do
      FAllpassL[I].Buffer[J] := 0.0;
    FAllpassL[I].Index := 0;

    for J := 0 to FAllpassR[I].BufferSize - 1 do
      FAllpassR[I].Buffer[J] := 0.0;
    FAllpassR[I].Index := 0;
  end;

  // Clear pre-delay
  for I := 0 to FPreDelaySize - 1 do
  begin
    FPreDelayBuffer[0][I] := 0.0;
    FPreDelayBuffer[1][I] := 0.0;
  end;
  FPreDelayIndex := 0;
end;

procedure TSedaiReverb.SetRoomSize(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 1.0 then AValue := 1.0;
  FRoomSize := AValue;
  UpdateParameters;
end;

procedure TSedaiReverb.SetDamping(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 1.0 then AValue := 1.0;
  FDamping := AValue;
  UpdateParameters;
end;

procedure TSedaiReverb.SetWidth(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 1.0 then AValue := 1.0;
  FWidth := AValue;
end;

procedure TSedaiReverb.SetPreDelayMs(AValue: Single);
begin
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 500.0 then AValue := 500.0;
  FPreDelayMs := AValue;
end;

procedure TSedaiReverb.UpdateParameters;
var
  I: Integer;
begin
  FRoomSize1 := FRoomSize * SCALE_ROOM + OFFSET_ROOM;
  FDamp1 := FDamping * SCALE_DAMP;
  FDamp2 := 1.0 - FDamp1;

  for I := 0 to NUM_COMBS - 1 do
  begin
    FCombL[I].Feedback := FRoomSize1;
    FCombR[I].Feedback := FRoomSize1;
    FCombL[I].Damp1 := FDamp1;
    FCombR[I].Damp1 := FDamp1;
    FCombL[I].Damp2 := FDamp2;
    FCombR[I].Damp2 := FDamp2;
  end;
end;

function TSedaiReverb.ProcessComb(var AComb: TCombFilter; AInput: Single): Single;
var
  Output: Single;
begin
  Output := AComb.Buffer[AComb.Index];

  // Low-pass filter in feedback path (damping)
  AComb.FilterStore := Output * AComb.Damp2 + AComb.FilterStore * AComb.Damp1;

  // Write new value
  AComb.Buffer[AComb.Index] := AInput + AComb.FilterStore * AComb.Feedback;

  // Advance index
  AComb.Index := AComb.Index + 1;
  if AComb.Index >= AComb.BufferSize then
    AComb.Index := 0;

  Result := Output;
end;

function TSedaiReverb.ProcessAllpass(var AAllpass: TAllpassFilter; AInput: Single): Single;
var
  BufferOut: Single;
begin
  BufferOut := AAllpass.Buffer[AAllpass.Index];

  Result := BufferOut - AInput;

  AAllpass.Buffer[AAllpass.Index] := AInput + BufferOut * AAllpass.Feedback;

  AAllpass.Index := AAllpass.Index + 1;
  if AAllpass.Index >= AAllpass.BufferSize then
    AAllpass.Index := 0;
end;

procedure TSedaiReverb.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I, J: Integer;
  Left, Right: Single;
  InputL, InputR: Single;
  OutL, OutR: Single;
  PreDelayL, PreDelayR: Single;
  PreDelaySamples: Integer;
  ReadIndex: Integer;
  Wet1, Wet2: Single;
begin
  // Calculate stereo spread
  Wet1 := FMix * (FWidth * 0.5 + 0.5);
  Wet2 := FMix * ((1.0 - FWidth) * 0.5);

  PreDelaySamples := Round(FPreDelayMs * 0.001 * FSampleRate);
  if PreDelaySamples >= FPreDelaySize then
    PreDelaySamples := FPreDelaySize - 1;

  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2];
    Right := AInput[I * 2 + 1];

    // Pre-delay
    if PreDelaySamples > 0 then
    begin
      ReadIndex := (FPreDelayIndex - PreDelaySamples + FPreDelaySize) mod FPreDelaySize;
      PreDelayL := FPreDelayBuffer[0][ReadIndex];
      PreDelayR := FPreDelayBuffer[1][ReadIndex];

      FPreDelayBuffer[0][FPreDelayIndex] := Left;
      FPreDelayBuffer[1][FPreDelayIndex] := Right;
      FPreDelayIndex := (FPreDelayIndex + 1) mod FPreDelaySize;

      InputL := PreDelayL;
      InputR := PreDelayR;
    end
    else
    begin
      InputL := Left;
      InputR := Right;
    end;

    // Mix to mono for reverb input
    InputL := (InputL + InputR) * FGain;
    InputR := InputL;

    // Process comb filters in parallel
    OutL := 0.0;
    OutR := 0.0;
    for J := 0 to NUM_COMBS - 1 do
    begin
      OutL := OutL + ProcessComb(FCombL[J], InputL);
      OutR := OutR + ProcessComb(FCombR[J], InputR);
    end;

    // Process allpass filters in series
    for J := 0 to NUM_ALLPASSES - 1 do
    begin
      OutL := ProcessAllpass(FAllpassL[J], OutL);
      OutR := ProcessAllpass(FAllpassR[J], OutR);
    end;

    // Mix output with stereo width
    AOutput[I * 2] := Left * (1.0 - FMix) + OutL * Wet1 + OutR * Wet2;
    AOutput[I * 2 + 1] := Right * (1.0 - FMix) + OutR * Wet1 + OutL * Wet2;
  end;
end;

procedure TSedaiReverb.LoadPresetSmallRoom;
begin
  FRoomSize := 0.3;
  FDamping := 0.7;
  FWidth := 0.8;
  FMix := 0.25;
  FPreDelayMs := 5.0;
  UpdateParameters;
end;

procedure TSedaiReverb.LoadPresetMediumHall;
begin
  FRoomSize := 0.6;
  FDamping := 0.5;
  FWidth := 1.0;
  FMix := 0.35;
  FPreDelayMs := 20.0;
  UpdateParameters;
end;

procedure TSedaiReverb.LoadPresetLargeHall;
begin
  FRoomSize := 0.85;
  FDamping := 0.3;
  FWidth := 1.0;
  FMix := 0.4;
  FPreDelayMs := 40.0;
  UpdateParameters;
end;

procedure TSedaiReverb.LoadPresetPlate;
begin
  FRoomSize := 0.7;
  FDamping := 0.2;
  FWidth := 1.0;
  FMix := 0.5;
  FPreDelayMs := 0.0;
  UpdateParameters;
end;

end.
