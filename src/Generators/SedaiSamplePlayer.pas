{*
 * Sedai Audio Foundation - Sample Player
 *
 * TSedaiSamplePlayer provides sample playback with variable pitch,
 * loop modes, start/end points, and interpolation options.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiSamplePlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiOscillator,
  SedaiAudioBuffer;

type
  // Loop mode
  TLoopMode = (
    lmNone,           // No loop (one-shot)
    lmForward,        // Forward loop
    lmPingPong,       // Bidirectional loop
    lmReverse         // Reverse loop
  );

  // Interpolation mode
  TInterpolationMode = (
    imNone,           // No interpolation (nearest)
    imLinear,         // Linear interpolation
    imCubic           // Cubic interpolation
  );

  { TSedaiSamplePlayer }
  // Sample playback with pitch and loops
  TSedaiSamplePlayer = class(TSedaiSignalGenerator)
  private
    // Sample data
    FSampleData: TSedaiAudioBuffer;
    FOwnsSample: Boolean;

    // Playback state
    FPosition: Double;            // Current position (fractional)
    FPlaybackRate: Double;        // Playback rate (1.0 = original)
    FPlaying: Boolean;
    FDirection: Integer;          // 1 = forward, -1 = reverse

    // Sample region
    FStartSample: Integer;
    FEndSample: Integer;
    FLoopStart: Integer;
    FLoopEnd: Integer;

    // Mode
    FLoopMode: TLoopMode;
    FInterpolation: TInterpolationMode;

    // Pitch
    FRootNote: Integer;           // MIDI note of original pitch
    FFineTune: Single;            // Fine tuning in cents

    procedure SetPlaybackRate(AValue: Double);
    procedure SetLoopMode(AValue: TLoopMode);
    procedure UpdatePlaybackRateFromFrequency;

    function ReadSampleInterpolated(APosition: Double; AChannel: Integer): Single;
    function LinearInterpolate(A, B, Frac: Single): Single;
    function CubicInterpolate(Y0, Y1, Y2, Y3, Frac: Single): Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // Load sample data
    procedure LoadSample(ASample: TSedaiAudioBuffer; ATakeOwnership: Boolean = False);
    procedure ClearSample;

    // Playback control
    procedure Play;
    procedure Stop;
    procedure Pause;
    procedure SetPosition(ASample: Integer);

    // Set pitch from MIDI note
    procedure SetNoteFrequency(ANote: Integer);

    // Generate sample
    function GenerateSample: Single; override;

    // Generate stereo sample
    procedure GenerateStereoSample(out ALeft, ARight: Single);

    // Properties
    property Position: Double read FPosition;
    property PlaybackRate: Double read FPlaybackRate write SetPlaybackRate;
    property Playing: Boolean read FPlaying;
    property StartSample: Integer read FStartSample write FStartSample;
    property EndSample: Integer read FEndSample write FEndSample;
    property LoopStart: Integer read FLoopStart write FLoopStart;
    property LoopEnd: Integer read FLoopEnd write FLoopEnd;
    property LoopMode: TLoopMode read FLoopMode write SetLoopMode;
    property Interpolation: TInterpolationMode read FInterpolation write FInterpolation;
    property RootNote: Integer read FRootNote write FRootNote;
    property FineTune: Single read FFineTune write FFineTune;
  end;

implementation

{ TSedaiSamplePlayer }

constructor TSedaiSamplePlayer.Create;
begin
  inherited Create;

  FSampleData := nil;
  FOwnsSample := False;

  FPosition := 0.0;
  FPlaybackRate := 1.0;
  FPlaying := False;
  FDirection := 1;

  FStartSample := 0;
  FEndSample := 0;
  FLoopStart := 0;
  FLoopEnd := 0;

  FLoopMode := lmNone;
  FInterpolation := imLinear;

  FRootNote := 60;  // C4
  FFineTune := 0.0;
end;

destructor TSedaiSamplePlayer.Destroy;
begin
  if FOwnsSample and Assigned(FSampleData) then
    FSampleData.Free;

  inherited Destroy;
end;

procedure TSedaiSamplePlayer.Reset;
begin
  inherited Reset;

  FPosition := FStartSample;
  FPlaying := False;
  FDirection := 1;
end;

procedure TSedaiSamplePlayer.SetPlaybackRate(AValue: Double);
begin
  if AValue < 0.01 then AValue := 0.01;
  if AValue > 100.0 then AValue := 100.0;
  FPlaybackRate := AValue;
end;

procedure TSedaiSamplePlayer.SetLoopMode(AValue: TLoopMode);
begin
  FLoopMode := AValue;
  if AValue = lmReverse then
    FDirection := -1
  else
    FDirection := 1;
end;

procedure TSedaiSamplePlayer.UpdatePlaybackRateFromFrequency;
var
  RootFreq, TargetFreq: Single;
  FineTuneRatio: Single;
begin
  if FSampleData = nil then Exit;

  // Calculate root frequency
  RootFreq := MIDINoteToFrequency(FRootNote);

  // Calculate target frequency
  TargetFreq := FFrequency;

  // Apply fine tune
  FineTuneRatio := Power(2.0, FFineTune / 1200.0);

  // Calculate playback rate
  if RootFreq > 0 then
    FPlaybackRate := (TargetFreq / RootFreq) * FineTuneRatio
  else
    FPlaybackRate := 1.0;

  // Adjust for sample rate difference
  if (FSampleData.SampleRate > 0) and (FSampleRate > 0) then
    FPlaybackRate := FPlaybackRate * (FSampleData.SampleRate / FSampleRate);
end;

procedure TSedaiSamplePlayer.LoadSample(ASample: TSedaiAudioBuffer; ATakeOwnership: Boolean);
begin
  // Free existing sample if we own it
  if FOwnsSample and Assigned(FSampleData) then
    FSampleData.Free;

  FSampleData := ASample;
  FOwnsSample := ATakeOwnership;

  if Assigned(FSampleData) then
  begin
    FStartSample := 0;
    FEndSample := FSampleData.SampleCount - 1;
    FLoopStart := 0;
    FLoopEnd := FSampleData.SampleCount - 1;
  end
  else
  begin
    FStartSample := 0;
    FEndSample := 0;
    FLoopStart := 0;
    FLoopEnd := 0;
  end;

  FPosition := FStartSample;
  FPlaying := False;
end;

procedure TSedaiSamplePlayer.ClearSample;
begin
  if FOwnsSample and Assigned(FSampleData) then
    FSampleData.Free;

  FSampleData := nil;
  FOwnsSample := False;
  FPosition := 0;
  FPlaying := False;
end;

procedure TSedaiSamplePlayer.Play;
begin
  if FSampleData = nil then Exit;

  FPlaying := True;
  if FLoopMode = lmReverse then
  begin
    FPosition := FEndSample;
    FDirection := -1;
  end
  else
  begin
    FPosition := FStartSample;
    FDirection := 1;
  end;
end;

procedure TSedaiSamplePlayer.Stop;
begin
  FPlaying := False;
  FPosition := FStartSample;
end;

procedure TSedaiSamplePlayer.Pause;
begin
  FPlaying := False;
end;

procedure TSedaiSamplePlayer.SetPosition(ASample: Integer);
begin
  if ASample < FStartSample then ASample := FStartSample;
  if ASample > FEndSample then ASample := FEndSample;
  FPosition := ASample;
end;

procedure TSedaiSamplePlayer.SetNoteFrequency(ANote: Integer);
begin
  FFrequency := MIDINoteToFrequency(ANote);
  UpdatePlaybackRateFromFrequency;
end;

function TSedaiSamplePlayer.LinearInterpolate(A, B, Frac: Single): Single;
begin
  Result := A + (B - A) * Frac;
end;

function TSedaiSamplePlayer.CubicInterpolate(Y0, Y1, Y2, Y3, Frac: Single): Single;
var
  A0, A1, A2, A3, Frac2: Single;
begin
  Frac2 := Frac * Frac;
  A0 := Y3 - Y2 - Y0 + Y1;
  A1 := Y0 - Y1 - A0;
  A2 := Y2 - Y0;
  A3 := Y1;
  Result := A0 * Frac * Frac2 + A1 * Frac2 + A2 * Frac + A3;
end;

function TSedaiSamplePlayer.ReadSampleInterpolated(APosition: Double; AChannel: Integer): Single;
var
  Index0, Index1, Index2, Index3: Integer;
  Frac: Single;
  SampleCount: Integer;
begin
  if FSampleData = nil then
  begin
    Result := 0.0;
    Exit;
  end;

  SampleCount := FSampleData.SampleCount;
  if SampleCount = 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  case FInterpolation of
    imNone:
      begin
        Index0 := Trunc(APosition);
        if (Index0 >= 0) and (Index0 < SampleCount) then
          Result := FSampleData.GetSample(AChannel mod FSampleData.Channels, Index0)
        else
          Result := 0.0;
      end;

    imLinear:
      begin
        Index0 := Trunc(APosition);
        Index1 := Index0 + 1;
        Frac := APosition - Index0;

        if Index1 >= SampleCount then Index1 := SampleCount - 1;
        if Index0 < 0 then Index0 := 0;

        Result := LinearInterpolate(
          FSampleData.GetSample(AChannel mod FSampleData.Channels, Index0),
          FSampleData.GetSample(AChannel mod FSampleData.Channels, Index1),
          Frac
        );
      end;

    imCubic:
      begin
        Index1 := Trunc(APosition);
        Index0 := Index1 - 1;
        Index2 := Index1 + 1;
        Index3 := Index1 + 2;
        Frac := APosition - Index1;

        // Clamp indices
        if Index0 < 0 then Index0 := 0;
        if Index1 < 0 then Index1 := 0;
        if Index2 >= SampleCount then Index2 := SampleCount - 1;
        if Index3 >= SampleCount then Index3 := SampleCount - 1;

        Result := CubicInterpolate(
          FSampleData.GetSample(AChannel mod FSampleData.Channels, Index0),
          FSampleData.GetSample(AChannel mod FSampleData.Channels, Index1),
          FSampleData.GetSample(AChannel mod FSampleData.Channels, Index2),
          FSampleData.GetSample(AChannel mod FSampleData.Channels, Index3),
          Frac
        );
      end;

    else
      Result := 0.0;
  end;
end;

function TSedaiSamplePlayer.GenerateSample: Single;
begin
  // Return mono (left channel or mixed)
  if not FPlaying or (FSampleData = nil) then
  begin
    Result := 0.0;
    Exit;
  end;

  Result := ReadSampleInterpolated(FPosition, 0) * FAmplitude;

  // Advance position
  FPosition := FPosition + FPlaybackRate * FDirection;

  // Handle loop/end
  case FLoopMode of
    lmNone:
      begin
        if FPosition >= FEndSample then
        begin
          FPlaying := False;
          FPosition := FEndSample;
        end;
      end;

    lmForward:
      begin
        if FPosition >= FLoopEnd then
          FPosition := FLoopStart + (FPosition - FLoopEnd);
      end;

    lmPingPong:
      begin
        if FDirection > 0 then
        begin
          if FPosition >= FLoopEnd then
          begin
            FPosition := FLoopEnd - (FPosition - FLoopEnd);
            FDirection := -1;
          end;
        end
        else
        begin
          if FPosition <= FLoopStart then
          begin
            FPosition := FLoopStart + (FLoopStart - FPosition);
            FDirection := 1;
          end;
        end;
      end;

    lmReverse:
      begin
        if FPosition <= FLoopStart then
          FPosition := FLoopEnd - (FLoopStart - FPosition);
      end;
  end;
end;

procedure TSedaiSamplePlayer.GenerateStereoSample(out ALeft, ARight: Single);
begin
  if not FPlaying or (FSampleData = nil) then
  begin
    ALeft := 0.0;
    ARight := 0.0;
    Exit;
  end;

  ALeft := ReadSampleInterpolated(FPosition, 0) * FAmplitude;

  if FSampleData.Channels >= 2 then
    ARight := ReadSampleInterpolated(FPosition, 1) * FAmplitude
  else
    ARight := ALeft;

  // Advance position (same logic as GenerateSample)
  FPosition := FPosition + FPlaybackRate * FDirection;

  // Handle loop/end
  case FLoopMode of
    lmNone:
      if FPosition >= FEndSample then
      begin
        FPlaying := False;
        FPosition := FEndSample;
      end;

    lmForward:
      if FPosition >= FLoopEnd then
        FPosition := FLoopStart + (FPosition - FLoopEnd);

    lmPingPong:
      begin
        if FDirection > 0 then
        begin
          if FPosition >= FLoopEnd then
          begin
            FPosition := FLoopEnd - (FPosition - FLoopEnd);
            FDirection := -1;
          end;
        end
        else if FPosition <= FLoopStart then
        begin
          FPosition := FLoopStart + (FLoopStart - FPosition);
          FDirection := 1;
        end;
      end;

    lmReverse:
      if FPosition <= FLoopStart then
        FPosition := FLoopEnd - (FLoopStart - FPosition);
  end;
end;

end.
