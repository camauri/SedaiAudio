{*
 * Sedai Audio Foundation - Audio Buffer
 *
 * TSedaiAudioBuffer provides managed audio buffer with support for
 * multiple channels, interleaved/non-interleaved formats, and
 * common audio operations like mixing, copying, and normalization.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAudioBuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes;

type
  { TSedaiAudioBuffer }
  // Managed audio buffer with multi-channel support
  TSedaiAudioBuffer = class
  private
    FData: array of Single;
    FChannels: Integer;
    FSampleCount: Integer;
    FSampleRate: Cardinal;
    FInterleaved: Boolean;
    FOwnsData: Boolean;

    function GetDataPtr: PSingle;
    function GetTotalSamples: Integer;
    function GetDuration: Double;

  public
    constructor Create;
    destructor Destroy; override;

    // ========================================================================
    // MEMORY MANAGEMENT
    // ========================================================================

    // Allocate buffer for specified channels and samples
    procedure Allocate(AChannels, ASampleCount: Integer);

    // Free buffer memory
    procedure Free;

    // Clear buffer (fill with zeros)
    procedure Clear;

    // Resize buffer (preserves existing data where possible)
    procedure Resize(ANewSampleCount: Integer);

    // ========================================================================
    // SAMPLE ACCESS
    // ========================================================================

    // Get sample value at channel and position
    function GetSample(AChannel, APosition: Integer): Single;

    // Set sample value at channel and position
    procedure SetSample(AChannel, APosition: Integer; AValue: Single);

    // Get pointer to channel data (for non-interleaved buffers)
    function GetChannelPtr(AChannel: Integer): PSingle;

    // Get pointer to frame (for interleaved buffers)
    function GetFramePtr(AFrame: Integer): PSingle;

    // ========================================================================
    // BUFFER OPERATIONS
    // ========================================================================

    // Copy from another buffer
    procedure CopyFrom(ASource: TSedaiAudioBuffer);

    // Copy from raw pointer
    procedure CopyFromPtr(ASource: PSingle; ASampleCount, AChannels: Integer;
                          ASourceInterleaved: Boolean);

    // Copy to raw pointer
    procedure CopyToPtr(ADest: PSingle; ASampleCount, AChannels: Integer;
                        ADestInterleaved: Boolean);

    // Mix another buffer into this one (additive)
    procedure Mix(ASource: TSedaiAudioBuffer; AGain: Single = 1.0);

    // Mix from raw pointer
    procedure MixFromPtr(ASource: PSingle; ASampleCount, AChannels: Integer;
                         AGain: Single = 1.0);

    // Apply gain to entire buffer
    procedure ApplyGain(AGain: Single);

    // Apply gain ramp (for fade in/out)
    procedure ApplyGainRamp(AStartGain, AEndGain: Single);

    // ========================================================================
    // FORMAT CONVERSION
    // ========================================================================

    // Convert between interleaved and non-interleaved
    procedure ConvertToInterleaved;
    procedure ConvertToNonInterleaved;

    // Convert mono to stereo (duplicate channel)
    procedure MonoToStereo;

    // Convert stereo to mono (average channels)
    procedure StereoToMono;

    // ========================================================================
    // ANALYSIS
    // ========================================================================

    // Get peak value (absolute maximum)
    function GetPeak: Single;

    // Get peak value for specific channel
    function GetChannelPeak(AChannel: Integer): Single;

    // Get RMS value
    function GetRMS: Single;

    // Get RMS value for specific channel
    function GetChannelRMS(AChannel: Integer): Single;

    // Check if buffer is silent (below threshold)
    function IsSilent(AThreshold: Single = SEDAI_SILENCE_THRESHOLD): Boolean;

    // ========================================================================
    // NORMALIZATION
    // ========================================================================

    // Normalize to specified peak level (default 1.0)
    procedure Normalize(ATargetPeak: Single = 1.0);

    // Normalize each channel independently
    procedure NormalizeChannels(ATargetPeak: Single = 1.0);

    // Hard clip samples to [-1, 1] range
    procedure Clip;

    // Soft clip with saturation
    procedure SoftClip(AThreshold: Single = 0.7);

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    // ========================================================================
    // COMPATIBILITY METHODS (for FileReader/FileWriter)
    // ========================================================================

    // Set format (sample rate and channels)
    procedure SetFormat(ASampleRate: Integer; AChannels: Integer);

    // Get channel data pointer (alias for GetChannelPtr with non-interleaved)
    function GetChannelData(AChannel: Integer): PSingle;

    // Read interleaved data from buffer
    procedure ReadInterleaved(ADest: PSingle; AOffset, AFrameCount: Integer);

    // Write interleaved data to buffer
    procedure WriteInterleaved(ASrc: PSingle; AOffset, AFrameCount: Integer);

    // Set size (alias for Resize with allocation)
    procedure SetSize(ASampleCount: Integer);

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    // Raw data pointer
    property Data: PSingle read GetDataPtr;

    // Number of channels
    property Channels: Integer read FChannels;

    // Number of samples per channel
    property SampleCount: Integer read FSampleCount;

    // Total samples (Channels * SampleCount)
    property TotalSamples: Integer read GetTotalSamples;

    // Sample rate
    property SampleRate: Cardinal read FSampleRate write FSampleRate;

    // True if data is interleaved (LRLRLR), False if planar (LLL...RRR...)
    property Interleaved: Boolean read FInterleaved;

    // Duration in seconds
    property Duration: Double read GetDuration;
  end;

implementation

{ TSedaiAudioBuffer }

constructor TSedaiAudioBuffer.Create;
begin
  inherited Create;

  SetLength(FData, 0);
  FChannels := 0;
  FSampleCount := 0;
  FSampleRate := SEDAI_DEFAULT_SAMPLE_RATE;
  FInterleaved := True;
  FOwnsData := True;
end;

destructor TSedaiAudioBuffer.Destroy;
begin
  Free;
  inherited Destroy;
end;

function TSedaiAudioBuffer.GetDataPtr: PSingle;
begin
  if Length(FData) > 0 then
    Result := @FData[0]
  else
    Result := nil;
end;

function TSedaiAudioBuffer.GetTotalSamples: Integer;
begin
  Result := FChannels * FSampleCount;
end;

function TSedaiAudioBuffer.GetDuration: Double;
begin
  if FSampleRate > 0 then
    Result := FSampleCount / FSampleRate
  else
    Result := 0.0;
end;

procedure TSedaiAudioBuffer.Allocate(AChannels, ASampleCount: Integer);
begin
  if (AChannels <= 0) or (ASampleCount <= 0) then
  begin
    Free;
    Exit;
  end;

  FChannels := AChannels;
  FSampleCount := ASampleCount;
  SetLength(FData, FChannels * FSampleCount);

  // Initialize to silence
  Clear;
end;

procedure TSedaiAudioBuffer.Free;
begin
  SetLength(FData, 0);
  FChannels := 0;
  FSampleCount := 0;
end;

procedure TSedaiAudioBuffer.Clear;
var
  I: Integer;
begin
  for I := 0 to High(FData) do
    FData[I] := 0.0;
end;

procedure TSedaiAudioBuffer.Resize(ANewSampleCount: Integer);
var
  OldData: array of Single;
  OldSampleCount: Integer;
  I, CopyCount: Integer;
begin
  if ANewSampleCount = FSampleCount then
    Exit;

  if ANewSampleCount <= 0 then
  begin
    Free;
    Exit;
  end;

  // Save old data
  OldData := Copy(FData);
  OldSampleCount := FSampleCount;

  // Reallocate
  FSampleCount := ANewSampleCount;
  SetLength(FData, FChannels * FSampleCount);

  // Clear new buffer
  Clear;

  // Copy old data
  CopyCount := Min(OldSampleCount, FSampleCount) * FChannels;
  for I := 0 to CopyCount - 1 do
    FData[I] := OldData[I];
end;

function TSedaiAudioBuffer.GetSample(AChannel, APosition: Integer): Single;
var
  Index: Integer;
begin
  Result := 0.0;

  if (AChannel < 0) or (AChannel >= FChannels) then
    Exit;
  if (APosition < 0) or (APosition >= FSampleCount) then
    Exit;

  if FInterleaved then
    Index := APosition * FChannels + AChannel
  else
    Index := AChannel * FSampleCount + APosition;

  Result := FData[Index];
end;

procedure TSedaiAudioBuffer.SetSample(AChannel, APosition: Integer; AValue: Single);
var
  Index: Integer;
begin
  if (AChannel < 0) or (AChannel >= FChannels) then
    Exit;
  if (APosition < 0) or (APosition >= FSampleCount) then
    Exit;

  if FInterleaved then
    Index := APosition * FChannels + AChannel
  else
    Index := AChannel * FSampleCount + APosition;

  FData[Index] := AValue;
end;

function TSedaiAudioBuffer.GetChannelPtr(AChannel: Integer): PSingle;
begin
  Result := nil;

  if (AChannel < 0) or (AChannel >= FChannels) then
    Exit;

  if FInterleaved then
    // For interleaved, return pointer to first sample of channel
    // (caller must handle stride)
    Result := @FData[AChannel]
  else
    // For non-interleaved, return pointer to channel data
    Result := @FData[AChannel * FSampleCount];
end;

function TSedaiAudioBuffer.GetFramePtr(AFrame: Integer): PSingle;
begin
  Result := nil;

  if (AFrame < 0) or (AFrame >= FSampleCount) then
    Exit;

  if FInterleaved then
    Result := @FData[AFrame * FChannels]
  else
    // For non-interleaved, this doesn't make as much sense
    Result := @FData[AFrame];
end;

procedure TSedaiAudioBuffer.CopyFrom(ASource: TSedaiAudioBuffer);
var
  I: Integer;
  CopyCount: Integer;
begin
  if ASource = nil then
    Exit;

  // Reallocate if necessary
  if (FChannels <> ASource.FChannels) or (FSampleCount <> ASource.FSampleCount) then
    Allocate(ASource.FChannels, ASource.FSampleCount);

  FInterleaved := ASource.FInterleaved;
  FSampleRate := ASource.FSampleRate;

  // Copy data
  CopyCount := Length(FData);
  if CopyCount > Length(ASource.FData) then
    CopyCount := Length(ASource.FData);

  for I := 0 to CopyCount - 1 do
    FData[I] := ASource.FData[I];
end;

procedure TSedaiAudioBuffer.CopyFromPtr(ASource: PSingle; ASampleCount, AChannels: Integer;
                                        ASourceInterleaved: Boolean);
var
  I, J: Integer;
  SrcIndex, DstIndex: Integer;
begin
  if ASource = nil then
    Exit;

  // Reallocate if necessary
  if (FChannels <> AChannels) or (FSampleCount <> ASampleCount) then
    Allocate(AChannels, ASampleCount);

  // Same format - direct copy
  if ASourceInterleaved = FInterleaved then
  begin
    for I := 0 to GetTotalSamples - 1 do
      FData[I] := ASource[I];
  end
  else
  begin
    // Format conversion required
    for J := 0 to FChannels - 1 do
    begin
      for I := 0 to FSampleCount - 1 do
      begin
        if ASourceInterleaved then
          SrcIndex := I * AChannels + J
        else
          SrcIndex := J * ASampleCount + I;

        if FInterleaved then
          DstIndex := I * FChannels + J
        else
          DstIndex := J * FSampleCount + I;

        FData[DstIndex] := ASource[SrcIndex];
      end;
    end;
  end;
end;

procedure TSedaiAudioBuffer.CopyToPtr(ADest: PSingle; ASampleCount, AChannels: Integer;
                                      ADestInterleaved: Boolean);
var
  I, J: Integer;
  SrcIndex, DstIndex: Integer;
  CopySamples, CopyChannels: Integer;
begin
  if ADest = nil then
    Exit;

  CopySamples := Min(FSampleCount, ASampleCount);
  CopyChannels := Min(FChannels, AChannels);

  // Same format - direct copy
  if ADestInterleaved = FInterleaved then
  begin
    for I := 0 to CopySamples * CopyChannels - 1 do
      ADest[I] := FData[I];
  end
  else
  begin
    // Format conversion required
    for J := 0 to CopyChannels - 1 do
    begin
      for I := 0 to CopySamples - 1 do
      begin
        if FInterleaved then
          SrcIndex := I * FChannels + J
        else
          SrcIndex := J * FSampleCount + I;

        if ADestInterleaved then
          DstIndex := I * AChannels + J
        else
          DstIndex := J * ASampleCount + I;

        ADest[DstIndex] := FData[SrcIndex];
      end;
    end;
  end;
end;

procedure TSedaiAudioBuffer.Mix(ASource: TSedaiAudioBuffer; AGain: Single);
var
  I: Integer;
  MixCount: Integer;
begin
  if ASource = nil then
    Exit;

  // Must have same format
  if (ASource.FChannels <> FChannels) or (ASource.FInterleaved <> FInterleaved) then
    Exit;

  MixCount := Min(GetTotalSamples, ASource.GetTotalSamples);

  for I := 0 to MixCount - 1 do
    FData[I] := FData[I] + ASource.FData[I] * AGain;
end;

procedure TSedaiAudioBuffer.MixFromPtr(ASource: PSingle; ASampleCount, AChannels: Integer;
                                       AGain: Single);
var
  I: Integer;
  MixCount: Integer;
begin
  if ASource = nil then
    Exit;

  // Must have same channel count (assumes same interleaving)
  if AChannels <> FChannels then
    Exit;

  MixCount := Min(GetTotalSamples, ASampleCount * AChannels);

  for I := 0 to MixCount - 1 do
    FData[I] := FData[I] + ASource[I] * AGain;
end;

procedure TSedaiAudioBuffer.ApplyGain(AGain: Single);
var
  I: Integer;
begin
  for I := 0 to High(FData) do
    FData[I] := FData[I] * AGain;
end;

procedure TSedaiAudioBuffer.ApplyGainRamp(AStartGain, AEndGain: Single);
var
  I, J: Integer;
  Gain, GainDelta: Single;
begin
  if FSampleCount <= 0 then
    Exit;

  GainDelta := (AEndGain - AStartGain) / FSampleCount;

  if FInterleaved then
  begin
    Gain := AStartGain;
    for I := 0 to FSampleCount - 1 do
    begin
      for J := 0 to FChannels - 1 do
        FData[I * FChannels + J] := FData[I * FChannels + J] * Gain;
      Gain := Gain + GainDelta;
    end;
  end
  else
  begin
    for J := 0 to FChannels - 1 do
    begin
      Gain := AStartGain;
      for I := 0 to FSampleCount - 1 do
      begin
        FData[J * FSampleCount + I] := FData[J * FSampleCount + I] * Gain;
        Gain := Gain + GainDelta;
      end;
    end;
  end;
end;

procedure TSedaiAudioBuffer.ConvertToInterleaved;
var
  TempData: array of Single;
  I, J: Integer;
begin
  if FInterleaved then
    Exit;

  TempData := Copy(FData);

  for J := 0 to FChannels - 1 do
  begin
    for I := 0 to FSampleCount - 1 do
      FData[I * FChannels + J] := TempData[J * FSampleCount + I];
  end;

  FInterleaved := True;
end;

procedure TSedaiAudioBuffer.ConvertToNonInterleaved;
var
  TempData: array of Single;
  I, J: Integer;
begin
  if not FInterleaved then
    Exit;

  TempData := Copy(FData);

  for J := 0 to FChannels - 1 do
  begin
    for I := 0 to FSampleCount - 1 do
      FData[J * FSampleCount + I] := TempData[I * FChannels + J];
  end;

  FInterleaved := False;
end;

procedure TSedaiAudioBuffer.MonoToStereo;
var
  TempData: array of Single;
  I: Integer;
begin
  if FChannels <> 1 then
    Exit;

  TempData := Copy(FData);
  FChannels := 2;
  SetLength(FData, FChannels * FSampleCount);
  FInterleaved := True;

  for I := 0 to FSampleCount - 1 do
  begin
    FData[I * 2] := TempData[I];      // Left
    FData[I * 2 + 1] := TempData[I];  // Right
  end;
end;

procedure TSedaiAudioBuffer.StereoToMono;
var
  TempData: array of Single;
  I: Integer;
begin
  if FChannels <> 2 then
    Exit;

  TempData := Copy(FData);
  FChannels := 1;
  SetLength(FData, FSampleCount);
  FInterleaved := True;

  if Length(TempData) = FSampleCount * 2 then
  begin
    for I := 0 to FSampleCount - 1 do
      FData[I] := (TempData[I * 2] + TempData[I * 2 + 1]) * 0.5;
  end;
end;

function TSedaiAudioBuffer.GetPeak: Single;
var
  I: Integer;
  AbsVal: Single;
begin
  Result := 0.0;

  for I := 0 to High(FData) do
  begin
    AbsVal := Abs(FData[I]);
    if AbsVal > Result then
      Result := AbsVal;
  end;
end;

function TSedaiAudioBuffer.GetChannelPeak(AChannel: Integer): Single;
var
  I: Integer;
  AbsVal: Single;
  Index: Integer;
begin
  Result := 0.0;

  if (AChannel < 0) or (AChannel >= FChannels) then
    Exit;

  for I := 0 to FSampleCount - 1 do
  begin
    if FInterleaved then
      Index := I * FChannels + AChannel
    else
      Index := AChannel * FSampleCount + I;

    AbsVal := Abs(FData[Index]);
    if AbsVal > Result then
      Result := AbsVal;
  end;
end;

function TSedaiAudioBuffer.GetRMS: Single;
var
  I: Integer;
  Sum: Double;
begin
  Result := 0.0;

  if Length(FData) = 0 then
    Exit;

  Sum := 0.0;
  for I := 0 to High(FData) do
    Sum := Sum + FData[I] * FData[I];

  Result := Sqrt(Sum / Length(FData));
end;

function TSedaiAudioBuffer.GetChannelRMS(AChannel: Integer): Single;
var
  I: Integer;
  Sum: Double;
  Index: Integer;
begin
  Result := 0.0;

  if (AChannel < 0) or (AChannel >= FChannels) then
    Exit;

  if FSampleCount = 0 then
    Exit;

  Sum := 0.0;
  for I := 0 to FSampleCount - 1 do
  begin
    if FInterleaved then
      Index := I * FChannels + AChannel
    else
      Index := AChannel * FSampleCount + I;

    Sum := Sum + FData[Index] * FData[Index];
  end;

  Result := Sqrt(Sum / FSampleCount);
end;

function TSedaiAudioBuffer.IsSilent(AThreshold: Single): Boolean;
begin
  Result := GetPeak < AThreshold;
end;

procedure TSedaiAudioBuffer.Normalize(ATargetPeak: Single);
var
  Peak, Gain: Single;
begin
  Peak := GetPeak;

  if Peak > SEDAI_SILENCE_THRESHOLD then
  begin
    Gain := ATargetPeak / Peak;
    ApplyGain(Gain);
  end;
end;

procedure TSedaiAudioBuffer.NormalizeChannels(ATargetPeak: Single);
var
  J, I: Integer;
  Peak, Gain: Single;
  Index: Integer;
begin
  for J := 0 to FChannels - 1 do
  begin
    Peak := GetChannelPeak(J);

    if Peak > SEDAI_SILENCE_THRESHOLD then
    begin
      Gain := ATargetPeak / Peak;

      for I := 0 to FSampleCount - 1 do
      begin
        if FInterleaved then
          Index := I * FChannels + J
        else
          Index := J * FSampleCount + I;

        FData[Index] := FData[Index] * Gain;
      end;
    end;
  end;
end;

procedure TSedaiAudioBuffer.Clip;
var
  I: Integer;
begin
  for I := 0 to High(FData) do
  begin
    if FData[I] > 1.0 then
      FData[I] := 1.0
    else if FData[I] < -1.0 then
      FData[I] := -1.0;
  end;
end;

procedure TSedaiAudioBuffer.SoftClip(AThreshold: Single);
var
  I: Integer;
  Sample, AbsSample: Single;
begin
  for I := 0 to High(FData) do
  begin
    Sample := FData[I];
    AbsSample := Abs(Sample);

    if AbsSample > AThreshold then
    begin
      // Soft knee saturation
      if Sample > 0 then
        FData[I] := AThreshold + (1.0 - AThreshold) * Tanh((AbsSample - AThreshold) / (1.0 - AThreshold))
      else
        FData[I] := -(AThreshold + (1.0 - AThreshold) * Tanh((AbsSample - AThreshold) / (1.0 - AThreshold)));
    end;
  end;
end;

{ Compatibility methods for FileReader/FileWriter }

procedure TSedaiAudioBuffer.SetFormat(ASampleRate: Integer; AChannels: Integer);
begin
  FSampleRate := ASampleRate;
  if AChannels <> FChannels then
  begin
    FChannels := AChannels;
    if FSampleCount > 0 then
      SetLength(FData, FChannels * FSampleCount);
  end;
  // Use non-interleaved for easier channel access
  FInterleaved := False;
end;

function TSedaiAudioBuffer.GetChannelData(AChannel: Integer): PSingle;
begin
  Result := nil;

  if (AChannel < 0) or (AChannel >= FChannels) then
    Exit;

  if FInterleaved then
    ConvertToNonInterleaved;

  if FSampleCount > 0 then
    Result := @FData[AChannel * FSampleCount];
end;

procedure TSedaiAudioBuffer.ReadInterleaved(ADest: PSingle; AOffset, AFrameCount: Integer);
var
  I, J: Integer;
  SrcIndex: Integer;
begin
  if ADest = nil then Exit;
  if (AOffset < 0) or (AOffset + AFrameCount > FSampleCount) then Exit;

  if FInterleaved then
  begin
    // Direct copy
    Move(FData[AOffset * FChannels], ADest^, AFrameCount * FChannels * SizeOf(Single));
  end
  else
  begin
    // Convert non-interleaved to interleaved output
    for I := 0 to AFrameCount - 1 do
    begin
      for J := 0 to FChannels - 1 do
      begin
        SrcIndex := J * FSampleCount + AOffset + I;
        ADest[I * FChannels + J] := FData[SrcIndex];
      end;
    end;
  end;
end;

procedure TSedaiAudioBuffer.WriteInterleaved(ASrc: PSingle; AOffset, AFrameCount: Integer);
var
  I, J: Integer;
  DstIndex: Integer;
begin
  if ASrc = nil then Exit;

  // Ensure we have enough space
  if AOffset + AFrameCount > FSampleCount then
    Resize(AOffset + AFrameCount);

  if FInterleaved then
  begin
    // Direct copy
    Move(ASrc^, FData[AOffset * FChannels], AFrameCount * FChannels * SizeOf(Single));
  end
  else
  begin
    // Convert interleaved input to non-interleaved storage
    for I := 0 to AFrameCount - 1 do
    begin
      for J := 0 to FChannels - 1 do
      begin
        DstIndex := J * FSampleCount + AOffset + I;
        FData[DstIndex] := ASrc[I * FChannels + J];
      end;
    end;
  end;
end;

procedure TSedaiAudioBuffer.SetSize(ASampleCount: Integer);
begin
  if ASampleCount <= 0 then
  begin
    Free;
    Exit;
  end;

  if FChannels <= 0 then
    FChannels := 2;  // Default to stereo

  FSampleCount := ASampleCount;
  SetLength(FData, FChannels * FSampleCount);
  Clear;
end;

end.
