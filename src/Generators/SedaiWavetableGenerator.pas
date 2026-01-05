{*
 * Sedai Audio Foundation - Wavetable Generator
 *
 * TSedaiWavetableGenerator provides wavetable synthesis with
 * morphing between waveforms, multiple wavetables, and
 * per-sample position modulation.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiWavetableGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiOscillator;

const
  DEFAULT_WAVETABLE_SIZE = 2048;
  MAX_WAVETABLES = 256;
  MAX_MIPMAP_LEVELS = 10;

type
  // Single wavetable data
  TWavetableData = array of Single;

  // Wavetable with multiple mipmap levels for anti-aliasing
  TWavetableMipmap = record
    Data: array[0..MAX_MIPMAP_LEVELS-1] of TWavetableData;
    Levels: Integer;
    Size: Integer;
  end;

  // Interpolation mode for wavetable reading
  TWavetableInterp = (
    wtInterpNone,           // Nearest sample
    wtInterpLinear,         // Linear interpolation
    wtInterpCubic           // Cubic interpolation
  );

  { TSedaiWavetable }
  // Container for wavetable frames (morphable wavetables)
  TSedaiWavetable = class
  private
    FFrames: array of TWavetableMipmap;
    FFrameCount: Integer;
    FTableSize: Integer;
    FName: string;

    procedure GenerateMipmaps(AFrameIndex: Integer);

  public
    constructor Create(ATableSize: Integer = DEFAULT_WAVETABLE_SIZE);
    destructor Destroy; override;

    // Add a frame from raw data
    function AddFrame(const AData: array of Single): Integer;

    // Add a frame by generating waveform
    function AddSineFrame: Integer;
    function AddSawFrame: Integer;
    function AddSquareFrame: Integer;
    function AddTriangleFrame: Integer;

    // Add frame from harmonic series
    function AddHarmonicFrame(const AHarmonics: array of Single): Integer;

    // Get sample with mipmap selection
    function GetSample(AFrame: Integer; APhase: Double; AMipmapLevel: Integer): Single;

    // Get interpolated sample between frames
    function GetSampleMorphed(AFramePos: Double; APhase: Double; AMipmapLevel: Integer): Single;

    // Clear all frames
    procedure Clear;

    property FrameCount: Integer read FFrameCount;
    property TableSize: Integer read FTableSize;
    property Name: string read FName write FName;
  end;

  { TSedaiWavetableGenerator }
  // Wavetable oscillator with morphing
  TSedaiWavetableGenerator = class(TSedaiSignalGenerator)
  private
    FWavetable: TSedaiWavetable;
    FOwnsWavetable: Boolean;

    // Note: FPhase and FPhaseIncrement are inherited from TSedaiSignalGenerator

    FFramePosition: Double;       // Current frame (fractional for morphing)
    FFrameModulation: Single;     // External frame modulation

    FInterpolation: TWavetableInterp;
    FAntiAlias: Boolean;          // Use mipmaps for anti-aliasing

    FUnisonVoices: Integer;
    FUnisonDetune: Single;        // Cents
    FUnisonSpread: Single;        // Stereo spread
    FUnisonPhases: array of Double;

    procedure SetFrequency(AValue: Single); reintroduce;
    procedure CalculatePhaseIncrement;
    function GetMipmapLevel: Integer;
    function LinearInterpolate(A, B, Frac: Single): Single;
    function CubicInterpolate(Y0, Y1, Y2, Y3, Frac: Single): Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // Load wavetable
    procedure LoadWavetable(ATable: TSedaiWavetable; ATakeOwnership: Boolean = False);

    // Create built-in wavetable
    procedure CreateBasicWavetable;
    procedure CreatePWMWavetable(ASteps: Integer = 64);
    procedure CreateSuperSawWavetable(ASteps: Integer = 32);

    // Generate sample
    function GenerateSample: Single; override;

    // Generate stereo with unison spread
    procedure GenerateStereoSample(out ALeft, ARight: Single);

    // Properties
    property Wavetable: TSedaiWavetable read FWavetable;
    // Note: Phase is inherited from TSedaiSignalGenerator (Single type)
    property FramePosition: Double read FFramePosition write FFramePosition;
    property FrameModulation: Single read FFrameModulation write FFrameModulation;
    property Interpolation: TWavetableInterp read FInterpolation write FInterpolation;
    property AntiAlias: Boolean read FAntiAlias write FAntiAlias;
    property UnisonVoices: Integer read FUnisonVoices write FUnisonVoices;
    property UnisonDetune: Single read FUnisonDetune write FUnisonDetune;
    property UnisonSpread: Single read FUnisonSpread write FUnisonSpread;
  end;

implementation

{ TSedaiWavetable }

constructor TSedaiWavetable.Create(ATableSize: Integer);
begin
  inherited Create;

  FTableSize := ATableSize;
  FFrameCount := 0;
  FName := 'Untitled';
  SetLength(FFrames, 0);
end;

destructor TSedaiWavetable.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSedaiWavetable.GenerateMipmaps(AFrameIndex: Integer);
var
  Level, I, J, SrcSize, DstSize: Integer;
  Src, Dst: TWavetableData;
begin
  if (AFrameIndex < 0) or (AFrameIndex >= FFrameCount) then Exit;

  // Level 0 is already set (original data)
  FFrames[AFrameIndex].Levels := 1;

  SrcSize := FTableSize;
  Level := 1;

  while (SrcSize > 4) and (Level < MAX_MIPMAP_LEVELS) do
  begin
    DstSize := SrcSize div 2;
    SetLength(FFrames[AFrameIndex].Data[Level], DstSize);

    Src := FFrames[AFrameIndex].Data[Level - 1];
    Dst := FFrames[AFrameIndex].Data[Level];

    // Simple averaging filter for downsampling
    for I := 0 to DstSize - 1 do
    begin
      J := I * 2;
      Dst[I] := (Src[J] + Src[J + 1]) * 0.5;
    end;

    FFrames[AFrameIndex].Levels := Level + 1;
    SrcSize := DstSize;
    Inc(Level);
  end;
end;

function TSedaiWavetable.AddFrame(const AData: array of Single): Integer;
var
  I, Len: Integer;
begin
  Result := FFrameCount;
  Inc(FFrameCount);
  SetLength(FFrames, FFrameCount);

  FFrames[Result].Size := FTableSize;

  // Copy data (resize if needed)
  SetLength(FFrames[Result].Data[0], FTableSize);
  Len := Length(AData);

  for I := 0 to FTableSize - 1 do
  begin
    if I < Len then
      FFrames[Result].Data[0][I] := AData[I]
    else
      FFrames[Result].Data[0][I] := 0.0;
  end;

  // Generate mipmaps for anti-aliasing
  GenerateMipmaps(Result);
end;

function TSedaiWavetable.AddSineFrame: Integer;
var
  Data: TWavetableData;
  I: Integer;
begin
  SetLength(Data, FTableSize);

  for I := 0 to FTableSize - 1 do
    Data[I] := Sin(2.0 * PI * I / FTableSize);

  Result := AddFrame(Data);
end;

function TSedaiWavetable.AddSawFrame: Integer;
var
  Data: TWavetableData;
  I: Integer;
  Phase: Double;
begin
  SetLength(Data, FTableSize);

  for I := 0 to FTableSize - 1 do
  begin
    Phase := I / FTableSize;
    Data[I] := 2.0 * Phase - 1.0;
  end;

  Result := AddFrame(Data);
end;

function TSedaiWavetable.AddSquareFrame: Integer;
var
  Data: TWavetableData;
  I: Integer;
begin
  SetLength(Data, FTableSize);

  for I := 0 to FTableSize - 1 do
  begin
    if I < FTableSize div 2 then
      Data[I] := 1.0
    else
      Data[I] := -1.0;
  end;

  Result := AddFrame(Data);
end;

function TSedaiWavetable.AddTriangleFrame: Integer;
var
  Data: TWavetableData;
  I: Integer;
  Phase: Double;
begin
  SetLength(Data, FTableSize);

  for I := 0 to FTableSize - 1 do
  begin
    Phase := I / FTableSize;
    if Phase < 0.25 then
      Data[I] := 4.0 * Phase
    else if Phase < 0.75 then
      Data[I] := 2.0 - 4.0 * Phase
    else
      Data[I] := -4.0 + 4.0 * Phase;
  end;

  Result := AddFrame(Data);
end;

function TSedaiWavetable.AddHarmonicFrame(const AHarmonics: array of Single): Integer;
var
  Data: TWavetableData;
  I, H: Integer;
  Phase: Double;
  Sum: Single;
begin
  SetLength(Data, FTableSize);

  for I := 0 to FTableSize - 1 do
  begin
    Phase := 2.0 * PI * I / FTableSize;
    Sum := 0.0;

    for H := 0 to High(AHarmonics) do
      Sum := Sum + AHarmonics[H] * Sin((H + 1) * Phase);

    Data[I] := Sum;
  end;

  // Normalize
  Sum := 0.0;
  for I := 0 to FTableSize - 1 do
    if Abs(Data[I]) > Sum then Sum := Abs(Data[I]);

  if Sum > 0.001 then
    for I := 0 to FTableSize - 1 do
      Data[I] := Data[I] / Sum;

  Result := AddFrame(Data);
end;

function TSedaiWavetable.GetSample(AFrame: Integer; APhase: Double; AMipmapLevel: Integer): Single;
var
  Index: Integer;
  Frac: Double;
  TblSize: Integer;
  Data: TWavetableData;
begin
  Result := 0.0;

  if (FFrameCount = 0) or (AFrame < 0) then Exit;
  if AFrame >= FFrameCount then AFrame := FFrameCount - 1;

  // Clamp mipmap level
  if AMipmapLevel < 0 then AMipmapLevel := 0;
  if AMipmapLevel >= FFrames[AFrame].Levels then
    AMipmapLevel := FFrames[AFrame].Levels - 1;

  Data := FFrames[AFrame].Data[AMipmapLevel];
  TblSize := Length(Data);

  if TblSize = 0 then Exit;

  // Wrap phase
  while APhase < 0 do APhase := APhase + 1.0;
  while APhase >= 1.0 do APhase := APhase - 1.0;

  // Calculate index
  Index := Trunc(APhase * TblSize);
  Frac := APhase * TblSize - Index;

  // Linear interpolation
  if Index >= TblSize - 1 then
    Result := Data[Index] * (1.0 - Frac) + Data[0] * Frac
  else
    Result := Data[Index] * (1.0 - Frac) + Data[Index + 1] * Frac;
end;

function TSedaiWavetable.GetSampleMorphed(AFramePos: Double; APhase: Double; AMipmapLevel: Integer): Single;
var
  Frame1, Frame2: Integer;
  Frac: Single;
  S1, S2: Single;
begin
  if FFrameCount = 0 then
  begin
    Result := 0.0;
    Exit;
  end;

  if FFrameCount = 1 then
  begin
    Result := GetSample(0, APhase, AMipmapLevel);
    Exit;
  end;

  // Clamp frame position
  if AFramePos < 0 then AFramePos := 0;
  if AFramePos >= FFrameCount then AFramePos := FFrameCount - 0.0001;

  Frame1 := Trunc(AFramePos);
  Frame2 := Frame1 + 1;
  if Frame2 >= FFrameCount then Frame2 := FFrameCount - 1;

  Frac := AFramePos - Frame1;

  S1 := GetSample(Frame1, APhase, AMipmapLevel);
  S2 := GetSample(Frame2, APhase, AMipmapLevel);

  Result := S1 + (S2 - S1) * Frac;
end;

procedure TSedaiWavetable.Clear;
var
  I, L: Integer;
begin
  for I := 0 to FFrameCount - 1 do
    for L := 0 to FFrames[I].Levels - 1 do
      SetLength(FFrames[I].Data[L], 0);

  SetLength(FFrames, 0);
  FFrameCount := 0;
end;

{ TSedaiWavetableGenerator }

constructor TSedaiWavetableGenerator.Create;
begin
  inherited Create;

  FWavetable := nil;
  FOwnsWavetable := False;

  FPhase := 0.0;
  FPhaseIncrement := 0.0;

  FFramePosition := 0.0;
  FFrameModulation := 0.0;

  FInterpolation := wtInterpLinear;
  FAntiAlias := True;

  FUnisonVoices := 1;
  FUnisonDetune := 15.0;
  FUnisonSpread := 0.5;
end;

destructor TSedaiWavetableGenerator.Destroy;
begin
  if FOwnsWavetable and Assigned(FWavetable) then
    FWavetable.Free;

  inherited Destroy;
end;

procedure TSedaiWavetableGenerator.Reset;
var
  I: Integer;
begin
  inherited Reset;

  FPhase := 0.0;

  for I := 0 to High(FUnisonPhases) do
    FUnisonPhases[I] := Random;
end;

procedure TSedaiWavetableGenerator.SetFrequency(AValue: Single);
begin
  FFrequency := AValue;
  CalculatePhaseIncrement;
end;

procedure TSedaiWavetableGenerator.CalculatePhaseIncrement;
begin
  if FSampleRate > 0 then
    FPhaseIncrement := FFrequency / FSampleRate
  else
    FPhaseIncrement := 0;
end;

function TSedaiWavetableGenerator.GetMipmapLevel: Integer;
var
  SamplesPerCycle: Double;
begin
  Result := 0;

  if (FSampleRate <= 0) or (FFrequency <= 0) then Exit;

  SamplesPerCycle := FSampleRate / FFrequency;

  // Calculate mipmap level based on samples per cycle
  // Higher frequencies need lower resolution tables
  if FWavetable <> nil then
  begin
    while (SamplesPerCycle < FWavetable.TableSize shr Result) and
          (Result < MAX_MIPMAP_LEVELS - 1) do
      Inc(Result);
  end;
end;

function TSedaiWavetableGenerator.LinearInterpolate(A, B, Frac: Single): Single;
begin
  Result := A + (B - A) * Frac;
end;

function TSedaiWavetableGenerator.CubicInterpolate(Y0, Y1, Y2, Y3, Frac: Single): Single;
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

procedure TSedaiWavetableGenerator.LoadWavetable(ATable: TSedaiWavetable; ATakeOwnership: Boolean);
begin
  if FOwnsWavetable and Assigned(FWavetable) then
    FWavetable.Free;

  FWavetable := ATable;
  FOwnsWavetable := ATakeOwnership;
end;

procedure TSedaiWavetableGenerator.CreateBasicWavetable;
begin
  if FOwnsWavetable and Assigned(FWavetable) then
    FWavetable.Free;

  FWavetable := TSedaiWavetable.Create(DEFAULT_WAVETABLE_SIZE);
  FOwnsWavetable := True;

  FWavetable.Name := 'Basic';
  FWavetable.AddSineFrame;      // 0
  FWavetable.AddTriangleFrame;  // 1
  FWavetable.AddSawFrame;       // 2
  FWavetable.AddSquareFrame;    // 3
end;

procedure TSedaiWavetableGenerator.CreatePWMWavetable(ASteps: Integer);
var
  I, J: Integer;
  Data: TWavetableData;
  PulseWidth: Single;
begin
  if FOwnsWavetable and Assigned(FWavetable) then
    FWavetable.Free;

  FWavetable := TSedaiWavetable.Create(DEFAULT_WAVETABLE_SIZE);
  FOwnsWavetable := True;
  FWavetable.Name := 'PWM';

  SetLength(Data, DEFAULT_WAVETABLE_SIZE);

  for I := 0 to ASteps - 1 do
  begin
    PulseWidth := 0.05 + 0.9 * (I / (ASteps - 1));

    for J := 0 to DEFAULT_WAVETABLE_SIZE - 1 do
    begin
      if J < Round(DEFAULT_WAVETABLE_SIZE * PulseWidth) then
        Data[J] := 1.0
      else
        Data[J] := -1.0;
    end;

    FWavetable.AddFrame(Data);
  end;
end;

procedure TSedaiWavetableGenerator.CreateSuperSawWavetable(ASteps: Integer);
var
  I, J, V: Integer;
  Data: TWavetableData;
  LocalPhase, Detune, Sum: Single;
  NumVoices: Integer;
begin
  if FOwnsWavetable and Assigned(FWavetable) then
    FWavetable.Free;

  FWavetable := TSedaiWavetable.Create(DEFAULT_WAVETABLE_SIZE);
  FOwnsWavetable := True;
  FWavetable.Name := 'SuperSaw';

  SetLength(Data, DEFAULT_WAVETABLE_SIZE);
  NumVoices := 7;

  for I := 0 to ASteps - 1 do
  begin
    Detune := 0.001 + 0.05 * (I / (ASteps - 1));  // Detune amount

    for J := 0 to DEFAULT_WAVETABLE_SIZE - 1 do
    begin
      Sum := 0.0;
      LocalPhase := J / DEFAULT_WAVETABLE_SIZE;

      for V := 0 to NumVoices - 1 do
      begin
        // Each voice has slightly different phase based on detune
        Sum := Sum + (2.0 * Frac(LocalPhase * (1.0 + (V - NumVoices div 2) * Detune)) - 1.0);
      end;

      Data[J] := Sum / NumVoices;
    end;

    FWavetable.AddFrame(Data);
  end;
end;

function TSedaiWavetableGenerator.GenerateSample: Single;
var
  FramePos: Double;
  MipLevel: Integer;
begin
  Result := 0.0;

  if FWavetable = nil then Exit;
  if FWavetable.FrameCount = 0 then Exit;

  // Calculate effective frame position
  FramePos := FFramePosition + FFrameModulation * FWavetable.FrameCount;
  if FramePos < 0 then FramePos := 0;
  if FramePos >= FWavetable.FrameCount then
    FramePos := FWavetable.FrameCount - 0.0001;

  // Get mipmap level for anti-aliasing
  if FAntiAlias then
    MipLevel := GetMipmapLevel
  else
    MipLevel := 0;

  // Get sample
  Result := FWavetable.GetSampleMorphed(FramePos, FPhase, MipLevel);
  Result := Result * FAmplitude;

  // Advance phase
  FPhase := FPhase + FPhaseIncrement;
  if FPhase >= 1.0 then
    FPhase := FPhase - 1.0;
end;

procedure TSedaiWavetableGenerator.GenerateStereoSample(out ALeft, ARight: Single);
var
  I: Integer;
  FramePos: Double;
  MipLevel: Integer;
  Sample: Single;
  DetuneRatio, VoicePhase, VoiceInc: Double;
  Pan, PanL, PanR: Single;
  SumL, SumR: Single;
begin
  ALeft := 0.0;
  ARight := 0.0;

  if FWavetable = nil then Exit;
  if FWavetable.FrameCount = 0 then Exit;

  // Setup unison phases if needed
  if Length(FUnisonPhases) < FUnisonVoices then
  begin
    SetLength(FUnisonPhases, FUnisonVoices);
    for I := 0 to FUnisonVoices - 1 do
      FUnisonPhases[I] := Random;
  end;

  FramePos := FFramePosition + FFrameModulation * FWavetable.FrameCount;
  if FramePos < 0 then FramePos := 0;
  if FramePos >= FWavetable.FrameCount then
    FramePos := FWavetable.FrameCount - 0.0001;

  if FAntiAlias then
    MipLevel := GetMipmapLevel
  else
    MipLevel := 0;

  SumL := 0.0;
  SumR := 0.0;

  for I := 0 to FUnisonVoices - 1 do
  begin
    // Calculate detune for this voice
    if FUnisonVoices > 1 then
      DetuneRatio := Power(2.0, ((I - (FUnisonVoices - 1) / 2) *
                     FUnisonDetune / (FUnisonVoices - 1)) / 1200.0)
    else
      DetuneRatio := 1.0;

    VoicePhase := FUnisonPhases[I];
    VoiceInc := FPhaseIncrement * DetuneRatio;

    // Get sample for this voice
    Sample := FWavetable.GetSampleMorphed(FramePos, VoicePhase, MipLevel);

    // Calculate stereo pan for this voice
    if FUnisonVoices > 1 then
      Pan := (I / (FUnisonVoices - 1) - 0.5) * 2.0 * FUnisonSpread
    else
      Pan := 0.0;

    PanL := Cos((Pan + 1.0) * PI * 0.25);
    PanR := Sin((Pan + 1.0) * PI * 0.25);

    SumL := SumL + Sample * PanL;
    SumR := SumR + Sample * PanR;

    // Advance this voice's phase
    FUnisonPhases[I] := FUnisonPhases[I] + VoiceInc;
    if FUnisonPhases[I] >= 1.0 then
      FUnisonPhases[I] := FUnisonPhases[I] - 1.0;
  end;

  // Normalize by voice count
  ALeft := SumL / FUnisonVoices * FAmplitude;
  ARight := SumR / FUnisonVoices * FAmplitude;

  // Also advance main phase
  FPhase := FPhase + FPhaseIncrement;
  if FPhase >= 1.0 then
    FPhase := FPhase - 1.0;
end;

end.
