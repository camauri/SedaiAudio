{*
 * Sedai Audio Foundation - 3D Spatial Audio
 *
 * This unit provides 3D spatial audio positioning with:
 * - 3D position and listener management
 * - Distance-based attenuation (inverse, linear, exponential)
 * - HRTF-inspired binaural panning
 * - Doppler effect (optional)
 * - Room reverb integration hooks
 *
 * (c) 2024-2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiSpatialAudio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes;

const
  // Speed of sound in meters per second (at 20C)
  SPEED_OF_SOUND = 343.0;

  // Default spatial parameters
  DEFAULT_REFERENCE_DISTANCE = 1.0;
  DEFAULT_MAX_DISTANCE = 100.0;
  DEFAULT_ROLLOFF_FACTOR = 1.0;
  DEFAULT_CONE_INNER_ANGLE = 360.0;
  DEFAULT_CONE_OUTER_ANGLE = 360.0;
  DEFAULT_CONE_OUTER_GAIN = 0.0;

type
  // ============================================================================
  // 3D POSITION TYPES
  // ============================================================================

  // 3D Vector for position, velocity, and orientation
  TVector3 = record
    X, Y, Z: Single;
  end;
  PVector3 = ^TVector3;

  // Listener orientation (forward + up vectors)
  TListenerOrientation = record
    Forward: TVector3;  // Direction listener is facing
    Up: TVector3;       // Up direction
  end;

  // Distance attenuation model
  TDistanceModel = (
    dmNone,           // No attenuation
    dmInverse,        // 1/distance (realistic)
    dmInverseClamp,   // Clamped inverse
    dmLinear,         // Linear falloff
    dmLinearClamp,    // Clamped linear
    dmExponential,    // Exponential decay
    dmExponentialClamp // Clamped exponential
  );

  // ============================================================================
  // SPATIAL SOURCE
  // ============================================================================

  // Parameters for a spatial audio source
  TSpatialSourceParams = record
    Position: TVector3;
    Velocity: TVector3;        // For Doppler effect
    Direction: TVector3;       // For directional sources

    // Distance attenuation
    ReferenceDistance: Single; // Distance at which gain = 1
    MaxDistance: Single;       // Maximum distance (for clamped models)
    RolloffFactor: Single;     // Attenuation rolloff

    // Directional cone
    ConeInnerAngle: Single;    // Inner cone angle (full volume)
    ConeOuterAngle: Single;    // Outer cone angle
    ConeOuterGain: Single;     // Gain outside outer cone

    // Options
    DopplerEnabled: Boolean;
    DopplerFactor: Single;
  end;

  // ============================================================================
  // SPATIAL LISTENER
  // ============================================================================

  { TSedaiSpatialListener - Represents the listener in 3D space }
  TSedaiSpatialListener = class
  private
    FPosition: TVector3;
    FVelocity: TVector3;
    FOrientation: TListenerOrientation;
    FGain: Single;
    FSampleRate: Integer;
  public
    constructor Create(ASampleRate: Integer = SEDAI_DEFAULT_SAMPLE_RATE);

    procedure SetPosition(AX, AY, AZ: Single);
    procedure SetVelocity(AVX, AVY, AVZ: Single);
    procedure SetOrientation(AFwdX, AFwdY, AFwdZ, AUpX, AUpY, AUpZ: Single);

    property Position: TVector3 read FPosition;
    property Velocity: TVector3 read FVelocity;
    property Orientation: TListenerOrientation read FOrientation;
    property Gain: Single read FGain write FGain;
    property SampleRate: Integer read FSampleRate;
  end;

  // ============================================================================
  // SPATIAL PROCESSOR
  // ============================================================================

  { TSedaiSpatialProcessor - Processes mono input to stereo with 3D positioning }
  TSedaiSpatialProcessor = class
  private
    FListener: TSedaiSpatialListener;
    FParams: TSpatialSourceParams;
    FDistanceModel: TDistanceModel;
    FSampleRate: Integer;
    FEnabled: Boolean;

    // Calculated values (cached)
    FLastDistance: Single;
    FLastGain: Single;
    FLastPanLeft: Single;
    FLastPanRight: Single;

    // Delay line for HRTF ITD (interaural time difference)
    FDelayBufferLeft: array of Single;
    FDelayBufferRight: array of Single;
    FDelayWritePos: Integer;
    FDelayMaxSamples: Integer;

    // Low-pass filter coefficients for HRTF ILD (interaural level difference)
    FFilterCoeffLeft: Single;
    FFilterCoeffRight: Single;
    FFilterStateLeft: Single;
    FFilterStateRight: Single;

    function CalculateDistance: Single;
    function CalculateDistanceGain(ADistance: Single): Single;
    function CalculateConeGain: Single;
    procedure CalculatePanning(ADistance: Single; out ALeftGain, ARightGain: Single;
                               out ALeftDelay, ARightDelay: Integer;
                               out ALeftFilter, ARightFilter: Single);
    function CalculateDopplerPitch: Single;

    procedure UpdateDelayBuffers;
  public
    constructor Create(AListener: TSedaiSpatialListener);
    destructor Destroy; override;

    // Process a mono sample into stereo
    procedure Process(AInput: Single; out ALeft, ARight: Single);

    // Process a buffer of mono samples into stereo
    procedure ProcessBuffer(const AInput: array of Single;
                           var ALeft, ARight: array of Single;
                           ACount: Integer);

    // Set source position
    procedure SetPosition(AX, AY, AZ: Single);
    procedure SetVelocity(AVX, AVY, AVZ: Single);
    procedure SetDirection(ADX, ADY, ADZ: Single);

    // Distance parameters
    procedure SetDistanceParams(ARefDist, AMaxDist, ARolloff: Single);
    procedure SetConeParams(AInnerAngle, AOuterAngle, AOuterGain: Single);

    // Update cached calculations
    procedure Update;

    property Listener: TSedaiSpatialListener read FListener;
    property DistanceModel: TDistanceModel read FDistanceModel write FDistanceModel;
    property Enabled: Boolean read FEnabled write FEnabled;
    property DopplerEnabled: Boolean read FParams.DopplerEnabled write FParams.DopplerEnabled;
    property DopplerFactor: Single read FParams.DopplerFactor write FParams.DopplerFactor;

    // Read-only calculated values
    property Distance: Single read FLastDistance;
    property Gain: Single read FLastGain;
  end;

  // ============================================================================
  // SPATIAL AUDIO CONTEXT
  // ============================================================================

  { TSedaiSpatialContext - Global context for spatial audio }
  TSedaiSpatialContext = class
  private
    FListener: TSedaiSpatialListener;
    FProcessors: TList;
    FSampleRate: Integer;
    FDefaultDistanceModel: TDistanceModel;
    FSpeedOfSound: Single;
  public
    constructor Create(ASampleRate: Integer = SEDAI_DEFAULT_SAMPLE_RATE);
    destructor Destroy; override;

    // Create a new spatial processor attached to this context
    function CreateProcessor: TSedaiSpatialProcessor;
    procedure RemoveProcessor(AProcessor: TSedaiSpatialProcessor);

    property Listener: TSedaiSpatialListener read FListener;
    property SampleRate: Integer read FSampleRate;
    property DefaultDistanceModel: TDistanceModel read FDefaultDistanceModel write FDefaultDistanceModel;
    property SpeedOfSound: Single read FSpeedOfSound write FSpeedOfSound;
  end;

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

// Vector operations
function Vec3(AX, AY, AZ: Single): TVector3;
function Vec3Add(const A, B: TVector3): TVector3;
function Vec3Sub(const A, B: TVector3): TVector3;
function Vec3Scale(const V: TVector3; S: Single): TVector3;
function Vec3Dot(const A, B: TVector3): Single;
function Vec3Cross(const A, B: TVector3): TVector3;
function Vec3Length(const V: TVector3): Single;
function Vec3Normalize(const V: TVector3): TVector3;
function Vec3Distance(const A, B: TVector3): Single;

// Create default spatial source params
function DefaultSpatialSourceParams: TSpatialSourceParams;

implementation

// ============================================================================
// VECTOR OPERATIONS
// ============================================================================

function Vec3(AX, AY, AZ: Single): TVector3;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

function Vec3Add(const A, B: TVector3): TVector3;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

function Vec3Sub(const A, B: TVector3): TVector3;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

function Vec3Scale(const V: TVector3; S: Single): TVector3;
begin
  Result.X := V.X * S;
  Result.Y := V.Y * S;
  Result.Z := V.Z * S;
end;

function Vec3Dot(const A, B: TVector3): Single;
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B.Z;
end;

function Vec3Cross(const A, B: TVector3): TVector3;
begin
  Result.X := A.Y * B.Z - A.Z * B.Y;
  Result.Y := A.Z * B.X - A.X * B.Z;
  Result.Z := A.X * B.Y - A.Y * B.X;
end;

function Vec3Length(const V: TVector3): Single;
begin
  Result := Sqrt(V.X * V.X + V.Y * V.Y + V.Z * V.Z);
end;

function Vec3Normalize(const V: TVector3): TVector3;
var
  Len: Single;
begin
  Len := Vec3Length(V);
  if Len > 0.0001 then
    Result := Vec3Scale(V, 1.0 / Len)
  else
    Result := Vec3(0, 0, 1);  // Default forward
end;

function Vec3Distance(const A, B: TVector3): Single;
begin
  Result := Vec3Length(Vec3Sub(A, B));
end;

function DefaultSpatialSourceParams: TSpatialSourceParams;
begin
  Result.Position := Vec3(0, 0, 0);
  Result.Velocity := Vec3(0, 0, 0);
  Result.Direction := Vec3(0, 0, 1);  // Forward
  Result.ReferenceDistance := DEFAULT_REFERENCE_DISTANCE;
  Result.MaxDistance := DEFAULT_MAX_DISTANCE;
  Result.RolloffFactor := DEFAULT_ROLLOFF_FACTOR;
  Result.ConeInnerAngle := DEFAULT_CONE_INNER_ANGLE;
  Result.ConeOuterAngle := DEFAULT_CONE_OUTER_ANGLE;
  Result.ConeOuterGain := DEFAULT_CONE_OUTER_GAIN;
  Result.DopplerEnabled := False;
  Result.DopplerFactor := 1.0;
end;

// ============================================================================
// TSedaiSpatialListener
// ============================================================================

constructor TSedaiSpatialListener.Create(ASampleRate: Integer);
begin
  inherited Create;
  FSampleRate := ASampleRate;
  FPosition := Vec3(0, 0, 0);
  FVelocity := Vec3(0, 0, 0);
  FOrientation.Forward := Vec3(0, 0, -1);  // Looking down -Z (OpenAL convention)
  FOrientation.Up := Vec3(0, 1, 0);        // Up is +Y
  FGain := 1.0;
end;

procedure TSedaiSpatialListener.SetPosition(AX, AY, AZ: Single);
begin
  FPosition := Vec3(AX, AY, AZ);
end;

procedure TSedaiSpatialListener.SetVelocity(AVX, AVY, AVZ: Single);
begin
  FVelocity := Vec3(AVX, AVY, AVZ);
end;

procedure TSedaiSpatialListener.SetOrientation(AFwdX, AFwdY, AFwdZ, AUpX, AUpY, AUpZ: Single);
begin
  FOrientation.Forward := Vec3Normalize(Vec3(AFwdX, AFwdY, AFwdZ));
  FOrientation.Up := Vec3Normalize(Vec3(AUpX, AUpY, AUpZ));
end;

// ============================================================================
// TSedaiSpatialProcessor
// ============================================================================

constructor TSedaiSpatialProcessor.Create(AListener: TSedaiSpatialListener);
begin
  inherited Create;
  FListener := AListener;
  FSampleRate := AListener.SampleRate;
  FParams := DefaultSpatialSourceParams;
  FDistanceModel := dmInverseClamp;
  FEnabled := True;

  FLastDistance := 0;
  FLastGain := 1.0;
  FLastPanLeft := 0.707;  // -3dB center
  FLastPanRight := 0.707;

  // Initialize delay buffers for ITD (max ~0.7ms for 180 degree azimuth)
  FDelayMaxSamples := Round(0.001 * FSampleRate);  // 1ms buffer
  SetLength(FDelayBufferLeft, FDelayMaxSamples);
  SetLength(FDelayBufferRight, FDelayMaxSamples);
  FDelayWritePos := 0;

  FFilterCoeffLeft := 1.0;
  FFilterCoeffRight := 1.0;
  FFilterStateLeft := 0;
  FFilterStateRight := 0;

  UpdateDelayBuffers;
end;

destructor TSedaiSpatialProcessor.Destroy;
begin
  SetLength(FDelayBufferLeft, 0);
  SetLength(FDelayBufferRight, 0);
  inherited Destroy;
end;

procedure TSedaiSpatialProcessor.UpdateDelayBuffers;
var
  I: Integer;
begin
  for I := 0 to FDelayMaxSamples - 1 do
  begin
    FDelayBufferLeft[I] := 0;
    FDelayBufferRight[I] := 0;
  end;
end;

function TSedaiSpatialProcessor.CalculateDistance: Single;
begin
  Result := Vec3Distance(FParams.Position, FListener.Position);
  if Result < 0.0001 then
    Result := 0.0001;  // Avoid division by zero
end;

function TSedaiSpatialProcessor.CalculateDistanceGain(ADistance: Single): Single;
var
  RefDist, MaxDist, Rolloff: Single;
  ClampedDist: Single;
begin
  RefDist := FParams.ReferenceDistance;
  MaxDist := FParams.MaxDistance;
  Rolloff := FParams.RolloffFactor;

  case FDistanceModel of
    dmNone:
      Result := 1.0;

    dmInverse:
      Result := RefDist / (RefDist + Rolloff * (ADistance - RefDist));

    dmInverseClamp:
      begin
        ClampedDist := Max(ADistance, RefDist);
        ClampedDist := Min(ClampedDist, MaxDist);
        Result := RefDist / (RefDist + Rolloff * (ClampedDist - RefDist));
      end;

    dmLinear:
      Result := 1.0 - Rolloff * (ADistance - RefDist) / (MaxDist - RefDist);

    dmLinearClamp:
      begin
        ClampedDist := Max(ADistance, RefDist);
        ClampedDist := Min(ClampedDist, MaxDist);
        Result := 1.0 - Rolloff * (ClampedDist - RefDist) / (MaxDist - RefDist);
      end;

    dmExponential:
      Result := Power(ADistance / RefDist, -Rolloff);

    dmExponentialClamp:
      begin
        ClampedDist := Max(ADistance, RefDist);
        ClampedDist := Min(ClampedDist, MaxDist);
        Result := Power(ClampedDist / RefDist, -Rolloff);
      end;
  else
    Result := 1.0;
  end;

  Result := Max(0.0, Min(1.0, Result));
end;

function TSedaiSpatialProcessor.CalculateConeGain: Single;
var
  SourceToListener: TVector3;
  CosAngle, Angle: Single;
  InnerCos, OuterCos: Single;
begin
  // Omnidirectional if cone angles are 360
  if (FParams.ConeInnerAngle >= 360.0) and (FParams.ConeOuterAngle >= 360.0) then
  begin
    Result := 1.0;
    Exit;
  end;

  // Calculate angle between source direction and source->listener vector
  SourceToListener := Vec3Normalize(Vec3Sub(FListener.Position, FParams.Position));
  CosAngle := Vec3Dot(FParams.Direction, SourceToListener);
  Angle := ArcCos(Max(-1.0, Min(1.0, CosAngle))) * 180.0 / Pi;  // In degrees

  // Calculate gain based on cone
  if Angle <= FParams.ConeInnerAngle / 2 then
    Result := 1.0
  else if Angle >= FParams.ConeOuterAngle / 2 then
    Result := FParams.ConeOuterGain
  else
  begin
    // Interpolate between inner and outer cone
    InnerCos := FParams.ConeInnerAngle / 2;
    OuterCos := FParams.ConeOuterAngle / 2;
    Result := 1.0 + (FParams.ConeOuterGain - 1.0) * (Angle - InnerCos) / (OuterCos - InnerCos);
  end;
end;

procedure TSedaiSpatialProcessor.CalculatePanning(ADistance: Single;
  out ALeftGain, ARightGain: Single;
  out ALeftDelay, ARightDelay: Integer;
  out ALeftFilter, ARightFilter: Single);
var
  SourceToListener: TVector3;
  Right: TVector3;
  Azimuth: Single;
  PanAngle: Single;
  ITD: Single;
  DelaySamples: Single;
begin
  // Calculate vector from source to listener in listener space
  SourceToListener := Vec3Normalize(Vec3Sub(FParams.Position, FListener.Position));

  // Calculate right vector from listener orientation
  Right := Vec3Cross(FListener.Orientation.Forward, FListener.Orientation.Up);

  // Calculate azimuth (horizontal angle) - positive = right
  Azimuth := Vec3Dot(SourceToListener, Right);

  // Convert to pan angle (-1 = left, +1 = right)
  PanAngle := Azimuth;
  PanAngle := Max(-1.0, Min(1.0, PanAngle));

  // Constant power panning
  ALeftGain := Cos((PanAngle + 1.0) * Pi / 4.0);
  ARightGain := Sin((PanAngle + 1.0) * Pi / 4.0);

  // Calculate ITD (interaural time difference)
  // For a head of ~17cm diameter, max ITD is ~0.5ms
  ITD := 0.0005 * PanAngle;  // Simplified
  DelaySamples := Abs(ITD) * FSampleRate;
  DelaySamples := Min(DelaySamples, FDelayMaxSamples - 1);

  if PanAngle > 0 then
  begin
    // Source is to the right, delay left ear
    ALeftDelay := Round(DelaySamples);
    ARightDelay := 0;
  end
  else
  begin
    // Source is to the left, delay right ear
    ALeftDelay := 0;
    ARightDelay := Round(DelaySamples);
  end;

  // Calculate ILD (interaural level difference) via low-pass filtering
  // Sound from one side gets high-frequency shadowing on the opposite ear
  // Filter coefficient: 0 = heavy filtering, 1 = no filtering
  if PanAngle > 0 then
  begin
    // Source is right, filter left
    ALeftFilter := 0.7 + 0.3 * (1.0 - Abs(PanAngle));
    ARightFilter := 1.0;
  end
  else
  begin
    // Source is left, filter right
    ALeftFilter := 1.0;
    ARightFilter := 0.7 + 0.3 * (1.0 - Abs(PanAngle));
  end;
end;

function TSedaiSpatialProcessor.CalculateDopplerPitch: Single;
var
  SourceToListener: TVector3;
  SourceVelProjection, ListenerVelProjection: Single;
  SpeedOfSound: Single;
begin
  if not FParams.DopplerEnabled then
  begin
    Result := 1.0;
    Exit;
  end;

  SpeedOfSound := SPEED_OF_SOUND;

  // Calculate direction from source to listener
  SourceToListener := Vec3Normalize(Vec3Sub(FListener.Position, FParams.Position));

  // Project velocities onto source-listener axis
  SourceVelProjection := Vec3Dot(FParams.Velocity, SourceToListener);
  ListenerVelProjection := Vec3Dot(FListener.Velocity, SourceToListener);

  // Doppler formula
  Result := (SpeedOfSound + ListenerVelProjection * FParams.DopplerFactor) /
            (SpeedOfSound + SourceVelProjection * FParams.DopplerFactor);

  // Clamp to reasonable range
  Result := Max(0.5, Min(2.0, Result));
end;

procedure TSedaiSpatialProcessor.Update;
var
  LeftDelay, RightDelay: Integer;
begin
  FLastDistance := CalculateDistance;
  FLastGain := CalculateDistanceGain(FLastDistance) * CalculateConeGain * FListener.Gain;

  CalculatePanning(FLastDistance, FLastPanLeft, FLastPanRight,
                   LeftDelay, RightDelay, FFilterCoeffLeft, FFilterCoeffRight);
end;

procedure TSedaiSpatialProcessor.Process(AInput: Single; out ALeft, ARight: Single);
var
  Gained: Single;
  DelayedLeft, DelayedRight: Single;
  ReadPosLeft, ReadPosRight: Integer;
  LeftDelay, RightDelay: Integer;
  FilterL, FilterR: Single;
begin
  if not FEnabled then
  begin
    ALeft := AInput * 0.707;  // Simple center pan
    ARight := AInput * 0.707;
    Exit;
  end;

  // Apply distance gain
  Gained := AInput * FLastGain;

  // Calculate panning with ITD delays
  CalculatePanning(FLastDistance, FLastPanLeft, FLastPanRight,
                   LeftDelay, RightDelay, FilterL, FilterR);

  // Write to delay buffers
  FDelayBufferLeft[FDelayWritePos] := Gained;
  FDelayBufferRight[FDelayWritePos] := Gained;

  // Read from delayed positions
  ReadPosLeft := (FDelayWritePos - LeftDelay + FDelayMaxSamples) mod FDelayMaxSamples;
  ReadPosRight := (FDelayWritePos - RightDelay + FDelayMaxSamples) mod FDelayMaxSamples;

  DelayedLeft := FDelayBufferLeft[ReadPosLeft];
  DelayedRight := FDelayBufferRight[ReadPosRight];

  // Apply ILD filtering (simple one-pole low-pass)
  FFilterStateLeft := FFilterStateLeft + FilterL * (DelayedLeft - FFilterStateLeft);
  FFilterStateRight := FFilterStateRight + FilterR * (DelayedRight - FFilterStateRight);

  // Apply panning gains
  ALeft := FFilterStateLeft * FLastPanLeft;
  ARight := FFilterStateRight * FLastPanRight;

  // Advance write position
  FDelayWritePos := (FDelayWritePos + 1) mod FDelayMaxSamples;
end;

procedure TSedaiSpatialProcessor.ProcessBuffer(const AInput: array of Single;
  var ALeft, ARight: array of Single; ACount: Integer);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    Process(AInput[I], ALeft[I], ARight[I]);
end;

procedure TSedaiSpatialProcessor.SetPosition(AX, AY, AZ: Single);
begin
  FParams.Position := Vec3(AX, AY, AZ);
  Update;
end;

procedure TSedaiSpatialProcessor.SetVelocity(AVX, AVY, AVZ: Single);
begin
  FParams.Velocity := Vec3(AVX, AVY, AVZ);
end;

procedure TSedaiSpatialProcessor.SetDirection(ADX, ADY, ADZ: Single);
begin
  FParams.Direction := Vec3Normalize(Vec3(ADX, ADY, ADZ));
  Update;
end;

procedure TSedaiSpatialProcessor.SetDistanceParams(ARefDist, AMaxDist, ARolloff: Single);
begin
  FParams.ReferenceDistance := Max(0.0001, ARefDist);
  FParams.MaxDistance := Max(ARefDist, AMaxDist);
  FParams.RolloffFactor := Max(0.0, ARolloff);
  Update;
end;

procedure TSedaiSpatialProcessor.SetConeParams(AInnerAngle, AOuterAngle, AOuterGain: Single);
begin
  FParams.ConeInnerAngle := Max(0.0, Min(360.0, AInnerAngle));
  FParams.ConeOuterAngle := Max(FParams.ConeInnerAngle, Min(360.0, AOuterAngle));
  FParams.ConeOuterGain := Max(0.0, Min(1.0, AOuterGain));
  Update;
end;

// ============================================================================
// TSedaiSpatialContext
// ============================================================================

constructor TSedaiSpatialContext.Create(ASampleRate: Integer);
begin
  inherited Create;
  FSampleRate := ASampleRate;
  FListener := TSedaiSpatialListener.Create(ASampleRate);
  FProcessors := TList.Create;
  FDefaultDistanceModel := dmInverseClamp;
  FSpeedOfSound := SPEED_OF_SOUND;
end;

destructor TSedaiSpatialContext.Destroy;
var
  I: Integer;
begin
  // Note: We don't free processors here - they should be freed by their owners
  for I := FProcessors.Count - 1 downto 0 do
    TSedaiSpatialProcessor(FProcessors[I]).Free;
  FProcessors.Free;
  FListener.Free;
  inherited Destroy;
end;

function TSedaiSpatialContext.CreateProcessor: TSedaiSpatialProcessor;
begin
  Result := TSedaiSpatialProcessor.Create(FListener);
  Result.DistanceModel := FDefaultDistanceModel;
  FProcessors.Add(Result);
end;

procedure TSedaiSpatialContext.RemoveProcessor(AProcessor: TSedaiSpatialProcessor);
var
  Idx: Integer;
begin
  Idx := FProcessors.IndexOf(AProcessor);
  if Idx >= 0 then
    FProcessors.Delete(Idx);
end;

end.
