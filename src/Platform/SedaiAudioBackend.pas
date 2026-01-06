{*
 * Sedai Audio Foundation - Audio Backend
 *
 * TSedaiAudioBackend provides SDL2-based audio output.
 * Simple, clean implementation - SDL2 is ONLY for audio output.
 *
 * Supports two modes:
 * - Push mode (preferred): Use QueueSamples() to push audio data
 * - Callback mode: SDL2 calls your callback to fill buffers
 *
 * Push mode gives YOU control of timing - recommended for video sync.
 *
 * (c) 2024-2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAudioBackend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SDL2, SedaiAudioTypes;

type
  TBackendState = (
    bsUninitialized,
    bsStopped,
    bsRunning,
    bsPaused,
    bsError
  );

  TBackendMode = (
    bmPush,      // We push audio with QueueSamples() - WE control timing
    bmCallback   // SDL2 pulls audio via callback - SDL2 controls timing
  );

  // Audio callback: fills interleaved stereo float buffer
  TSedaiAudioCallback = procedure(AOutput: PSingle; AFrameCount: Integer;
                                  AUserData: Pointer);

  { TSedaiAudioBackend }
  TSedaiAudioBackend = class
  private
    FState: TBackendState;
    FMode: TBackendMode;
    FDeviceID: TSDL_AudioDeviceID;
    FSampleRate: Cardinal;
    FBufferSize: Integer;
    FChannels: Integer;
    FCallback: TSedaiAudioCallback;
    FUserData: Pointer;
    FCallbackCount: QWord;
    FQueuedSamples: QWord;

    procedure InternalCallback(AStream: Pointer; ALength: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    function Initialize: Boolean;
    procedure Shutdown;

    // Start in push mode (preferred) or callback mode
    function Start: Boolean;                    // Start in current mode
    function StartPushMode: Boolean;            // Start in push mode (no callback)
    function StartCallbackMode: Boolean;        // Start in callback mode
    procedure Stop;
    procedure Pause;
    procedure Resume;

    // Push mode: queue samples for playback
    // Data is interleaved float samples (L,R,L,R,... for stereo)
    function QueueSamples(AData: PSingle; ASampleCount: Integer): Boolean;

    // Get queued audio size in bytes (push mode)
    function GetQueuedAudioSize: Cardinal;

    // Clear queued audio (push mode)
    procedure ClearQueue;

    procedure SetCallback(ACallback: TSedaiAudioCallback; AUserData: Pointer);
    procedure SetSampleRate(AValue: Cardinal);
    procedure SetDesiredBufferSize(AValue: Integer);
    procedure SetChannels(AValue: Integer);
    procedure SetMode(AValue: TBackendMode);

    // Thread synchronization - use these when modifying audio state from main thread
    procedure Lock;
    procedure Unlock;

    property State: TBackendState read FState;
    property Mode: TBackendMode read FMode write SetMode;
    property SampleRate: Cardinal read FSampleRate;
    property BufferSize: Integer read FBufferSize;
    property Channels: Integer read FChannels;
    property CallbackCount: QWord read FCallbackCount;
    property QueuedSamples: QWord read FQueuedSamples;
    property DeviceID: TSDL_AudioDeviceID read FDeviceID;  // For SDL_LockAudioDevice
  end;

implementation

var
  GlobalBackendInstance: TSedaiAudioBackend = nil;

procedure SDLAudioCallback(UserData: Pointer; Stream: PUInt8; Len: Integer); cdecl;
begin
  if Assigned(GlobalBackendInstance) then
    GlobalBackendInstance.InternalCallback(Stream, Len)
  else
    FillChar(Stream^, Len, 0);
end;

{ TSedaiAudioBackend }

constructor TSedaiAudioBackend.Create;
begin
  inherited Create;
  FState := bsUninitialized;
  FMode := bmPush;  // Default to push mode (we control timing)
  FDeviceID := 0;
  FSampleRate := 44100;
  FBufferSize := 1024;
  FChannels := 2;
  FCallback := nil;
  FUserData := nil;
  FCallbackCount := 0;
  FQueuedSamples := 0;
  GlobalBackendInstance := Self;
end;

destructor TSedaiAudioBackend.Destroy;
begin
  Shutdown;
  if GlobalBackendInstance = Self then
    GlobalBackendInstance := nil;
  inherited Destroy;
end;

procedure TSedaiAudioBackend.InternalCallback(AStream: Pointer; ALength: Integer);
var
  FrameCount: Integer;
begin
  FrameCount := ALength div (SizeOf(Single) * FChannels);

  if Assigned(FCallback) then
    FCallback(PSingle(AStream), FrameCount, FUserData)
  else
    FillChar(AStream^, ALength, 0);

  Inc(FCallbackCount);
end;

function TSedaiAudioBackend.Initialize: Boolean;
begin
  Result := False;
  if FState <> bsUninitialized then
  begin
    Result := True;
    Exit;
  end;

  if SDL_InitSubSystem(SDL_INIT_AUDIO) < 0 then
  begin
    FState := bsError;
    Exit;
  end;

  FState := bsStopped;
  Result := True;
end;

procedure TSedaiAudioBackend.Shutdown;
begin
  if FState = bsUninitialized then Exit;

  Stop;

  SDL_QuitSubSystem(SDL_INIT_AUDIO);
  FState := bsUninitialized;
end;

function TSedaiAudioBackend.Start: Boolean;
begin
  // Start in current mode
  if FMode = bmPush then
    Result := StartPushMode
  else
    Result := StartCallbackMode;
end;

function TSedaiAudioBackend.StartPushMode: Boolean;
var
  DesiredSpec, ObtainedSpec: TSDL_AudioSpec;
begin
  Result := False;
  FMode := bmPush;

  if FState = bsUninitialized then
    if not Initialize then Exit;

  if FState = bsRunning then
  begin
    Result := True;
    Exit;
  end;

  // Close existing device
  if FDeviceID <> 0 then
  begin
    SDL_CloseAudioDevice(FDeviceID);
    FDeviceID := 0;
  end;

  // Configure audio - NO CALLBACK for push mode
  FillChar(DesiredSpec, SizeOf(DesiredSpec), 0);
  DesiredSpec.freq := FSampleRate;
  DesiredSpec.format := AUDIO_F32SYS;
  DesiredSpec.channels := FChannels;
  DesiredSpec.samples := FBufferSize;
  DesiredSpec.callback := nil;   // NO callback - we use SDL_QueueAudio
  DesiredSpec.userdata := nil;

  // Open device
  FDeviceID := SDL_OpenAudioDevice(nil, 0, @DesiredSpec, @ObtainedSpec, 0);
  if FDeviceID = 0 then
  begin
    FState := bsError;
    Exit;
  end;

  // Update with obtained values
  FSampleRate := ObtainedSpec.freq;
  FBufferSize := ObtainedSpec.samples;
  FChannels := ObtainedSpec.channels;

  // Start playback
  SDL_PauseAudioDevice(FDeviceID, 0);
  FState := bsRunning;
  FQueuedSamples := 0;
  Result := True;
end;

function TSedaiAudioBackend.StartCallbackMode: Boolean;
var
  DesiredSpec, ObtainedSpec: TSDL_AudioSpec;
begin
  Result := False;
  FMode := bmCallback;

  if FState = bsUninitialized then
    if not Initialize then Exit;

  if FState = bsRunning then
  begin
    Result := True;
    Exit;
  end;

  // Close existing device
  if FDeviceID <> 0 then
  begin
    SDL_CloseAudioDevice(FDeviceID);
    FDeviceID := 0;
  end;

  // Configure audio with callback
  FillChar(DesiredSpec, SizeOf(DesiredSpec), 0);
  DesiredSpec.freq := FSampleRate;
  DesiredSpec.format := AUDIO_F32SYS;
  DesiredSpec.channels := FChannels;
  DesiredSpec.samples := FBufferSize;
  DesiredSpec.callback := @SDLAudioCallback;
  DesiredSpec.userdata := nil;

  // Open device
  FDeviceID := SDL_OpenAudioDevice(nil, 0, @DesiredSpec, @ObtainedSpec, 0);
  if FDeviceID = 0 then
  begin
    FState := bsError;
    Exit;
  end;

  // Update with obtained values
  FSampleRate := ObtainedSpec.freq;
  FBufferSize := ObtainedSpec.samples;
  FChannels := ObtainedSpec.channels;

  // Start playback
  SDL_PauseAudioDevice(FDeviceID, 0);
  FState := bsRunning;
  Result := True;
end;

procedure TSedaiAudioBackend.Stop;
begin
  if FDeviceID <> 0 then
  begin
    SDL_PauseAudioDevice(FDeviceID, 1);
    SDL_CloseAudioDevice(FDeviceID);
    FDeviceID := 0;
  end;
  FState := bsStopped;
end;

procedure TSedaiAudioBackend.Pause;
begin
  if (FState = bsRunning) and (FDeviceID <> 0) then
  begin
    SDL_PauseAudioDevice(FDeviceID, 1);
    FState := bsPaused;
  end;
end;

procedure TSedaiAudioBackend.Resume;
begin
  if (FState = bsPaused) and (FDeviceID <> 0) then
  begin
    SDL_PauseAudioDevice(FDeviceID, 0);
    FState := bsRunning;
  end;
end;

procedure TSedaiAudioBackend.SetCallback(ACallback: TSedaiAudioCallback; AUserData: Pointer);
begin
  FCallback := ACallback;
  FUserData := AUserData;
end;

procedure TSedaiAudioBackend.SetSampleRate(AValue: Cardinal);
begin
  if FState <> bsRunning then
    FSampleRate := AValue;
end;

procedure TSedaiAudioBackend.SetDesiredBufferSize(AValue: Integer);
begin
  if FState <> bsRunning then
    FBufferSize := AValue;
end;

procedure TSedaiAudioBackend.SetChannels(AValue: Integer);
begin
  if FState <> bsRunning then
    FChannels := AValue;
end;

procedure TSedaiAudioBackend.SetMode(AValue: TBackendMode);
begin
  if FState <> bsRunning then
    FMode := AValue;
end;

function TSedaiAudioBackend.QueueSamples(AData: PSingle; ASampleCount: Integer): Boolean;
var
  ByteCount: Cardinal;
begin
  Result := False;

  if (FState <> bsRunning) or (FMode <> bmPush) then Exit;
  if (AData = nil) or (ASampleCount <= 0) then Exit;

  // Calculate byte count (samples * channels * sizeof(float))
  ByteCount := Cardinal(ASampleCount) * Cardinal(FChannels) * SizeOf(Single);

  // Queue audio data
  if SDL_QueueAudio(FDeviceID, AData, ByteCount) = 0 then
  begin
    Inc(FQueuedSamples, ASampleCount);
    Result := True;
  end;
end;

function TSedaiAudioBackend.GetQueuedAudioSize: Cardinal;
begin
  if (FState = bsRunning) and (FDeviceID <> 0) then
    Result := SDL_GetQueuedAudioSize(FDeviceID)
  else
    Result := 0;
end;

procedure TSedaiAudioBackend.ClearQueue;
begin
  if (FState = bsRunning) and (FDeviceID <> 0) then
  begin
    SDL_ClearQueuedAudio(FDeviceID);
    FQueuedSamples := 0;
  end;
end;

procedure TSedaiAudioBackend.Lock;
begin
  if FDeviceID <> 0 then
    SDL_LockAudioDevice(FDeviceID);
end;

procedure TSedaiAudioBackend.Unlock;
begin
  if FDeviceID <> 0 then
    SDL_UnlockAudioDevice(FDeviceID);
end;

end.
