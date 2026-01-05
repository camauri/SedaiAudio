{*
 * Sedai Audio Foundation - Cross-Platform Thread Abstraction
 *
 * TSedaiThread provides thread management with priority control
 * using platform-native APIs for real-time audio applications.
 *
 * Platform implementations:
 * - Windows: CreateThread + SetThreadPriority
 * - Linux: pthread_create + pthread_setschedparam (SCHED_FIFO)
 * - macOS: pthread_create + pthread_set_qos_class_self_np
 * - Fallback: FPC BeginThread
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiThread;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix, Unix, PThreads,
  {$ENDIF}
  Classes, SysUtils;

type
  // Thread priority levels
  TSedaiThreadPriority = (
    stpIdle,          // Lowest priority (background tasks)
    stpLow,           // Below normal
    stpNormal,        // Default priority
    stpHigh,          // Above normal (audio processing)
    stpRealtime       // Time-critical (requires privileges on some systems)
  );

  // Thread state
  TSedaiThreadState = (
    stsIdle,          // Not started
    stsRunning,       // Currently executing
    stsStopping,      // Stop requested
    stsStopped        // Terminated
  );

  // Forward declaration
  TSedaiThread = class;

  // Thread procedure type (for functional approach)
  TSedaiThreadProc = procedure(AThread: TSedaiThread; AUserData: Pointer);

  { TSedaiThread }
  // Cross-platform thread with priority control
  TSedaiThread = class
  private
    FState: TSedaiThreadState;
    FPriority: TSedaiThreadPriority;
    FTerminated: Boolean;
    FUserData: Pointer;
    FThreadProc: TSedaiThreadProc;

    {$IFDEF WINDOWS}
    FHandle: THandle;
    FThreadID: DWORD;
    {$ELSE}
    FHandle: TThreadID;
    FPThread: pthread_t;
    {$ENDIF}

    procedure SetPriority(AValue: TSedaiThreadPriority);
    function GetTerminated: Boolean;

  protected
    // Override this method in descendants for OOP approach
    // Or set ThreadProc property for functional approach
    procedure Execute; virtual;

    // Apply priority to the current thread (call from within Execute)
    procedure ApplyPriority;

  public
    constructor Create;
    destructor Destroy; override;

    // Start the thread
    procedure Start;

    // Request thread termination (sets FTerminated flag)
    // The thread should check Terminated property and exit gracefully
    procedure Stop;

    // Wait for thread to finish
    // Returns True if thread finished within timeout
    // ATimeoutMs = 0 means wait indefinitely
    function WaitFor(ATimeoutMs: Cardinal = 0): Boolean;

    // Forcefully terminate thread (dangerous, avoid if possible)
    procedure Kill;

    // Check if thread should terminate (call from Execute)
    property Terminated: Boolean read GetTerminated;

    // Thread state
    property State: TSedaiThreadState read FState;

    // Thread priority (can be changed while running)
    property Priority: TSedaiThreadPriority read FPriority write SetPriority;

    // User data pointer (passed to ThreadProc)
    property UserData: Pointer read FUserData write FUserData;

    // Thread procedure (alternative to overriding Execute)
    property ThreadProc: TSedaiThreadProc read FThreadProc write FThreadProc;

    // Platform-specific handle (for advanced use)
    {$IFDEF WINDOWS}
    property Handle: THandle read FHandle;
    {$ELSE}
    property Handle: TThreadID read FHandle;
    {$ENDIF}
  end;

  { TSedaiFrameTimer }
  // Specialized thread for frame-accurate timing (e.g., 50Hz/60Hz playback)
  // Calls a callback at precise intervals
  TSedaiFrameTimer = class(TSedaiThread)
  private
    FFrameRate: Integer;           // Frames per second (e.g., 50 for PAL)
    FFrameCallback: TSedaiThreadProc;
    FFrameCount: QWord;            // Total frames processed

  protected
    procedure Execute; override;

  public
    constructor Create(AFrameRate: Integer);

    // Frame callback (called at each frame interval)
    property FrameCallback: TSedaiThreadProc read FFrameCallback write FFrameCallback;

    // Target frame rate
    property FrameRate: Integer read FFrameRate write FFrameRate;

    // Total frames processed since start
    property FrameCount: QWord read FFrameCount;
  end;

implementation

uses
  SedaiTiming;

// ============================================================================
// PLATFORM-SPECIFIC THREAD ENTRY POINTS
// ============================================================================

{$IFDEF WINDOWS}
function WinThreadFunc(AParam: Pointer): DWORD; stdcall;
var
  LThread: TSedaiThread;
begin
  LThread := TSedaiThread(AParam);
  LThread.FState := stsRunning;
  LThread.ApplyPriority;

  try
    LThread.Execute;
  except
    // Swallow exceptions in thread
  end;

  LThread.FState := stsStopped;
  Result := 0;
end;
{$ENDIF}

{$IFDEF UNIX}
function UnixThreadFunc(AParam: Pointer): Pointer; cdecl;
var
  LThread: TSedaiThread;
begin
  LThread := TSedaiThread(AParam);
  LThread.FState := stsRunning;
  LThread.ApplyPriority;

  try
    LThread.Execute;
  except
    // Swallow exceptions in thread
  end;

  LThread.FState := stsStopped;
  Result := nil;
end;
{$ENDIF}

{ TSedaiThread }

constructor TSedaiThread.Create;
begin
  inherited Create;
  FState := stsIdle;
  FPriority := stpNormal;
  FTerminated := False;
  FUserData := nil;
  FThreadProc := nil;

  {$IFDEF WINDOWS}
  FHandle := 0;
  FThreadID := 0;
  {$ELSE}
  FHandle := 0;
  FillChar(FPThread, SizeOf(FPThread), 0);
  {$ENDIF}
end;

destructor TSedaiThread.Destroy;
begin
  if FState = stsRunning then
  begin
    Stop;
    WaitFor(1000);  // Wait up to 1 second
  end;

  {$IFDEF WINDOWS}
  if FHandle <> 0 then
    CloseHandle(FHandle);
  {$ENDIF}

  inherited Destroy;
end;

procedure TSedaiThread.Execute;
begin
  // Default implementation: call ThreadProc if assigned
  if Assigned(FThreadProc) then
    FThreadProc(Self, FUserData);
end;

procedure TSedaiThread.ApplyPriority;
{$IFDEF WINDOWS}
var
  LWinPriority: Integer;
{$ENDIF}
{$IFDEF UNIX}
var
  LSchedParam: sched_param;
  LPolicy: Integer;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  case FPriority of
    stpIdle:     LWinPriority := THREAD_PRIORITY_IDLE;
    stpLow:      LWinPriority := THREAD_PRIORITY_BELOW_NORMAL;
    stpNormal:   LWinPriority := THREAD_PRIORITY_NORMAL;
    stpHigh:     LWinPriority := THREAD_PRIORITY_ABOVE_NORMAL;
    stpRealtime: LWinPriority := THREAD_PRIORITY_TIME_CRITICAL;
  else
    LWinPriority := THREAD_PRIORITY_NORMAL;
  end;
  SetThreadPriority(GetCurrentThread, LWinPriority);
  {$ENDIF}

  {$IFDEF UNIX}
  // Linux/macOS: Use SCHED_FIFO for realtime, SCHED_OTHER for normal
  FillChar(LSchedParam, SizeOf(LSchedParam), 0);

  case FPriority of
    stpIdle, stpLow, stpNormal:
      begin
        LPolicy := SCHED_OTHER;
        LSchedParam.sched_priority := 0;
      end;
    stpHigh:
      begin
        // Try SCHED_FIFO with medium priority
        LPolicy := SCHED_FIFO;
        LSchedParam.sched_priority := 50;
      end;
    stpRealtime:
      begin
        // SCHED_FIFO with high priority (requires CAP_SYS_NICE or root)
        LPolicy := SCHED_FIFO;
        LSchedParam.sched_priority := 80;
      end;
  else
    LPolicy := SCHED_OTHER;
    LSchedParam.sched_priority := 0;
  end;

  // Try to set priority (may fail without privileges)
  pthread_setschedparam(pthread_self, LPolicy, @LSchedParam);
  {$ENDIF}
end;

procedure TSedaiThread.SetPriority(AValue: TSedaiThreadPriority);
begin
  FPriority := AValue;

  // If thread is running, apply priority immediately
  if FState = stsRunning then
  begin
    {$IFDEF WINDOWS}
    if FHandle <> 0 then
    begin
      case FPriority of
        stpIdle:     SetThreadPriority(FHandle, THREAD_PRIORITY_IDLE);
        stpLow:      SetThreadPriority(FHandle, THREAD_PRIORITY_BELOW_NORMAL);
        stpNormal:   SetThreadPriority(FHandle, THREAD_PRIORITY_NORMAL);
        stpHigh:     SetThreadPriority(FHandle, THREAD_PRIORITY_ABOVE_NORMAL);
        stpRealtime: SetThreadPriority(FHandle, THREAD_PRIORITY_TIME_CRITICAL);
      end;
    end;
    {$ENDIF}
    // For Unix, priority change while running requires the thread itself to call ApplyPriority
  end;
end;

function TSedaiThread.GetTerminated: Boolean;
begin
  Result := FTerminated;
end;

procedure TSedaiThread.Start;
{$IFDEF UNIX}
var
  LAttr: pthread_attr_t;
{$ENDIF}
begin
  if FState <> stsIdle then Exit;

  FTerminated := False;
  FState := stsRunning;

  {$IFDEF WINDOWS}
  FHandle := CreateThread(
    nil,                    // Security attributes
    0,                      // Stack size (default)
    @WinThreadFunc,         // Thread function
    Self,                   // Parameter
    0,                      // Creation flags
    FThreadID               // Thread ID
  );
  if FHandle = 0 then
    FState := stsStopped;
  {$ENDIF}

  {$IFDEF UNIX}
  pthread_attr_init(@LAttr);
  pthread_attr_setdetachstate(@LAttr, PTHREAD_CREATE_JOINABLE);

  if pthread_create(@FPThread, @LAttr, @UnixThreadFunc, Self) <> 0 then
    FState := stsStopped
  else
    FHandle := FPThread;

  pthread_attr_destroy(@LAttr);
  {$ENDIF}
end;

procedure TSedaiThread.Stop;
begin
  if FState = stsRunning then
  begin
    FState := stsStopping;
    FTerminated := True;
  end;
end;

function TSedaiThread.WaitFor(ATimeoutMs: Cardinal): Boolean;
{$IFDEF WINDOWS}
var
  LWaitResult: DWORD;
{$ENDIF}
begin
  Result := False;

  if FState = stsIdle then
  begin
    Result := True;
    Exit;
  end;

  {$IFDEF WINDOWS}
  if FHandle <> 0 then
  begin
    if ATimeoutMs = 0 then
      LWaitResult := WaitForSingleObject(FHandle, INFINITE)
    else
      LWaitResult := WaitForSingleObject(FHandle, ATimeoutMs);
    Result := (LWaitResult = WAIT_OBJECT_0);
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  if FPThread <> 0 then
  begin
    // pthread_join doesn't have timeout, so we poll with sleep
    if ATimeoutMs = 0 then
    begin
      pthread_join(FPThread, nil);
      Result := True;
    end
    else
    begin
      // Poll with timeout
      while (ATimeoutMs > 0) and (FState = stsRunning) do
      begin
        Sleep(10);
        if ATimeoutMs > 10 then
          Dec(ATimeoutMs, 10)
        else
          ATimeoutMs := 0;
      end;
      Result := (FState <> stsRunning);
      if Result then
        pthread_join(FPThread, nil);
    end;
  end;
  {$ENDIF}
end;

procedure TSedaiThread.Kill;
begin
  {$IFDEF WINDOWS}
  if FHandle <> 0 then
  begin
    TerminateThread(FHandle, 1);
    CloseHandle(FHandle);
    FHandle := 0;
  end;
  {$ENDIF}

  {$IFDEF UNIX}
  if FPThread <> 0 then
  begin
    pthread_cancel(FPThread);
    FPThread := 0;
  end;
  {$ENDIF}

  FState := stsStopped;
  FTerminated := True;
end;

{ TSedaiFrameTimer }

constructor TSedaiFrameTimer.Create(AFrameRate: Integer);
begin
  inherited Create;
  FFrameRate := AFrameRate;
  FFrameCallback := nil;
  FFrameCount := 0;
  Priority := stpRealtime;  // Default to realtime for audio
end;

procedure TSedaiFrameTimer.Execute;
var
  LFrameTimeMicros: Int64;
  LNextFrameTicks: Int64;
  LCurrentTicks: Int64;
  LSleepMicros: Int64;
begin
  if FFrameRate <= 0 then
    FFrameRate := 50;  // Default to PAL

  // Calculate frame time in microseconds
  LFrameTimeMicros := 1000000 div FFrameRate;

  // Initialize timing
  LNextFrameTicks := TSedaiTiming.GetTicks +
                     TSedaiTiming.MicrosecondsToTicks(LFrameTimeMicros);

  while not Terminated do
  begin
    LCurrentTicks := TSedaiTiming.GetTicks;

    // Process all pending frames (catch up if behind)
    while (LCurrentTicks >= LNextFrameTicks) and not Terminated do
    begin
      // Call frame callback
      if Assigned(FFrameCallback) then
        FFrameCallback(Self, UserData);

      Inc(FFrameCount);

      // Schedule next frame
      Inc(LNextFrameTicks, TSedaiTiming.MicrosecondsToTicks(LFrameTimeMicros));

      // Safety: if more than 5 frames behind, reset timing
      if LCurrentTicks > LNextFrameTicks +
         TSedaiTiming.MicrosecondsToTicks(LFrameTimeMicros * 5) then
      begin
        LNextFrameTicks := LCurrentTicks +
                          TSedaiTiming.MicrosecondsToTicks(LFrameTimeMicros);
        Break;
      end;

      LCurrentTicks := TSedaiTiming.GetTicks;
    end;

    // Calculate sleep time
    LSleepMicros := TSedaiTiming.TicksToMicroseconds(
                      LNextFrameTicks - TSedaiTiming.GetTicks);

    // Precise sleep
    if LSleepMicros > 0 then
      TSedaiTiming.PreciseSleep(LSleepMicros);
  end;
end;

end.
