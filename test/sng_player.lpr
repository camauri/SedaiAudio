{*
 * Sedai Audio Foundation - GoatTracker .sng Player
 * Plays music from GoatTracker v2 song files
 *}

program sng_player;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF WINDOWS}
  Windows, MMSystem,
  {$ENDIF}
  SysUtils, Classes, SDL2, SedaiSIDEvo, SedaiGoatTracker;

var
  GQuitRequested: Boolean = False;
  GPlayer: TSedaiGoatTracker;
  GPlayerThread: TThreadID;
  GPlayerThreadRunning: Boolean = False;
  GPaused: Boolean = False;
  GFrameCount: Integer = 0;

// ============================================================================
// WINDOWS CONSOLE KEYBOARD INPUT
// ============================================================================

{$IFDEF WINDOWS}
var
  GConsoleHandle: THandle;
  GOriginalConsoleMode: DWORD;

procedure InitConsoleInput;
begin
  GConsoleHandle := GetStdHandle(STD_INPUT_HANDLE);
  if GConsoleHandle <> INVALID_HANDLE_VALUE then
    GetConsoleMode(GConsoleHandle, @GOriginalConsoleMode);
end;

procedure RestoreConsoleInput;
begin
  if GConsoleHandle <> INVALID_HANDLE_VALUE then
    SetConsoleMode(GConsoleHandle, GOriginalConsoleMode);
end;

function ConsoleKeyPressed: Boolean;
var
  ANumEvents: DWORD;
  AInputRec: INPUT_RECORD;
  AEventsRead: DWORD;
begin
  Result := False;
  if GConsoleHandle = INVALID_HANDLE_VALUE then Exit;

  if GetNumberOfConsoleInputEvents(GConsoleHandle, @ANumEvents) then
  begin
    while ANumEvents > 0 do
    begin
      if PeekConsoleInput(GConsoleHandle, @AInputRec, 1, @AEventsRead) then
      begin
        if (AEventsRead > 0) and (AInputRec.EventType = KEY_EVENT) and
           (AInputRec.Event.KeyEvent.bKeyDown) then
        begin
          Result := True;
          Exit;
        end
        else
          ReadConsoleInput(GConsoleHandle, @AInputRec, 1, @AEventsRead);
      end;
      Dec(ANumEvents);
    end;
  end;
end;

function ConsoleReadKey: Char;
var
  AInputRec: INPUT_RECORD;
  AEventsRead: DWORD;
  AVk: Word;
  ACtrl: Boolean;
begin
  Result := #0;
  if GConsoleHandle = INVALID_HANDLE_VALUE then Exit;

  if ConsoleKeyPressed then
  begin
    if ReadConsoleInput(GConsoleHandle, @AInputRec, 1, @AEventsRead) then
    begin
      if (AEventsRead > 0) and (AInputRec.EventType = KEY_EVENT) and
         (AInputRec.Event.KeyEvent.bKeyDown) then
      begin
        AVk := AInputRec.Event.KeyEvent.wVirtualKeyCode;
        ACtrl := (AInputRec.Event.KeyEvent.dwControlKeyState and
                  (LEFT_CTRL_PRESSED or RIGHT_CTRL_PRESSED)) <> 0;

        if ACtrl and (AVk = Ord('C')) then
        begin
          GQuitRequested := True;
          Result := #27;
          Exit;
        end;

        if AVk = VK_ESCAPE then
          Result := #27
        else if AVk = VK_RETURN then
          Result := #13
        else if AVk = VK_SPACE then
          Result := ' '
        else if (AVk >= Ord('A')) and (AVk <= Ord('Z')) then
          Result := Chr(AVk)
        else if (AVk >= Ord('0')) and (AVk <= Ord('9')) then
          Result := Chr(AVk)
        else
          Result := AInputRec.Event.KeyEvent.AsciiChar;
      end;
    end;
  end;
end;

{$ELSE}
// Linux/Mac placeholder
procedure InitConsoleInput;
begin
end;

procedure RestoreConsoleInput;
begin
end;

function ConsoleKeyPressed: Boolean;
begin
  Result := False;
end;

function ConsoleReadKey: Char;
begin
  Result := #0;
end;
{$ENDIF}

// ============================================================================
// HIGH PRECISION TIMER (Windows)
// ============================================================================

{$IFDEF WINDOWS}
var
  GTimerFrequency: Int64;
  GTimerInitialized: Boolean = False;

procedure InitHighPrecisionTimer;
begin
  if not GTimerInitialized then
  begin
    QueryPerformanceFrequency(@GTimerFrequency);
    GTimerInitialized := True;
  end;
end;

function GetHighPrecisionTicks: Int64;
var
  ATicks: Int64;
begin
  QueryPerformanceCounter(@ATicks);
  Result := ATicks;
end;

function TicksToMicroseconds(ATicks: Int64): Int64;
begin
  Result := (ATicks * 1000000) div GTimerFrequency;
end;

function MicrosecondsToTicks(AMicroseconds: Int64): Int64;
begin
  Result := (AMicroseconds * GTimerFrequency) div 1000000;
end;

// Busy-wait for precise timing (uses less than 1ms accuracy)
procedure PreciseSleep(AMicroseconds: Int64);
var
  AStart, AEnd, ATarget: Int64;
begin
  if AMicroseconds <= 0 then Exit;

  AStart := GetHighPrecisionTicks;
  ATarget := MicrosecondsToTicks(AMicroseconds);
  AEnd := AStart + ATarget;

  // For longer waits, use Sleep for most of the time
  if AMicroseconds > 2000 then
    Sleep((AMicroseconds - 1000) div 1000);

  // Busy-wait for the final portion for precision
  while GetHighPrecisionTicks < AEnd do
    ; // Spin
end;

{$ELSE}
// Non-Windows fallback
procedure InitHighPrecisionTimer;
begin
end;

function GetHighPrecisionTicks: Int64;
begin
  Result := SDL_GetTicks * 1000; // Convert ms to approximate ticks
end;

function TicksToMicroseconds(ATicks: Int64): Int64;
begin
  Result := ATicks; // Already in microseconds approximation
end;

function MicrosecondsToTicks(AMicroseconds: Int64): Int64;
begin
  Result := AMicroseconds;
end;

procedure PreciseSleep(AMicroseconds: Int64);
begin
  SDL_Delay(AMicroseconds div 1000);
end;
{$ENDIF}

// ============================================================================
// PLAYER THREAD - High priority thread for precise timing
// ============================================================================

{$IFDEF WINDOWS}
function PlayerThreadProc(AParam: Pointer): PtrInt;
var
  AFrameTimeMicros: Int64;
  ANextFrameTime: Int64;
  ACurrentTime: Int64;
  ASleepTime: Int64;
  AThreadHandle: THandle;
begin
  Result := 0;

  // Set thread to high priority for precise timing
  AThreadHandle := GetCurrentThread;
  SetThreadPriority(AThreadHandle, THREAD_PRIORITY_TIME_CRITICAL);

  // Initialize timing
  InitHighPrecisionTimer;
  AFrameTimeMicros := 1000000 div GPlayer.FrameRate;
  ANextFrameTime := GetHighPrecisionTicks + MicrosecondsToTicks(AFrameTimeMicros);

  while GPlayerThreadRunning and not GQuitRequested do
  begin
    ACurrentTime := GetHighPrecisionTicks;

    // Process all pending frames
    while (ACurrentTime >= ANextFrameTime) and GPlayerThreadRunning do
    begin
      if GPlayer.IsPlaying and not GPaused then
      begin
        GPlayer.Update;
        Inc(GFrameCount);
      end;

      // Schedule next frame
      Inc(ANextFrameTime, MicrosecondsToTicks(AFrameTimeMicros));

      // Safety reset if too far behind
      if ACurrentTime > ANextFrameTime + MicrosecondsToTicks(AFrameTimeMicros * 5) then
      begin
        ANextFrameTime := ACurrentTime + MicrosecondsToTicks(AFrameTimeMicros);
        Break;
      end;

      // Re-read current time for next iteration
      ACurrentTime := GetHighPrecisionTicks;
    end;

    // Calculate sleep time
    ASleepTime := TicksToMicroseconds(ANextFrameTime - GetHighPrecisionTicks);

    // Precise sleep
    if ASleepTime > 1500 then
      PreciseSleep(ASleepTime - 500)
    else if ASleepTime > 100 then
      ; // Busy wait
  end;
end;

procedure StartPlayerThread;
begin
  GPlayerThreadRunning := True;
  GFrameCount := 0;
  BeginThread(@PlayerThreadProc, nil, GPlayerThread);
end;

procedure StopPlayerThread;
begin
  GPlayerThreadRunning := False;
  // Wait for thread to finish (with timeout)
  WaitForThreadTerminate(GPlayerThread, 1000);
end;

{$ELSE}
// Non-Windows: no threading, use simple timing
procedure StartPlayerThread;
begin
  GPlayerThreadRunning := True;
  GFrameCount := 0;
end;

procedure StopPlayerThread;
begin
  GPlayerThreadRunning := False;
end;
{$ENDIF}

// ============================================================================
// MAIN PROGRAM
// ============================================================================

procedure ShowUsage;
begin
  WriteLn;
  WriteLn('GoatTracker SNG Player - Sedai Audio Foundation');
  WriteLn('------------------------------------------------');
  WriteLn;
  WriteLn('Usage: sng_player <filename.sng> [subtune]');
  WriteLn;
  WriteLn('Plays music from GoatTracker v2 song files (.sng format).');
  WriteLn;
  WriteLn('Controls during playback:');
  WriteLn('  SPACE    - Pause/Resume');
  WriteLn('  R        - Restart');
  WriteLn('  I        - Show song info');
  WriteLn('  F        - Toggle frame rate (50/60 Hz)');
  WriteLn('  1/2/3    - Mute/Unmute voice 1/2/3');
  WriteLn('  +/-      - Next/Previous subtune');
  WriteLn('  ESC/Q    - Quit');
  WriteLn;
end;

procedure ShowSongInfo;
begin
  if not Assigned(GPlayer) then Exit;

  WriteLn;
  WriteLn('=== Song Info ===');
  GPlayer.PrintSongInfo;
  WriteLn;
end;

var
  AFilename: string;
  ASubtune: Integer;
  AKey: Char;
begin
  WriteLn;
  WriteLn('GoatTracker SNG Player - Sedai Audio Foundation');
  WriteLn('------------------------------------------------');
  WriteLn;

  // Check command line
  if ParamCount < 1 then
  begin
    ShowUsage;
    Halt(1);
  end;

  AFilename := ParamStr(1);
  if not FileExists(AFilename) then
  begin
    WriteLn('Error: File not found: ', AFilename);
    Halt(1);
  end;

  // Optional subtune parameter
  ASubtune := 0;
  if ParamCount >= 2 then
    ASubtune := StrToIntDef(ParamStr(2), 0);

  // Initialize console input
  {$IFDEF WINDOWS}
  InitConsoleInput;
  // Request 1ms timer resolution for precise timing
  timeBeginPeriod(1);
  {$ENDIF}

  // Initialize SDL for audio
  if SDL_Init(SDL_INIT_AUDIO or SDL_INIT_TIMER) < 0 then
  begin
    WriteLn('Error: SDL initialization failed');
    {$IFDEF WINDOWS}
    timeEndPeriod(1);
    {$ENDIF}
    Halt(1);
  end;

  try
    // Create player (it will create its own SIDEvo instance)
    GPlayer := TSedaiGoatTracker.Create;

    WriteLn('Loading: ', AFilename);
    WriteLn;

    // Load the song file
    if not GPlayer.LoadFromFile(AFilename) then
    begin
      WriteLn('Error: Failed to load file');
      GPlayer.Free;
      Halt(1);
    end;

    WriteLn;
    WriteLn('Title:    ', GPlayer.Info.Title);
    WriteLn('Author:   ', GPlayer.Info.Author);
    WriteLn('Copyright:', GPlayer.Info.Copyright);
    WriteLn('Subtunes: ', GPlayer.Info.SubtuneCount);
    WriteLn;
    WriteLn('Controls: SPACE=Pause R=Restart L=Loop V=PatVerbose W=WaveVerbose S=SpeedTbl 1/2/3=Mute Q=Quit');
    WriteLn('Frame rate: ', GPlayer.FrameRate, ' Hz');
    WriteLn;

    // Start playback with thread
    GPlayer.Looping := False;  // Default: play once
    GPlayer.Play(ASubtune);
    GPaused := False;
    {$IFDEF WINDOWS}
    StartPlayerThread;
    {$ENDIF}

    // Main loop - just handle UI, timing is in thread
    while not GQuitRequested do
    begin
      // Check if song finished
      if GPlayer.SongFinished then
      begin
        WriteLn;
        WriteLn('Song finished.');
        Break;
      end;

      // Check for keys
      AKey := UpCase(ConsoleReadKey);
      case AKey of
        #27, 'Q': GQuitRequested := True;
        ' ': begin
               if GPaused then
               begin
                 GPlayer.Resume;
                 GPaused := False;
                 WriteLn(#13, 'Resumed                                                  ');
               end
               else
               begin
                 GPlayer.Pause;
                 GPaused := True;
                 WriteLn(#13, 'Paused                                                   ');
               end;
             end;
        'R': begin
               GPlayer.Stop;
               GPlayer.Play(ASubtune);
               GPaused := False;
               GFrameCount := 0;
               WriteLn(#13, 'Restarted                                                ');
             end;
        'I': ShowSongInfo;
        'W': begin
               GPlayer.WaveVerbose := not GPlayer.WaveVerbose;
               if GPlayer.WaveVerbose then
                 WriteLn(#13, 'WaveTable verbose: ON                                    ')
               else
                 WriteLn(#13, 'WaveTable verbose: OFF                                   ');
             end;
        'V': begin
               GPlayer.Verbose := not GPlayer.Verbose;
               if GPlayer.Verbose then
                 WriteLn(#13, 'Pattern verbose: ON                                      ')
               else
                 WriteLn(#13, 'Pattern verbose: OFF                                     ');
             end;
        'S': begin
               WriteLn;
               GPlayer.PrintSpeedTable;
               WriteLn;
             end;
        'L': begin
               GPlayer.Looping := not GPlayer.Looping;
               if GPlayer.Looping then
                 WriteLn(#13, 'Looping: ON                                              ')
               else
                 WriteLn(#13, 'Looping: OFF                                             ');
             end;
        '1': begin
               GPlayer.SetVoiceMute(0, not GPlayer.IsVoiceMuted(0));
               if GPlayer.IsVoiceMuted(0) then
                 WriteLn(#13, 'Voice 1: MUTED                                           ')
               else
                 WriteLn(#13, 'Voice 1: ON                                              ');
             end;
        '2': begin
               GPlayer.SetVoiceMute(1, not GPlayer.IsVoiceMuted(1));
               if GPlayer.IsVoiceMuted(1) then
                 WriteLn(#13, 'Voice 2: MUTED                                           ')
               else
                 WriteLn(#13, 'Voice 2: ON                                              ');
             end;
        '3': begin
               GPlayer.SetVoiceMute(2, not GPlayer.IsVoiceMuted(2));
               if GPlayer.IsVoiceMuted(2) then
                 WriteLn(#13, 'Voice 3: MUTED                                           ')
               else
                 WriteLn(#13, 'Voice 3: ON                                              ');
             end;
        '+', '=': begin
               if ASubtune < GPlayer.Info.SubtuneCount - 1 then
               begin
                 Inc(ASubtune);
                 GPlayer.Stop;
                 GPlayer.Play(ASubtune);
                 GPaused := False;
                 GFrameCount := 0;
                 WriteLn(#13, 'Subtune: ', ASubtune, '                                       ');
               end;
             end;
        '-', '_': begin
               if ASubtune > 0 then
               begin
                 Dec(ASubtune);
                 GPlayer.Stop;
                 GPlayer.Play(ASubtune);
                 GPaused := False;
                 GFrameCount := 0;
                 WriteLn(#13, 'Subtune: ', ASubtune, '                                       ');
               end;
             end;
      end;

      // Show progress periodically
      if (GFrameCount mod 50) = 0 then
      begin
        Write(#13, 'Frame: ', GFrameCount:6, '  Time: ', (GFrameCount / GPlayer.FrameRate):6:1, 's');
        if GPaused then
          Write(' [PAUSED]')
        else if GPlayer.Looping then
          Write(' [LOOP]  ')
        else
          Write('         ');
      end;

      // Sleep a bit to avoid busy loop (UI doesn't need high precision)
      SDL_Delay(10);
    end;

    WriteLn;
    WriteLn('Stopping...');
    {$IFDEF WINDOWS}
    StopPlayerThread;
    {$ENDIF}
    GPlayer.Stop;

  finally
    if Assigned(GPlayer) then
      GPlayer.Free;
    SDL_Quit;
    {$IFDEF WINDOWS}
    RestoreConsoleInput;
    timeEndPeriod(1);  // Restore timer resolution
    {$ENDIF}
  end;

  WriteLn('Done.');
end.
