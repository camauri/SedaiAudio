{*
 * Sedai Audio Foundation - GoatTracker .sng Player
 *
 * DUAL-MODE AUDIO ENGINE:
 * - SAF Mode (default): Uses Sedai Audio Foundation for audio output
 * - SDL2 Mode (--sdl2): Uses direct SDL2 for compatibility/debugging
 *
 * This player follows the EXACT architecture of the original GoatTracker:
 * - Audio callback requests samples
 * - FillBuffer generates samples, calling PlayRoutine at correct intervals
 * - Register writes use SIDWRITEDELAY timing (14 cycles between each)
 *
 * The key insight from bme_snd.c is that player timing is integrated
 * INTO the audio generation via sample counting, not a separate timer.
 *}

program sng_player;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes, SDL2, SedaiSIDEvo, SedaiGoatTracker,
  SedaiAudioFoundation, SedaiAudioBackend;

const
  SAMPLE_RATE = 44100;
  BUFFER_SIZE = 2048;

type
  TAudioMode = (amSAF, amSDL2);

var
  GQuitRequested: Boolean = False;
  GPlayer: TSedaiGoatTracker;
  GPaused: Boolean = False;
  GAudioDeviceID: TSDL_AudioDeviceID = 0;
  GAudioMode: TAudioMode = amSAF;  // Default: use SAF
  GSAFBackend: TSedaiAudioBackend = nil;

// ============================================================================
// SDL2 AUDIO CALLBACK
// ============================================================================

procedure AudioCallback(AUserData: Pointer; AStream: PUInt8; ALen: Integer); cdecl;
var
  Samples: Integer;
  OutBuffer: PSmallInt;
begin
  Samples := ALen div 2;  // 16-bit samples
  OutBuffer := PSmallInt(AStream);

  if not Assigned(GPlayer) or GPaused or GPlayer.SongFinished then
  begin
    // Silence
    FillChar(AStream^, ALen, 0);
    Exit;
  end;

  // FillBuffer does EVERYTHING correctly:
  // - Calls PlayRoutine at correct intervals (using BPM timing)
  // - Writes SID registers with SIDWRITEDELAY timing
  // - Generates 16-bit audio samples directly
  GPlayer.FillBuffer(OutBuffer, Samples);
end;

// ============================================================================
// SAF AUDIO CALLBACK (Stereo Float Output)
// ============================================================================

var
  GSAFTempBuffer: array[0..BUFFER_SIZE * 2 - 1] of SmallInt;  // Mono 16-bit temp buffer

procedure SAFAudioCallback(AOutput: PSingle; AFrameCount: Integer; AUserData: Pointer);
var
  I: Integer;
  Sample: Single;
begin
  if not Assigned(GPlayer) or GPaused or GPlayer.SongFinished then
  begin
    // Silence (stereo interleaved)
    for I := 0 to AFrameCount * 2 - 1 do
      AOutput[I] := 0.0;
    Exit;
  end;

  // Generate mono 16-bit samples into temp buffer
  GPlayer.FillBuffer(@GSAFTempBuffer[0], AFrameCount);

  // Convert mono 16-bit to stereo float
  for I := 0 to AFrameCount - 1 do
  begin
    // Convert 16-bit signed to float (-1.0 to 1.0)
    Sample := GSAFTempBuffer[I] / 32768.0;
    // Output stereo (same sample to both channels)
    AOutput[I * 2] := Sample;      // Left
    AOutput[I * 2 + 1] := Sample;  // Right
  end;
end;

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
// MAIN PROGRAM
// ============================================================================

procedure ShowUsage;
begin
  WriteLn;
  WriteLn('GoatTracker SNG Player - Sedai Audio Foundation');
  WriteLn('------------------------------------------------');
  WriteLn;
  WriteLn('Usage: sng_player [options] <filename.sng> [subtune]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  --sdl2   Use direct SDL2 audio (bypass SAF)');
  WriteLn;
  WriteLn('Plays music from GoatTracker v2 song files (.sng format).');
  WriteLn('By default uses Sedai Audio Foundation (SAF) for audio output.');
  WriteLn;
  WriteLn('Controls during playback:');
  WriteLn('  SPACE    - Pause/Resume');
  WriteLn('  R        - Restart');
  WriteLn('  I        - Show song info');
  WriteLn('  L        - Toggle looping');
  WriteLn('  V        - Toggle pattern verbose');
  WriteLn('  W        - Toggle wavetable verbose');
  WriteLn('  S        - Show speed table');
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

function InitAudioSAF: Boolean;
begin
  Result := False;

  // Create SAF audio backend
  GSAFBackend := TSedaiAudioBackend.Create;
  GSAFBackend.SetSampleRate(SAMPLE_RATE);
  GSAFBackend.SetDesiredBufferSize(BUFFER_SIZE);
  GSAFBackend.SetChannels(2);  // Stereo output
  GSAFBackend.SetCallback(@SAFAudioCallback, nil);
  GSAFBackend.SetMode(bmCallback);  // IMPORTANT: Use callback mode, not push mode!

  if not GSAFBackend.Initialize then
  begin
    WriteLn('Error: Failed to initialize SAF audio backend');
    FreeAndNil(GSAFBackend);
    Exit;
  end;

  WriteLn('Audio (SAF): ', GSAFBackend.SampleRate, ' Hz, 2 ch (stereo), ',
          GSAFBackend.BufferSize, ' samples');

  Result := True;
end;

function InitAudioSDL2: Boolean;
var
  Wanted, Obtained: TSDL_AudioSpec;
begin
  Result := False;

  FillChar(Wanted, SizeOf(Wanted), 0);
  Wanted.freq := SAMPLE_RATE;
  Wanted.format := AUDIO_S16;
  Wanted.channels := 1;
  Wanted.samples := BUFFER_SIZE;
  Wanted.callback := @AudioCallback;
  Wanted.userdata := nil;

  GAudioDeviceID := SDL_OpenAudioDevice(nil, 0, @Wanted, @Obtained, 0);
  if GAudioDeviceID = 0 then
  begin
    WriteLn('Error: Failed to open audio device: ', SDL_GetError);
    Exit;
  end;

  WriteLn('Audio (SDL2): ', Obtained.freq, ' Hz, ', Obtained.channels, ' ch (mono), ',
          Obtained.samples, ' samples');

  Result := True;
end;

function InitAudio: Boolean;
begin
  if GAudioMode = amSAF then
    Result := InitAudioSAF
  else
    Result := InitAudioSDL2;
end;

procedure CloseAudio;
begin
  if GAudioMode = amSAF then
  begin
    if Assigned(GSAFBackend) then
    begin
      GSAFBackend.Stop;
      GSAFBackend.Shutdown;
      FreeAndNil(GSAFBackend);
    end;
  end
  else
  begin
    if GAudioDeviceID <> 0 then
    begin
      SDL_CloseAudioDevice(GAudioDeviceID);
      GAudioDeviceID := 0;
    end;
  end;
end;

procedure StartAudio;
begin
  if GAudioMode = amSAF then
  begin
    if Assigned(GSAFBackend) then
      GSAFBackend.Start;
  end
  else
  begin
    if GAudioDeviceID <> 0 then
      SDL_PauseAudioDevice(GAudioDeviceID, 0);
  end;
end;

procedure StopAudioPlayback;
begin
  if GAudioMode = amSAF then
  begin
    if Assigned(GSAFBackend) then
      GSAFBackend.Stop;
  end
  else
  begin
    if GAudioDeviceID <> 0 then
      SDL_PauseAudioDevice(GAudioDeviceID, 1);
  end;
end;

procedure LockAudio;
begin
  if GAudioMode = amSDL2 then
  begin
    if GAudioDeviceID <> 0 then
      SDL_LockAudioDevice(GAudioDeviceID);
  end;
  // SAF doesn't need explicit locking for our use case
end;

procedure UnlockAudio;
begin
  if GAudioMode = amSDL2 then
  begin
    if GAudioDeviceID <> 0 then
      SDL_UnlockAudioDevice(GAudioDeviceID);
  end;
end;

procedure ParseCommandLine(out AFilename: string; out ASubtune: Integer);
var
  I: Integer;
  Param: string;
begin
  AFilename := '';
  ASubtune := 0;
  GAudioMode := amSAF;  // Default: use SAF

  for I := 1 to ParamCount do
  begin
    Param := ParamStr(I);

    if (Param = '--sdl2') or (Param = '-sdl2') then
      GAudioMode := amSDL2
    else if (Param[1] <> '-') and (AFilename = '') then
      AFilename := Param
    else if (Param[1] <> '-') and (AFilename <> '') then
      ASubtune := StrToIntDef(Param, 0);
  end;
end;

var
  AFilename: string;
  ASubtune: Integer;
  AKey: Char;
begin
  WriteLn;
  WriteLn('GoatTracker SNG Player - Sedai Audio Foundation');
  WriteLn('------------------------------------------------');

  // Parse command line
  ParseCommandLine(AFilename, ASubtune);

  // Check if we have a filename
  if AFilename = '' then
  begin
    ShowUsage;
    Halt(1);
  end;

  if not FileExists(AFilename) then
  begin
    WriteLn('Error: File not found: ', AFilename);
    Halt(1);
  end;

  // Show audio mode
  if GAudioMode = amSAF then
    WriteLn('Audio Mode: SAF (Sedai Audio Foundation)')
  else
    WriteLn('Audio Mode: Direct SDL2');
  WriteLn;

  // Initialize console input
  {$IFDEF WINDOWS}
  InitConsoleInput;
  {$ENDIF}

  // Initialize SDL (needed for both modes - SAF uses SDL internally)
  if SDL_Init(SDL_INIT_AUDIO) < 0 then
  begin
    WriteLn('Error: SDL initialization failed: ', SDL_GetError);
    Halt(1);
  end;

  try
    // Create player (it will create its own SIDEvo instance)
    GPlayer := TSedaiGoatTracker.Create;
    GPlayer.SetSampleRate(SAMPLE_RATE);

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
    WriteLn('Title:    ', GPlayer.SongName);
    WriteLn('Author:   ', GPlayer.Author);
    WriteLn('Copyright:', GPlayer.Copyright);
    WriteLn('Subtunes: ', GPlayer.SubtuneCount);
    WriteLn;
    WriteLn('Controls: SPACE=Pause R=Restart L=Loop V=PatVerbose W=WaveVerbose');
    WriteLn('          1/2/3=Mute +/-=Subtune Q=Quit');
    WriteLn('Frame rate: ', GPlayer.FrameRate, ' Hz');
    WriteLn;

    // Initialize audio
    if not InitAudio then
    begin
      GPlayer.Free;
      Halt(1);
    end;

    // Start playback
    GPlayer.Looping := False;
    GPlayer.Play(ASubtune);
    GPaused := False;

    // Start audio
    StartAudio;

    // Main loop - handle UI
    while not GQuitRequested do
    begin
      // Check if song finished
      if GPlayer.SongFinished and not GPlayer.Looping then
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
               GPaused := not GPaused;
               if GPaused then
               begin
                 GPlayer.Pause;
                 WriteLn(#13, 'Paused                                                   ');
               end
               else
               begin
                 GPlayer.Resume;
                 WriteLn(#13, 'Resumed                                                  ');
               end;
             end;
        'R': begin
               LockAudio;
               GPlayer.Stop;
               GPlayer.Play(ASubtune);
               GPaused := False;
               UnlockAudio;
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
               GPlayer.PrintTables;
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
               if ASubtune < GPlayer.SubtuneCount - 1 then
               begin
                 Inc(ASubtune);
                 LockAudio;
                 GPlayer.Stop;
                 GPlayer.Play(ASubtune);
                 GPaused := False;
                 UnlockAudio;
                 WriteLn(#13, 'Subtune: ', ASubtune, '                                       ');
               end;
             end;
        '-', '_': begin
               if ASubtune > 0 then
               begin
                 Dec(ASubtune);
                 LockAudio;
                 GPlayer.Stop;
                 GPlayer.Play(ASubtune);
                 GPaused := False;
                 UnlockAudio;
                 WriteLn(#13, 'Subtune: ', ASubtune, '                                       ');
               end;
             end;
      end;

      // Show progress periodically (every 50 player frames = 1 second at PAL)
      if (GPlayer.FrameCounter mod 50) = 0 then
      begin
        Write(#13, 'Frame: ', GPlayer.FrameCounter:6, '  Time: ', (GPlayer.FrameCounter / GPlayer.FrameRate):6:1, 's');
        if GPaused then
          Write(' [PAUSED]')
        else if GPlayer.Looping then
          Write(' [LOOP]  ')
        else
          Write('         ');
      end;

      // Sleep to avoid busy loop
      SDL_Delay(10);
    end;

    WriteLn;
    WriteLn('Stopping...');

    // Stop audio first
    StopAudioPlayback;
    CloseAudio;

    GPlayer.Stop;

  finally
    if Assigned(GPlayer) then
      GPlayer.Free;
    SDL_Quit;
    {$IFDEF WINDOWS}
    RestoreConsoleInput;
    {$ENDIF}
  end;

  WriteLn('Done.');
end.
