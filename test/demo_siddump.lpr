{*
 * Sedai Audio Foundation - SIDDump Player Demo
 * Plays SID music from SIDDump text files
 *}

program demo_siddump;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, SDL2, SedaiSIDEvo, SedaiSIDDumpPlayer;

var
  GQuitRequested: Boolean = False;
  GPlayer: TSedaiSIDDumpPlayer;

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
  WriteLn('SIDDump Player - Sedai Audio Foundation');
  WriteLn('-----------------------------------------');
  WriteLn;
  WriteLn('Usage: demo_siddump <filename.txt>');
  WriteLn('       demo_siddump -t              (run glissando test only)');
  WriteLn;
  WriteLn('Plays SID music from SIDDump text format files.');
  WriteLn;
  WriteLn('To create a SIDDump file from a .sid file:');
  WriteLn('  siddump -t <file.sid> > output.txt');
  WriteLn;
  WriteLn('Controls during playback:');
  WriteLn('  SPACE    - Pause/Resume');
  WriteLn('  R        - Restart');
  WriteLn('  L        - Toggle looping');
  WriteLn('  1/2/3    - Mute/Unmute voice 1/2/3');
  WriteLn('  ESC/Q    - Quit');
  WriteLn;
end;

// Test glissando ascendente come nel dump (frames 88-127)
// Voice 3: Pulse ($41), ADSR $14C8, PW $800, Freq da $0191 a $05D5
procedure TestGlissando;
const
  SID_CLOCK = 985248;
  // Frequenze dal dump: $0191, $01AD, $01C9, $01E5, $0201, $021D, $0239...
  FreqRegs: array[0..39] of Word = (
    $0191, $01AD, $01C9, $01E5, $0201, $021D, $0239, $0255, $0271, $028D,
    $02A9, $02C5, $02E1, $02FD, $0319, $0335, $0351, $036D, $0389, $03A5,
    $03C1, $03DD, $03F9, $0415, $0431, $044D, $0469, $0485, $04A1, $04BD,
    $04D9, $04F5, $0511, $052D, $0549, $0565, $0581, $059D, $05B9, $05D5
  );
var
  i: Integer;
  AFreqHz: Single;
begin
  WriteLn;
  WriteLn('=== TEST GLISSANDO (Voice 3, Pulse, sweep F#0 -> F-2) ===');
  WriteLn;

  // Setup Voice 2 (index 2) come nel dump
  SIDEvo.SetWaveform(2, SIDEVO_WAVE_PULSE);  // $41 = Pulse + Gate
  SIDEvo.SetPulseWidth(2, 0.5);              // $800 / $1000 = 0.5
  SIDEvo.SetAttack(2, 1/15);                 // $1 / 15
  SIDEvo.SetDecay(2, 4/15);                  // $4 / 15
  SIDEvo.SetSustain(2, 12/15);               // $C / 15
  SIDEvo.SetRelease(2, 8/15);                // $8 / 15

  // Prima frequenza e GateOn
  AFreqHz := (FreqRegs[0] * SID_CLOCK) / 16777216.0;
  WriteLn('Frame 0: Freq=$', IntToHex(FreqRegs[0], 4), ' = ', AFreqHz:0:1, ' Hz - GateOn');
  SIDEvo.SetFrequencyHz(2, AFreqHz);
  SIDEvo.GateOn(2);

  SDL_Delay(20);  // 1 frame = 20ms

  // Glissando: aggiorna frequenza ogni frame
  for i := 1 to High(FreqRegs) do
  begin
    AFreqHz := (FreqRegs[i] * SID_CLOCK) / 16777216.0;
    Write('Frame ', i:2, ': Freq=$', IntToHex(FreqRegs[i], 4), ' = ', AFreqHz:0:1, ' Hz');

    SIDEvo.SetFrequencyHz(2, AFreqHz);
    SIDEvo.UpdateActiveVoice(2);  // Aggiorna frequenza in tempo reale

    WriteLn(' [updated]');
    SDL_Delay(20);  // 50 Hz = 20ms per frame
  end;

  // Tieni la nota finale per un po'
  WriteLn('Holding final note...');
  SDL_Delay(500);

  // GateOff
  WriteLn('GateOff');
  SIDEvo.GateOff(2);
  SDL_Delay(500);

  WriteLn('=== TEST COMPLETE ===');
  WriteLn;
end;

procedure ShowProgress;
var
  APos, ADur: Single;
  AProgress: Integer;
  ABar: string;
  i: Integer;
begin
  if not Assigned(GPlayer) then Exit;

  APos := GPlayer.GetPositionSeconds;
  ADur := GPlayer.GetDurationSeconds;
  AProgress := Round(GPlayer.GetProgress * 50);

  ABar := '[';
  for i := 1 to 50 do
  begin
    if i <= AProgress then
      ABar := ABar + '='
    else
      ABar := ABar + ' ';
  end;
  ABar := ABar + ']';

  Write(#13, ABar, ' ', APos:5:1, '/', ADur:5:1, 's  Frame: ',
        GPlayer.CurrentFrame:5, '/', GPlayer.FrameCount:5, '  ');

  if GPlayer.State = spsPaused then
    Write('[PAUSED] ')
  else if GPlayer.Looping then
    Write('[LOOP]   ')
  else
    Write('         ');
end;

var
  AFilename: string;
  AKey: Char;
  ALastUpdate: UInt32;
  AFrameTime: UInt32;
  ATestOnly: Boolean;
begin
  WriteLn;
  WriteLn('SIDDump Player - Sedai Audio Foundation');
  WriteLn('-----------------------------------------');
  WriteLn;

  // Check command line
  if ParamCount < 1 then
  begin
    ShowUsage;
    Halt(1);
  end;

  ATestOnly := (ParamStr(1) = '-t') or (ParamStr(1) = '-T') or (ParamStr(1) = '--test');

  if not ATestOnly then
  begin
    AFilename := ParamStr(1);
    if not FileExists(AFilename) then
    begin
      WriteLn('Error: File not found: ', AFilename);
      Halt(1);
    end;
  end;

  // Initialize console input
  {$IFDEF WINDOWS}
  InitConsoleInput;
  {$ENDIF}

  // Initialize SDL for audio
  if SDL_Init(SDL_INIT_AUDIO or SDL_INIT_TIMER) < 0 then
  begin
    WriteLn('Error: SDL initialization failed');
    Halt(1);
  end;

  // Test-only mode
  if ATestOnly then
  begin
    WriteLn('Running glissando test...');
    WriteLn;
    SIDEvoInit(8);
    SIDEvoVol(0.8);
    TestGlissando;
    SIDEvoShutdown;
    SDL_Quit;
    {$IFDEF WINDOWS}
    RestoreConsoleInput;
    {$ENDIF}
    WriteLn('Done.');
    Halt(0);
  end;

  try
    // Create player (it will create its own SIDEvo instance)
    GPlayer := TSedaiSIDDumpPlayer.Create;
    GPlayer.Looping := False;  // User must enable looping manually with 'L'

    WriteLn('Loading: ', AFilename);

    // Load the dump file
    if not GPlayer.LoadFromFile(AFilename) then
    begin
      WriteLn('Error: Failed to load file');
      GPlayer.Free;
      Halt(1);
    end;

    WriteLn('Title:    ', GPlayer.Info.Title);
    WriteLn('Frames:   ', GPlayer.FrameCount);
    WriteLn('Duration: ', GPlayer.GetDurationSeconds:0:2, ' seconds');
    WriteLn;
    WriteLn('Controls: SPACE=Pause  R=Restart  L=Loop  1/2/3=Mute  ESC/Q=Quit');
    WriteLn;

    // Start playback
    GPlayer.Play;

    // Frame timing: 50 Hz for PAL
    AFrameTime := 1000 div GPlayer.FrameRate;
    ALastUpdate := SDL_GetTicks;

    // Main loop
    while not GQuitRequested do
    begin
      // Check for keys
      AKey := UpCase(ConsoleReadKey);
      case AKey of
        #27, 'Q': GQuitRequested := True;
        ' ': GPlayer.Pause;
        'R': begin
               GPlayer.Rewind;
               GPlayer.Play;
             end;
        'L': begin
               GPlayer.Looping := not GPlayer.Looping;
               if GPlayer.Looping then
                 WriteLn(#13, 'Looping: ON                                              ')
               else
                 WriteLn(#13, 'Looping: OFF                                             ');
             end;
        '1': begin
               GPlayer.ToggleVoiceMute(0);
               if GPlayer.IsVoiceMuted(0) then
                 WriteLn(#13, 'Voice 1: MUTED                                           ')
               else
                 WriteLn(#13, 'Voice 1: ON                                              ');
             end;
        '2': begin
               GPlayer.ToggleVoiceMute(1);
               if GPlayer.IsVoiceMuted(1) then
                 WriteLn(#13, 'Voice 2: MUTED                                           ')
               else
                 WriteLn(#13, 'Voice 2: ON                                              ');
             end;
        '3': begin
               GPlayer.ToggleVoiceMute(2);
               if GPlayer.IsVoiceMuted(2) then
                 WriteLn(#13, 'Voice 3: MUTED                                           ')
               else
                 WriteLn(#13, 'Voice 3: ON                                              ');
             end;
      end;

      // Update at frame rate
      if SDL_GetTicks - ALastUpdate >= AFrameTime then
      begin
        ALastUpdate := SDL_GetTicks;

        // Process next frame - only when playing
        if GPlayer.State = spsPlaying then
        begin
          if not GPlayer.Update then
          begin
            // Playback finished
            if not GPlayer.Looping then
            begin
              WriteLn;
              WriteLn('Playback finished.');
              Break;
            end;
          end;
        end;

        // Show progress
        ShowProgress;
      end;

      // Small sleep to avoid CPU hogging
      SDL_Delay(1);
    end;

    WriteLn;
    WriteLn('Stopping...');
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
