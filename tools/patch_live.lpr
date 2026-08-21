program patch_live;
// ============================================================================
// Play a .patch live, and RELOAD IT WHENEVER THE FILE CHANGES ON DISK.
//
// This is the ear loop the whole workbench exists for: keep this running, edit
// the patch in a text editor, save, and hear the change. No restart, no menu,
// no rebuilding. The instrument is the file.
//
//   patch_live <file.patch> [polyphony]
//
// Keys: z s x d c v g b h n j m  = one octave from C, like a tracker keyboard
//       , .   octave down / up      SPACE  all notes off
//       R     force a reload         I     show the compiled stages
//       Q     quit
//
// Must be run from a REAL terminal: with stdin redirected the keyboard stays
// inert by design (see sng_player, which had the same trap).
// ============================================================================
{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  // cthreads MUST stay first on Unix: the backend fills the buffer from its own
  // thread, so the threaded memory manager has to be installed before anything.
  {$IFDEF UNIX}cthreads, BaseUnix, TermIO,{$ENDIF}
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, ctypes, SDL2, SedaiAudioTypes, SedaiAudioBackend,
  SedaiAudioSDL2Dyn, SedaiPatchGraph, SedaiPatchModules, SedaiPatchVoices,
  SedaiPatchEvents, SedaiMIDIInput;

const
  // Settable too: a server that refuses to change rate resamples instead, and
  // its resampler both costs latency and buffers. Matching the server's own
  // rate takes it out of the path entirely — check with
  //   pw-metadata -n settings | grep clock.rate
  DEF_RATE    = 44100;
  // Settable, because latency is the whole point of playing live: at 1024 the
  // key-to-sound round trip is about 59 ms and feels like rubber; at 128 it is
  // about 8 ms, which is inside what an acoustic player already lives with
  // (three metres from an amplifier is 9 ms).
  DEF_BUFFER  = 256;
  POLL_MS     = 250;      // how often the patch file is checked for changes
  // The keyboard is polled far more often than the file. It used to share the
  // file's rate, which put up to 25 ms between a keypress and the note — more
  // than the whole audio buffer at 128 samples, and the reason lowering the
  // buffer changed nothing.
  KEY_POLL_MS = 2;
  // A terminal reports key-down and never key-up, so a note cannot be held.
  // It sounds for this long and releases itself. Pressing the key again
  // retriggers rather than silencing: the previous code toggled, and since a
  // held key auto-repeats every ~30 ms the toggle flapped on and off, which is
  // why so many presses made no sound.
  GATE_MS     = 320;
  STUCK_MS    = 20000;    // window path: only to unstick a lost key-up

var
  GPool: TSedaiPatchVoicePool;
  GBackend: TSedaiAudioBackend;
  GFile: string;
  GPoly: Integer = 8;
  GBuffer: Integer = DEF_BUFFER;
  GRate: Integer = DEF_RATE;
  GNoteAt: array[0..127] of QWord;   // when each sounding note began, 0 = silent
  GMaxHeld: Integer = 0;
  GWindow: PSDL_Window = nil;        // nil = no display, fall back to the terminal
  GOctave: Integer = 4;
  GQuit: Boolean = False;
  GMono, GRight: array[0..8191] of Single;
  GStamp: TDateTime;
  GReloads: Integer = 0;
  GMIDI: TSedaiMIDIInput = nil;
  GMidiSpec: string = '';
  GWantMIDI: Boolean = False;
  GMidiIn: Integer = 0;               // messages seen, for the closing report
  // The default bend range, and it is a CONVENTION rather than a fact: the wire
  // says how far the wheel moved, never what that means. Two semitones is what
  // nearly every instrument has meant by full deflection since the 1980s.
  GBendSemis: Single = 2.0;
  GListMIDI: Boolean = False;

// ---------------------------------------------------------------------------
// MIDI in. Everything here runs on the MAIN thread — Poll is called from the
// same loop that reloads the patch — so there is exactly one producer on the
// pool's queue, which is what the queue requires. The handlers POST; they never
// touch a voice, and no lock is taken anywhere near the audio callback.
// ---------------------------------------------------------------------------
type
  TMidiBridge = class
    procedure Note(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean);
    procedure Ctrl(AChannel, AController, AValue: Byte);
    procedure Bend(AChannel: Byte; AValue: Integer);
    procedure Press(AChannel, AValue: Byte);
  end;

var
  GBridge: TMidiBridge = nil;

procedure TMidiBridge.Note(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean);
begin
  Inc(GMidiIn);
  if GPool = nil then Exit;
  if ANoteOn then
    // 0..127 to 0..1. Linear, because anything else is a taste the instrument
    // should express, not something the driver should decide for every patch.
    GPool.PostNoteOn(ANote, AVelocity / 127.0)
  else
    GPool.PostNoteOff(ANote);
  Write(#13'  midi ', ANote, '   voci ', GPool.ActiveVoices,
        '   messaggi ', GMidiIn, '        ');
end;

procedure TMidiBridge.Ctrl(AChannel, AController, AValue: Byte);
begin
  Inc(GMidiIn);
  if GPool = nil then Exit;
  // Everything goes through, including the pedal and the panics: the pool
  // decides what is note logic, and a `cc` module in the patch decides what is
  // sound. This bridge is a wire, and a wire that filters is a bug waiting.
  GPool.PostControl(AController, AValue / 127.0);
end;

procedure TMidiBridge.Bend(AChannel: Byte; AValue: Integer);
begin
  Inc(GMidiIn);
  if GPool = nil then Exit;
  GPool.PostPitchBend(AValue / 8192.0 * GBendSemis);
end;

procedure TMidiBridge.Press(AChannel, AValue: Byte);
begin
  Inc(GMidiIn);
  if GPool = nil then Exit;
  GPool.PostPressure(AValue / 127.0);
end;

// ---------------------------------------------------------------------------
// Audio. The pool is swapped under a lock by the reloader, never mid-block.
// ---------------------------------------------------------------------------
procedure AudioCallback(AOutput: PSingle; AFrameCount: Integer; AUserData: Pointer);
var
  I: Integer;
  V: Single;
begin
  if AFrameCount > Length(GMono) then AFrameCount := Length(GMono);
  GBackend.Lock;
  try
    if GPool <> nil then
    begin
      GPool.Render(AFrameCount);
      // Take the patch's own channels when it declares two; a mono patch is
      // duplicated, which is presentation, not part of the signal.
      if GPool.OutputCount >= 2 then
        for I := 0 to AFrameCount - 1 do
        begin
          GMono[I] := GPool.MixSample(0, I);
          GRight[I] := GPool.MixSample(1, I);
        end
      else
        for I := 0 to AFrameCount - 1 do
        begin
          GMono[I] := GPool.MixSample(0, I);
          GRight[I] := GMono[I];
        end;
    end
    else
      for I := 0 to AFrameCount - 1 do begin GMono[I] := 0.0; GRight[I] := 0.0; end;
  finally
    GBackend.Unlock;
  end;

  for I := 0 to AFrameCount - 1 do
  begin
    V := GMono[I];
    if V > 1.0 then V := 1.0 else if V < -1.0 then V := -1.0;
    AOutput[I * 2] := V;
    V := GRight[I];
    if V > 1.0 then V := 1.0 else if V < -1.0 then V := -1.0;
    AOutput[I * 2 + 1] := V;
  end;
end;

// ---------------------------------------------------------------------------
// Console input, Unix and Windows. Same shape as sng_player's, which is where
// the Unix half was finally written after living as an empty stub for months.
// ---------------------------------------------------------------------------
{$IFDEF UNIX}
var
  GTermSaved: TermIOs;
  GTermRaw: Boolean = False;

procedure RestoreConsole;
begin
  if GTermRaw then
  begin
    TCSetAttr(0, TCSANOW, GTermSaved);
    GTermRaw := False;
  end;
end;

procedure TermSignal(ASignal: LongInt); cdecl;
begin
  RestoreConsole;
  fpSignal(ASignal, SignalHandler(SIG_DFL));
  fpKill(fpGetpid, ASignal);
end;

procedure InitConsole;
var
  T: TermIOs;
begin
  if TCGetAttr(0, GTermSaved) <> 0 then Exit;   // not a terminal: stay inert
  T := GTermSaved;
  T.c_lflag := T.c_lflag and (not (ICANON or ECHO));
  T.c_cc[VMIN] := 0;
  T.c_cc[VTIME] := 0;
  if TCSetAttr(0, TCSANOW, T) <> 0 then Exit;
  GTermRaw := True;
  fpSignal(SIGINT, SignalHandler(@TermSignal));
  fpSignal(SIGTERM, SignalHandler(@TermSignal));
  fpSignal(SIGHUP, SignalHandler(@TermSignal));
end;

function ReadKey(out AKey: Char): Boolean;
begin
  Result := GTermRaw and (fpRead(0, AKey, 1) = 1);
end;
{$ELSE}
var
  GStdIn: THandle;
  GOldMode: DWORD;

procedure InitConsole;
begin
  GStdIn := GetStdHandle(STD_INPUT_HANDLE);
  if GStdIn <> INVALID_HANDLE_VALUE then
  begin
    GetConsoleMode(GStdIn, @GOldMode);
    SetConsoleMode(GStdIn, GOldMode and (not (ENABLE_LINE_INPUT or ENABLE_ECHO_INPUT)));
  end;
end;

procedure RestoreConsole;
begin
  if GStdIn <> INVALID_HANDLE_VALUE then SetConsoleMode(GStdIn, GOldMode);
end;

function ReadKey(out AKey: Char): Boolean;
var
  Rec: INPUT_RECORD;
  N: DWORD;
begin
  Result := False;
  AKey := #0;
  if GStdIn = INVALID_HANDLE_VALUE then Exit;
  if not GetNumberOfConsoleInputEvents(GStdIn, @N) then Exit;
  while N > 0 do
  begin
    if not ReadConsoleInput(GStdIn, @Rec, 1, @N) then Exit;
    if (Rec.EventType = KEY_EVENT) and Rec.Event.KeyEvent.bKeyDown then
    begin
      AKey := Rec.Event.KeyEvent.AsciiChar;
      Exit(AKey <> #0);
    end;
    if not GetNumberOfConsoleInputEvents(GStdIn, @N) then Exit;
  end;
end;
{$ENDIF}

// ---------------------------------------------------------------------------

{ A terminal reports key-down and never key-up, so a note can be struck but not
  held — and a held key auto-repeats, which retriggers instead of sustaining.
  Neither lets you judge a pad. An SDL2 window gives KEYDOWN and KEYUP as
  separate events plus a repeat flag to discard, costs no system permission
  (unlike evdev) and works the same on Windows. It is opened when a display is
  there and skipped when it is not, so this still runs over ssh. }

{ One key, one meaning, whichever door it came through. }
procedure HandleKeyDown(AKey: Char); forward;

function OpenKeyWindow: Boolean;
begin
  Result := False;
  if not EnsureAudioSDL2Bound then Exit;
  if not Assigned(SDL_CreateWindow) then Exit;
  if SDL_InitSubSystem(SDL_INIT_VIDEO) <> 0 then Exit;
  GWindow := SDL_CreateWindow('patch_live — keyboard', 100, 100, 420, 120, 4 { SDL_WINDOW_SHOWN });
  if GWindow = nil then Exit;
  if Assigned(SDL_RaiseWindow) then SDL_RaiseWindow(GWindow);
  Result := True;
end;

procedure CloseKeyWindow;
begin
  if (GWindow <> nil) and Assigned(SDL_DestroyWindow) then SDL_DestroyWindow(GWindow);
  GWindow := nil;
end;

{ How many keys are down right now. A PC keyboard is a matrix and stops
  reporting past a handful of simultaneous keys — which ones, and how many,
  depends on their positions and not on the count. Showing it here means the
  tool answers "is this me or the keyboard?" without a separate probe. }
function HeldCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 127 do
    if GNoteAt[I] <> 0 then Inc(Result);
end;

function KeyToSemitone(AKey: Char): Integer;
begin
  case LowerCase(AKey) of
    'z': Result := 0;   's': Result := 1;   'x': Result := 2;
    'd': Result := 3;   'c': Result := 4;   'v': Result := 5;
    'g': Result := 6;   'b': Result := 7;   'h': Result := 8;
    'n': Result := 9;   'j': Result := 10;  'm': Result := 11;
  else
    Result := -1;
  end;
end;

// Load the patch into a fresh pool and swap it in. If it fails to compile the
// old pool keeps playing and the error is printed — a broken save must never
// leave you in silence wondering what happened.
function ReloadPatch: Boolean;
var
  NewPool, Old: TSedaiPatchVoicePool;
begin
  Result := False;
  NewPool := TSedaiPatchVoicePool.Create;
  if not NewPool.LoadFromFile(GFile, GPoly) then
  begin
    WriteLn(#13'  ! ', NewPool.LastError);
    NewPool.Free;
    Exit;
  end;
  NewPool.Prepare(GRate, GBuffer);
  NewPool.Reset;

  GBackend.Lock;
  try
    Old := GPool;
    GPool := NewPool;
  finally
    GBackend.Unlock;
  end;
  Old.Free;

  Inc(GReloads);
  FillChar(GNoteAt, SizeOf(GNoteAt), 0);
  Result := True;
end;

function StampOf(const AFile: string): TDateTime;
var
  Rec: TSearchRec;
begin
  Result := 0;
  if FindFirst(AFile, faAnyFile, Rec) = 0 then
  begin
    Result := FileDateToDateTime(Rec.Time);
    FindClose(Rec);
  end;
end;

procedure ShowStages;
begin
  WriteLn;
  Write(GPool.Describe);
end;

var
  K: Char;
  LastFileCheck, NowMs: QWord;
  Idx: Integer;
  Lim: QWord;
  Ev: TSDL_Event;
  UseWindow: Boolean;
  Semi, Note, I, Positional: Integer;
  NowStamp: TDateTime;
  Arg: string;
  MidiPorts: TSedaiMIDIPortArray;

procedure HandleKeyDown(AKey: Char);
var
  Semi, Note: Integer;
begin
  if AKey in [#27, 'q', 'Q'] then begin GQuit := True; Exit; end;
  if AKey in ['r', 'R'] then
  begin
    GStamp := StampOf(GFile);
    if ReloadPatch then WriteLn(#13'  reloaded (', GReloads, ')');
    Exit;
  end;
  if AKey in ['i', 'I'] then begin ShowStages; Exit; end;
  if AKey = ' ' then
  begin
    GPool.PostAllNotesOff;
    FillChar(GNoteAt, SizeOf(GNoteAt), 0);
    Exit;
  end;
  if AKey = ',' then
  begin
    if GOctave > 0 then Dec(GOctave);
    Write(#13'  octave ', GOctave, '    ');
    Exit;
  end;
  if AKey = '.' then
  begin
    if GOctave < 8 then Inc(GOctave);
    Write(#13'  octave ', GOctave, '    ');
    Exit;
  end;

  Semi := KeyToSemitone(AKey);
  if Semi < 0 then Exit;
  Note := GOctave * 12 + Semi;
  if (Note < 0) or (Note > 127) then Exit;

  // Posted, not called: the queue is the only thing that crosses to the audio
  // thread now, so this no longer takes the lock the callback also wants.
  // A key already sounding is released first, so a press retriggers rather
  // than cancelling. With the window this only happens on a genuine repeat
  // press; on the terminal it also absorbs auto-repeat.
  if GNoteAt[Note] <> 0 then GPool.PostNoteOff(Note);
  // A computer keyboard has no velocity to give, so it gives all of it.
  GPool.PostNoteOn(Note, 1.0);
  GNoteAt[Note] := GetTickCount64;
  if HeldCount > GMaxHeld then GMaxHeld := HeldCount;
  Write(#13'  note ', Note, '   tasti tenuti ', HeldCount,
        ' (max ', GMaxHeld, ')   voci ', GPool.ActiveVoices, '     ');
end;

begin
  // Flags may appear anywhere; everything that is not a flag keeps the old
  // positional order, so every command line that worked before still does.
  GFile := '';
  Positional := 0;
  for I := 1 to ParamCount do
  begin
    Arg := ParamStr(I);
    if Arg = '--list-midi' then GListMIDI := True
    else if Arg = '--midi' then GWantMIDI := True
    else if Copy(Arg, 1, 7) = '--midi=' then
    begin
      GWantMIDI := True;
      GMidiSpec := Copy(Arg, 8, Length(Arg));
    end
    else if Copy(Arg, 1, 7) = '--bend=' then
      GBendSemis := StrToFloatDef(Copy(Arg, 8, Length(Arg)), 2.0)
    else
    begin
      Inc(Positional);
      case Positional of
        1: GFile := Arg;
        2: GPoly := StrToIntDef(Arg, 8);
        3: GBuffer := StrToIntDef(Arg, DEF_BUFFER);
        4: GRate := StrToIntDef(Arg, DEF_RATE);
      end;
    end;
  end;

  // Listing the ports needs no patch and no audio device, so it answers before
  // anything else is set up — which is the whole point of having it here.
  if GListMIDI then
  begin
    if not TSedaiMIDIInput.Available then
    begin
      WriteLn('  nessun backend MIDI su questa macchina.');
      Halt(1);
    end;
    GMIDI := TSedaiMIDIInput.Create;
    try
      if GMIDI.Enumerate(MidiPorts) then
      begin
        WriteLn;
        WriteLn('  ', TSedaiMIDIInput.BackendName, ' — ', Length(MidiPorts), ' porte');
        WriteLn;
        for I := 0 to High(MidiPorts) do
          WriteLn(Format('   %3d:%-2d  %-4s  %s: %s',
                  [MidiPorts[I].Client, MidiPorts[I].Port,
                   BoolToStr(MidiPorts[I].CanSend, 'in', '--'),
                   MidiPorts[I].ClientName, MidiPorts[I].PortName]));
        WriteLn;
        WriteLn('  "in" = puo'' mandarci note.  Uso: patch_live <patch> --midi=24:0');
        WriteLn;
      end
      else
        WriteLn(StdErr, '  ', GMIDI.LastError);
    finally
      GMIDI.Free;
      GMIDI := nil;
    end;
    Halt(0);
  end;

  if GFile = '' then
  begin
    WriteLn('usage: patch_live <file.patch> [polyphony] [buffer] [rate] [flags]');
    WriteLn('       buffer in samples: 64 128 256 512 1024 (default ', DEF_BUFFER, ')');
    WriteLn('       rate: match the audio server or it resamples (default ', DEF_RATE, ')');
    WriteLn('flags: --midi            connetti a tutto cio'' che puo'' inviare');
    WriteLn('       --midi=<porta>    "24:0" o un pezzo del nome della porta');
    WriteLn('       --list-midi       elenca le porte ed esci');
    WriteLn('       --bend=<semitoni> escursione del pitch bend (default 2)');
    WriteLn('keys : z s x d c v g b h n j m = notes,  , . = octave,  SPACE = all off');
    WriteLn('       R = reload,  I = stages,  Q = quit');
    Halt(2);
  end;
  if GPoly < 1 then GPoly := 1;
  if GBuffer < 32 then GBuffer := 32;
  if GRate < 8000 then GRate := DEF_RATE;

  GPool := TSedaiPatchVoicePool.Create;
  if not GPool.LoadFromFile(GFile, GPoly) then
  begin
    WriteLn(StdErr, GPool.LastError);
    Halt(1);
  end;
  if GPool.LastError <> '' then WriteLn('  ', GPool.LastError);
  if GPool.Warnings <> '' then Write(GPool.Warnings);
  GPool.Prepare(GRate, GBuffer);
  GPool.Reset;
  GStamp := StampOf(GFile);

  GBackend := TSedaiAudioBackend.Create;
  GBackend.SetSampleRate(GRate);
  GBackend.SetDesiredBufferSize(GBuffer);
  GBackend.SetChannels(2);
  GBackend.SetCallback(@AudioCallback, nil);
  GBackend.SetMode(bmCallback);

  if not GBackend.Initialize then
  begin
    WriteLn(StdErr, 'audio device would not open (on Linux this usually means the');
    WriteLn(StdErr, 'SDL2 -dev package is missing: the bindings dlopen libSDL2.so)');
    Halt(1);
  end;
  if not GBackend.Start then
  begin
    WriteLn(StdErr, 'audio device would not start');
    Halt(1);
  end;

  UseWindow := OpenKeyWindow;
  InitConsole;
  WriteLn;
  WriteLn('  ', ExtractFileName(GFile), '  —  ', GPoly, ' voices, ',
          GBackend.SampleRate, ' Hz, buffer ', GBackend.BufferSize,
          ' (', (GBackend.BufferSize * 1000.0 / GRate):0:1, ' ms/blocco)');
  Write(GPool.Describe);
  // MIDI is opened after the audio device, so a failure here leaves a working
  // instrument you can still play from the computer keyboard rather than an
  // exit. Deafness is a degradation, not a fault.
  if GWantMIDI then
  begin
    if not TSedaiMIDIInput.Available then
      WriteLn('  midi: nessun backend (libasound2 non installata) — solo tastiera')
    else
    begin
      GMIDI := TSedaiMIDIInput.Create;
      if not GMIDI.Open('patch_live') then
      begin
        WriteLn('  midi: ', GMIDI.LastError);
        FreeAndNil(GMIDI);
      end
      else
      begin
        GBridge := TMidiBridge.Create;
        GMIDI.OnNote := @GBridge.Note;
        GMIDI.OnController := @GBridge.Ctrl;
        GMIDI.OnPitchBend := @GBridge.Bend;
        GMIDI.OnPressure := @GBridge.Press;
        if GMidiSpec <> '' then I := GMIDI.ConnectByName(GMidiSpec)
        else I := GMIDI.ConnectAnything;
        WriteLn('  midi: porta ', GMIDI.PortSpec, ', ', I, ' sorgenti collegate',
                '  (bend ', GBendSemis:0:1, ' semitoni)');
        if I = 0 then
        begin
          // Worth saying plainly: connected to nothing looks exactly like
          // broken, and one of the two is fixed with a single command.
          WriteLn('  midi: NIENTE COLLEGATO. Da un altro terminale:');
          WriteLn('        aconnect -l                     # cosa c''e''');
          WriteLn('        aconnect <sorgente> ', GMIDI.PortSpec, '        # collega');
          WriteLn('        aplaymidi -p ', GMIDI.PortSpec, ' <file.mid>   # o suona un file');
        end;
      end;
    end;
  end;
  if UseWindow then
    WriteLn('  input: SDL2 window (give it focus). Notes last as long as the key.')
  else
    WriteLn('  input: terminal — no display found. Notes release after ',
            GATE_MS, ' ms; a key cannot be held.');
  WriteLn('  editing the file and saving reloads it.');
  WriteLn('  white  z x c v b n m     black  s d  g h j');
  WriteLn('  , . octave   SPACE all off   R reload   I stages   Q quit');
  WriteLn;

  try
    LastFileCheck := GetTickCount64;
    while not GQuit do
    begin
      // The patch file is the instrument: watch it and pick up every save.
      // Checked on its own slow clock — a stat() every 2 ms would be silly.
      if GetTickCount64 - LastFileCheck >= POLL_MS then
      begin
      LastFileCheck := GetTickCount64;
      NowStamp := StampOf(GFile);
      if (NowStamp <> 0) and (NowStamp <> GStamp) then
      begin
        GStamp := NowStamp;
        Sleep(60);          // let the editor finish writing before reading
        if ReloadPatch then
        begin
          Write(#13'  reloaded (', GReloads, ')   ');
          Write(GPool.Describe);
        end;
      end;
      end;

      // MIDI, on this same thread: one producer on the pool's queue, which is
      // what it requires. Polled here rather than from a thread of its own —
      // this loop wakes every 2 ms, below the audio buffer at every setting.
      if GMIDI <> nil then GMIDI.Poll;

      if GWindow <> nil then
      begin
        // Real key-up: a note lasts exactly as long as the finger does.
        while SDL_PollEvent(@Ev) = 1 do
        begin
          if Ev.type_ = SDL_QUITEV then GQuit := True
          else if (Ev.type_ = SDL_KEYDOWN) and (Ev.key.repeat_ = 0) then
            HandleKeyDown(Chr(Ev.key.keysym.sym and $FF))
          else if Ev.type_ = SDL_KEYUP then
          begin
            Semi := KeyToSemitone(Chr(Ev.key.keysym.sym and $FF));
            if Semi >= 0 then
            begin
              Note := GOctave * 12 + Semi;
              if (Note >= 0) and (Note <= 127) and (GNoteAt[Note] <> 0) then
              begin
                GPool.PostNoteOff(Note);
                GNoteAt[Note] := 0;
                Write(#13'  off  ', Note, '   tasti tenuti ', HeldCount,
                      ' (max ', GMaxHeld, ')   voci ', GPool.ActiveVoices, '     ');
              end;
            end;
          end;
        end;
      end;

      // Time-based release. On the terminal it IS the note length, because a
      // key cannot be held. With the window it is only a safety net, set far
      // longer than any real note, so a lost key-up cannot leave a voice stuck
      // sounding forever.
      if UseWindow then Lim := STUCK_MS else Lim := GATE_MS;
      NowMs := GetTickCount64;
      for Idx := 0 to 127 do
        if (GNoteAt[Idx] <> 0) and (NowMs - GNoteAt[Idx] >= Lim) then
        begin
          GPool.PostNoteOff(Idx);
          GNoteAt[Idx] := 0;
        end;

      // Only one door at a time. With the window open the terminal must stay
      // shut: it reports key-down and never key-up, so a key typed into the
      // terminal by mistake would start a note that nothing ever ends.
      if not UseWindow then
        while ReadKey(K) do HandleKeyDown(K);

      Sleep(KEY_POLL_MS);
    end;
  finally
    if GMIDI <> nil then GMIDI.Close;
    GBackend.Lock;
    try GPool.AllNotesOff; finally GBackend.Unlock; end;
    Sleep(120);
    GBackend.Stop;
    GBackend.Shutdown;
    GBackend.Free;
    GPool.Free;
    FreeAndNil(GBridge);
    FreeAndNil(GMIDI);
    RestoreConsole;
  end;

  WriteLn;
  WriteLn;
  WriteLn('  done. ', GReloads, ' reloads.  Massimo di tasti tenuti insieme: ', GMaxHeld, '.');
  if GMidiIn > 0 then
    WriteLn('  MIDI: ', GMidiIn, ' messaggi ricevuti.')
  else if GWantMIDI then
    WriteLn('  MIDI: nessun messaggio ricevuto. Era collegato qualcosa?');
  if GMaxHeld > 0 then
  begin
    if GMaxHeld < 6 then
      WriteLn('  Se premendone di piu'' non saliva, il limite e'' della TASTIERA:'#10 +
              '  una matrice a membrana blocca oltre 2-4 tasti secondo quali sono.'#10 +
              '  Prova tasti lontani (z + m + .) contro vicini (z x c v).')
    else
      WriteLn('  La tastiera ne regge almeno ', GMaxHeld, ': il limite non e'' li''.');
  end;
end.
