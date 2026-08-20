// ============================================================================
// SedaiMIDIInput — live MIDI input. ALSA sequencer on Linux, winmm on Windows.
//
// The other end of SedaiPatchEvents: this turns a wire into note events, and the
// queue carries them to the audio thread. Nothing here touches a voice.
//
// THE LIBRARY IS LOADED AT RUNTIME, never linked. Same reasoning as
// SedaiAudioSDL2Dyn: a static import makes the OS loader map libasound on every
// launch of every host, including the ones that will never open a port, and a
// machine without it could not start the program at all. Loaded this way, a
// missing library is the honest answer "there is no MIDI here" rather than a
// failure to launch.
//
// POLLED, NOT THREADED, and that is a decision worth defending. A thread
// blocking on the port would shave the poll interval off the input latency —
// but the host that wants it (patch_live) already runs a 2 ms loop, and its own
// audio buffer is 2.9 ms at the shortest setting it offers and 23 ms at the
// default. A thread, and the shutdown problem that comes with unblocking one,
// buys less than the jitter already there. Call Poll from wherever the host
// already wakes up; if a measurement ever says the interval matters, a blocking
// version goes behind the same Poll and nothing above it changes.
//
// ON WINDOWS THE CALLBACK IS DIFFERENT and cannot be helped: winmm delivers
// from a system thread on which almost nothing is legal — no allocation, no
// locks, no system calls. So there the callback does the one thing it may and
// pushes the packed message into a ring; Poll drains it on the host's thread,
// where the handlers may do as they please. Same API, same events, different
// machinery underneath.
//
// TIMESTAMPS. Events are reported as "now". Without a clock shared between the
// sequencer and the audio device, a sample position would be a guess dressed up
// as precision. The sample-accurate path in SedaiPatchEvents exists for
// producers that genuinely know their position — a file player does — and a
// keyboard is not one of them.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiMIDIInput;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

const
  // Only used by the Windows branch, but declared unconditionally so the class
  // below is the same shape on both platforms.
  SEDAI_MIDI_RING = 1024;      // power of two, ~16 KB, never touched on Linux

type
  // One port somebody else offers. Client and Port are the numbers ALSA and
  // `aconnect` speak in; on Windows Client is always 0 and Port is the device
  // index, so a caller addresses either with the same pair.
  TSedaiMIDIPortInfo = record
    Client: Integer;
    Port: Integer;
    ClientName: string;
    PortName: string;
    // True when this port can SEND to us — which is what a keyboard is, and
    // what most of the ports on a machine are not.
    CanSend: Boolean;
  end;
  TSedaiMIDIPortArray = array of TSedaiMIDIPortInfo;

  // Fired from whatever thread calls Poll. Velocity and value are raw MIDI
  // 0..127: the caller scales them, because only the caller knows to what.
  TSedaiMIDINoteEvent = procedure(AChannel, ANote, AVelocity: Byte;
                                  ANoteOn: Boolean) of object;
  TSedaiMIDICtrlEvent = procedure(AChannel, AController, AValue: Byte) of object;
  // -8192..+8191, centre 0. Deliberately NOT converted to semitones here: the
  // bend range is a property of the instrument, not of the wire.
  TSedaiMIDIBendEvent = procedure(AChannel: Byte; AValue: Integer) of object;

  { TSedaiMIDIInput }

  TSedaiMIDIInput = class
  private
  {$IFDEF UNIX}
    FSeq: Pointer;
  {$ENDIF}
  {$IFDEF WINDOWS}
    FHandle: Pointer;
    // Single producer (the winmm system thread), single consumer (Poll).
    // Free-running counters masked on use, same shape as SedaiPatchEvents.
    FRing: array[0..SEDAI_MIDI_RING - 1] of LongWord;
    FHead, FTail: Cardinal;
    FRingDropped: Cardinal;
  {$ENDIF}
    FOpen: Boolean;
    FClientId: Integer;
    FPortId: Integer;
    FLastError: string;
    FConnected: Integer;
    FNoteCount: QWord;
    FOnNote: TSedaiMIDINoteEvent;
    FOnController: TSedaiMIDICtrlEvent;
    FOnPitchBend: TSedaiMIDIBendEvent;
    procedure DoNote(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean);
    procedure DoController(AChannel, AController, AValue: Byte);
    procedure DoBend(AChannel: Byte; AValue: Integer);
    // Decode one packed MIDI message, status in the low byte. Used by the
    // Windows path; the wire is the same even when the driver is not.
    procedure DispatchRaw(AMessage: LongWord);
  public
    constructor Create;
    destructor Destroy; override;

    // Is the platform's MIDI library present at all? Cheap after the first call.
    class function Available: Boolean;
    class function BackendName: string;

    // Every port on the machine, whether it can send to us or not — a caller
    // showing the user a list should show all of them and say which is which.
    // Works whether or not this object is open.
    function Enumerate(out APorts: TSedaiMIDIPortArray): Boolean;

    // Create our own input port. AClientName is what other programs will see.
    function Open(const AClientName: string = 'Sedai'): Boolean;
    procedure Close;

    // Subscribe to a sender. On Windows there is no routing layer, so the FIRST
    // connect chooses the device and later ones are refused.
    function ConnectFrom(AClient, APort: Integer): Boolean;
    // "24:0", or any part of a port's or client's name, case-insensitive.
    // Returns how many ports were connected — 0 means nothing matched, which is
    // worth saying out loud rather than starting silently deaf.
    function ConnectByName(const ASpec: string): Integer;
    // Everything that can send. The sane default for "just let me play".
    function ConnectAnything: Integer;

    // Drain whatever has arrived and fire the events. Returns how many messages
    // were handled. Never blocks.
    function Poll: Integer;

    property IsOpen: Boolean read FOpen;
    property ClientId: Integer read FClientId;
    property PortId: Integer read FPortId;
    // What to hand to `aplaymidi -p`, or to type into `aconnect`. Empty when
    // closed, and on Windows always empty: there is nothing to address.
    function PortSpec: string;
    property Connected: Integer read FConnected;
    property NotesSeen: QWord read FNoteCount;
    // Messages lost because the ring was full. Windows only; on Linux ALSA owns
    // the buffering and reports its own overruns.
    function RingDropped: Cardinal;
    property LastError: string read FLastError;

    property OnNote: TSedaiMIDINoteEvent read FOnNote write FOnNote;
    property OnController: TSedaiMIDICtrlEvent read FOnController write FOnController;
    property OnPitchBend: TSedaiMIDIBendEvent read FOnPitchBend write FOnPitchBend;
  end;

implementation

uses
  ctypes, dynlibs;

{$IFDEF UNIX}
// ---------------------------------------------------------------------------
// ALSA sequencer.
//
// snd_seq_event_t is 28 bytes and has NO PADDING — which is only true because
// snd_seq_ev_ext is declared __attribute__((packed)) in the header. Without
// that, the 64-bit pointer inside it would widen the data union from 12 bytes
// to 16 and every field after `time` would be read from the wrong place. The
// size is asserted at open time rather than trusted, because getting it wrong
// does not crash: it plays the wrong notes, which is far harder to diagnose.
// ---------------------------------------------------------------------------
const
  ALSA_LIB = 'libasound.so.2';

  SND_SEQ_OPEN_INPUT          = 2;
  SND_SEQ_NONBLOCK            = 1;

  SND_SEQ_PORT_CAP_READ       = 1 shl 0;
  SND_SEQ_PORT_CAP_WRITE      = 1 shl 1;
  SND_SEQ_PORT_CAP_SUBS_READ  = 1 shl 5;
  SND_SEQ_PORT_CAP_SUBS_WRITE = 1 shl 6;

  SND_SEQ_PORT_TYPE_MIDI_GENERIC = 1 shl 1;
  SND_SEQ_PORT_TYPE_APPLICATION  = 1 shl 20;

  SND_SEQ_EVENT_NOTEON     = 6;
  SND_SEQ_EVENT_NOTEOFF    = 7;
  SND_SEQ_EVENT_CONTROLLER = 10;
  SND_SEQ_EVENT_PITCHBEND  = 13;

  SND_SEQ_EVENT_SIZE = 28;

type
  TSndSeqRealTime = packed record
    tv_sec, tv_nsec: cuint32;
  end;
  TSndSeqTimestamp = packed record
    case Integer of
      0: (tick: cuint32);
      1: (time: TSndSeqRealTime);
  end;
  TSndSeqAddr = packed record
    client, port: cuchar;
  end;
  TSndSeqEvNote = packed record
    channel, note, velocity, off_velocity: cuchar;
    duration: cuint32;
  end;
  TSndSeqEvCtrl = packed record
    channel: cuchar;
    unused: array[0..2] of cuchar;
    param: cuint32;
    value: cint32;
  end;
  TSndSeqEventData = packed record
    case Integer of
      0: (note: TSndSeqEvNote);
      1: (control: TSndSeqEvCtrl);
      2: (raw8: array[0..11] of cuchar);
  end;
  PSndSeqEvent = ^TSndSeqEvent;
  TSndSeqEvent = packed record
    etype: cuchar;
    flags: cuchar;
    tag: cuchar;
    queue: cuchar;
    time: TSndSeqTimestamp;
    source: TSndSeqAddr;
    dest: TSndSeqAddr;
    data: TSndSeqEventData;
  end;

var
  GAlsa: TLibHandle = NilHandle;
  GAlsaTried: Boolean = False;
  GAlsaOk: Boolean = False;

  snd_seq_open: function(handle: PPointer; name: PAnsiChar;
                         streams, mode: cint): cint; cdecl;
  snd_seq_close: function(handle: Pointer): cint; cdecl;
  snd_seq_set_client_name: function(seq: Pointer; name: PAnsiChar): cint; cdecl;
  snd_seq_client_id: function(handle: Pointer): cint; cdecl;
  snd_seq_create_simple_port: function(seq: Pointer; name: PAnsiChar;
                                       caps, ptype: cuint): cint; cdecl;
  snd_seq_delete_simple_port: function(seq: Pointer; port: cint): cint; cdecl;
  snd_seq_connect_from: function(seq: Pointer; my_port, src_client,
                                 src_port: cint): cint; cdecl;
  snd_seq_event_input: function(handle: Pointer; ev: PPointer): cint; cdecl;
  snd_seq_event_input_pending: function(seq: Pointer; fetch: cint): cint; cdecl;

  snd_seq_client_info_sizeof: function: csize_t; cdecl;
  snd_seq_client_info_set_client: procedure(info: Pointer; client: cint); cdecl;
  snd_seq_client_info_get_client: function(info: Pointer): cint; cdecl;
  snd_seq_client_info_get_name: function(info: Pointer): PAnsiChar; cdecl;
  snd_seq_query_next_client: function(handle, info: Pointer): cint; cdecl;

  snd_seq_port_info_sizeof: function: csize_t; cdecl;
  snd_seq_port_info_set_client: procedure(info: Pointer; client: cint); cdecl;
  snd_seq_port_info_set_port: procedure(info: Pointer; port: cint); cdecl;
  snd_seq_port_info_get_port: function(info: Pointer): cint; cdecl;
  snd_seq_port_info_get_name: function(info: Pointer): PAnsiChar; cdecl;
  snd_seq_port_info_get_capability: function(info: Pointer): cuint; cdecl;
  snd_seq_query_next_port: function(handle, info: Pointer): cint; cdecl;

  snd_strerror: function(errnum: cint): PAnsiChar; cdecl;

function BindAlsa: Boolean;
var
  Missing: Boolean;

  function Sym(const AName: string): Pointer;
  begin
    Result := GetProcedureAddress(GAlsa, AName);
    if Result = nil then Missing := True;
  end;

begin
  if GAlsaTried then Exit(GAlsaOk);
  GAlsaTried := True;
  GAlsaOk := False;

  GAlsa := LoadLibrary(ALSA_LIB);
  if GAlsa = NilHandle then Exit(False);

  Missing := False;
  Pointer(snd_seq_open) := Sym('snd_seq_open');
  Pointer(snd_seq_close) := Sym('snd_seq_close');
  Pointer(snd_seq_set_client_name) := Sym('snd_seq_set_client_name');
  Pointer(snd_seq_client_id) := Sym('snd_seq_client_id');
  Pointer(snd_seq_create_simple_port) := Sym('snd_seq_create_simple_port');
  Pointer(snd_seq_delete_simple_port) := Sym('snd_seq_delete_simple_port');
  Pointer(snd_seq_connect_from) := Sym('snd_seq_connect_from');
  Pointer(snd_seq_event_input) := Sym('snd_seq_event_input');
  Pointer(snd_seq_event_input_pending) := Sym('snd_seq_event_input_pending');

  Pointer(snd_seq_client_info_sizeof) := Sym('snd_seq_client_info_sizeof');
  Pointer(snd_seq_client_info_set_client) := Sym('snd_seq_client_info_set_client');
  Pointer(snd_seq_client_info_get_client) := Sym('snd_seq_client_info_get_client');
  Pointer(snd_seq_client_info_get_name) := Sym('snd_seq_client_info_get_name');
  Pointer(snd_seq_query_next_client) := Sym('snd_seq_query_next_client');

  Pointer(snd_seq_port_info_sizeof) := Sym('snd_seq_port_info_sizeof');
  Pointer(snd_seq_port_info_set_client) := Sym('snd_seq_port_info_set_client');
  Pointer(snd_seq_port_info_set_port) := Sym('snd_seq_port_info_set_port');
  Pointer(snd_seq_port_info_get_port) := Sym('snd_seq_port_info_get_port');
  Pointer(snd_seq_port_info_get_name) := Sym('snd_seq_port_info_get_name');
  Pointer(snd_seq_port_info_get_capability) := Sym('snd_seq_port_info_get_capability');
  Pointer(snd_seq_query_next_port) := Sym('snd_seq_query_next_port');

  Pointer(snd_strerror) := Sym('snd_strerror');

  // A library that loaded but is missing a symbol must not be used at all:
  // half-bound pointers are a nil call waiting for the worst moment.
  if Missing then
  begin
    UnloadLibrary(GAlsa);
    GAlsa := NilHandle;
    Exit(False);
  end;

  GAlsaOk := True;
  Result := True;
end;

function AlsaErr(ACode: Integer): string;
begin
  if Assigned(snd_strerror) then Result := string(snd_strerror(ACode))
  else Result := Format('error %d', [ACode]);
end;
{$ENDIF}

{$IFDEF WINDOWS}
// ---------------------------------------------------------------------------
// winmm.
// ---------------------------------------------------------------------------
const
  WINMM_LIB         = 'winmm.dll';
  MIM_DATA          = 963;
  CALLBACK_FUNCTION = $00030000;
  MAXPNAMELEN       = 32;

type
  TMidiInCaps = packed record
    wMid: Word;
    wPid: Word;
    vDriverVersion: LongWord;
    szPname: array[0..MAXPNAMELEN - 1] of AnsiChar;
    dwSupport: LongWord;
  end;

var
  GWinMM: TLibHandle = NilHandle;
  GWinMMTried: Boolean = False;
  GWinMMOk: Boolean = False;

  midiInGetNumDevs: function: LongWord; stdcall;
  midiInGetDevCapsA: function(uDeviceID: PtrUInt; pmic: Pointer;
                              cbmic: LongWord): LongWord; stdcall;
  midiInOpen: function(phmi: PPointer; uDeviceID: LongWord;
                       dwCallback, dwInstance: PtrUInt;
                       fdwOpen: LongWord): LongWord; stdcall;
  midiInStart: function(hmi: Pointer): LongWord; stdcall;
  midiInStop: function(hmi: Pointer): LongWord; stdcall;
  midiInReset: function(hmi: Pointer): LongWord; stdcall;
  midiInClose: function(hmi: Pointer): LongWord; stdcall;

function BindWinMM: Boolean;
begin
  if GWinMMTried then Exit(GWinMMOk);
  GWinMMTried := True;
  GWinMMOk := False;
  GWinMM := LoadLibrary(WINMM_LIB);
  if GWinMM = NilHandle then Exit(False);
  Pointer(midiInGetNumDevs)  := GetProcedureAddress(GWinMM, 'midiInGetNumDevs');
  Pointer(midiInGetDevCapsA) := GetProcedureAddress(GWinMM, 'midiInGetDevCapsA');
  Pointer(midiInOpen)        := GetProcedureAddress(GWinMM, 'midiInOpen');
  Pointer(midiInStart)       := GetProcedureAddress(GWinMM, 'midiInStart');
  Pointer(midiInStop)        := GetProcedureAddress(GWinMM, 'midiInStop');
  Pointer(midiInReset)       := GetProcedureAddress(GWinMM, 'midiInReset');
  Pointer(midiInClose)       := GetProcedureAddress(GWinMM, 'midiInClose');
  GWinMMOk := Assigned(midiInGetNumDevs) and Assigned(midiInGetDevCapsA) and
              Assigned(midiInOpen) and Assigned(midiInStart) and
              Assigned(midiInStop) and Assigned(midiInReset) and
              Assigned(midiInClose);
  if not GWinMMOk then
  begin
    UnloadLibrary(GWinMM);
    GWinMM := NilHandle;
  end;
  Result := GWinMMOk;
end;

// The system-thread callback. Everything it may not do is everything else, so
// it pushes the message and returns. Declared before the class because the
// class hands it a Self pointer as dwInstance.
procedure MidiInProc(hMidiIn: Pointer; wMsg: LongWord; dwInstance: PtrUInt;
  dwParam1, dwParam2: PtrUInt); stdcall;
var
  M: TSedaiMIDIInput;
begin
  if wMsg <> MIM_DATA then Exit;
  M := TSedaiMIDIInput(Pointer(dwInstance));
  if M = nil then Exit;
  if M.FHead - M.FTail >= SEDAI_MIDI_RING then
  begin
    Inc(M.FRingDropped);
    Exit;
  end;
  M.FRing[M.FHead and (SEDAI_MIDI_RING - 1)] := LongWord(dwParam1);
  WriteBarrier;
  M.FHead := M.FHead + 1;
end;
{$ENDIF}

{ TSedaiMIDIInput }

constructor TSedaiMIDIInput.Create;
begin
  inherited Create;
  FOpen := False;
  FClientId := -1;
  FPortId := -1;
  FConnected := 0;
  FNoteCount := 0;
  FLastError := '';
{$IFDEF UNIX}
  FSeq := nil;
{$ENDIF}
{$IFDEF WINDOWS}
  FHandle := nil;
  FHead := 0;
  FTail := 0;
  FRingDropped := 0;
{$ENDIF}
end;

destructor TSedaiMIDIInput.Destroy;
begin
  Close;
  inherited Destroy;
end;

class function TSedaiMIDIInput.Available: Boolean;
begin
{$IFDEF UNIX}
  Result := BindAlsa;
{$ELSE}
{$IFDEF WINDOWS}
  Result := BindWinMM;
{$ELSE}
  Result := False;
{$ENDIF}
{$ENDIF}
end;

class function TSedaiMIDIInput.BackendName: string;
begin
{$IFDEF UNIX}
  Result := 'ALSA sequencer';
{$ELSE}
{$IFDEF WINDOWS}
  Result := 'Windows MME';
{$ELSE}
  Result := 'none';
{$ENDIF}
{$ENDIF}
end;

// --- event dispatch, shared by both platforms ---

procedure TSedaiMIDIInput.DoNote(AChannel, ANote, AVelocity: Byte;
  ANoteOn: Boolean);
begin
  Inc(FNoteCount);
  if Assigned(FOnNote) then FOnNote(AChannel, ANote, AVelocity, ANoteOn);
end;

procedure TSedaiMIDIInput.DoController(AChannel, AController, AValue: Byte);
begin
  if Assigned(FOnController) then FOnController(AChannel, AController, AValue);
end;

procedure TSedaiMIDIInput.DoBend(AChannel: Byte; AValue: Integer);
begin
  if Assigned(FOnPitchBend) then FOnPitchBend(AChannel, AValue);
end;

procedure TSedaiMIDIInput.DispatchRaw(AMessage: LongWord);
var
  Status, Chan, D1, D2: Byte;
begin
  Status := Byte(AMessage and $FF);
  if Status < $80 then Exit;             // running status is already resolved
  D1 := Byte((AMessage shr 8) and $7F);
  D2 := Byte((AMessage shr 16) and $7F);
  Chan := Status and $0F;
  case Status and $F0 of
    $80: DoNote(Chan, D1, D2, False);
    // A note-on with velocity 0 IS a note-off. Both conventions are on the wire
    // and an instrument that honours only one of them hangs notes forever.
    $90: if D2 = 0 then DoNote(Chan, D1, 0, False)
         else DoNote(Chan, D1, D2, True);
    $B0: DoController(Chan, D1, D2);
    $E0: DoBend(Chan, (Integer(D2) shl 7) + Integer(D1) - 8192);
  end;
end;

function TSedaiMIDIInput.RingDropped: Cardinal;
begin
{$IFDEF WINDOWS}
  Result := FRingDropped;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TSedaiMIDIInput.PortSpec: string;
begin
{$IFDEF UNIX}
  if FOpen then Result := Format('%d:%d', [FClientId, FPortId]) else Result := '';
{$ELSE}
  Result := '';
{$ENDIF}
end;

// ---------------------------------------------------------------------------
{$IFDEF UNIX}

function TSedaiMIDIInput.Enumerate(out APorts: TSedaiMIDIPortArray): Boolean;
var
  Seq: Pointer;
  Temp: Boolean;
  CInfo, PInfo: Pointer;
  CSize, PSize: PtrUInt;
  Cl, Pt: Integer;
  Caps: Cardinal;
  CName: string;
  N: Integer;
  R: Integer;
begin
  SetLength(APorts, 0);
  Result := False;
  if not BindAlsa then
  begin
    FLastError := ALSA_LIB + ' is not installed';
    Exit;
  end;

  // Enumerating needs a client of our own, but not a port: if we are not open
  // yet, borrow one and give it back.
  Temp := not FOpen;
  if Temp then
  begin
    R := snd_seq_open(@Seq, 'default', SND_SEQ_OPEN_INPUT, SND_SEQ_NONBLOCK);
    if R < 0 then
    begin
      FLastError := 'cannot open the sequencer: ' + AlsaErr(R);
      Exit;
    end;
  end
  else
    Seq := FSeq;

  // The () matters: without it these name the procedure variable, not its
  // result. And the structs are opaque, so their size is asked for rather than
  // declared — which is exactly what the snd_seq_*_alloca macros do in C.
  CSize := snd_seq_client_info_sizeof();
  PSize := snd_seq_port_info_sizeof();
  CInfo := GetMem(CSize);
  PInfo := GetMem(PSize);
  try
    FillChar(PByte(CInfo)^, CSize, 0);
    FillChar(PByte(PInfo)^, PSize, 0);
    N := 0;
    snd_seq_client_info_set_client(CInfo, -1);
    while snd_seq_query_next_client(Seq, CInfo) >= 0 do
    begin
      Cl := snd_seq_client_info_get_client(CInfo);
      CName := string(snd_seq_client_info_get_name(CInfo));
      snd_seq_port_info_set_client(PInfo, Cl);
      snd_seq_port_info_set_port(PInfo, -1);
      while snd_seq_query_next_port(Seq, PInfo) >= 0 do
      begin
        Pt := snd_seq_port_info_get_port(PInfo);
        Caps := snd_seq_port_info_get_capability(PInfo);
        SetLength(APorts, N + 1);
        APorts[N].Client := Cl;
        APorts[N].Port := Pt;
        APorts[N].ClientName := CName;
        APorts[N].PortName := string(snd_seq_port_info_get_name(PInfo));
        // It can feed us only if it is readable AND accepts a subscription.
        // Readable alone is not enough and is the usual reason a connect that
        // "should have worked" quietly did not.
        APorts[N].CanSend :=
          ((Caps and SND_SEQ_PORT_CAP_READ) <> 0) and
          ((Caps and SND_SEQ_PORT_CAP_SUBS_READ) <> 0);
        Inc(N);
      end;
    end;
    Result := True;
  finally
    FreeMem(PInfo);
    FreeMem(CInfo);
    if Temp then snd_seq_close(Seq);
  end;
end;

function TSedaiMIDIInput.Open(const AClientName: string): Boolean;
var
  R: Integer;
begin
  Result := False;
  if FOpen then Exit(True);
  if not BindAlsa then
  begin
    FLastError := ALSA_LIB + ' is not installed — there is no MIDI on this machine';
    Exit;
  end;

  // The layout is not negotiable and not guessable. If alsa-lib was built with
  // a different one, stop here and say so, rather than play noise.
  if SizeOf(TSndSeqEvent) <> SND_SEQ_EVENT_SIZE then
  begin
    FLastError := Format('snd_seq_event_t is %d bytes here, expected %d — ' +
                         'the ALSA binding does not match this system',
                         [SizeOf(TSndSeqEvent), SND_SEQ_EVENT_SIZE]);
    Exit;
  end;

  R := snd_seq_open(@FSeq, 'default', SND_SEQ_OPEN_INPUT, SND_SEQ_NONBLOCK);
  if R < 0 then
  begin
    FSeq := nil;
    FLastError := 'cannot open the sequencer: ' + AlsaErr(R);
    Exit;
  end;
  snd_seq_set_client_name(FSeq, PAnsiChar(AnsiString(AClientName)));
  FClientId := snd_seq_client_id(FSeq);

  R := snd_seq_create_simple_port(FSeq, PAnsiChar(AnsiString(AClientName + ' in')),
         SND_SEQ_PORT_CAP_WRITE or SND_SEQ_PORT_CAP_SUBS_WRITE,
         SND_SEQ_PORT_TYPE_MIDI_GENERIC or SND_SEQ_PORT_TYPE_APPLICATION);
  if R < 0 then
  begin
    FLastError := 'cannot create the port: ' + AlsaErr(R);
    snd_seq_close(FSeq);
    FSeq := nil;
    Exit;
  end;
  FPortId := R;
  FOpen := True;
  FLastError := '';
  Result := True;
end;

procedure TSedaiMIDIInput.Close;
begin
  if not FOpen then Exit;
  if FPortId >= 0 then snd_seq_delete_simple_port(FSeq, FPortId);
  if FSeq <> nil then snd_seq_close(FSeq);
  FSeq := nil;
  FOpen := False;
  FClientId := -1;
  FPortId := -1;
  FConnected := 0;
end;

function TSedaiMIDIInput.ConnectFrom(AClient, APort: Integer): Boolean;
var
  R: Integer;
begin
  Result := False;
  if not FOpen then
  begin
    FLastError := 'not open';
    Exit;
  end;
  // Never subscribe to ourselves: it is a feedback loop with a keyboard in it.
  if AClient = FClientId then Exit;
  R := snd_seq_connect_from(FSeq, FPortId, AClient, APort);
  if R < 0 then
  begin
    FLastError := Format('cannot connect from %d:%d: %s',
                         [AClient, APort, AlsaErr(R)]);
    Exit;
  end;
  Inc(FConnected);
  Result := True;
end;

function TSedaiMIDIInput.Poll: Integer;
var
  Ev: PSndSeqEvent;
  R: Integer;
  Bend: Integer;
begin
  Result := 0;
  if not FOpen then Exit;
  while snd_seq_event_input_pending(FSeq, 1) > 0 do
  begin
    Ev := nil;
    R := snd_seq_event_input(FSeq, @Ev);
    if (R < 0) or (Ev = nil) then Break;
    case Ev^.etype of
      SND_SEQ_EVENT_NOTEON:
        // Same trap as the raw wire: velocity 0 means off.
        if Ev^.data.note.velocity = 0 then
          DoNote(Ev^.data.note.channel, Ev^.data.note.note, 0, False)
        else
          DoNote(Ev^.data.note.channel, Ev^.data.note.note,
                 Ev^.data.note.velocity, True);
      SND_SEQ_EVENT_NOTEOFF:
        DoNote(Ev^.data.note.channel, Ev^.data.note.note,
               Ev^.data.note.velocity, False);
      SND_SEQ_EVENT_CONTROLLER:
        DoController(Ev^.data.control.channel,
                     Byte(Ev^.data.control.param and $7F),
                     Byte(Ev^.data.control.value and $7F));
      SND_SEQ_EVENT_PITCHBEND:
        begin
          // ALSA hands this over ALREADY CENTRED on zero, unlike the wire,
          // where it is 0..16383 around 8192. Passing it through unchanged is
          // the whole reason to read it from here rather than from raw bytes.
          Bend := Ev^.data.control.value;
          DoBend(Ev^.data.control.channel, Bend);
        end;
    end;
    Inc(Result);
  end;
end;

{$ENDIF}

// ---------------------------------------------------------------------------
{$IFDEF WINDOWS}

function TSedaiMIDIInput.Enumerate(out APorts: TSedaiMIDIPortArray): Boolean;
var
  N, I: Integer;
  Caps: TMidiInCaps;
begin
  SetLength(APorts, 0);
  Result := False;
  if not BindWinMM then
  begin
    FLastError := WINMM_LIB + ' is not available';
    Exit;
  end;
  // The () matters, and its absence does NOT fail to compile: without it this
  // names the procedure VARIABLE and the cast converts its address, which is a
  // range error with checks on and a nonsense device count with them off. Same
  // trap as snd_seq_client_info_sizeof above; found by running under Wine,
  // because compiling cannot see it.
  N := Integer(midiInGetNumDevs());
  SetLength(APorts, N);
  for I := 0 to N - 1 do
  begin
    FillChar(Caps, SizeOf(Caps), 0);
    midiInGetDevCapsA(PtrUInt(I), @Caps, SizeOf(Caps));
    APorts[I].Client := 0;
    APorts[I].Port := I;
    APorts[I].ClientName := 'MME';
    APorts[I].PortName := string(PAnsiChar(@Caps.szPname[0]));
    // Every MME input device is by definition something that can send to us.
    APorts[I].CanSend := True;
  end;
  Result := True;
end;

function TSedaiMIDIInput.Open(const AClientName: string): Boolean;
begin
  // There is nothing to open until a device is chosen: on MME the port and the
  // device are the same object, so ConnectFrom does the opening.
  Result := BindWinMM;
  if not Result then
    FLastError := WINMM_LIB + ' is not available — there is no MIDI here'
  else
  begin
    FOpen := True;
    FLastError := '';
  end;
end;

procedure TSedaiMIDIInput.Close;
begin
  if FHandle <> nil then
  begin
    midiInStop(FHandle);
    midiInReset(FHandle);
    midiInClose(FHandle);
    FHandle := nil;
  end;
  FOpen := False;
  FConnected := 0;
end;

function TSedaiMIDIInput.ConnectFrom(AClient, APort: Integer): Boolean;
var
  R: LongWord;
begin
  Result := False;
  if not FOpen then
  begin
    FLastError := 'not open';
    Exit;
  end;
  if FHandle <> nil then
  begin
    // MME has no routing layer: one handle is one device, and merging two would
    // have to be done here. Refused rather than silently ignored.
    FLastError := 'MME takes one device at a time; already connected';
    Exit;
  end;
  R := midiInOpen(@FHandle, LongWord(APort), PtrUInt(@MidiInProc),
                  PtrUInt(Pointer(Self)), CALLBACK_FUNCTION);
  if R <> 0 then
  begin
    FHandle := nil;
    FLastError := Format('midiInOpen(%d) failed with %d', [APort, R]);
    Exit;
  end;
  R := midiInStart(FHandle);
  if R <> 0 then
  begin
    midiInClose(FHandle);
    FHandle := nil;
    FLastError := Format('midiInStart failed with %d', [R]);
    Exit;
  end;
  Inc(FConnected);
  Result := True;
end;

function TSedaiMIDIInput.Poll: Integer;
var
  Msg: LongWord;
begin
  Result := 0;
  while FHead <> FTail do
  begin
    ReadBarrier;
    Msg := FRing[FTail and (SEDAI_MIDI_RING - 1)];
    ReadWriteBarrier;
    FTail := FTail + 1;
    DispatchRaw(Msg);
    Inc(Result);
  end;
end;

{$ENDIF}

// ---------------------------------------------------------------------------
// Platform-independent connect helpers, written once on top of Enumerate and
// ConnectFrom so the matching rules cannot drift between the two backends.
// ---------------------------------------------------------------------------

function TSedaiMIDIInput.ConnectByName(const ASpec: string): Integer;
var
  Ports: TSedaiMIDIPortArray;
  I, C, P, Colon: Integer;
  Spec, Needle: string;
begin
  Result := 0;
  Spec := Trim(ASpec);
  if Spec = '' then Exit;

  // "24:0" — an exact address. Tried first, because a user who typed numbers
  // means those numbers, even if they also appear inside some port's name.
  Colon := Pos(':', Spec);
  if Colon > 1 then
  begin
    C := StrToIntDef(Copy(Spec, 1, Colon - 1), -1);
    P := StrToIntDef(Copy(Spec, Colon + 1, Length(Spec)), -1);
    if (C >= 0) and (P >= 0) then
    begin
      if ConnectFrom(C, P) then Result := 1;
      Exit;
    end;
  end;

  if not Enumerate(Ports) then Exit;
  Needle := LowerCase(Spec);
  for I := 0 to High(Ports) do
    if Ports[I].CanSend and
       ((Pos(Needle, LowerCase(Ports[I].PortName)) > 0) or
        (Pos(Needle, LowerCase(Ports[I].ClientName)) > 0)) then
      if ConnectFrom(Ports[I].Client, Ports[I].Port) then Inc(Result);
end;

function TSedaiMIDIInput.ConnectAnything: Integer;
var
  Ports: TSedaiMIDIPortArray;
  I: Integer;
begin
  Result := 0;
  if not Enumerate(Ports) then Exit;
  for I := 0 to High(Ports) do
    // Skip client 0: that is the kernel's own timer and announce ports, which
    // send events that are not music and would count as traffic.
    if Ports[I].CanSend and (Ports[I].Client <> 0) then
      if ConnectFrom(Ports[I].Client, Ports[I].Port) then Inc(Result);
end;

{$IF (not defined(UNIX)) and (not defined(WINDOWS))}
// A platform with no backend still has to compile and still has to answer.
function TSedaiMIDIInput.Enumerate(out APorts: TSedaiMIDIPortArray): Boolean;
begin
  SetLength(APorts, 0);
  FLastError := 'no MIDI backend on this platform';
  Result := False;
end;

function TSedaiMIDIInput.Open(const AClientName: string): Boolean;
begin
  FLastError := 'no MIDI backend on this platform';
  Result := False;
end;

procedure TSedaiMIDIInput.Close;
begin
  FOpen := False;
end;

function TSedaiMIDIInput.ConnectFrom(AClient, APort: Integer): Boolean;
begin
  Result := False;
end;

function TSedaiMIDIInput.Poll: Integer;
begin
  Result := 0;
end;
{$ENDIF}

end.
