// ============================================================================
// SedaiPatchVoices — a patch is a VOICE TEMPLATE; this plays several at once.
//
// The pool holds N independent graphs built from the same patch text, each with
// its own module instances and therefore its own oscillator phases, filter state
// and envelope. That is the whole reason a chord does not sound like one note
// three times as loud: nothing is shared, so nothing phase-locks.
//
// Building N graphs from the same source is deliberate rather than cloning a
// compiled one: a module carries DSP state, and a deep copy would have to know
// how to duplicate each kind. Parsing is cheap and happens once at load.
//
// Voice stealing is by age. A voice is free again once its gate is closed AND
// its output has fallen below the silence floor for a whole block — which asks
// the audio rather than the envelope, so it works for any patch, including ones
// whose tail is a delay or a resonant filter ringing rather than an ADSR.
//
// TWO WAYS IN, and the difference matters. NoteOn/NoteOff act at once and touch
// the voices directly: correct only for a caller that already owns the audio
// thread, which is what an offline renderer is. Post* puts an event in a
// lock-free queue and is what anything else must use — a MIDI port, a keyboard,
// another thread of any kind. Nothing in the audio path then waits on anything.
//
// SAMPLE-ACCURATE. Render drains the queue as it goes and SPLITS THE BLOCK at
// each event time, so a note starts on the sample it was posted for rather than
// on the next block boundary. At a 256-sample buffer that boundary is 5.8 ms of
// slop, which is audible as a chord that does not quite land together; more to
// the point, it means the same MIDI file rendered live and rendered offline are
// the same rendering, and that is what makes the live path testable at all.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchVoices;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math, SedaiPatchGraph, SedaiPatchModules, SedaiPatchFile,
  SedaiPatchEvents;

const
  SEDAI_PATCH_SILENCE = 1.0e-4;   // -80 dB: below this a released voice is done

  // The output stage. Voices sum without attenuation, so three notes of an
  // ordinary patch already pass 1.0 and what you hear is a squared wave —
  // measured on basic.patch: one note 0.477, two 0.771, three 1.162, four
  // 1.571. Hard clipping is the worst possible answer to that, so the sum is
  // shaped instead: untouched below the knee, asymptotic to 1 above it.
  SEDAI_LIMIT_KNEE = 0.70;

type
  TSedaiPatchVoice = class
  private
    FGraph: TSedaiPatchGraph;
    FNote: TSedaiModNote;
    FActive: Boolean;
    FGateOpen: Boolean;
    FMidiNote: Integer;
    FVelocity: Single;
    // The key has been let go but the sustain pedal is down, so the gate stays
    // open. Kept per voice rather than per note because the same note may be
    // sounding twice — press, hold the pedal, press again — and only the one
    // whose key was released is waiting.
    FPendingRelease: Boolean;
    FAge: QWord;
    FQuietBlocks: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Graph: TSedaiPatchGraph read FGraph;
    property Active: Boolean read FActive;
    property MidiNote: Integer read FMidiNote;
  end;

  { TSedaiPatchVoicePool }

  TSedaiPatchVoicePool = class
  private
    FVoices: array of TSedaiPatchVoice;
    FMix: array of array of Single;   // [channel][sample]
    FVoicePeak: array of Single;      // loudest sample per voice this block
    FSampleRate: Cardinal;
    FBlockSize: Integer;
    FClock: QWord;                    // counts note-ons; only used for age
    FSamplePos: QWord;                // the pool's own clock, in frames
    FQueue: TSedaiPatchEventQueue;
    FSustain: Boolean;
    FBendSemitones: Single;
    // Where every controller stands right now, kept by the pool because a
    // controller belongs to the CHANNEL and not to a note: a voice allocated
    // later has to be born with the wheel where the wheel is, not at zero.
    FCC: array[0..SEDAI_CTRL_MAX] of Single;
    FCCSeen: array[0..SEDAI_CTRL_MAX] of Boolean;
    FLastError: string;
    FWarnings: string;
    FPatchVoices: Integer;
    FForceSampleRate: Boolean;
    FMasterGain: Single;
    FLimit: Boolean;
    function FindFree: Integer;
    function FindOldest: Integer;
    procedure RetuneVoice(AIndex: Integer);
    procedure ApplyEvent(const AEvent: TSedaiPatchEvent);
    procedure ApplyDueEvents(ANow: QWord);
    procedure RenderChunk(AOffset, ACount, AChannels: Integer);
    procedure SetSustain(AOn: Boolean);
    // Push every controller that has ever arrived into one voice's graph, with
    // no smoothing. Called when a voice is allocated: it must START where the
    // player's hands are, and a wheel that slid up over the first 10 ms of
    // every note would be a fault you could hear.
    procedure SeedControllers(AIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    // Build APolyphony independent instances of the same patch file.
    function LoadFromFile(const AFilename: string; APolyphony: Integer): Boolean;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
    procedure Reset;

    // --- immediate, for a caller that IS the audio thread ---
    // MIDI note numbers; 60 = middle C, which the patch's own base frequency
    // decides the pitch of. Pitch reaches the graph as volts per octave.
    // Velocity is 0..1 and defaults to full, so every existing caller keeps the
    // behaviour it had before there was such a thing.
    procedure NoteOn(AMidiNote: Integer; AVelocity: Single = 1.0);
    procedure NoteOff(AMidiNote: Integer);
    procedure AllNotesOff;
    // Bend applies to every sounding voice and to every one that follows, the
    // way a wheel does. In semitones, signed.
    procedure PitchBend(ASemitones: Single);
    // A controller: mod wheel, breath, a pedal, aftertouch (number 128). 0..1.
    // The pool does not decide what it means — it hands it to every voice, and
    // a `cc` module inside the patch is what turns it into a signal. A patch
    // with no such module is unaffected, which is why sending everything is
    // safe.
    procedure SetControl(ANumber: Integer; AValue: Single);

    // --- queued, for ANY OTHER THREAD ---
    // These take no lock and touch no voice: they hand the event to the ring,
    // and Render picks it up at the right sample. AAt is a position on this
    // pool's own clock (see SamplePos); 0 means "as soon as you can", which is
    // what live playing means. One producer only — see SedaiPatchEvents.
    function Post(const AEvent: TSedaiPatchEvent): Boolean;
    function PostNoteOn(ANote: Integer; AVelocity: Single = 1.0;
                        AAt: QWord = 0): Boolean;
    function PostNoteOff(ANote: Integer; AAt: QWord = 0): Boolean;
    function PostAllNotesOff(AAt: QWord = 0): Boolean;
    function PostPitchBend(ASemitones: Single; AAt: QWord = 0): Boolean;
    function PostSustain(AOn: Boolean; AAt: QWord = 0): Boolean;
    function PostControl(ANumber: Integer; AValue: Single;
                         AAt: QWord = 0): Boolean;
    // Channel pressure, which has no controller number on the wire and gets one
    // here so that a patch routes it exactly like a wheel.
    function PostPressure(AValue: Single; AAt: QWord = 0): Boolean;

    // Render ACount frames of the mix. Only voices that are actually sounding
    // are walked, so an idle pool costs nothing.
    procedure Render(ACount: Integer);
    function MixSample(AChannel, AIndex: Integer): Single; inline;
    function OutputCount: Integer;
    // True when the patch takes audio from outside rather than making its own.
    // Such a patch is an EFFECT: rendering it with nothing connected measures
    // silence, which proves nothing about the effect, so callers that generate
    // fixtures or previews skip it rather than recording a zero.
    function HasAudioInput: Boolean;
    function ActiveVoices: Integer;
    // Where each output radiates FROM, on the instrument's own axis (-1..+1),
    // and how far apart in metres the outermost points really are. The patch
    // declares these; an arrangement is what turns them into a place.
    function OutputPos(AIndex: Integer): Single;
    function Extent: Single;

    function Describe: string;

    function VoiceCount: Integer;
    property LastError: string read FLastError;
    // Non-fatal things the patch said. Print them: they are the only notice you
    // get that an included file changed under you.
    property Warnings: string read FWarnings;
    // What the patch itself asked for, 0 if it did not say. Polyphony is a
    // property of the instrument, not of how it was launched.
    property PatchVoices: Integer read FPatchVoices;
    property MasterGain: Single read FMasterGain write FMasterGain;
    // On by default: an instrument that crackles when you play a chord is
    // broken, and nobody should have to know why. Switch it off to measure the
    // raw sum, or when something downstream owns the level.
    property Limit: Boolean read FLimit write FLimit;

    // Frames rendered since the last Reset. A producer that knows the sample
    // position of an event — a file player, a MIDI port with a timestamp —
    // schedules against this. It is written only by Render, so a reader in
    // another thread sees a value that is at most one block stale, which is
    // exactly the accuracy such a producer can act on anyway.
    property SamplePos: QWord read FSamplePos;
    // Events thrown away because the ring was full. Print it: a queue that
    // overflows is a performance with notes missing from it.
    function EventsDropped: Cardinal;
    function EventsPending: Integer;
    // The pedal, as the pool currently believes it to be.
    property Sustain: Boolean read FSustain;
    // What a controller stands at, as the pool believes it. Useful to a display
    // and to a test; the sound reads it through a `cc` module, not through this.
    function Control(ANumber: Integer): Single;
    property Bend: Single read FBendSemitones;
  end;

implementation

{ TSedaiPatchVoice }

constructor TSedaiPatchVoice.Create;
begin
  inherited Create;
  FGraph := TSedaiPatchGraph.Create;
  FNote := nil;
  FActive := False;
  FGateOpen := False;
  FMidiNote := -1;
  FVelocity := 1.0;
  FPendingRelease := False;
  FAge := 0;
  FQuietBlocks := 0;
end;

destructor TSedaiPatchVoice.Destroy;
begin
  FGraph.Free;
  inherited Destroy;
end;

{ TSedaiPatchVoicePool }

constructor TSedaiPatchVoicePool.Create;
begin
  inherited Create;
  SetLength(FVoices, 0);
  FSampleRate := 44100;
  FBlockSize := 256;
  FClock := 0;
  FSamplePos := 0;
  FSustain := False;
  FBendSemitones := 0.0;
  FMasterGain := 1.0;
  FLimit := True;
  FForceSampleRate := False;
  FQueue := TSedaiPatchEventQueue.Create;
  // Sized here as well as in Prepare so that posting to a pool nobody prepared
  // drops the event on the floor with a count, rather than losing it silently.
  FQueue.Allocate(SEDAI_EVENT_QUEUE_DEFAULT);
end;

destructor TSedaiPatchVoicePool.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do FVoices[I].Free;
  SetLength(FVoices, 0);
  FQueue.Free;
  inherited Destroy;
end;

function TSedaiPatchVoicePool.LoadFromFile(const AFilename: string;
  APolyphony: Integer): Boolean;
var
  I: Integer;
  Res: TSedaiPatchLoadResult;
  V: TSedaiPatchVoice;
  M: TSedaiPatchModule;
begin
  Result := False;
  FLastError := '';
  FWarnings := '';
  FPatchVoices := 0;
  if APolyphony < 1 then APolyphony := 1;

  for I := 0 to High(FVoices) do FVoices[I].Free;
  SetLength(FVoices, 0);

  // The patch may state its own polyphony, and when it does it wins: a
  // monophonic bass is monophonic by nature, not because of how it was
  // launched. The caller's number is the default for a patch that says nothing.
  I := 0;
  while I < APolyphony do
  begin
    V := TSedaiPatchVoice.Create;
    Res := LoadPatchFromFile(V.FGraph, AFilename);
    // Warnings survive: an include that has moved on since the patch was
    // written is exactly the thing nobody notices until the sound is wrong.
    if (I = 0) and (Res.Warnings <> '') then FWarnings := Res.Warnings;
    if (I = 0) and (Res.Voices > 0) then
    begin
      FPatchVoices := Res.Voices;
      APolyphony := Res.Voices;
    end;
    if not Res.Success then
    begin
      if Res.ErrorLine > 0 then
        FLastError := Format('%s:%d: %s', [AFilename, Res.ErrorLine, Res.ErrorText])
      else
        FLastError := Res.ErrorText;
      V.Free;
      Exit;
    end;
    FForceSampleRate := Res.ForceSampleRate;
    if not V.FGraph.Compile(FForceSampleRate) then
    begin
      FLastError := V.FGraph.LastError;
      V.Free;
      Exit;
    end;
    M := V.FGraph.ModuleByName('note');
    if M is TSedaiModNote then V.FNote := TSedaiModNote(M);

    SetLength(FVoices, Length(FVoices) + 1);
    FVoices[High(FVoices)] := V;
    Inc(I);
  end;

  if (Length(FVoices) > 0) and (FVoices[0].FNote = nil) then
    // Not fatal: a drone patch with no keyboard is perfectly legitimate, it just
    // cannot be played from one.
    FLastError := 'note: the patch has no module named "note", so it cannot be played by note';

  Result := True;
end;

procedure TSedaiPatchVoicePool.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
var
  I: Integer;
begin
  FSampleRate := ASampleRate;
  FBlockSize := ABlockSize;
  SetLength(FMix, OutputCount);
  for I := 0 to High(FMix) do
    if Length(FMix[I]) < ABlockSize then SetLength(FMix[I], ABlockSize);
  SetLength(FVoicePeak, Length(FVoices));
  // The ring is sized HERE and never again: growing it while a producer is
  // running would move the memory under that thread's feet, and the whole point
  // of the queue is that the audio path never allocates.
  FQueue.Allocate(SEDAI_EVENT_QUEUE_DEFAULT);
  for I := 0 to High(FVoices) do
  begin
    FVoices[I].FGraph.Prepare(ASampleRate, ABlockSize);
    FVoices[I].FGraph.ResetState;
  end;
end;

procedure TSedaiPatchVoicePool.Reset;
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
  begin
    FVoices[I].FGraph.ResetState;
    FVoices[I].FActive := False;
    FVoices[I].FGateOpen := False;
    FVoices[I].FMidiNote := -1;
    FVoices[I].FVelocity := 1.0;
    FVoices[I].FPendingRelease := False;
    FVoices[I].FQuietBlocks := 0;
  end;
  FClock := 0;
  FSamplePos := 0;
  FSustain := False;
  FBendSemitones := 0.0;
  // The wheels go back where the patch put them. A controller is performance
  // state, and a render that started from wherever the last one left off would
  // not be reproducible — which is the whole basis of the sound fixtures.
  for I := 0 to SEDAI_CTRL_MAX do
  begin
    FCC[I] := 0.0;
    FCCSeen[I] := False;
  end;
  // Anything still queued belongs to a performance that has just ended.
  FQueue.Clear;
end;

function TSedaiPatchVoicePool.FindFree: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FVoices) do
    if not FVoices[I].FActive then Exit(I);
end;

function TSedaiPatchVoicePool.FindOldest: Integer;
var
  I: Integer;
  Best: QWord;
begin
  Result := 0;
  if Length(FVoices) = 0 then Exit(-1);
  Best := FVoices[0].FAge;
  for I := 1 to High(FVoices) do
    if FVoices[I].FAge < Best then
    begin
      Best := FVoices[I].FAge;
      Result := I;
    end;
end;

// Push a voice's current note, bend, gate and velocity into its graph. Every
// place that changes any one of those goes through here, so the four can never
// drift apart — which is what happened when each caller rebuilt the pitch.
procedure TSedaiPatchVoicePool.RetuneVoice(AIndex: Integer);
var
  V: TSedaiPatchVoice;
  G: Single;
begin
  V := FVoices[AIndex];
  if (V.FNote = nil) or (V.FMidiNote < 0) then Exit;
  if V.FGateOpen then G := 1.0 else G := 0.0;
  // 60 = the patch's own base frequency; a semitone is 1/12 of a volt, and the
  // bend is just more semitones — which is the whole reason pitch is a signal
  // and not a note number.
  V.FNote.SetNote((V.FMidiNote - 60 + FBendSemitones) / 12.0, G, V.FVelocity);
end;

procedure TSedaiPatchVoicePool.NoteOn(AMidiNote: Integer; AVelocity: Single);
var
  Idx: Integer;
begin
  if Length(FVoices) = 0 then Exit;
  if (AMidiNote < 0) or (AMidiNote > 127) then Exit;
  if AVelocity < 0.0 then AVelocity := 0.0;
  if AVelocity > 1.0 then AVelocity := 1.0;
  Idx := FindFree;
  if Idx < 0 then
  begin
    Idx := FindOldest;                   // steal by age
    FVoices[Idx].FGraph.ResetState;      // a stolen voice starts clean
  end;
  Inc(FClock);
  FVoices[Idx].FActive := True;
  FVoices[Idx].FGateOpen := True;
  FVoices[Idx].FPendingRelease := False;
  FVoices[Idx].FMidiNote := AMidiNote;
  FVoices[Idx].FVelocity := AVelocity;
  FVoices[Idx].FAge := FClock;
  FVoices[Idx].FQuietBlocks := 0;
  SeedControllers(Idx);
  RetuneVoice(Idx);
end;

procedure TSedaiPatchVoicePool.NoteOff(AMidiNote: Integer);
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
    if FVoices[I].FActive and FVoices[I].FGateOpen and
       (not FVoices[I].FPendingRelease) and
       (FVoices[I].FMidiNote = AMidiNote) then
    begin
      // With the pedal down the key stops mattering: the note is remembered as
      // waiting, and the gate does not move until the foot does.
      if FSustain then
        FVoices[I].FPendingRelease := True
      else
      begin
        FVoices[I].FGateOpen := False;
        RetuneVoice(I);
      end;
    end;
end;

procedure TSedaiPatchVoicePool.AllNotesOff;
var
  I: Integer;
begin
  // A panic releases everything whatever the pedal is doing, but does NOT
  // pretend the pedal came up: it is still under a foot we cannot see.
  for I := 0 to High(FVoices) do
    if FVoices[I].FActive and FVoices[I].FGateOpen then
    begin
      FVoices[I].FGateOpen := False;
      FVoices[I].FPendingRelease := False;
      RetuneVoice(I);
    end;
end;

procedure TSedaiPatchVoicePool.PitchBend(ASemitones: Single);
var
  I: Integer;
begin
  if ASemitones = FBendSemitones then Exit;
  FBendSemitones := ASemitones;
  // Every sounding voice moves, including the ones in their release: a bend
  // that let go of the tail would sound like a fault.
  for I := 0 to High(FVoices) do
    if FVoices[I].FActive then RetuneVoice(I);
end;

procedure TSedaiPatchVoicePool.SetControl(ANumber: Integer; AValue: Single);
var
  I: Integer;
begin
  if (ANumber < 0) or (ANumber > SEDAI_CTRL_MAX) then Exit;
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 1.0 then AValue := 1.0;
  FCC[ANumber] := AValue;
  FCCSeen[ANumber] := True;
  // Every voice, not only the sounding ones: a voice in its release is still
  // making a sound the wheel is entitled to change, and an idle one costs a
  // comparison inside a module that will not match.
  for I := 0 to High(FVoices) do
    FVoices[I].FGraph.SetController(ANumber, AValue, False);
end;

procedure TSedaiPatchVoicePool.SeedControllers(AIndex: Integer);
var
  N: Integer;
begin
  for N := 0 to SEDAI_CTRL_MAX do
    if FCCSeen[N] then
      FVoices[AIndex].FGraph.SetController(N, FCC[N], True);
end;

function TSedaiPatchVoicePool.Control(ANumber: Integer): Single;
begin
  if (ANumber < 0) or (ANumber > SEDAI_CTRL_MAX) then Exit(0.0);
  Result := FCC[ANumber];
end;

procedure TSedaiPatchVoicePool.SetSustain(AOn: Boolean);
var
  I: Integer;
begin
  if AOn = FSustain then Exit;
  FSustain := AOn;
  if not AOn then
    // The foot came up: everything that was waiting for it releases now.
    for I := 0 to High(FVoices) do
      if FVoices[I].FActive and FVoices[I].FPendingRelease then
      begin
        FVoices[I].FPendingRelease := False;
        FVoices[I].FGateOpen := False;
        RetuneVoice(I);
      end;
end;

// --- the producer side. None of this touches a voice. ---

function TSedaiPatchVoicePool.Post(const AEvent: TSedaiPatchEvent): Boolean;
begin
  Result := FQueue.Post(AEvent);
end;

function TSedaiPatchVoicePool.PostNoteOn(ANote: Integer; AVelocity: Single;
  AAt: QWord): Boolean;
begin
  Result := FQueue.PostNoteOn(ANote, AVelocity, AAt);
end;

function TSedaiPatchVoicePool.PostNoteOff(ANote: Integer; AAt: QWord): Boolean;
begin
  Result := FQueue.PostNoteOff(ANote, AAt);
end;

function TSedaiPatchVoicePool.PostAllNotesOff(AAt: QWord): Boolean;
begin
  Result := FQueue.PostAllNotesOff(AAt);
end;

function TSedaiPatchVoicePool.PostPitchBend(ASemitones: Single;
  AAt: QWord): Boolean;
begin
  Result := FQueue.PostPitchBend(ASemitones, AAt);
end;

function TSedaiPatchVoicePool.PostSustain(AOn: Boolean; AAt: QWord): Boolean;
var
  V: Single;
begin
  if AOn then V := 1.0 else V := 0.0;
  Result := FQueue.PostController(SEDAI_CC_SUSTAIN, V, AAt);
end;

function TSedaiPatchVoicePool.PostControl(ANumber: Integer; AValue: Single;
  AAt: QWord): Boolean;
begin
  Result := FQueue.PostController(ANumber, AValue, AAt);
end;

function TSedaiPatchVoicePool.PostPressure(AValue: Single; AAt: QWord): Boolean;
begin
  Result := FQueue.PostController(SEDAI_CTRL_PRESSURE, AValue, AAt);
end;

function TSedaiPatchVoicePool.EventsDropped: Cardinal;
begin
  Result := FQueue.Dropped;
end;

function TSedaiPatchVoicePool.EventsPending: Integer;
begin
  Result := FQueue.Count;
end;

// --- the consumer side, inside Render ---

procedure TSedaiPatchVoicePool.ApplyEvent(const AEvent: TSedaiPatchEvent);
begin
  case AEvent.Kind of
    ekNoteOn:      NoteOn(AEvent.Note, AEvent.Value);
    ekNoteOff:     NoteOff(AEvent.Note);
    ekAllNotesOff: AllNotesOff;
    ekPitchBend:   PitchBend(AEvent.Value);
    ekController:
      begin
        // EVERY controller reaches the patch, including the three below: the
        // pool acting on the pedal does not consume it, and a patch is entitled
        // to open a filter with the same foot that holds the notes.
        SetControl(AEvent.Param, AEvent.Value);
        // These three are note logic rather than sound design, and belong to
        // the pool because no patch can implement them: voice stealing and
        // release are not things a graph can see.
        case AEvent.Param of
          SEDAI_CC_SUSTAIN:      SetSustain(AEvent.Value >= 0.5);
          SEDAI_CC_ALL_SOUND_OFF,
          SEDAI_CC_ALL_NOTES_OFF: AllNotesOff;
        end;
      end;
  end;
end;

// Pop everything that is due at ANow. An At of 0 means "at once" and is always
// due; anything already in the past is due too, because a late event is still
// better played than dropped.
procedure TSedaiPatchVoicePool.ApplyDueEvents(ANow: QWord);
var
  E: TSedaiPatchEvent;
  At: QWord;
begin
  while FQueue.PeekAt(At) do
  begin
    if (At <> 0) and (At > ANow) then Break;
    if not FQueue.Pop(E) then Break;
    ApplyEvent(E);
  end;
end;

// Render ACount frames of every sounding voice into FMix starting at AOffset,
// accumulating each voice's loudest sample of the WHOLE block in FVoicePeak —
// retirement is a per-block decision and must not be fooled by a short chunk
// that happens to fall in a zero crossing.
procedure TSedaiPatchVoicePool.RenderChunk(AOffset, ACount, AChannels: Integer);
var
  I, K, C: Integer;
  V: TSedaiPatchVoice;
  S: Single;
begin
  for I := 0 to High(FVoices) do
  begin
    V := FVoices[I];
    if not V.FActive then Continue;      // an idle voice costs nothing

    V.FGraph.Render(ACount);
    for C := 0 to AChannels - 1 do
      for K := 0 to ACount - 1 do
      begin
        S := V.FGraph.OutputSample(C, K);
        if Abs(S) > FVoicePeak[I] then FVoicePeak[I] := Abs(S);
        FMix[C][AOffset + K] := FMix[C][AOffset + K] + S;
      end;
  end;
end;

procedure TSedaiPatchVoicePool.Render(ACount: Integer);
var
  I, K, C, NCh, Ofs, N: Integer;
  V: TSedaiPatchVoice;
  M, G: Single;
  NextAt, Now_: QWord;
begin
  NCh := OutputCount;
  if Length(FMix) < NCh then SetLength(FMix, NCh);
  for C := 0 to NCh - 1 do
  begin
    if Length(FMix[C]) < ACount then SetLength(FMix[C], ACount);
    for K := 0 to ACount - 1 do FMix[C][K] := 0.0;
  end;
  if Length(FVoicePeak) < Length(FVoices) then
    SetLength(FVoicePeak, Length(FVoices));
  for I := 0 to High(FVoices) do FVoicePeak[I] := 0.0;

  // Walk the block in chunks that end wherever something has to happen. With
  // nothing queued this is one chunk and costs exactly what it used to.
  Ofs := 0;
  while Ofs < ACount do
  begin
    Now_ := FSamplePos + QWord(Ofs);
    ApplyDueEvents(Now_);

    N := ACount - Ofs;
    // Stop short at the next event, if it lands inside what is left. Anything
    // due now has already been applied, so this can only be a future one.
    if FQueue.PeekAt(NextAt) then
      if (NextAt > Now_) and (NextAt < FSamplePos + QWord(ACount)) then
        N := Integer(NextAt - Now_);
    if N < 1 then N := 1;                // never fail to advance

    RenderChunk(Ofs, N, NCh);
    Inc(Ofs, N);
  end;
  Inc(FSamplePos, QWord(ACount));

  for I := 0 to High(FVoices) do
  begin
    V := FVoices[I];
    if not V.FActive then Continue;
    // Retire the voice when the gate is shut and the sound has actually gone,
    // measured on the output rather than guessed from the envelope.
    if (not V.FGateOpen) and (FVoicePeak[I] < SEDAI_PATCH_SILENCE) then
    begin
      Inc(V.FQuietBlocks);
      if V.FQuietBlocks >= 2 then
      begin
        V.FActive := False;
        V.FMidiNote := -1;
        V.FPendingRelease := False;
        V.FGraph.ResetState;
      end;
    end
    else
      V.FQuietBlocks := 0;
  end;

  if FMasterGain <> 1.0 then
    for C := 0 to NCh - 1 do
      for K := 0 to ACount - 1 do FMix[C][K] := FMix[C][K] * FMasterGain;

  if FLimit then
    for K := 0 to ACount - 1 do
    begin
      // One reduction per sample, computed on the loudest channel and applied
      // to all of them. Limiting each channel on its own would move the image
      // sideways whenever the level rose, which is worse than the clipping.
      M := 0.0;
      for C := 0 to NCh - 1 do
        if Abs(FMix[C][K]) > M then M := Abs(FMix[C][K]);
      if M > SEDAI_LIMIT_KNEE then
      begin
        // Linear below the knee, tanh above, and the two meet with the same
        // slope — so nothing is coloured until it would have clipped, and
        // below the knee the signal is bit-identical to the unlimited sum.
        G := (SEDAI_LIMIT_KNEE + (1.0 - SEDAI_LIMIT_KNEE) *
              Tanh((M - SEDAI_LIMIT_KNEE) / (1.0 - SEDAI_LIMIT_KNEE))) / M;
        for C := 0 to NCh - 1 do FMix[C][K] := FMix[C][K] * G;
      end;
    end;
end;

function TSedaiPatchVoicePool.MixSample(AChannel, AIndex: Integer): Single;
begin
  if (AChannel >= 0) and (AChannel < Length(FMix)) and
     (AIndex >= 0) and (AIndex < Length(FMix[AChannel])) then
    Result := FMix[AChannel][AIndex]
  else
    Result := 0.0;
end;

// Every voice is the same patch, so they all declare the same channel count.
function TSedaiPatchVoicePool.HasAudioInput: Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(FVoices) = 0 then Exit;
  for I := 0 to FVoices[0].FGraph.ModuleCount - 1 do
    if SameText(FVoices[0].FGraph.ModuleAt(I).TypeName, 'input') then Exit(True);
end;

function TSedaiPatchVoicePool.OutputCount: Integer;
begin
  if Length(FVoices) = 0 then Result := 1
  else Result := FVoices[0].FGraph.OutputCount;
end;

function TSedaiPatchVoicePool.OutputPos(AIndex: Integer): Single;
begin
  if Length(FVoices) = 0 then Result := 0.0
  else Result := FVoices[0].FGraph.OutputPos(AIndex);
end;

function TSedaiPatchVoicePool.Extent: Single;
begin
  if Length(FVoices) = 0 then Result := 0.0
  else Result := FVoices[0].FGraph.Extent;
end;

function TSedaiPatchVoicePool.VoiceCount: Integer;
begin
  Result := Length(FVoices);
end;

function TSedaiPatchVoicePool.ActiveVoices: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FVoices) do
    if FVoices[I].FActive then Inc(Result);
end;

function TSedaiPatchVoicePool.Describe: string;
begin
  if Length(FVoices) = 0 then Exit('  (no voices)');
  Result := Format('  %d voices, each an independent instance of the patch'#10,
                   [Length(FVoices)]) + FVoices[0].FGraph.Describe;
end;

end.
