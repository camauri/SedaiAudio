// ============================================================================
// SedaiPatchEvents — the note events, and the ring that carries them across
// the thread boundary WITHOUT A LOCK.
//
// The problem this exists to solve: a note comes from somewhere that is not the
// audio thread — a MIDI port, a keyboard, a file player — and has to reach the
// voice pool, which lives inside the audio callback. The obvious answer is a
// mutex around the pool, and that is what the workbench did first. It works
// until it doesn't: the audio callback must never wait for anything, and a
// mutex held by a thread the scheduler has just descheduled is exactly a wait.
// The result is not a wrong note, it is a gap in the sound.
//
// So: a single-producer / single-consumer ring buffer, sized once and never
// again. One thread writes, one thread reads, and neither ever blocks. Nothing
// is allocated after Allocate, because an allocation in the audio path can take
// a lock inside the memory manager and lose the same way.
//
//   * The indices are FREE-RUNNING counters, masked only when used to address
//     the array. Unsigned subtraction wraps correctly, so Head - Tail is the
//     occupancy even across the 2^32 boundary, and full and empty are told
//     apart without wasting a slot or keeping a third variable.
//
//   * Barriers, not atomics. On the only ordering x86 does not give for free —
//     store-then-load — this queue does not depend on anything. What it does
//     need is that the COMPILER not hoist the slot write past the index write,
//     and that is what WriteBarrier/ReadBarrier are for. Aligned 32-bit loads
//     and stores are already indivisible on every CPU SAF targets.
//
//   * OVERFLOW IS COUNTED, NEVER SILENT. A dropped note is a wrong performance,
//     and the one thing worse than dropping it is not knowing. Dropped is there
//     to be printed.
//
// SAMPLE POSITION. An event carries At, an absolute position on the pool's own
// sample clock, and that is what makes a chord land as a chord: the consumer
// splits the block at the event and the note starts on the sample it was meant
// to. At = 0 means "as soon as you can", which is what a finger on a key wants.
//
// The scope of At is deliberately small: it positions an event INSIDE the block
// about to be rendered, so that a live path can be as accurate as an offline
// one. It is not a sequencer. This queue is a FIFO and pops in the order things
// were posted, so a producer that posts a far-future event puts it in front of
// everything behind it — head-of-line blocking, bounded by however far ahead
// that producer schedules. Keep it inside a block or two and there is nothing
// to think about; build a song out of it and there is.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchEvents;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  // What a producer can say. Deliberately the vocabulary of a performance and
  // not of the MIDI wire: a bend arrives here already in semitones, a
  // controller already in 0..1, so the pool never has to know that 8192 is the
  // middle of anything. Translating the wire is the input driver's job.
  TSedaiEventKind = (
    ekNoteOn,        // Note, Value = velocity 0..1
    ekNoteOff,       // Note
    ekAllNotesOff,   // panic / release everything
    ekController,    // Param = controller number, Value = 0..1
    ekPitchBend      // Value = semitones, signed
  );

  TSedaiPatchEvent = record
    Kind: TSedaiEventKind;
    Note: Byte;        // MIDI note number, 60 = the patch's own base frequency
    Channel: Byte;     // kept so a later multitimbral pool can filter on it
    Param: Word;       // controller number for ekController
    Value: Single;     // velocity, controller value, or bend in semitones
    // Absolute position on the consumer's sample clock. 0 = at once, which is
    // what live playing means: there is no meaningful timestamp for "now".
    At: QWord;
  end;

  { TSedaiPatchEventQueue }
  // One producer, one consumer. Two producers on the same queue is not a race
  // that shows up in testing and then bites later — it is simply broken, and
  // there is no assertion that can catch it cheaply. Give each producer its own.

  TSedaiPatchEventQueue = class
  private
    FRing: array of TSedaiPatchEvent;
    FMask: Cardinal;       // Capacity - 1; 0 means never allocated
    FHead: Cardinal;       // written by the PRODUCER only
    FTail: Cardinal;       // written by the CONSUMER only
    FDropped: Cardinal;    // written by the producer, read by anyone
  public
    constructor Create;

    // Size the ring. Call before either side starts — from Prepare, not from
    // anywhere near a running audio callback: this reallocates, and the whole
    // point of the class is that nothing else ever does.
    procedure Allocate(ACapacity: Integer);
    // Throw away everything pending. Only safe with the producer stopped.
    procedure Clear;

    // --- producer side ---
    function Post(const AEvent: TSedaiPatchEvent): Boolean;
    function PostNoteOn(ANote: Integer; AVelocity: Single = 1.0;
                        AAt: QWord = 0): Boolean;
    function PostNoteOff(ANote: Integer; AAt: QWord = 0): Boolean;
    function PostAllNotesOff(AAt: QWord = 0): Boolean;
    function PostController(AParam: Integer; AValue: Single;
                            AAt: QWord = 0): Boolean;
    function PostPitchBend(ASemitones: Single; AAt: QWord = 0): Boolean;

    // --- consumer side ---
    // The time of the next event without removing it, so the consumer can work
    // out how far it may render before something has to happen.
    function PeekAt(out AAt: QWord): Boolean;
    function Pop(out AEvent: TSedaiPatchEvent): Boolean;

    function Count: Integer;
    function Capacity: Integer;
    // How many events were thrown away because the ring was full. Print it.
    property Dropped: Cardinal read FDropped;
  end;

const
  // The three controller numbers that are note logic rather than sound design,
  // and so belong to the voice pool rather than to a patch. They still reach
  // the patch as signals like every other controller — the pool acting on them
  // does not consume them.
  SEDAI_CC_SUSTAIN        = 64;
  SEDAI_CC_ALL_SOUND_OFF  = 120;
  SEDAI_CC_ALL_NOTES_OFF  = 123;

  // The expressive ones, named because a patch that says `cc num=breath` is
  // readable and one that says `cc num=2` is a lookup.
  SEDAI_CC_MOD            = 1;
  SEDAI_CC_BREATH         = 2;
  SEDAI_CC_FOOT           = 4;
  SEDAI_CC_EXPRESSION     = 11;

  // Channel pressure is NOT a controller on the wire — it has its own status
  // byte and no number. Given one here anyway, above the 127 the wire can
  // reach, so that everything a player can lean on arrives through one door and
  // a patch routes it the same way. Param is a Word; there is room.
  SEDAI_CTRL_PRESSURE     = 128;
  SEDAI_CTRL_MAX          = 128;

  // 1024 events is about eight seconds of very busy playing, and costs 16 KB.
  // The size that matters is not the average but the worst gap between two
  // drains, and a block is milliseconds: this is enormous on purpose, because
  // the whole cost is memory that is never touched.
  SEDAI_EVENT_QUEUE_DEFAULT = 1024;

implementation

constructor TSedaiPatchEventQueue.Create;
begin
  inherited Create;
  FMask := 0;
  FHead := 0;
  FTail := 0;
  FDropped := 0;
end;

procedure TSedaiPatchEventQueue.Allocate(ACapacity: Integer);
var
  N: Cardinal;
begin
  if ACapacity < 16 then ACapacity := 16;
  // Round up to a power of two: the masking is what makes the free-running
  // counters legal, and it is only legal on a power of two.
  N := 16;
  while (N < Cardinal(ACapacity)) and (N < $40000000) do N := N shl 1;
  SetLength(FRing, N);
  FMask := N - 1;
  FHead := 0;
  FTail := 0;
  FDropped := 0;
end;

procedure TSedaiPatchEventQueue.Clear;
begin
  FTail := FHead;
end;

function TSedaiPatchEventQueue.Post(const AEvent: TSedaiPatchEvent): Boolean;
begin
  if FMask = 0 then Exit(False);              // never allocated: say so
  // Occupancy by unsigned subtraction. Capacity is FMask + 1, so the ring is
  // full when the difference has reached it — i.e. when it exceeds FMask.
  if FHead - FTail > FMask then
  begin
    Inc(FDropped);
    Exit(False);
  end;
  FRing[FHead and FMask] := AEvent;
  // Publish the slot before publishing its existence. Without this the
  // compiler is free to make the index visible first, and the consumer would
  // read a slot that has not been written yet.
  WriteBarrier;
  FHead := FHead + 1;
  Result := True;
end;

function TSedaiPatchEventQueue.PostNoteOn(ANote: Integer; AVelocity: Single;
  AAt: QWord): Boolean;
var
  E: TSedaiPatchEvent;
begin
  if (ANote < 0) or (ANote > 127) then Exit(False);
  FillChar(E, SizeOf(E), 0);
  E.Kind := ekNoteOn;
  E.Note := ANote;
  // A velocity of zero is a note-off on the wire, but by the time it gets here
  // the driver has already resolved that. Clamping instead of rejecting keeps a
  // badly scaled producer audible rather than silent.
  if AVelocity < 0.0 then AVelocity := 0.0;
  if AVelocity > 1.0 then AVelocity := 1.0;
  E.Value := AVelocity;
  E.At := AAt;
  Result := Post(E);
end;

function TSedaiPatchEventQueue.PostNoteOff(ANote: Integer; AAt: QWord): Boolean;
var
  E: TSedaiPatchEvent;
begin
  if (ANote < 0) or (ANote > 127) then Exit(False);
  FillChar(E, SizeOf(E), 0);
  E.Kind := ekNoteOff;
  E.Note := ANote;
  E.At := AAt;
  Result := Post(E);
end;

function TSedaiPatchEventQueue.PostAllNotesOff(AAt: QWord): Boolean;
var
  E: TSedaiPatchEvent;
begin
  FillChar(E, SizeOf(E), 0);
  E.Kind := ekAllNotesOff;
  E.At := AAt;
  Result := Post(E);
end;

function TSedaiPatchEventQueue.PostController(AParam: Integer; AValue: Single;
  AAt: QWord): Boolean;
var
  E: TSedaiPatchEvent;
begin
  if (AParam < 0) or (AParam > 65535) then Exit(False);
  FillChar(E, SizeOf(E), 0);
  E.Kind := ekController;
  E.Param := AParam;
  E.Value := AValue;
  E.At := AAt;
  Result := Post(E);
end;

function TSedaiPatchEventQueue.PostPitchBend(ASemitones: Single;
  AAt: QWord): Boolean;
var
  E: TSedaiPatchEvent;
begin
  FillChar(E, SizeOf(E), 0);
  E.Kind := ekPitchBend;
  E.Value := ASemitones;
  E.At := AAt;
  Result := Post(E);
end;

function TSedaiPatchEventQueue.PeekAt(out AAt: QWord): Boolean;
begin
  AAt := 0;
  if (FMask = 0) or (FHead = FTail) then Exit(False);
  // Read the index before the slot it points at.
  ReadBarrier;
  AAt := FRing[FTail and FMask].At;
  Result := True;
end;

function TSedaiPatchEventQueue.Pop(out AEvent: TSedaiPatchEvent): Boolean;
begin
  if (FMask = 0) or (FHead = FTail) then
  begin
    FillChar(AEvent, SizeOf(AEvent), 0);
    Exit(False);
  end;
  ReadBarrier;
  AEvent := FRing[FTail and FMask];
  // Release the slot only once it has actually been copied out, or the producer
  // may overwrite it from under us.
  ReadWriteBarrier;
  FTail := FTail + 1;
  Result := True;
end;

function TSedaiPatchEventQueue.Count: Integer;
begin
  if FMask = 0 then Exit(0);
  Result := Integer(FHead - FTail);
end;

function TSedaiPatchEventQueue.Capacity: Integer;
begin
  Result := Length(FRing);
end;

end.
