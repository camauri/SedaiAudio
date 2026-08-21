// ============================================================================
// SedaiRandom — a random generator small enough for every voice to own one.
//
// WHY THIS EXISTS. Until now the generators drew from FPC's global Random, and
// a global generator means every sound in the program shares one stream: draw a
// number anywhere and every plucked string, every noise band, every sample-and-
// hold downstream changes. That is not a theoretical worry. It was found by
// measurement: adding ONE FILE to library/patches changed the rendering of
// instruments.patch, because patch_fixture sorts the file names first and FPC's
// TStringList.Sort picks a RANDOM PIVOT —
//
//     stringl.inc:1741   Pivot := L + Random(R - L); // they say random is best
//
// — so the sort consumed a different number of draws and the Karplus string was
// plucked with different noise. An instrument whose sound depends on how many
// files sit in a directory cannot be regression-tested, and two voices sharing
// a stream can correlate in ways nobody chose.
//
// So: 8 bytes of state, owned by the object that uses it. No global, no lock,
// no allocation, and nothing else can reach in and disturb it.
//
// THE ALGORITHM is SplitMix64 — the mixing function Java's SplittableRandom and
// Go's runtime use to seed everything else. One add and two multiply-xorshifts,
// which is a handful of instructions; a period of 2^64; no bad seeds at all, so
// a state of zero is as good as any other and there is no "must not be 0" rule
// to forget. It is a deliberate step up from the linear congruential generator
// this codebase used before: an LCG's low bits are barely random, and audio
// code that takes a float from the whole word hears that.
//
// SEEDING. SedaiNextSeed hands out a different seed to every object that asks,
// and hands out the SAME sequence on every run. Both halves matter: different,
// or a section of strings would all be detuned identically and stop sounding
// like a section; the same every run, or nothing could be measured twice.
//
// The reproducibility this gives is "same program, same construction order" —
// which is what a renderer, a test and a fixture all have. Where an exact
// stream matters regardless of construction order, set the seed explicitly.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiRandom;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

type
  { TSedaiRandom }

  TSedaiRandom = record
  private
    FState: QWord;
  public
    // Any value is a legal seed, including zero.
    procedure Seed(AValue: QWord); inline;
    // The raw draw: 32 bits taken from the TOP of the mixed word, where the
    // quality is.
    function NextCardinal: Cardinal; inline;
    function NextFloat: Single; inline;                    // [0, 1)
    function NextBipolar: Single; inline;                  // [-1, +1)
    // 0..ALimit-1, by multiply-and-shift rather than modulo, which would bias
    // the low values whenever ALimit does not divide 2^32.
    function NextBelow(ALimit: Integer): Integer; inline;
  end;

// A distinct seed per caller, identical from run to run. Call it once, in a
// constructor — never in the audio path.
function SedaiNextSeed: QWord;

// A seed derived from a NAME rather than from the order things were built in.
//
// SedaiNextSeed is reproducible only for the same construction sequence, and
// that turned out not to be enough: adding one patch to a directory changed the
// sound of the patches rendered after it, because they were handed different
// seeds. Measured, not reasoned about — the sound fixtures caught it.
//
// A module that seeds itself from its own name depends on nothing else. Two
// patches can then be rendered in either order, or one of them not at all, and
// each still sounds the same. Same name in two patches means the same stream,
// which is right: they are the same module in the same place.
function SedaiSeedFromName(const AName: string): QWord;

// Restart the dispenser, so that building the same objects again gets the same
// seeds again and therefore the same sound. This is what a test means when it
// wants two renders to match: it is the honest replacement for setting the
// global RandSeed, which only ever worked because everything shared one stream.
// For measurement and reproduction — never in the middle of a performance,
// where it would hand a second voice the seed the first one already has.
procedure SedaiSeedSequence(AFrom: QWord = 0);

implementation

const
  // SplitMix64's golden-ratio increment and its two mixing multipliers.
  GAMMA = QWord($9E3779B97F4A7C15);
  MIX_A = QWord($BF58476D1CE4E5B9);
  MIX_B = QWord($94D049BB133111EB);

procedure TSedaiRandom.Seed(AValue: QWord);
begin
  FState := AValue;
end;

function TSedaiRandom.NextCardinal: Cardinal;
var
  Z: QWord;
begin
  FState := FState + GAMMA;
  Z := FState;
  Z := (Z xor (Z shr 30)) * MIX_A;
  Z := (Z xor (Z shr 27)) * MIX_B;
  Z := Z xor (Z shr 31);
  Result := Cardinal(Z shr 32);
end;

function TSedaiRandom.NextFloat: Single;
begin
  // 24 bits is exactly the mantissa of a Single: taking more would be thrown
  // away by the rounding, and taking the top bits keeps the best ones.
  Result := (NextCardinal shr 8) * (1.0 / 16777216.0);
end;

function TSedaiRandom.NextBipolar: Single;
begin
  Result := NextFloat * 2.0 - 1.0;
end;

function TSedaiRandom.NextBelow(ALimit: Integer): Integer;
begin
  if ALimit <= 1 then Exit(0);
  Result := Integer((QWord(NextCardinal) * QWord(ALimit)) shr 32);
end;

var
  // Interlocked because objects may be built on more than one thread; it is
  // touched once per construction and never while rendering.
  GSeedCounter: QWord = 0;

function SedaiSeedFromName(const AName: string): QWord;
var
  I: Integer;
begin
  // FNV-1a, 64-bit. Small, well spread, and it has no state — which is the
  // whole point: the answer depends on the name and on nothing else.
  Result := QWord(14695981039346656037);
  for I := 1 to Length(AName) do
  begin
    Result := Result xor QWord(Ord(AName[I]));
    Result := Result * QWord(1099511628211);
  end;
end;

procedure SedaiSeedSequence(AFrom: QWord);
begin
  GSeedCounter := AFrom;
end;

function SedaiNextSeed: QWord;
var
  N: QWord;
begin
  N := InterlockedIncrement64(GSeedCounter);
  // Run the counter through the same mixer the generator uses, so two objects
  // built one after another do not start from neighbouring states and produce
  // visibly related streams.
  Result := (N * GAMMA) xor MIX_B;
end;

end.
