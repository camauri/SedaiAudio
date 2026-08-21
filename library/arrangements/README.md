# Arrangements

A `.patch` says what an instrument **is**. It deliberately does not say where it
stands, how loud it is against the others, or what format the result has — an
instrument that had already decided it was "on the left" could never be turned
around. Those are the arrangement's business, and this is where they are said.

    bin/<plat>/arr_render trio.arr song.mid out.wav
    bin/<plat>/arr_render trio.arr                  # just describe it

Same shape as a patch, for the same reasons: it is text, so it diffs and
versions like source.

## The grammar

Five statements. Anything after `#` is a comment.

    rate    <hz>
    listen  at=<x>,<y>,<z> [facing=<x>,<y>,<z>]
    part    <name> = <file.patch> [channel=<1..16>] [voices=<n>]
    place   <name> at=<x>,<y>,<z> [facing=<x>,<y>,<z>] [extent=<metres>]
    gain    <name> = <value>

Coordinates are metres, with **−Z in front of the listener** — the OpenAL
convention the spatial layer already used. `channel=` is the MIDI channel that
drives that part, written 1..16 the way a person writes it.

An instrument nobody aimed **looks at the listener**, because that is what a
player does and it beats leaving one staring at a wall.

## More than one radiating point

A patch may declare several `output` lines with `pos=` along its own axis and an
`extent=` in metres — a handpan is one object whose tone fields are spread over
a shell a foot wide. Those points are placed *around* the instrument's position,
on the axis it faces across, so moving the instrument moves them together and
turning it turns them. The arrangement's `extent=` overrides what the patch
declared; say nothing and the patch's own value stands. One output is a point
source, which is every electronic sound.

## Traps

**A space after `=` splits the word in two.** `at= 1,0,2` is two words and the
value is lost. Lining columns up is the natural thing to do and this is what it
costs, so the error message says so by name. Same trap as `amount= 0.5` in a
patch.

**A MIDI channel no part claims is counted, not dropped.** `arr_render` prints
how many events went nowhere — it is the usual reason half an arrangement is
missing.

**Distance attenuates.** A part four metres away is quieter than one at the
reference distance, which is the point, but it means the peak of a rendered
arrangement is well below what a single `patch_play` gives. Level staging is
yours: `gain` per part.

## How it is tested

`saf_regression` builds a one-instrument arrangement, places it left, and
requires that **mirroring every position swaps the two channels exactly** — not
approximately. That single invariant catches an asymmetric panning law, a sign
error in the right-hand axis, and a distance model that treats the two sides
differently, none of which are audible on one listen.
