# Example patches

Text patches for the SAF Patch Workbench. A patch is a table of modules, values
and connections — the ARP 2500 / EMS VCS3 matrix written as rows instead of pins,
which is why it diffs and versions like source.

Render one to a WAV, monophonically:

    ./build.sh --source job/tools/saf/patch_render.lpr --dest bin/<plat>/patch_render
    bin/<plat>/patch_render library/patches/basic.patch out.wav 2 0 1.2
                            ^patch                     ^wav   ^s ^semitones ^gate

Or play notes through it polyphonically:

    ./build.sh --source job/tools/saf/patch_play.lpr --dest bin/<plat>/patch_play
    bin/<plat>/patch_play library/patches/poly.patch out.wav 3 8 60,64,67 1.5
                          ^patch                     ^wav   ^s ^voices ^notes ^gate

Or keep the patch playing while you edit it — this is the ear loop the workbench
exists for:

    ./build.sh --source job/tools/saf/patch_live.lpr --dest bin/<plat>/patch_live
    bin/<plat>/patch_live library/patches/poly.patch 8 128 48000
                          ^patch                     ^voices ^buffer ^rate

Edit the file in any editor, save, and the sound changes without a restart. A
save that does not compile prints the error and leaves the previous version
playing, so a typo never drops you into silence wondering what happened.

Keys: `z x c v b n m` are the white notes and `s d g h j` the black ones, one
octave from C; `,` and `.` change octave, SPACE releases everything, `R` forces a
reload, `I` prints the compiled stages, `Q` quits.

It opens a small **SDL2 window** — give it focus and play there. A terminal
reports key-down and never key-up, so a note could be struck but never held, and
you cannot judge a pad you cannot hold. With no display it falls back to the
terminal, where a note releases itself after a fixed time.

Buffer and rate are worth setting. **Match the rate to your audio server** or it
resamples, and the resampler costs latency: `pw-metadata -n settings | grep
clock.rate` tells you. The buffer is the other half — measured key-to-sound, 1024
samples is about 59 ms and unplayable, 128 is about 8 ms and inside what an
acoustic player already lives with.

One thing no software can fix: a PC keyboard is a matrix without diodes and stops
reporting past three or four simultaneous keys, whichever they are. Measured here
on two different keyboards. Chords need MIDI.

## Playing it from MIDI

    bin/<plat>/patch_live library/patches/poly.patch 8 128 48000 --midi
    bin/<plat>/patch_live library/patches/poly.patch --midi=24:0     # one port
    bin/<plat>/patch_live --list-midi                                # what exists

`--midi` connects to everything that can send; `--midi=<spec>` takes either an
address like `24:0` or any part of a port's name. Velocity arrives as
`note.vel`, the sustain pedal (CC 64) holds notes past the key, and the pitch
wheel bends every sounding voice — `--bend=<semitones>` sets how far, default 2,
because the wire says how far the wheel moved and never what that means.

**With no keyboard plugged in, this is still testable**, and it was tested that
way: `alsa-utils` is a sequencer somebody else wrote, so none of it is our code
checking our own work.

    bin/<plat>/midi_probe --list                       every port on the machine
    bin/<plat>/midi_probe 14:0 30                      watch what arrives
    bin/<plat>/midi_probe 14:0 45 --quiet \
        --patch=library/patches/basic.patch --wav=out.wav    ...and play it, to a file

In another terminal, `aplaymidi -p 14:0 song.mid` sends a real file through the
kernel sequencer to that port. The `--patch` form renders in step with the wall
clock and writes what the audio device would have played, so the live path can
be **measured** and not only listened to: the same file through `patch_midi`
offline and through the wire live must give the same note counts.

The one thing this rig cannot reach is the USB driver layer under a physical
keyboard. Everything from the sequencer port inward is exercised for real.

Or play a MIDI file through the patch:

    ./build.sh --source job/tools/saf/patch_midi.lpr --dest bin/<plat>/patch_midi
    bin/<plat>/patch_midi song.mid library/patches/poly.patch out.wav 16 0 0 0.35
                          ^mid     ^patch                    ^wav  ^voices ^s ^transpose ^gain

Voices sum, so a chord is louder than a single note and the gain is yours to
set — a dynamic normaliser would pump. `seconds = 0` renders the whole file plus
two seconds for the tails to ring out.

Notes are MIDI numbers, and 60 sounds the patch's own base frequency. Give them
onsets to arpeggiate: `60:0,64:0.3,67:0.6`. A patch is a VOICE TEMPLATE — the
pool builds N independent instances of it, so each note has its own oscillator
phase, filter state and envelope, and when the notes outnumber the voices the
oldest is stolen.

## The syntax, in full

Seven statements, one per line. Anything after `#` is a comment, and blank lines
are ignored. Order matters only in that a module must be declared before it is
named.

    include "<file>" as <prefix> [hash=<checksum>]
    voices  <n>
    mode    sample                     # optional; forces every stage sample-by-sample
    module  <name> = <type> [key=value ...]
    set     <module>.<port> = <value>
    connect <module>.<port> -> <module>.<port> [amount=<value>] [normalled]
    output  <module>.<port> [pos=<-1..+1>] [extent=<metres>]

**`module`** names an instance and its type. Any `key=value` after the type does
one of two things, in this order: the module gets first refusal (that is how
`osc shape=saw`, `inst instrument="Drawbar Organ"` and `sconv ir=body.wav` work,
since none of those is a number), and anything it does not claim is used to set
an input port of the same name. So `module gl = glide time=0.012` needs no `set`
line. A value containing spaces must be double-quoted.

**`set`** writes a port's constant — the knob under the modulation. It does not
disconnect anything: a port holds this value *plus* whatever is patched into it.

**`connect`** adds a source to an input. Several connections into the same input
**sum**, which is why there is no mixer module. `amount=` scales that one source
and **may be negative**, which is the attenuverter of a modular system. A
connection marked `normalled` is a default that yields: it disappears the moment
anything else is patched into that input.

**`output`** declares a channel. One line is mono, two are stereo, eight are 7.1
— the channel count belongs to the patch, and the ports stay one signal each.

An output is a point the instrument **radiates from**, not a format, so it may
carry where that point sits: `pos=` on the instrument's own axis, −1 to +1, and
`extent=` how far apart in metres the outermost points really are. A handpan is
one object whose tone fields are spread across a shell a foot wide, and saying
so lets an arrangement narrow that width as the instrument moves away, the way a
real object's does. Say nothing and it is a point source, which is every
electronic sound.

Note the coordinates are the **instrument's**, never the listener's: "one end of
the shell", not "left". Left is the arrangement's word, and an instrument that
had already decided it could never be turned around.

**`voices`** is how many notes the instrument can sound at once. **The patch
wins**: a monophonic bass is monophonic by nature, not because of how it was
launched, so a `voices` line overrides whatever a tool was told on its command
line. Say nothing and the tool's own default stands.

**`include`** brings in another patch file under a prefix. Its modules become
`<prefix>.<name>`, so two included files may both call something `osc1` without
colliding, and a name tells you which file it came from:

    include "parts/moog_core.patch" as core
    connect core.f2.out -> amp.in

Paths are relative to the **including file**, not to where you ran the tool.
Nesting is allowed to eight deep, which is also how a cycle is caught.

An included file may not declare `output` or `voices`: those belong to the
finished instrument, not to a part of it, and a file that declared them would be
an instrument rather than a component.

The optional `hash=` is a checksum of the included file as it was when you wrote
the line. When it no longer matches you get a warning naming both values — the
included file has moved on, and it may or may not still sound the same. Nobody
but you can decide that, so it is said and not enforced. The tools print it; the
`found` value is the one to paste back in once you have listened.

Nothing here computes. There are no variables, no arithmetic, no conditions —
the moment you want to give a value a name you are programming, and for that
there is SedaiBasic. A patch stays a table of what exists and how it is wired,
which is what keeps it diffable and what will let a GUI be a view of it rather
than a second source of truth.

### Values

Plain numbers, written with a **dot** whatever the machine's locale says.
Suffixes are accepted and converted:

| written | means |
|---|---|
| `440` `440Hz` | 440 |
| `120ms` | 0.12 |
| `1.5s` | 1.5 |
| `35%` | 0.35 |

⚠️ `dB` is accepted as a suffix and **stripped without converting** — `-6dB`
gives you −6, not 0.5. Write the linear value until that is fixed.

### Ports

`<module>.<port>`, and the port names are the module's own: `in`, `out`, `pitch`,
`gate`, `cutoff`, `gain`, and so on. A patch that names a port a module does not
have fails to compile and says which module and which port, rather than ignoring
the line.

### Where the shape comes from

A patch is a **table**: modules, values, connections. That is the ARP 2500 and
EMS VCS3 matrix written as rows instead of pins — the same information, and it
diffs and versions like source because a table is a file. The mechanical
matrices had a real drawback, crosstalk between adjacent buses; in software that
drawback does not exist, so the metaphor is taken without its price.

| Patch | What it shows |
|---|---|
| `basic.patch` | The classic subtractive voice: oscillator → filter → amplifier, envelope on the gain, keyboard on pitch. Every stage is acyclic, so the whole graph runs at block rate. |
| `vibrato.patch` | The same voice with an LFO patched into pitch. Raise `lfo1 rate` into the audio range and it becomes FM without changing a single connection — that is the point of having one signal type. |
| `feedback.patch` | A real loop: the filter's output returns to its own input through a VCA. The graph detects the cycle, isolates those two modules, and runs only them one sample at a time. Everything else stays at block rate. |
| `poly.patch` | Meant to be played. Its keyboard connections are `normalled`, so they are there until you patch something into those inputs yourself. Render it with `patch_play`. |
| `sequence.patch` | A sequencer driving everything, no keyboard at all — the music is made by the clock. A square LFO IS the clock; no dedicated module was needed. |
| `burble.patch` | Noise into a sample-and-hold, clocked, driving pitch. No notes written anywhere: the melody is random but stepped. |
| `ringmod.patch` | Two sines multiplied. The spectrum comes out at the sum and the difference and nowhere else — measured at 97 and 537 Hz from 220 and 317, with nothing left at either original. |
| `lead.patch` | A subtractive lead with some bite. Two detuned oscillators plus a sub an octave down, and — the thing that actually matters — a SECOND envelope on the filter cutoff. Measured, its spectral centroid sweeps 4.1x from attack to sustain, where `poly.patch` sits at 1.0x and sounds static because of it. |
| `fx_chain.patch` | An EFFECT patch: its source is a module of type `input`, so the incoming audio takes the oscillator's place. Same graph, same scheduler, same modules — no second architecture. Run it with `patch_fx`. |
| `fx.patch` | The bridge at work: a native lead running into SAF's own distortion, chorus and reverb — units that had been written long before and that no patch could reach. |
| `echo.patch` | A loop through a 120 ms delay line. The graph works out that the shortest cycle carries 5293 samples of delay and advances the loop in chunks of 5293 rather than one at a time - bit-identical output, 42% faster. |

Format:

    mode    = block | sample          # sample forces every stage per-sample
    module  <name> = <type> [key=value ...]
    set     <module>.<port> = <value>
    connect <module>.<out> -> <module>.<in> [amount=x] [normalled]
    output  <module>.<port>

Values take unit suffixes: `440Hz`, `2ms`, `120ms`, `50%`, or a plain number.
Pitch inputs are volts-per-octave: `1.0` is one octave, so a constant offset is a
transposition and any modulator is automatically musical.

Or run an existing audio file through a patch:

    ./build.sh --source job/tools/saf/patch_fx.lpr --dest bin/<plat>/patch_fx
    bin/<plat>/patch_fx in.wav library/patches/fx_chain.patch out.wav 3 0.9
                        ^in    ^patch                         ^out  ^tail ^gain

The patch needs a module of type `input`; its output is the incoming audio.
Reads whatever SedaiAudioFileReader handles and sums multi-channel input to
mono. The tail argument keeps rendering after the input ends so reverb and delay
tails ring out instead of being chopped.

Core module types: `osc`, `filter`, `amp`, `env`, `lfo`, `delay`, `input`, `note`.

For electronic sound design: `seq` (clock-driven step sequencer), `sh`
(sample and hold), `ring` (multiplier — ring modulator with two audio signals,
plain VCA with a control signal on one side), `glide` (portamento), `noise`
(white, pink, brown, blue, violet, from SAF's own generator).

Three things are deliberately NOT modules, because the port model already gives
them: an **attenuverter** is `amount=` on a connection, and it may be negative;
a **CV mixer** is several connections into one input, which sum, with the
input's own value as the offset; and **FM** is one oscillator patched into
another's pitch — exponential FM, which is what the analogue instruments of that
period actually did. Fewer module types with more inputs, as Serge argued.

Any `key=value` at a module declaration also sets an input port of the same
name, so `module gl = glide time=0.012` needs no separate `set` line.

Bridged from SAF's existing units, all prefixed `s`: `sdelay`, `schorus`,
`sflanger`, `sphaser`, `sreverb`, `scomp`, `slimiter`, `sdist`, `sautospace`,
`sbody`, `sconv`, `seq3`.

`seq3` is the parametric EQ — named for its shape because `seq` was already the
step sequencer, and an EQ mistakable for a sequencer in a patch file is worse
than an awkward name. Bands are addressed by index: `b0type=peaking b0freq=800
b0gain=-4 b0q=1.4`, up to eight, plus `gain` for the output and `bNoff` to
disable one. A band turns on with the first key that names it.

`sconv` takes `ir=<path>` (or `irraw=` to skip normalisation) and loads the
impulse response through the SAF reader — `job/ir/` has measured violin, guitar
and sax bodies. Without an IR it is a pass-through. Each takes a
`mix` input for dry/wet, provided by the bridge itself so a patch does not have
to know which units have one of their own. They are block-oriented, so they
declare `supports=block` and the graph refuses to put them inside a feedback
cycle.

A connection marked `normalled` is a default that yields: it is dropped the
moment anything else is patched into the same input. That is what a semi-modular
does with its internal wiring, and it is why an instrument makes a sound before
you have patched anything — every patch is then a delta from something that
already works.

Every module also understands `supports=block`, which declares that it must not
be advanced one sample at a time. Put such a module inside a feedback cycle and
the patch refuses to compile, naming the module and the cycle — the expensive
block-of-one path is made unreachable rather than made cheaper.

Oscillators are band-limited (PolyBLEP on the discontinuities, and the triangle
is built by integrating the corrected square), which puts residual aliasing 17
to 18 dB below the naive shapes.

## Traps

Every one of these cost real time to find. They are here so they cost nobody
else any.

**`note.vel` is how hard the key was pressed**, and it is a signal like any
other rather than a taller gate. That separation is the point: on almost every
acoustic instrument playing harder makes the sound *brighter* as much as louder,
so velocity usually wants to reach a filter cutoff at least as much as an
amplifier — and with one connection each it can reach both.

    connect note.gate -> env1.gate
    connect env1.out  -> amp.gain    amount=0.3
    connect note.vel  -> amp.gain    amount=0.2     # louder
    connect note.vel  -> filt.cutoff amount=0.5     # and brighter

It reads **1.0 until something says otherwise**, so a patch that ignores it and
a player that has none to give both sound exactly as they did before this
existed — `patch_render` and a sequencer have no velocity, and must not be
silent for it. And it **keeps its value through the note-off**: the release is
still part of the note that was struck, and a release stage that suddenly saw a
different velocity would be describing a different note.

**The note module must be NAMED `note`.** The voice pool looks it up by name,
not by type, so `module kbd = note` is a perfectly good note module that nothing
will ever find — the patch compiles, renders, and is silent. The loader does say
so (*"the patch has no module named note, so it cannot be played by note"*), and
the tools print it, so **read the warning line**. A patch with no keyboard at all
is legitimate — a drone, a sequenced piece — which is why this is a warning and
not an error.

**`set` writes a PORT; declaration keys go on the `module` line.** `sdist` takes
`drive` as a declaration key, not a port, so `set drv.drive = 1.6` fails with
*"module drv has no port drive"* while `module drv = sdist drive=1.6` works. The
module reference above says which is which: the `declaration keys` column is the
`module` line, the `inputs` column is `set` and `connect`.

**`amount=` must not have a space after the `=`.** `amount= 0.5` splits into two
tokens and reports *"amount is not a number"*.

**⚠️ `dB` is accepted as a suffix and stripped WITHOUT converting.** `-6dB`
becomes −6, not 0.5. On a port with a range you get an error — *"gain = -6 is
outside 0 .. 16"* — but on a bipolar input or an `amount=` it passes silently and
is wrong by a factor of a hundred. Write the linear value.

**Block-only modules cannot go in a feedback cycle.** Everything prefixed `s`,
plus `inst`. The patch refuses to compile and names the module and the cycle,
rather than running the expensive block-of-one path or sounding wrong quietly.

**A silent patch is usually a closed gate.** `patch_render` does not play notes
at all — it renders the graph with the gate shut, which for any patch with an
envelope on the amplifier means silence. Use `patch_play` to hear notes.

**Voices sum.** Three notes of an ordinary patch pass 1.0; the pool now has a
soft limiter on the output, linear below 0.70 and asymptotic above, so chords no
longer square off. Below the knee the signal is untouched.

## Module reference

<!-- BEGIN MODULE REFERENCE -->
<!-- GENERATED BY patch_doc — do not edit by hand.
     Regenerate with: bin/<plat>/patch_doc >> library/patches/README.md -->

Every row below is produced by building the module and asking it, so it
cannot drift from the code. `rate` says whether the module may sit inside a
feedback cycle: `both` may, `block` may not and the patch refuses to compile
if you try. Inputs show their role, the value they hold when nothing is
patched in, and the range they clamp to.

### Core

| type | rate | declaration keys | inputs `[role default min..max]` | outputs |
|---|---|---|---|---|
| `osc` | both | freq, shape | pitch [pitch 0], pw [0..+ 0.5 0..1], sync [gate 0] | out |
| `filter` | both | cutoff, mode | in [audio 0], cutoff [pitch 0], res [0..+ 0.2 0..1] | out |
| `amp` | both | — | in [audio 0], gain [0..+ 0 0..16] | out |
| `env` | both | a, d, r, s | gate [gate 0] | out |
| `lfo` | both | phase, rate, shape | rate [pitch 0] | out |
| `delay` | both | time | in [audio 0] | out |
| `input` | both | channel | — | out |
| `note` | both | — | — | pitch, gate, vel |

### Electronic

| type | rate | declaration keys | inputs `[role default min..max]` | outputs |
|---|---|---|---|---|
| `seq` | both | gatems, gates, steps, values | clock [gate 0], reset [gate 0] | out, gate |
| `sh` | both | — | in [-..+ 0], trig [gate 0] | out |
| `ring` | both | — | a [audio 0], b [audio 0] | out |
| `glide` | both | — | in [pitch 0], time [0..+ 0.08 0..10] | out |
| `noise` | both | color, seed, type | — | out |
| `quant` | both | scale | in [pitch 0] | out |
| `follow` | both | — | in [audio 0], attack [0..+ 0.005 0.0001..2], release [0..+ 0.12 0.0001..8] | out |
| `fold` | both | — | in [audio 0], fold [0..+ 1 0..16], sym [-..+ 0 -1..1] | out |
| `lpg` | both | — | in [audio 0], cv [0..+ 0 0..1], resp [0..+ 0.02 0..1] | out |

### Instruments

| type | rate | declaration keys | inputs `[role default min..max]` | outputs |
|---|---|---|---|---|
| `karplus` | both | freq | pitch [pitch 0], gate [gate 0], amp [0..+ 1 0..8] | out |
| `modal` | both | freq | pitch [pitch 0], gate [gate 0], amp [0..+ 1.25 0..8] | out |
| `bowed` | both | freq | pitch [pitch 0], gate [gate 0], amp [0..+ 5 0..8] | out |
| `reed` | both | freq | pitch [pitch 0], gate [gate 0], amp [0..+ 1.8 0..8] | out |
| `fmop` | both | detune, feedback, fixedfreq, ratio | pitch [pitch 0], gate [gate 0], amp [0..+ 0.33 0..8], phasem [audio 0] | out |

### Instrument library

| type | rate | declaration keys | inputs `[role default min..max]` | outputs |
|---|---|---|---|---|
| `inst` | block | freq, instrument, library, preset, source | pitch [pitch 0], gate [gate 0], amp [0..+ 0.6 0..8] | out, outR |

### Space

| type | rate | declaration keys | inputs `[role default min..max]` | outputs |
|---|---|---|---|---|
| `pan` | both | — | in [audio 0], pan [-..+ 0 -1..1] | out, outR |
| `width` | both | — | in [audio 0], inR [audio 0], width [0..+ 1 0..2] | out, outR |
| `space` | both | doppler, max, ref, rolloff | in [audio 0], x [-..+ 0 -100..100], y [-..+ 0 -100..100], z [-..+ -1 -100..100] | out, outR |

### Body

| type | rate | declaration keys | inputs `[role default min..max]` | outputs |
|---|---|---|---|---|
| `formant` | both | body, kind | in [audio 0], mix [0..+ 1 0..1] | out |
| `tube` | both | mode | in [audio 0], freq [0..+ 220 20..8000], res [0..+ 0.9 0..0.999], mix [0..+ 1 0..1] | out |

### Bridged from SAF units

| type | rate | declaration keys | inputs `[role default min..max]` | outputs |
|---|---|---|---|---|
| `sdelay` | block | feedback, moddepth, modrate, time | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `schorus` | block | depth, rate, voices | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `sflanger` | block | — | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `sphaser` | block | — | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `sreverb` | block | damping, size, width | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `scomp` | block | ratio, threshold | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `slimiter` | block | — | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `sdist` | block | drive, gain, tone | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `sautospace` | block | reflect, size, width | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `sbody` | block | body, kind, width | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `sconv` | block | ir, irraw | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
| `seq3` | block | gain, bNtype, bNfreq, bNgain, bNq, bNoff  (N = 0..7) | in [audio 0], inR [audio 0], mix [0..+ 1 0..1] | out, outR |
<!-- END MODULE REFERENCE -->


## The instruments

SAF's synthesis engines are modules too — `karplus` (plucked string), `modal`
(struck percussion), `bowed` (bowed string), `reed` (single-reed wind) and
`fmop` (an FM operator). Each takes `pitch`, `gate` and `amp`, and `freq=` at
declaration sets the note the pitch input is relative to. Pitch stays live
between triggers, so an LFO patched into it gives vibrato on a sounding note
rather than only at the attack — see `sustained.patch`.

They are sample-first, so unlike the bridged effects they may sit inside a
feedback cycle.

Each carries an output trim, because the engines were validated separately and
land as much as 24 dB apart on their own (at 220 Hz, RMS: FM operator 0.458,
bowed string 0.030). The trim brings a bare module to about RMS 0.15, so the
five are usable in one patch without one burying another; it only sets the
default of the `amp` input, which the patch still owns.

## The whole library: `inst`

The five above make sound from nothing. The other six techniques — classic, FM,
wavetable, additive, partial, sample, SID — need DATA: a table, harmonic tracks,
a recording. That data already has a home in `TSAFPart` and the `.safinst`
registry, so `inst` hands the job to the machinery built for it, and one module
type reaches **all eleven techniques and all nine libraries**:

    module s = inst instrument="Drawbar Organ"
    module s = inst library=library/orchestra.safinst instrument="Violin"
    module s = inst source=additive preset=strings

Quote a name that contains spaces. `library=` loads a `.safinst` file, once per
path however many voices ask for it; a name that is not found says so and names
the fix. See `library.patch`.

Two things about `inst` are worth knowing before they surprise you:

- It is **block-only**, so it cannot go inside a feedback cycle. Gate edges are
  still sample-accurate — the block is split at each edge — but the module
  cannot be advanced one sample at a time and does not pretend to be.
- **Pitch is latched at the trigger.** A Part hands the frequency to the voice at
  note-on (Karplus bakes it into the delay line right there), so an LFO on
  `inst.pitch` chooses the note, it does not bend it. The five native instrument
  modules *do* bend — use those when the pitch has to move while the note
  sounds.

`inst` also carries an output trim, for the same reason as the others: measured
across the built-in library a bare Part peaks between 1.01 and 1.47, so at unity
it would clip before the patch had done anything. The default is 0.6. Presets
derived from recordings sit lower still — the four recorded libraries measured
between 0.05 and 0.54 at that default — so expect to raise `amp` for those.

It is one voice. Polyphony belongs to the patch voice pool, which already runs N
independent graphs; a polyphonic Part inside each would be polyphony twice over.

## Shaping and control

    quant    snaps a pitch to a scale: chromatic, major, minor, dorian,
             phrygian, pentatonic, minorpent, blues, whole, octave, fifth
    follow   an envelope follower — turns any signal into a control voltage
             tracking its loudness, separate `attack` and `release`
    fold     a wavefolder — ADDS harmonics by reflecting the signal about +-1
             instead of removing them with a filter
    lpg      a low-pass gate — one control opens amplitude AND brightness at
             once, through a vactrol whose lag (`resp`) is the character

`quant` is what makes a random voltage into a melody rather than a warble:
patch `noise` through `sh` into it and the sequence is in key. Verified by
measuring the notes it produces — on `major` only degrees 0 2 4 5 7 9 11
appear, on `minorpent` only 0 3 5 7 10, on `whole` only 0 2 4 6 8 10.

`lpg` is the one worth understanding, because it is not a VCA. Sweeping its
`cv` from 0.15 to 1.0 raises the level by 35 dB *and* the spectral centroid
from 710 Hz to 2728 Hz — a VCA would hold the centroid flat. Quieter is also
duller, which is what a struck object does. `westcoast.patch` builds a voice
the Buchla way: no filter anywhere, timbre from `fold`, articulation from `lpg`.

One honest note on `follow`: its attack is the time constant of the detector,
not of a step response. On a 220 Hz saw a 3 ms setting measures about 9 ms to
90%, because the detector needs a couple of cycles of the waveform to see the
peak. That is inherent to following an oscillating signal, not slack in the
module.

## The body stage

What the sound radiates *through*. These are not effects applied to a finished
sound — a tube, a violin's box, a plate is the thing that turns an excitation
into an instrument.

    formant   kind=none|violin|viola|cello|sax|clarinet, mix
    tube      mode=full|odd, freq, res, mix
    sbody     kind=none|sax|violin|guitar, width, mix   (block, measured modes)
    sconv     convolution; a pass-through until an impulse response is loaded

`formant` and `tube` are sample-first, so they may sit inside a feedback cycle —
which matters more here than anywhere else, because an excitation feeding a body
that feeds back into the excitation *is* the physical model. The tube's `freq`
is a port, so the body can be retuned while it sounds; no real instrument does
that, and it is the first thing anyone tries with a patch cord in hand.

## The spatial stage

Where a mono signal becomes a pair. This is the only place in the graph where
that happens — every port before it carries one signal, and that stays true.

    pan     mono -> pair, constant power, `pan` from -1 to 1
    width   pair -> pair, mid/side, `width` 0 = mono, 1 = unchanged, 2 = double
    space   mono -> pair, a position in a room: x, y, z in metres, distance
            attenuation, interaural time and level difference, Doppler
    sautospace   the block-oriented widener (width, size, reflect, mix)

All three natives are sample-first, which is the point: `x` and `z` are ports
like any other, so a source can be *moved* while it sounds. `orbit.patch` circles
the listener with two LFOs at the same rate a quarter turn apart, and nothing
else — that is what the `phase=` on the LFO is for.

`width` scales the side and leaves the mid alone, so a mono fold returns exactly
the same signal at any width; measured across width 0, 1 and 2 the mid RMS is
identical to four decimals.

Two things `space` will do that are worth expecting. It has a **front/back
ambiguity**: a source at 60°, 90° and 120° measures the same, because interaural
time and level differences alone cannot separate them — that is the cone of
confusion, and resolving it needs spectral cues this model does not have. And it
keeps a **head-shadow floor**: a source straight to one side sits about 15 dB
down in the far ear, not silent, because sound diffracts around a head. Without
that floor the far ear goes to exactly zero and the result is the hard-panned
ping-pong of early stereo rather than anything a room does.

## Why there is no mixer module

Summing is what a port already does: several links into one input, each with its
own `amount`. Aux sends are more links from the same output. Inserts are modules
in series. What `TSedaiMixer` adds beyond that is solo, mute and metering, which
are things a user interface does, not things a signal graph does. A mixer module
would be a second way to do what the wires already do — the same reason there is
no attenuverter, no CV mixer and no FM module.

## Ins and outs

A port carries **one mono signal**. That is a decision, not an omission: stereo
inside a port would force every module to know a channel count and every patch to
care about it.

The *boundary* is where channels exist. Repeat the `output` line and the patch
has that many channels, in declaration order; `module l = input channel=0` takes
a channel of the source file. So a patch can be mono in and stereo out, stereo
in and stereo out, or anything else — `stereo.patch` and `fx_stereo.patch` are
the two examples.

Width comes from the two channels being *different*, not from one signal panned.
But not arbitrarily different: two wholly independent voices measure a left/right
correlation near zero, which is the abuse of the early stereo era — one
instrument hard left, another hard right, a picture no room produces. A real
violin measured here sits at correlation 0.771. So `stereo.patch` sends both
oscillators to both filters at different amounts and detunes slightly: measured
correlation 0.923, side/mid 0.201, and it survives a mono fold with no
cancellation.
