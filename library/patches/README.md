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
    bin/<plat>/patch_live library/patches/poly.patch 8

Edit the file in any editor, save, and the sound changes without a restart. A
save that does not compile prints the error and leaves the previous version
playing, so a typo never drops you into silence wondering what happened.

Keys: `z s x d c v g b h n j m` are one octave from C, `,` and `.` change
octave, SPACE releases everything, `R` forces a reload, `I` prints the compiled
stages, `Q` quits. It needs a REAL terminal — with stdin redirected the keyboard
stays inert by design.

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

| Patch | What it shows |
|---|---|
| `basic.patch` | The classic subtractive voice: oscillator → filter → amplifier, envelope on the gain, keyboard on pitch. Every stage is acyclic, so the whole graph runs at block rate. |
| `vibrato.patch` | The same voice with an LFO patched into pitch. Raise `lfo1 rate` into the audio range and it becomes FM without changing a single connection — that is the point of having one signal type. |
| `feedback.patch` | A real loop: the filter's output returns to its own input through a VCA. The graph detects the cycle, isolates those two modules, and runs only them one sample at a time. Everything else stays at block rate. |
| `poly.patch` | Meant to be played. Its keyboard connections are `normalled`, so they are there until you patch something into those inputs yourself. Render it with `patch_play`. |
| `lead.patch` | A subtractive lead with some bite. Two detuned oscillators plus a sub an octave down, and — the thing that actually matters — a SECOND envelope on the filter cutoff. Measured, its spectral centroid sweeps 4.1x from attack to sustain, where `poly.patch` sits at 1.0x and sounds static because of it. |
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

Module types: `osc`, `filter`, `amp`, `env`, `lfo`, `delay`, `note`.

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

A port carries **one mono signal**. That is a decision, not an omission: stereo
inside a port would force every module to know a channel count and every patch to
care about it. Stereo arrives later as a pair of ports or an explicit per-port
channel count.
