# Example patches

Text patches for the SAF Patch Workbench. A patch is a table of modules, values
and connections — the ARP 2500 / EMS VCS3 matrix written as rows instead of pins,
which is why it diffs and versions like source.

Render one to a WAV:

    ./build.sh --source job/tools/saf/patch_render.lpr --dest bin/<plat>/patch_render
    bin/<plat>/patch_render library/patches/basic.patch out.wav 2 0 1.2
                            ^patch                     ^wav   ^s ^semitones ^gate

| Patch | What it shows |
|---|---|
| `basic.patch` | The classic subtractive voice: oscillator → filter → amplifier, envelope on the gain, keyboard on pitch. Every stage is acyclic, so the whole graph runs at block rate. |
| `vibrato.patch` | The same voice with an LFO patched into pitch. Raise `lfo1 rate` into the audio range and it becomes FM without changing a single connection — that is the point of having one signal type. |
| `feedback.patch` | A real loop: the filter's output returns to its own input through a VCA. The graph detects the cycle, isolates those two modules, and runs only them one sample at a time. Everything else stays at block rate. |
| `echo.patch` | A loop through a 120 ms delay line. The graph works out that the shortest cycle carries 5293 samples of delay and advances the loop in chunks of 5293 rather than one at a time - bit-identical output, 42% faster. |

Format:

    mode    = block | sample          # sample forces every stage per-sample
    module  <name> = <type> [key=value ...]
    set     <module>.<port> = <value>
    connect <module>.<out> -> <module>.<in> [amount=x]
    output  <module>.<port>

Values take unit suffixes: `440Hz`, `2ms`, `120ms`, `50%`, or a plain number.
Pitch inputs are volts-per-octave: `1.0` is one octave, so a constant offset is a
transposition and any modulator is automatically musical.

Module types: `osc`, `filter`, `amp`, `env`, `lfo`, `delay`, `note`.

A port carries **one mono signal**. That is a decision, not an omission: stereo
inside a port would force every module to know a channel count and every patch to
care about it. Stereo arrives later as a pair of ports or an explicit per-port
channel count.
