# Sedai Audio Foundation

[![Development](https://img.shields.io/badge/status-development-orange.svg)]()
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Maurizio%20Cammalleri-0077B5?logo=linkedin)](https://www.linkedin.com/in/maurizio-cammalleri-80a89a11/)
[![Substack](https://img.shields.io/badge/Substack-Maurizio%20Cammalleri-FF6719?logo=substack)](https://cammalleri.substack.com/)

A modular audio synthesis and DAW foundation library in Free Pascal — and, on
top of it, a **patch workbench**: instruments described as text, built out of
connectable modules, and playable from a MIDI keyboard.

> **DISCLAIMER**: advanced development, **not production-ready**. The API is
> stabilising and most of it works, but there will be bugs. Try it and report
> what breaks.

| Branch | Status | |
|---|---|---|
| **main** | [![Stable](https://img.shields.io/badge/status-stable-green.svg)]() | Preliminary release, stable enough for testing |
| **develop** | [![Development](https://img.shields.io/badge/status-development-orange.svg)]() | Active development |

---

## What it is

Three layers, and you can stop at any of them.

**A library.** Eighty-one units of DSP — oscillators, filters, envelopes,
effects, a mixer, a transport, file I/O — each a small class that does one
thing, meant to be combined rather than configured.

**A patch workbench.** A text file lists modules and the connections between
them, and that *is* the instrument. There is one kind of connection: any output
can feed any input. There is no modulation system and no mod matrix, because
audio, pitch, gate and knobs are the same thing — which is what lets one LFO be
vibrato or FM depending on nothing but its rate.

**A language on top.** A table cannot say *"and nine of these, each a little
quieter than the last"*. So an instrument can instead be a program in SedaiBasic
MODERN, which when run **prints** the patch. Loops, inheritance, real
arithmetic — and the result is still a text file the engine already knows how to
read.

Plus two things that stand on their own: a **cycle-exact MOS 6581/8580 SID**
emulation, and a **GoatTracker v2** player that plays `.sng` files.

---

## Quick start

```
./setup.sh          # fetches SDL2
./build.sh          # library + tools
```

**Render an instrument to a WAV — no audio device needed.**

```
bin/x86_64-linux/saf_play library/winds.safinst "Tenor Sax"
bin/x86_64-linux/saf_play                       # a tour of every shipped library
```

**Play a GoatTracker tune.**

```
bin/x86_64-linux/sng_player song.sng
```

**Turn an instrument written as a program into a patch.**

```
sb library/instruments/hammond.bas > library/patches/hammond.patch
```

**Check that everything still works.**

```
bin/x86_64-linux/saf_regression      # 193 checks, headless
bin/x86_64-linux/sedaisid_test       # SID Evo against reSID
```

> **Not yet shipped.** Several tools used daily still live in the gitignored
> working area `job/tools/saf/` and are **not in the repository**:
> `patch_live` (the live ear loop with hot reload and MIDI in), `patch_midi`,
> `patch_fixture` (the sound guard), `patch_doc` (generates the module
> reference) and `midi_probe`. They are referenced below because they are how
> the work is actually done; a fresh clone will not have them yet. Moving them
> into `tools/` is the next housekeeping job.

---

## The Patch Workbench

An instrument is a text file. It diffs and versions like source, which is most
of why it exists.

```
module note = note
module osc1 = osc    shape=saw freq=110
module filt = filter mode=lowpass cutoff=800
module env1 = env    a=0.005 d=0.15 s=0.6 r=0.3
module amp  = amp

connect note.pitch -> osc1.pitch
connect note.gate  -> env1.gate
connect osc1.out   -> filt.in
connect filt.out   -> amp.in
connect env1.out   -> amp.gain  amount=0.3

output  amp.out
```

Keep `patch_live` running while you edit the file and the sound changes without
a restart — a save that does not compile prints the error and leaves the previous
version playing. (That tool is one of the five not yet in the repository; see
the note above.)

Seven statements, **40 module types**, and 27 patches shipped in
[`library/patches/`](library/patches/). The full grammar, the traps with their
real error messages, and a **module reference generated from the registry** —
not transcribed, so it cannot drift — are in
[`library/patches/README.md`](library/patches/README.md).

Some things worth knowing without reading all of it:

- **Connections sum.** Several sources into one input add up, which is why there
  is no mixer module. `amount=` scales one source and may be negative, which is
  the attenuverter of a modular system.
- **Sample rate is scoped, never global.** The graph is decomposed into strongly
  connected components; only modules actually inside a feedback loop advance one
  sample at a time. Measured on a 20-node chain: 436× realtime mixed, against
  365× if the whole graph is driven per-sample.
- **Feedback is a technique, not an error.** A cycle is detected, its back edges
  read the previous sample, and that unit delay is what makes the loop
  computable.
- **A patch is a voice template.** The pool builds N independent copies, so a
  chord is not one note three times as loud: nothing is shared, so nothing
  phase-locks.

### Velocity

`note.vel` is how hard the key was pressed, and it is a signal like any other
rather than a taller gate. On almost every acoustic instrument playing harder
makes the sound *brighter* as much as louder, so velocity usually wants to reach
a filter cutoff at least as much as an amplifier.

Eight of the shipped instruments respond to it, under a rule worth copying: **at
full velocity each one is bit-identical to what it was before velocity existed.**
Full strength is the instrument's voice; velocity can only darken and soften from
there. That is arranged, not hoped for — and the sound fixtures prove it.

---

## Instruments in SedaiBasic MODERN

    sb library/instruments/hammond.bas > library/patches/hammond.patch

**This is not a translation. It is an execution.** The `.bas` is a real program
that really runs: the loops loop, the arithmetic is done, a child class overrides
a method and the override is really dispatched. The `.patch` is the program's
*side effect* — the objects in the library do not make sound, they take notes,
and at the end they print what they were told.

So you are not writing a description in disguise. You have the whole language.
The `.patch` that comes out is flat and dull, exactly as it should be; the
intelligence lives in the `.bas`.

```basic
For i = 0 To This.Count - 1
  cents = This.Detune * (i / (This.Count - 1) - 0.5)
  This.Src(i) = New Osc
  This.Src(i)->Init("osc" + Str(i))
  This.Src(i)->KeyN("freq", This.BaseFreq)
  This.Src(i)->Pitch.Value = cents / 1200.0
  This.Src(i)->Pitch.Add(This.Kbd->Pitch)
  This.Filt->In.Add(This.Src(i)->Out, 1.0 / This.Count)
Next
```

Changing `Count` from 3 to 12 takes that instrument from 8 modules to 17, with
the detune spread and the mix share computing themselves — and the level stays
put, because the share is `1/Count` and nobody has to know what the parent
decided.

**Neither project knows about the other.** `sb` has never heard of SAF: it prints
text. SAF has never heard of `sb`: it reads a `.patch`, which is what it read
before. Nothing links, so there is no dependency that could become circular — and
an instrument, once generated, keeps working where SedaiBasic is not installed.

`tools/patch_bas` does the other direction and generates the MODERN library from
the module registry. Details, and the traps, in
[`library/instruments/README.md`](library/instruments/README.md).

---

## Playing it: MIDI in

```
bin/x86_64-linux/patch_live library/patches/poly.patch 8 128 48000 --midi
bin/x86_64-linux/patch_live --list-midi
```

(`patch_live` and `patch_midi` are two of the five tools not yet in the
repository — see the note under Quick start.)

ALSA sequencer on Linux, Windows MME on Windows, and the system library is
loaded at runtime rather than linked — so a machine without it says "no MIDI"
instead of failing to start. Velocity arrives as `note.vel`, the sustain pedal
holds notes past the key, and the pitch wheel bends every sounding voice.

Note events cross into the audio thread through a **lock-free queue**, and the
block is split at each event's sample position. At a 256-sample buffer, rounding
events to block boundaries would throw away 5.8 ms of timing — more than the
MIDI cable itself loses transmitting the note.

Two things no software can fix, both measured here: a PC keyboard is a matrix
without diodes and stops reporting past three or four simultaneous keys, and the
audio buffer is the other half of the latency — 1024 samples is about 59 ms and
feels like rubber, 128 is about 8 ms and is inside what an acoustic player
already lives with.

Standard MIDI files play through `TSedaiMIDIPlayer`, or straight into a patch:

```
bin/x86_64-linux/patch_midi song.mid library/patches/poly.patch out.wav 16
```

---

## Synthesis engines

The `.safinst` preset system has **eleven techniques** — the eleven values of
`TSAFPartSource` — and a single universal voice (`TSedaiVoice`) can be any of
them.

| Engine | Unit | |
|---|---|---|
| Classic / subtractive | `SedaiOscillator` | PolyBLEP/PolyBLAMP band-limited saw, square, pulse, triangle; SID mode |
| FM | `SedaiFMOperator` | 6-operator DX7-style, selectable algorithms, feedback |
| Wavetable | `SedaiWavetableGenerator` | morphing between frames, mipmaps against aliasing |
| Additive | `SedaiAdditiveGenerator` | up to 64 harmonics, per-voice drift, unison |
| Sample | `SedaiSamplePlayer` | loop modes, pitch, interpolation |
| Free-partial | `SedaiPartialGenerator` | McAulay-Quatieri sinusoidal model: N partials with time-varying frequency and amplitude, note-transposable, analysed from a WAV |
| Karplus-Strong | `SedaiKarplusGenerator` | plucked string and percussion |
| Waveguide reed | `SedaiReedGenerator` | self-oscillating single reed: nonlinear reed plus bore (clarinet, sax) |
| Bowed string | `SedaiBowedGenerator` | nonlinear bow friction plus string waveguide |
| Modal | `SedaiModalGenerator` | a struck body as a bank of ringing modes |
| SID | `SedaiOscillator` (SID mode) | the chip's oscillators as an ordinary voice source; the cycle-exact chip is separate, below |

Noise (`SedaiNoiseGenerator`: white, pink, brown, blue, violet) is a generator
rather than a preset technique, and is used from inside the others.

The physical models are joined by a shared **formant body**
(`SedaiFormantBody`), a **tube resonator** (`SedaiTubeResonator`) and a
**convolver** (`SedaiConvolver`) for measured impulse responses — the "body" that
keeps a self-oscillating model from sounding like a tube instead of an
instrument.

### SID Evo

`src/SID/SedaiSIDEvo.pas` is a MOS 6581/8580 emulation on the reSID
two-integrator-loop model, plus EVO extensions: up to 64 voices, stereo, LFOs,
extended waveforms.

It is **bit-exact against reSID** on both code paths — 0 mismatches over
15,764,000 cycles — including the nonlinear 6581 filter distortion, and including
`expf`, whose glibc implementation was carried into Pascal to close the last
gap. The filter sweep is 35/35 exact and a full tune is 0 mismatches over
45,321,500 cycles. Getting there meant finding one wrong constant
(`W0_MAX_1` = 105414, not 105413) and one rounding difference (`Trunc`, not
`Round`); the harnesses that found them live in `job/tools/resid/`.

---

## GoatTracker player

Native playback of GoatTracker v2 `.sng` files: a clean reimplementation of the
player routine (`gplay.c` / `gsid.cpp` / `gsound.c`) in
`src/Players/SedaiGoatTracker.pas`, driving SID Evo.

```
bin/x86_64-linux/sng_player song.sng [subtune]
bin/x86_64-linux/sng_dump   song.sng          # structure and SID registers
```

Instrument tables, wavetables, pulse and filter tables, all pattern commands,
per-voice muting, subtune selection. Controls during playback: SPACE pause,
`R` restart, `L` loop, `V`/`W` verbose, `S` tables, `1`/`2`/`3` mute voices,
`+`/`-` subtune, `Q` quit.

---

## The shipped instrument library

Ready-to-play instruments as `.safinst` text libraries under
[`library/`](library/). Load one with `TSedaiInstrumentRegistry` and apply a
preset to a `TSAFPart`.

| Library | Contents | Technique | Preset data |
|---|---|---|---|
| `builtin.safinst` | 32 synth presets | all | GPL-3.0 (SAF) |
| `winds.safinst` | clarinet, soprano/alto/tenor sax | waveguide reed + formant body | GPL-3.0 (SAF) |
| `strings.safinst` | violin, viola, cello | bowed string + formant body | GPL-3.0 (SAF) |
| `percussion.safinst` | bell, marimba, tubular bell, woodblock, tom | modal | GPL-3.0 (SAF) |
| `orchestra.safinst` | strings, woodwinds, brass, muted trumpets (15) | additive resynthesis | CC0 (VSCO-2-CE) |
| `pipes_reeds.safinst` | organs, recorders, ocarina, harmonica, accordion (8) | additive resynthesis | CC0 (VCSL + FreePats) |
| `mallets.safinst` | glockenspiel, vibraphone, tubular bells, marimba, xylophone, balafon, wine glass (7) | free-partial | CC0 (VCSL) |
| `plucked_keys.safinst` | nylon/electric guitar, electric bass, harps, grand/upright piano, harpsichord (8) | free-partial | CC0 (FreePats + VCSL) |
| `vcsl.safinst` | alto recorder, saxello, tenor sax | additive resynthesis | CC0 (VCSL) |

The presets were derived from CC0 recordings by analysis, not sampling: a
partial tracker and an additive analyser turn a WAV into a playable, transposable
instrument. Attribution detail in [`library/README.md`](library/README.md).

```pascal
reg := TSedaiInstrumentRegistry.CreateEmpty;
fs  := TFileStream.Create('library/winds.safinst', fmOpenRead);
try reg.LoadFromStream(fs); finally fs.Free; end;

part := TSAFPart.Create;
part.SetSampleRate(48000);
reg.ApplyToPartByName('Tenor Sax', part);
part.NoteOn(60, 1.0);                        // then part.RenderBlock(@buf, frames)
```

---

## Audio file I/O

| Format | Read | Write | |
|---|---|---|---|
| WAV | ✅ | ✅ | 8/16/24/32-bit PCM, 32/64-bit float |
| AIFF / AIFC | ✅ | ✅ | big-endian PCM |
| FLAC | ✅ | ✅ | pure Pascal, lossless, verified against `ffmpeg` |
| OGG Vorbis | ✅ | — | pure Pascal; bisection seek |
| MP3 | ✅ | — | pure Pascal; gapless |

Writing offers RPDF, TPDF and noise-shaped dither. Everything is pure Pascal:
the only external dependency in the whole project is SDL2, and only for opening
an audio device — offline rendering needs none.

---

## Architecture

Layers, bottom up. Nothing in a lower layer knows about a higher one.

```
Patch / Language     .patch graph · SedaiBasic MODERN instruments
Engine / Transport   parts, presets, tracks, clips, automation, project
Voice / Mixer        voice pool, modulation matrix, channels, buses, master
Generators · Processors · Effects · Modulators
Core                 types, buffers, ports, signal nodes, randomness
Platform             audio backend, MIDI input, threads, timing
```

### Every unit

**`src/`** — `SedaiAudioFoundation`: the high-level facade
(`TSAFSynthType = (safClassic, safFM, safWavetable)`). Note that the additive
generator, SID Evo and the patch workbench are standalone: the facade does not
wrap them.

| Folder | Units |
|---|---|
| **Core** (7) | `SedaiAudioTypes` core types · `SedaiAudioObject` root class · `SedaiSignalNode` DSP node · `SedaiAudioBuffer` managed buffer · `SedaiParameterPort` modulatable parameter · `SedaiSpatialAudio` positional maths · `SedaiRandom` per-object generator |
| **Platform** (5) | `SedaiAudioBackend` device, push or callback · `SedaiAudioSDL2Dyn` runtime-loaded SDL2 · `SedaiMIDIInput` ALSA / winmm · `SedaiThread` · `SedaiTiming` |
| **Generators** (11) | `SedaiOscillator` · `SedaiNoiseGenerator` · `SedaiWavetableGenerator` · `SedaiSamplePlayer` · `SedaiFMOperator` · `SedaiAdditiveGenerator` · `SedaiPartialGenerator` · `SedaiKarplusGenerator` · `SedaiReedGenerator` · `SedaiBowedGenerator` · `SedaiModalGenerator` |
| **Modulators** (3) | `SedaiEnvelope` ADSR, 4 curves, SID mode · `SedaiLFO` tempo-syncable · `SedaiStepModulator` |
| **Processors** (6) | `SedaiFilter` 6 types, 12/24/48 dB/oct · `SedaiAmplifier` · `SedaiCompressor` · `SedaiLimiter` · `SedaiDistortion` · `SedaiEQ` |
| **Effects** (11) | `SedaiEffect` base · `SedaiDelay` · `SedaiReverb` · `SedaiChorus` · `SedaiFlanger` · `SedaiPhaser` · `SedaiAutoSpace` mono-safe widener · `SedaiBodyResonator` · `SedaiTubeResonator` · `SedaiFormantBody` · `SedaiConvolver` short-FIR |
| **Voice** (3) | `SedaiVoice` universal voice · `SedaiVoiceManager` allocation and stealing · `SedaiModulationMatrix` |
| **Mixer** (3) | `SedaiMixerChannel` · `SedaiBus` aux, group, master · `SedaiMixer` |
| **Engine** (5) | `SedaiEngine` · `SedaiAudioEngine` · `SedaiPart` instrument part · `SedaiInstrumentPreset` `.safinst` · `SedaiSpatialChain` body → space → room |
| **Transport** (4) | `SedaiTransport` · `SedaiTrack` audio and MIDI · `SedaiClip` · `SedaiProject` `.safproj` |
| **Patch** (11) | `SedaiPatchGraph` ports, modules, Tarjan, two schedulers · `SedaiPatchFile` parser · `SedaiPatchVoices` polyphonic pool · `SedaiPatchEvents` lock-free note queue · `SedaiPatchModules` core types · `SedaiPatchElectronic` · `SedaiPatchInstruments` · `SedaiPatchPart` library instruments · `SedaiPatchSpace` · `SedaiPatchBody` · `SedaiPatchLegacy` bridge to the block-oriented units |
| **FileIO** (7) | `SedaiAudioFileReader` · `SedaiAudioFileWriter` · `SedaiAudioDecoder` · `SedaiFLACDecoder` · `SedaiFLACEncoder` · `SedaiVorbisDecoder` · `SedaiMP3Decoder` |
| **Players** (2) | `SedaiGoatTracker` · `SedaiMIDIPlayer` |
| **SID** (1) | `SedaiSIDEvo` |
| **Wavetable** (1) | `SedaiWavetableLoader` Serum, Vital, Surge, generic WAV |

### Design principles

1. **Single responsibility.** Each class does one thing.
2. **Composition over inheritance.** Build by combining.
3. **Real-time safe.** No allocations in the audio thread; lock-free where it
   matters. The note queue is a fixed ring sized once, because one `malloc` in
   the callback is a click.
4. **Every generator owns its randomness.** A shared generator means every sound
   shares one stream: draw a number anywhere and every plucked string changes.
   Found by measurement, not by reasoning — adding one file to a directory used
   to change the sound of an instrument.
5. **Testable in isolation**, and tested — see below.

---

## Building

FPC 3.2.2 or newer. The only dependency is SDL2, and only for audio output.

```
./setup.sh            # or setup.ps1 on Windows: fetches SDL2
./build.sh            # everything; --tests adds the QA suite
./build.sh --target saf_regression
./build.sh --target patch_bas --os win64 --cpu x86_64
```

`build.ps1` and `build.sh` are functional ports of each other; the switch
mapping (`-Target x` ⇄ `--target x`) is in each script's header. Output goes to
`bin/<cpu>-<os>/`, compiled units to `lib/<cpu>-<os>/`.

| Target | Kind | |
|---|---|---|
| `sng_player` | tool | GoatTracker `.sng` player |
| `sng_dump` | tool | `.sng` structure and SID register dump |
| `patch_bas` | tool | `.patch` ⇄ SedaiBasic MODERN bridge; `--lib` generates the MODERN library |
| `saf_play` | demo | loads a library, plays a phrase, renders a WAV — offline |
| `demo_synth` | demo | interactive SDL2 synth demo |
| `test_saf_main` | test | facade test (classic / FM / wavetable) |
| `audiotest` | test | backend and render path |
| `sedaisid_test` | test | SID Evo verification against reSID |
| `saf_regression` | test | headless regression suite, 193 checks |

Sources for tools live in `tools/`, everything else in `test/`.

**On Linux SDL2 needs its `-dev` package** (`libsdl2-dev` / `SDL2-devel` /
`sdl2`): the bindings `dlopen("libSDL2.so")`, which only the dev symlink
provides. Missing it gives silent no-audio, not a crash.

**`cthreads` must be the first unit** of any program that opens audio on Unix,
because SDL2 calls back from its own thread.

---

## API reference

The high-level facade in `SedaiAudioFoundation.pas` exports **128 global
functions** — a `Play*` API for getting a sound out in one line. It covers
classic, FM, wavetable, additive, Karplus and sample playback; SID Evo, the
physical models and the patch workbench are used through their own units.

Names below are as declared. Pascal is case-insensitive, so `MIDINoteToFreq` and
`MidiNoteToFreq` are the same identifier.

| Group | |
|---|---|
| **Audio** | `InitAudio(VoiceCount)` `ShutdownAudio` `SetMasterVolume(V)` `GetMasterVolume` `GetActiveVoices` `GetMaxVoices` `GetSampleRate` `PrintStatus` |
| **Classic** | `PlaySine` `PlaySquare` `PlaySaw` `PlayTriangle` `PlayPulse` `PlayPWM` `PlaySuperSaw` `PlayNoise` `PlayLead` `PlayBass` `PlayPad` `PlayClassic(Freq, Preset)` `PlayClassicAdv` |
| **FM** | `PlayEPiano` `PlayFMBrass` `PlayFMBell` `PlayFMOrgan` `PlayFMLead` `PlayFMBass` `PlayFM(Freq, Preset)` `PlayFMAdv` |
| **Wavetable** | `PlaySerum` `PlayWasp` `PlayPPG` `PlayWavetable(Freq, Type)` `PlayWavetableAdv` `PlayCustomWavetable` `PlayLoadedWavetable` and the `*Adv` forms |
| **Additive** | `PlayAdditive` `PlayAdditiveAdv` `PlayAdditiveBell` `PlayAdditiveOrgan` `PlayAdditiveStrings` |
| **Karplus / sample** | `PlayKarplus` `PlayKarplusAdv` `PlayKarplusBass` `PlayPluck` `PlaySample` `PlaySampleAdv` |
| **Voice control** | `NoteOff(I)` `NoteRelease(I)` `ReleaseVoice(I)` `RetriggerVoice(I)` `RetriggerVoiceHard(I)` `StopAll` `SmoothStopAll(FadeMs)` `SetVoiceFrequency` `SetVoiceAmplitude` `SetVoicePan` `SetVoiceADSR` `SetVoicePulseWidth` `SetVoiceFilter` `SetVoiceFilterEnabled` `SetVoiceFilterParams` |
| **Instrument shaping** | `SetInstrumentOscillator` `SetInstrumentOscMode` `SetInstrumentSubOsc` `SetInstrumentSync` `SetInstrumentRingMod` `SetInstrumentVibrato` `SetInstrumentTremolo` `SetInstrumentLFO` `SetInstrumentFilterLFO` `SetInstrumentModulation` `SetInstrumentModulationLFO` `ClearInstrumentModulation` |
| **MIDI voices** | `MIDIAllocateVoice` `MIDIIsVoiceActive(I)` `MIDIGetFreeVoiceCount` `MIDIVoiceOn(I)` `MIDIVoiceOff(I)` `MIDIReleaseVoice(I)` `MIDIReleaseAllVoices` |
| **MIDI voice config** | `MIDISetVoiceFrequency` `MIDISetVoiceAmplitude` `MIDISetVoiceWavetable` `MIDISetVoicePan` and the matching `MIDIGetVoiceFrequency` `MIDIGetVoiceAmplitude` `MIDIGetVoiceWavetable` `MIDIGetVoicePan` |
| **MIDI conversions** | `MIDINoteToFreq(Note)` `FreqToMIDINote(Hz)` `MIDIVelocityToAmplitude(V)` `MIDIAmplitudeToVelocity(A)` `MIDIPanToSedai(P)` `SedaiPanToMIDI(P)` `MIDINoteToName(Note)` `MIDINoteToOctave(Note)` |
| **MIDI playback** | `MIDIPlayNote` `MIDIPlayNoteWithFreq` `PlayWavetableMIDI` `RegisterMidiUpdateCallback` `UnregisterMidiUpdateCallback` |
| **Wavetable files** | `LoadWavetableFile` `LoadWavetableDirectory` `ScanWavetableDirectory` `IsWavetableLoaded` `GetLoadedWavetables` `GetWavetableFormats` `ClearWavetableCache` `PrintWavetableInfo` |
| **Samples** | `LoadSampleFile` `RegisterSample` `IsSampleRegistered` |
| **Musical** | `PlayChordClassic` `PlayChordFM` `PlayChordWavetable` `PlayScaleClassic` `PlayScaleFM` `PlayScaleWavetable` `PlayNote` `PlayOnVoice` `Beep` |

MIDI **files** use the object API in `SedaiMIDIPlayer` (`TSedaiMIDIPlayer`), not
globals: `LoadFromFile`, `Play` / `Pause` / `Stop` / `Rewind`,
`AdvanceSamples(N)` from the audio callback, `Playing` / `Paused` / `Loaded`,
`GetDurationSeconds`, `SongName` / `Copyright` / `TrackCount`.

Live MIDI **input** uses `SedaiMIDIInput` (`TSedaiMIDIInput`): `Enumerate`,
`Open`, `ConnectByName`, `Poll`, and `OnNote` / `OnController` / `OnPitchBend`.

---

## How we know it works

Four independent guards, all runnable.

**`saf_regression` — 193 checks, headless.** The whole render path with no audio
device: engine to mixer to master, every source type, cycle detection, file
formats round-tripping, the note queue, sample-accurate events, the sustain
pedal. It also runs as a Windows binary under Wine, same 193.

**Sound fixtures — 25 patches.** Every shipped patch has a signature (hash, peak,
RMS, spectral centroid). `patch_fixture` says which sounds changed and by how
much, so a change to the engine cannot alter an instrument quietly. It writes
the reference **only** with `--update`.

**`sedaisid_test` — bit-exact against reSID.** Not "close": zero mismatches over
tens of millions of cycles, on both the classic and the distortion filter paths.

**The MODERN round trip — 24 of 24.** A shipped `.patch` is lifted back into
SedaiBasic, run, and the regenerated patch must render **byte-identically** to
the original. Two effect patches are skipped (they need an audio input) and one
is refused rather than half-translated.

The live MIDI path was verified without owning a MIDI keyboard, by feeding the
same file to it from `aplaymidi` — somebody else's sequencer, through the kernel
— and comparing against the offline render: 125/125 notes both ways, peak 0.9803
against 0.9816, RMS 0.1843 against 0.1847. What that rig cannot reach is the USB
driver under a physical keyboard; everything from the sequencer port inward is
exercised for real.

---

## Status

| | |
|---|---|
| Units | 81 (~55,000 lines of Pascal) |
| Synthesis techniques | 11 |
| Module types in the workbench | 40 |
| Shipped patches | 27 |
| Instrument libraries | 9 `.safinst` |
| Dependencies | SDL2, for audio output only |
| Platforms | Linux and Windows, both first-class; developed on both |

**Done and working**: the core library, the mixer, the transport and project
layer, all eleven synthesis techniques, the effect chain, file I/O, SID Evo,
the GoatTracker player, the patch workbench, live MIDI input, the SedaiBasic
MODERN bridge.

**Planned**: granular synthesis; vector synthesis; IFFT / spectral resynthesis;
an arrangement layer (buses, position in the room, output format); brass
physical modelling, which currently does not lock to pitch.

### Known limitations

| | |
|---|---|
| API stability | Still moving. Pin a commit for anything serious. |
| OGG / MP3 | Decode only |
| Directivity | The cone is gain-only, so a source turned away gets quieter rather than duller |
| `include` in patches | The MODERN lifter refuses patches that use it, rather than translating half of one |
| Documentation | Unit interfaces are the reference; this file is the map |

---

## License

**GNU General Public License v3.0 only** (GPL-3.0-only). The commercial option
was withdrawn; see [`LICENSING.md`](LICENSING.md).

### Third-party derived components

Two parts are derived from GPL-2.0-or-later projects and keep that lineage:

- **`SedaiSIDEvo`** — the SID emulation follows the model and the tables of
  **reSID** by Dag Lem, and the nonlinear filter of **reSID-fp**.
- **`SedaiGoatTracker`** — a reimplementation of the **GoatTracker v2** player
  routine by Lasse Öörni (Cadaver) and contributors.

Credit is given on principle, and only for what was actually used.

### Contact

Maurizio Cammalleri — maurizio.cammalleri@gmail.com

---

## Contributing

Issues and pull requests are welcome. Two things make a change easy to accept:

1. **Say how you know.** A number, a comparison, a test that fails before and
   passes after. "Sounds better" is a real argument, but say what you listened
   to and against what.
2. **Run the guards.** `saf_regression` and `patch_fixture` before and after. If
   a fixture moved, that is not a failure — it is a question, and the answer
   belongs in the pull request.
