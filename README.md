# Sedai Audio Foundation

[![Development](https://img.shields.io/badge/status-development-orange.svg)]()
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-Maurizio%20Cammalleri-0077B5?logo=linkedin)](https://www.linkedin.com/in/maurizio-cammalleri-80a89a11/)
[![Substack](https://img.shields.io/badge/Substack-Maurizio%20Cammalleri-FF6719?logo=substack)](https://cammalleri.substack.com/)

A professional-grade, modular audio synthesis and DAW foundation library written in Free Pascal.

> **DISCLAIMER**: This library is in advanced development stage but **not yet ready for production use**. The API is stabilizing, most features are complete and functional, but there may be bugs. We encourage you to try it out and report any issues or suggestions!

## Branch Information

| Branch | Status | Description |
|--------|--------|-------------|
| **main** | [![Stable](https://img.shields.io/badge/status-stable-green.svg)]() | Preliminary release stable enough for testing |
| **develop** | [![Development](https://img.shields.io/badge/status-development-orange.svg)]() | Active development branch - compiles successfully but may contain bugs |

> **Recommended:** Use the `main` branch for testing and benchmarking. The `develop` branch contains the latest features but may have stability issues.

---

## Overview

Sedai Audio Foundation provides a comprehensive audio synthesis framework following a **modular building-block architecture**: small, well-defined classes that can be combined to build any type of synthesizer, effect processor, or DAW component.

### Key Features

- **Synthesis Engines**: Classic/Subtractive, FM (DX7-style) and Wavetable are exposed through the high-level facade (`TSAFSynthType`); Additive (`SedaiAdditiveGenerator`) and SID Evo (`SedaiSIDEvo`) are available as standalone units
- **Professional Mixer**: Channels, aux buses, group buses, master bus with metering
- **Audio Effects**: Delay, Reverb, Chorus, Flanger, Phaser, Distortion, Compressor, Limiter, EQ
- **Advanced Filters**: 6 filter types, multi-pole cascading (12/24/48 dB/oct)
- **DAW Foundation**: Transport-driven render, audio + MIDI tracks (MIDI tracks play through a per-track instrument), clips, automation, audio recording, working undo/redo, and project save/load (native `.safproj` text format, with a format-dispatch seam for SMF / Dawproject / etc.)
- **Real-time MIDI Playback**: Standard MIDI file support with 16-channel polyphony
- **GoatTracker Player**: Native playback of GoatTracker v2 .sng files with full command support
- **Audio File I/O**: WAV read/write (8/16/24/32-bit PCM, 32/64-bit float) with professional dithering
- **40+ Built-in Presets**: Ready-to-use sounds for all synthesis types
- **Cross-Platform**: Works on Linux and Windows via SDL2

---

## Design Principles

1. **Single Responsibility**: Each class does ONE thing well
2. **Composition over Inheritance**: Build complex systems by combining simple blocks
3. **Separation of Concerns**: Audio generation, processing, control, and routing are separate layers
4. **DAW-Ready**: Timeline, automation, undo/redo, and project management built-in
5. **Real-time Safe**: No allocations in audio thread, lock-free where possible
6. **Testable**: Every component can be tested in isolation

---

## Technologies

### Core Technologies

| Technology | Version | Purpose |
|------------|---------|---------|
| **Free Pascal** | 3.0.4+ | Primary programming language |
| **SDL2** | 2.0+ | Cross-platform audio device access (audio output only) |
| **Lazarus** | Optional | IDE support |

### Platform Abstraction

| Module | Purpose |
|--------|---------|
| **SedaiTiming** | Cross-platform high-precision timing (µs accuracy) |
| **SedaiThread** | Cross-platform thread management with priority control |

SDL2 is now used **only for audio hardware output**. All timing and threading is handled by platform-native APIs for maximum precision.

### Audio Technologies

| Component | Technology |
|-----------|------------|
| **Sample Rate** | 44100 Hz (CD quality) |
| **Bit Depth** | 32-bit floating point internal processing |
| **Buffer Size** | 1024 samples (~23ms latency) |
| **Voice Polyphony** | 32 default (unlimited, hardware-dependent) |

### Synthesis Technologies

| Engine | Description |
|--------|-------------|
| **Classic/Subtractive** | Analog-style synthesis with oscillators, filters, LFO |
| **FM (Frequency Modulation)** | DX7-style 6-operator FM synthesis |
| **Wavetable** | Modern wavetable synthesis with morphing |
| **Additive** | Harmonic spectrum synthesis |
| **SID Evo** | C64 SID-inspired with 12 waveforms (4 classic + 8 extended) |

### Tracker Formats

| Format | Description |
|--------|-------------|
| **GoatTracker v2** | Native .sng file playback with wavetable commands |

### Signal Processing

| Technology | Description |
|------------|-------------|
| **ADSR Envelopes** | 4-stage envelopes with 4 curve types (Linear, Exponential, Logarithmic, S-Curve) |
| **Biquad Filters** | Low-pass, High-pass, Band-pass, Notch, Allpass, Peaking |
| **Multi-pole Filters** | 12dB, 24dB, 48dB per octave cascaded filters |
| **Audio Effects** | Delay, Reverb, Chorus, Flanger, Phaser, Distortion, Compressor, Limiter, EQ |
| **Stereo Processing** | Panning, stereo width control |

### MIDI Technologies

| Component | Description |
|-----------|-------------|
| **MIDI Parser** | Standard MIDI File (SMF) Format 0 and 1 |
| **MIDI Sequencer** | Real-time event scheduling with tempo control |
| **Channel Support** | 16 MIDI channels with per-channel configuration |
| **Controllers** | Pitch bend, modulation wheel, velocity |

### Wavetable Formats

| Format | Extension | Description |
|--------|-----------|-------------|
| **Serum** | .wav | 2048 samples per frame, industry standard |
| **Vital** | .wav | Serum-compatible format |
| **Surge/SurgeXT** | .wt | Native Surge wavetable format |
| **Generic WAV** | .wav | Standard audio files |

### Platforms

| Platform | Compiler Target | Status |
|----------|-----------------|--------|
| **Windows 10/11** | x86_64-win64, i386-win32 | Supported |
| **Linux** | x86_64-linux, i386-linux | Supported |
| **macOS** | x86_64-darwin, aarch64-darwin | Planned |

---

## Architecture

### Layer Structure

The library follows a modular, layered architecture designed for maximum flexibility and code reuse:

```
+-----------------------------------------------------------------------------+
|  Layer 5: Application                                                        |
|  DAW UI, Plugin Hosts, Demo Applications                                     |
+-----------------------------------------------------------------------------+
|  Layer 4: Project & Session                                                  |
|  TSedaiProject, TSedaiTrack, TSedaiClip, TSedaiAutomationLane               |
|  TSedaiUndoStack, TSedaiCommand                                              |
+-----------------------------------------------------------------------------+
|  Layer 3: Transport & Timing                                                 |
|  TSedaiTransport, TSedaiTempoMap, TSedaiTimePosition, TSedaiMetronome       |
+-----------------------------------------------------------------------------+
|  Layer 2: Synthesis & Processing                                             |
|  +------------------+------------------+------------------+                  |
|  | Generators       | Processors       | Effects          |                  |
|  | TSedaiOscillator | TSedaiFilter     | TSedaiChorus     |                  |
|  | TSedaiNoise      | TSedaiAmplifier  | TSedaiDelay      |                  |
|  | TSedaiWavetable  | TSedaiDistortion | TSedaiReverb     |                  |
|  | TSedaiSample     | TSedaiCompressor | TSedaiFlanger    |                  |
|  | TSedaiFMOperator | TSedaiLimiter    | TSedaiPhaser     |                  |
|  |                  | TSedaiEQ         |                  |                  |
|  +------------------+------------------+------------------+                  |
|  +------------------+------------------+------------------+                  |
|  | Modulators       | Voice Management | Routing          |                  |
|  | TSedaiEnvelope   | TSedaiVoice      | TSedaiMixer      |                  |
|  | TSedaiLFO        | TSedaiVoiceMgr   | TSedaiModMatrix  |                  |
|  | TSedaiStepMod    |                  | TSedaiParamPort  |                  |
|  +------------------+------------------+------------------+                  |
+-----------------------------------------------------------------------------+
|  Layer 1: Foundation                                                         |
|  TSedaiAudioObject, TSedaiSignalNode, TSedaiAudioBuffer                     |
+-----------------------------------------------------------------------------+
|  Layer 0: Platform                                                           |
|  TSedaiAudioBackend (SDL2), TSedaiTiming, TSedaiThread                      |
+-----------------------------------------------------------------------------+
```

### Key Architectural Components

#### DAG-Based Effect Routing

The library provides a set of composable effect processors, all derived from
the `TSedaiEffect` base class in `src/Effects/`: `TSedaiReverb`, `TSedaiDelay`,
`TSedaiChorus`, `TSedaiFlanger`, `TSedaiPhaser`. You chain them by passing the
audio buffer through each processor in turn.

> **Note:** an earlier design (see `ARCHITECTURE_PROPOSAL.md`, kept as a
> historical document) proposed a DAG-based `TSedaiEffectGraph` and a
> `TSedaiSynthEngine` / `TSedaiVoicePool` / `TSedaiMultiPoleFilter` base
> hierarchy. These classes are **not** part of the current public API; voice
> management lives inside each synth/player (e.g. `TSedaiSIDEvo`,
> `TSedaiVoiceManager`).

#### Modular Synth Engine (Part / Instrument)

The high-level facade (`SedaiAudioFoundation`, the `Play*`/`MIDI*` API) is **not** a
self-contained voice loop — it delegates to a modular **Part/Instrument engine** built
from the framework's own units:

```
TSAFEngine
 ├─ Part = Instrument(preset) → TSedaiVoiceManager (universal voices) → Mixer channel
 ├─ Part ...                                                                  ↓
 └─ ...                                                              MasterBus → output
```

- **`TSedaiVoice`** is a *universal* voice: a single voice can be an oscillator stack,
  an FM synth, or a wavetable generator (`TVoiceSourceType`), with a shared envelope /
  filter / amp / pan chain and a per-voice **modulation matrix** (`TSedaiModulationMatrix`)
  routing envelopes / LFOs / velocity / key-track to pitch, cutoff and amplitude.
- **`TSAFPart`** (`src/Engine/SedaiPart.pas`) is a monotimbral instrument: a
  `TSedaiVoiceManager` pool configured from a preset (classic / FM / wavetable, or a
  loaded wavetable table), rendered to a stereo buffer.
- **`TSAFEngine`** (`src/Engine/SedaiEngine.pas`) hosts many Parts, one per mixer channel,
  summed through the `TSedaiMixer` master bus. The facade drives a global `TSAFEngine`;
  the audio backend runs in callback mode and pulls the engine render.

#### Professional Mixer Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│  Mixer Architecture                                              │
│                                                                  │
│  Channels (N) ──┬── Aux Sends (8) ──── Aux Buses (8) ──┐        │
│                 │                                       │        │
│                 └── Groups (8) ─────────────────────────┼──┐     │
│                                                         │  │     │
│                                                         ▼  ▼     │
│                                              ┌────────────────┐  │
│                                              │  Master Bus    │  │
│                                              │  (limiter,     │  │
│                                              │   metering)    │  │
│                                              └───────┬────────┘  │
│                                                      │           │
│                                                      ▼           │
│                                               Audio Output       │
└─────────────────────────────────────────────────────────────────┘
```

### Module Overview

#### Foundation Layer

| Module | Description |
|--------|-------------|
| **SedaiAudioTypes** | Core types: TWaveType, TADSR, TStereoSample, TFilterType |
| **SedaiAudioObject** | Root base class for all audio objects |
| **SedaiSignalNode** | Abstract DSP node for graph-based processing |
| **SedaiAudioBuffer** | Managed audio buffer with utility methods |
| **SedaiParameterPort** | Modulatable parameter with range and curve |

#### Platform Layer

| Module | Description |
|--------|-------------|
| **SedaiAudioBackend** | SDL2 audio device with dual-mode support (Push or Callback) |
| **SedaiTiming** | Cross-platform high-precision timing abstraction |
| **SedaiThread** | Cross-platform thread abstraction with priority control |

**SedaiAudioBackend Modes**:
- **Push Mode** (default): Application pushes samples via `QueueSamples()` - you control timing
- **Callback Mode**: SDL2 pulls audio via callback - SDL2 controls timing

#### Generators

| Module | Description |
|--------|-------------|
| **SedaiOscillator** | Multi-waveform oscillator (PolyBLEP, SID mode) |
| **SedaiNoiseGenerator** | Noise generators (White, Pink, Brown, Blue, Violet) |
| **SedaiWavetableGenerator** | Wavetable synthesis with morphing & mipmaps |
| **SedaiSamplePlayer** | Sample playback (loop modes, pitch, interpolation) |
| **SedaiFMOperator** | FM synthesis (6-operator DX7-style, algorithms) |
| **SedaiAdditiveGenerator** | Additive synthesis (up to 64 harmonics, 10 presets) |

#### Modulators

| Module | Description |
|--------|-------------|
| **SedaiEnvelope** | ADSR envelope (4 curves, SID mode) |
| **SedaiLFO** | LFO with tempo sync |
| **SedaiStepModulator** | Step sequencer modulator |

#### Processors

| Module | Description |
|--------|-------------|
| **SedaiFilter** | Multi-mode filter (6 types, 3 slopes, SID mode) |
| **SedaiAmplifier** | Gain stage with saturation modes |
| **SedaiDistortion** | Multi-type distortion |
| **SedaiCompressor** | Dynamics compressor |
| **SedaiLimiter** | Brickwall limiter with lookahead |
| **SedaiEQ** | 8-band parametric EQ |

#### Effects

| Module | Description |
|--------|-------------|
| **SedaiEffect** | Effect base class (tempo sync, helpers) |
| **SedaiChorus** | Multi-voice chorus |
| **SedaiDelay** | Multi-mode delay (Simple, PingPong, MultiTap, Tape, BBD) |
| **SedaiReverb** | Freeverb algorithm |
| **SedaiFlanger** | Flanger effect |
| **SedaiPhaser** | Multi-stage phaser |

#### Voice Management

| Module | Description |
|--------|-------------|
| **SedaiVoice** | Complete synthesizer voice |
| **SedaiVoiceManager** | Polyphonic voice allocation |
| **SedaiModulationMatrix** | Modulation routing |

#### Mixer

| Module | Description |
|--------|-------------|
| **SedaiMixerChannel** | Mixer channel with inserts and sends |
| **SedaiBus** | Aux, Group, and Master bus classes |
| **SedaiMixer** | Full mixing console |

#### Transport & DAW

| Module | Description |
|--------|-------------|
| **SedaiTransport** | Transport control (play, stop, record, loop, tempo, SMPTE) |
| **SedaiClip** | Audio and MIDI clip classes |
| **SedaiTrack** | Track with clips, automation, routing |
| **SedaiProject** | Complete DAW project with undo/redo |

#### SID Synthesis

| Module | Description |
|--------|-------------|
| **SedaiSIDEvo** | MOS 6581/8580 SID emulator — a bit-exact reSID port (classic path) plus EVO extensions (up to 64 voices, stereo, LFO, extended waveforms) |

#### Players

| Module | Description |
|--------|-------------|
| **SedaiGoatTracker** | GoatTracker v2 .sng parser and player; byte-exact SID register output vs. the original player |
| **SedaiMIDIPlayer** | Standard MIDI file player |

#### File I/O

| Module | Description |
|--------|-------------|
| **SedaiAudioFileReader** | Audio file loading (WAV PCM/Float) |
| **SedaiAudioFileWriter** | Audio file export with dithering |

---

## Synthesis Engines

### Classic (Subtractive) Synthesis

- Up to 3 oscillators per voice
- 5 waveform types: Sine, Square, Sawtooth, Triangle, Noise
- Subtractive filter with resonance
- LFO modulation (pitch, filter, amplitude)
- Per-oscillator ADSR envelopes

**Presets**: sine, square, saw, triangle, lead, bass, pad, strings, brass, organ, pluck, synthkeys, warmbass

### FM Synthesis

- 6 operators (DX7-style)
- 5 algorithms: Simple, Stack, Parallel, Feedback, Complex
- Per-operator ADSR envelopes
- Operator feedback control
- Modulation depth scaling

**Presets**: epiano, brass, bell, organ, lead, bass, choir, fmstrings, fmpad, marimba, flute, churchbell

### Wavetable Synthesis

- Up to 4 oscillators per voice
- Real-time wavetable morphing with interpolation
- Multiple mix modes: Add, Multiply, Ring Modulation
- Support for external wavetable formats

**Presets**: serum, wasp, ppg, vocal, metallic, glass, organ, evolving, digitalchaos

**Supported Wavetable Formats**: Serum (.wav, 2048 samples/frame), Vital (.wav), Surge/SurgeXT (.wt), Generic WAV

### Additive Synthesis

Additive synthesis builds complex timbres by combining multiple sine wave oscillators (harmonics/partials). Each harmonic can be individually controlled for amplitude, phase, and detuning.

#### Features

| Feature | Description |
|---------|-------------|
| **Max Harmonics** | Up to 64 harmonics per voice |
| **Per-harmonic Level** | Individual amplitude control (0.0-1.0) |
| **Per-harmonic Detune** | Cents-based detuning for inharmonicity |
| **Per-harmonic Phase** | Initial phase offset (0-2pi) |
| **Per-harmonic Envelopes** | Optional ADSR envelope per harmonic |
| **Automatic Nyquist Limiting** | Prevents aliasing by limiting harmonics |

#### Preset Waveforms (10)

| Preset | Description |
|--------|-------------|
| **Sine** | Pure sine wave (fundamental only) |
| **Saw** | Sawtooth wave (all harmonics at 1/n amplitude) |
| **Square** | Square wave (odd harmonics at 1/n amplitude) |
| **Triangle** | Triangle wave (odd harmonics at 1/n^2 amplitude) |
| **Organ** | Hammond-style drawbar simulation |
| **Bell** | Inharmonic partials for metallic sounds |
| **Strings** | Rich harmonics with gradual rolloff |
| **Choir** | Formant-like harmonic structure |
| **Brass** | Strong mid-harmonics for brass tones |
| **Flute** | Mostly fundamental with weak harmonics |

#### Classic Waveform Formulas

```
Sine:     Only fundamental (no harmonics)
Square:   Odd harmonics (1,3,5,7...) at 1/n amplitude
Sawtooth: All harmonics (1,2,3,4...) at 1/n amplitude
Triangle: Odd harmonics at 1/n^2 amplitude with alternating phase
```

#### Usage Example

```pascal
uses
  SedaiAdditiveGenerator;

var
  Additive: TSedaiAdditiveGenerator;
begin
  Additive := TSedaiAdditiveGenerator.Create;
  Additive.SetSampleRate(44100);

  // Load a preset
  Additive.LoadOrganWave;

  // Or create custom spectrum
  Additive.ClearAllHarmonics;
  Additive.SetHarmonicLevel(0, 1.0);   // Fundamental
  Additive.SetHarmonicLevel(2, 0.5);   // 3rd harmonic
  Additive.SetHarmonicDetune(2, 5);    // +5 cents detune
  Additive.SetHarmonicLevel(4, 0.3);   // 5th harmonic

  // Play a note
  Additive.NoteOn(440.0, 0.8);  // A4 at 80% velocity

  // Generate samples
  while Playing do
    Sample := Additive.GenerateSample;

  Additive.NoteOff;
  Additive.Free;
end.
```

### SID Evo Synthesis

A Commodore 64 SID emulator with extended capabilities. The classic emulation path is a
**bit-exact port of reSID** (cycle-accurate MOS 6581/8580), verified sample-for-sample
against the reference engine; on top of it the "Evo" layer adds modern waveforms and
features (up to 64 voices, stereo, per-voice LFO, 3D positioning) for creative
flexibility. It therefore serves both as a faithful chiptune engine for `.sng`/`.sid`
playback and as a standalone synthesizer.

#### Classic SID Waveforms (4)

| Flag | Waveform | Description |
|------|----------|-------------|
| `$01` | **Triangle** | Classic mellow tone |
| `$02` | **Sawtooth** | Rich harmonics, brassy sound |
| `$04` | **Pulse** | Square/PWM, classic chiptune |
| `$08` | **Noise** | White noise for percussion |

#### Extended "Evo" Waveforms (8)

| Flag | Waveform | Description |
|------|----------|-------------|
| `$10` | **Sine** | Pure sine wave |
| `$20` | **Supersaw** | Multiple detuned saws (trance/EDM) |
| `$40` | **PWM** | Auto-modulating pulse width |
| `$80` | **Half-Sine** | Half-rectified sine (bass) |
| `$0100` | **Full-Sine** | Full-rectified sine (warm distortion) |
| `$0200` | **Formant** | Vocal-like formant synthesis |
| `$0400` | **Metallic** | Metallic/bell-like tones |
| `$8000` | **Custom** | User-defined wavetable |

#### ReSID-Accurate Features

The classic emulation path is a faithful port of **reSID** (Dag Lem's cycle-accurate
MOS 6581/8580 engine). Every core building block reproduces reSID's integer model
exactly:

| Feature | Description |
|---------|-------------|
| **24-bit Accumulator** | Authentic phase accumulator; FREQ added to the low 16 bits each cycle |
| **Hard Sync** | Oscillator reset on the source MSB rising edge (reSID `synchronize`) |
| **Ring Modulation** | Triangle MSB EOR'd with the sync source MSB |
| **Combined Waveforms** | OSC3 sample lookup tables for SAW+TRI / PUL+TRI / PUL+SAW / PUL+SAW+TRI |
| **23-bit LFSR Noise** | Authentic noise generator (taps 22 EOR 17), clocked by accumulator bit 19 |
| **15-bit Rate Counter** | With double-increment wraparound at $8000 (the ADSR delay bug) |
| **Exponential Counter** | Period breakpoints at envelope levels 255, 93, 54, 26, 14, 6, 0 |
| **Hold-Zero** | Envelope frozen when it reaches zero |
| **Two-Integrator-Loop Filter** | Authentic 6581/8580 topology; cutoff from the reSID cubic-spline `f0` tables (both chip models), resonance `1024/(0.707 + res/15)`, `mixer_DC` and DAC offsets |
| **External Filter** | C64 output RC stage (low-pass 1/RC = 100000, high-pass 1/RC = 100) |

##### Verified Bit-Exact

The classic path has been verified **bit-for-bit against reSID** by driving both
engines from the same per-cycle register stream and comparing the raw output:

- **Full mixed output**: 0 sample differences over **15.76 million cycles** of a real
  tune (oscillators + envelopes + filter + external filter + mixer), including frames
  that use hard sync and ring modulation.
- **Cycle-exact sub-systems**: all 4 classic + the combined waveforms, the noise LFSR,
  hard sync, ring modulation and the full ADSR envelope (every Attack/Decay/Sustain/
  Release setting) reproduce reSID's `readOSC`/`readENV`/`output()` with zero deviation.

> `SedaiSIDEvo` is a Pascal **port of reSID** (and reSID-fp for the optional distortion
> model), partially built on the SAF framework. As a derivative work of reSID it is
> **GPL-2.0-or-later, GPL-only** — the project's dual "GPL-3.0 OR Commercial" license does
> **not** apply to the SID engine. Credit and thanks to Dag Lem (reSID) and Antti S.
> Lankila (reSID-fp). See the [License](#license) section for details.

#### Sampling Methods

The output stage offers the same three decimation strategies as reSID, selectable at
runtime via `SetSamplingMethod`:

| Method | Constant | Description |
|--------|----------|-------------|
| **Fast** | `ssmFast` | Point sampling (aliases high harmonics, exactly like reSID `SAMPLE_FAST`). **Default** — this is what GoatTracker uses, so it is the closest match for tracker playback. |
| **Interpolate** | `ssmInterpolate` | Linear interpolation between the two bracketing cycles (reSID `SAMPLE_INTERPOLATE`); cheap, no FIR latency. |
| **Resample** | `ssmResample` | Band-limited Kaiser-windowed sinc FIR resampling (reSID `SAMPLE_RESAMPLE_INTERPOLATE`); cleanest output, heaviest CPU. Use for hi-fi offline rendering. |

#### Filter Models

| Model | Constant | Description |
|-------|----------|-------------|
| **Classic** | `sfmClassic` | reSID two-integrator-loop (linear). **Default**, bit-exact to reSID. |
| **Distortion** | `sfmDistortion` | reSID-fp non-linear 6581 filter (signal-dependent cutoff via a kinked DAC + Schraudolph fast-exp + output waveshaper). Reproduces the real 6581's "warm" filter distortion that the linear model lacks. Single-precision, GoatTracker filter parameters. |

#### SID Models

| Model | Characteristics |
|-------|-----------------|
| **6581** | Original SID, darker filter, more distortion |
| **8580** | Revised SID, cleaner filter, less "gritty" |

#### Authenticity Levels

| Level | Constant | Description |
|-------|----------|-------------|
| **Evolved** | `salEvolved` | Clean hi-fi sound, modern synthesis |
| **Hybrid** | `salHybrid` | Mix of evolved and SID characteristics |
| **SID-Like** | `salSIDLike` | More authentic for casual playback |
| **SID Full** | `salSIDFull` | Maximum authenticity (for .sng playback) |

#### EVO Extended Features

The "Evo" layer turns the bit-exact SID into a polyphonic stereo synth. These
features are active under the `salEvolved` / `salHybrid` authenticity levels
(the extended voices 3+ and the stereo mix). Under `salSIDFull` you get the pure
3-voice reSID core (what the GoatTracker player uses, bit-exact).

| Feature | Status | Description |
|---------|--------|-------------|
| **8–64 voices** | ✅ | Configurable in groups of 8 (`Initialize(groups)`) |
| **11 octaves** | ✅ | C0–B10 (≈16 Hz–20 kHz) |
| **Extended waveforms** | ✅ | Sine, Supersaw, PWM, Half-Sine (Full-Sine / Formant / Metallic flags reserved) |
| **Stereo output** | ✅ | Per-voice and per-group pan + stereo width |
| **Per-voice LFO** | ✅ | Vibrato (pitch), PWM, tremolo (amplitude), auto-pan — on the extended voices |
| **Global LFO** | ✅ | Tremolo + auto-pan over the whole mix |
| **Mixer headroom** | ✅ | Soft-clip on the summed EVO mix (smooth knee, no harsh clipping) |

> Notes: the per-voice LFO targets the *extended* voices (the 3 classic reSID
> core voices stay cycle-accurate and are not modulated). A cutoff LFO does not
> apply to extended voices, which bypass the SID filter. Per-voice continuous
> level/pan are EVO additions; the chip's authentic volume is the 4-bit `$D418`
> register (`SetMasterVolume` writes it).

**Common to both modes**:
- 4 classic SID waveforms combinable via OR bitmask (like the original SID)
- ADSR envelopes, pulse-width modulation, hard restart
- 6581 / 8580 chip models

#### Usage Example

```pascal
uses
  SedaiSIDEvo;

var
  SIDEvo: TSedaiSIDEvo;
begin
  SIDEvo := TSedaiSIDEvo.Create;
  SIDEvo.Initialize(1);  // 1 group = 8 voices

  // Set maximum SID authenticity for .sng playback
  SIDEvo.SetAuthenticityLevel(salSIDFull);
  SIDEvo.SetClockMode(scmPAL);
  SIDEvo.SetChipModel(smMOS6581);

  // Use SID register API
  SIDEvo.SetFrequencyReg(0, $1CD6);  // Voice 0 frequency
  SIDEvo.SetWaveform(0, SIDEVO_WAVE_TRIANGLE);
  SIDEvo.SetADSR(0, 0.0, 0.1, 0.8, 0.3);
  SIDEvo.GateOn(0);

  // Or use evolved API
  SIDEvo.PlayNote(1, 69, 0.8);  // Voice 1, A4, velocity 0.8

  SIDEvo.Free;
end.
```

---

## GoatTracker Player

Native playback support for GoatTracker v2 .sng files, the popular Commodore 64 music tracker.
`SedaiGoatTracker` is a clean Pascal reimplementation of the GoatTracker v2 player routine
(`gplay.c` / `gsid.cpp` / `gsound.c`).

### Fidelity

The player's SID register output has been verified **byte-for-byte against the real
GoatTracker player** (relocated to `.sid` and dumped with a 6502 + reSID reference): the
control/gate, ADSR, frequency and filter register streams match with **zero divergences**
on all three voices. The reimplementation faithfully reproduces:

- the exact SID register write order (`sidorder`) and inter-write delay (`SIDWRITEDELAY = 14` cycles);
- the hard-restart behaviour (`adparam = $0F00`, gate-timer bits `$40`/`$80`);
- real-time effect skipping (`optimizerealtime` — per-frame commands run only on non-tick-0 frames);
- **pulse skipping** (`optimizepulse` — the pulse table is not advanced on the new-note frame), so the pulse-width register tracks the original player to within 1 LSB;
- 8-bit `vibtime` wraparound and the fine-vibrato speed-table semantics;
- wavetable / pulsetable / filtertable / speedtable processing and the note frequency table.

Combined with the bit-exact SID core, `sng_player` is **audibly indistinguishable from
GoatTracker** for the classic (default) configuration.

### Supported Features

- Full song structure (orderlist, patterns, instruments, wavetables)
- 3-channel playback with per-channel mute control
- Loop detection and seamless looping
- Real-time tempo control
- Multiple subtunes

### Pattern Commands

| Command | Name | Description |
|---------|------|-------------|
| `$01` | Portamento Up | Slide pitch up |
| `$02` | Portamento Down | Slide pitch down |
| `$03` | Tone Portamento | Slide to target note |
| `$04` | Vibrato | Pitch vibrato effect |
| `$05` | Attack/Decay | Set ADSR attack/decay |
| `$06` | Sustain/Release | Set ADSR sustain/release |
| `$07` | Waveform | Set voice waveform |
| `$08` | Wavetable Pointer | Set wavetable start position |
| `$09` | Pulse Width | Set pulse width |
| `$0A` | Pulse Width Modulation | Modulate pulse width |
| `$0B` | Filter Control | Filter on/off and settings |
| `$0C` | Filter Cutoff | Set filter cutoff frequency |
| `$0D` | Master Volume | Set global volume |
| `$0E` | Funk Tempo | Set funk-style tempo |
| `$0F` | Tempo | Set playback speed |

### Wavetable Commands

| Command | Name | Description |
|---------|------|-------------|
| `$F0` | Portamento Up | Continuous pitch slide up |
| `$F1` | Portamento Down | Continuous pitch slide down |
| `$F2` | Tone Portamento | Slide to target note |
| `$F3` | Vibrato | Apply vibrato effect |
| `$F4` | Set Attack/Decay | Set ADSR AD values |
| `$F5` | Set Sustain/Release | Set ADSR SR values |
| `$F6` | Set Waveform | Change voice waveform |
| `$F7` | Delay | Wait N frames |
| `$F8` | (Reserved) | - |
| `$F9` | (Reserved) | - |
| `$FA` | Set Vibrato Params | Configure vibrato depth/speed |
| `$FB` | Goto | Jump to wavetable position |
| `$FC` | End | Stop wavetable processing |
| `$FD` | Set Master Volume | Change global volume |
| `$FE` | (Reserved) | - |

### Running

```
sng_player [--sdl2] <file.sng> [subtune]
```

By default audio is rendered through the Sedai Audio Foundation (SAF) backend; pass
`--sdl2` to use direct SDL2 output (compatibility/debugging). The optional trailing
number selects the initial subtune.

### Demo Player Controls

```
SPACE    - Pause/Resume playback
R        - Restart song from beginning
I        - Show song info
L        - Toggle loop mode
A        - Cycle SID sampling: Fast -> Interpolate -> Resample
B        - Cycle audio buffer size: 2048 -> 4096 -> 8192 (latency vs. dropouts)
D        - Toggle filter model: Classic (clean) <-> Distortion (reSID-fp)
V        - Toggle pattern command verbose output
W        - Toggle wavetable command verbose output
S        - Print SpeedTable contents
1/2/3    - Mute/unmute voice 1/2/3
+/-      - Next/Previous subtune
ESC / Q  - Quit player
```

### Usage Example

```pascal
program GoatTrackerDemo;

uses
  SedaiGoatTracker;

var
  Player: TSedaiGoatTracker;
  Buffer: array[0..2047] of SmallInt;  // mono 16-bit
begin
  // The player creates and owns its own SID core internally.
  Player := TSedaiGoatTracker.Create;
  try
    Player.SetSampleRate(44100);

    if Player.LoadFromFile('mysong.sng') then
    begin
      WriteLn('Title: ', Player.SongName, ' / ', Player.Author);
      Player.Play(0);  // start subtune 0

      // FillBuffer generates audio AND drives the player routine at the
      // correct rate internally (no separate per-frame call needed).
      // In a real app you call this from your audio callback.
      while not Player.SongFinished do
        Player.FillBuffer(@Buffer[0], Length(Buffer));

      Player.Stop;
    end;
  finally
    Player.Free;
  end;
end.
```

---

## MIDI Support

- Standard MIDI file format 0/1 parsing
- 16-channel support with per-channel configuration
- Real-time tempo control
- Pitch bend and modulation wheel support
- Velocity-sensitive playback
- General MIDI program mapping to wavetable types

---

## Audio File I/O

The library includes professional audio file reading and writing capabilities.

### Supported Formats

| Format | Read | Write | Notes |
|--------|------|-------|-------|
| **WAV PCM 8-bit** | ✓ | ✓ | Unsigned, 128 = silence |
| **WAV PCM 16-bit** | ✓ | ✓ | Standard CD quality |
| **WAV PCM 24-bit** | ✓ | ✓ | Professional audio |
| **WAV PCM 32-bit** | ✓ | ✓ | Maximum PCM resolution |
| **WAV Float 32-bit** | ✓ | ✓ | IEEE 754 floating point |
| **WAV Float 64-bit** | ✓ | - | High-precision source files |
| **OGG Vorbis** | Planned | Planned | - |
| **FLAC** | Planned | Planned | - |
| **AIFF** | Planned | - | - |
| **MP3** | Planned | - | Decode only |

### Dithering Options

When exporting to lower bit depths, professional dithering is available:

| Dither Type | Description |
|-------------|-------------|
| **None** | No dithering (truncation) |
| **RPDF** | Rectangular probability density function |
| **TPDF** | Triangular probability density function (recommended) |
| **Noise Shaped** | First-order noise shaping for minimum audible artifacts |

### Usage Example

```pascal
uses
  SedaiAudioFileReader, SedaiAudioFileWriter, SedaiAudioBuffer;

var
  Reader: TSedaiAudioFileReader;
  Writer: TSedaiAudioFileWriter;
  Buffer: TSedaiAudioBuffer;
  Settings: TAudioExportSettings;
begin
  // Read a WAV file
  Reader := TSedaiAudioFileReader.Create;
  if Reader.OpenFile('input.wav') then
  begin
    WriteLn('Sample Rate: ', Reader.Info.SampleRate);
    WriteLn('Channels: ', Reader.Info.Channels);
    WriteLn('Duration: ', Reader.Info.Duration:0:2, ' seconds');

    // Read entire file into buffer
    if Reader.ReadAll(Buffer) then
    begin
      // Process buffer...
      Buffer.Normalize(0.99);

      // Export to 16-bit WAV with TPDF dithering
      Settings := TSedaiAudioFileWriter.GetDefaultSettings(aefWAV16);
      Settings.DitherType := dtTPDF;

      Writer := TSedaiAudioFileWriter.Create;
      if Writer.CreateFile('output.wav', Settings) then
        Writer.WriteBuffer(Buffer);
      Writer.Free;

      Buffer.Free;
    end;
    Reader.Close;
  end;
  Reader.Free;
end.
```

---

## System Requirements

### Compiler

- **Free Pascal** 3.0.4 or higher
- **Lazarus** (optional, for IDE support)

### Dependencies

- **SDL2** (Simple DirectMedia Layer 2)

### Operating Systems

- Linux (Ubuntu, Debian, Fedora, Arch)
- Windows 10/11

---

## Installation

### Windows (Recommended)

The project includes an automated setup script that handles all dependencies:

```powershell
# Run the setup script (requires Administrator privileges for some operations)
.\setup.ps1
```

The setup script will:
- Detect or install Free Pascal Compiler (FPC)
- Download and configure SDL2 libraries
- Set up the project directory structure
- Verify the installation

For manual installation or troubleshooting, run:

```powershell
.\setup.ps1 -Help
```

### Linux (Debian/Ubuntu)

```bash
# Install Free Pascal and SDL2
sudo apt install fpc fp-units-base fp-units-rtl libsdl2-dev

# Clone the repository and build
git clone <repository-url>
cd SedaiAudioFoundation
chmod +x build.sh
./build.sh
```

### Linux (Fedora/RHEL)

```bash
sudo dnf install fpc SDL2-devel
```

### Linux (Arch)

```bash
sudo pacman -S fpc sdl2
```

### Manual Windows Installation

If you prefer manual installation:

1. **Free Pascal Compiler**: Download and install from [freepascal.org](https://www.freepascal.org/download.html)
2. **SDL2 Runtime**: Download `SDL2.dll` from [GitHub SDL Releases](https://github.com/libsdl-org/SDL/releases)
3. **SDL2 Headers**: The Pascal SDL2 bindings are included in the project
4. Place `SDL2.dll` in the project `bin\` directory or system PATH

---

## Building

### Using Build Scripts

The project includes cross-platform build scripts that support custom compiler paths.

#### Linux

```bash
# Make script executable (first time only)
chmod +x build.sh

# Build with system FPC
./build.sh

# Build with custom FPC path
./build.sh --fpc /path/to/fpc

# Build only demos
./build.sh --demos-only

# Clean build artifacts
./build.sh --clean

# Show help
./build.sh --help
```

#### Windows (PowerShell)

```powershell
# Build with system FPC
.\build.ps1

# Build with custom FPC path
.\build.ps1 -FpcPath "C:\FPC\3.2.2\bin\i386-win32\fpc.exe"

# Build specific target
.\build.ps1 -Target sng_player

# Clean build artifacts
.\build.ps1 -Clean

# Build with debug symbols
.\build.ps1 -Debug

# Show help
.\build.ps1 -Help
```

### Build Targets

| Target | Description |
|--------|-------------|
| `test_saf_main` | Main SAF API test (Classic, FM, Wavetable synthesis) |
| `demo_synth` | Synth demo |
| `sng_player` | GoatTracker .sng player |
| `sng_dump` | Per-frame SID register dump tool (for diagnostics / reference comparison) |
| `audiotest` | Audio backend test |
| `sedaisid_test` | SedaiSIDEvo verification / regression test |

### Building a single target

```powershell
.\build.ps1 -Target sng_player          # build just one target
.\build.ps1 -Target sng_player -Clean   # clean then build
```

Sources live under `test/` (`*.lpr`); the build script resolves each target to
its source file and emits the executable into `bin/<cpu>-<os>/`.

---

## Demo Programs

### audiotest

The main test program with a comprehensive menu system.

```bash
./audiotest
```

**Features**:
- Classic, FM, and Wavetable synthesis tests
- MIDI file loading and playback
- Channel mapping configuration
- Tempo control
- System status display

### demo_synth

Synthesis demonstration driving the SAF facade (Classic / FM / Wavetable).

```bash
./demo_synth
```

### test_saf_main

Exercises the main SAF API (Classic, FM and Wavetable synthesis paths).

```bash
./test_saf_main
```

### sedaisid_test

Verification/regression test for the `SedaiSIDEvo` SID emulation. Compares the
emulator state (accumulators, envelopes, output) against a reference dump.

```bash
./sedaisid_test
```

### sng_player

Interactive GoatTracker v2 .sng file player with SID Evo synthesis.

```bash
./sng_player mysong.sng           # Uses SAF audio backend (default)
./sng_player --sdl2 mysong.sng    # Uses direct SDL2 audio (fallback)
```

**Dual Audio Backend**:

The player supports two audio modes:

| Mode | Description |
|------|-------------|
| **SAF** (default) | Uses Sedai Audio Foundation backend - stereo 32-bit float output |
| **SDL2** (`--sdl2`) | Direct SDL2 audio - mono 16-bit output, useful for debugging/compatibility |

**Features**:
- Full GoatTracker song playback
- Dual audio backend (SAF or direct SDL2)
- Real-time channel muting (keys 1/2/3)
- Pause/resume and restart controls
- Loop mode toggle
- Verbose mode for debugging pattern and wavetable commands
- SpeedTable inspection

**Controls**:
- `SPACE` - Pause/Resume
- `R` - Restart song
- `L` - Toggle loop mode
- `V` - Toggle pattern verbose output
- `W` - Toggle wavetable verbose output
- `S` - Print SpeedTable
- `A` - Cycle SID sampling: Fast (GoatTracker) / Interpolate / Resample (VICE)
- `B` - Cycle audio buffer size: 2048 / 4096 / 8192 (latency vs dropout robustness)
- `D` - Toggle filter model: Classic (clean reSID) / Distortion (reSID-fp nonlinear 6581, VICE-like)
- `1`/`2`/`3` - Mute/unmute channels
- `+`/`-` - Next/Previous subtune
- `Q` or `ESC` - Quit

### sng_dump

SNG register dump tool for debugging and comparison with VICE/VSID.

```bash
./sng_dump mysong.sng [subtune] [output.dump]
```

**Features**:
- Dumps SID register state per frame
- Analyzes pattern and wavetable commands in song file
- Tracks volume/filter changes
- Output compatible with VICE dump format for comparison

---

## Quick Start

### Basic Usage

```pascal
program MyAudioApp;

uses
  SedaiAudioFoundation;

begin
  // Initialize audio system
  if InitAudio(32) then  // 32 voices
  begin
    SetMasterVolume(0.7);

    // Play a simple sine wave
    PlaySine(440.0);  // A4 note
    Sleep(1000);

    // Play a wavetable preset
    PlaySerum(261.63);  // C4 note
    Sleep(1000);

    // Play FM piano
    PlayEPiano(329.63);  // E4 note
    Sleep(1000);

    StopAll;
    ShutdownAudio;
  end;
end.
```

### MIDI Playback

```pascal
program MyMIDIPlayer;

uses
  SedaiMIDIPlayer;

var
  Player: TSedaiMIDIPlayer;  // object API (there is no global MIDI API)
begin
  Player := TSedaiMIDIPlayer.Create;
  try
    Player.SetSampleRate(44100);

    if Player.LoadFromFile('song.mid') then
    begin
      WriteLn('Title: ', Player.SongName,
              '  Duration: ', Player.GetDurationSeconds:0:1, 's');
      Player.Play;

      // Drive the sequencer from your audio callback by advancing it the
      // same number of samples you render each block:
      //   Player.AdvanceSamples(FrameCount);
      while Player.Playing do
        Player.AdvanceSamples(441);  // e.g. 10 ms blocks at 44.1 kHz

      Player.Stop;
    end;
  finally
    Player.Free;
  end;
end.
```

### Advanced Voice Control

```pascal
var
  VoiceIndex: Integer;
begin
  // Get voice index for advanced control
  VoiceIndex := PlayWavetableAdv(440.0, 'serum');

  if VoiceIndex >= 0 then
  begin
    SetVoicePan(VoiceIndex, -0.5);  // Pan left
    Sleep(2000);
    NoteOff(VoiceIndex);  // Release with ADSR
  end;
end;
```

---

## API Reference

### Audio Initialization

| Function | Description |
|----------|-------------|
| `InitAudio(VoiceCount)` | Initialize audio system with specified voice count |
| `ShutdownAudio` | Shutdown audio system |
| `SetMasterVolume(Volume)` | Set master volume (0.0-1.0) |
| `GetMasterVolume` | Get current master volume |
| `GetActiveVoices` | Get number of currently active voices |
| `GetMaxVoices` | Get maximum voice count |
| `GetSampleRate` | Get audio sample rate |
| `PrintStatus` | Print voice status information |

### Classic Synthesis

| Function | Description |
|----------|-------------|
| `PlaySine(Freq)` | Play sine wave |
| `PlaySquare(Freq)` | Play square wave |
| `PlaySaw(Freq)` | Play sawtooth wave |
| `PlayTriangle(Freq)` | Play triangle wave |
| `PlayLead(Freq)` | Play lead preset |
| `PlayBass(Freq)` | Play bass preset |
| `PlayPad(Freq)` | Play pad preset |
| `PlayClassic(Freq, Preset)` | Play classic preset |
| `PlayClassicAdv(Freq, Preset)` | Play and return voice index |

### FM Synthesis

| Function | Description |
|----------|-------------|
| `PlayEPiano(Freq)` | Play electric piano |
| `PlayFMBrass(Freq)` | Play FM brass |
| `PlayFMBell(Freq)` | Play FM bell |
| `PlayFMOrgan(Freq)` | Play FM organ |
| `PlayFMLead(Freq)` | Play FM lead |
| `PlayFMBass(Freq)` | Play FM bass |
| `PlayFM(Freq, Preset)` | Play FM preset |
| `PlayFMAdv(Freq, Preset)` | Play and return voice index |

### Wavetable Synthesis

| Function | Description |
|----------|-------------|
| `PlaySerum(Freq)` | Play Serum-style wavetable |
| `PlayWasp(Freq)` | Play WASP-style wavetable |
| `PlayPPG(Freq)` | Play PPG-style wavetable |
| `PlayWavetable(Freq, Type)` | Play wavetable preset |
| `PlayWavetableAdv(Freq, Type)` | Play and return voice index |

### Voice Control

| Function | Description |
|----------|-------------|
| `NoteOff(VoiceIndex)` | Release note (starts ADSR release) |
| `NoteRelease(VoiceIndex)` | Alternative release function |
| `SetVoicePan(VoiceIndex, Pan)` | Set voice pan (-1.0 to 1.0) |
| `StopAll` | Stop all voices immediately |
| `SmoothStopAll(FadeMs)` | Fade out all voices |

### MIDI Voice Management

| Function | Description |
|----------|-------------|
| `MidiAllocateVoice` | Allocate a MIDI voice, returns index or -1 |
| `MidiIsVoiceActive(Index)` | Check if voice is active |
| `MidiGetFreeVoiceCount` | Get number of available voices |
| `MidiVoiceOn(Index)` | Start playing allocated voice |
| `MidiVoiceOff(Index)` | Stop voice (ADSR release) |
| `MidiReleaseVoice(Index)` | Stop and deallocate voice |
| `MidiReleaseAllVoices` | Release all MIDI voices |

### MIDI Voice Configuration

| Function | Description |
|----------|-------------|
| `MidiSetVoiceFrequency(Index, Freq)` | Set voice frequency in Hz |
| `MidiSetVoiceAmplitude(Index, Amp)` | Set amplitude (0.0-1.0) |
| `MidiSetVoiceWavetable(Index, Type)` | Set wavetable preset |
| `MidiSetVoicePan(Index, Pan)` | Set pan (-1.0 to 1.0) |
| `MidiGetVoiceFrequency(Index)` | Get current frequency |
| `MidiGetVoiceAmplitude(Index)` | Get current amplitude |
| `MidiGetVoiceWavetable(Index)` | Get current wavetable |
| `MidiGetVoicePan(Index)` | Get current pan |

### MIDI Utility Functions

| Function | Description |
|----------|-------------|
| `MidiNoteToFreq(Note)` | Convert MIDI note (0-127) to frequency Hz |
| `MidiFreqToNote(Freq)` | Convert frequency to MIDI note |
| `MidiVelocityToAmp(Velocity)` | Convert velocity (0-127) to amplitude |
| `MidiAmpToVelocity(Amp)` | Convert amplitude to velocity |
| `MidiPanToSedai(Pan)` | Convert MIDI pan (0-127) to Sedai (-1 to 1) |
| `SedaiPanToMidi(Pan)` | Convert Sedai pan to MIDI pan |
| `MidiNoteToName(Note)` | Get note name (e.g., "C4", "A#3") |
| `MidiNoteToOctave(Note)` | Get octave number |

### MIDI-Optimized Playback

| Function | Description |
|----------|-------------|
| `PlayWavetableMidi(Freq, Type, Amp)` | Play with MIDI voice allocation |
| `PlayMidiNote(Note, Velocity, Type)` | Play by MIDI note number |

### MIDI File Playback

MIDI playback uses the object API in `SedaiMIDIPlayer` (class `TSedaiMIDIPlayer`),
not global functions:

| Member | Description |
|--------|-------------|
| `Create` / `SetSampleRate(Hz)` | Construct the player and set its sample rate |
| `LoadFromFile(Filename)` | Load a standard MIDI file |
| `Play` / `Pause` / `Stop` / `Rewind` | Transport control |
| `AdvanceSamples(Count)` | Advance the sequencer by N samples (call from the audio callback) |
| `Playing` / `Paused` / `Loaded` | State properties |
| `GetDurationSeconds` / `GetPositionSeconds` | Timing info |
| `SongName` / `Copyright` / `TrackCount` | Metadata |

### Musical Helpers

| Function | Description |
|----------|-------------|
| `PlayChordClassic(Note1, Note2, Note3, Preset)` | Play a classic chord |
| `PlayChordFM(Note1, Note2, Note3, Preset)` | Play an FM chord |
| `PlayChordWavetable(Note1, Note2, Note3, Preset)` | Play a wavetable chord |
| `PlayScaleClassic(BaseFreq, Preset)` | Play a classic scale |
| `PlayScaleFM(BaseFreq, Preset)` | Play an FM scale |
| `PlayScaleWavetable(BaseFreq, Preset)` | Play a wavetable scale |

---

## Code Metrics

| Metric | Value |
|--------|-------|
| **Total Lines** | ~30,000+ lines of Pascal code |
| **Source Units** | 43+ units (.pas files) |
| **Demo Programs** | 10+ programs (.lpr files) |
| **Synthesis Engines** | 5 (Classic, FM, Wavetable, Additive, SID Evo) |
| **Audio Effects** | 9 (Delay, Reverb, Chorus, Flanger, Phaser, Distortion, Compressor, Limiter, EQ) |
| **Filter Types** | 6 (LP, HP, BP, Notch, Allpass, Peaking) |
| **Filter Slopes** | 3 (12dB, 24dB, 48dB per octave) |
| **Dependencies** | SDL2 only |
| **Voice Polyphony** | 32 default (configurable, hardware-dependent) |
| **Sample Rate** | 44100 Hz |
| **Bit Depth** | 32-bit floating point |
| **Audio Buffer** | 1024 samples (~23ms latency) |
| **SID Evo Waveforms** | 12 (4 classic + 8 extended) |
| **SID Evo Voices** | 8-64 (configurable in groups of 8) |
| **Mixer Channels** | Unlimited (performance-dependent) |
| **Aux Buses** | 8 |
| **Group Buses** | 8 |
| **Pan Laws** | 5 (Linear, ConstantPower, -3dB, -4.5dB, -6dB) |
| **MIDI Channels** | 16 |
| **Wavetable Formats** | 4 (Serum, Vital, Surge, Generic WAV) |

---

## Roadmap

### Implementation Status

All 8 implementation phases are **complete**:

| Phase | Description | Status |
|-------|-------------|--------|
| Phase 1 | Foundation (Core) | **Complete** |
| Phase 2 | Generators & Modulators | **Complete** |
| Phase 3 | Processors & Effects | **Complete** |
| Phase 4 | Voice Management | **Complete** |
| Phase 5 | Mixing | **Complete** |
| Phase 6 | DAW Core | **Complete** |
| Phase 7 | Advanced Synthesis | **Complete** |
| Phase 8 | Professional Features | **Complete** |

### Future Synthesis Techniques

The following synthesis techniques are planned for future versions:

| Technique | Description | Complexity | Status |
|-----------|-------------|------------|--------|
| **IFFT Synthesis** | Inverse Fast Fourier Transform for spectral manipulation and resynthesis | Medium-High | Planned |
| **Granular Synthesis** | Time-stretching and pitch-shifting through micro-sound grains | Medium | Planned |
| **Physical Modeling** | Realistic instrument simulation using mathematical models of physical systems | High | Planned |

#### IFFT Synthesis - Implementation Details

Spectral synthesis using Inverse FFT for powerful frequency-domain manipulation.

**Requirements:**
- FFT/IFFT engine (Cooley-Tukey radix-2 or similar algorithm)
- Spectral frame buffer management (amplitude + phase per frequency bin)
- Overlap-add or overlap-save for real-time streaming
- Spectral interpolation for smooth morphing between frames

**Implementation Steps:**
1. FFT engine with configurable frame size (512, 1024, 2048, 4096 samples)
2. Spectral frame buffer with magnitude/phase representation
3. Real-time resynthesis with 50-75% overlap
4. API for spectral manipulation (filtering, pitch shifting, time stretching)

**Use Cases:** Vocoder effects, spectral freeze, cross-synthesis, noise reduction

#### Granular Synthesis - Implementation Details

Micro-sound based synthesis for extreme time-stretching and textural effects.

**Requirements:**
- Grain scheduler (density, spray/randomization control)
- Grain pool management (hundreds of simultaneous grains)
- Per-grain parameters: position, duration, pitch, envelope, pan
- Audio source (sample buffer or real-time input)

**Implementation Steps:**
1. Grain class with envelope shapes (Hanning, Gaussian, Trapezoid, Rectangle)
2. Grain scheduler with density (grains/second) and position spray
3. Sample source with random access and interpolation
4. Efficient mixing of active grains with voice limiting

**Use Cases:** Time-stretching without pitch change, ambient textures, glitch effects, soundscapes

#### Physical Modeling - Implementation Details

Mathematical simulation of physical instrument behavior for realistic sounds.

**Requirements:**
- Mathematical models for specific instruments (strings, winds, percussion)
- Waveguide synthesis or mass-spring models
- Physical parameters (tension, material density, resonances)
- Numerical stability at all parameter ranges

**Implementation Steps:**
1. Karplus-Strong base algorithm (plucked string)
2. Digital waveguide for strings and tubes (bidirectional delay lines)
3. Excitation models (bow friction, breath pressure, hammer impact)
4. Body resonance filters (formant-based or impulse response)

**Use Cases:** Realistic guitar, piano, wind instruments, custom impossible instruments

### Short-Term Improvements

| Feature | Description | Priority |
|---------|-------------|----------|
| **OGG/FLAC Decoding** | Add OGG Vorbis and FLAC decoders to file reader | Medium |
| **Sample Playback Engine** | One-shot and looped sample playback with pitch control | High |
| **Voice Stealing** | Intelligent note stealing when polyphony limit reached | Medium |
| **Improved Anti-aliasing** | PolyBLEP for oscillators, oversampling for distortion | Medium |

### Medium-Term Features

| Feature | Description | Priority |
|---------|-------------|----------|
| **Multitimbral Mode** | Multiple synth engines running simultaneously | Medium |
| **Arpeggiator** | Pattern-based note arpeggiation | Medium |
| **Step Sequencer** | Internal step sequencer for patterns | Medium |
| **macOS Support** | Darwin x86_64 and aarch64 targets | Medium |

### Known Issues & Limitations

| Issue | Description | Workaround |
|-------|-------------|------------|
| **Limited Audio File Support** | Only WAV files supported currently | OGG/FLAC coming soon |
| **Windows Focus** | Linux support may have minor issues | Report bugs |
| **API Stability** | API may change in minor versions | Pin version for production |
| **Documentation** | API documentation is embedded in source | Read unit interfaces |

---

## License

This project is dual-licensed: **GNU General Public License v3.0 (GPL-3.0) OR Commercial**.
See [`LICENSING.md`](LICENSING.md) for the full picture, [`LICENSE`](LICENSE) for the
GPL-3.0 text, and [`LICENSE.GPL-2.0`](LICENSE.GPL-2.0) for the GPL-2.0 text.

You are free to use, modify, and distribute this software under the terms of the GPL-3.0 license.

### Exception — SID engine and GoatTracker player (GPL-2.0-or-later, GPL-only)

These files are **ports of GPL'd third-party engines**, so the dual license above **does
not apply to them** — they are distributed under the **same license as their upstream,
GNU GPL version 2 or any later version (GPL-2.0-or-later)**, GPL-only (full text in
[`LICENSE.GPL-2.0`](LICENSE.GPL-2.0)):

- `src/SID/SedaiSIDEvo.pas` + `src/SID/SedaiSIDEvo_WaveTables.inc` — a Pascal port of
  **reSID / reSID-fp** (partially built on the SAF framework);
- `src/Players/SedaiGoatTracker.pas` — a reimplementation of the **GoatTracker 2** player
  routine (and it uses GoatTracker's frequency tables).

No commercial/proprietary license can be granted for these without permission from the
respective upstream copyright holders. `sng_player` links them, so the built player is GPL.
See [`LICENSING.md`](LICENSING.md) for the full breakdown.

> **reSID** is the cycle-accurate MOS 6581/8580 emulator by **Dag Lem** (Copyright © 2004
> Dag Lem); the optional non-linear "distortion" filter is from **reSID-fp** by
> **Antti S. Lankila**. **GoatTracker 2** is by **Lasse Öörni** (Cadaver / Covert Bitops).
> Band-limited resampling follows **Julius O. Smith III**. All credit for the original SID
> emulation and player routine goes to them — our heartfelt thanks for releasing this work
> under the GPL. The bit-exact accuracy of `SedaiSIDEvo` and `SedaiGoatTracker` is a
> tribute to reSID and GoatTracker.

### Commercial Licensing

The commercial option covers the Sedai Audio Foundation code authored for this project; it
does **not** cover the reSID-derived SID engine (see the exception above). For commercial
use or proprietary licensing options, please contact the author:

**Maurizio Cammalleri**
Email: maurizio.cammalleri@gmail.com

---

## Contributing

We welcome contributions in the following areas:

- **Bug Reports**: Open issues for any bugs found
- **New Effects**: Implement additional audio effects
- **Platform Support**: Test and fix issues on Linux/macOS
- **Documentation**: Improve code comments and examples
- **Presets**: Create and share synthesis presets

For bug reports or suggestions, please open an issue on the project repository.

---

**Sedai Audio Foundation** - Professional Audio Synthesis for Free Pascal
