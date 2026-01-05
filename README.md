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

- **5 Synthesis Engines**: Classic/Subtractive, FM (DX7-style), Wavetable, Additive, SID Evo
- **Professional Mixer**: Channels, aux buses, group buses, master bus with metering
- **Audio Effects**: Delay, Reverb, Chorus, Flanger, Phaser, Distortion, Compressor, Limiter, EQ
- **Advanced Filters**: 6 filter types, multi-pole cascading (12/24/48 dB/oct)
- **DAW Foundation**: Transport, tracks, clips, automation, undo/redo
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

The library implements a **Directed Acyclic Graph (DAG)** for flexible effect routing:

```pascal
// Effect graph example
EffectGraph.AddNode(DelayNode);
EffectGraph.AddNode(ReverbNode);
EffectGraph.AddNode(CompressorNode);
EffectGraph.Connect(DelayNode, ReverbNode);
EffectGraph.Connect(ReverbNode, CompressorNode);
EffectGraph.Process(InputBuffer, OutputBuffer, FrameCount, Channels);
```

#### Modular Synth Engine Base Class

All synthesis engines can derive from `TSedaiSynthEngine` for consistent voice management:

```pascal
TSedaiSynthEngine = class
  VoicePool: TSedaiVoicePool;      // Polyphonic voice allocation
  EffectGraph: TSedaiEffectGraph;   // Per-engine effect chain
  MasterFilter: TSedaiMultiPoleFilter;
  LFOs: array[0..3] of TSedaiLFO;  // 4 global LFOs

  procedure NoteOn(ANote, AVelocity: Integer); virtual;
  procedure NoteOff(ANote: Integer); virtual;
  procedure ProcessStereo(AOutputL, AOutputR: PSingle; AFrameCount: Integer); virtual;
end;
```

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
| **SedaiSIDEvo** | Evolved SID-style synthesizer (enhanced ReSID implementation) |

#### Players

| Module | Description |
|--------|-------------|
| **SedaiGoatTracker** | GoatTracker v2 .sng file parser and player |
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

A Commodore 64 SID-inspired synthesis engine with extended capabilities. Provides authentic chiptune sounds while adding modern waveforms for creative flexibility. This is an **enhanced reimplementation of the ReSID engine** combining cycle-accurate SID emulation with modern synthesis features.

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

| Feature | Description |
|---------|-------------|
| **15-bit Rate Counter** | With wraparound at $8000 (ADSR delay bug) |
| **Exponential Counter** | Period changes at levels 255, 93, 54, 26, 14, 6, 0 |
| **Hold-Zero** | Envelope frozen when reaching zero |
| **24-bit Accumulator** | Authentic phase accumulator |
| **23-bit LFSR Noise** | Authentic noise generator |
| **Waveform AND** | Combined waveforms via AND operation |
| **Two-Integrator-Loop Filter** | Authentic 6581/8580 filter topology |

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

| Feature | Description |
|---------|-------------|
| **8-64 Voices** | Configurable in groups of 8 |
| **11 Octaves** | C0 to B10 (16Hz-20kHz) |
| **Stereo Output** | Per-voice panning |
| **Spatial 3D** | X/Y/Z positioning |
| **Per-voice LFO** | Pitch, filter, amplitude modulation |
| **Extended Waveforms** | Sine, Supersaw, PWM, Half-Sine, Full-Sine, Formant, Metallic |

**Features**:
- Waveforms can be combined via OR bitmask (like the original SID)
- 3 independent voices with ADSR envelopes
- Pulse width modulation support
- Master volume control
- Hard restart capability

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

### Supported Features

- Full song structure (orderlist, patterns, instruments, wavetables)
- 3-channel playback with per-channel mute control
- Loop detection and seamless looping
- Real-time tempo control

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

### Demo Player Controls

```
SPACE  - Pause/Resume playback
R      - Restart song from beginning
L      - Toggle loop mode
V      - Toggle pattern command verbose output
W      - Toggle wavetable command verbose output
S      - Print SpeedTable contents
1/2/3  - Mute/unmute channel 1/2/3
Q      - Quit player
```

### Usage Example

```pascal
program GoatTrackerDemo;

uses
  SedaiSIDEvo, SedaiGoatTracker;

var
  SIDEvo: TSedaiSIDEvo;
  GTPlayer: TSedaiGoatTracker;
begin
  SIDEvo := TSedaiSIDEvo.Create(44100);
  GTPlayer := TSedaiGoatTracker.Create(SIDEvo);

  if GTPlayer.LoadSong('mysong.sng') then
  begin
    GTPlayer.Play;

    while GTPlayer.IsPlaying do
    begin
      GTPlayer.Process;  // Call each frame
      Sleep(20);         // ~50 Hz update rate
    end;

    GTPlayer.Stop;
  end;

  GTPlayer.Free;
  SIDEvo.Free;
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
| `test_compilation` | Unit compilation test |
| `test_saf_main` | Main SAF API test (Classic, FM, Wavetable synthesis) |
| `demo_synth` | Synth demo |
| `sng_player` | GoatTracker .sng player |
| `sng_dump` | SNG register dump tool (VICE format) |
| `audiotest` | Audio backend test |
| `sedaisid_test` | SedaiSIDEvo verification test |

### Manual Compilation

```bash
# Compile main test program
fpc -Mdelphi audiotest.lpr

# Compile individual demos
fpc -Mdelphi demo_presets.pas
fpc -Mdelphi demo_midi_player.pas
fpc -Mdelphi demo_filters_effects.pas
fpc -Mdelphi demo_additive.pas
```

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

### demo_presets

Demonstrates all built-in synthesis presets.

```bash
./demo_presets
```

**Menu Options**:
1. Classic Synthesis Presets (6 presets)
2. FM Synthesis Presets (6 presets)
3. Wavetable Presets (6 presets)
4. Synthesis Engines Comparison
5. Full Demo (all presets)

### demo_midi_player

Interactive MIDI file player with real-time controls.

```bash
./demo_midi_player
```

**Controls**:
- `P` - Play/Pause
- `S` - Stop
- `+`/`-` - Volume control
- `F`/`L` - Faster/Slower tempo
- `I` - File info
- `C` - Channel info
- `T` - Playback status
- `M` - Load another file
- `Q` - Quit

**Note**: Place a `.mid` file in the application directory or provide the full path when prompted.

### demo_filters_effects

Tests audio filters and effects processing.

```bash
./demo_filters_effects
```

**Tests**:
- Biquad filters (LP, HP, BP, Notch, Allpass, Peaking)
- Multi-pole filters (12dB, 24dB, 48dB/octave)
- Effects (Delay, Reverb, Chorus, Flanger, Distortion)

**Note**: This demo does NOT require audio output - it tests mathematical correctness.

### demo_additive

Demonstrates additive synthesis with harmonic control.

```bash
./demo_additive
```

**Features**:
- Pure tone and complex wave generation
- 6 harmonic spectrum presets
- Harmonic vs. inharmonic synthesis comparison
- Custom spectrum creation
- ADSR envelope visualization

**Note**: This demo does NOT require audio output.

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
  SedaiAudioFoundation, SedaiMIDIFoundation;

begin
  if InitAudio(64) and InitMidi then
  begin
    SetMasterVolume(0.7);
    SetupMidiGeneralMidi;

    if LoadMidiFile('song.mid') then
    begin
      MidiPlay;

      while IsMidiPlaying do
        Sleep(100);

      MidiStop;
    end;

    ShutdownMidi;
    ShutdownAudio;
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

| Function | Description |
|----------|-------------|
| `InitMidi` | Initialize MIDI system |
| `ShutdownMidi` | Shutdown MIDI system |
| `LoadMidiFile(Filename)` | Load a MIDI file |
| `MidiPlay` | Start playback |
| `MidiPause` | Pause playback |
| `MidiStop` | Stop playback |
| `IsMidiPlaying` | Check if playing |
| `GetMidiProgress` | Get playback progress (0-100%) |
| `SetMidiTempo(Percent)` | Set tempo (1.0 = normal) |
| `SetupMidiGeneralMidi` | Apply General MIDI mapping |

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

This project is licensed under the **GNU General Public License v3.0 (GPL-3.0)**.

You are free to use, modify, and distribute this software under the terms of the GPL-3.0 license. See the [LICENSE](LICENSE) file for details.

### Commercial Licensing

For commercial use or proprietary licensing options, please contact the author:

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
