{*
 * Sedai Audio Foundation - Core Types
 *
 * This unit contains all fundamental types, enumerations, records, and constants
 * used throughout the Sedai Audio Foundation library.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAudioTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

const
  // ============================================================================
  // AUDIO CONFIGURATION CONSTANTS
  // ============================================================================

  SEDAI_DEFAULT_SAMPLE_RATE = 44100;
  SEDAI_DEFAULT_BLOCK_SIZE = 1024;
  SEDAI_DEFAULT_CHANNELS = 2;
  SEDAI_DEFAULT_BIT_DEPTH = 32;  // 32-bit float
  SEDAI_DEFAULT_MAX_VOICES = 32;

  SEDAI_MIN_FREQUENCY = 20.0;
  SEDAI_MAX_FREQUENCY = 20000.0;
  SEDAI_MIN_AMPLITUDE = 0.0;
  SEDAI_MAX_AMPLITUDE = 1.0;
  SEDAI_SILENCE_THRESHOLD = 0.00001;  // -100dB threshold for silence detection

  // Mathematical constants
  SEDAI_TWO_PI = 2.0 * Pi;
  SEDAI_HALF_PI = Pi / 2.0;

  // SID clock frequencies
  SEDAI_SID_CLOCK_PAL = 985248;
  SEDAI_SID_CLOCK_NTSC = 1022727;

  // MIDI constants
  SEDAI_MIDI_NOTE_A4 = 69;
  SEDAI_MIDI_FREQ_A4 = 440.0;
  SEDAI_MIDI_MAX_CHANNELS = 16;
  SEDAI_MIDI_MAX_NOTES = 128;
  SEDAI_MIDI_MAX_VELOCITY = 127;

  // PPQ (Pulses Per Quarter note) for MIDI timing
  SEDAI_DEFAULT_PPQ = 480;

type
  // ============================================================================
  // SID TYPES
  // ============================================================================

  // SID chip model (used by filter and SID emulation)
  TSIDModel = (
    smMOS6581,            // Original (darker, more distorted filter)
    smMOS8580             // Revised (cleaner, different filter curve)
  );

  // ============================================================================
  // BASIC AUDIO TYPES
  // ============================================================================

  // Sample types
  TSample = Single;
  PSample = ^TSample;
  TSampleArray = array of TSample;

  // Stereo sample
  TStereoSample = record
    Left: TSample;
    Right: TSample;
  end;
  PStereoSample = ^TStereoSample;

  // Audio buffer pointer types
  TAudioBufferMono = array of TSample;
  TAudioBufferStereo = array of TStereoSample;

  // ============================================================================
  // WAVEFORM TYPES
  // ============================================================================

  TWaveformType = (
    wtSine,       // Pure sine wave
    wtSawtooth,   // Sawtooth wave (ramp up)
    wtSquare,     // Square wave (50% duty cycle)
    wtPulse,      // Pulse wave (variable duty cycle)
    wtTriangle,   // Triangle wave
    wtNoise,      // White noise
    // Extended waveforms
    wtSuperSaw,   // Detuned sawtooth stack
    wtPWM,        // Pulse width modulation
    wtHalfSine,   // Half-wave rectified sine
    wtFullSine,   // Full-wave rectified sine
    wtFormant,    // Formant synthesis
    wtMetallic,   // Ring modulation style
    wtCustom      // User-defined wavetable
  );

  // Set of waveforms for combined waveform generation
  // Allows combining multiple waveforms like SID chip does
  TWaveformSet = set of TWaveformType;

  // Waveform combination mode
  TWaveformCombineMode = (
    wcmMix,       // Additive mixing (sum of waveforms, normalized)
    wcmAND,       // Bitwise AND (SID-style, requires digital conversion)
    wcmMultiply,  // Ring modulation (multiply waveforms)
    wcmMin,       // Minimum of all waveforms
    wcmMax        // Maximum of all waveforms
  );

  // Noise types
  TNoiseType = (
    ntWhite,      // White noise (flat spectrum)
    ntPink,       // Pink noise (-3dB/octave)
    ntBrown       // Brown/Red noise (-6dB/octave)
  );

  // ============================================================================
  // FILTER TYPES
  // ============================================================================

  TFilterType = (
    ftLowPass,    // Low-pass filter
    ftHighPass,   // High-pass filter
    ftBandPass,   // Band-pass filter
    ftNotch,      // Notch (band-reject) filter
    ftAllPass,    // All-pass filter (phase shift only)
    ftPeaking     // Peaking EQ filter
  );

  TFilterSlope = (
    fs12dB,       // 12 dB/octave (2-pole)
    fs24dB,       // 24 dB/octave (4-pole)
    fs48dB        // 48 dB/octave (8-pole)
  );

  // SID chip model (affects filter response)
  TSIDChipModel = (
    scm6581,      // Original MOS 6581
    scm8580       // Revised MOS 8580
  );

  // ============================================================================
  // ENVELOPE TYPES
  // ============================================================================

  TEnvelopeState = (
    esIdle,       // Not active
    esAttack,     // Attack phase
    esDecay,      // Decay phase
    esSustain,    // Sustain phase
    esRelease     // Release phase
  );

  TEnvelopeCurve = (
    ecLinear,     // Linear curve
    ecExponential,// Exponential curve (fast start, slow end)
    ecLogarithmic,// Logarithmic curve (slow start, fast end)
    ecSCurve      // S-curve (smooth transitions)
  );

  // ADSR parameters record
  TADSRParams = record
    Attack: Single;   // Attack time in seconds
    Decay: Single;    // Decay time in seconds
    Sustain: Single;  // Sustain level (0-1)
    Release: Single;  // Release time in seconds
    Curve: TEnvelopeCurve;
  end;

  // ============================================================================
  // VOICE TYPES
  // ============================================================================

  TVoiceState = (
    vsIdle,       // Voice not in use
    vsAttack,     // Voice starting (attack phase)
    vsPlaying,    // Voice actively playing
    vsReleasing,  // Voice in release phase
    vsStealing    // Voice being stolen (fade out)
  );

  TVoiceStealPolicy = (
    vspOldest,        // Steal the oldest voice
    vspQuietest,      // Steal the quietest voice
    vspLowestPriority,// Steal lowest priority voice
    vspNone           // Don't steal (reject new notes)
  );

  // ============================================================================
  // TRANSPORT TYPES
  // ============================================================================

  TTransportState = (
    tsStopped,    // Transport stopped
    tsPlaying,    // Transport playing
    tsRecording,  // Transport recording
    tsPaused      // Transport paused
  );

  // Time signature
  TTimeSignature = record
    Numerator: Integer;   // Beats per bar
    Denominator: Integer; // Beat value (4 = quarter note, 8 = eighth note)
  end;

  // ============================================================================
  // TRACK & PROJECT TYPES
  // ============================================================================

  TTrackType = (
    ttAudio,      // Audio track
    ttMIDI,       // MIDI track
    ttInstrument, // Instrument track (MIDI + synth)
    ttAux,        // Auxiliary/bus track
    ttMaster      // Master output track
  );

  TAutomationMode = (
    amRead,       // Read automation only
    amWrite,      // Write automation (destructive)
    amTouch,      // Write while touching control
    amLatch       // Write until stop
  );

  TAutomationCurve = (
    acStep,       // Step (instant change)
    acLinear,     // Linear interpolation
    acBezier      // Bezier curve
  );

  // ============================================================================
  // PARAMETER TYPES
  // ============================================================================

  TParameterCurve = (
    pcLinear,     // Linear response
    pcLogarithmic,// Logarithmic response (audio taper)
    pcExponential // Exponential response
  );

  // ============================================================================
  // EFFECT TYPES
  // ============================================================================

  TDelayType = (
    dtSimple,     // Simple mono delay
    dtPingPong,   // Stereo ping-pong delay
    dtMultitap,   // Multi-tap delay
    dtTape,       // Tape delay emulation
    dtBBD         // Bucket brigade delay emulation
  );

  TDistortionType = (
    distSoftClip,   // Soft clipping (tube-like)
    distHardClip,   // Hard clipping (transistor-like)
    distFoldback,   // Foldback distortion
    distBitcrush,   // Bit crusher
    distTube,       // Tube emulation
    distTape        // Tape saturation
  );

  TEQBandType = (
    eqLowShelf,   // Low shelf
    eqHighShelf,  // High shelf
    eqPeak,       // Parametric peak
    eqLowPass,    // Low-pass
    eqHighPass    // High-pass
  );

  // ============================================================================
  // LOOP MODES
  // ============================================================================

  TLoopMode = (
    lmOff,        // No looping
    lmForward,    // Forward loop
    lmPingPong    // Ping-pong (forward-backward) loop
  );

  // ============================================================================
  // WARP MODES (for audio clips)
  // ============================================================================

  TWarpMode = (
    wmOff,        // No warping (pitch follows tempo)
    wmRepitch,    // Repitch (simple resampling)
    wmComplex,    // Complex algorithm (preserve formants)
    wmBeats       // Beat-aware (for drums/percussion)
  );

  // ============================================================================
  // PAN LAWS
  // ============================================================================

  TPanLaw = (
    plLinear,         // Linear panning
    plConstantPower,  // Constant power (-3dB center)
    plMinus3dB,       // -3dB center
    plMinus4_5dB,     // -4.5dB center
    plMinus6dB        // -6dB center
  );

  // ============================================================================
  // BACKEND TYPES
  // ============================================================================

  TBackendState = (
    bsStopped,    // Backend stopped
    bsStarting,   // Backend initializing
    bsRunning,    // Backend running
    bsError       // Backend error
  );

  // ============================================================================
  // MIDI TYPES
  // ============================================================================

  TMIDIEventType = (
    metNoteOff,           // Note Off
    metNoteOn,            // Note On
    metPolyAftertouch,    // Polyphonic Aftertouch
    metControlChange,     // Control Change
    metProgramChange,     // Program Change
    metChannelAftertouch, // Channel Aftertouch
    metPitchBend,         // Pitch Bend
    metSysEx,             // System Exclusive
    metMeta               // Meta event (tempo, time sig, etc.)
  );

  // MIDI event record
  TMIDIEvent = record
    Time: Int64;          // Time in ticks or samples
    EventType: TMIDIEventType;
    Channel: Byte;        // 0-15
    Data1: Byte;          // Note number or CC number
    Data2: Byte;          // Velocity or CC value
    DataEx: array of Byte;// Extended data for SysEx/Meta
  end;
  PMIDIEvent = ^TMIDIEvent;

  // ============================================================================
  // MODULATION TARGET
  // ============================================================================

  TModulationTarget = record
    TargetObject: TObject;  // Target object (TSedaiAudioObject)
    ParameterName: string;  // Parameter name
    Amount: Single;         // Modulation amount
    Bipolar: Boolean;       // True = -1..+1, False = 0..+1
  end;

  // ============================================================================
  // AUTOMATION POINT
  // ============================================================================

  TAutomationPoint = record
    Position: Int64;        // Position in samples
    Value: Single;          // Parameter value
    Curve: TAutomationCurve;// Interpolation curve
    BezierX1, BezierY1: Single; // Bezier control point 1
    BezierX2, BezierY2: Single; // Bezier control point 2
  end;

  // ============================================================================
  // FADE CURVE
  // ============================================================================

  TFadeCurve = (
    fcLinear,     // Linear fade
    fcLogarithmic,// Logarithmic fade
    fcExponential,// Exponential fade
    fcSCurve      // S-curve fade
  );

  TFade = record
    Length: Int64;        // Fade length in samples
    Curve: TFadeCurve;    // Fade curve type
  end;

  // ============================================================================
  // TEMPO EVENT (for tempo map)
  // ============================================================================

  TTempoEvent = record
    Position: Int64;          // Position in samples
    Tempo: Single;            // Tempo in BPM
    TimeSignature: TTimeSignature;
  end;

  // ============================================================================
  // WAVETABLE TYPES
  // ============================================================================

  // Wavetable format enumeration
  TWavetableFormat = (
    wtfUnknown,    // Unknown format
    wtfSerum,      // Serum format (2048 samples per frame)
    wtfVital,      // Vital format (Serum-compatible)
    wtfSurge,      // Surge .wt format
    wtfGeneric     // Generic WAV (single-cycle waveform)
  );

  // Wavetable data structure
  TWavetable = record
    Name: string;                        // Wavetable name (filename without extension)
    Samples: array of array of Single;   // [WaveIndex][SampleIndex]
    WaveCount: Integer;                  // Number of waves in wavetable
    SampleLength: Integer;               // Samples per wave (typically 2048)
    IsLoaded: Boolean;                   // True if data is valid
  end;

  // Wavetable file info
  TWavetableInfo = record
    Name: string;
    Format: TWavetableFormat;
    FileSize: Int64;
    FrameCount: Integer;
    SampleRate: Integer;
    IsValid: Boolean;
  end;

  // String array type for directory scanning
  TStringArray = array of string;

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

// Waveform type conversion
function WaveformTypeToString(AWaveform: TWaveformType): string;
function StringToWaveformType(const AStr: string): TWaveformType;

// Filter type conversion
function FilterTypeToString(AFilter: TFilterType): string;
function FilterSlopeToString(ASlope: TFilterSlope): string;

// Envelope state conversion
function EnvelopeStateToString(AState: TEnvelopeState): string;

// Voice state conversion
function VoiceStateToString(AState: TVoiceState): string;

// Transport state conversion
function TransportStateToString(AState: TTransportState): string;

// MIDI utilities
function MIDINoteToFrequency(ANote: Integer): Single;
function FrequencyToMIDINote(AFreq: Single): Integer;
function MIDINoteToName(ANote: Integer): string;

// dB conversion
function LinearToDecibel(ALinear: Single): Single;
function DecibelToLinear(ADecibel: Single): Single;

// Clamp utilities
function ClampSample(AValue: Single): Single; inline;
function ClampParameter(AValue, AMin, AMax: Single): Single; inline;

// Default ADSR
function DefaultADSR: TADSRParams;

// Default time signature
function DefaultTimeSignature: TTimeSignature;

implementation

// ============================================================================
// WAVEFORM TYPE CONVERSION
// ============================================================================

function WaveformTypeToString(AWaveform: TWaveformType): string;
begin
  case AWaveform of
    wtSine:      Result := 'Sine';
    wtSawtooth:  Result := 'Sawtooth';
    wtSquare:    Result := 'Square';
    wtPulse:     Result := 'Pulse';
    wtTriangle:  Result := 'Triangle';
    wtNoise:     Result := 'Noise';
    wtSuperSaw:  Result := 'SuperSaw';
    wtPWM:       Result := 'PWM';
    wtHalfSine:  Result := 'HalfSine';
    wtFullSine:  Result := 'FullSine';
    wtFormant:   Result := 'Formant';
    wtMetallic:  Result := 'Metallic';
    wtCustom:    Result := 'Custom';
  else
    Result := 'Unknown';
  end;
end;

function StringToWaveformType(const AStr: string): TWaveformType;
var
  LStr: string;
begin
  LStr := LowerCase(AStr);
  if LStr = 'sine' then Result := wtSine
  else if LStr = 'sawtooth' then Result := wtSawtooth
  else if LStr = 'saw' then Result := wtSawtooth
  else if LStr = 'square' then Result := wtSquare
  else if LStr = 'pulse' then Result := wtPulse
  else if LStr = 'triangle' then Result := wtTriangle
  else if LStr = 'noise' then Result := wtNoise
  else if LStr = 'supersaw' then Result := wtSuperSaw
  else if LStr = 'pwm' then Result := wtPWM
  else if LStr = 'halfsine' then Result := wtHalfSine
  else if LStr = 'fullsine' then Result := wtFullSine
  else if LStr = 'formant' then Result := wtFormant
  else if LStr = 'metallic' then Result := wtMetallic
  else if LStr = 'custom' then Result := wtCustom
  else Result := wtSine;
end;

// ============================================================================
// FILTER TYPE CONVERSION
// ============================================================================

function FilterTypeToString(AFilter: TFilterType): string;
begin
  case AFilter of
    ftLowPass:  Result := 'LowPass';
    ftHighPass: Result := 'HighPass';
    ftBandPass: Result := 'BandPass';
    ftNotch:    Result := 'Notch';
    ftAllPass:  Result := 'AllPass';
    ftPeaking:  Result := 'Peaking';
  else
    Result := 'Unknown';
  end;
end;

function FilterSlopeToString(ASlope: TFilterSlope): string;
begin
  case ASlope of
    fs12dB: Result := '12dB/oct';
    fs24dB: Result := '24dB/oct';
    fs48dB: Result := '48dB/oct';
  else
    Result := 'Unknown';
  end;
end;

// ============================================================================
// ENVELOPE STATE CONVERSION
// ============================================================================

function EnvelopeStateToString(AState: TEnvelopeState): string;
begin
  case AState of
    esIdle:    Result := 'Idle';
    esAttack:  Result := 'Attack';
    esDecay:   Result := 'Decay';
    esSustain: Result := 'Sustain';
    esRelease: Result := 'Release';
  else
    Result := 'Unknown';
  end;
end;

// ============================================================================
// VOICE STATE CONVERSION
// ============================================================================

function VoiceStateToString(AState: TVoiceState): string;
begin
  case AState of
    vsIdle:      Result := 'Idle';
    vsAttack:    Result := 'Attack';
    vsPlaying:   Result := 'Playing';
    vsReleasing: Result := 'Releasing';
    vsStealing:  Result := 'Stealing';
  else
    Result := 'Unknown';
  end;
end;

// ============================================================================
// TRANSPORT STATE CONVERSION
// ============================================================================

function TransportStateToString(AState: TTransportState): string;
begin
  case AState of
    tsStopped:   Result := 'Stopped';
    tsPlaying:   Result := 'Playing';
    tsRecording: Result := 'Recording';
    tsPaused:    Result := 'Paused';
  else
    Result := 'Unknown';
  end;
end;

// ============================================================================
// MIDI UTILITIES
// ============================================================================

function MIDINoteToFrequency(ANote: Integer): Single;
begin
  // f = 440 * 2^((n-69)/12)
  Result := SEDAI_MIDI_FREQ_A4 * Power(2.0, (ANote - SEDAI_MIDI_NOTE_A4) / 12.0);
end;

function FrequencyToMIDINote(AFreq: Single): Integer;
begin
  // n = 69 + 12 * log2(f/440)
  if AFreq <= 0 then
    Result := 0
  else
    Result := Round(SEDAI_MIDI_NOTE_A4 + 12.0 * (Ln(AFreq / SEDAI_MIDI_FREQ_A4) / Ln(2.0)));
end;

function MIDINoteToName(ANote: Integer): string;
const
  NoteNames: array[0..11] of string = (
    'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'
  );
var
  Octave: Integer;
  NoteName: Integer;
begin
  Octave := (ANote div 12) - 1;
  NoteName := ANote mod 12;
  Result := NoteNames[NoteName] + IntToStr(Octave);
end;

// ============================================================================
// DB CONVERSION
// ============================================================================

function LinearToDecibel(ALinear: Single): Single;
begin
  if ALinear <= 0.0 then
    Result := -120.0  // Effectively -infinity
  else
    Result := 20.0 * Log10(ALinear);
end;

function DecibelToLinear(ADecibel: Single): Single;
begin
  if ADecibel <= -120.0 then
    Result := 0.0
  else
    Result := Power(10.0, ADecibel / 20.0);
end;

// ============================================================================
// CLAMP UTILITIES
// ============================================================================

function ClampSample(AValue: Single): Single; inline;
begin
  if AValue > 1.0 then
    Result := 1.0
  else if AValue < -1.0 then
    Result := -1.0
  else
    Result := AValue;
end;

function ClampParameter(AValue, AMin, AMax: Single): Single; inline;
begin
  if AValue < AMin then
    Result := AMin
  else if AValue > AMax then
    Result := AMax
  else
    Result := AValue;
end;

// ============================================================================
// DEFAULT VALUES
// ============================================================================

function DefaultADSR: TADSRParams;
begin
  Result.Attack := 0.01;    // 10ms
  Result.Decay := 0.1;      // 100ms
  Result.Sustain := 0.7;    // 70%
  Result.Release := 0.3;    // 300ms
  Result.Curve := ecLinear;
end;

function DefaultTimeSignature: TTimeSignature;
begin
  Result.Numerator := 4;
  Result.Denominator := 4;
end;

end.
