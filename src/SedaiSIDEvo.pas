{*
 * Sedai Audio Foundation - Professional audio synthesis library
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * SedaiSIDEvo - Evolved SID-style synthesizer
 *
 * This module provides an evolved SID-inspired synthesizer that leverages
 * the Sedai Audio Foundation for high-quality audio synthesis.
 *
 * Specifications:
 * - 8 to 64 voices (activatable in groups of 8)
 * - 11 octaves (C0 to B10) covering full audible range 16Hz-20kHz
 * - Stereo output with per-voice panning
 * - Spatial positioning (3D-like effects)
 * - Per-voice multi-mode filters (LP, HP, BP, Notch, Peak)
 * - Extended waveforms beyond original SID
 * - Higher quality audio (44100 Hz, 32-bit float)
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

unit SedaiSIDEvo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  SedaiAudioTypes, SedaiAudioFoundation, SedaiClassicProcessor,
  SedaiFilters, SedaiStereoProcessor, SedaiADSRProcessor;

const
  // ============================================================================
  // SID EVO CONFIGURATION
  // ============================================================================

  // Voice configuration
  SIDEVO_VOICES_PER_GROUP = 8;      // Voices per group (fixed)
  SIDEVO_MAX_GROUPS = 8;            // Maximum groups (8x8=64 voices)
  SIDEVO_MAX_VOICES = SIDEVO_VOICES_PER_GROUP * SIDEVO_MAX_GROUPS; // 64 voices total
  SIDEVO_DEFAULT_GROUPS = 1;        // Default: 1 group = 8 voices

  // Frequency range (full audible spectrum)
  SIDEVO_NUM_OCTAVES = 11;          // C0 to B10 (covers 16Hz to ~20kHz)
  SIDEVO_NOTES_PER_OCTAVE = 12;
  SIDEVO_TOTAL_NOTES = SIDEVO_NUM_OCTAVES * SIDEVO_NOTES_PER_OCTAVE; // 132 notes

  // Frequency limits
  SIDEVO_MIN_FREQ = 16.35;          // C0 (~16 Hz)
  SIDEVO_MAX_FREQ = 19912.0;        // B10 (~20 kHz)

  // ============================================================================
  // WAVEFORM CONSTANTS
  // ============================================================================

  // Classic SID waveforms (bit flags, can be combined)
  SIDEVO_WAVE_NONE      = $00;
  SIDEVO_WAVE_TRIANGLE  = $01;      // Classic mellow tone
  SIDEVO_WAVE_SAWTOOTH  = $02;      // Rich harmonics, brassy
  SIDEVO_WAVE_PULSE     = $04;      // Square/PWM, classic chiptune
  SIDEVO_WAVE_NOISE     = $08;      // White noise for percussion

  // Extended waveforms (SIDEvo additions)
  SIDEVO_WAVE_SINE      = $10;      // Pure sine wave
  SIDEVO_WAVE_SUPERSAW  = $20;      // Multiple detuned saws (trance/EDM)
  SIDEVO_WAVE_PWM       = $40;      // Auto-modulating pulse width
  SIDEVO_WAVE_HALFSIN   = $80;      // Half-rectified sine (bass)

  // Extended waveforms (second byte for future expansion)
  SIDEVO_WAVE_FULLSIN   = $0100;    // Full-rectified sine (warm distortion)
  SIDEVO_WAVE_FORMANT   = $0200;    // Formant/vocal-like
  SIDEVO_WAVE_METALLIC  = $0400;    // Metallic/bell-like
  SIDEVO_WAVE_CUSTOM    = $8000;    // User-defined wavetable

  // ============================================================================
  // FILTER CONSTANTS
  // ============================================================================

  // Filter types (can be combined for complex responses)
  SIDEVO_FILTER_OFF      = $00;
  SIDEVO_FILTER_LOWPASS  = $01;     // Removes high frequencies
  SIDEVO_FILTER_HIGHPASS = $02;     // Removes low frequencies
  SIDEVO_FILTER_BANDPASS = $04;     // Passes only a band
  SIDEVO_FILTER_NOTCH    = $08;     // Removes a band (opposite of BP)
  SIDEVO_FILTER_PEAK     = $10;     // Boosts a frequency band
  SIDEVO_FILTER_ALLPASS  = $20;     // Phase shift only

  // Filter slopes
  SIDEVO_SLOPE_12DB = 0;            // 12 dB/octave (2-pole, gentle)
  SIDEVO_SLOPE_24DB = 1;            // 24 dB/octave (4-pole, classic analog)
  SIDEVO_SLOPE_48DB = 2;            // 48 dB/octave (8-pole, aggressive)

  // ============================================================================
  // CONTROL FLAGS
  // ============================================================================

  SIDEVO_FLAG_GATE      = $01;      // Voice is gated (playing)
  SIDEVO_FLAG_SYNC      = $02;      // Hard sync to another oscillator
  SIDEVO_FLAG_RING_MOD  = $04;      // Ring modulation enabled
  SIDEVO_FLAG_FILTER    = $08;      // Voice routed through global filter
  SIDEVO_FLAG_MUTE      = $10;      // Voice muted
  SIDEVO_FLAG_SOLO      = $20;      // Voice soloed

  // ============================================================================
  // SID AUTHENTICITY CONSTANTS
  // ============================================================================

  // SID chip characteristics
  SID_CLOCK_PAL         = 985248;   // PAL clock frequency (Hz)
  SID_CLOCK_NTSC        = 1022727;  // NTSC clock frequency (Hz)
  SID_ACCUMULATOR_BITS  = 24;       // Phase accumulator bits
  SID_OUTPUT_BITS       = 12;       // DAC output resolution
  SID_NOISE_BITS        = 23;       // LFSR noise register bits

  // SID authenticity levels
  SIDEVO_AUTH_EVOLVED   = 0;        // Full evolved mode (clean, hi-fi)
  SIDEVO_AUTH_HYBRID    = 1;        // Mix of evolved and SID characteristics
  SIDEVO_AUTH_SID_LIKE  = 2;        // More SID-like (for dump playback)
  SIDEVO_AUTH_SID_FULL  = 3;        // Maximum SID authenticity

type
  // ============================================================================
  // TYPE DEFINITIONS
  // ============================================================================

  // Extended waveform type (16-bit to support all waveforms)
  TSIDEvoWaveform = Word;

  // SID authenticity level
  TSIDEvoAuthenticityLevel = (
    salEvolved,                       // Clean, hi-fi evolved sound
    salHybrid,                        // Mix of evolved and SID characteristics
    salSIDLike,                       // More SID-like for SIDDump playback
    salSIDFull                        // Maximum SID authenticity
  );

  // SID clock mode (affects timing)
  TSIDClockMode = (
    scmPAL,                           // PAL (50Hz, 985248 Hz clock)
    scmNTSC                           // NTSC (60Hz, 1022727 Hz clock)
  );

  // SID oscillator state (for authentic waveform generation)
  TSIDOscillatorState = record
    Accumulator: LongWord;            // 24-bit phase accumulator
    NoiseShiftReg: LongWord;          // 23-bit LFSR for noise
    PreviousAccumMSB: Boolean;        // For noise clocking
    TestBit: Boolean;                 // Test bit for noise reset
  end;

  // Voice group state
  TSIDEvoGroupState = (
    sgsDisabled,                    // Group not active
    sgsEnabled,                     // Group active and ready
    sgsSuspended                    // Group temporarily suspended
  );

  // LFO target
  TSIDEvoLFOTarget = (
    ltNone,
    ltPitch,                        // Vibrato
    ltPulseWidth,                   // PWM
    ltFilterCutoff,                 // Filter sweep
    ltAmplitude,                    // Tremolo
    ltPan                           // Auto-pan
  );

  // SID Evo Voice state
  TSIDEvoVoice = record
    // Core parameters
    IsActive: Boolean;
    Frequency: Single;              // In Hz (16 Hz to 20 kHz)
    NoteNumber: Integer;            // 0-131 (11 octaves)
    Detune: Single;                 // -1.0 to 1.0 semitones

    // Waveform
    Waveform: TSIDEvoWaveform;      // SIDEVO_WAVE_xxx (can combine)
    PulseWidth: Single;             // 0.0 to 1.0 (for pulse wave)
    SupersawDetune: Single;         // 0.0 to 1.0 (for supersaw)
    SupersawMix: Single;            // 0.0 to 1.0 (center vs sides)

    // ADSR envelope
    Attack: Single;                 // 0.0 to 1.0 -> mapped to seconds
    Decay: Single;
    Sustain: Single;                // Level 0.0 to 1.0
    Release: Single;

    // Volume and pan
    Volume: Single;                 // 0.0 to 1.0
    Pan: Single;                    // -1.0 (left) to 1.0 (right)

    // Spatial positioning (3D-like)
    SpatialX: Single;               // -1.0 to 1.0 (left-right)
    SpatialY: Single;               // -1.0 to 1.0 (back-front)
    SpatialZ: Single;               // -1.0 to 1.0 (down-up)

    // Per-voice filter
    FilterEnabled: Boolean;
    FilterType: Byte;               // SIDEVO_FILTER_xxx
    FilterSlope: Byte;              // SIDEVO_SLOPE_xxx
    FilterCutoff: Single;           // 0.0 to 1.0 -> 20Hz to 20kHz
    FilterResonance: Single;        // 0.0 to 1.0 (Q factor)
    FilterEnvAmount: Single;        // -1.0 to 1.0 (envelope modulation)
    FilterKeyTrack: Single;         // 0.0 to 1.0 (keyboard tracking)

    // LFO (per-voice)
    LFORate: Single;                // 0.1 to 50 Hz
    LFODepth: Single;               // 0.0 to 1.0
    LFOTarget: TSIDEvoLFOTarget;
    LFOPhase: Single;               // Internal phase

    // Modulation
    Flags: Byte;                    // SIDEVO_FLAG_xxx
    RingModSource: Integer;         // Voice index for ring modulation
    SyncSource: Integer;            // Voice index for hard sync
    ModWheel: Single;               // 0.0 to 1.0 (modulation wheel)
    PitchBend: Single;              // -1.0 to 1.0 (pitch bend)

    // Internal state
    SynthVoiceIndex: Integer;       // Index in Sedai synthesis engine
    Filter: TMultiPoleFilter;       // Per-voice multi-pole filter
    Phase: Single;                  // Oscillator phase
    GroupIndex: Integer;            // Which group this voice belongs to
    LastWaveform: TSIDEvoWaveform;  // Track waveform changes for retrigger optimization

    // SID authenticity state
    SIDOsc: TSIDOscillatorState;    // SID-style oscillator state
  end;

  // Voice group
  TSIDEvoVoiceGroup = record
    State: TSIDEvoGroupState;
    Volume: Single;                 // Group volume multiplier
    Pan: Single;                    // Group pan offset
    Transpose: Integer;             // Semitones transposition
    VoiceOffset: Integer;           // First voice index in this group
  end;

  // Global SID Evo state
  TSIDEvoState = record
    // Global settings
    MasterVolume: Single;           // 0.0 to 1.0
    StereoWidth: Single;            // 0.0 (mono) to 2.0 (extra wide)
    Tuning: Single;                 // A4 frequency (default 440.0)

    // Voice groups
    ActiveGroups: Integer;          // Number of active groups (1-8)
    Groups: array[0..SIDEVO_MAX_GROUPS-1] of TSIDEvoVoiceGroup;

    // All voices
    Voices: array[0..SIDEVO_MAX_VOICES-1] of TSIDEvoVoice;

    // Global filter (like original SID)
    GlobalFilterEnabled: Boolean;
    GlobalFilterType: Byte;
    GlobalFilterSlope: Byte;
    GlobalFilterCutoff: Single;
    GlobalFilterResonance: Single;
    GlobalFilter: TMultiPoleFilter;

    // Global LFO
    GlobalLFORate: Single;
    GlobalLFODepth: Single;
    GlobalLFOTarget: TSIDEvoLFOTarget;
    GlobalLFOPhase: Single;

    // SID Authenticity settings
    AuthenticityLevel: TSIDEvoAuthenticityLevel;
    ClockMode: TSIDClockMode;
    ClockFrequency: LongWord;         // Actual clock frequency used
    EnableBitCrushing: Boolean;       // Reduce to 12-bit output
    EnableWaveformAND: Boolean;       // Enable SID-style waveform AND combination
    EnableLFSRNoise: Boolean;         // Use authentic 23-bit LFSR noise
    EnableSIDFilter: Boolean;         // Use SID-style filter characteristics
    FilterDistortion: Single;         // 0.0-1.0 filter distortion amount
    DACNonLinearity: Single;          // 0.0-1.0 DAC non-linearity
  end;

  { TSedaiSIDEvo }
  TSedaiSIDEvo = class
  private
    FState: TSIDEvoState;
    FInitialized: Boolean;
    FSampleRate: Single;
    FActiveVoiceCount: Integer;

    function MapADSRToSeconds(AValue: Single; AIsAttack: Boolean): Single;
    function MapCutoffToHz(ACutoff: Single): Single;
    function WaveformToSedaiType(AWaveform: TSIDEvoWaveform): TWaveType;
    function FilterTypeToSedai(AFilterType: Byte): TFilterType;
    function FilterSlopeToSedai(ASlope: Byte): TFilterSlope;
    procedure UpdateVoiceFilter(AVoiceIndex: Integer);
    procedure ApplySpatialEffect(AVoiceIndex: Integer; var ALeft, ARight: Single);
    function GetEffectiveVoiceCount: Integer;
    function IsVoiceInActiveGroup(AVoiceIndex: Integer): Boolean;

    // SID authenticity helpers
    procedure InitSIDOscillator(AVoiceIndex: Integer);
    function CalcSIDFrequencyReg(AFreqHz: Single): Word;
    function GenerateSIDWaveform(AVoiceIndex: Integer): Single;
    function GenerateSIDTriangle(AAccum: LongWord; ARingMod: Boolean; ASourceAccum: LongWord): Word;
    function GenerateSIDSawtooth(AAccum: LongWord): Word;
    function GenerateSIDPulse(AAccum: LongWord; APulseWidth: Word): Word;
    function GenerateSIDNoise(var AShiftReg: LongWord): Word;
    function CombineSIDWaveforms(AWaveform: TSIDEvoWaveform; ATri, ASaw, APulse, ANoise: Word): Word;
    function ApplyBitCrushing(ASample: Single): Single;
    function ApplyDACNonLinearity(ASample: Single): Single;
    procedure ClockSIDOscillator(AVoiceIndex: Integer);
    procedure UpdateAuthenticitySettings;

  public
    constructor Create;
    destructor Destroy; override;

    // ========================================================================
    // INITIALIZATION
    // ========================================================================

    function Initialize(AGroups: Integer = SIDEVO_DEFAULT_GROUPS): Boolean;
    procedure Shutdown;

    // Group management
    procedure SetActiveGroups(ACount: Integer);
    function GetActiveGroups: Integer;
    procedure EnableGroup(AGroupIndex: Integer);
    procedure DisableGroup(AGroupIndex: Integer);
    procedure SetGroupVolume(AGroupIndex: Integer; AVolume: Single);
    procedure SetGroupPan(AGroupIndex: Integer; APan: Single);
    procedure SetGroupTranspose(AGroupIndex: Integer; ASemitones: Integer);

    // ========================================================================
    // SID-STYLE REGISTER API (for compatibility)
    // ========================================================================

    procedure SetFrequencyReg(AVoice: Integer; AFreqReg: Word);
    procedure SetFrequencyHz(AVoice: Integer; AFreqHz: Single);
    procedure SetNote(AVoice: Integer; ANoteNumber: Integer);
    procedure SetDetune(AVoice: Integer; ADetune: Single);
    procedure SetPulseWidth(AVoice: Integer; APulseWidth: Single);
    procedure SetWaveform(AVoice: Integer; AWaveform: TSIDEvoWaveform);
    procedure SetControlFlags(AVoice: Integer; AFlags: Byte);

    // ADSR
    procedure SetADSR(AVoice: Integer; AAttack, ADecay, ASustain, ARelease: Single);
    procedure SetAttack(AVoice: Integer; AAttack: Single);
    procedure SetDecay(AVoice: Integer; ADecay: Single);
    procedure SetSustain(AVoice: Integer; ASustain: Single);
    procedure SetRelease(AVoice: Integer; ARelease: Single);

    // ========================================================================
    // FILTER API
    // ========================================================================

    // Per-voice filter
    procedure SetVoiceFilter(AVoice: Integer; AEnabled: Boolean);
    procedure SetVoiceFilterType(AVoice: Integer; AFilterType: Byte);
    procedure SetVoiceFilterSlope(AVoice: Integer; ASlope: Byte);
    procedure SetVoiceFilterCutoff(AVoice: Integer; ACutoff: Single);
    procedure SetVoiceFilterResonance(AVoice: Integer; AResonance: Single);
    procedure SetVoiceFilterEnvAmount(AVoice: Integer; AAmount: Single);
    procedure SetVoiceFilterKeyTrack(AVoice: Integer; AAmount: Single);

    // Global filter (like original SID)
    procedure SetGlobalFilter(AEnabled: Boolean);
    procedure SetGlobalFilterType(AFilterType: Byte);
    procedure SetGlobalFilterSlope(ASlope: Byte);
    procedure SetGlobalFilterCutoff(ACutoff: Single);
    procedure SetGlobalFilterResonance(AResonance: Single);
    procedure SetFilterRouting(AVoice: Integer; ARouteToGlobal: Boolean);

    // ========================================================================
    // LFO API
    // ========================================================================

    // Per-voice LFO
    procedure SetVoiceLFO(AVoice: Integer; ARate, ADepth: Single; ATarget: TSIDEvoLFOTarget);
    procedure SetVoiceLFORate(AVoice: Integer; ARate: Single);
    procedure SetVoiceLFODepth(AVoice: Integer; ADepth: Single);
    procedure SetVoiceLFOTarget(AVoice: Integer; ATarget: TSIDEvoLFOTarget);

    // Global LFO
    procedure SetGlobalLFO(ARate, ADepth: Single; ATarget: TSIDEvoLFOTarget);

    // ========================================================================
    // VOLUME AND PANNING
    // ========================================================================

    procedure SetVoiceVolume(AVoice: Integer; AVolume: Single);
    procedure SetVoiceMute(AVoice: Integer; AMuted: Boolean);
    function IsVoiceMuted(AVoice: Integer): Boolean;
    procedure SetMasterVolume(AVolume: Single);
    procedure SetVoicePan(AVoice: Integer; APan: Single);
    procedure SetStereoWidth(AWidth: Single);
    procedure SetTuning(AA4Freq: Single);

    // ========================================================================
    // SPATIAL POSITIONING
    // ========================================================================

    procedure SetVoicePosition(AVoice: Integer; AX, AY, AZ: Single);
    procedure SetVoicePositionXY(AVoice: Integer; AX, AY: Single);

    // ========================================================================
    // MODULATION
    // ========================================================================

    procedure SetRingModulation(AVoice: Integer; AEnabled: Boolean; ASourceVoice: Integer = -1);
    procedure SetHardSync(AVoice: Integer; AEnabled: Boolean; ASourceVoice: Integer = -1);
    procedure SetModWheel(AVoice: Integer; AValue: Single);
    procedure SetPitchBend(AVoice: Integer; AValue: Single);
    procedure SetGlobalPitchBend(AValue: Single);

    // ========================================================================
    // SUPERSAW PARAMETERS
    // ========================================================================

    procedure SetSupersawDetune(AVoice: Integer; ADetune: Single);
    procedure SetSupersawMix(AVoice: Integer; AMix: Single);

    // ========================================================================
    // PLAYBACK CONTROL
    // ========================================================================

    procedure GateOn(AVoice: Integer);
    procedure GateOff(AVoice: Integer);
    procedure AllGatesOff;
    procedure PlayNote(AVoice: Integer; ANoteNumber: Integer; AVolume: Single = 0.8);
    procedure PlayFrequency(AVoice: Integer; AFreqHz: Single; AVolume: Single = 0.8);
    procedure StopVoice(AVoice: Integer);
    procedure StopAll;
    procedure StopGroup(AGroupIndex: Integer);
    procedure Panic;  // Emergency stop all with fast release

    // Update active voice parameters (for SID-style real-time modulation)
    procedure UpdateActiveVoice(AVoice: Integer);

    // ========================================================================
    // BASIC-FRIENDLY HIGH-LEVEL API
    // ========================================================================

    procedure Sound(AVoice: Integer; AFreqHz: Single; ADurationMs: Integer);
    procedure Envelope(AVoice: Integer; AA, AD, AS_, AR: Single);
    procedure Wave(AVoice: Integer; AWaveform: TSIDEvoWaveform);
    procedure Filter(AVoice: Integer; AFilterType: Byte; ACutoff, AResonance: Single);
    procedure Pan(AVoice: Integer; APanPosition: Single);
    procedure Vol(AVoice: Integer; AVolume: Single); overload;
    procedure Vol(AVolume: Single); overload;
    procedure LFO(AVoice: Integer; ARate, ADepth: Single; ATarget: Integer);

    // ========================================================================
    // UTILITY AND QUERY
    // ========================================================================

    function IsVoiceActive(AVoice: Integer): Boolean;
    function GetVoiceCount: Integer;
    function GetMaxVoices: Integer;
    function NoteToFrequency(ANoteNumber: Integer): Single;
    function FrequencyToNote(AFreqHz: Single): Integer;
    function GetNoteName(ANoteNumber: Integer): string;

    // Status
    procedure PrintStatus;
    function GetActiveVoiceCount: Integer;
    function GetGroupInfo(AGroupIndex: Integer): string;

    // ========================================================================
    // SID AUTHENTICITY CONTROL
    // ========================================================================

    // Main authenticity control
    procedure SetAuthenticityLevel(ALevel: TSIDEvoAuthenticityLevel);
    function GetAuthenticityLevel: TSIDEvoAuthenticityLevel;

    // Clock mode (affects frequency calculations)
    procedure SetClockMode(AMode: TSIDClockMode);
    function GetClockMode: TSIDClockMode;

    // Individual authenticity features
    procedure SetBitCrushing(AEnabled: Boolean);
    procedure SetWaveformAND(AEnabled: Boolean);
    procedure SetLFSRNoise(AEnabled: Boolean);
    procedure SetSIDFilter(AEnabled: Boolean);

    // Fine-tuning
    procedure SetFilterDistortion(AAmount: Single);
    procedure SetDACNonLinearity(AAmount: Single);

    // Presets
    procedure SetSIDMode;             // Quick setup for SID dump playback
    procedure SetEvolvedMode;         // Quick setup for evolved mode

    // Properties
    property MasterVolume: Single read FState.MasterVolume write SetMasterVolume;
    property StereoWidth: Single read FState.StereoWidth write SetStereoWidth;
    property Tuning: Single read FState.Tuning write SetTuning;
    property Initialized: Boolean read FInitialized;
    property ActiveGroups: Integer read FState.ActiveGroups;
    property AuthenticityLevel: TSIDEvoAuthenticityLevel read FState.AuthenticityLevel write SetAuthenticityLevel;
    property ClockMode: TSIDClockMode read FState.ClockMode write SetClockMode;
  end;

// ============================================================================
// GLOBAL INSTANCE AND CONVENIENCE FUNCTIONS
// ============================================================================

var
  SIDEvo: TSedaiSIDEvo = nil;

// Initialization
procedure SIDEvoInit(AGroups: Integer = SIDEVO_DEFAULT_GROUPS);
procedure SIDEvoShutdown;

// Voice control
procedure SIDEvoSound(AVoice: Integer; AFreq: Single; ADurationMs: Integer);
procedure SIDEvoEnvelope(AVoice: Integer; AA, AD, AS_, AR: Single);
procedure SIDEvoWave(AVoice: Integer; AWaveform: TSIDEvoWaveform);
procedure SIDEvoFilter(AVoice: Integer; AType: Byte; ACutoff, AResonance: Single);
procedure SIDEvoPan(AVoice: Integer; APan: Single);
procedure SIDEvoVol(AVolume: Single); overload;
procedure SIDEvoVol(AVoice: Integer; AVolume: Single); overload;
procedure SIDEvoPlay(AVoice: Integer; ANote: Integer; AVolume: Single = 0.8);
procedure SIDEvoStop(AVoice: Integer);
procedure SIDEvoStopAll;

// Group control
procedure SIDEvoSetGroups(ACount: Integer);
procedure SIDEvoGroupVol(AGroup: Integer; AVolume: Single);
procedure SIDEvoGroupPan(AGroup: Integer; APan: Single);

// LFO
procedure SIDEvoLFO(AVoice: Integer; ARate, ADepth: Single; ATarget: Integer);

// SID Authenticity
procedure SIDEvoSetSIDMode;           // Enable SID-like mode for dump playback
procedure SIDEvoSetEvolvedMode;       // Enable evolved mode
procedure SIDEvoSetAuthenticity(ALevel: Integer);  // 0=Evolved, 3=Full SID
procedure SIDEvoSetClockMode(AMode: Integer);      // 0=PAL, 1=NTSC

implementation

const
  // ADSR timing mapping (SID-style exponential curves, extended range)
  ADSR_ATTACK_TIMES: array[0..15] of Single = (
    0.002, 0.008, 0.016, 0.024, 0.038, 0.056, 0.068, 0.080,
    0.100, 0.250, 0.500, 0.800, 1.000, 3.000, 5.000, 8.000
  );

  ADSR_DECAY_RELEASE_TIMES: array[0..15] of Single = (
    0.006, 0.024, 0.048, 0.072, 0.114, 0.168, 0.204, 0.240,
    0.300, 0.750, 1.500, 2.400, 3.000, 9.000, 15.00, 24.00
  );

  NOTE_NAMES: array[0..11] of string = (
    'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'
  );

{ TSedaiSIDEvo }

constructor TSedaiSIDEvo.Create;
var
  i, g: Integer;
begin
  inherited Create;
  FInitialized := False;
  FSampleRate := 44100.0;
  FActiveVoiceCount := 0;

  // Initialize state
  FillChar(FState, SizeOf(TSIDEvoState), 0);
  FState.MasterVolume := 0.8;
  FState.StereoWidth := 1.0;
  FState.Tuning := 440.0;
  FState.ActiveGroups := SIDEVO_DEFAULT_GROUPS;

  // Initialize groups
  for g := 0 to SIDEVO_MAX_GROUPS - 1 do
  begin
    FState.Groups[g].State := sgsDisabled;
    FState.Groups[g].Volume := 1.0;
    FState.Groups[g].Pan := 0.0;
    FState.Groups[g].Transpose := 0;
    FState.Groups[g].VoiceOffset := g * SIDEVO_VOICES_PER_GROUP;
  end;
  // Enable first group by default
  FState.Groups[0].State := sgsEnabled;

  // Initialize all voices
  for i := 0 to SIDEVO_MAX_VOICES - 1 do
  begin
    with FState.Voices[i] do
    begin
      IsActive := False;
      Frequency := 440.0;
      NoteNumber := 69;
      Detune := 0.0;
      Waveform := SIDEVO_WAVE_TRIANGLE;
      PulseWidth := 0.5;
      SupersawDetune := 0.3;
      SupersawMix := 0.5;
      Attack := 0.1;
      Decay := 0.1;
      Sustain := 0.8;
      Release := 0.3;
      Volume := 0.8;
      Pan := 0.0;
      SpatialX := 0.0;
      SpatialY := 0.0;
      SpatialZ := 0.0;
      FilterEnabled := False;
      FilterType := SIDEVO_FILTER_LOWPASS;
      FilterSlope := SIDEVO_SLOPE_24DB;
      FilterCutoff := 1.0;
      FilterResonance := 0.0;
      FilterEnvAmount := 0.0;
      FilterKeyTrack := 0.0;
      LFORate := 5.0;
      LFODepth := 0.0;
      LFOTarget := ltNone;
      LFOPhase := 0.0;
      Flags := 0;
      RingModSource := -1;
      SyncSource := -1;
      ModWheel := 0.0;
      PitchBend := 0.0;
      SynthVoiceIndex := -1;
      Phase := 0.0;
      GroupIndex := i div SIDEVO_VOICES_PER_GROUP;

      // Initialize SID oscillator state
      SIDOsc.Accumulator := 0;
      SIDOsc.NoiseShiftReg := $7FFFFF;  // Initial LFSR state (all 1s)
      SIDOsc.PreviousAccumMSB := False;
      SIDOsc.TestBit := False;
    end;
  end;

  // Initialize SID authenticity settings (default: evolved mode)
  FState.AuthenticityLevel := salEvolved;
  FState.ClockMode := scmPAL;
  FState.ClockFrequency := SID_CLOCK_PAL;
  FState.EnableBitCrushing := False;
  FState.EnableWaveformAND := False;
  FState.EnableLFSRNoise := False;
  FState.EnableSIDFilter := False;
  FState.FilterDistortion := 0.0;
  FState.DACNonLinearity := 0.0;
end;

destructor TSedaiSIDEvo.Destroy;
begin
  if FInitialized then
    Shutdown;
  inherited Destroy;
end;

function TSedaiSIDEvo.Initialize(AGroups: Integer): Boolean;
var
  AVoiceCount: Integer;
begin
  Result := False;

  if FInitialized then
  begin
    WriteLn('SIDEvo: Already initialized');
    Result := True;
    Exit;
  end;

  // Validate group count
  if AGroups < 1 then AGroups := 1;
  if AGroups > SIDEVO_MAX_GROUPS then AGroups := SIDEVO_MAX_GROUPS;

  AVoiceCount := AGroups * SIDEVO_VOICES_PER_GROUP;

  // Initialize Sedai Audio Foundation with enough voices
  if not InitAudio(AVoiceCount * 2) then
  begin
    WriteLn('SIDEvo ERROR: Failed to initialize audio system');
    Exit;
  end;

  FSampleRate := GetSampleRate;
  FInitialized := True;
  SetActiveGroups(AGroups);

  WriteLn('');
  WriteLn('SEDAI SID EVO - Evolved SID Synthesizer');
  WriteLn('------------------------------------------------------------------');
  WriteLn('Voice Groups: ', AGroups, ' (', AVoiceCount, ' voices total)');
  WriteLn('Octaves:      ', SIDEVO_NUM_OCTAVES, ' (C0 to B10, 16Hz-20kHz)');
  WriteLn('Sample Rate:  ', Round(FSampleRate), ' Hz');
  WriteLn('------------------------------------------------------------------');
  WriteLn('Waveforms: TRI, SAW, PULSE, NOISE, SINE, SUPERSAW, PWM...');
  WriteLn('Filters: LP, HP, BP, NOTCH, PEAK (12/24/48 dB/oct)');
  WriteLn('Features: Stereo, Spatial 3D, Per-voice LFO, Ring Mod');
  WriteLn('------------------------------------------------------------------');
  WriteLn('');

  Result := True;
end;

procedure TSedaiSIDEvo.Shutdown;
begin
  if not FInitialized then Exit;

  StopAll;
  ShutdownAudio;
  FInitialized := False;

  WriteLn('SIDEvo: Shutdown complete');
end;

// ============================================================================
// GROUP MANAGEMENT
// ============================================================================

procedure TSedaiSIDEvo.SetActiveGroups(ACount: Integer);
var
  i: Integer;
begin
  if ACount < 1 then ACount := 1;
  if ACount > SIDEVO_MAX_GROUPS then ACount := SIDEVO_MAX_GROUPS;

  FState.ActiveGroups := ACount;

  // Enable/disable groups
  for i := 0 to SIDEVO_MAX_GROUPS - 1 do
  begin
    if i < ACount then
      FState.Groups[i].State := sgsEnabled
    else
      FState.Groups[i].State := sgsDisabled;
  end;

  WriteLn('SIDEvo: Active groups set to ', ACount, ' (', ACount * SIDEVO_VOICES_PER_GROUP, ' voices)');
end;

function TSedaiSIDEvo.GetActiveGroups: Integer;
begin
  Result := FState.ActiveGroups;
end;

procedure TSedaiSIDEvo.EnableGroup(AGroupIndex: Integer);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
    FState.Groups[AGroupIndex].State := sgsEnabled;
end;

procedure TSedaiSIDEvo.DisableGroup(AGroupIndex: Integer);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
  begin
    StopGroup(AGroupIndex);
    FState.Groups[AGroupIndex].State := sgsDisabled;
  end;
end;

procedure TSedaiSIDEvo.SetGroupVolume(AGroupIndex: Integer; AVolume: Single);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
  begin
    if AVolume < 0.0 then AVolume := 0.0;
    if AVolume > 1.0 then AVolume := 1.0;
    FState.Groups[AGroupIndex].Volume := AVolume;
  end;
end;

procedure TSedaiSIDEvo.SetGroupPan(AGroupIndex: Integer; APan: Single);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
  begin
    if APan < -1.0 then APan := -1.0;
    if APan > 1.0 then APan := 1.0;
    FState.Groups[AGroupIndex].Pan := APan;
  end;
end;

procedure TSedaiSIDEvo.SetGroupTranspose(AGroupIndex: Integer; ASemitones: Integer);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
    FState.Groups[AGroupIndex].Transpose := ASemitones;
end;

function TSedaiSIDEvo.GetEffectiveVoiceCount: Integer;
begin
  Result := FState.ActiveGroups * SIDEVO_VOICES_PER_GROUP;
end;

function TSedaiSIDEvo.IsVoiceInActiveGroup(AVoiceIndex: Integer): Boolean;
var
  AGroupIndex: Integer;
begin
  AGroupIndex := AVoiceIndex div SIDEVO_VOICES_PER_GROUP;
  Result := (AGroupIndex >= 0) and (AGroupIndex < FState.ActiveGroups) and
            (FState.Groups[AGroupIndex].State = sgsEnabled);
end;

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

function TSedaiSIDEvo.MapADSRToSeconds(AValue: Single; AIsAttack: Boolean): Single;
var
  AIndex: Integer;
begin
  AIndex := Round(AValue * 15);
  if AIndex < 0 then AIndex := 0;
  if AIndex > 15 then AIndex := 15;

  if AIsAttack then
    Result := ADSR_ATTACK_TIMES[AIndex]
  else
    Result := ADSR_DECAY_RELEASE_TIMES[AIndex];
end;

function TSedaiSIDEvo.MapCutoffToHz(ACutoff: Single): Single;
begin
  // Logarithmic mapping: 0.0=20Hz, 1.0=20kHz
  Result := 20.0 * Power(1000.0, ACutoff);
  if Result > 20000.0 then Result := 20000.0;
  if Result < 20.0 then Result := 20.0;
end;

function TSedaiSIDEvo.WaveformToSedaiType(AWaveform: TSIDEvoWaveform): TWaveType;
begin
  // Priority order for combined waveforms
  if (AWaveform and SIDEVO_WAVE_NOISE) <> 0 then
    Result := wtNoise
  else if (AWaveform and SIDEVO_WAVE_SUPERSAW) <> 0 then
    Result := wtSawtooth  // Will be processed specially
  else if (AWaveform and SIDEVO_WAVE_PULSE) <> 0 then
    Result := wtSquare
  else if (AWaveform and SIDEVO_WAVE_PWM) <> 0 then
    Result := wtSquare
  else if (AWaveform and SIDEVO_WAVE_SAWTOOTH) <> 0 then
    Result := wtSawtooth
  else if (AWaveform and SIDEVO_WAVE_TRIANGLE) <> 0 then
    Result := wtTriangle
  else if (AWaveform and SIDEVO_WAVE_SINE) <> 0 then
    Result := wtSine
  else
    Result := wtSine;
end;

function TSedaiSIDEvo.FilterTypeToSedai(AFilterType: Byte): TFilterType;
begin
  if (AFilterType and SIDEVO_FILTER_NOTCH) <> 0 then
    Result := ftNotch
  else if (AFilterType and SIDEVO_FILTER_PEAK) <> 0 then
    Result := ftPeaking
  else if (AFilterType and SIDEVO_FILTER_BANDPASS) <> 0 then
    Result := ftBandPass
  else if (AFilterType and SIDEVO_FILTER_HIGHPASS) <> 0 then
    Result := ftHighPass
  else if (AFilterType and SIDEVO_FILTER_ALLPASS) <> 0 then
    Result := ftAllPass
  else
    Result := ftLowPass;
end;

function TSedaiSIDEvo.FilterSlopeToSedai(ASlope: Byte): TFilterSlope;
begin
  case ASlope of
    SIDEVO_SLOPE_12DB: Result := fs12dB;
    SIDEVO_SLOPE_24DB: Result := fs24dB;
    SIDEVO_SLOPE_48DB: Result := fs48dB;
  else
    Result := fs24dB;
  end;
end;

procedure TSedaiSIDEvo.UpdateVoiceFilter(AVoiceIndex: Integer);
var
  AFreqHz, AQ: Single;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= SIDEVO_MAX_VOICES) then Exit;

  with FState.Voices[AVoiceIndex] do
  begin
    if FilterEnabled then
    begin
      AFreqHz := MapCutoffToHz(FilterCutoff);

      // Apply keyboard tracking
      if FilterKeyTrack > 0 then
        AFreqHz := AFreqHz * Power(2.0, (NoteNumber - 60) * FilterKeyTrack / 12.0);

      // Q range: 0.707 (Butterworth, flat) to 20.0 (strong resonance, near self-oscillation)
      // Linear mapping for predictable response - SID style
      AQ := 0.707 + FilterResonance * 19.3;  // Q from 0.707 to 20.0

      // Create internal filter object for reference
      Filter := TSedaiFilters.CreateMultiPoleFilter(
        FilterTypeToSedai(FilterType),
        AFreqHz,
        AQ,
        FilterSlopeToSedai(FilterSlope),
        FSampleRate
      );

      // Also apply to the actual synthesis engine if voice is active
      if IsActive and (SynthVoiceIndex >= 0) then
      begin
        // Update filter parameters smoothly (preserves filter state, no clicks)
        SedaiAudioFoundation.SetVoiceFilterParams(SynthVoiceIndex, AFreqHz, AQ);
      end;
    end
    else
    begin
      // Disable filter in synthesis engine if voice is active
      if IsActive and (SynthVoiceIndex >= 0) then
        SedaiAudioFoundation.SetVoiceFilterEnabled(SynthVoiceIndex, False);
    end;
  end;
end;

procedure TSedaiSIDEvo.ApplySpatialEffect(AVoiceIndex: Integer; var ALeft, ARight: Single);
var
  ADistance, AAttenuation: Single;
begin
  with FState.Voices[AVoiceIndex] do
  begin
    TSedaiStereoProcessor.CalculatePan(SpatialX, (ALeft + ARight) * 0.5,
                                        ALeft, ARight, plConstantPower);

    ADistance := (1.0 - SpatialY) * 0.5 + 0.5;
    AAttenuation := 1.0 / (1.0 + (1.0 - ADistance) * 2.0);
    ALeft := ALeft * AAttenuation;
    ARight := ARight * AAttenuation;

    TSedaiStereoProcessor.ApplyStereoWidth(ALeft, ARight, FState.StereoWidth);
  end;
end;

// ============================================================================
// SID-STYLE REGISTER API
// ============================================================================

procedure TSedaiSIDEvo.SetFrequencyReg(AVoice: Integer; AFreqReg: Word);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  FState.Voices[AVoice].Frequency := AFreqReg * 0.0596;
end;

procedure TSedaiSIDEvo.SetFrequencyHz(AVoice: Integer; AFreqHz: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AFreqHz < SIDEVO_MIN_FREQ then AFreqHz := SIDEVO_MIN_FREQ;
  if AFreqHz > SIDEVO_MAX_FREQ then AFreqHz := SIDEVO_MAX_FREQ;
  FState.Voices[AVoice].Frequency := AFreqHz;
  FState.Voices[AVoice].NoteNumber := FrequencyToNote(AFreqHz);
end;

procedure TSedaiSIDEvo.SetNote(AVoice: Integer; ANoteNumber: Integer);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ANoteNumber < 0 then ANoteNumber := 0;
  if ANoteNumber >= SIDEVO_TOTAL_NOTES then ANoteNumber := SIDEVO_TOTAL_NOTES - 1;

  FState.Voices[AVoice].NoteNumber := ANoteNumber;
  FState.Voices[AVoice].Frequency := NoteToFrequency(ANoteNumber);
end;

procedure TSedaiSIDEvo.SetDetune(AVoice: Integer; ADetune: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ADetune < -1.0 then ADetune := -1.0;
  if ADetune > 1.0 then ADetune := 1.0;
  FState.Voices[AVoice].Detune := ADetune;
end;

procedure TSedaiSIDEvo.SetPulseWidth(AVoice: Integer; APulseWidth: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if APulseWidth < 0.0 then APulseWidth := 0.0;
  if APulseWidth > 1.0 then APulseWidth := 1.0;
  FState.Voices[AVoice].PulseWidth := APulseWidth;

  // Update synthesis engine immediately if voice is active
  with FState.Voices[AVoice] do
  begin
    if IsActive and (SynthVoiceIndex >= 0) and ((Waveform and SIDEVO_WAVE_PULSE) <> 0) then
      SedaiAudioFoundation.SetVoicePulseWidth(SynthVoiceIndex, APulseWidth);
  end;
end;

procedure TSedaiSIDEvo.SetWaveform(AVoice: Integer; AWaveform: TSIDEvoWaveform);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  FState.Voices[AVoice].Waveform := AWaveform;
end;

procedure TSedaiSIDEvo.SetControlFlags(AVoice: Integer; AFlags: Byte);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  FState.Voices[AVoice].Flags := AFlags;
end;

procedure TSedaiSIDEvo.SetADSR(AVoice: Integer; AAttack, ADecay, ASustain, ARelease: Single);
begin
  SetAttack(AVoice, AAttack);
  SetDecay(AVoice, ADecay);
  SetSustain(AVoice, ASustain);
  SetRelease(AVoice, ARelease);
end;

procedure TSedaiSIDEvo.SetAttack(AVoice: Integer; AAttack: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AAttack < 0.0 then AAttack := 0.0;
  if AAttack > 1.0 then AAttack := 1.0;
  FState.Voices[AVoice].Attack := AAttack;
end;

procedure TSedaiSIDEvo.SetDecay(AVoice: Integer; ADecay: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ADecay < 0.0 then ADecay := 0.0;
  if ADecay > 1.0 then ADecay := 1.0;
  FState.Voices[AVoice].Decay := ADecay;
end;

procedure TSedaiSIDEvo.SetSustain(AVoice: Integer; ASustain: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ASustain < 0.0 then ASustain := 0.0;
  if ASustain > 1.0 then ASustain := 1.0;
  FState.Voices[AVoice].Sustain := ASustain;
end;

procedure TSedaiSIDEvo.SetRelease(AVoice: Integer; ARelease: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ARelease < 0.0 then ARelease := 0.0;
  if ARelease > 1.0 then ARelease := 1.0;
  FState.Voices[AVoice].Release := ARelease;
end;

// ============================================================================
// FILTER API
// ============================================================================

procedure TSedaiSIDEvo.SetVoiceFilter(AVoice: Integer; AEnabled: Boolean);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  FState.Voices[AVoice].FilterEnabled := AEnabled;
  if AEnabled then UpdateVoiceFilter(AVoice);
end;

procedure TSedaiSIDEvo.SetVoiceFilterType(AVoice: Integer; AFilterType: Byte);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  FState.Voices[AVoice].FilterType := AFilterType;
  if FState.Voices[AVoice].FilterEnabled then UpdateVoiceFilter(AVoice);
end;

procedure TSedaiSIDEvo.SetVoiceFilterSlope(AVoice: Integer; ASlope: Byte);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  FState.Voices[AVoice].FilterSlope := ASlope;
  if FState.Voices[AVoice].FilterEnabled then UpdateVoiceFilter(AVoice);
end;

procedure TSedaiSIDEvo.SetVoiceFilterCutoff(AVoice: Integer; ACutoff: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ACutoff < 0.0 then ACutoff := 0.0;
  if ACutoff > 1.0 then ACutoff := 1.0;
  FState.Voices[AVoice].FilterCutoff := ACutoff;
  if FState.Voices[AVoice].FilterEnabled then UpdateVoiceFilter(AVoice);
end;

procedure TSedaiSIDEvo.SetVoiceFilterResonance(AVoice: Integer; AResonance: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AResonance < 0.0 then AResonance := 0.0;
  if AResonance > 1.0 then AResonance := 1.0;
  FState.Voices[AVoice].FilterResonance := AResonance;
  if FState.Voices[AVoice].FilterEnabled then UpdateVoiceFilter(AVoice);
end;

procedure TSedaiSIDEvo.SetVoiceFilterEnvAmount(AVoice: Integer; AAmount: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AAmount < -1.0 then AAmount := -1.0;
  if AAmount > 1.0 then AAmount := 1.0;
  FState.Voices[AVoice].FilterEnvAmount := AAmount;
end;

procedure TSedaiSIDEvo.SetVoiceFilterKeyTrack(AVoice: Integer; AAmount: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AAmount < 0.0 then AAmount := 0.0;
  if AAmount > 1.0 then AAmount := 1.0;
  FState.Voices[AVoice].FilterKeyTrack := AAmount;
end;

procedure TSedaiSIDEvo.SetGlobalFilter(AEnabled: Boolean);
begin
  FState.GlobalFilterEnabled := AEnabled;
end;

procedure TSedaiSIDEvo.SetGlobalFilterType(AFilterType: Byte);
begin
  FState.GlobalFilterType := AFilterType;
end;

procedure TSedaiSIDEvo.SetGlobalFilterSlope(ASlope: Byte);
begin
  FState.GlobalFilterSlope := ASlope;
end;

procedure TSedaiSIDEvo.SetGlobalFilterCutoff(ACutoff: Single);
begin
  if ACutoff < 0.0 then ACutoff := 0.0;
  if ACutoff > 1.0 then ACutoff := 1.0;
  FState.GlobalFilterCutoff := ACutoff;
end;

procedure TSedaiSIDEvo.SetGlobalFilterResonance(AResonance: Single);
begin
  if AResonance < 0.0 then AResonance := 0.0;
  if AResonance > 1.0 then AResonance := 1.0;
  FState.GlobalFilterResonance := AResonance;
end;

procedure TSedaiSIDEvo.SetFilterRouting(AVoice: Integer; ARouteToGlobal: Boolean);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ARouteToGlobal then
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags or SIDEVO_FLAG_FILTER
  else
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags and not SIDEVO_FLAG_FILTER;
end;

// ============================================================================
// LFO API
// ============================================================================

procedure TSedaiSIDEvo.SetVoiceLFO(AVoice: Integer; ARate, ADepth: Single; ATarget: TSIDEvoLFOTarget);
begin
  SetVoiceLFORate(AVoice, ARate);
  SetVoiceLFODepth(AVoice, ADepth);
  SetVoiceLFOTarget(AVoice, ATarget);
end;

procedure TSedaiSIDEvo.SetVoiceLFORate(AVoice: Integer; ARate: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ARate < 0.1 then ARate := 0.1;
  if ARate > 50.0 then ARate := 50.0;
  FState.Voices[AVoice].LFORate := ARate;
end;

procedure TSedaiSIDEvo.SetVoiceLFODepth(AVoice: Integer; ADepth: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ADepth < 0.0 then ADepth := 0.0;
  if ADepth > 1.0 then ADepth := 1.0;
  FState.Voices[AVoice].LFODepth := ADepth;
end;

procedure TSedaiSIDEvo.SetVoiceLFOTarget(AVoice: Integer; ATarget: TSIDEvoLFOTarget);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  FState.Voices[AVoice].LFOTarget := ATarget;
end;

procedure TSedaiSIDEvo.SetGlobalLFO(ARate, ADepth: Single; ATarget: TSIDEvoLFOTarget);
begin
  if ARate < 0.1 then ARate := 0.1;
  if ARate > 50.0 then ARate := 50.0;
  if ADepth < 0.0 then ADepth := 0.0;
  if ADepth > 1.0 then ADepth := 1.0;

  FState.GlobalLFORate := ARate;
  FState.GlobalLFODepth := ADepth;
  FState.GlobalLFOTarget := ATarget;
end;

// ============================================================================
// VOLUME AND PANNING
// ============================================================================

procedure TSedaiSIDEvo.SetVoiceVolume(AVoice: Integer; AVolume: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AVolume < 0.0 then AVolume := 0.0;
  if AVolume > 1.0 then AVolume := 1.0;
  FState.Voices[AVoice].Volume := AVolume;
end;

procedure TSedaiSIDEvo.SetVoiceMute(AVoice: Integer; AMuted: Boolean);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AMuted then
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags or SIDEVO_FLAG_MUTE
  else
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags and not SIDEVO_FLAG_MUTE;

  // If voice is active and now muted, stop it immediately
  if AMuted and FState.Voices[AVoice].IsActive then
    GateOff(AVoice);
end;

function TSedaiSIDEvo.IsVoiceMuted(AVoice: Integer): Boolean;
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then
    Result := False
  else
    Result := (FState.Voices[AVoice].Flags and SIDEVO_FLAG_MUTE) <> 0;
end;

procedure TSedaiSIDEvo.SetMasterVolume(AVolume: Single);
begin
  if AVolume < 0.0 then AVolume := 0.0;
  if AVolume > 1.0 then AVolume := 1.0;
  FState.MasterVolume := AVolume;
  if FInitialized then
    SedaiAudioFoundation.SetMasterVolume(AVolume);
end;

procedure TSedaiSIDEvo.SetVoicePan(AVoice: Integer; APan: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if APan < -1.0 then APan := -1.0;
  if APan > 1.0 then APan := 1.0;
  FState.Voices[AVoice].Pan := APan;
  FState.Voices[AVoice].SpatialX := APan;

  if FState.Voices[AVoice].IsActive and (FState.Voices[AVoice].SynthVoiceIndex >= 0) then
    SedaiAudioFoundation.SetVoicePan(FState.Voices[AVoice].SynthVoiceIndex, APan);
end;

procedure TSedaiSIDEvo.SetStereoWidth(AWidth: Single);
begin
  if AWidth < 0.0 then AWidth := 0.0;
  if AWidth > 2.0 then AWidth := 2.0;
  FState.StereoWidth := AWidth;
end;

procedure TSedaiSIDEvo.SetTuning(AA4Freq: Single);
begin
  if AA4Freq < 400.0 then AA4Freq := 400.0;
  if AA4Freq > 480.0 then AA4Freq := 480.0;
  FState.Tuning := AA4Freq;
end;

// ============================================================================
// SPATIAL POSITIONING
// ============================================================================

procedure TSedaiSIDEvo.SetVoicePosition(AVoice: Integer; AX, AY, AZ: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;

  if AX < -1.0 then AX := -1.0; if AX > 1.0 then AX := 1.0;
  if AY < -1.0 then AY := -1.0; if AY > 1.0 then AY := 1.0;
  if AZ < -1.0 then AZ := -1.0; if AZ > 1.0 then AZ := 1.0;

  FState.Voices[AVoice].SpatialX := AX;
  FState.Voices[AVoice].SpatialY := AY;
  FState.Voices[AVoice].SpatialZ := AZ;
  FState.Voices[AVoice].Pan := AX;
end;

procedure TSedaiSIDEvo.SetVoicePositionXY(AVoice: Integer; AX, AY: Single);
begin
  SetVoicePosition(AVoice, AX, AY, 0.0);
end;

// ============================================================================
// MODULATION
// ============================================================================

procedure TSedaiSIDEvo.SetRingModulation(AVoice: Integer; AEnabled: Boolean; ASourceVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;

  if AEnabled then
  begin
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags or SIDEVO_FLAG_RING_MOD;
    if (ASourceVoice >= 0) and (ASourceVoice < GetEffectiveVoiceCount) then
      FState.Voices[AVoice].RingModSource := ASourceVoice
    else
      FState.Voices[AVoice].RingModSource := (AVoice + GetEffectiveVoiceCount - 1) mod GetEffectiveVoiceCount;
  end
  else
  begin
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags and not SIDEVO_FLAG_RING_MOD;
    FState.Voices[AVoice].RingModSource := -1;
  end;
end;

procedure TSedaiSIDEvo.SetHardSync(AVoice: Integer; AEnabled: Boolean; ASourceVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;

  if AEnabled then
  begin
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags or SIDEVO_FLAG_SYNC;
    if (ASourceVoice >= 0) and (ASourceVoice < GetEffectiveVoiceCount) then
      FState.Voices[AVoice].SyncSource := ASourceVoice
    else
      FState.Voices[AVoice].SyncSource := (AVoice + GetEffectiveVoiceCount - 1) mod GetEffectiveVoiceCount;
  end
  else
  begin
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags and not SIDEVO_FLAG_SYNC;
    FState.Voices[AVoice].SyncSource := -1;
  end;
end;

procedure TSedaiSIDEvo.SetModWheel(AVoice: Integer; AValue: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 1.0 then AValue := 1.0;
  FState.Voices[AVoice].ModWheel := AValue;
end;

procedure TSedaiSIDEvo.SetPitchBend(AVoice: Integer; AValue: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AValue < -1.0 then AValue := -1.0;
  if AValue > 1.0 then AValue := 1.0;
  FState.Voices[AVoice].PitchBend := AValue;
end;

procedure TSedaiSIDEvo.SetGlobalPitchBend(AValue: Single);
var
  i: Integer;
begin
  for i := 0 to GetEffectiveVoiceCount - 1 do
    SetPitchBend(i, AValue);
end;

// ============================================================================
// SUPERSAW PARAMETERS
// ============================================================================

procedure TSedaiSIDEvo.SetSupersawDetune(AVoice: Integer; ADetune: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if ADetune < 0.0 then ADetune := 0.0;
  if ADetune > 1.0 then ADetune := 1.0;
  FState.Voices[AVoice].SupersawDetune := ADetune;
end;

procedure TSedaiSIDEvo.SetSupersawMix(AVoice: Integer; AMix: Single);
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if AMix < 0.0 then AMix := 0.0;
  if AMix > 1.0 then AMix := 1.0;
  FState.Voices[AVoice].SupersawMix := AMix;
end;

// ============================================================================
// PLAYBACK CONTROL
// ============================================================================

procedure TSedaiSIDEvo.GateOn(AVoice: Integer);
var
  APreset: string;
  AGroupIndex: Integer;
  AEffectivePan: Single;
  AAttackTime, ADecayTime, AReleaseTime: Single;
  AAttackIdx, ADecayIdx, AReleaseIdx: Integer;
  ANeedFullInit: Boolean;
begin
  if not FInitialized then Exit;
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;
  if not IsVoiceInActiveGroup(AVoice) then Exit;
  // Don't play muted voices
  if (FState.Voices[AVoice].Flags and SIDEVO_FLAG_MUTE) <> 0 then Exit;

  with FState.Voices[AVoice] do
  begin
    AGroupIndex := AVoice div SIDEVO_VOICES_PER_GROUP;

    // Check if we need full initialization or just retrigger
    // Full init needed if: first time OR waveform changed
    ANeedFullInit := (SynthVoiceIndex < 0) or (LastWaveform <> Waveform);

    // Convert 0.0-1.0 ADSR values to SID timing using lookup tables
    AAttackIdx := Round(Attack * 15);
    if AAttackIdx < 0 then AAttackIdx := 0;
    if AAttackIdx > 15 then AAttackIdx := 15;
    AAttackTime := ADSR_ATTACK_TIMES[AAttackIdx];

    ADecayIdx := Round(Decay * 15);
    if ADecayIdx < 0 then ADecayIdx := 0;
    if ADecayIdx > 15 then ADecayIdx := 15;
    ADecayTime := ADSR_DECAY_RELEASE_TIMES[ADecayIdx];

    AReleaseIdx := Round(Release * 15);
    if AReleaseIdx < 0 then AReleaseIdx := 0;
    if AReleaseIdx > 15 then AReleaseIdx := 15;
    AReleaseTime := ADSR_DECAY_RELEASE_TIMES[AReleaseIdx];

    // Track if voice was already active (for voice count)
    if not IsActive then
      Inc(FActiveVoiceCount);

    if ANeedFullInit then
    begin
      // Determine preset based on waveform
      case WaveformToSedaiType(Waveform) of
        wtSine:     APreset := 'sine';
        wtSquare:   APreset := 'square';
        wtSawtooth: APreset := 'saw';
        wtTriangle: APreset := 'triangle';
        wtNoise:    APreset := 'noise';
      else
        APreset := 'sine';
      end;

      // Full initialization: create preset and start attack
      PlayOnVoice(AVoice, Frequency, APreset);
      // Apply ADSR after preset init (preset has default ADSR)
      SedaiAudioFoundation.SetVoiceADSR(AVoice, AAttackTime, ADecayTime, Sustain, AReleaseTime);
      // Apply pulse width for square/pulse waves
      if (Waveform and SIDEVO_WAVE_PULSE) <> 0 then
        SedaiAudioFoundation.SetVoicePulseWidth(AVoice, PulseWidth);
      LastWaveform := Waveform;
    end
    else
    begin
      // Retrigger: update frequency and ADSR, then restart attack
      SedaiAudioFoundation.SetVoiceFrequency(AVoice, Frequency);
      // Apply ADSR BEFORE retrigger so new envelope uses correct values
      SedaiAudioFoundation.SetVoiceADSR(AVoice, AAttackTime, ADecayTime, Sustain, AReleaseTime);
      // Apply pulse width for square/pulse waves
      if (Waveform and SIDEVO_WAVE_PULSE) <> 0 then
        SedaiAudioFoundation.SetVoicePulseWidth(AVoice, PulseWidth);
      // Use hard retrigger for authentic SID behavior - always resets ADSR to zero
      // This is essential for proper trills and arpeggios
      SedaiAudioFoundation.RetriggerVoiceHard(AVoice);
    end;

    SynthVoiceIndex := AVoice;  // Fixed mapping
    IsActive := True;
    Flags := Flags or SIDEVO_FLAG_GATE;

    // Apply pan (voice pan + group pan offset)
    AEffectivePan := Pan + FState.Groups[AGroupIndex].Pan;
    if AEffectivePan < -1.0 then AEffectivePan := -1.0;
    if AEffectivePan > 1.0 then AEffectivePan := 1.0;
    SedaiAudioFoundation.SetVoicePan(AVoice, AEffectivePan);

    // Apply per-voice filter if enabled
    if FilterEnabled then
    begin
      SedaiAudioFoundation.SetVoiceFilter(
        AVoice,
        True,
        FilterTypeToSedai(FilterType),
        MapCutoffToHz(FilterCutoff),
        0.707 + FilterResonance * 19.3,  // Q from 0.707 to 20.0 (strong resonance)
        FilterSlopeToSedai(FilterSlope)
      );
    end
    else
      SedaiAudioFoundation.SetVoiceFilterEnabled(AVoice, False);
  end;
end;

procedure TSedaiSIDEvo.GateOff(AVoice: Integer);
begin
  if not FInitialized then Exit;
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;

  with FState.Voices[AVoice] do
  begin
    if IsActive and (SynthVoiceIndex >= 0) then
    begin
      // Start release phase (ADSR) - let the envelope fade naturally
      NoteOff(SynthVoiceIndex);
      // NOTE: Don't call ReleaseVoice here - it cuts sound immediately!
      // With fixed voice mapping (1:1), we reuse the same synth voice anyway
      Flags := Flags and not SIDEVO_FLAG_GATE;
      IsActive := False;
      Dec(FActiveVoiceCount);
      if FActiveVoiceCount < 0 then FActiveVoiceCount := 0;
    end;
  end;
end;

procedure TSedaiSIDEvo.AllGatesOff;
var
  i: Integer;
begin
  for i := 0 to GetEffectiveVoiceCount - 1 do
    GateOff(i);
end;

procedure TSedaiSIDEvo.UpdateActiveVoice(AVoice: Integer);
var
  AAttackTime, ADecayTime, AReleaseTime: Single;
  AAttackIdx, ADecayIdx, AReleaseIdx: Integer;
begin
  if not FInitialized then Exit;
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;

  with FState.Voices[AVoice] do
  begin
    if IsActive and (SynthVoiceIndex >= 0) then
    begin
      // Update frequency of active voice in real-time (for glissando, vibrato, etc.)
      SetVoiceFrequency(SynthVoiceIndex, Frequency);

      // Also update ADSR in case it changed (important for arpeggios and dynamic changes)
      AAttackIdx := Round(Attack * 15);
      if AAttackIdx < 0 then AAttackIdx := 0;
      if AAttackIdx > 15 then AAttackIdx := 15;
      AAttackTime := ADSR_ATTACK_TIMES[AAttackIdx];

      ADecayIdx := Round(Decay * 15);
      if ADecayIdx < 0 then ADecayIdx := 0;
      if ADecayIdx > 15 then ADecayIdx := 15;
      ADecayTime := ADSR_DECAY_RELEASE_TIMES[ADecayIdx];

      AReleaseIdx := Round(Release * 15);
      if AReleaseIdx < 0 then AReleaseIdx := 0;
      if AReleaseIdx > 15 then AReleaseIdx := 15;
      AReleaseTime := ADSR_DECAY_RELEASE_TIMES[AReleaseIdx];

      SedaiAudioFoundation.SetVoiceADSR(SynthVoiceIndex, AAttackTime, ADecayTime, Sustain, AReleaseTime);
    end;
  end;
end;

procedure TSedaiSIDEvo.PlayNote(AVoice: Integer; ANoteNumber: Integer; AVolume: Single);
var
  AGroupIndex: Integer;
  ATransposedNote: Integer;
begin
  if not FInitialized then Exit;
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;

  AGroupIndex := AVoice div SIDEVO_VOICES_PER_GROUP;

  // Apply group transposition
  ATransposedNote := ANoteNumber + FState.Groups[AGroupIndex].Transpose;
  if ATransposedNote < 0 then ATransposedNote := 0;
  if ATransposedNote >= SIDEVO_TOTAL_NOTES then ATransposedNote := SIDEVO_TOTAL_NOTES - 1;

  if FState.Voices[AVoice].IsActive then
    GateOff(AVoice);

  SetNote(AVoice, ATransposedNote);
  SetVoiceVolume(AVoice, AVolume * FState.Groups[AGroupIndex].Volume);
  GateOn(AVoice);
end;

procedure TSedaiSIDEvo.PlayFrequency(AVoice: Integer; AFreqHz: Single; AVolume: Single);
var
  AGroupIndex: Integer;
begin
  if not FInitialized then Exit;
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then Exit;

  AGroupIndex := AVoice div SIDEVO_VOICES_PER_GROUP;

  if FState.Voices[AVoice].IsActive then
    GateOff(AVoice);

  SetFrequencyHz(AVoice, AFreqHz);
  SetVoiceVolume(AVoice, AVolume * FState.Groups[AGroupIndex].Volume);
  GateOn(AVoice);
end;

procedure TSedaiSIDEvo.StopVoice(AVoice: Integer);
begin
  GateOff(AVoice);
  if (AVoice >= 0) and (AVoice < GetEffectiveVoiceCount) then
    FState.Voices[AVoice].IsActive := False;
end;

procedure TSedaiSIDEvo.StopAll;
begin
  AllGatesOff;
  if FInitialized then
    SedaiAudioFoundation.StopAll;
  FActiveVoiceCount := 0;
end;

procedure TSedaiSIDEvo.StopGroup(AGroupIndex: Integer);
var
  i, AStart, AEnd: Integer;
begin
  if (AGroupIndex < 0) or (AGroupIndex >= SIDEVO_MAX_GROUPS) then Exit;

  AStart := AGroupIndex * SIDEVO_VOICES_PER_GROUP;
  AEnd := AStart + SIDEVO_VOICES_PER_GROUP - 1;

  for i := AStart to AEnd do
    StopVoice(i);
end;

procedure TSedaiSIDEvo.Panic;
var
  i: Integer;
begin
  // Emergency stop all voices immediately
  for i := 0 to SIDEVO_MAX_VOICES - 1 do
  begin
    FState.Voices[i].IsActive := False;
    FState.Voices[i].Flags := 0;
  end;

  if FInitialized then
    SedaiAudioFoundation.StopAll;

  FActiveVoiceCount := 0;
  WriteLn('SIDEvo: PANIC - All voices stopped!');
end;

// ============================================================================
// BASIC-FRIENDLY HIGH-LEVEL API
// ============================================================================

procedure TSedaiSIDEvo.Sound(AVoice: Integer; AFreqHz: Single; ADurationMs: Integer);
begin
  PlayFrequency(AVoice, AFreqHz, FState.Voices[AVoice].Volume);
end;

procedure TSedaiSIDEvo.Envelope(AVoice: Integer; AA, AD, AS_, AR: Single);
begin
  SetADSR(AVoice, AA, AD, AS_, AR);
end;

procedure TSedaiSIDEvo.Wave(AVoice: Integer; AWaveform: TSIDEvoWaveform);
begin
  SetWaveform(AVoice, AWaveform);
end;

procedure TSedaiSIDEvo.Filter(AVoice: Integer; AFilterType: Byte; ACutoff, AResonance: Single);
begin
  SetVoiceFilterType(AVoice, AFilterType);
  SetVoiceFilterCutoff(AVoice, ACutoff);
  SetVoiceFilterResonance(AVoice, AResonance);
  SetVoiceFilter(AVoice, True);
end;

procedure TSedaiSIDEvo.Pan(AVoice: Integer; APanPosition: Single);
begin
  SetVoicePan(AVoice, APanPosition);
end;

procedure TSedaiSIDEvo.Vol(AVoice: Integer; AVolume: Single);
begin
  SetVoiceVolume(AVoice, AVolume);
end;

procedure TSedaiSIDEvo.Vol(AVolume: Single);
begin
  SetMasterVolume(AVolume);
end;

procedure TSedaiSIDEvo.LFO(AVoice: Integer; ARate, ADepth: Single; ATarget: Integer);
begin
  SetVoiceLFO(AVoice, ARate, ADepth, TSIDEvoLFOTarget(ATarget));
end;

// ============================================================================
// UTILITY AND QUERY
// ============================================================================

function TSedaiSIDEvo.IsVoiceActive(AVoice: Integer): Boolean;
begin
  if (AVoice < 0) or (AVoice >= GetEffectiveVoiceCount) then
    Result := False
  else
    Result := FState.Voices[AVoice].IsActive;
end;

function TSedaiSIDEvo.GetVoiceCount: Integer;
begin
  Result := GetEffectiveVoiceCount;
end;

function TSedaiSIDEvo.GetMaxVoices: Integer;
begin
  Result := SIDEVO_MAX_VOICES;
end;

function TSedaiSIDEvo.NoteToFrequency(ANoteNumber: Integer): Single;
begin
  // Using tuning reference (default A4=440Hz)
  // Note 0 = C0 (our system), MIDI 12 = C0
  // A4 = note 57 in our system (0-based from C0)
  Result := FState.Tuning * Power(2.0, (ANoteNumber - 57) / 12.0);
end;

function TSedaiSIDEvo.FrequencyToNote(AFreqHz: Single): Integer;
var
  ANoteFloat: Single;
begin
  if AFreqHz <= 0 then
  begin
    Result := 0;
    Exit;
  end;

  ANoteFloat := 12.0 * (Ln(AFreqHz / FState.Tuning) / Ln(2.0)) + 57;

  Result := Round(ANoteFloat);
  if Result < 0 then Result := 0;
  if Result >= SIDEVO_TOTAL_NOTES then Result := SIDEVO_TOTAL_NOTES - 1;
end;

function TSedaiSIDEvo.GetNoteName(ANoteNumber: Integer): string;
var
  AOctave, ANoteIndex: Integer;
begin
  if ANoteNumber < 0 then ANoteNumber := 0;
  if ANoteNumber >= SIDEVO_TOTAL_NOTES then ANoteNumber := SIDEVO_TOTAL_NOTES - 1;

  AOctave := ANoteNumber div 12;
  ANoteIndex := ANoteNumber mod 12;

  Result := NOTE_NAMES[ANoteIndex] + IntToStr(AOctave);
end;

procedure TSedaiSIDEvo.PrintStatus;
var
  i: Integer;
  AWaveNames: array[0..16] of string = ('---', 'TRI', 'SAW', 'T+S', 'PLS',
    'T+P', 'S+P', 'TSP', 'NOI', '---', '---', '---', '---', '---', '---', '---', 'SIN');
  AWaveName: string;
begin
  WriteLn('');
  WriteLn('SID EVO STATUS');
  WriteLn('----------------------------------------------');
  WriteLn('Initialized:    ', FInitialized);
  WriteLn('Master Volume:  ', FState.MasterVolume:0:2);
  WriteLn('Stereo Width:   ', FState.StereoWidth:0:2);
  WriteLn('Tuning (A4):    ', FState.Tuning:0:1, ' Hz');
  WriteLn('Active Groups:  ', FState.ActiveGroups, ' (', GetEffectiveVoiceCount, ' voices)');
  WriteLn('Active Voices:  ', FActiveVoiceCount);
  WriteLn('----------------------------------------------');

  for i := 0 to GetEffectiveVoiceCount - 1 do
  begin
    with FState.Voices[i] do
    begin
      if IsActive then
      begin
        if Waveform <= 16 then
          AWaveName := AWaveNames[Waveform]
        else
          AWaveName := IntToStr(Waveform);
        WriteLn('V', i:2, ': ', GetNoteName(NoteNumber):4, ' ', Frequency:7:1, ' Hz  ', AWaveName);
      end;
    end;
  end;

  WriteLn('----------------------------------------------');
  WriteLn('');
end;

function TSedaiSIDEvo.GetActiveVoiceCount: Integer;
begin
  Result := FActiveVoiceCount;
end;

function TSedaiSIDEvo.GetGroupInfo(AGroupIndex: Integer): string;
begin
  if (AGroupIndex < 0) or (AGroupIndex >= SIDEVO_MAX_GROUPS) then
  begin
    Result := 'Invalid group';
    Exit;
  end;

  with FState.Groups[AGroupIndex] do
  begin
    case State of
      sgsDisabled: Result := Format('Group %d: DISABLED', [AGroupIndex]);
      sgsEnabled: Result := Format('Group %d: ENABLED Vol=%.2f Pan=%.2f Trans=%d',
                                   [AGroupIndex, Volume, Pan, Transpose]);
      sgsSuspended: Result := Format('Group %d: SUSPENDED', [AGroupIndex]);
    end;
  end;
end;

// ============================================================================
// GLOBAL CONVENIENCE FUNCTIONS
// ============================================================================

procedure SIDEvoInit(AGroups: Integer);
begin
  if SIDEvo = nil then
    SIDEvo := TSedaiSIDEvo.Create;
  SIDEvo.Initialize(AGroups);
end;

procedure SIDEvoShutdown;
begin
  if SIDEvo <> nil then
  begin
    SIDEvo.Shutdown;
    FreeAndNil(SIDEvo);
  end;
end;

procedure SIDEvoSound(AVoice: Integer; AFreq: Single; ADurationMs: Integer);
begin
  if SIDEvo <> nil then
    SIDEvo.Sound(AVoice, AFreq, ADurationMs);
end;

procedure SIDEvoEnvelope(AVoice: Integer; AA, AD, AS_, AR: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.Envelope(AVoice, AA, AD, AS_, AR);
end;

procedure SIDEvoWave(AVoice: Integer; AWaveform: TSIDEvoWaveform);
begin
  if SIDEvo <> nil then
    SIDEvo.Wave(AVoice, AWaveform);
end;

procedure SIDEvoFilter(AVoice: Integer; AType: Byte; ACutoff, AResonance: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.Filter(AVoice, AType, ACutoff, AResonance);
end;

procedure SIDEvoPan(AVoice: Integer; APan: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.Pan(AVoice, APan);
end;

procedure SIDEvoVol(AVolume: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.Vol(AVolume);
end;

procedure SIDEvoVol(AVoice: Integer; AVolume: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.Vol(AVoice, AVolume);
end;

procedure SIDEvoPlay(AVoice: Integer; ANote: Integer; AVolume: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.PlayNote(AVoice, ANote, AVolume);
end;

procedure SIDEvoStop(AVoice: Integer);
begin
  if SIDEvo <> nil then
    SIDEvo.StopVoice(AVoice);
end;

procedure SIDEvoStopAll;
begin
  if SIDEvo <> nil then
    SIDEvo.StopAll;
end;

procedure SIDEvoSetGroups(ACount: Integer);
begin
  if SIDEvo <> nil then
    SIDEvo.SetActiveGroups(ACount);
end;

procedure SIDEvoGroupVol(AGroup: Integer; AVolume: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.SetGroupVolume(AGroup, AVolume);
end;

procedure SIDEvoGroupPan(AGroup: Integer; APan: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.SetGroupPan(AGroup, APan);
end;

procedure SIDEvoLFO(AVoice: Integer; ARate, ADepth: Single; ATarget: Integer);
begin
  if SIDEvo <> nil then
    SIDEvo.LFO(AVoice, ARate, ADepth, ATarget);
end;

procedure SIDEvoSetSIDMode;
begin
  if SIDEvo <> nil then
    SIDEvo.SetSIDMode;
end;

procedure SIDEvoSetEvolvedMode;
begin
  if SIDEvo <> nil then
    SIDEvo.SetEvolvedMode;
end;

procedure SIDEvoSetAuthenticity(ALevel: Integer);
begin
  if SIDEvo <> nil then
    SIDEvo.SetAuthenticityLevel(TSIDEvoAuthenticityLevel(ALevel));
end;

procedure SIDEvoSetClockMode(AMode: Integer);
begin
  if SIDEvo <> nil then
    SIDEvo.SetClockMode(TSIDClockMode(AMode));
end;

// ============================================================================
// SID AUTHENTICITY IMPLEMENTATION
// ============================================================================

procedure TSedaiSIDEvo.InitSIDOscillator(AVoiceIndex: Integer);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= SIDEVO_MAX_VOICES) then Exit;

  with FState.Voices[AVoiceIndex].SIDOsc do
  begin
    Accumulator := 0;
    NoiseShiftReg := $7FFFFF;  // All 23 bits set
    PreviousAccumMSB := False;
    TestBit := False;
  end;
end;

function TSedaiSIDEvo.CalcSIDFrequencyReg(AFreqHz: Single): Word;
begin
  // SID frequency register formula: FreqReg = (Freq * 16777216) / ClockFreq
  // For 16-bit register value
  Result := Round((AFreqHz * 16777216.0) / FState.ClockFrequency);
end;

function TSedaiSIDEvo.GenerateSIDTriangle(AAccum: LongWord; ARingMod: Boolean; ASourceAccum: LongWord): Word;
var
  AMSB: Boolean;
  AOutput: LongWord;
begin
  // SID triangle uses bits 11-23 of accumulator
  // If ring mod, XOR with MSB of modulation source

  if ARingMod then
    AMSB := ((AAccum xor ASourceAccum) and $800000) <> 0
  else
    AMSB := (AAccum and $800000) <> 0;

  // Upper 12 bits of accumulator
  AOutput := (AAccum shr 12) and $FFF;

  // Flip for triangle shape when MSB is set
  if AMSB then
    AOutput := AOutput xor $FFF;

  Result := Word(AOutput);
end;

function TSedaiSIDEvo.GenerateSIDSawtooth(AAccum: LongWord): Word;
begin
  // SID sawtooth: upper 12 bits of accumulator
  Result := Word((AAccum shr 12) and $FFF);
end;

function TSedaiSIDEvo.GenerateSIDPulse(AAccum: LongWord; APulseWidth: Word): Word;
begin
  // SID pulse: compare upper 12 bits with pulse width
  if ((AAccum shr 12) and $FFF) >= APulseWidth then
    Result := $FFF
  else
    Result := 0;
end;

function TSedaiSIDEvo.GenerateSIDNoise(var AShiftReg: LongWord): Word;
begin
  // SID noise: 23-bit LFSR with taps at bits 17 and 22
  // Output is bits 0, 2, 5, 9, 11, 14, 18, 20 combined into 8 bits, scaled to 12

  // The result uses selected bits from the shift register
  Result := Word(
    ((AShiftReg and $400000) shr 11) or  // bit 22 -> bit 11
    ((AShiftReg and $100000) shr 10) or  // bit 20 -> bit 10
    ((AShiftReg and $010000) shr 7) or   // bit 16 -> bit 9
    ((AShiftReg and $002000) shr 5) or   // bit 13 -> bit 8
    ((AShiftReg and $000800) shr 4) or   // bit 11 -> bit 7
    ((AShiftReg and $000080) shr 1) or   // bit 7 -> bit 6
    ((AShiftReg and $000010) shl 1) or   // bit 4 -> bit 5
    ((AShiftReg and $000004) shl 2)      // bit 2 -> bit 4
  ) shl 4;  // Scale 8-bit to 12-bit
end;

procedure TSedaiSIDEvo.ClockSIDOscillator(AVoiceIndex: Integer);
var
  AFreqReg: Word;
  ANewBit: LongWord;
  ACurrentMSB: Boolean;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= SIDEVO_MAX_VOICES) then Exit;

  with FState.Voices[AVoiceIndex] do
  begin
    // Calculate frequency register from Hz
    AFreqReg := CalcSIDFrequencyReg(Frequency);

    // Update accumulator (24-bit)
    SIDOsc.Accumulator := (SIDOsc.Accumulator + AFreqReg) and $FFFFFF;

    // Check for MSB transition (for noise clocking)
    ACurrentMSB := (SIDOsc.Accumulator and $080000) <> 0;  // Bit 19

    // Clock noise LFSR on bit 19 transition (high to low)
    if SIDOsc.PreviousAccumMSB and not ACurrentMSB then
    begin
      // LFSR feedback: XOR bits 17 and 22
      ANewBit := ((SIDOsc.NoiseShiftReg shr 17) xor (SIDOsc.NoiseShiftReg shr 22)) and 1;
      SIDOsc.NoiseShiftReg := ((SIDOsc.NoiseShiftReg shl 1) or ANewBit) and $7FFFFF;
    end;

    SIDOsc.PreviousAccumMSB := ACurrentMSB;

    // Test bit resets LFSR
    if SIDOsc.TestBit then
      SIDOsc.NoiseShiftReg := 0;
  end;
end;

function TSedaiSIDEvo.CombineSIDWaveforms(AWaveform: TSIDEvoWaveform; ATri, ASaw, APulse, ANoise: Word): Word;
var
  AResult: Word;
  ACount: Integer;
begin
  // SID combines waveforms using AND logic (not addition!)
  // This creates the distinctive "zeroed out" sound when combining waveforms

  AResult := $FFF;  // Start with all bits set
  ACount := 0;

  if (AWaveform and SIDEVO_WAVE_TRIANGLE) <> 0 then
  begin
    AResult := AResult and ATri;
    Inc(ACount);
  end;

  if (AWaveform and SIDEVO_WAVE_SAWTOOTH) <> 0 then
  begin
    AResult := AResult and ASaw;
    Inc(ACount);
  end;

  if (AWaveform and SIDEVO_WAVE_PULSE) <> 0 then
  begin
    AResult := AResult and APulse;
    Inc(ACount);
  end;

  if (AWaveform and SIDEVO_WAVE_NOISE) <> 0 then
  begin
    AResult := AResult and ANoise;
    Inc(ACount);
  end;

  // If no classic waveforms selected, return silence
  if ACount = 0 then
    Result := 0
  else
    Result := AResult;
end;

function TSedaiSIDEvo.GenerateSIDWaveform(AVoiceIndex: Integer): Single;
var
  ATri, ASaw, APulse, ANoise: Word;
  APulseWidthReg: Word;
  ACombined: Word;
  ARingMod: Boolean;
  ASourceAccum: LongWord;
begin
  Result := 0.0;
  if (AVoiceIndex < 0) or (AVoiceIndex >= SIDEVO_MAX_VOICES) then Exit;

  with FState.Voices[AVoiceIndex] do
  begin
    // Clock the oscillator
    ClockSIDOscillator(AVoiceIndex);

    // Check for ring modulation
    ARingMod := (Flags and SIDEVO_FLAG_RING_MOD) <> 0;
    if ARingMod and (RingModSource >= 0) and (RingModSource < SIDEVO_MAX_VOICES) then
      ASourceAccum := FState.Voices[RingModSource].SIDOsc.Accumulator
    else
      ASourceAccum := 0;

    // Generate individual waveforms
    ATri := GenerateSIDTriangle(SIDOsc.Accumulator, ARingMod, ASourceAccum);
    ASaw := GenerateSIDSawtooth(SIDOsc.Accumulator);

    // Convert pulse width (0.0-1.0 to 0-4095)
    APulseWidthReg := Round(PulseWidth * 4095);
    APulse := GenerateSIDPulse(SIDOsc.Accumulator, APulseWidthReg);

    ANoise := GenerateSIDNoise(SIDOsc.NoiseShiftReg);

    // Combine waveforms SID-style (AND) or evolved-style (mix)
    if FState.EnableWaveformAND then
      ACombined := CombineSIDWaveforms(Waveform, ATri, ASaw, APulse, ANoise)
    else
    begin
      // Evolved mode: additive mixing (normalized)
      ACombined := 0;
      if (Waveform and SIDEVO_WAVE_TRIANGLE) <> 0 then
        ACombined := ACombined + ATri;
      if (Waveform and SIDEVO_WAVE_SAWTOOTH) <> 0 then
        ACombined := ACombined + ASaw;
      if (Waveform and SIDEVO_WAVE_PULSE) <> 0 then
        ACombined := ACombined + APulse;
      if (Waveform and SIDEVO_WAVE_NOISE) <> 0 then
        ACombined := ACombined + ANoise;

      // Clamp to 12 bits
      if ACombined > $FFF then ACombined := $FFF;
    end;

    // Convert 12-bit unsigned to -1.0 to 1.0
    Result := (ACombined / 2048.0) - 1.0;
  end;
end;

function TSedaiSIDEvo.ApplyBitCrushing(ASample: Single): Single;
var
  AQuantized: Integer;
begin
  // Reduce to 12-bit resolution (like SID DAC)
  // Map -1.0..1.0 to 0..4095, quantize, map back
  AQuantized := Round((ASample + 1.0) * 2047.5);
  if AQuantized < 0 then AQuantized := 0;
  if AQuantized > 4095 then AQuantized := 4095;

  Result := (AQuantized / 2047.5) - 1.0;
end;

function TSedaiSIDEvo.ApplyDACNonLinearity(ASample: Single): Single;
var
  ANonLin: Single;
begin
  // Apply subtle non-linearity similar to SID DAC imperfections
  // This adds warmth and slight asymmetry

  ANonLin := FState.DACNonLinearity;
  if ANonLin <= 0.0 then
  begin
    Result := ASample;
    Exit;
  end;

  // Soft saturation with asymmetry
  if ASample >= 0 then
    Result := ASample - (ANonLin * ASample * ASample * 0.3)
  else
    Result := ASample + (ANonLin * ASample * ASample * 0.2);

  // Clamp
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

procedure TSedaiSIDEvo.UpdateAuthenticitySettings;
begin
  case FState.AuthenticityLevel of
    salEvolved:
    begin
      FState.EnableBitCrushing := False;
      FState.EnableWaveformAND := False;
      FState.EnableLFSRNoise := False;
      FState.EnableSIDFilter := False;
      FState.FilterDistortion := 0.0;
      FState.DACNonLinearity := 0.0;
    end;

    salHybrid:
    begin
      FState.EnableBitCrushing := False;
      FState.EnableWaveformAND := False;
      FState.EnableLFSRNoise := True;
      FState.EnableSIDFilter := False;
      FState.FilterDistortion := 0.2;
      FState.DACNonLinearity := 0.1;
    end;

    salSIDLike:
    begin
      FState.EnableBitCrushing := True;
      FState.EnableWaveformAND := True;
      FState.EnableLFSRNoise := True;
      FState.EnableSIDFilter := True;
      FState.FilterDistortion := 0.5;
      FState.DACNonLinearity := 0.3;
    end;

    salSIDFull:
    begin
      FState.EnableBitCrushing := True;
      FState.EnableWaveformAND := True;
      FState.EnableLFSRNoise := True;
      FState.EnableSIDFilter := True;
      FState.FilterDistortion := 0.8;
      FState.DACNonLinearity := 0.5;
    end;
  end;

  WriteLn('SIDEvo: Authenticity level set to ', Ord(FState.AuthenticityLevel));
end;

procedure TSedaiSIDEvo.SetAuthenticityLevel(ALevel: TSIDEvoAuthenticityLevel);
begin
  FState.AuthenticityLevel := ALevel;
  UpdateAuthenticitySettings;
end;

function TSedaiSIDEvo.GetAuthenticityLevel: TSIDEvoAuthenticityLevel;
begin
  Result := FState.AuthenticityLevel;
end;

procedure TSedaiSIDEvo.SetClockMode(AMode: TSIDClockMode);
begin
  FState.ClockMode := AMode;
  case AMode of
    scmPAL:  FState.ClockFrequency := SID_CLOCK_PAL;
    scmNTSC: FState.ClockFrequency := SID_CLOCK_NTSC;
  end;
  WriteLn('SIDEvo: Clock mode set to ', Ord(AMode), ' (', FState.ClockFrequency, ' Hz)');
end;

function TSedaiSIDEvo.GetClockMode: TSIDClockMode;
begin
  Result := FState.ClockMode;
end;

procedure TSedaiSIDEvo.SetBitCrushing(AEnabled: Boolean);
begin
  FState.EnableBitCrushing := AEnabled;
end;

procedure TSedaiSIDEvo.SetWaveformAND(AEnabled: Boolean);
begin
  FState.EnableWaveformAND := AEnabled;
end;

procedure TSedaiSIDEvo.SetLFSRNoise(AEnabled: Boolean);
begin
  FState.EnableLFSRNoise := AEnabled;
end;

procedure TSedaiSIDEvo.SetSIDFilter(AEnabled: Boolean);
begin
  FState.EnableSIDFilter := AEnabled;
end;

procedure TSedaiSIDEvo.SetFilterDistortion(AAmount: Single);
begin
  if AAmount < 0.0 then AAmount := 0.0;
  if AAmount > 1.0 then AAmount := 1.0;
  FState.FilterDistortion := AAmount;
end;

procedure TSedaiSIDEvo.SetDACNonLinearity(AAmount: Single);
begin
  if AAmount < 0.0 then AAmount := 0.0;
  if AAmount > 1.0 then AAmount := 1.0;
  FState.DACNonLinearity := AAmount;
end;

procedure TSedaiSIDEvo.SetSIDMode;
begin
  // Quick setup for SID dump playback - most authentic
  SetAuthenticityLevel(salSIDLike);
  SetClockMode(scmPAL);

  WriteLn('');
  WriteLn('+------------------------------------------------------------+');
  WriteLn('|       SID MODE ENABLED - Authentic SID Sound               |');
  WriteLn('+------------------------------------------------------------+');
  WriteLn('|  - 12-bit DAC emulation                                    |');
  WriteLn('|  - SID waveform combination (AND)                          |');
  WriteLn('|  - 23-bit LFSR noise generator                             |');
  WriteLn('|  - SID filter characteristics                              |');
  WriteLn('|  - PAL clock (985248 Hz)                                   |');
  WriteLn('+------------------------------------------------------------+');
  WriteLn('');
end;

procedure TSedaiSIDEvo.SetEvolvedMode;
begin
  // Quick setup for evolved mode - clean hi-fi sound
  SetAuthenticityLevel(salEvolved);

  WriteLn('');
  WriteLn('+------------------------------------------------------------+');
  WriteLn('|       EVOLVED MODE ENABLED - Hi-Fi Sound                   |');
  WriteLn('+------------------------------------------------------------+');
  WriteLn('|  - Full 32-bit float precision                             |');
  WriteLn('|  - Clean waveform mixing                                   |');
  WriteLn('|  - High-quality noise                                      |');
  WriteLn('|  - Linear filter response                                  |');
  WriteLn('|  - Extended waveforms available                            |');
  WriteLn('+------------------------------------------------------------+');
  WriteLn('');
end;

// ============================================================================
// FINALIZATION
// ============================================================================

initialization
  SIDEvo := nil;

finalization
  SIDEvoShutdown;

end.
