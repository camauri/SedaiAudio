{*
 * Sedai Audio Foundation - SID Evo (Evolved SID Emulation)
 *
 * TSedaiSIDEvo provides cycle-accurate emulation of the MOS 6581/8580
 * Sound Interface Device (SID) chip, enhanced with modern features.
 *
 * SID Authenticity Features (ReSID-style):
 * - 24-bit phase accumulator
 * - 23-bit LFSR noise generator
 * - Clock-accurate envelope with 15-bit rate counter and $8000 wraparound
 * - ADSR delay bug emulation
 * - Exponential counter periods (255, 93, 54, 26, 14, 6, 0)
 * - Two-Integrator-Loop filter (6581/8580 models)
 * - Combined waveform AND behavior
 * - Ring modulation and hard sync
 *
 * EVO Enhancements:
 * - 8 to 64 voices (activatable in groups of 8)
 * - 11 octaves (C0 to B10) covering 16Hz-20kHz
 * - Stereo output with per-voice panning
 * - Per-voice LFO (pitch, PWM, filter, amplitude, pan)
 * - Extended waveforms (Sine, Supersaw, PWM, etc.)
 * - Table-driven modulation (wavetable/pulsetable/filtertable)
 * - Higher quality audio (44100+ Hz, 32-bit float)
 *
 * Note: Spatial 3D audio is now handled by TSedaiSpatialAudio in SAF Core.
 *
 * Authenticity Levels:
 * - salEvolved: Clean hi-fi sound with all EVO features
 * - salHybrid: Mix of evolved and SID characteristics
 * - salSIDLike: More SID-like for tracker playback
 * - salSIDFull: Maximum SID authenticity (ReSID behavior)
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiSIDEvo;

{$mode objfpc}{$H+}
{$R-}  // Disable range checking - SID emulation uses intentional integer overflow

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiOscillator;

const
  // ============================================================================
  // SID CLOCK AND TIMING
  // ============================================================================
  SID_CLOCK_PAL = 985248;           // PAL clock frequency (Hz)
  SID_CLOCK_NTSC = 1022727;         // NTSC clock frequency (Hz)

  // ============================================================================
  // SID EVO CONFIGURATION
  // ============================================================================
  SIDEVO_VOICES_PER_GROUP = 8;      // Voices per group (fixed)
  SIDEVO_MAX_GROUPS = 8;            // Maximum groups (8x8=64 voices)
  SIDEVO_MAX_VOICES = SIDEVO_VOICES_PER_GROUP * SIDEVO_MAX_GROUPS; // 64 voices
  SIDEVO_DEFAULT_GROUPS = 1;        // Default: 1 group = 8 voices

  // Frequency range
  SIDEVO_NUM_OCTAVES = 11;          // C0 to B10
  SIDEVO_NOTES_PER_OCTAVE = 12;
  SIDEVO_TOTAL_NOTES = SIDEVO_NUM_OCTAVES * SIDEVO_NOTES_PER_OCTAVE; // 132 notes
  SIDEVO_MIN_FREQ = 16.35;          // C0 (~16 Hz)
  SIDEVO_MAX_FREQ = 19912.0;        // B10 (~20 kHz)

  // ============================================================================
  // WAVEFORM CONSTANTS
  // ============================================================================
  // Classic SID waveforms (bit flags, can be combined)
  SIDEVO_WAVE_NONE      = $00;
  SIDEVO_WAVE_TRIANGLE  = $01;
  SIDEVO_WAVE_SAWTOOTH  = $02;
  SIDEVO_WAVE_PULSE     = $04;
  SIDEVO_WAVE_NOISE     = $08;

  // Extended waveforms (SIDEvo additions)
  SIDEVO_WAVE_SINE      = $10;
  SIDEVO_WAVE_SUPERSAW  = $20;
  SIDEVO_WAVE_PWM       = $40;      // Auto-modulating pulse width
  SIDEVO_WAVE_HALFSIN   = $80;

  // SID register waveform bits
  SID_WAVE_TRIANGLE = $10;
  SID_WAVE_SAWTOOTH = $20;
  SID_WAVE_PULSE = $40;
  SID_WAVE_NOISE = $80;

  // ============================================================================
  // CONTROL FLAGS
  // ============================================================================
  SID_CTRL_GATE = $01;
  SID_CTRL_SYNC = $02;
  SID_CTRL_RINGMOD = $04;
  SID_CTRL_TEST = $08;

  SIDEVO_FLAG_GATE      = $01;
  SIDEVO_FLAG_SYNC      = $02;
  SIDEVO_FLAG_RING_MOD  = $04;
  SIDEVO_FLAG_FILTER    = $08;
  SIDEVO_FLAG_MUTE      = $10;
  SIDEVO_FLAG_SOLO      = $20;

  // ============================================================================
  // FILTER CONSTANTS
  // ============================================================================
  SID_FILT_LP = $10;
  SID_FILT_BP = $20;
  SID_FILT_HP = $40;
  SID_FILT_3OFF = $80;

  // User-friendly filter type constants (for BASIC-friendly API)
  SIDEVO_FILTER_OFF      = $00;
  SIDEVO_FILTER_LOWPASS  = $01;
  SIDEVO_FILTER_HIGHPASS = $02;
  SIDEVO_FILTER_BANDPASS = $04;
  SIDEVO_FILTER_NOTCH    = $08;
  SIDEVO_FILTER_PEAK     = $10;

  // ============================================================================
  // SID ADSR RATE COUNTER PERIODS (from ReSID)
  // Number of clock cycles between rate counter increments
  // Rate counter wraps at $8000 (32768)
  // ============================================================================
  SID_RATE_COUNTER_PERIODS: array[0..15] of Word = (
    9,      // 0:   2ms
    32,     // 1:   8ms
    63,     // 2:  16ms
    95,     // 3:  24ms
    149,    // 4:  38ms
    220,    // 5:  56ms
    267,    // 6:  68ms
    313,    // 7:  80ms
    392,    // 8: 100ms
    977,    // 9: 250ms
    1954,   // A: 500ms
    3126,   // B: 800ms
    3907,   // C:   1s
    11720,  // D:   3s
    19532,  // E:   5s
    31251   // F:   8s
  );

  // Rate counter MSB mask (bit 15) - used for ADSR delay bug handling
  // When this bit is set, ReSID increments again and masks to 15 bits
  SID_RATE_COUNTER_MSB = $8000;

  // Rate counter mask (15 bits)
  SID_RATE_COUNTER_MASK = $7FFF;

  // Sustain level lookup (4-bit value to 8-bit envelope level)
  SID_SUSTAIN_LEVELS: array[0..15] of Byte = (
    $00, $11, $22, $33, $44, $55, $66, $77,
    $88, $99, $AA, $BB, $CC, $DD, $EE, $FF
  );

  // ============================================================================
  // COMBINED WAVEFORM LOOKUP TABLES (ReSID OSC3 samples)
  // These tables contain 8-bit samples from real SID chips, indexed by 12-bit
  // accumulator value. Output is shifted left 4 bits to get 12-bit result.
  // ============================================================================
  {$I SedaiSIDEvo_WaveTables.inc}

type
  // ============================================================================
  // SID AUTHENTICITY TYPES
  // ============================================================================
  TSIDEvoAuthenticityLevel = (
    salEvolved,     // Clean, hi-fi evolved sound
    salHybrid,      // Mix of evolved and SID characteristics
    salSIDLike,     // More SID-like for tracker playback
    salSIDFull      // Maximum SID authenticity (ReSID behavior)
  );

  TSIDClockMode = (
    scmPAL,         // PAL (50Hz, 985248 Hz clock)
    scmNTSC         // NTSC (60Hz, 1022727 Hz clock)
  );

  // ============================================================================
  // SID ENVELOPE STATE (Clock-accurate, ReSID-style)
  // ============================================================================
  TSIDEnvelopeState = (
    sesAttack,        // Counting up from 0 to 255
    sesDecaySustain,  // Counting down from 255 to sustain level
    sesRelease,       // Counting down from current to 0
    sesFrozen         // Counter frozen at zero (hold-zero)
  );

  TSIDEnvelopeStateRec = record
    State: TSIDEnvelopeState;
    EnvelopeCounter: Byte;      // 8-bit envelope counter (0-255)
    RateCounter: Word;          // 15-bit rate counter
    RatePeriod: Word;           // Current rate period from table
    ExpCounter: Byte;           // Exponential counter (counts rate periods)
    ExpPeriod: Byte;            // Exponential period (1,2,4,8,16,30)
    Gate: Boolean;              // Gate bit (key on/off)
    Attack: Byte;               // Attack rate (0-15)
    Decay: Byte;                // Decay rate (0-15)
    Sustain: Byte;              // Sustain level (0-15)
    Release: Byte;              // Release rate (0-15)
    HoldZero: Boolean;          // ADSR delay bug: hold at zero
    EnvelopePipeline: Byte;     // Pipeline register (1-cycle delay)
  end;

  // ============================================================================
  // SID OSCILLATOR STATE
  // ============================================================================
  TSIDOscillatorState = record
    Accumulator: Cardinal;      // 24-bit phase accumulator
    ShiftRegister: Cardinal;    // 23-bit LFSR for noise
    PreviousAccumMSB: Boolean;  // For noise clocking (bit 19)
    PreviousSyncMSB: Boolean;   // For hard sync detection (bit 23)
    TestBit: Boolean;           // Test bit for noise reset
  end;

  // ============================================================================
  // SID FILTER STATE (Two-Integrator-Loop, ReSID-exact)
  // ============================================================================
  TSIDFilterState = record
    // Filter enabled (ReSID-exact: for testing)
    Enabled: Boolean;

    // Registers
    CutoffLo: Byte;             // 3 bits (FC low)
    CutoffHi: Byte;             // 8 bits (FC high)
    ResFilt: Byte;              // Resonance (4 bits) + filter routing (4 bits)
    ModeVol: Byte;              // Filter mode (4 bits) + volume (4 bits)

    // Internal state (Two-Integrator-Loop)
    Vhp: Integer;               // High-pass output (integer for ReSID compatibility)
    Vbp: Integer;               // Band-pass output
    Vlp: Integer;               // Low-pass output
    Vnf: Integer;               // Not filtered (bypass)

    // Filter coefficients (computed from chip model)
    W0: Integer;                // Cutoff frequency coefficient (fixed point)
    W0_ceil_1: Integer;         // Ceiling for 1-cycle stability
    W0_ceil_dt: Integer;        // Ceiling for delta_t stability
    _1024_div_Q: Integer;       // 1024/Q for resonance

    // Mixer DC offset (ReSID-exact)
    MixerDC: Integer;
  end;

  // ============================================================================
  // SID VOICE STATE (ReSID-exact DC offsets)
  // ============================================================================
  TSIDVoiceState = record
    WaveZero: Integer;          // Waveform D/A zero level
    VoiceDC: Integer;           // Multiplying D/A DC offset
  end;

  // ============================================================================
  // SID EXTERNAL FILTER STATE (ReSID-exact C64 output stage)
  // Low-pass:  R = 10kOhm, C = 1000pF; w0lp = 1/RC = 100000
  // High-pass: R =  1kOhm, C =   10uF; w0hp = 1/RC = 100
  // ============================================================================
  TSIDExternalFilterState = record
    Enabled: Boolean;
    Vlp: Integer;               // Low-pass state
    Vhp: Integer;               // High-pass state
    Vo: Integer;                // Output
    W0lp: Integer;              // Low-pass cutoff (scaled)
    W0hp: Integer;              // High-pass cutoff (scaled)
    MixerDC: Integer;           // Maximum mixer DC offset
  end;

  // ============================================================================
  // LFO TARGET
  // ============================================================================
  TSIDEvoLFOTarget = (
    ltNone,
    ltPitch,          // Vibrato
    ltPulseWidth,     // PWM
    ltFilterCutoff,   // Filter sweep
    ltAmplitude,      // Tremolo
    ltPan             // Auto-pan
  );

  // ============================================================================
  // TABLE-DRIVEN MODULATION (EVO FEATURE)
  // ============================================================================
  TSIDEvoTableCommand = (
    stcEnd,                     // End of table
    stcJump,                    // Jump to position
    stcDelay,                   // Wait N frames
    stcSetWaveform,             // Set waveform
    stcSetPulseWidth,           // Set pulse width (0-4095)
    stcPulseWidthUp,            // Increment pulse width
    stcPulseWidthDown,          // Decrement pulse width
    stcSetFilterCutoff,         // Set filter cutoff
    stcFilterCutoffUp,          // Increment filter cutoff
    stcFilterCutoffDown,        // Decrement filter cutoff
    stcSetADSR,                 // Set ADSR values
    stcPortaUp,                 // Portamento up
    stcPortaDown,               // Portamento down
    stcVibrato                  // Apply vibrato
  );

  TSIDEvoTableEntry = record
    Command: TSIDEvoTableCommand;
    Param1: Word;
    Param2: Word;
  end;

  TSIDEvoTableProgram = record
    Entries: array[0..255] of TSIDEvoTableEntry;
    Length: Integer;
    Enabled: Boolean;
  end;

  TSIDEvoTableState = record
    WaveTable: TSIDEvoTableProgram;
    WavePos: Integer;
    WaveDelay: Integer;
    PulseTable: TSIDEvoTableProgram;
    PulsePos: Integer;
    PulseDelay: Integer;
    FilterTable: TSIDEvoTableProgram;
    FilterPos: Integer;
    FilterDelay: Integer;
    VibratoSpeed: Word;
    VibratoDepth: Word;
    VibratoPhase: Single;
    PortaSpeed: Word;
    PortaTarget: Single;
  end;

  // ============================================================================
  // VOICE GROUP STATE
  // ============================================================================
  TSIDEvoGroupState = (
    sgsDisabled,
    sgsEnabled,
    sgsSuspended
  );

  TSIDEvoVoiceGroup = record
    State: TSIDEvoGroupState;
    Volume: Single;
    Pan: Single;
    Transpose: Integer;
    VoiceOffset: Integer;
  end;

  // ============================================================================
  // EXTENDED WAVEFORM TYPE
  // ============================================================================
  TSIDEvoWaveform = Word;

  // ============================================================================
  // SID EVO VOICE
  // ============================================================================
  TSIDEvoVoice = record
    // Core parameters
    IsActive: Boolean;
    Frequency: Single;          // In Hz
    NoteNumber: Integer;        // 0-131
    Detune: Single;             // -1.0 to 1.0 semitones

    // Waveform
    Waveform: TSIDEvoWaveform;
    PulseWidth: Single;         // 0.0 to 1.0
    SupersawDetune: Single;
    SupersawMix: Single;

    // ADSR (high-level, mapped to SID registers)
    Attack: Single;
    Decay: Single;
    Sustain: Single;
    Release: Single;

    // Volume and pan
    Volume: Single;
    Pan: Single;                // -1.0 (left) to 1.0 (right)

    // LFO
    LFORate: Single;
    LFODepth: Single;
    LFOTarget: TSIDEvoLFOTarget;
    LFOPhase: Single;

    // Modulation
    Flags: Byte;
    RingModSource: Integer;
    SyncSource: Integer;
    ModWheel: Single;
    PitchBend: Single;

    // Internal state
    SynthVoiceIndex: Integer;
    Phase: Single;              // For extended waveforms
    GroupIndex: Integer;
    LastWaveform: TSIDEvoWaveform;

    // SID authenticity state
    SIDOsc: TSIDOscillatorState;
    SIDEnv: TSIDEnvelopeStateRec;
    SIDVoice: TSIDVoiceState;     // ReSID-exact DC offsets
    TableState: TSIDEvoTableState;

    // SID register copies
    FreqLo: Byte;
    FreqHi: Byte;
    PWLo: Byte;
    PWHi: Byte;
    Control: Byte;
    AttackDecay: Byte;
    SustainRelease: Byte;
    WaveformOutput: Integer;    // 12-bit waveform output
  end;

  // ============================================================================
  // GLOBAL SID EVO STATE
  // ============================================================================
  TSIDEvoState = record
    MasterVolume: Single;
    StereoWidth: Single;
    Tuning: Single;             // A4 frequency (default 440.0)

    ActiveGroups: Integer;
    Groups: array[0..SIDEVO_MAX_GROUPS-1] of TSIDEvoVoiceGroup;
    Voices: array[0..SIDEVO_MAX_VOICES-1] of TSIDEvoVoice;

    // Global LFO
    GlobalLFORate: Single;
    GlobalLFODepth: Single;
    GlobalLFOTarget: TSIDEvoLFOTarget;
    GlobalLFOPhase: Single;

    // SID Authenticity
    AuthenticityLevel: TSIDEvoAuthenticityLevel;
    ClockMode: TSIDClockMode;
    ClockFrequency: Cardinal;
    EnableBitCrushing: Boolean;
    EnableWaveformAND: Boolean;
    EnableLFSRNoise: Boolean;
    DACNonLinearity: Single;
    EnableSIDEnvelope: Boolean;

    // Voice 3 disconnection
    Filter3Off: Boolean;
  end;

  { TSedaiSIDEvo }
  TSedaiSIDEvo = class(TSedaiAudioObject)
  private
    FState: TSIDEvoState;
    FFilter: TSIDFilterState;
    FExtFilter: TSIDExternalFilterState;  // ReSID-exact external filter
    FModel: TSIDModel;
    FCyclesPerSample: Single;
    FCycleAccum: Single;
    FSampleOffset: Int64;
    FInitialized: Boolean;

    // ReSID-exact bus value and external input
    FBusValue: Byte;              // Last value written to any SID register
    FBusValueTTL: Integer;        // Time to live for bus value ($2000 cycles)
    FExtIn: Integer;              // External audio input (20-bit, scaled)

    // TEST: Simple accumulators directly in class (not in FState record)
    FTestAcc0: Cardinal;
    FTestAcc1: Cardinal;
    FTestAcc2: Cardinal;

    // SOLUTION 1: Cached frequencies for current FRAME (read once per 882 samples)
    // This prevents race conditions when registers change mid-frame
    FCachedFreq: array[0..2] of Cardinal;
    FFrameSampleCount: Integer;  // Counts samples in current frame (0-881)
    FFrameCached: Boolean;       // True if frequencies cached for this frame

    // SOLUTION 2: Double buffer for all SID registers (optional)
    // Write buffer: player writes here
    // Read buffer: audio generation reads from here
    // Swap at frame boundary
    FDoubleBufferEnabled: Boolean;
    FWriteBuffer: array[0..2] of record
      FreqLo, FreqHi: Byte;
      PWLo, PWHi: Byte;
      Control: Byte;
      AttackDecay, SustainRelease: Byte;
    end;
    FReadBuffer: array[0..2] of record
      FreqLo, FreqHi: Byte;
      PWLo, PWHi: Byte;
      Control: Byte;
      AttackDecay, SustainRelease: Byte;
    end;

    // DAC tables
    FWaveformDAC: array[0..4095] of Integer;
    FEnvelopeDAC: array[0..255] of Integer;

    // ReSID-exact filter cutoff tables (integer for fixed-point math)
    FFilterF0_6581: array[0..2047] of Integer;
    FFilterF0_8580: array[0..2047] of Integer;
    FFilterF0: PInteger;  // Points to current table based on model

    // ReSID-exact combined waveform lookup tables
    // Pointers to current tables based on chip model (point to const arrays)
    FWave_ST: PByte;   // Points to current Sawtooth + Triangle table (4096 bytes)
    FWave_PT: PByte;   // Points to current Pulse + Triangle table (4096 bytes)
    FWave_PS: PByte;   // Points to current Pulse + Sawtooth table (4096 bytes)
    FWave_PST: PByte;  // Points to current Pulse + Sawtooth + Triangle table (4096 bytes)
    FWaveTablesLoaded: Boolean;  // True if wave tables are available

    // SAF Oscillators for extended waveforms (inherited from Sedai Audio Foundation)
    // Extended voices (3-63) use TSedaiOscillator for float-based waveforms
    FExtOscillators: array[0..SIDEVO_MAX_VOICES-1] of TSedaiOscillator;

    procedure InitializeTables;
    procedure InitializeVoice(AIndex: Integer);
    procedure InitializeFilter;

    // SID oscillator
    procedure ClockOscillator(AIndex: Integer);
    procedure SynchronizeOscillators;
    procedure ShiftNoiseRegister(AIndex: Integer);

    // SID waveform output (ReSID-exact)
    function OutputTriangle(AIndex: Integer): Integer;
    function OutputSawtooth(AIndex: Integer): Integer;
    function OutputPulse(AIndex: Integer): Integer;
    function OutputNoise(AIndex: Integer): Integer;
    function GetWaveformOutput12(AIndex: Integer): Integer;
    function GetVoiceOutput(AIndex: Integer): Integer;
    function CombineWaveforms(AWaveform: Byte; AAcc: Cardinal; APulse: Boolean;
      ARingMod: Boolean; ASyncAcc: Cardinal): Integer;

    // SID envelope (ReSID-style)
    procedure ClockEnvelope(AIndex: Integer);
    procedure SetEnvelopeGate(AIndex: Integer; AGate: Boolean);
    function GetExpPeriod(AEnvelopeCounter: Byte): Byte;

    // SID filter (ReSID-exact)
    procedure ClockFilter(Voice1, Voice2, Voice3, ExtIn: Integer);
    procedure UpdateFilterCoefficients;
    function FilterOutput: Integer;

    // External filter (ReSID-exact C64 output stage)
    procedure InitializeExternalFilter;
    procedure ClockExternalFilter(Vi: Integer);
    function ExternalFilterOutput: Integer;

    // Helpers
    function GetEffectiveVoiceCount: Integer;
    function IsVoiceInActiveGroup(AIndex: Integer): Boolean;
    function CalcFrequencyReg(AFreqHz: Single): Word;
    function GenerateExtendedWaveform(AIndex: Integer): Single;
    function ApplyBitCrushing(ASample: Single): Single;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure SampleRateChanged; override;

    // ========================================================================
    // INITIALIZATION
    // ========================================================================
    function Initialize(AGroups: Integer = SIDEVO_DEFAULT_GROUPS): Boolean;
    procedure Shutdown;

    // ========================================================================
    // SID REGISTER API (authentic)
    // ========================================================================
    procedure WriteRegister(AAddress: Byte; AValue: Byte);
    function ReadRegister(AAddress: Byte): Byte;

    // ========================================================================
    // VOICE API (EVO enhanced)
    // ========================================================================
    procedure SetFrequencyHz(AVoice: Integer; AFreqHz: Single);
    procedure SetFrequencyReg(AVoice: Integer; AFreqReg: Word);
    procedure SetNote(AVoice: Integer; ANoteNumber: Integer);
    procedure SetPulseWidth(AVoice: Integer; APulseWidth: Single);
    procedure SetWaveform(AVoice: Integer; AWaveform: TSIDEvoWaveform);
    procedure SetControlFlags(AVoice: Integer; AFlags: Byte);
    procedure SetADSR(AVoice: Integer; AAttack, ADecay, ASustain, ARelease: Single);
    procedure SetAttack(AVoice: Integer; AAttack: Single);
    procedure SetDecay(AVoice: Integer; ADecay: Single);
    procedure SetSustain(AVoice: Integer; ASustain: Single);
    procedure SetRelease(AVoice: Integer; ARelease: Single);
    procedure SetVoiceVolume(AVoice: Integer; AVolume: Single);
    procedure SetVoicePan(AVoice: Integer; APan: Single);
    procedure SetVoiceMute(AVoice: Integer; AMuted: Boolean);
    function IsVoiceMuted(AVoice: Integer): Boolean;

    // ========================================================================
    // PLAYBACK CONTROL
    // ========================================================================
    procedure GateOn(AVoice: Integer);
    procedure GateOff(AVoice: Integer);
    procedure AllGatesOff;
    procedure ResetVoiceEnvelope(AVoice: Integer);
    procedure PlayNote(AVoice: Integer; ANoteNumber: Integer; AVolume: Single = 0.8);
    procedure StopVoice(AVoice: Integer);
    procedure StopAll;

    // ========================================================================
    // FILTER API
    // ========================================================================
    procedure SetFilterCutoff(ACutoff: Word);  // 11-bit (0-2047)
    procedure SetFilterResonance(ARes: Byte);  // 0-15
    procedure SetFilterMode(ALP, ABP, AHP: Boolean);
    procedure SetFilterVoiceRouting(AVoice1, AVoice2, AVoice3, AExtIn: Boolean);

    // ReSID-exact: filter enable/disable (for testing)
    procedure EnableFilter(AEnable: Boolean);

    // ReSID-exact: external audio input (20-bit scaled value)
    procedure SetExternalInput(AValue: Integer);

    // SID register interface (authentic) - for direct register access
    procedure SetFilterFCLo(AValue: Byte);       // $D415 - Filter cutoff low (3 bits)
    procedure SetFilterFCHi(AValue: Byte);       // $D416 - Filter cutoff high (8 bits)
    procedure SetFilterResFilt(AValue: Byte);    // $D417 - Resonance / Voice routing
    procedure SetFilterModeVol(AValue: Byte);    // $D418 - Filter mode / Master volume
    procedure SetFilter3Off(ADisconnect: Boolean);

    // ========================================================================
    // GLOBAL SETTINGS
    // ========================================================================
    procedure SetMasterVolume(AVolume: Single);
    procedure SetAuthenticityLevel(ALevel: TSIDEvoAuthenticityLevel);
    function GetAuthenticityLevel: TSIDEvoAuthenticityLevel;
    procedure SetClockMode(AMode: TSIDClockMode);
    function GetClockMode: TSIDClockMode;
    procedure SetChipModel(AModel: TSIDModel);

    // ========================================================================
    // GROUP MANAGEMENT
    // ========================================================================
    procedure SetActiveGroups(ACount: Integer);
    function GetActiveGroups: Integer;
    procedure EnableGroup(AGroupIndex: Integer);
    procedure DisableGroup(AGroupIndex: Integer);
    procedure SetGroupVolume(AGroupIndex: Integer; AVolume: Single);
    procedure SetGroupPan(AGroupIndex: Integer; APan: Single);
    procedure SetGroupTranspose(AGroupIndex: Integer; ASemitones: Integer);
    procedure SetStereoWidth(AWidth: Single);

    // ========================================================================
    // AUDIO GENERATION
    // ========================================================================

    // Clock one cycle (authentic SID timing)
    procedure ClockCycle;

    // Generate one audio sample (call at output sample rate)
    function GenerateSample: Single;

    // Simple sample generation (no fractional accumulator - for tracker playback)
    function GenerateSampleSimple: Single;

    // Legacy sample generation (old simplified approach - clocks once per sample)
    // This matches the old architecture where oscillator/envelope are clocked
    // once per audio sample instead of ~22 times (ReSID-exact rate)
    function GenerateSampleLegacy: Single;

    // Raw sample generation (no filter, direct waveform + envelope output)
    // For testing: bypasses filter entirely to isolate jitter source
    function GenerateSampleRaw: Single;

    // No-loop sample generation (1 clock per sample with scaled frequencies)
    // This avoids the for loop that causes jitter, using frequency scaling instead
    function GenerateSampleNoLoop: Single;

    // Pure accumulator test - ONLY updates accumulator, no sync/noise/envelope
    // For isolating jitter source
    function GenerateSamplePure: Single;

    // Oscillator-only test - uses real SID oscillator with freq from registers
    // but NO filter, NO envelope - just raw sawtooth output
    // For isolating if oscillator or filter causes jitter
    function GenerateSampleOscOnly: Single;

    // Oscillator with FIXED frequency - uses ClockOscillator but forces freq
    // For isolating if register read causes jitter
    function GenerateSampleOscFixed: Single;

    // Oscillator reading directly from registers (no cache) - for debugging
    function GenerateSampleOscDirect: Single;

    // Generate stereo sample
    procedure GenerateStereoSample(out ALeft, ARight: Single);

    // Process one stereo sample - main rendering method
    procedure ProcessAudioStereo(out ALeftSample, ARightSample: Single);

    // Get current output without clocking
    function Output: Single;

    // Notify that a new frame has started (call AFTER writing registers)
    // This resets the frequency cache so the new values are picked up
    procedure NotifyNewFrame;

    // ReSID-style clock and generate
    function ClockAndGenerate(var ADeltaCycles: Integer; ABuffer: PSingle;
      AMaxSamples: Integer): Integer;

    // ========================================================================
    // DEBUG / TESTING API - Access internal state for verification
    // ========================================================================
    function GetAccumulator(AVoice: Integer): Cardinal;
    function GetShiftRegister(AVoice: Integer): Cardinal;
    function GetEnvelopeCounter(AVoice: Integer): Byte;
    function GetEnvelopeState(AVoice: Integer): Integer;
    function GetRateCounter(AVoice: Integer): Word;
    function GetVoiceOutputDebug(AVoice: Integer): Integer;  // Voice output before filter

    // ========================================================================
    // LFO API (EVO feature)
    // ========================================================================
    procedure SetVoiceLFO(AVoice: Integer; ARate, ADepth: Single; ATarget: TSIDEvoLFOTarget);
    procedure SetVoiceLFORate(AVoice: Integer; ARate: Single);
    procedure SetVoiceLFODepth(AVoice: Integer; ADepth: Single);
    procedure SetVoiceLFOTarget(AVoice: Integer; ATarget: TSIDEvoLFOTarget);
    procedure SetGlobalLFO(ARate, ADepth: Single; ATarget: TSIDEvoLFOTarget);

    // ========================================================================
    // MODULATION API
    // ========================================================================
    procedure SetRingModulation(AVoice: Integer; AEnabled: Boolean; ASourceVoice: Integer = -1);
    procedure SetHardSync(AVoice: Integer; AEnabled: Boolean; ASourceVoice: Integer = -1);
    procedure SetModWheel(AVoice: Integer; AValue: Single);
    procedure SetPitchBend(AVoice: Integer; AValue: Single);
    procedure SetGlobalPitchBend(AValue: Single);
    procedure SetSupersawDetune(AVoice: Integer; ADetune: Single);
    procedure SetSupersawMix(AVoice: Integer; AMix: Single);
    procedure SetDetune(AVoice: Integer; ADetune: Single);

    // ========================================================================
    // BASIC-FRIENDLY HIGH-LEVEL API
    // ========================================================================
    procedure Sound(AVoice: Integer; AFreqHz: Single; ADurationMs: Integer);
    procedure Envelope(AVoice: Integer; AA, AD, AS_, AR: Single);
    procedure Wave(AVoice: Integer; AWaveform: TSIDEvoWaveform);
    procedure Filter(AVoice: Integer; AFilterType: Byte; ACutoff, AResonance: Single);
    procedure Pan(AVoice: Integer; APanPosition: Single);
    procedure SetVol(AVoice: Integer; AVolume: Single); overload;
    procedure SetVol(AVolume: Single); overload;
    procedure LFO(AVoice: Integer; ARate, ADepth: Single; ATarget: Integer);

    // ========================================================================
    // PLAYBACK CONTROL (extended)
    // ========================================================================
    procedure PlayFrequency(AVoice: Integer; AFreqHz: Single; AVolume: Single = 0.8);
    procedure StopGroup(AGroupIndex: Integer);
    procedure Panic;
    procedure UpdateActiveVoice(AVoice: Integer);

    // ========================================================================
    // TABLE-DRIVEN MODULATION API (EVO feature)
    // ========================================================================
    procedure SetWaveTable(AVoice: Integer; const AEntries: array of TSIDEvoTableEntry);
    procedure EnableWaveTable(AVoice: Integer; AEnabled: Boolean);
    procedure ResetWaveTable(AVoice: Integer);
    procedure SetPulseTable(AVoice: Integer; const AEntries: array of TSIDEvoTableEntry);
    procedure EnablePulseTable(AVoice: Integer; AEnabled: Boolean);
    procedure ResetPulseTable(AVoice: Integer);
    procedure SetFilterTable(AVoice: Integer; const AEntries: array of TSIDEvoTableEntry);
    procedure EnableFilterTable(AVoice: Integer; AEnabled: Boolean);
    procedure ResetFilterTable(AVoice: Integer);
    procedure ProcessTables(AVoice: Integer);
    procedure ProcessAllTables;

    // ========================================================================
    // UTILITY
    // ========================================================================
    function GetEnvelopeLevel(AVoice: Integer): Byte;
    function GetWaveformOutput(AVoice: Integer): Integer;
    function NoteToFrequency(ANoteNumber: Integer): Single;
    function FrequencyToNote(AFreqHz: Single): Integer;
    function GetNoteName(ANoteNumber: Integer): string;
    function IsVoiceActive(AVoice: Integer): Boolean;
    function GetVoiceCount: Integer;
    function GetMaxVoices: Integer;
    function GetActiveVoiceCount: Integer;
    function GetGroupInfo(AGroupIndex: Integer): string;
    procedure PrintStatus;

    // ========================================================================
    // SID AUTHENTICITY CONTROL
    // ========================================================================
    procedure SetBitCrushing(AEnabled: Boolean);
    procedure SetWaveformAND(AEnabled: Boolean);
    procedure SetLFSRNoise(AEnabled: Boolean);
    procedure SetDACNonLinearity(AAmount: Single);
    procedure SetSIDMode;
    procedure SetEvolvedMode;
    procedure SetTuning(AA4Freq: Single);

    // ========================================================================
    // PROPERTIES
    // ========================================================================
    property Model: TSIDModel read FModel write SetChipModel;
    property Clock: Cardinal read FState.ClockFrequency write FState.ClockFrequency;
    property AuthenticityLevel: TSIDEvoAuthenticityLevel
      read FState.AuthenticityLevel write SetAuthenticityLevel;
    property ClockMode: TSIDClockMode read FState.ClockMode write SetClockMode;
    property MasterVolume: Single read FState.MasterVolume write SetMasterVolume;
    property StereoWidth: Single read FState.StereoWidth write SetStereoWidth;
    property Initialized: Boolean read FInitialized;
    property Tuning: Single read FState.Tuning write SetTuning;
    property ActiveGroups: Integer read FState.ActiveGroups;

    // Double buffer control (Solution 2)
    property DoubleBufferEnabled: Boolean read FDoubleBufferEnabled write FDoubleBufferEnabled;
    procedure SwapBuffers;  // Call at frame boundary to swap read/write buffers
    procedure CacheFrequencies;  // Solution 1: cache frequencies for current sample
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
procedure SIDEvoSetSIDMode;
procedure SIDEvoSetEvolvedMode;
procedure SIDEvoSetAuthenticity(ALevel: Integer);
procedure SIDEvoSetClockMode(AMode: Integer);

implementation

const
  // Note frequency table (A4 = 440Hz)
  NOTE_FREQUENCIES: array[0..131] of Single = (
    16.35, 17.32, 18.35, 19.45, 20.60, 21.83, 23.12, 24.50, 25.96, 27.50, 29.14, 30.87,  // C0-B0
    32.70, 34.65, 36.71, 38.89, 41.20, 43.65, 46.25, 49.00, 51.91, 55.00, 58.27, 61.74,  // C1-B1
    65.41, 69.30, 73.42, 77.78, 82.41, 87.31, 92.50, 98.00, 103.8, 110.0, 116.5, 123.5,  // C2-B2
    130.8, 138.6, 146.8, 155.6, 164.8, 174.6, 185.0, 196.0, 207.7, 220.0, 233.1, 246.9,  // C3-B3
    261.6, 277.2, 293.7, 311.1, 329.6, 349.2, 370.0, 392.0, 415.3, 440.0, 466.2, 493.9,  // C4-B4
    523.3, 554.4, 587.3, 622.3, 659.3, 698.5, 740.0, 784.0, 830.6, 880.0, 932.3, 987.8,  // C5-B5
    1047,  1109,  1175,  1245,  1319,  1397,  1480,  1568,  1661,  1760,  1865,  1976,   // C6-B6
    2093,  2217,  2349,  2489,  2637,  2794,  2960,  3136,  3322,  3520,  3729,  3951,   // C7-B7
    4186,  4435,  4699,  4978,  5274,  5588,  5920,  6272,  6645,  7040,  7459,  7902,   // C8-B8
    8372,  8870,  9397,  9956,  10548, 11175, 11840, 12544, 13290, 14080, 14917, 15804,  // C9-B9
    16744, 17740, 18795, 19912, 21096, 22351, 23680, 25088, 26580, 28160, 29834, 31609   // C10-B10
  );

{ TSedaiSIDEvo }

constructor TSedaiSIDEvo.Create;
var
  I, G: Integer;
begin
  inherited Create;

  FModel := smMOS6581;
  FInitialized := False;

  // Initialize state
  FillChar(FState, SizeOf(FState), 0);
  FState.MasterVolume := 0.0;  // Default OFF like C128 BASIC VOL command
  FState.StereoWidth := 1.0;
  FState.Tuning := 440.0;
  FState.ActiveGroups := SIDEVO_DEFAULT_GROUPS;
  FState.ClockMode := scmPAL;
  FState.ClockFrequency := SID_CLOCK_PAL;
  FState.AuthenticityLevel := salSIDFull;
  FState.EnableSIDEnvelope := True;
  FState.EnableLFSRNoise := True;
  FState.EnableWaveformAND := True;

  // Initialize groups
  for G := 0 to SIDEVO_MAX_GROUPS - 1 do
  begin
    FState.Groups[G].State := sgsDisabled;
    FState.Groups[G].Volume := 1.0;
    FState.Groups[G].Pan := 0.0;
    FState.Groups[G].Transpose := 0;
    FState.Groups[G].VoiceOffset := G * SIDEVO_VOICES_PER_GROUP;
  end;
  FState.Groups[0].State := sgsEnabled;

  // Initialize voices
  for I := 0 to SIDEVO_MAX_VOICES - 1 do
    InitializeVoice(I);

  // Create SAF oscillators for extended waveforms (voices 3-63)
  // These inherit waveform implementations from Sedai Audio Foundation
  for I := 0 to SIDEVO_MAX_VOICES - 1 do
  begin
    FExtOscillators[I] := TSedaiOscillator.Create;
    FExtOscillators[I].SetSampleRate(FSampleRate);
    FExtOscillators[I].Amplitude := 1.0;
    FExtOscillators[I].Bandlimited := True;
  end;

  // Initialize filter
  InitializeFilter;

  // Initialize tables
  InitializeTables;

  // Calculate cycles per sample
  FCyclesPerSample := FState.ClockFrequency / FSampleRate;
  FCycleAccum := 0;
  FSampleOffset := 0;

  // Initialize frame-level caching
  FFrameSampleCount := 0;
  FFrameCached := False;
end;

destructor TSedaiSIDEvo.Destroy;
var
  I: Integer;
begin
  Shutdown;
  // Free SAF oscillators
  for I := 0 to SIDEVO_MAX_VOICES - 1 do
    if Assigned(FExtOscillators[I]) then
      FExtOscillators[I].Free;
  inherited Destroy;
end;

// Type for spline control points
type
  TSplinePoint = array[0..1] of Integer;
  PSplinePointArray = ^TSplinePointArray;
  TSplinePointArray = array[0..63] of TSplinePoint;

// Cubic spline interpolation helper (ReSID-exact algorithm)
// Calculates coefficients a, b, c, d for cubic polynomial y = ax^3 + bx^2 + cx + d
procedure CubicCoefficients(x1, y1, x2, y2, k1, k2: Double; out a, b, c, d: Double);
var
  dx, dy: Double;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  a := ((k1 + k2) - 2 * dy / dx) / (dx * dx);
  b := ((k2 - k1) / dx - 3 * (x1 + x2) * a) / 2;
  c := k1 - (3 * x1 * a + 2 * b) * x1;
  d := y1 - ((x1 * a + b) * x1 + c) * x1;
end;

// ReSID-exact spline interpolation
// Fills the filter table using cubic spline interpolation between control points
procedure SplineInterpolate(Points: PSplinePointArray; PointCount: Integer;
                           Table: PInteger; TableSize: Integer);
var
  p0, p1, p2, p3: Integer;  // Point indices
  x0, y0, x1, y1, x2, y2, x3, y3: Double;  // Point coordinates
  k1, k2: Double;           // Tangents at p1 and p2
  a, b, c, d: Double;       // Cubic polynomial coefficients
  x, y: Double;
  xi: Integer;
  TablePtr: PInteger;
begin
  // Initialize table to zero
  TablePtr := Table;
  for xi := 0 to TableSize - 1 do
  begin
    TablePtr^ := 0;
    Inc(TablePtr);
  end;

  // Set up points for first curve segment
  p0 := 0;
  p1 := 1;
  p2 := 2;
  p3 := 3;

  // Draw each curve segment (between p1 and p2)
  // Note: loop until p2 reaches second-to-last point (pn-1), since we need 4 points
  while (p2 < PointCount) and (p3 < PointCount) do
  begin
    x0 := Points^[p0][0]; y0 := Points^[p0][1];
    x1 := Points^[p1][0]; y1 := Points^[p1][1];
    x2 := Points^[p2][0]; y2 := Points^[p2][1];
    x3 := Points^[p3][0]; y3 := Points^[p3][1];

    // Skip if p1 and p2 are the same (single point)
    if x1 = x2 then
    begin
      Inc(p0); Inc(p1); Inc(p2); Inc(p3);
      Continue;
    end;

    // Calculate tangents k1 and k2 based on neighboring points
    if (x0 = x1) and (x2 = x3) then
    begin
      // Both end points repeated: straight line
      k1 := (y2 - y1) / (x2 - x1);
      k2 := k1;
    end
    else if x0 = x1 then
    begin
      // p0 and p1 equal: use f''(x1) = 0
      k2 := (y3 - y1) / (x3 - x1);
      k1 := (3 * (y2 - y1) / (x2 - x1) - k2) / 2;
    end
    else if x2 = x3 then
    begin
      // p2 and p3 equal: use f''(x2) = 0
      k1 := (y2 - y0) / (x2 - x0);
      k2 := (3 * (y2 - y1) / (x2 - x1) - k1) / 2;
    end
    else
    begin
      // Normal curve: use Catmull-Rom style tangents
      k1 := (y2 - y0) / (x2 - x0);
      k2 := (y3 - y1) / (x3 - x1);
    end;

    // Calculate cubic polynomial coefficients
    CubicCoefficients(x1, y1, x2, y2, k1, k2, a, b, c, d);

    // Evaluate polynomial at each integer x in [x1, x2]
    for xi := Round(x1) to Round(x2) do
    begin
      if (xi >= 0) and (xi < TableSize) then
      begin
        x := xi;
        y := ((a * x + b) * x + c) * x + d;
        if y < 0 then y := 0;
        // ReSID uses: w0 = 2*pi*f*1.048576 (to facilitate /1000000 via >>20)
        TablePtr := Table;
        Inc(TablePtr, xi);
        TablePtr^ := Round(2 * PI * y * 1.048576);
      end;
    end;

    Inc(p0); Inc(p1); Inc(p2); Inc(p3);
  end;
end;

procedure TSedaiSIDEvo.InitializeTables;
const
  // ReSID 6581 filter cutoff spline points (FC -> Hz)
  // Non-linear with discontinuity at $80
  // Note: End points are repeated for spline boundary conditions
  F0_POINTS_6581: array[0..30, 0..1] of Integer = (
    (0, 220), (0, 220),           // Repeated start point
    (128, 230), (256, 250), (384, 300), (512, 420),
    (640, 780), (768, 1600), (832, 2300), (896, 3200),
    (960, 4300), (992, 5000), (1008, 5400), (1016, 5700),
    (1023, 6000), (1023, 6000),   // Discontinuity
    (1024, 4600), (1024, 4600),
    (1032, 4800), (1056, 5300), (1088, 6000), (1120, 6600),
    (1152, 7200), (1280, 9500), (1408, 12000), (1536, 14500),
    (1664, 16000), (1792, 17100), (1920, 17700),
    (2047, 18000), (2047, 18000)  // Repeated end point
  );
  // ReSID 8580 filter cutoff - more linear 0-12500 Hz
  F0_POINTS_8580: array[0..18, 0..1] of Integer = (
    (0, 0), (0, 0),               // Repeated start point
    (128, 800), (256, 1600), (384, 2500), (512, 3300),
    (640, 4100), (768, 4800), (896, 5600), (1024, 6500),
    (1152, 7500), (1280, 8400), (1408, 9200), (1536, 9800),
    (1664, 10500), (1792, 11000), (1920, 11700),
    (2047, 12500), (2047, 12500)  // Repeated end point
  );
var
  I: Integer;
  Points6581: PSplinePointArray;
  Points8580: PSplinePointArray;
begin
  // Waveform DAC (linear)
  for I := 0 to 4095 do
    FWaveformDAC[I] := I - 2048;

  // Envelope DAC
  for I := 0 to 255 do
    FEnvelopeDAC[I] := I;

  // Build 6581 filter cutoff table with ReSID-exact cubic spline interpolation
  Points6581 := @F0_POINTS_6581;
  SplineInterpolate(Points6581, Length(F0_POINTS_6581), @FFilterF0_6581[0], 2048);

  // Build 8580 filter cutoff table with ReSID-exact cubic spline interpolation
  Points8580 := @F0_POINTS_8580;
  SplineInterpolate(Points8580, Length(F0_POINTS_8580), @FFilterF0_8580[0], 2048);

  // Set current filter table based on model
  if FModel = smMOS6581 then
  begin
    FFilterF0 := @FFilterF0_6581[0];
    // ReSID-exact: Point to 6581 combined waveform tables
    FWave_ST := @WAVE6581_ST[0];
    FWave_PT := @WAVE6581_PT[0];
    FWave_PS := @WAVE6581_PS[0];
    FWave_PST := @WAVE6581_PST[0];
  end
  else
  begin
    FFilterF0 := @FFilterF0_8580[0];
    // ReSID-exact: Point to 8580 combined waveform tables
    FWave_ST := @WAVE8580_ST[0];
    FWave_PT := @WAVE8580_PT[0];
    FWave_PS := @WAVE8580_PS[0];
    FWave_PST := @WAVE8580_PST[0];
  end;
  FWaveTablesLoaded := True;
end;

procedure TSedaiSIDEvo.InitializeVoice(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= SIDEVO_MAX_VOICES) then Exit;

  with FState.Voices[AIndex] do
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
    Volume := 1.0;
    Pan := 0.0;
    LFORate := 5.0;
    LFODepth := 0.0;
    LFOTarget := ltNone;
    LFOPhase := 0.0;
    Flags := 0;
    RingModSource := -1;
    SyncSource := -1;
    ModWheel := 0.0;
    PitchBend := 0.0;
    Phase := 0.0;
    GroupIndex := AIndex div SIDEVO_VOICES_PER_GROUP;

    // SID register copies
    FreqLo := 0;
    FreqHi := 0;
    PWLo := 0;
    PWHi := 0;
    Control := 0;
    AttackDecay := 0;
    SustainRelease := 0;
    WaveformOutput := 0;

    // SID oscillator state (ReSID-exact)
    SIDOsc.Accumulator := 0;
    SIDOsc.ShiftRegister := $7FFFF8;  // ReSID-exact initial LFSR state
    SIDOsc.PreviousAccumMSB := False;
    SIDOsc.PreviousSyncMSB := False;
    SIDOsc.TestBit := False;

    // SID envelope state (ReSID-style)
    SIDEnv.State := sesRelease;  // ReSID starts in RELEASE, not frozen
    SIDEnv.EnvelopeCounter := 0;
    SIDEnv.RateCounter := 0;
    SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[0];
    SIDEnv.ExpCounter := 0;
    SIDEnv.ExpPeriod := 1;
    SIDEnv.Gate := False;
    SIDEnv.Attack := 0;
    SIDEnv.Decay := 0;
    SIDEnv.Sustain := 0;
    SIDEnv.Release := 0;
    SIDEnv.HoldZero := True;
    SIDEnv.EnvelopePipeline := 0;

    // SID voice DC offsets (ReSID-exact)
    // Set based on chip model
    if FModel = smMOS6581 then
    begin
      // 6581: wave_zero = 0x380, voice_DC = 0x800*0xff
      SIDVoice.WaveZero := $380;
      SIDVoice.VoiceDC := $800 * $FF;
    end
    else
    begin
      // 8580: wave_zero = 0x800, voice_DC = 0
      SIDVoice.WaveZero := $800;
      SIDVoice.VoiceDC := 0;
    end;

    // Table state
    FillChar(TableState, SizeOf(TableState), 0);
  end;
end;

procedure TSedaiSIDEvo.InitializeFilter;
begin
  FillChar(FFilter, SizeOf(FFilter), 0);
  FFilter.Enabled := True;  // ReSID-exact: filter enabled by default
  FFilter.ModeVol := $0F;   // Max volume, no filter

  // ReSID-exact filter initialization
  FFilter.Vhp := 0;
  FFilter.Vbp := 0;
  FFilter.Vlp := 0;
  FFilter.Vnf := 0;

  // Set mixer DC based on chip model (ReSID-exact)
  // 6581: mixer_DC = -0xfff*0xff/18 >> 7 = -1044225/18 >> 7 = -58012 >> 7 = -453
  // 8580: mixer_DC = 0
  // Note: Pascal shr is logical, so use div for negative numbers
  if FModel = smMOS6581 then
    FFilter.MixerDC := (-$FFF * $FF div 18) div 128  // ReSID-exact: -453
  else
    FFilter.MixerDC := 0;

  UpdateFilterCoefficients;

  // Initialize external filter (C64 output stage)
  InitializeExternalFilter;

  // Initialize bus value and ext_in
  FBusValue := 0;
  FBusValueTTL := 0;
  FExtIn := 0;
end;

procedure TSedaiSIDEvo.Reset;
var
  I: Integer;
begin
  inherited Reset;

  for I := 0 to SIDEVO_MAX_VOICES - 1 do
    InitializeVoice(I);

  InitializeFilter;
  FCycleAccum := 0;
  FSampleOffset := 0;
end;

procedure TSedaiSIDEvo.SampleRateChanged;
var
  I: Integer;
begin
  inherited SampleRateChanged;

  if FSampleRate > 0 then
    FCyclesPerSample := FState.ClockFrequency / FSampleRate
  else
    FCyclesPerSample := 1;

  // Update SAF oscillators sample rate
  for I := 0 to SIDEVO_MAX_VOICES - 1 do
    if Assigned(FExtOscillators[I]) then
      FExtOscillators[I].SetSampleRate(FSampleRate);
end;

function TSedaiSIDEvo.Initialize(AGroups: Integer): Boolean;
var
  G: Integer;
begin
  Result := False;

  if (AGroups < 1) or (AGroups > SIDEVO_MAX_GROUPS) then
    AGroups := SIDEVO_DEFAULT_GROUPS;

  FState.ActiveGroups := AGroups;

  // Enable requested groups
  for G := 0 to SIDEVO_MAX_GROUPS - 1 do
  begin
    if G < AGroups then
      FState.Groups[G].State := sgsEnabled
    else
      FState.Groups[G].State := sgsDisabled;
  end;

  FInitialized := True;
  Result := True;
end;

procedure TSedaiSIDEvo.Shutdown;
begin
  StopAll;
  FInitialized := False;
end;

// ============================================================================
// SID REGISTER API
// ============================================================================

procedure TSedaiSIDEvo.WriteRegister(AAddress: Byte; AValue: Byte);
const
  BUS_VALUE_TTL = $2000;  // ReSID-exact: bus value lasts $2000 (8192) cycles
var
  Voice: Integer;
begin
  // ReSID-exact: store bus value for read of write-only registers
  FBusValue := AValue;
  FBusValueTTL := BUS_VALUE_TTL;

  case AAddress of
    // Voice 1 (0-6)
    $00..$06:
      begin
        Voice := 0;
        case AAddress of
          $00: FState.Voices[Voice].FreqLo := AValue;
          $01: FState.Voices[Voice].FreqHi := AValue;
          $02: FState.Voices[Voice].PWLo := AValue;
          $03: FState.Voices[Voice].PWHi := AValue and $0F;
          $04:
            begin
              // Gate transition
              if ((AValue and SID_CTRL_GATE) <> 0) and
                 ((FState.Voices[Voice].Control and SID_CTRL_GATE) = 0) then
              begin
                SetEnvelopeGate(Voice, True);
              end
              else if ((AValue and SID_CTRL_GATE) = 0) and
                      ((FState.Voices[Voice].Control and SID_CTRL_GATE) <> 0) then
              begin
                SetEnvelopeGate(Voice, False);
              end;

              // Test bit (ReSID-exact behavior)
              if (AValue and SID_CTRL_TEST) <> 0 then
              begin
                // Test bit set: accumulator and shift register are cleared
                FState.Voices[Voice].SIDOsc.Accumulator := 0;
                FState.Voices[Voice].SIDOsc.ShiftRegister := 0;
                FState.Voices[Voice].SIDOsc.TestBit := True;
              end
              else if (FState.Voices[Voice].Control and SID_CTRL_TEST) <> 0 then
              begin
                // Test bit cleared: shift register is reset to 0x7ffff8
                FState.Voices[Voice].SIDOsc.ShiftRegister := $7FFFF8;
                FState.Voices[Voice].SIDOsc.TestBit := False;
              end
              else
                FState.Voices[Voice].SIDOsc.TestBit := False;

              FState.Voices[Voice].Control := AValue;
            end;
          $05:
            begin
              FState.Voices[Voice].AttackDecay := AValue;
              FState.Voices[Voice].SIDEnv.Attack := AValue shr 4;
              FState.Voices[Voice].SIDEnv.Decay := AValue and $0F;
              // ReSID-exact: update rate_period if currently in corresponding state
              if FState.Voices[Voice].SIDEnv.State = sesAttack then
                FState.Voices[Voice].SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[AValue shr 4]
              else if FState.Voices[Voice].SIDEnv.State = sesDecaySustain then
                FState.Voices[Voice].SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[AValue and $0F];
            end;
          $06:
            begin
              FState.Voices[Voice].SustainRelease := AValue;
              FState.Voices[Voice].SIDEnv.Sustain := AValue shr 4;
              FState.Voices[Voice].SIDEnv.Release := AValue and $0F;
              // ReSID-exact: update rate_period if currently in release state
              if FState.Voices[Voice].SIDEnv.State = sesRelease then
                FState.Voices[Voice].SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[AValue and $0F];
            end;
        end;
      end;

    // Voice 2 (7-D)
    $07..$0D:
      begin
        Voice := 1;
        case AAddress of
          $07: FState.Voices[Voice].FreqLo := AValue;
          $08: FState.Voices[Voice].FreqHi := AValue;
          $09: FState.Voices[Voice].PWLo := AValue;
          $0A: FState.Voices[Voice].PWHi := AValue and $0F;
          $0B:
            begin
              if ((AValue and SID_CTRL_GATE) <> 0) and
                 ((FState.Voices[Voice].Control and SID_CTRL_GATE) = 0) then
                SetEnvelopeGate(Voice, True)
              else if ((AValue and SID_CTRL_GATE) = 0) and
                      ((FState.Voices[Voice].Control and SID_CTRL_GATE) <> 0) then
                SetEnvelopeGate(Voice, False);

              // Test bit (ReSID-exact behavior)
              if (AValue and SID_CTRL_TEST) <> 0 then
              begin
                FState.Voices[Voice].SIDOsc.Accumulator := 0;
                FState.Voices[Voice].SIDOsc.ShiftRegister := 0;
                FState.Voices[Voice].SIDOsc.TestBit := True;
              end
              else if (FState.Voices[Voice].Control and SID_CTRL_TEST) <> 0 then
              begin
                FState.Voices[Voice].SIDOsc.ShiftRegister := $7FFFF8;
                FState.Voices[Voice].SIDOsc.TestBit := False;
              end
              else
                FState.Voices[Voice].SIDOsc.TestBit := False;

              FState.Voices[Voice].Control := AValue;
            end;
          $0C:
            begin
              FState.Voices[Voice].AttackDecay := AValue;
              FState.Voices[Voice].SIDEnv.Attack := AValue shr 4;
              FState.Voices[Voice].SIDEnv.Decay := AValue and $0F;
              // ReSID-exact: update rate_period if currently in corresponding state
              if FState.Voices[Voice].SIDEnv.State = sesAttack then
                FState.Voices[Voice].SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[AValue shr 4]
              else if FState.Voices[Voice].SIDEnv.State = sesDecaySustain then
                FState.Voices[Voice].SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[AValue and $0F];
            end;
          $0D:
            begin
              FState.Voices[Voice].SustainRelease := AValue;
              FState.Voices[Voice].SIDEnv.Sustain := AValue shr 4;
              FState.Voices[Voice].SIDEnv.Release := AValue and $0F;
              // ReSID-exact: update rate_period if currently in release state
              if FState.Voices[Voice].SIDEnv.State = sesRelease then
                FState.Voices[Voice].SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[AValue and $0F];
            end;
        end;
      end;

    // Voice 3 (E-14)
    $0E..$14:
      begin
        Voice := 2;
        case AAddress of
          $0E: FState.Voices[Voice].FreqLo := AValue;
          $0F: FState.Voices[Voice].FreqHi := AValue;
          $10: FState.Voices[Voice].PWLo := AValue;
          $11: FState.Voices[Voice].PWHi := AValue and $0F;
          $12:
            begin
              if ((AValue and SID_CTRL_GATE) <> 0) and
                 ((FState.Voices[Voice].Control and SID_CTRL_GATE) = 0) then
                SetEnvelopeGate(Voice, True)
              else if ((AValue and SID_CTRL_GATE) = 0) and
                      ((FState.Voices[Voice].Control and SID_CTRL_GATE) <> 0) then
                SetEnvelopeGate(Voice, False);

              // Test bit (ReSID-exact behavior)
              if (AValue and SID_CTRL_TEST) <> 0 then
              begin
                FState.Voices[Voice].SIDOsc.Accumulator := 0;
                FState.Voices[Voice].SIDOsc.ShiftRegister := 0;
                FState.Voices[Voice].SIDOsc.TestBit := True;
              end
              else if (FState.Voices[Voice].Control and SID_CTRL_TEST) <> 0 then
              begin
                FState.Voices[Voice].SIDOsc.ShiftRegister := $7FFFF8;
                FState.Voices[Voice].SIDOsc.TestBit := False;
              end
              else
                FState.Voices[Voice].SIDOsc.TestBit := False;

              FState.Voices[Voice].Control := AValue;
            end;
          $13:
            begin
              FState.Voices[Voice].AttackDecay := AValue;
              FState.Voices[Voice].SIDEnv.Attack := AValue shr 4;
              FState.Voices[Voice].SIDEnv.Decay := AValue and $0F;
              // ReSID-exact: update rate_period if currently in corresponding state
              if FState.Voices[Voice].SIDEnv.State = sesAttack then
                FState.Voices[Voice].SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[AValue shr 4]
              else if FState.Voices[Voice].SIDEnv.State = sesDecaySustain then
                FState.Voices[Voice].SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[AValue and $0F];
            end;
          $14:
            begin
              FState.Voices[Voice].SustainRelease := AValue;
              FState.Voices[Voice].SIDEnv.Sustain := AValue shr 4;
              FState.Voices[Voice].SIDEnv.Release := AValue and $0F;
              // ReSID-exact: update rate_period if currently in release state
              if FState.Voices[Voice].SIDEnv.State = sesRelease then
                FState.Voices[Voice].SIDEnv.RatePeriod := SID_RATE_COUNTER_PERIODS[AValue and $0F];
            end;
        end;
      end;

    // Filter registers
    $15: FFilter.CutoffLo := AValue and $07;
    $16: FFilter.CutoffHi := AValue;
    $17: FFilter.ResFilt := AValue;
    $18:
      begin
        FFilter.ModeVol := AValue;
        FState.Filter3Off := (AValue and SID_FILT_3OFF) <> 0;
      end;
  end;

  // Update filter coefficients when cutoff or resonance changes
  if AAddress in [$15, $16, $17] then
    UpdateFilterCoefficients;
end;

function TSedaiSIDEvo.ReadRegister(AAddress: Byte): Byte;
begin
  case AAddress of
    // Readable registers
    $19: Result := 0;  // Potentiometer X (not emulated)
    $1A: Result := 0;  // Potentiometer Y (not emulated)
    // ReSID-exact: wave.readOSC() returns output() >> 4
    // This is the 12-bit waveform output shifted right 4 bits to get 8 bits
    $1B: Result := GetWaveformOutput12(2) shr 4;  // Voice 3 osc
    $1C: Result := FState.Voices[2].SIDEnv.EnvelopeCounter;  // Voice 3 envelope
  else
    // ReSID-exact: write-only registers return bus_value
    // The bus value decays over time (TTL managed in ClockCycle)
    Result := FBusValue;
  end;
end;

// ============================================================================
// SID OSCILLATOR (ReSID-exact implementation)
// ============================================================================

procedure TSedaiSIDEvo.ClockOscillator(AIndex: Integer);
var
  Freq: Cardinal;
  PrevAccum: Cardinal;
  Bit19Prev, Bit19Now: Boolean;
begin
  if (AIndex < 0) or (AIndex >= SIDEVO_MAX_VOICES) then Exit;

  with FState.Voices[AIndex] do
  begin
    // No operation if test bit is set (ReSID-exact)
    if SIDOsc.TestBit then Exit;

    // Save previous accumulator
    PrevAccum := SIDOsc.Accumulator;

    // ReSID-exact: read frequency directly from registers
    // Like ReSID, we use the current register values immediately
    Freq := FreqLo or (Cardinal(FreqHi) shl 8);

    // Calculate new accumulator value
    SIDOsc.Accumulator := (SIDOsc.Accumulator + Freq) and $FFFFFF;

    // Check whether the MSB is set high. This is used for synchronization.
    // msb_rising = !(accumulator_prev & 0x800000) && (accumulator & 0x800000)
    SIDOsc.PreviousSyncMSB := ((PrevAccum and $800000) = 0) and
                              ((SIDOsc.Accumulator and $800000) <> 0);

    // Shift noise register once for each time accumulator bit 19 is set high.
    // ReSID: if (!(accumulator_prev & 0x080000) && (accumulator & 0x080000))
    Bit19Prev := (PrevAccum and $080000) <> 0;
    Bit19Now := (SIDOsc.Accumulator and $080000) <> 0;
    if (not Bit19Prev) and Bit19Now then
      ShiftNoiseRegister(AIndex);
  end;
end;

// ReSID-exact: Synchronize oscillators.
// This must be done after all the oscillators have been clock()'ed since the
// oscillators operate in parallel.
//
// ReSID logic: Each voice calls synchronize() which checks if THIS voice's MSB
// is rising, and if so, resets the DESTINATION voice's accumulator.
// Sync chain: voice0 -> voice1 -> voice2 -> voice0
// So: voice0.sync_dest = voice1, voice1.sync_dest = voice2, voice2.sync_dest = voice0
procedure TSedaiSIDEvo.SynchronizeOscillators;
var
  I: Integer;
  SyncDestIdx: Integer;
  SyncSourceIdx: Integer;
  ThisMSBRising: Boolean;
  DestSyncEnabled: Boolean;
  ThisSyncEnabled: Boolean;
  SyncSourceMSBRising: Boolean;
begin
  for I := 0 to 2 do
  begin
    // This voice is the sync SOURCE
    // Sync destinations: voice0 -> voice1, voice1 -> voice2, voice2 -> voice0
    SyncDestIdx := (I + 1) mod 3;
    // Sync sources (for special case): voice0 <- voice2, voice1 <- voice0, voice2 <- voice1
    SyncSourceIdx := (I + 2) mod 3;

    ThisMSBRising := FState.Voices[I].SIDOsc.PreviousSyncMSB;
    DestSyncEnabled := (FState.Voices[SyncDestIdx].Control and SID_CTRL_SYNC) <> 0;
    ThisSyncEnabled := (FState.Voices[I].Control and SID_CTRL_SYNC) <> 0;
    SyncSourceMSBRising := FState.Voices[SyncSourceIdx].SIDOsc.PreviousSyncMSB;

    // ReSID: if (msb_rising && sync_dest->sync && !(sync && sync_source->msb_rising))
    // Translation: if THIS voice's MSB is rising AND destination has sync enabled
    //              AND NOT (this voice has sync enabled AND this voice's source is also rising)
    if ThisMSBRising and DestSyncEnabled and
       (not (ThisSyncEnabled and SyncSourceMSBRising)) then
    begin
      // Reset destination's accumulator
      FState.Voices[SyncDestIdx].SIDOsc.Accumulator := 0;
    end;
  end;
end;

procedure TSedaiSIDEvo.ShiftNoiseRegister(AIndex: Integer);
var
  Bit22, Bit17, Bit0: Cardinal;
begin
  if (AIndex < 0) or (AIndex >= SIDEVO_MAX_VOICES) then Exit;

  with FState.Voices[AIndex].SIDOsc do
  begin
    // ReSID-exact LFSR feedback: bit 22 XOR bit 17
    // The new bit 0 is the XOR of bits 22 and 17
    Bit22 := (ShiftRegister shr 22) and 1;
    Bit17 := (ShiftRegister shr 17) and 1;
    Bit0 := Bit22 xor Bit17;

    // Shift left and insert new bit at position 0
    ShiftRegister := ShiftRegister shl 1;
    ShiftRegister := ShiftRegister and $7FFFFF;  // Mask to 23 bits
    ShiftRegister := ShiftRegister or Bit0;
  end;
end;

// ============================================================================
// ReSID-exact waveform output functions
// ============================================================================

// Triangle waveform (ReSID-exact)
function TSedaiSIDEvo.OutputTriangle(AIndex: Integer): Integer;
var
  MSB: Cardinal;
  SyncSourceIdx: Integer;
  Acc: Cardinal;
begin
  // Get sync source index for ring modulation
  SyncSourceIdx := (AIndex + 2) mod 3;
  Acc := FState.Voices[AIndex].SIDOsc.Accumulator;

  // Ring modulation substitutes the MSB with MSB EOR sync_source MSB
  if (FState.Voices[AIndex].Control and SID_CTRL_RINGMOD) <> 0 then
    MSB := (Acc xor FState.Voices[SyncSourceIdx].SIDOsc.Accumulator) and $800000
  else
    MSB := Acc and $800000;

  // The MSB is used to create the falling edge by inverting lower 11 bits
  // ReSID-exact: ((msb ? ~accumulator : accumulator) >> 11) & 0xfff
  if MSB <> 0 then
    Result := ((not Acc) shr 11) and $FFF  // Explicit parentheses for precedence
  else
    Result := (Acc shr 11) and $FFF;
end;

// Sawtooth waveform (ReSID-exact)
function TSedaiSIDEvo.OutputSawtooth(AIndex: Integer): Integer;
begin
  Result := FState.Voices[AIndex].SIDOsc.Accumulator shr 12;
end;

// Pulse waveform (ReSID-exact)
function TSedaiSIDEvo.OutputPulse(AIndex: Integer): Integer;
var
  PW: Cardinal;
begin
  with FState.Voices[AIndex] do
  begin
    PW := PWLo or ((PWHi and $0F) shl 8);
    // Test bit holds pulse at 0xfff
    if SIDOsc.TestBit or ((SIDOsc.Accumulator shr 12) >= PW) then
      Result := $FFF
    else
      Result := $000;
  end;
end;

// Noise waveform (ReSID-exact)
function TSedaiSIDEvo.OutputNoise(AIndex: Integer): Integer;
begin
  with FState.Voices[AIndex].SIDOsc do
  begin
    // Extract noise bits from LFSR (ReSID-exact bit positions)
    Result := ((ShiftRegister and $400000) shr 11) or  // bit 22 -> out 11
              ((ShiftRegister and $100000) shr 10) or  // bit 20 -> out 10
              ((ShiftRegister and $010000) shr 7) or   // bit 16 -> out 9
              ((ShiftRegister and $002000) shr 5) or   // bit 13 -> out 8
              ((ShiftRegister and $000800) shr 4) or   // bit 11 -> out 7
              ((ShiftRegister and $000080) shr 1) or   // bit 7  -> out 6
              ((ShiftRegister and $000010) shl 1) or   // bit 4  -> out 5
              ((ShiftRegister and $000004) shl 2);     // bit 2  -> out 4
  end;
end;

function TSedaiSIDEvo.CombineWaveforms(AWaveform: Byte; AAcc: Cardinal;
  APulse: Boolean; ARingMod: Boolean; ASyncAcc: Cardinal): Integer;
begin
  // This function is deprecated - waveform selection is now done in GetWaveformOutput12
  Result := 0;
end;

// Get 12-bit waveform output (ReSID-exact with OSC3 lookup tables)
function TSedaiSIDEvo.GetWaveformOutput12(AIndex: Integer): Integer;
var
  WaveformBits: Byte;
  SawOut, TriOut, PulseOut: Integer;
begin
  Result := 0;
  if (AIndex < 0) or (AIndex >= 3) then Exit;

  with FState.Voices[AIndex] do
  begin
    WaveformBits := (Control shr 4) and $0F;

    case WaveformBits of
      $0: Result := 0;                                      // No waveform
      $1: Result := OutputTriangle(AIndex);                 // Triangle
      $2: Result := OutputSawtooth(AIndex);                 // Sawtooth
      $3: begin
            // Sawtooth+Triangle: use OSC3 lookup table
            // ReSID: wave__ST[output__S_()] << 4
            SawOut := OutputSawtooth(AIndex);
            if Assigned(FWave_ST) then
              Result := Integer((FWave_ST + SawOut)^) shl 4
            else
              Result := SawOut and OutputTriangle(AIndex);  // Fallback to AND
          end;
      $4: Result := OutputPulse(AIndex);                    // Pulse
      $5: begin
            // Pulse+Triangle: use OSC3 lookup table
            // ReSID: (wave_P_T[output___T() >> 1] << 4) & output_P__()
            TriOut := OutputTriangle(AIndex);
            PulseOut := OutputPulse(AIndex);
            if Assigned(FWave_PT) then
              Result := (Integer((FWave_PT + (TriOut shr 1))^) shl 4) and PulseOut
            else
              Result := TriOut and PulseOut;  // Fallback to AND
          end;
      $6: begin
            // Pulse+Sawtooth: use OSC3 lookup table
            // ReSID: (wave_PS_[output__S_()] << 4) & output_P__()
            SawOut := OutputSawtooth(AIndex);
            PulseOut := OutputPulse(AIndex);
            if Assigned(FWave_PS) then
              Result := (Integer((FWave_PS + SawOut)^) shl 4) and PulseOut
            else
              Result := SawOut and PulseOut;  // Fallback to AND
          end;
      $7: begin
            // Pulse+Sawtooth+Triangle: use OSC3 lookup table
            // ReSID: (wave_PST[output__S_()] << 4) & output_P__()
            SawOut := OutputSawtooth(AIndex);
            PulseOut := OutputPulse(AIndex);
            if Assigned(FWave_PST) then
              Result := (Integer((FWave_PST + SawOut)^) shl 4) and PulseOut
            else
              Result := SawOut and OutputTriangle(AIndex) and PulseOut;  // Fallback to AND
          end;
      $8: Result := OutputNoise(AIndex);                    // Noise
      // Combined waveforms with noise output 0 (ReSID behavior)
      $9..$F: Result := 0;
    end;
  end;
end;

// Get voice output (ReSID-exact: 20-bit range)
// Formula: (wave.output() - wave_zero) * envelope.output() + voice_DC
function TSedaiSIDEvo.GetVoiceOutput(AIndex: Integer): Integer;
var
  WaveOut: Integer;
begin
  Result := 0;
  if (AIndex < 0) or (AIndex >= 3) then Exit;

  with FState.Voices[AIndex] do
  begin
    // Get 12-bit waveform output
    WaveOut := GetWaveformOutput12(AIndex);

    // Store for register read
    WaveformOutput := WaveOut;

    // ReSID-exact voice output formula:
    // output = (wave - wave_zero) * envelope + voice_DC
    // Range: [-2048*255, 2047*255] = 20 bits
    // Apply per-voice volume (default 1.0 preserves existing behavior)
    Result := Round(((WaveOut - SIDVoice.WaveZero) * SIDEnv.EnvelopeCounter + SIDVoice.VoiceDC) * Volume);
  end;
end;

// ============================================================================
// SID ENVELOPE (ReSID-exact implementation)
// ============================================================================
// This is a direct port of the ReSID envelope generator algorithm.
// The envelope generator uses:
// - 15-bit rate counter with ADSR delay bug at $8000
// - Exponential counter for decay/release exponential curve
// - Hold-zero flag for ADSR delay bug
// - Exact sustain level lookup table
//
// Reference: https://docs.rs/resid-rs/1.0.3/src/resid/envelope.rs.html
// ============================================================================

function TSedaiSIDEvo.GetExpPeriod(AEnvelopeCounter: Byte): Byte;
begin
  // This function is not used in the new implementation
  // Kept for compatibility but exp period is set directly in ClockEnvelope
  Result := 1;
end;

procedure TSedaiSIDEvo.SetEnvelopeGate(AIndex: Integer; AGate: Boolean);
begin
  if (AIndex < 0) or (AIndex >= SIDEVO_MAX_VOICES) then Exit;

  with FState.Voices[AIndex].SIDEnv do
  begin
    if AGate and not Gate then
    begin
      // Gate ON - start attack phase
      State := sesAttack;
      RatePeriod := SID_RATE_COUNTER_PERIODS[Attack];
      // Switching to attack state unlocks the zero freeze (ReSID-exact)
      HoldZero := False;
    end
    else if not AGate and Gate then
    begin
      // Gate OFF - start release phase
      State := sesRelease;
      RatePeriod := SID_RATE_COUNTER_PERIODS[Release];
    end;
    Gate := AGate;
  end;
end;

procedure TSedaiSIDEvo.ClockEnvelope(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= SIDEVO_MAX_VOICES) then Exit;

  with FState.Voices[AIndex].SIDEnv do
  begin
    // ========================================================================
    // RATE COUNTER (ReSID-exact)
    // ========================================================================
    // Check for ADSR delay bug:
    // If the rate counter comparison value is set below the current value of
    // the rate counter, the counter will continue counting up until it wraps
    // around to zero at 2^15 = 0x8000, and then count rate_period - 1 before
    // the envelope can finally be stepped.
    // This has been verified by sampling ENV3.
    // ========================================================================

    Inc(RateCounter);

    // ADSR delay bug: when MSB is set, increment again and mask to 15 bits
    if (RateCounter and SID_RATE_COUNTER_MSB) <> 0 then
    begin
      Inc(RateCounter);
      RateCounter := RateCounter and SID_RATE_COUNTER_MASK;
    end;

    // Check for rate counter period match
    if RateCounter <> RatePeriod then
      Exit;

    // Rate counter matched - reset and process envelope
    RateCounter := 0;

    // ========================================================================
    // EXPONENTIAL COUNTER (ReSID-exact)
    // ========================================================================
    // The first envelope step in the attack state also resets the exponential
    // counter. This has been verified by sampling ENV3.
    // ========================================================================

    // ========================================================================
    // EXPONENTIAL COUNTER (ReSID-exact)
    // ========================================================================
    // ReSID: if (state == ATTACK || ++exponential_counter == exponential_counter_period)
    // In attack state, exp counter is NOT incremented but IS reset (short-circuit OR).
    // In decay/release, exp counter is incremented and compared to period.
    // ========================================================================
    if State = sesAttack then
    begin
      // Attack: reset exp counter without incrementing (short-circuit behavior)
      ExpCounter := 0;
    end
    else
    begin
      // Decay/Release: increment and check against period
      Inc(ExpCounter);
      if ExpCounter <> ExpPeriod then
        Exit;
      ExpCounter := 0;
    end;

    // Check whether the envelope counter is frozen at zero (ADSR delay bug)
    if HoldZero then
      Exit;

    // ======================================================================
    // ENVELOPE STATE MACHINE (ReSID-exact)
    // ======================================================================
    case State of
      sesAttack:
        begin
          // ReSID: ++envelope_counter &= 0xff
          // Increment envelope counter (Byte auto-wraps in Pascal)
          Inc(EnvelopeCounter);

          // Check if attack complete (reached $FF)
          if EnvelopeCounter = $FF then
          begin
            State := sesDecaySustain;
            RatePeriod := SID_RATE_COUNTER_PERIODS[Decay];
          end;
        end;

      sesDecaySustain:
        begin
          // Count down towards sustain level
          if EnvelopeCounter <> SID_SUSTAIN_LEVELS[Sustain] then
            Dec(EnvelopeCounter);
        end;

      sesRelease:
        begin
          // ReSID: --envelope_counter &= 0xff
          // Count down towards zero (Byte auto-wraps in Pascal)
          Dec(EnvelopeCounter);
        end;

      sesFrozen:
        begin
          // Should not reach here if HoldZero is properly set
        end;
    end;

    // ======================================================================
    // EXPONENTIAL PERIOD UPDATE (ReSID-exact)
    // ======================================================================
    // Check for change of exponential counter period.
    // These are exact threshold values from ReSID.
    // NOTE: ReSID does NOT reset exponential_counter when period changes.
    // ======================================================================
    case EnvelopeCounter of
      $FF: ExpPeriod := 1;   // 255: fastest
      $5D: ExpPeriod := 2;   // 93
      $36: ExpPeriod := 4;   // 54
      $1A: ExpPeriod := 8;   // 26
      $0E: ExpPeriod := 16;  // 14
      $06: ExpPeriod := 30;  // 6
      $00:
        begin
          ExpPeriod := 1;
          HoldZero := True;  // ADSR delay bug: freeze at zero
        end;
    end;
  end;
end;

// ============================================================================
// SID FILTER (ReSID-exact Two-Integrator-Loop)
// ============================================================================

procedure TSedaiSIDEvo.UpdateFilterCoefficients;
const
  // ReSID-exact w0 ceiling values
  // w0_max_1 = 2*pi*16000*1.048576 = 105413
  // w0_max_dt = 2*pi*4000*1.048576 = 26353
  W0_MAX_1 = 105413;
  W0_MAX_DT = 26353;
var
  FC: Integer;
  Res: Integer;
begin
  // Calculate 11-bit cutoff
  FC := (FFilter.CutoffLo and $07) or ((FFilter.CutoffHi and $FF) shl 3);
  // Ensure FC is within bounds (0-2047)
  if FC > 2047 then FC := 2047;

  // Get w0 from cutoff table (ReSID stores pre-computed w0 values)
  // ReSID: w0 = 2*pi*f*1.048576 (scaled for >>20 division)
  if FFilterF0 <> nil then
    FFilter.W0 := PInteger(PByte(FFilterF0) + FC * SizeOf(Integer))^
  else
    FFilter.W0 := FFilterF0_6581[FC];

  // w0_ceil limits for filter stability (ReSID-exact values)
  // For 1-cycle: limit f0 to 16kHz
  FFilter.W0_ceil_1 := FFilter.W0;
  if FFilter.W0_ceil_1 > W0_MAX_1 then
    FFilter.W0_ceil_1 := W0_MAX_1;

  // For delta_t: limit f0 to 4kHz
  FFilter.W0_ceil_dt := FFilter.W0;
  if FFilter.W0_ceil_dt > W0_MAX_DT then
    FFilter.W0_ceil_dt := W0_MAX_DT;

  // Resonance: 4 bits (0-15)
  // ReSID-exact: _1024_div_Q = 1024.0/(0.707 + 1.0*res/0x0f)
  // Q range is [0.707, 1.707] - NOT [0.707, 8.0]!
  Res := (FFilter.ResFilt shr 4) and $0F;
  FFilter._1024_div_Q := Round(1024.0 / (0.707 + 1.0 * Res / $0F));
end;

// ReSID-exact filter clock (1 cycle)
procedure TSedaiSIDEvo.ClockFilter(Voice1, Voice2, Voice3, ExtIn: Integer);
var
  Filt: Byte;
  Voice3Off: Boolean;
  Vi: Integer;
  dVbp, dVlp: Integer;
begin
  // Scale each voice down from 20 to 13 bits (ReSID-exact)
  Voice1 := Voice1 shr 7;
  Voice2 := Voice2 shr 7;

  // NB! Voice 3 is not silenced by voice3off if it is routed through the filter
  Filt := FFilter.ResFilt and $0F;
  Voice3Off := FState.Filter3Off;

  if Voice3Off and ((Filt and $04) = 0) then
    Voice3 := 0
  else
    Voice3 := Voice3 shr 7;

  // Scale ext_in from 20 to 13 bits (ReSID-exact)
  ExtIn := ExtIn shr 7;

  // ReSID-exact: if filter is disabled, bypass all filtering
  if not FFilter.Enabled then
  begin
    FFilter.Vnf := Voice1 + Voice2 + Voice3 + ExtIn;
    FFilter.Vhp := 0;
    FFilter.Vbp := 0;
    FFilter.Vlp := 0;
    Exit;
  end;

  // Route voices into or around filter (ReSID-exact switch with ext_in)
  case Filt of
    $0: begin Vi := 0; FFilter.Vnf := Voice1 + Voice2 + Voice3 + ExtIn; end;
    $1: begin Vi := Voice1; FFilter.Vnf := Voice2 + Voice3 + ExtIn; end;
    $2: begin Vi := Voice2; FFilter.Vnf := Voice1 + Voice3 + ExtIn; end;
    $3: begin Vi := Voice1 + Voice2; FFilter.Vnf := Voice3 + ExtIn; end;
    $4: begin Vi := Voice3; FFilter.Vnf := Voice1 + Voice2 + ExtIn; end;
    $5: begin Vi := Voice1 + Voice3; FFilter.Vnf := Voice2 + ExtIn; end;
    $6: begin Vi := Voice2 + Voice3; FFilter.Vnf := Voice1 + ExtIn; end;
    $7: begin Vi := Voice1 + Voice2 + Voice3; FFilter.Vnf := ExtIn; end;
    $8: begin Vi := ExtIn; FFilter.Vnf := Voice1 + Voice2 + Voice3; end;
    $9: begin Vi := Voice1 + ExtIn; FFilter.Vnf := Voice2 + Voice3; end;
    $A: begin Vi := Voice2 + ExtIn; FFilter.Vnf := Voice1 + Voice3; end;
    $B: begin Vi := Voice1 + Voice2 + ExtIn; FFilter.Vnf := Voice3; end;
    $C: begin Vi := Voice3 + ExtIn; FFilter.Vnf := Voice1 + Voice2; end;
    $D: begin Vi := Voice1 + Voice3 + ExtIn; FFilter.Vnf := Voice2; end;
    $E: begin Vi := Voice2 + Voice3 + ExtIn; FFilter.Vnf := Voice1; end;
    $F: begin Vi := Voice1 + Voice2 + Voice3 + ExtIn; FFilter.Vnf := 0; end;
  else
    Vi := 0;
    FFilter.Vnf := Voice1 + Voice2 + Voice3 + ExtIn;
  end;

  // ReSID-exact filter calculation:
  // dVbp = (w0_ceil_1*Vhp >> 20)
  // dVlp = (w0_ceil_1*Vbp >> 20)
  // Vbp -= dVbp
  // Vlp -= dVlp
  // Vhp = (Vbp*_1024_div_Q >> 10) - Vlp - Vi

  // Use Int64 to prevent overflow: W0_ceil_1 (17-bit) * Vhp (16-bit) > 32-bit
  dVbp := (Int64(FFilter.W0_ceil_1) * FFilter.Vhp) shr 20;
  dVlp := (Int64(FFilter.W0_ceil_1) * FFilter.Vbp) shr 20;
  FFilter.Vbp := FFilter.Vbp - dVbp;
  FFilter.Vlp := FFilter.Vlp - dVlp;
  FFilter.Vhp := (Int64(FFilter.Vbp) * FFilter._1024_div_Q) shr 10 - FFilter.Vlp - Vi;
end;

// ReSID-exact filter output
function TSedaiSIDEvo.FilterOutput: Integer;
var
  HpBpLp: Byte;
  Vf: Integer;
  Vol: Integer;
begin
  Vol := FFilter.ModeVol and $0F;

  // ReSID-exact: if filter is disabled, output (Vnf + mixer_DC) * vol
  if not FFilter.Enabled then
  begin
    Result := (FFilter.Vnf + FFilter.MixerDC) * Vol;
    Exit;
  end;

  HpBpLp := (FFilter.ModeVol shr 4) and $07;

  // Mix highpass, bandpass, and lowpass outputs (ReSID-exact switch)
  case HpBpLp of
    $0: Vf := 0;
    $1: Vf := FFilter.Vlp;
    $2: Vf := FFilter.Vbp;
    $3: Vf := FFilter.Vlp + FFilter.Vbp;
    $4: Vf := FFilter.Vhp;
    $5: Vf := FFilter.Vlp + FFilter.Vhp;
    $6: Vf := FFilter.Vbp + FFilter.Vhp;
    $7: Vf := FFilter.Vlp + FFilter.Vbp + FFilter.Vhp;
  else
    Vf := 0;
  end;

  // Sum non-filtered and filtered output, multiply by volume
  // ReSID: (Vnf + Vf + mixer_DC) * vol
  Result := (FFilter.Vnf + Vf + FFilter.MixerDC) * Vol;
end;

// ============================================================================
// EXTERNAL FILTER (ReSID-exact C64 output stage)
// Low-pass:  R = 10kOhm, C = 1000pF; w0lp = 1/RC = 100000
// High-pass: R =  1kOhm, C =   10uF; w0hp = 1/RC = 100
// ============================================================================

procedure TSedaiSIDEvo.InitializeExternalFilter;
begin
  FExtFilter.Enabled := True;
  FExtFilter.Vlp := 0;
  FExtFilter.Vhp := 0;
  FExtFilter.Vo := 0;
  // ReSID: Multiply with 1.048576 to facilitate division by 1 000 000
  // by right-shifting 20 times (2 ^ 20 = 1048576)
  FExtFilter.W0lp := 104858;   // 100000 * 1.048576
  FExtFilter.W0hp := 105;      // 100 * 1.048576

  // Maximum mixer DC output level (for 6581)
  // ReSID: ((((0x800 - 0x380) + 0x800)*0xff*3 - 0xfff*0xff/18) >> 7)*0x0f
  if FModel = smMOS6581 then
    FExtFilter.MixerDC := ((((($800 - $380) + $800) * $FF * 3 - ($FFF * $FF div 18)) shr 7) * $0F)
  else
    FExtFilter.MixerDC := 0;
end;

procedure TSedaiSIDEvo.ClockExternalFilter(Vi: Integer);
var
  dVlp, dVhp: Integer;
begin
  if not FExtFilter.Enabled then
  begin
    // Remove maximum DC level since there is no filter to do it
    FExtFilter.Vlp := 0;
    FExtFilter.Vhp := 0;
    FExtFilter.Vo := Vi - FExtFilter.MixerDC;
    Exit;
  end;

  // ReSID-exact external filter calculation:
  // dVlp = (w0lp >> 8) * (Vi - Vlp) >> 12
  // dVhp = w0hp * (Vlp - Vhp) >> 20
  // Vo = Vlp - Vhp
  // Vlp += dVlp
  // Vhp += dVhp
  // Use Int64 for safety (Vi can be large from filter output)
  dVlp := (Int64(FExtFilter.W0lp shr 8) * (Vi - FExtFilter.Vlp)) shr 12;
  dVhp := (Int64(FExtFilter.W0hp) * (FExtFilter.Vlp - FExtFilter.Vhp)) shr 20;
  FExtFilter.Vo := FExtFilter.Vlp - FExtFilter.Vhp;
  FExtFilter.Vlp := FExtFilter.Vlp + dVlp;
  FExtFilter.Vhp := FExtFilter.Vhp + dVhp;
end;

function TSedaiSIDEvo.ExternalFilterOutput: Integer;
begin
  Result := FExtFilter.Vo;
end;

// ============================================================================
// CLOCK CYCLE (ReSID-exact)
// ============================================================================

procedure TSedaiSIDEvo.ClockCycle;
var
  I: Integer;
  Voice1, Voice2, Voice3: Integer;
begin
  // ReSID-exact: age bus value (TTL decrements each cycle)
  // Pre-decrement, then check (ReSID: if (--bus_value_ttl <= 0))
  Dec(FBusValueTTL);
  if FBusValueTTL <= 0 then
  begin
    FBusValue := 0;
    FBusValueTTL := 0;
  end;

  // ReSID-exact clock order:
  // 1. Clock envelopes FIRST (amplitude modulators)
  for I := 0 to 2 do
    ClockEnvelope(I);

  // 2. Clock oscillators SECOND
  for I := 0 to 2 do
    ClockOscillator(I);

  // 3. Synchronize oscillators AFTER all have been clocked
  SynchronizeOscillators;

  // 4. Get voice outputs (20-bit, ReSID-exact formula)
  Voice1 := GetVoiceOutput(0);
  Voice2 := GetVoiceOutput(1);
  Voice3 := GetVoiceOutput(2);

  // 5. Clock internal filter with voice outputs and ext_in
  ClockFilter(Voice1, Voice2, Voice3, FExtIn);

  // 6. Clock external filter (C64 output stage)
  ClockExternalFilter(FilterOutput);
end;

// ============================================================================
// AUDIO GENERATION
// ============================================================================

function TSedaiSIDEvo.GenerateSample: Single;
const
  // ReSID-exact output range: ((4095*255 >> 7)*3*15*2) / 2 = 367065
  OUTPUT_DIVISOR = 367065.0;
var
  I, CyclesToRun: Integer;
  RawOutput: Integer;
begin
  // SOLUTION 1: Cache frequencies at start of sample to prevent race conditions
  CacheFrequencies;
  
  // Run enough cycles for one sample
  FCycleAccum := FCycleAccum + FCyclesPerSample;
  CyclesToRun := Trunc(FCycleAccum);
  FCycleAccum := FCycleAccum - CyclesToRun;

  for I := 1 to CyclesToRun do
    ClockCycle;

  // Get external filter output (ReSID-exact: includes DC removal)
  RawOutput := ExternalFilterOutput;

  // ReSID-exact normalization to [-1.0, 1.0]
  Result := RawOutput / OUTPUT_DIVISOR;

  // Apply master volume
  Result := Result * FState.MasterVolume;

  // Clamp
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

function TSedaiSIDEvo.GenerateSampleSimple: Single;
const
  // Fixed 22 cycles per sample (44100 Hz / 985248 Hz clock ≈ 22.34)
  CYCLES_PER_SAMPLE = 22;
  // ReSID-exact output range: ((4095*255 >> 7)*3*15*2) / 2 = 367065
  OUTPUT_DIVISOR = 367065.0;
var
  I: Integer;
  RawOutput: Integer;
begin
  // SOLUTION 1: Cache frequencies at start of sample
  CacheFrequencies;
  
  // Run fixed number of cycles - no fractional accumulator
  for I := 1 to CYCLES_PER_SAMPLE do
    ClockCycle;

  // Get external filter output (same path as GenerateSample)
  RawOutput := ExternalFilterOutput;

  // ReSID-exact normalization to [-1.0, 1.0]
  Result := RawOutput / OUTPUT_DIVISOR;

  // Apply master volume
  Result := Result * FState.MasterVolume;

  // Clamp
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

function TSedaiSIDEvo.GenerateSampleNoLoop: Single;
const
  OUTPUT_DIVISOR = 367065.0;
var
  RawOutput: Integer;
begin
  // SOLUTION 1: Cache frequencies at start of sample
  CacheFrequencies;
  
  // UNROLLED: Call ClockCycle 22 times without a for loop
  // This tests if the for loop itself is causing jitter
  ClockCycle; ClockCycle; ClockCycle; ClockCycle; ClockCycle;
  ClockCycle; ClockCycle; ClockCycle; ClockCycle; ClockCycle;
  ClockCycle; ClockCycle; ClockCycle; ClockCycle; ClockCycle;
  ClockCycle; ClockCycle; ClockCycle; ClockCycle; ClockCycle;
  ClockCycle; ClockCycle;

  // Get external filter output
  RawOutput := ExternalFilterOutput;

  // Normalize to [-1.0, 1.0]
  Result := RawOutput / OUTPUT_DIVISOR;

  // Apply master volume
  Result := Result * FState.MasterVolume;

  // Clamp
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

function TSedaiSIDEvo.GenerateSamplePure: Single;
const
  // Fixed frequency like SAW-INT-NOLOOP: ~440 Hz
  FIXED_FREQ = 7604;
var
  Sum: Integer;
begin
  // PURE TEST v4: Use FTestAcc0/1/2 directly in class (not in FState record)
  // This tests if nested record access is the problem
  FTestAcc0 := (FTestAcc0 + FIXED_FREQ) and $FFFFFF;
  FTestAcc1 := (FTestAcc1 + FIXED_FREQ) and $FFFFFF;
  FTestAcc2 := (FTestAcc2 + FIXED_FREQ) and $FFFFFF;

  // Output: 3 sawtooths
  Sum := ((FTestAcc0 shr 12) - 2048) +
         ((FTestAcc1 shr 12) - 2048) +
         ((FTestAcc2 shr 12) - 2048);

  Result := (Sum / 6144.0) * 0.3;

  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

function TSedaiSIDEvo.GenerateSampleOscOnly: Single;
const
  CLOCKS = 22;
var
  I, J: Integer;
  WaveOut: Integer;
  Sum: Integer;
begin
  // SOLUTION 1: Cache frequencies at start of sample
  CacheFrequencies;
  
  // Clock oscillators 22 times per sample (like ReSID)
  // but NO envelope, NO filter - just raw oscillator output
  for I := 1 to CLOCKS do
  begin
    for J := 0 to 2 do
      ClockOscillator(J);
  end;

  // Get raw sawtooth output from each voice (12-bit)
  Sum := 0;
  for J := 0 to 2 do
  begin
    WaveOut := FState.Voices[J].SIDOsc.Accumulator shr 12;
    Sum := Sum + (WaveOut - 2048);  // Center around 0
  end;

  // Normalize to [-1.0, 1.0] with some headroom
  Result := (Sum / 6144.0) * 0.3;

  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

function TSedaiSIDEvo.GenerateSampleOscFixed: Single;
const
  CLOCKS = 22;
  FIXED_FREQ = 7604;
var
  I, J: Integer;
  WaveOut: Integer;
  Sum: Integer;
  PrevAccum: Cardinal;
begin
  for I := 1 to CLOCKS do
  begin
    for J := 0 to 2 do
    begin
      with FState.Voices[J] do
      begin
        if not SIDOsc.TestBit then
        begin
          PrevAccum := SIDOsc.Accumulator;
          SIDOsc.Accumulator := (SIDOsc.Accumulator + FIXED_FREQ) and $FFFFFF;
          SIDOsc.PreviousSyncMSB := ((PrevAccum and $800000) = 0) and
                                    ((SIDOsc.Accumulator and $800000) <> 0);
        end;
      end;
    end;
  end;
  Sum := 0;
  for J := 0 to 2 do
  begin
    WaveOut := FState.Voices[J].SIDOsc.Accumulator shr 12;
    Sum := Sum + (WaveOut - 2048);
  end;
  Result := (Sum / 6144.0) * 0.3;
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

function TSedaiSIDEvo.GenerateSampleOscDirect: Single;
const
  CLOCKS = 22;
var
  I, J: Integer;
  WaveOut: Integer;
  Sum: Integer;
  PrevAccum: Cardinal;
  Freq: Cardinal;
begin
  // Same as OscFixed but reads frequency DIRECTLY from registers (no cache)
  // This tests if the register values themselves are the problem
  for I := 1 to CLOCKS do
  begin
    for J := 0 to 2 do
    begin
      with FState.Voices[J] do
      begin
        if not SIDOsc.TestBit then
        begin
          PrevAccum := SIDOsc.Accumulator;
          // Read frequency directly from registers each clock cycle
          Freq := FreqLo or (Cardinal(FreqHi) shl 8);
          SIDOsc.Accumulator := (SIDOsc.Accumulator + Freq) and $FFFFFF;
          SIDOsc.PreviousSyncMSB := ((PrevAccum and $800000) = 0) and
                                    ((SIDOsc.Accumulator and $800000) <> 0);
        end;
      end;
    end;
  end;
  Sum := 0;
  for J := 0 to 2 do
  begin
    WaveOut := FState.Voices[J].SIDOsc.Accumulator shr 12;
    Sum := Sum + (WaveOut - 2048);
  end;
  Result := (Sum / 6144.0) * 0.3;
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

const
  SAMPLES_PER_FRAME = 882;  // 44100 / 50 Hz

procedure TSedaiSIDEvo.CacheFrequencies;
var
  I: Integer;
begin
  // SOLUTION 1: Cache frequencies ONCE per frame (882 samples)
  // This ensures all samples in a frame use the same frequency values
  // Just like a real C64 where registers change at 50Hz

  // Increment frame sample counter
  Inc(FFrameSampleCount);

  // Only cache at the start of a new frame
  if FFrameSampleCount >= SAMPLES_PER_FRAME then
  begin
    FFrameSampleCount := 0;
    FFrameCached := False;
  end;

  // Cache frequencies only once per frame
  if not FFrameCached then
  begin
    for I := 0 to 2 do
    begin
      if FDoubleBufferEnabled then
        FCachedFreq[I] := FReadBuffer[I].FreqLo or (FReadBuffer[I].FreqHi shl 8)
      else
        FCachedFreq[I] := FState.Voices[I].FreqLo or (FState.Voices[I].FreqHi shl 8);
    end;
    FFrameCached := True;
  end;
end;

procedure TSedaiSIDEvo.NotifyNewFrame;
var
  I: Integer;
begin
  // Called by player AFTER writing registers to signal a new frame started
  // This immediately caches the new frequency values for audio generation
  // Solves the race condition between frame timer and audio callback
  FFrameSampleCount := 0;
  FFrameCached := False;

  // Cache frequencies immediately from the just-written registers
  for I := 0 to 2 do
  begin
    if FDoubleBufferEnabled then
      FCachedFreq[I] := FReadBuffer[I].FreqLo or (FReadBuffer[I].FreqHi shl 8)
    else
      FCachedFreq[I] := FState.Voices[I].FreqLo or (FState.Voices[I].FreqHi shl 8);
  end;
  FFrameCached := True;
end;

procedure TSedaiSIDEvo.SwapBuffers;
var
  I: Integer;
  Temp: record
    FreqLo, FreqHi: Byte;
    PWLo, PWHi: Byte;
    Control: Byte;
    AttackDecay, SustainRelease: Byte;
  end;
begin
  // SOLUTION 2: Swap write buffer to read buffer at frame boundary
  // This ensures audio generation sees a consistent snapshot of all registers
  if not FDoubleBufferEnabled then Exit;
  
  for I := 0 to 2 do
  begin
    // Copy write buffer to read buffer (field by field for anonymous record)
    FReadBuffer[I].FreqLo := FWriteBuffer[I].FreqLo;
    FReadBuffer[I].FreqHi := FWriteBuffer[I].FreqHi;
    FReadBuffer[I].PWLo := FWriteBuffer[I].PWLo;
    FReadBuffer[I].PWHi := FWriteBuffer[I].PWHi;
    FReadBuffer[I].Control := FWriteBuffer[I].Control;
    FReadBuffer[I].AttackDecay := FWriteBuffer[I].AttackDecay;
    FReadBuffer[I].SustainRelease := FWriteBuffer[I].SustainRelease;
  end;
end;

function TSedaiSIDEvo.GenerateSampleLegacy: Single;
const
  // Output divisor for 12-bit waveform * 8-bit envelope * volume
  // Max: 4095 * 255 * 15 = 15,663,825 - but we scale per voice
  OUTPUT_DIVISOR = 2048.0;  // Simple 12-bit to float conversion
var
  I: Integer;
  WaveOut, EnvOut: Integer;
  VoiceSum: Integer;
  FilteredSum, UnfilteredSum: Integer;
  Voice1, Voice2, Voice3: Integer;
begin
  // Legacy approach: clock oscillators and envelopes ONCE per audio sample
  // (not ~22 times like ReSID-exact)
  for I := 0 to 2 do
  begin
    ClockOscillator(I);
    ClockEnvelope(I);
  end;

  // Synchronize oscillators
  SynchronizeOscillators;

  // Get voice outputs using existing ReSID-exact formula
  Voice1 := GetVoiceOutput(0);
  Voice2 := GetVoiceOutput(1);
  Voice3 := GetVoiceOutput(2);

  // Clock filter ONCE with voice outputs
  ClockFilter(Voice1, Voice2, Voice3, FExtIn);

  // Clock external filter
  ClockExternalFilter(FilterOutput);

  // Get output and normalize
  Result := ExternalFilterOutput / 367065.0;  // ReSID-exact divisor

  // Apply master volume
  Result := Result * FState.MasterVolume;

  // Clamp
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

function TSedaiSIDEvo.GenerateSampleRaw: Single;
var
  I: Integer;
  WaveOut: Integer;
  EnvOut: Byte;
  VoiceOut: Integer;
  Sum: Integer;
  Vol: Integer;
begin
  // RAW approach: minimal processing, no filter
  // Clock oscillators and envelopes once
  for I := 0 to 2 do
  begin
    ClockOscillator(I);
    ClockEnvelope(I);
  end;
  SynchronizeOscillators;

  // Sum voice outputs directly (no filter)
  Sum := 0;
  for I := 0 to 2 do
  begin
    // Get 12-bit waveform
    WaveOut := GetWaveformOutput12(I);
    // Get 8-bit envelope
    EnvOut := FState.Voices[I].SIDEnv.EnvelopeCounter;
    // Voice output: (wave - 2048) * envelope
    VoiceOut := (WaveOut - 2048) * EnvOut;
    Sum := Sum + VoiceOut;
  end;

  // Apply master volume (0-15 in low nibble of ModeVol)
  Vol := FFilter.ModeVol and $0F;
  Sum := Sum * Vol;

  // Normalize: max is 3 * 2048 * 255 * 15 = 23,500,800
  Result := Sum / 23500800.0;

  // Apply SAF master volume
  Result := Result * FState.MasterVolume;

  // Clamp
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

procedure TSedaiSIDEvo.GenerateStereoSample(out ALeft, ARight: Single);
var
  SIDMono: Single;
  EffectivePan, PanL, PanR: Single;
  Width: Single;
  I: Integer;
  ExtSample, EnvLevel: Single;
  VoiceL, VoiceR: Single;
begin
  // Generate SID core sample using ReSID-exact emulation (voices 0-2)
  SIDMono := GenerateSample;

  // Get stereo width (0.0 = mono, 1.0 = full stereo)
  Width := FState.StereoWidth;

  // Calculate effective pan from first active group
  EffectivePan := 0.0;
  if FState.Groups[0].State = sgsEnabled then
    EffectivePan := FState.Groups[0].Pan;

  // Apply stereo width to SID core output
  if Width < 0.01 then
  begin
    ALeft := SIDMono;
    ARight := SIDMono;
  end
  else
  begin
    PanL := Cos((EffectivePan + 1.0) * PI * 0.25);
    PanR := Sin((EffectivePan + 1.0) * PI * 0.25);
    ALeft := SIDMono * ((1.0 - Width) + Width * PanL * 1.414);
    ARight := SIDMono * ((1.0 - Width) + Width * PanR * 1.414);
  end;

  // Add extended waveform voices (3-63) if using EVO mode
  if FState.AuthenticityLevel in [salEvolved, salHybrid] then
  begin
    for I := 3 to GetEffectiveVoiceCount - 1 do
    begin
      with FState.Voices[I] do
      begin
        if not IsActive then Continue;

        // Check if voice uses extended waveform
        if (Waveform and (SIDEVO_WAVE_SINE or SIDEVO_WAVE_SUPERSAW or
                          SIDEVO_WAVE_PWM or SIDEVO_WAVE_HALFSIN)) <> 0 then
        begin
          // Generate extended waveform
          ExtSample := GenerateExtendedWaveform(I);

          // Apply envelope (use simplified linear envelope for extended voices)
          EnvLevel := SIDEnv.EnvelopeCounter / 255.0;
          ExtSample := ExtSample * EnvLevel * Volume;

          // Apply per-voice panning
          PanL := Cos((Pan + 1.0) * PI * 0.25);
          PanR := Sin((Pan + 1.0) * PI * 0.25);

          VoiceL := ExtSample * PanL;
          VoiceR := ExtSample * PanR;

          // Mix into output
          ALeft := ALeft + VoiceL * Width;
          ARight := ARight + VoiceR * Width;
        end;
      end;
    end;
  end;

  // Final clamp
  if ALeft > 1.0 then ALeft := 1.0
  else if ALeft < -1.0 then ALeft := -1.0;
  if ARight > 1.0 then ARight := 1.0
  else if ARight < -1.0 then ARight := -1.0;
end;

procedure TSedaiSIDEvo.ProcessAudioStereo(out ALeftSample, ARightSample: Single);
begin
  GenerateStereoSample(ALeftSample, ARightSample);
end;

function TSedaiSIDEvo.Output: Single;
const
  // ReSID-exact output range calculation:
  // Voice output: (wave - wave_zero) * envelope = (-2048..2047) * 255 = -522240..521985
  // After >> 7 scaling: -4080..4078
  // 3 voices: -12240..12234
  // After filter + volume (0-15): -183600..183510
  // Full range: ((4095*255 >> 7)*3*15*2) = 734130
  // Half range: 367065
  OUTPUT_DIVISOR = 367065.0;
var
  RawOutput: Integer;
begin
  // Get external filter output (ReSID-exact: includes DC removal)
  RawOutput := ExternalFilterOutput;

  // ReSID-exact normalization to [-1.0, 1.0]
  Result := RawOutput / OUTPUT_DIVISOR;

  // Apply master volume
  Result := Result * FState.MasterVolume;

  // Clamp
  if Result > 1.0 then Result := 1.0;
  if Result < -1.0 then Result := -1.0;
end;

// ============================================================================
// DEBUG / TESTING API
// ============================================================================

function TSedaiSIDEvo.GetAccumulator(AVoice: Integer): Cardinal;
begin
  if (AVoice >= 0) and (AVoice < SIDEVO_MAX_VOICES) then
    Result := FState.Voices[AVoice].SIDOsc.Accumulator
  else
    Result := 0;
end;

function TSedaiSIDEvo.GetShiftRegister(AVoice: Integer): Cardinal;
begin
  if (AVoice >= 0) and (AVoice < SIDEVO_MAX_VOICES) then
    Result := FState.Voices[AVoice].SIDOsc.ShiftRegister
  else
    Result := 0;
end;

function TSedaiSIDEvo.GetEnvelopeCounter(AVoice: Integer): Byte;
begin
  if (AVoice >= 0) and (AVoice < SIDEVO_MAX_VOICES) then
    Result := FState.Voices[AVoice].SIDEnv.EnvelopeCounter
  else
    Result := 0;
end;

function TSedaiSIDEvo.GetEnvelopeState(AVoice: Integer): Integer;
begin
  if (AVoice >= 0) and (AVoice < SIDEVO_MAX_VOICES) then
    Result := Ord(FState.Voices[AVoice].SIDEnv.State)
  else
    Result := 0;
end;

function TSedaiSIDEvo.GetRateCounter(AVoice: Integer): Word;
begin
  if (AVoice >= 0) and (AVoice < SIDEVO_MAX_VOICES) then
    Result := FState.Voices[AVoice].SIDEnv.RateCounter
  else
    Result := 0;
end;

function TSedaiSIDEvo.GetVoiceOutputDebug(AVoice: Integer): Integer;
begin
  if (AVoice >= 0) and (AVoice < 3) then
    Result := GetVoiceOutput(AVoice)
  else
    Result := 0;
end;

function TSedaiSIDEvo.ClockAndGenerate(var ADeltaCycles: Integer;
  ABuffer: PSingle; AMaxSamples: Integer): Integer;
const
  FIXP_SHIFT = 16;
  FIXP_MASK = $FFFF;
  // ReSID-exact: rounding offset for "nearest sample" picking
  FIXP_ROUND = 1 shl (FIXP_SHIFT - 1);  // 32768
var
  SamplesGenerated: Integer;
  NextSampleOffset: Int64;
  DeltaSample: Integer;
  I: Integer;
  CyclesPerSampleFixed: Int64;
begin
  if FSampleRate > 0 then
    CyclesPerSampleFixed := Int64(Trunc((FState.ClockFrequency / FSampleRate) *
      (1 shl FIXP_SHIFT) + 0.5))
  else
    CyclesPerSampleFixed := 1 shl FIXP_SHIFT;

  SamplesGenerated := 0;

  while True do
  begin
    // ReSID-exact: add rounding offset for "nearest sample" picking
    NextSampleOffset := FSampleOffset + CyclesPerSampleFixed + FIXP_ROUND;
    DeltaSample := NextSampleOffset shr FIXP_SHIFT;

    if DeltaSample > ADeltaCycles then
      Break;

    if SamplesGenerated >= AMaxSamples then
    begin
      Result := SamplesGenerated;
      Exit;
    end;

    for I := 1 to DeltaSample do
      ClockCycle;

    ADeltaCycles := ADeltaCycles - DeltaSample;
    // ReSID-exact: subtract rounding offset when saving
    FSampleOffset := (NextSampleOffset and FIXP_MASK) - FIXP_ROUND;

    ABuffer[SamplesGenerated] := Output;
    Inc(SamplesGenerated);
  end;

  for I := 1 to ADeltaCycles do
    ClockCycle;

  FSampleOffset := FSampleOffset - (ADeltaCycles shl FIXP_SHIFT);
  ADeltaCycles := 0;

  Result := SamplesGenerated;
end;

// ============================================================================
// VOICE API (EVO Enhanced)
// ============================================================================

function TSedaiSIDEvo.CalcFrequencyReg(AFreqHz: Single): Word;
begin
  // FreqReg = Freq * 16777216 / ClockFreq
  Result := Round(AFreqHz * 16777216.0 / FState.ClockFrequency);
end;

procedure TSedaiSIDEvo.SetFrequencyHz(AVoice: Integer; AFreqHz: Single);
var
  FreqReg: Word;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;

  FState.Voices[AVoice].Frequency := AFreqHz;
  FreqReg := CalcFrequencyReg(AFreqHz);
  FState.Voices[AVoice].FreqLo := FreqReg and $FF;
  FState.Voices[AVoice].FreqHi := (FreqReg shr 8) and $FF;
end;

procedure TSedaiSIDEvo.SetNote(AVoice: Integer; ANoteNumber: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if (ANoteNumber < 0) or (ANoteNumber >= SIDEVO_TOTAL_NOTES) then Exit;

  FState.Voices[AVoice].NoteNumber := ANoteNumber;
  SetFrequencyHz(AVoice, NOTE_FREQUENCIES[ANoteNumber]);
end;

procedure TSedaiSIDEvo.SetPulseWidth(AVoice: Integer; APulseWidth: Single);
var
  PW: Word;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;

  FState.Voices[AVoice].PulseWidth := APulseWidth;
  PW := Round(APulseWidth * 4095);
  FState.Voices[AVoice].PWLo := PW and $FF;
  FState.Voices[AVoice].PWHi := (PW shr 8) and $0F;
end;

procedure TSedaiSIDEvo.SetWaveform(AVoice: Integer; AWaveform: TSIDEvoWaveform);
var
  SIDWave: Byte;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;

  FState.Voices[AVoice].Waveform := AWaveform;

  // Map EVO waveform to SID control register
  SIDWave := 0;
  if (AWaveform and SIDEVO_WAVE_TRIANGLE) <> 0 then SIDWave := SIDWave or SID_WAVE_TRIANGLE;
  if (AWaveform and SIDEVO_WAVE_SAWTOOTH) <> 0 then SIDWave := SIDWave or SID_WAVE_SAWTOOTH;
  if (AWaveform and SIDEVO_WAVE_PULSE) <> 0 then SIDWave := SIDWave or SID_WAVE_PULSE;
  if (AWaveform and SIDEVO_WAVE_NOISE) <> 0 then SIDWave := SIDWave or SID_WAVE_NOISE;

  // Keep gate and other control bits
  FState.Voices[AVoice].Control := (FState.Voices[AVoice].Control and $0F) or SIDWave;
end;

procedure TSedaiSIDEvo.SetADSR(AVoice: Integer; AAttack, ADecay, ASustain, ARelease: Single);
var
  A, D, S, R: Byte;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;

  FState.Voices[AVoice].Attack := AAttack;
  FState.Voices[AVoice].Decay := ADecay;
  FState.Voices[AVoice].Sustain := ASustain;
  FState.Voices[AVoice].Release := ARelease;

  // Map 0.0-1.0 to 0-15
  A := Round(AAttack * 15);
  D := Round(ADecay * 15);
  S := Round(ASustain * 15);
  R := Round(ARelease * 15);

  FState.Voices[AVoice].AttackDecay := (A shl 4) or D;
  FState.Voices[AVoice].SustainRelease := (S shl 4) or R;
  FState.Voices[AVoice].SIDEnv.Attack := A;
  FState.Voices[AVoice].SIDEnv.Decay := D;
  FState.Voices[AVoice].SIDEnv.Sustain := S;
  FState.Voices[AVoice].SIDEnv.Release := R;
end;

procedure TSedaiSIDEvo.SetVoiceVolume(AVoice: Integer; AVolume: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].Volume := AVolume;
end;

procedure TSedaiSIDEvo.SetVoicePan(AVoice: Integer; APan: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].Pan := APan;
end;

procedure TSedaiSIDEvo.SetFrequencyReg(AVoice: Integer; AFreqReg: Word);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].FreqLo := AFreqReg and $FF;
  FState.Voices[AVoice].FreqHi := (AFreqReg shr 8) and $FF;
  // Inverse of CalcFrequencyReg: FreqHz = FreqReg * ClockFreq / 16777216
  FState.Voices[AVoice].Frequency := AFreqReg * FState.ClockFrequency / 16777216.0;
end;

procedure TSedaiSIDEvo.SetControlFlags(AVoice: Integer; AFlags: Byte);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].Control := AFlags;
end;

procedure TSedaiSIDEvo.SetAttack(AVoice: Integer; AAttack: Single);
var
  A: Byte;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if AAttack < 0.0 then AAttack := 0.0;
  if AAttack > 1.0 then AAttack := 1.0;
  FState.Voices[AVoice].Attack := AAttack;
  A := Round(AAttack * 15);
  FState.Voices[AVoice].AttackDecay := (A shl 4) or (FState.Voices[AVoice].AttackDecay and $0F);
  FState.Voices[AVoice].SIDEnv.Attack := A;
end;

procedure TSedaiSIDEvo.SetDecay(AVoice: Integer; ADecay: Single);
var
  D: Byte;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if ADecay < 0.0 then ADecay := 0.0;
  if ADecay > 1.0 then ADecay := 1.0;
  FState.Voices[AVoice].Decay := ADecay;
  D := Round(ADecay * 15);
  FState.Voices[AVoice].AttackDecay := (FState.Voices[AVoice].AttackDecay and $F0) or D;
  FState.Voices[AVoice].SIDEnv.Decay := D;
end;

procedure TSedaiSIDEvo.SetSustain(AVoice: Integer; ASustain: Single);
var
  S: Byte;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if ASustain < 0.0 then ASustain := 0.0;
  if ASustain > 1.0 then ASustain := 1.0;
  FState.Voices[AVoice].Sustain := ASustain;
  S := Round(ASustain * 15);
  FState.Voices[AVoice].SustainRelease := (S shl 4) or (FState.Voices[AVoice].SustainRelease and $0F);
  FState.Voices[AVoice].SIDEnv.Sustain := S;
end;

procedure TSedaiSIDEvo.SetRelease(AVoice: Integer; ARelease: Single);
var
  R: Byte;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if ARelease < 0.0 then ARelease := 0.0;
  if ARelease > 1.0 then ARelease := 1.0;
  FState.Voices[AVoice].Release := ARelease;
  R := Round(ARelease * 15);
  FState.Voices[AVoice].SustainRelease := (FState.Voices[AVoice].SustainRelease and $F0) or R;
  FState.Voices[AVoice].SIDEnv.Release := R;
end;

procedure TSedaiSIDEvo.SetVoiceMute(AVoice: Integer; AMuted: Boolean);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if AMuted then
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags or SIDEVO_FLAG_MUTE
  else
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags and not SIDEVO_FLAG_MUTE;
end;

function TSedaiSIDEvo.IsVoiceMuted(AVoice: Integer): Boolean;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then
  begin
    Result := True;
    Exit;
  end;
  Result := (FState.Voices[AVoice].Flags and SIDEVO_FLAG_MUTE) <> 0;
end;

// ============================================================================
// PLAYBACK CONTROL
// ============================================================================

procedure TSedaiSIDEvo.GateOn(AVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;

  FState.Voices[AVoice].IsActive := True;
  FState.Voices[AVoice].Control := FState.Voices[AVoice].Control or SID_CTRL_GATE;
  SetEnvelopeGate(AVoice, True);
end;

procedure TSedaiSIDEvo.GateOff(AVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;

  FState.Voices[AVoice].Control := FState.Voices[AVoice].Control and not SID_CTRL_GATE;
  SetEnvelopeGate(AVoice, False);
end;

procedure TSedaiSIDEvo.AllGatesOff;
var
  I: Integer;
begin
  for I := 0 to SIDEVO_MAX_VOICES - 1 do
    GateOff(I);
end;

procedure TSedaiSIDEvo.ResetVoiceEnvelope(AVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;

  // Fully reset the SID envelope state machine to avoid the ADSR delay bug:
  // When EnvelopeCounter is $FF (Sustain=15) and attack does Inc(EnvelopeCounter),
  // it wraps to $00 and triggers HoldZero := True, permanently freezing the voice.
  // By resetting to a clean initial state before GateOn, the attack always starts
  // from 0 and increments upward without wrapping.
  with FState.Voices[AVoice].SIDEnv do
  begin
    EnvelopeCounter := 0;
    RateCounter := 0;
    ExpCounter := 0;
    ExpPeriod := 1;
    HoldZero := True;    // Will be cleared by next GateOn -> SetEnvelopeGate
    Gate := False;
    State := sesRelease;
    EnvelopePipeline := 0;
  end;
  // Clear the gate bit in the control register
  FState.Voices[AVoice].Control := FState.Voices[AVoice].Control and not SID_CTRL_GATE;
end;

procedure TSedaiSIDEvo.PlayNote(AVoice: Integer; ANoteNumber: Integer; AVolume: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;

  SetNote(AVoice, ANoteNumber);
  SetVoiceVolume(AVoice, AVolume);
  GateOn(AVoice);
end;

procedure TSedaiSIDEvo.StopVoice(AVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;

  GateOff(AVoice);
  FState.Voices[AVoice].IsActive := False;
end;

procedure TSedaiSIDEvo.StopAll;
var
  I: Integer;
begin
  for I := 0 to SIDEVO_MAX_VOICES - 1 do
    StopVoice(I);
end;

// ============================================================================
// FILTER API
// ============================================================================

procedure TSedaiSIDEvo.SetFilterCutoff(ACutoff: Word);
begin
  FFilter.CutoffLo := ACutoff and $07;
  FFilter.CutoffHi := (ACutoff shr 3) and $FF;
  UpdateFilterCoefficients;
end;

procedure TSedaiSIDEvo.SetFilterResonance(ARes: Byte);
begin
  FFilter.ResFilt := (FFilter.ResFilt and $0F) or ((ARes and $0F) shl 4);
  UpdateFilterCoefficients;
end;

procedure TSedaiSIDEvo.SetFilterMode(ALP, ABP, AHP: Boolean);
begin
  FFilter.ModeVol := FFilter.ModeVol and $0F;
  if ALP then FFilter.ModeVol := FFilter.ModeVol or SID_FILT_LP;
  if ABP then FFilter.ModeVol := FFilter.ModeVol or SID_FILT_BP;
  if AHP then FFilter.ModeVol := FFilter.ModeVol or SID_FILT_HP;
end;

procedure TSedaiSIDEvo.SetFilterVoiceRouting(AVoice1, AVoice2, AVoice3, AExtIn: Boolean);
begin
  FFilter.ResFilt := FFilter.ResFilt and $F0;
  if AVoice1 then FFilter.ResFilt := FFilter.ResFilt or $01;
  if AVoice2 then FFilter.ResFilt := FFilter.ResFilt or $02;
  if AVoice3 then FFilter.ResFilt := FFilter.ResFilt or $04;
  if AExtIn then FFilter.ResFilt := FFilter.ResFilt or $08;
end;

procedure TSedaiSIDEvo.EnableFilter(AEnable: Boolean);
begin
  // ReSID-exact: enable/disable filter for testing purposes
  FFilter.Enabled := AEnable;
end;

procedure TSedaiSIDEvo.SetExternalInput(AValue: Integer);
begin
  // ReSID-exact: set external audio input (20-bit scaled value)
  FExtIn := AValue;
end;

procedure TSedaiSIDEvo.SetFilterFCLo(AValue: Byte);
begin
  // $D415 - Filter cutoff low (bits 0-2)
  FFilter.CutoffLo := AValue and $07;
  UpdateFilterCoefficients;
end;

procedure TSedaiSIDEvo.SetFilterFCHi(AValue: Byte);
begin
  // $D416 - Filter cutoff high (bits 3-10)
  FFilter.CutoffHi := AValue;
  UpdateFilterCoefficients;
end;

procedure TSedaiSIDEvo.SetFilterResFilt(AValue: Byte);
begin
  // $D417 - Resonance (bits 4-7) / Voice routing (bits 0-3)
  FFilter.ResFilt := AValue;
  UpdateFilterCoefficients;
end;

procedure TSedaiSIDEvo.SetFilterModeVol(AValue: Byte);
begin
  // $D418 - Filter mode (bits 4-7) / Master volume (bits 0-3)
  FFilter.ModeVol := AValue;
  // Volume is in bits 0-3 (0-15)
  FState.MasterVolume := (AValue and $0F) / 15.0;
end;

procedure TSedaiSIDEvo.SetFilter3Off(ADisconnect: Boolean);
begin
  // Voice 3 disconnect from audio output (for modulation use)
  if ADisconnect then
    FFilter.ModeVol := FFilter.ModeVol or SID_FILT_3OFF
  else
    FFilter.ModeVol := FFilter.ModeVol and not SID_FILT_3OFF;
end;

// ============================================================================
// GLOBAL SETTINGS
// ============================================================================

procedure TSedaiSIDEvo.SetMasterVolume(AVolume: Single);
begin
  FState.MasterVolume := AVolume;
end;

procedure TSedaiSIDEvo.SetAuthenticityLevel(ALevel: TSIDEvoAuthenticityLevel);
begin
  FState.AuthenticityLevel := ALevel;

  case ALevel of
    salEvolved:
      begin
        FState.EnableBitCrushing := False;
        FState.EnableWaveformAND := False;
        FState.EnableLFSRNoise := False;
        FState.EnableSIDEnvelope := False;
        FState.DACNonLinearity := 0;
      end;
    salHybrid:
      begin
        FState.EnableBitCrushing := False;
        FState.EnableWaveformAND := True;
        FState.EnableLFSRNoise := True;
        FState.EnableSIDEnvelope := True;
        FState.DACNonLinearity := 0.2;
      end;
    salSIDLike:
      begin
        FState.EnableBitCrushing := True;
        FState.EnableWaveformAND := True;
        FState.EnableLFSRNoise := True;
        FState.EnableSIDEnvelope := True;
        FState.DACNonLinearity := 0.5;
      end;
    salSIDFull:
      begin
        FState.EnableBitCrushing := True;
        FState.EnableWaveformAND := True;
        FState.EnableLFSRNoise := True;
        FState.EnableSIDEnvelope := True;
        FState.DACNonLinearity := 1.0;
      end;
  end;
end;

procedure TSedaiSIDEvo.SetClockMode(AMode: TSIDClockMode);
begin
  FState.ClockMode := AMode;
  case AMode of
    scmPAL: FState.ClockFrequency := SID_CLOCK_PAL;
    scmNTSC: FState.ClockFrequency := SID_CLOCK_NTSC;
  end;

  if FSampleRate > 0 then
    FCyclesPerSample := FState.ClockFrequency / FSampleRate;

  InitializeTables;
end;

procedure TSedaiSIDEvo.SetChipModel(AModel: TSIDModel);
begin
  FModel := AModel;
  InitializeTables;
end;

// ============================================================================
// GROUP MANAGEMENT
// ============================================================================

procedure TSedaiSIDEvo.SetActiveGroups(ACount: Integer);
var
  G: Integer;
begin
  if (ACount < 1) then ACount := 1;
  if (ACount > SIDEVO_MAX_GROUPS) then ACount := SIDEVO_MAX_GROUPS;

  FState.ActiveGroups := ACount;

  for G := 0 to SIDEVO_MAX_GROUPS - 1 do
  begin
    if G < ACount then
      FState.Groups[G].State := sgsEnabled
    else
      FState.Groups[G].State := sgsDisabled;
  end;
end;

procedure TSedaiSIDEvo.EnableGroup(AGroupIndex: Integer);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
    FState.Groups[AGroupIndex].State := sgsEnabled;
end;

procedure TSedaiSIDEvo.DisableGroup(AGroupIndex: Integer);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
    FState.Groups[AGroupIndex].State := sgsDisabled;
end;

procedure TSedaiSIDEvo.SetGroupVolume(AGroupIndex: Integer; AVolume: Single);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
    FState.Groups[AGroupIndex].Volume := AVolume;
end;

procedure TSedaiSIDEvo.SetGroupPan(AGroupIndex: Integer; APan: Single);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
    FState.Groups[AGroupIndex].Pan := APan;
end;

procedure TSedaiSIDEvo.SetGroupTranspose(AGroupIndex: Integer; ASemitones: Integer);
begin
  if (AGroupIndex >= 0) and (AGroupIndex < SIDEVO_MAX_GROUPS) then
    FState.Groups[AGroupIndex].Transpose := ASemitones;
end;

function TSedaiSIDEvo.GetAuthenticityLevel: TSIDEvoAuthenticityLevel;
begin
  Result := FState.AuthenticityLevel;
end;

function TSedaiSIDEvo.GetClockMode: TSIDClockMode;
begin
  Result := FState.ClockMode;
end;

function TSedaiSIDEvo.GetActiveGroups: Integer;
begin
  Result := FState.ActiveGroups;
end;

procedure TSedaiSIDEvo.SetStereoWidth(AWidth: Single);
begin
  if AWidth < 0.0 then AWidth := 0.0;
  if AWidth > 1.0 then AWidth := 1.0;
  FState.StereoWidth := AWidth;
end;

// ============================================================================
// UTILITY
// ============================================================================

function TSedaiSIDEvo.GetEffectiveVoiceCount: Integer;
begin
  Result := FState.ActiveGroups * SIDEVO_VOICES_PER_GROUP;
end;

function TSedaiSIDEvo.IsVoiceInActiveGroup(AIndex: Integer): Boolean;
var
  GroupIdx: Integer;
begin
  Result := False;
  if (AIndex < 0) or (AIndex >= SIDEVO_MAX_VOICES) then Exit;

  GroupIdx := AIndex div SIDEVO_VOICES_PER_GROUP;
  if GroupIdx < SIDEVO_MAX_GROUPS then
    Result := FState.Groups[GroupIdx].State = sgsEnabled;
end;

function TSedaiSIDEvo.GetEnvelopeLevel(AVoice: Integer): Byte;
begin
  Result := 0;
  if (AVoice >= 0) and (AVoice < SIDEVO_MAX_VOICES) then
    Result := FState.Voices[AVoice].SIDEnv.EnvelopeCounter;
end;

function TSedaiSIDEvo.GetWaveformOutput(AVoice: Integer): Integer;
begin
  Result := 0;
  if (AVoice >= 0) and (AVoice < SIDEVO_MAX_VOICES) then
    Result := FState.Voices[AVoice].WaveformOutput;
end;

function TSedaiSIDEvo.NoteToFrequency(ANoteNumber: Integer): Single;
begin
  if (ANoteNumber >= 0) and (ANoteNumber < SIDEVO_TOTAL_NOTES) then
    Result := NOTE_FREQUENCIES[ANoteNumber]
  else
    Result := 440.0;
end;

function TSedaiSIDEvo.IsVoiceActive(AVoice: Integer): Boolean;
begin
  Result := False;
  if (AVoice >= 0) and (AVoice < SIDEVO_MAX_VOICES) then
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

function TSedaiSIDEvo.GenerateExtendedWaveform(AIndex: Integer): Single;
var
  VoiceWave: TSIDEvoWaveform;
  Osc: TSedaiOscillator;
begin
  // Extended waveforms are now inherited from Sedai Audio Foundation (SAF)
  // via TSedaiOscillator. This ensures code reuse and consistency.
  Result := 0;
  if (AIndex < 0) or (AIndex >= SIDEVO_MAX_VOICES) then Exit;
  if not Assigned(FExtOscillators[AIndex]) then Exit;

  Osc := FExtOscillators[AIndex];
  VoiceWave := FState.Voices[AIndex].Waveform;

  // Configure oscillator based on voice settings
  Osc.Frequency := FState.Voices[AIndex].Frequency;
  Osc.PulseWidth := FState.Voices[AIndex].PulseWidth;

  // Map SIDEvo waveform flags to SAF waveform types
  // Extended waveforms are mutually exclusive (Option A)
  if (VoiceWave and SIDEVO_WAVE_SINE) <> 0 then
    Osc.Waveform := wtSine
  else if (VoiceWave and SIDEVO_WAVE_SUPERSAW) <> 0 then
    Osc.Waveform := wtSuperSaw
  else if (VoiceWave and SIDEVO_WAVE_PWM) <> 0 then
    Osc.Waveform := wtPWM
  else if (VoiceWave and SIDEVO_WAVE_HALFSIN) <> 0 then
    Osc.Waveform := wtHalfSine
  else
    // No extended waveform selected
    Exit;

  // Generate sample using SAF's TSedaiOscillator
  // This inherits all extended waveform implementations from SAF
  Result := Osc.GenerateSample;
end;

function TSedaiSIDEvo.ApplyBitCrushing(ASample: Single): Single;
begin
  // 12-bit quantization (SID output resolution)
  if FState.EnableBitCrushing then
    Result := Round(ASample * 2048) / 2048.0
  else
    Result := ASample;
end;

// ============================================================================
// LFO API IMPLEMENTATION
// ============================================================================

procedure TSedaiSIDEvo.SetVoiceLFO(AVoice: Integer; ARate, ADepth: Single; ATarget: TSIDEvoLFOTarget);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].LFORate := ARate;
  FState.Voices[AVoice].LFODepth := ADepth;
  FState.Voices[AVoice].LFOTarget := ATarget;
end;

procedure TSedaiSIDEvo.SetVoiceLFORate(AVoice: Integer; ARate: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].LFORate := ARate;
end;

procedure TSedaiSIDEvo.SetVoiceLFODepth(AVoice: Integer; ADepth: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].LFODepth := ADepth;
end;

procedure TSedaiSIDEvo.SetVoiceLFOTarget(AVoice: Integer; ATarget: TSIDEvoLFOTarget);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].LFOTarget := ATarget;
end;

procedure TSedaiSIDEvo.SetGlobalLFO(ARate, ADepth: Single; ATarget: TSIDEvoLFOTarget);
begin
  FState.GlobalLFORate := ARate;
  FState.GlobalLFODepth := ADepth;
  FState.GlobalLFOTarget := ATarget;
end;

// ============================================================================
// MODULATION API IMPLEMENTATION
// ============================================================================

procedure TSedaiSIDEvo.SetRingModulation(AVoice: Integer; AEnabled: Boolean; ASourceVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if AEnabled then
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags or SIDEVO_FLAG_RING_MOD
  else
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags and (not SIDEVO_FLAG_RING_MOD);
  if ASourceVoice >= 0 then
    FState.Voices[AVoice].RingModSource := ASourceVoice
  else if ASourceVoice < 0 then
    FState.Voices[AVoice].RingModSource := (AVoice + SIDEVO_MAX_VOICES - 1) mod SIDEVO_MAX_VOICES;
end;

procedure TSedaiSIDEvo.SetHardSync(AVoice: Integer; AEnabled: Boolean; ASourceVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if AEnabled then
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags or SIDEVO_FLAG_SYNC
  else
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags and (not SIDEVO_FLAG_SYNC);
  if ASourceVoice >= 0 then
    FState.Voices[AVoice].SyncSource := ASourceVoice
  else if ASourceVoice < 0 then
    FState.Voices[AVoice].SyncSource := (AVoice + SIDEVO_MAX_VOICES - 1) mod SIDEVO_MAX_VOICES;
end;

procedure TSedaiSIDEvo.SetModWheel(AVoice: Integer; AValue: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if AValue < 0.0 then AValue := 0.0;
  if AValue > 1.0 then AValue := 1.0;
  FState.Voices[AVoice].ModWheel := AValue;
end;

procedure TSedaiSIDEvo.SetPitchBend(AVoice: Integer; AValue: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if AValue < -1.0 then AValue := -1.0;
  if AValue > 1.0 then AValue := 1.0;
  FState.Voices[AVoice].PitchBend := AValue;
end;

procedure TSedaiSIDEvo.SetGlobalPitchBend(AValue: Single);
var
  I: Integer;
begin
  if AValue < -1.0 then AValue := -1.0;
  if AValue > 1.0 then AValue := 1.0;
  for I := 0 to SIDEVO_MAX_VOICES - 1 do
    FState.Voices[I].PitchBend := AValue;
end;

procedure TSedaiSIDEvo.SetSupersawDetune(AVoice: Integer; ADetune: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if ADetune < 0.0 then ADetune := 0.0;
  if ADetune > 1.0 then ADetune := 1.0;
  FState.Voices[AVoice].SupersawDetune := ADetune;
end;

procedure TSedaiSIDEvo.SetSupersawMix(AVoice: Integer; AMix: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if AMix < 0.0 then AMix := 0.0;
  if AMix > 1.0 then AMix := 1.0;
  FState.Voices[AVoice].SupersawMix := AMix;
end;

procedure TSedaiSIDEvo.SetDetune(AVoice: Integer; ADetune: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  if ADetune < -1.0 then ADetune := -1.0;
  if ADetune > 1.0 then ADetune := 1.0;
  FState.Voices[AVoice].Detune := ADetune;
end;

// ============================================================================
// BASIC-FRIENDLY HIGH-LEVEL API IMPLEMENTATION
// ============================================================================

procedure TSedaiSIDEvo.Sound(AVoice: Integer; AFreqHz: Single; ADurationMs: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  SetFrequencyHz(AVoice, AFreqHz);
  GateOn(AVoice);
  // Note: Duration handling would require a timer mechanism
  // For now, just start the sound - user must call GateOff manually
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
var
  Cutoff11: Word;
  Res4: Byte;
begin
  // Convert 0-1 ranges to SID register values
  Cutoff11 := Round(ACutoff * 2047);
  Res4 := Round(AResonance * 15);
  SetFilterCutoff(Cutoff11);
  SetFilterResonance(Res4);
  // Route this voice to filter
  if (AVoice >= 0) and (AVoice < SIDEVO_MAX_VOICES) then
    FState.Voices[AVoice].Flags := FState.Voices[AVoice].Flags or SIDEVO_FLAG_FILTER;
  // Set filter mode based on type
  SetFilterMode(
    (AFilterType and SIDEVO_FILTER_LOWPASS) <> 0,
    (AFilterType and SIDEVO_FILTER_BANDPASS) <> 0,
    (AFilterType and SIDEVO_FILTER_HIGHPASS) <> 0
  );
end;

procedure TSedaiSIDEvo.Pan(AVoice: Integer; APanPosition: Single);
begin
  SetVoicePan(AVoice, APanPosition);
end;

procedure TSedaiSIDEvo.SetVol(AVoice: Integer; AVolume: Single);
begin
  SetVoiceVolume(AVoice, AVolume);
end;

procedure TSedaiSIDEvo.SetVol(AVolume: Single);
begin
  SetMasterVolume(AVolume);
end;

procedure TSedaiSIDEvo.LFO(AVoice: Integer; ARate, ADepth: Single; ATarget: Integer);
begin
  SetVoiceLFO(AVoice, ARate, ADepth, TSIDEvoLFOTarget(ATarget));
end;

// ============================================================================
// PLAYBACK CONTROL (extended) IMPLEMENTATION
// ============================================================================

procedure TSedaiSIDEvo.PlayFrequency(AVoice: Integer; AFreqHz: Single; AVolume: Single);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  SetFrequencyHz(AVoice, AFreqHz);
  SetVoiceVolume(AVoice, AVolume);
  GateOn(AVoice);
end;

procedure TSedaiSIDEvo.StopGroup(AGroupIndex: Integer);
var
  I, StartVoice, EndVoice: Integer;
begin
  if (AGroupIndex < 0) or (AGroupIndex >= SIDEVO_MAX_GROUPS) then Exit;
  StartVoice := AGroupIndex * SIDEVO_VOICES_PER_GROUP;
  EndVoice := StartVoice + SIDEVO_VOICES_PER_GROUP - 1;
  for I := StartVoice to EndVoice do
    GateOff(I);
end;

procedure TSedaiSIDEvo.Panic;
var
  I: Integer;
begin
  // Emergency stop: immediate release on all voices
  for I := 0 to SIDEVO_MAX_VOICES - 1 do
  begin
    FState.Voices[I].SIDEnv.Release := 0;  // Fastest release
    GateOff(I);
    FState.Voices[I].IsActive := False;
  end;
end;

procedure TSedaiSIDEvo.UpdateActiveVoice(AVoice: Integer);
begin
  // Placeholder for real-time parameter updates
  // The voice parameters are already updated directly
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  // Nothing specific needed - parameters are read in real-time during audio generation
end;

// ============================================================================
// TABLE-DRIVEN MODULATION API IMPLEMENTATION
// ============================================================================

procedure TSedaiSIDEvo.SetWaveTable(AVoice: Integer; const AEntries: array of TSIDEvoTableEntry);
var
  I, Len: Integer;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  Len := Length(AEntries);
  if Len > 256 then Len := 256;
  for I := 0 to Len - 1 do
    FState.Voices[AVoice].TableState.WaveTable.Entries[I] := AEntries[I];
  FState.Voices[AVoice].TableState.WaveTable.Length := Len;
end;

procedure TSedaiSIDEvo.EnableWaveTable(AVoice: Integer; AEnabled: Boolean);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].TableState.WaveTable.Enabled := AEnabled;
  if AEnabled then
    FState.Voices[AVoice].TableState.WavePos := 0;
end;

procedure TSedaiSIDEvo.ResetWaveTable(AVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].TableState.WavePos := 0;
  FState.Voices[AVoice].TableState.WaveDelay := 0;
end;

procedure TSedaiSIDEvo.SetPulseTable(AVoice: Integer; const AEntries: array of TSIDEvoTableEntry);
var
  I, Len: Integer;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  Len := Length(AEntries);
  if Len > 256 then Len := 256;
  for I := 0 to Len - 1 do
    FState.Voices[AVoice].TableState.PulseTable.Entries[I] := AEntries[I];
  FState.Voices[AVoice].TableState.PulseTable.Length := Len;
end;

procedure TSedaiSIDEvo.EnablePulseTable(AVoice: Integer; AEnabled: Boolean);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].TableState.PulseTable.Enabled := AEnabled;
  if AEnabled then
    FState.Voices[AVoice].TableState.PulsePos := 0;
end;

procedure TSedaiSIDEvo.ResetPulseTable(AVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].TableState.PulsePos := 0;
  FState.Voices[AVoice].TableState.PulseDelay := 0;
end;

procedure TSedaiSIDEvo.SetFilterTable(AVoice: Integer; const AEntries: array of TSIDEvoTableEntry);
var
  I, Len: Integer;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  Len := Length(AEntries);
  if Len > 256 then Len := 256;
  for I := 0 to Len - 1 do
    FState.Voices[AVoice].TableState.FilterTable.Entries[I] := AEntries[I];
  FState.Voices[AVoice].TableState.FilterTable.Length := Len;
end;

procedure TSedaiSIDEvo.EnableFilterTable(AVoice: Integer; AEnabled: Boolean);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].TableState.FilterTable.Enabled := AEnabled;
  if AEnabled then
    FState.Voices[AVoice].TableState.FilterPos := 0;
end;

procedure TSedaiSIDEvo.ResetFilterTable(AVoice: Integer);
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  FState.Voices[AVoice].TableState.FilterPos := 0;
  FState.Voices[AVoice].TableState.FilterDelay := 0;
end;

procedure TSedaiSIDEvo.ProcessTables(AVoice: Integer);
var
  Entry: TSIDEvoTableEntry;
  TS: ^TSIDEvoTableState;
begin
  if (AVoice < 0) or (AVoice >= SIDEVO_MAX_VOICES) then Exit;
  TS := @FState.Voices[AVoice].TableState;

  // Process WaveTable
  if TS^.WaveTable.Enabled and (TS^.WaveTable.Length > 0) then
  begin
    if TS^.WaveDelay > 0 then
      Dec(TS^.WaveDelay)
    else if TS^.WavePos < TS^.WaveTable.Length then
    begin
      Entry := TS^.WaveTable.Entries[TS^.WavePos];
      case Entry.Command of
        stcEnd: TS^.WaveTable.Enabled := False;
        stcJump: TS^.WavePos := Entry.Param1 - 1;
        stcDelay: TS^.WaveDelay := Entry.Param1;
        stcSetWaveform: SetWaveform(AVoice, Entry.Param1);
      end;
      Inc(TS^.WavePos);
    end;
  end;

  // Process PulseTable
  if TS^.PulseTable.Enabled and (TS^.PulseTable.Length > 0) then
  begin
    if TS^.PulseDelay > 0 then
      Dec(TS^.PulseDelay)
    else if TS^.PulsePos < TS^.PulseTable.Length then
    begin
      Entry := TS^.PulseTable.Entries[TS^.PulsePos];
      case Entry.Command of
        stcEnd: TS^.PulseTable.Enabled := False;
        stcJump: TS^.PulsePos := Entry.Param1 - 1;
        stcDelay: TS^.PulseDelay := Entry.Param1;
        stcSetPulseWidth: SetPulseWidth(AVoice, Entry.Param1 / 4095.0);
        stcPulseWidthUp: SetPulseWidth(AVoice, FState.Voices[AVoice].PulseWidth + Entry.Param1 / 4095.0);
        stcPulseWidthDown: SetPulseWidth(AVoice, FState.Voices[AVoice].PulseWidth - Entry.Param1 / 4095.0);
      end;
      Inc(TS^.PulsePos);
    end;
  end;

  // Process FilterTable
  if TS^.FilterTable.Enabled and (TS^.FilterTable.Length > 0) then
  begin
    if TS^.FilterDelay > 0 then
      Dec(TS^.FilterDelay)
    else if TS^.FilterPos < TS^.FilterTable.Length then
    begin
      Entry := TS^.FilterTable.Entries[TS^.FilterPos];
      case Entry.Command of
        stcEnd: TS^.FilterTable.Enabled := False;
        stcJump: TS^.FilterPos := Entry.Param1 - 1;
        stcDelay: TS^.FilterDelay := Entry.Param1;
        stcSetFilterCutoff: SetFilterCutoff(Entry.Param1);
      end;
      Inc(TS^.FilterPos);
    end;
  end;
end;

procedure TSedaiSIDEvo.ProcessAllTables;
var
  I: Integer;
begin
  for I := 0 to GetEffectiveVoiceCount - 1 do
    ProcessTables(I);
end;

// ============================================================================
// UTILITY (extended) IMPLEMENTATION
// ============================================================================

const
  NOTE_NAMES: array[0..11] of string = (
    'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B'
  );

function TSedaiSIDEvo.FrequencyToNote(AFreqHz: Single): Integer;
var
  I: Integer;
  BestMatch: Integer;
  BestDiff, Diff: Single;
begin
  BestMatch := 69; // A4 default
  BestDiff := 99999.0;
  for I := 0 to SIDEVO_TOTAL_NOTES - 1 do
  begin
    Diff := Abs(NOTE_FREQUENCIES[I] - AFreqHz);
    if Diff < BestDiff then
    begin
      BestDiff := Diff;
      BestMatch := I;
    end;
  end;
  Result := BestMatch;
end;

function TSedaiSIDEvo.GetNoteName(ANoteNumber: Integer): string;
var
  Octave, NoteInOctave: Integer;
begin
  if (ANoteNumber < 0) or (ANoteNumber >= SIDEVO_TOTAL_NOTES) then
  begin
    Result := '?';
    Exit;
  end;
  Octave := ANoteNumber div 12;
  NoteInOctave := ANoteNumber mod 12;
  Result := NOTE_NAMES[NoteInOctave] + IntToStr(Octave);
end;

function TSedaiSIDEvo.GetActiveVoiceCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GetEffectiveVoiceCount - 1 do
    if FState.Voices[I].IsActive then
      Inc(Result);
end;

function TSedaiSIDEvo.GetGroupInfo(AGroupIndex: Integer): string;
var
  StateStr: string;
begin
  Result := '';
  if (AGroupIndex < 0) or (AGroupIndex >= SIDEVO_MAX_GROUPS) then Exit;
  case FState.Groups[AGroupIndex].State of
    sgsDisabled: StateStr := 'Disabled';
    sgsEnabled: StateStr := 'Enabled';
    sgsSuspended: StateStr := 'Suspended';
  end;
  Result := Format('Group %d: %s, Vol=%.2f, Pan=%.2f, Transpose=%d',
    [AGroupIndex, StateStr,
     FState.Groups[AGroupIndex].Volume,
     FState.Groups[AGroupIndex].Pan,
     FState.Groups[AGroupIndex].Transpose]);
end;

procedure TSedaiSIDEvo.PrintStatus;
var
  I: Integer;
begin
  WriteLn('');
  WriteLn('SEDAI SID EVO STATUS');
  WriteLn('====================');
  WriteLn('Master Volume: ', FState.MasterVolume:0:2);
  WriteLn('Stereo Width:  ', FState.StereoWidth:0:2);
  WriteLn('Tuning (A4):   ', FState.Tuning:0:2, ' Hz');
  WriteLn('Active Groups: ', FState.ActiveGroups);
  WriteLn('Active Voices: ', GetActiveVoiceCount, ' / ', GetEffectiveVoiceCount);
  WriteLn('');
  WriteLn('Authenticity:  ', Ord(FState.AuthenticityLevel));
  WriteLn('Clock Mode:    ', Ord(FState.ClockMode));
  WriteLn('');
  for I := 0 to FState.ActiveGroups - 1 do
    WriteLn(GetGroupInfo(I));
  WriteLn('');
end;

// ============================================================================
// SID AUTHENTICITY CONTROL IMPLEMENTATION
// ============================================================================

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

procedure TSedaiSIDEvo.SetDACNonLinearity(AAmount: Single);
begin
  if AAmount < 0.0 then AAmount := 0.0;
  if AAmount > 1.0 then AAmount := 1.0;
  FState.DACNonLinearity := AAmount;
end;

procedure TSedaiSIDEvo.SetSIDMode;
begin
  // Quick setup for authentic SID playback
  SetAuthenticityLevel(salSIDFull);
  FState.EnableBitCrushing := True;
  FState.EnableWaveformAND := True;
  FState.EnableLFSRNoise := True;
  FState.DACNonLinearity := 0.96;
end;

procedure TSedaiSIDEvo.SetEvolvedMode;
begin
  // Quick setup for clean, evolved sound
  SetAuthenticityLevel(salEvolved);
  FState.EnableBitCrushing := False;
  FState.EnableWaveformAND := False;
  FState.EnableLFSRNoise := False;
  FState.DACNonLinearity := 0.0;
end;

procedure TSedaiSIDEvo.SetTuning(AA4Freq: Single);
begin
  if AA4Freq < 400.0 then AA4Freq := 400.0;
  if AA4Freq > 480.0 then AA4Freq := 480.0;
  FState.Tuning := AA4Freq;
end;

// ============================================================================
// GLOBAL FUNCTIONS IMPLEMENTATION
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

procedure SIDEvoPan(AVoice: Integer; APan: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.Pan(AVoice, APan);
end;

procedure SIDEvoVol(AVolume: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.SetVol(AVolume);
end;

procedure SIDEvoVol(AVoice: Integer; AVolume: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.SetVol(AVoice, AVolume);
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

procedure SIDEvoFilter(AVoice: Integer; AType: Byte; ACutoff, AResonance: Single);
begin
  if SIDEvo <> nil then
    SIDEvo.Filter(AVoice, AType, ACutoff, AResonance);
end;

end.
