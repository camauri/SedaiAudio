{*
 * Sedai Audio Foundation - Main API
 *
 * This unit provides the unified public API for the Sedai Audio Foundation
 * library. It wraps all synthesis engines, voice management, and audio output
 * in a simple, easy-to-use interface.
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAudioFoundation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  SedaiAudioTypes, SedaiAudioBackend, SedaiOscillator, SedaiEnvelope,
  SedaiFilter, SedaiFMOperator, SedaiWavetableGenerator, SedaiWavetableLoader,
  SedaiVoice, SedaiVoiceManager, SedaiPart, SedaiEngine, SedaiSpatialAudio;

const
  // ============================================================================
  // CONFIGURATION
  // ============================================================================
  SAF_MAX_VOICES = 64;
  MIDI_MAX_VOICES = 256;
  SAF_DEFAULT_SAMPLE_RATE = 44100;
  SAF_DEFAULT_BUFFER_SIZE = 1024;

  // ============================================================================
  // NOTE FREQUENCIES (A4 = 440 Hz) - Full Audible Spectrum 20 Hz to 20 kHz
  // ============================================================================
  // Octave 0 (sub-bass, ~16-31 Hz)
  NoteC0  = 16.35;   NoteCs0 = 17.32;   NoteD0  = 18.35;   NoteDs0 = 19.45;
  NoteE0  = 20.60;   NoteF0  = 21.83;   NoteFs0 = 23.12;   NoteG0  = 24.50;
  NoteGs0 = 25.96;   NoteA0  = 27.50;   NoteAs0 = 29.14;   NoteB0  = 30.87;
  // Octave 1
  NoteC1  = 32.70;   NoteCs1 = 34.65;   NoteD1  = 36.71;   NoteDs1 = 38.89;
  NoteE1  = 41.20;   NoteF1  = 43.65;   NoteFs1 = 46.25;   NoteG1  = 49.00;
  NoteGs1 = 51.91;   NoteA1  = 55.00;   NoteAs1 = 58.27;   NoteB1  = 61.74;
  // Octave 2
  NoteC2  = 65.41;   NoteCs2 = 69.30;   NoteD2  = 73.42;   NoteDs2 = 77.78;
  NoteE2  = 82.41;   NoteF2  = 87.31;   NoteFs2 = 92.50;   NoteG2  = 98.00;
  NoteGs2 = 103.83;  NoteA2  = 110.00;  NoteAs2 = 116.54;  NoteB2  = 123.47;
  // Octave 3
  NoteC3  = 130.81;  NoteCs3 = 138.59;  NoteD3  = 146.83;  NoteDs3 = 155.56;
  NoteE3  = 164.81;  NoteF3  = 174.61;  NoteFs3 = 185.00;  NoteG3  = 196.00;
  NoteGs3 = 207.65;  NoteA3  = 220.00;  NoteAs3 = 233.08;  NoteB3  = 246.94;
  // Octave 4 (Middle C)
  NoteC4  = 261.63;  NoteCs4 = 277.18;  NoteD4  = 293.66;  NoteDs4 = 311.13;
  NoteE4  = 329.63;  NoteF4  = 349.23;  NoteFs4 = 369.99;  NoteG4  = 392.00;
  NoteGs4 = 415.30;  NoteA4  = 440.00;  NoteAs4 = 466.16;  NoteB4  = 493.88;
  // Octave 5
  NoteC5  = 523.25;  NoteCs5 = 554.37;  NoteD5  = 587.33;  NoteDs5 = 622.25;
  NoteE5  = 659.25;  NoteF5  = 698.46;  NoteFs5 = 739.99;  NoteG5  = 783.99;
  NoteGs5 = 830.61;  NoteA5  = 880.00;  NoteAs5 = 932.33;  NoteB5  = 987.77;
  // Octave 6
  NoteC6  = 1046.50; NoteCs6 = 1108.73; NoteD6  = 1174.66; NoteDs6 = 1244.51;
  NoteE6  = 1318.51; NoteF6  = 1396.91; NoteFs6 = 1479.98; NoteG6  = 1567.98;
  NoteGs6 = 1661.22; NoteA6  = 1760.00; NoteAs6 = 1864.66; NoteB6  = 1975.53;
  // Octave 7
  NoteC7  = 2093.00; NoteCs7 = 2217.46; NoteD7  = 2349.32; NoteDs7 = 2489.02;
  NoteE7  = 2637.02; NoteF7  = 2793.83; NoteFs7 = 2959.96; NoteG7  = 3135.96;
  NoteGs7 = 3322.44; NoteA7  = 3520.00; NoteAs7 = 3729.31; NoteB7  = 3951.07;
  // Octave 8
  NoteC8  = 4186.01; NoteCs8 = 4434.92; NoteD8  = 4698.63; NoteDs8 = 4978.03;
  NoteE8  = 5274.04; NoteF8  = 5587.65; NoteFs8 = 5919.91; NoteG8  = 6271.93;
  NoteGs8 = 6644.88; NoteA8  = 7040.00; NoteAs8 = 7458.62; NoteB8  = 7902.13;
  // Octave 9
  NoteC9  = 8372.02; NoteCs9 = 8869.84; NoteD9  = 9397.27; NoteDs9 = 9956.06;
  NoteE9  = 10548.08; NoteF9  = 11175.30; NoteFs9 = 11839.82; NoteG9  = 12543.85;
  NoteGs9 = 13289.75; NoteA9  = 14080.00; NoteAs9 = 14917.24; NoteB9  = 15804.27;
  // Octave 10 (up to ~20 kHz - theoretical human hearing limit)
  NoteC10  = 16744.04; NoteCs10 = 17739.69; NoteD10  = 18794.55; NoteDs10 = 19912.13;

  // ============================================================================
  // UNDERSCORE ALIASES (NOTE_XX format) - Full Audible Spectrum
  // ============================================================================
  // Octave 0
  NOTE_C0  = NoteC0;   NOTE_CS0 = NoteCs0;  NOTE_D0  = NoteD0;   NOTE_DS0 = NoteDs0;
  NOTE_E0  = NoteE0;   NOTE_F0  = NoteF0;   NOTE_FS0 = NoteFs0;  NOTE_G0  = NoteG0;
  NOTE_GS0 = NoteGs0;  NOTE_A0  = NoteA0;   NOTE_AS0 = NoteAs0;  NOTE_B0  = NoteB0;
  // Octave 1
  NOTE_C1  = NoteC1;   NOTE_CS1 = NoteCs1;  NOTE_D1  = NoteD1;   NOTE_DS1 = NoteDs1;
  NOTE_E1  = NoteE1;   NOTE_F1  = NoteF1;   NOTE_FS1 = NoteFs1;  NOTE_G1  = NoteG1;
  NOTE_GS1 = NoteGs1;  NOTE_A1  = NoteA1;   NOTE_AS1 = NoteAs1;  NOTE_B1  = NoteB1;
  // Octave 2
  NOTE_C2  = NoteC2;   NOTE_CS2 = NoteCs2;  NOTE_D2  = NoteD2;   NOTE_DS2 = NoteDs2;
  NOTE_E2  = NoteE2;   NOTE_F2  = NoteF2;   NOTE_FS2 = NoteFs2;  NOTE_G2  = NoteG2;
  NOTE_GS2 = NoteGs2;  NOTE_A2  = NoteA2;   NOTE_AS2 = NoteAs2;  NOTE_B2  = NoteB2;
  // Octave 3
  NOTE_C3  = NoteC3;   NOTE_CS3 = NoteCs3;  NOTE_D3  = NoteD3;   NOTE_DS3 = NoteDs3;
  NOTE_E3  = NoteE3;   NOTE_F3  = NoteF3;   NOTE_FS3 = NoteFs3;  NOTE_G3  = NoteG3;
  NOTE_GS3 = NoteGs3;  NOTE_A3  = NoteA3;   NOTE_AS3 = NoteAs3;  NOTE_B3  = NoteB3;
  // Octave 4
  NOTE_C4  = NoteC4;   NOTE_CS4 = NoteCs4;  NOTE_D4  = NoteD4;   NOTE_DS4 = NoteDs4;
  NOTE_E4  = NoteE4;   NOTE_F4  = NoteF4;   NOTE_FS4 = NoteFs4;  NOTE_G4  = NoteG4;
  NOTE_GS4 = NoteGs4;  NOTE_A4  = NoteA4;   NOTE_AS4 = NoteAs4;  NOTE_B4  = NoteB4;
  // Octave 5
  NOTE_C5  = NoteC5;   NOTE_CS5 = NoteCs5;  NOTE_D5  = NoteD5;   NOTE_DS5 = NoteDs5;
  NOTE_E5  = NoteE5;   NOTE_F5  = NoteF5;   NOTE_FS5 = NoteFs5;  NOTE_G5  = NoteG5;
  NOTE_GS5 = NoteGs5;  NOTE_A5  = NoteA5;   NOTE_AS5 = NoteAs5;  NOTE_B5  = NoteB5;
  // Octave 6
  NOTE_C6  = NoteC6;   NOTE_CS6 = NoteCs6;  NOTE_D6  = NoteD6;   NOTE_DS6 = NoteDs6;
  NOTE_E6  = NoteE6;   NOTE_F6  = NoteF6;   NOTE_FS6 = NoteFs6;  NOTE_G6  = NoteG6;
  NOTE_GS6 = NoteGs6;  NOTE_A6  = NoteA6;   NOTE_AS6 = NoteAs6;  NOTE_B6  = NoteB6;
  // Octave 7
  NOTE_C7  = NoteC7;   NOTE_CS7 = NoteCs7;  NOTE_D7  = NoteD7;   NOTE_DS7 = NoteDs7;
  NOTE_E7  = NoteE7;   NOTE_F7  = NoteF7;   NOTE_FS7 = NoteFs7;  NOTE_G7  = NoteG7;
  NOTE_GS7 = NoteGs7;  NOTE_A7  = NoteA7;   NOTE_AS7 = NoteAs7;  NOTE_B7  = NoteB7;
  // Octave 8
  NOTE_C8  = NoteC8;   NOTE_CS8 = NoteCs8;  NOTE_D8  = NoteD8;   NOTE_DS8 = NoteDs8;
  NOTE_E8  = NoteE8;   NOTE_F8  = NoteF8;   NOTE_FS8 = NoteFs8;  NOTE_G8  = NoteG8;
  NOTE_GS8 = NoteGs8;  NOTE_A8  = NoteA8;   NOTE_AS8 = NoteAs8;  NOTE_B8  = NoteB8;
  // Octave 9
  NOTE_C9  = NoteC9;   NOTE_CS9 = NoteCs9;  NOTE_D9  = NoteD9;   NOTE_DS9 = NoteDs9;
  NOTE_E9  = NoteE9;   NOTE_F9  = NoteF9;   NOTE_FS9 = NoteFs9;  NOTE_G9  = NoteG9;
  NOTE_GS9 = NoteGs9;  NOTE_A9  = NoteA9;   NOTE_AS9 = NoteAs9;  NOTE_B9  = NoteB9;
  // Octave 10 (up to ~20 kHz)
  NOTE_C10  = NoteC10;  NOTE_CS10 = NoteCs10; NOTE_D10  = NoteD10;  NOTE_DS10 = NoteDs10;

type
  // Synthesis type
  TSAFSynthType = (
    safClassic,       // Classic subtractive synthesis
    safFM,            // FM synthesis (DX7-style)
    safWavetable      // Wavetable synthesis
  );

  // Voice state
  TSAFVoiceState = record
    Active: Boolean;
    SynthType: TSAFSynthType;
    Frequency: Single;
    Amplitude: Single;
    Pan: Single;
    // Components
    Oscillator: TSedaiOscillator;
    Envelope: TSedaiEnvelope;
    Filter: TSedaiFilter;
    FMSynth: TSedaiFMSynth;
    WTGenerator: TSedaiWavetableGenerator;
  end;

  // MIDI voice state for voice pool
  TMIDIVoiceState = record
    IsAllocated: Boolean;
    IsPlaying: Boolean;
    Frequency: Single;
    Amplitude: Single;
    Pan: Single;
    WavetableType: string;
    SynthVoiceIndex: Integer;
    NoteNumber: Byte;
    Velocity: Byte;
  end;

// ============================================================================
// INITIALIZATION
// ============================================================================

function InitAudio(AMaxVoices: Integer = SAF_MAX_VOICES): Boolean;
procedure ShutdownAudio;

function GetSampleRate: Cardinal;
function GetActiveVoices: Integer;
function GetMaxVoices: Integer;

// ============================================================================
// MASTER VOLUME
// ============================================================================

procedure SetMasterVolume(AVolume: Single);
function GetMasterVolume: Single;

// ============================================================================
// CLASSIC SYNTHESIS (Subtractive)
// ============================================================================

procedure PlaySine(AFreq: Single; AAmplitude: Single = 0.5);
procedure PlaySquare(AFreq: Single; AAmplitude: Single = 0.5);
procedure PlaySaw(AFreq: Single; AAmplitude: Single = 0.5);
procedure PlayTriangle(AFreq: Single; AAmplitude: Single = 0.5);
procedure PlayPulse(AFreq: Single; APulseWidth: Single = 0.5; AAmplitude: Single = 0.5);
procedure PlayNoise(AAmplitude: Single = 0.3);

procedure PlayClassic(AFreq: Single; const APreset: string = 'sine');
procedure PlayLead(AFreq: Single);
procedure PlayBass(AFreq: Single);
procedure PlayPad(AFreq: Single);

// ============================================================================
// FM SYNTHESIS
// ============================================================================

procedure PlayFM(AFreq: Single; const APreset: string = 'epiano');
procedure PlayEPiano(AFreq: Single);
procedure PlayFMBrass(AFreq: Single);
procedure PlayFMBell(AFreq: Single);
procedure PlayFMOrgan(AFreq: Single);
procedure PlayFMLead(AFreq: Single);
procedure PlayFMBass(AFreq: Single);

// ============================================================================
// WAVETABLE SYNTHESIS
// ============================================================================

procedure PlayWavetable(AFreq: Single; const APreset: string = 'basic');
procedure PlaySerum(AFreq: Single);
procedure PlaySuperSaw(AFreq: Single);
procedure PlayPWM(AFreq: Single);
procedure PlayWasp(AFreq: Single);
procedure PlayPPG(AFreq: Single);
procedure PlayScaleWavetable(ABaseFreq: Single; const APreset: string = 'basic');

// Wavetable Loading API
function LoadWavetableFile(const AFilename: string): Boolean;
function LoadWavetableDirectory(const APath: string): Integer;
procedure ScanWavetableDirectory(const APath: string);
function GetLoadedWavetableCount: Integer;
function GetLoadedWavetableName(AIndex: Integer): string;
function GetLoadedWavetables: TStringArray;
function IsWavetableLoaded(const AName: string): Boolean;
function GetWavetableCacheInfo: string;
function GetWavetableFormats: TStringArray;
procedure ClearWavetableCache;
procedure PlayLoadedWavetable(AFreq: Single; const AWavetableName: string);
function PlayLoadedWavetableAdv(AFreq: Single; const AWavetableName: string): Integer;
procedure PlayCustomWavetable(AFreq: Single; const ACustomWavetable: TWavetable);
function PlayCustomWavetableAdv(AFreq: Single; const ACustomWavetable: TWavetable): Integer;
procedure ListLoadedWavetables;
procedure PrintWavetableInfo(const AFilename: string);

// ============================================================================
// ADVANCED VOICE CONTROL (returns voice index)
// ============================================================================

function PlayClassicAdv(AFreq: Single; const APreset: string = 'sine'): Integer;
function PlayFMAdv(AFreq: Single; const APreset: string = 'epiano'): Integer;
function PlayWavetableAdv(AFreq: Single; const APreset: string = 'basic'): Integer;

procedure NoteOff(AVoiceIndex: Integer);
procedure NoteRelease(AVoiceIndex: Integer);
procedure SetVoicePan(AVoiceIndex: Integer; APan: Single);
procedure SetVoiceFrequency(AVoiceIndex: Integer; AFreq: Single);
procedure SetVoiceAmplitude(AVoiceIndex: Integer; AAmplitude: Single);
procedure SetVoiceADSR(AVoiceIndex: Integer; AAttack, ADecay, ASustain, ARelease: Single);
procedure RetriggerVoice(AVoiceIndex: Integer);
procedure RetriggerVoiceHard(AVoiceIndex: Integer);
procedure SetVoicePulseWidth(AVoiceIndex: Integer; APulseWidth: Single);
procedure SetVoiceFilter(AVoiceIndex: Integer; AEnabled: Boolean;
  AFilterType: TFilterType; AFreq, AQ: Single; ASlope: TFilterSlope = fs12dB);
procedure SetVoiceFilterEnabled(AVoiceIndex: Integer; AEnabled: Boolean);
procedure SetVoiceFilterParams(AVoiceIndex: Integer; AFreq, AQ: Single);

// ============================================================================
// SYSTEM CONTROL
// ============================================================================

procedure StopAll;
procedure SmoothStopAll(AFadeTimeMs: Integer = 300);
procedure PrintStatus;

// Utility functions
procedure Beep(AFreq: Single; ADurationMs: Integer; AAmplitude: Single = 0.3);
procedure PlayNote(AChannel: Integer; AFreq: Single; ADurationMs: Integer;
  AAmplitude: Single = 0.5; const APreset: string = 'sine');
procedure PlayOnVoice(AVoiceIndex: Integer; AFreq: Single; const APreset: string = 'sine');
procedure ReleaseVoice(AVoiceIndex: Integer);

// ============================================================================
// MIDI UTILITIES
// ============================================================================

function MIDINoteToFreq(ANote: Byte): Single;
function FreqToMIDINote(AFreq: Single): Byte;
function MIDIVelocityToAmplitude(AVelocity: Byte): Single;
function MIDIAmplitudeToVelocity(AAmplitude: Single): Byte;
function MIDIPanToSedai(APan: Byte): Single;
function SedaiPanToMIDI(APan: Single): Byte;
function MIDINoteToName(ANote: Byte): string;
function MIDINoteToOctave(ANote: Byte): Integer;

// ============================================================================
// MIDI VOICE MANAGEMENT
// ============================================================================

function MIDIAllocateVoice: Integer;
function MIDIIsVoiceActive(AVoiceIndex: Integer): Boolean;
function MIDIGetFreeVoiceCount: Integer;
procedure MIDIVoiceOn(AVoiceIndex: Integer);
procedure MIDIVoiceOff(AVoiceIndex: Integer);
procedure MIDIReleaseVoice(AVoiceIndex: Integer);
procedure MIDIReleaseAllVoices;
procedure MIDISetVoiceFrequency(AVoiceIndex: Integer; AFrequency: Single);
procedure MIDISetVoiceAmplitude(AVoiceIndex: Integer; AAmplitude: Single);
procedure MIDISetVoiceWavetable(AVoiceIndex: Integer; const AWavetableType: string);
procedure MIDISetVoicePan(AVoiceIndex: Integer; APan: Single);
function MIDIGetVoiceFrequency(AVoiceIndex: Integer): Single;
function MIDIGetVoiceAmplitude(AVoiceIndex: Integer): Single;
function MIDIGetVoiceWavetable(AVoiceIndex: Integer): string;
function MIDIGetVoicePan(AVoiceIndex: Integer): Single;
function MIDIPlayNote(ANote, AVelocity: Byte; const APreset: string = 'basic'): Integer;
function MIDIPlayNoteWithFreq(AFrequency: Single; AVelocity: Byte; const APreset: string = 'basic'): Integer;
function PlayWavetableMIDI(AFrequency: Single; const AWavetableType: string;
  AVelocity: Byte; APan: Byte = 64): Integer;

// ============================================================================
// MIDI INTEGRATION (Callback for external MIDI sequencer)
// ============================================================================

type
  TMIDIUpdateProc = procedure(ADeltaTimeSeconds: Single);

procedure RegisterMidiUpdateCallback(ACallback: TMIDIUpdateProc);
procedure UnregisterMidiUpdateCallback;

// ============================================================================
// CHORD AND SCALE HELPERS
// ============================================================================

procedure PlayChordWavetable(ANote1, ANote2, ANote3: Single; const APreset: string = 'basic');

procedure PlayChordClassic(ANote1, ANote2, ANote3: Single; const APreset: string = 'sine');
procedure PlayChordFM(ANote1, ANote2, ANote3: Single; const APreset: string = 'epiano');
procedure PlayScaleClassic(ABaseFreq: Single; const APreset: string = 'sine');
procedure PlayScaleFM(ABaseFreq: Single; const APreset: string = 'epiano');

implementation

const
  PART_VOICES     = 16;     // polyphony per instrument Part
  HANDLE_PART_MUL = 1000;   // voice handle = partIndex * HANDLE_PART_MUL + slot

var
  GAudioBackend: TSedaiAudioBackend = nil;
  // The modular Part/Instrument engine the facade now delegates to.
  GEngine: TSAFEngine = nil;
  // Part registry: one Part per distinct (source, preset). Index == engine part.
  GPartSrc: array of TSAFPartSource;
  GPartPreset: array of string;
  GPartCount: Integer = 0;
  GMaxVoices: Integer = SAF_MAX_VOICES;
  GMasterVolume: Single = 0.7;
  GSampleRate: Cardinal = SAF_DEFAULT_SAMPLE_RATE;
  GInitialized: Boolean = False;
  // MIDI Voice Pool
  GMIDIVoicePool: array[0..MIDI_MAX_VOICES-1] of TMIDIVoiceState;
  GMIDIVoicePoolInitialized: Boolean = False;
  // MIDI Integration callback
  GMIDIUpdateCallback: TMIDIUpdateProc = nil;

// ============================================================================
// INTERNAL: AUDIO CALLBACK
// ============================================================================

procedure AudioCallback(AOutput: PSingle; AFrameCount: Integer; AUserData: Pointer);
var
  I: Integer;
begin
  // Delegate the whole mix to the modular engine: all Parts -> mixer -> master.
  if not Assigned(GEngine) then
  begin
    for I := 0 to AFrameCount * 2 - 1 do
      AOutput[I] := 0.0;
    Exit;
  end;

  GEngine.RenderBlock(AOutput, AFrameCount);

  // Master volume (0..1 linear) applied post-mix; the master bus already
  // limited/clamped inside RenderBlock.
  if GMasterVolume <> 1.0 then
    for I := 0 to AFrameCount * 2 - 1 do
      AOutput[I] := AOutput[I] * GMasterVolume;
end;

// ============================================================================
// INTERNAL: VOICE ALLOCATION
// ============================================================================

// Resolve a public voice handle (partIndex*HANDLE_PART_MUL + slot) to the
// underlying universal voice, or nil if stale / out of range.
function HandleVoice(AHandle: Integer): TSedaiVoice;
var
  PartIdx, Slot: Integer;
  Part: TSAFPart;
begin
  Result := nil;
  if (AHandle < 0) or not Assigned(GEngine) then Exit;

  PartIdx := AHandle div HANDLE_PART_MUL;
  Slot := AHandle mod HANDLE_PART_MUL;

  Part := GEngine.GetPart(PartIdx);
  if Part = nil then Exit;

  Result := Part.VoiceManager.GetVoice(Slot);
end;

// Find (or lazily create) the Part for a given source + preset. Returns the
// engine part index, or -1 on failure.
function GetOrCreatePart(ASource: TSAFPartSource; const APreset: string): Integer;
var
  I: Integer;
  Part: TSAFPart;
  Nm: string;
begin
  for I := 0 to GPartCount - 1 do
    if (GPartSrc[I] = ASource) and (GPartPreset[I] = APreset) then
      Exit(I);

  Result := -1;
  if not Assigned(GEngine) then Exit;

  case ASource of
    psFM:        Nm := 'FM:' + APreset;
    psWavetable: Nm := 'WT:' + APreset;
  else
    Nm := 'CL:' + APreset;
  end;

  Part := GEngine.AddPart(Nm, PART_VOICES);
  if Part = nil then Exit;
  Part.SetInstrument(ASource, APreset);

  SetLength(GPartSrc, GPartCount + 1);
  SetLength(GPartPreset, GPartCount + 1);
  GPartSrc[GPartCount] := ASource;
  GPartPreset[GPartCount] := APreset;
  Result := GPartCount;
  Inc(GPartCount);
end;

// Trigger a note on the matching instrument Part and return a voice handle.
function PlayOnPart(ASource: TSAFPartSource; const APreset: string;
  AFreq, AVel: Single): Integer;
var
  PartIdx, Slot: Integer;
  Part: TSAFPart;
begin
  Result := -1;
  if not GInitialized then Exit;

  PartIdx := GetOrCreatePart(ASource, LowerCase(APreset));
  if PartIdx < 0 then Exit;

  Part := GEngine.GetPart(PartIdx);
  if Part = nil then Exit;

  Slot := Part.NoteOnFreq(AFreq, AVel);
  if Slot < 0 then Exit;

  Result := PartIdx * HANDLE_PART_MUL + Slot;
end;

// ============================================================================
// INITIALIZATION
// ============================================================================

function InitAudio(AMaxVoices: Integer): Boolean;
begin
  Result := False;

  if GInitialized then
  begin
    Result := True;
    Exit;
  end;

  GMaxVoices := AMaxVoices;
  if GMaxVoices > SAF_MAX_VOICES then
    GMaxVoices := SAF_MAX_VOICES;

  // Create and initialize audio backend
  GAudioBackend := TSedaiAudioBackend.Create;
  GAudioBackend.SetSampleRate(SAF_DEFAULT_SAMPLE_RATE);
  GAudioBackend.SetDesiredBufferSize(SAF_DEFAULT_BUFFER_SIZE);
  GAudioBackend.SetChannels(2);
  GAudioBackend.SetCallback(@AudioCallback, nil);
  // Pull-driven output: SDL calls AudioCallback (which renders the engine).
  GAudioBackend.SetMode(bmCallback);

  if not GAudioBackend.Initialize then
  begin
    WriteLn('SAF Error: Failed to initialize audio backend');
    FreeAndNil(GAudioBackend);
    Exit;
  end;

  GSampleRate := GAudioBackend.SampleRate;

  // Build the modular engine at the backend's sample rate. Parts are created
  // on demand by the Play* functions.
  GEngine := TSAFEngine.Create(GSampleRate);
  GPartCount := 0;
  SetLength(GPartSrc, 0);
  SetLength(GPartPreset, 0);

  // Start audio (callback is guarded until GEngine exists, which it now does)
  if not GAudioBackend.Start then
  begin
    WriteLn('SAF Error: Failed to start audio');
    FreeAndNil(GEngine);
    GAudioBackend.Shutdown;
    FreeAndNil(GAudioBackend);
    Exit;
  end;

  GInitialized := True;
  Result := True;

  WriteLn('SAF: Audio initialized');
  WriteLn('  Sample rate: ', GSampleRate);
  WriteLn('  Buffer size: ', GAudioBackend.BufferSize);
  WriteLn('  Max voices: ', GMaxVoices);
end;

procedure ShutdownAudio;
begin
  if not GInitialized then Exit;

  // Stop and free audio backend first so the callback stops touching GEngine.
  if Assigned(GAudioBackend) then
  begin
    GAudioBackend.Stop;
    GAudioBackend.Shutdown;
    FreeAndNil(GAudioBackend);
  end;

  // Free the engine (which owns all Parts / voices / mixer).
  FreeAndNil(GEngine);
  GPartCount := 0;
  SetLength(GPartSrc, 0);
  SetLength(GPartPreset, 0);

  GInitialized := False;
  WriteLn('SAF: Audio shutdown');
end;

function GetSampleRate: Cardinal;
begin
  Result := GSampleRate;
end;

function GetActiveVoices: Integer;
var
  I: Integer;
begin
  Result := 0;
  if not Assigned(GEngine) then Exit;
  for I := 0 to GEngine.PartCount - 1 do
    Inc(Result, GEngine.GetPart(I).ActiveVoiceCount);
end;

function GetMaxVoices: Integer;
begin
  Result := GMaxVoices;
end;

// ============================================================================
// MASTER VOLUME
// ============================================================================

procedure SetMasterVolume(AVolume: Single);
begin
  if AVolume < 0 then AVolume := 0;
  if AVolume > 1 then AVolume := 1;
  GMasterVolume := AVolume;
end;

function GetMasterVolume: Single;
begin
  Result := GMasterVolume;
end;

// ============================================================================
// CLASSIC SYNTHESIS
// ============================================================================

function PlayClassicAdv(AFreq: Single; const APreset: string): Integer;
begin
  // Each preset maps to its own classic Part; the voice gets the exact Hz.
  Result := PlayOnPart(psClassic, APreset, AFreq, 0.8);
end;

procedure PlayClassic(AFreq: Single; const APreset: string);
begin
  PlayClassicAdv(AFreq, APreset);
end;

procedure PlaySine(AFreq: Single; AAmplitude: Single);
var
  V: Integer;
begin
  V := PlayClassicAdv(AFreq, 'sine');
  if V >= 0 then
    SetVoiceAmplitude(V, AAmplitude);
end;

procedure PlaySquare(AFreq: Single; AAmplitude: Single);
var
  V: Integer;
begin
  V := PlayClassicAdv(AFreq, 'square');
  if V >= 0 then
    SetVoiceAmplitude(V, AAmplitude);
end;

procedure PlaySaw(AFreq: Single; AAmplitude: Single);
var
  V: Integer;
begin
  V := PlayClassicAdv(AFreq, 'saw');
  if V >= 0 then
    SetVoiceAmplitude(V, AAmplitude);
end;

procedure PlayTriangle(AFreq: Single; AAmplitude: Single);
var
  V: Integer;
begin
  V := PlayClassicAdv(AFreq, 'triangle');
  if V >= 0 then
    SetVoiceAmplitude(V, AAmplitude);
end;

procedure PlayPulse(AFreq: Single; APulseWidth: Single; AAmplitude: Single);
var
  V: Integer;
begin
  V := PlayClassicAdv(AFreq, 'pulse');
  if V >= 0 then
  begin
    SetVoiceAmplitude(V, AAmplitude);
    SetVoicePulseWidth(V, APulseWidth);
  end;
end;

procedure PlayNoise(AAmplitude: Single);
var
  V: Integer;
begin
  V := PlayClassicAdv(1000, 'noise');
  if V >= 0 then
    SetVoiceAmplitude(V, AAmplitude);
end;

procedure PlayLead(AFreq: Single);
begin
  PlayClassic(AFreq, 'lead');
end;

procedure PlayBass(AFreq: Single);
begin
  PlayClassic(AFreq, 'bass');
end;

procedure PlayPad(AFreq: Single);
begin
  PlayClassic(AFreq, 'pad');
end;

// ============================================================================
// FM SYNTHESIS
// ============================================================================

function PlayFMAdv(AFreq: Single; const APreset: string): Integer;
begin
  // FM presets are configured per-Part (ConfigureFMVoice); pitch is note-driven.
  Result := PlayOnPart(psFM, APreset, AFreq, 0.8);
end;

procedure PlayFM(AFreq: Single; const APreset: string);
begin
  PlayFMAdv(AFreq, APreset);
end;

procedure PlayEPiano(AFreq: Single);
begin
  PlayFM(AFreq, 'epiano');
end;

procedure PlayFMBrass(AFreq: Single);
begin
  PlayFM(AFreq, 'brass');
end;

procedure PlayFMBell(AFreq: Single);
begin
  PlayFM(AFreq, 'bell');
end;

procedure PlayFMOrgan(AFreq: Single);
begin
  PlayFM(AFreq, 'organ');
end;

procedure PlayFMLead(AFreq: Single);
begin
  PlayFM(AFreq, 'lead');
end;

procedure PlayFMBass(AFreq: Single);
begin
  PlayFM(AFreq, 'bass');
end;

// ============================================================================
// WAVETABLE SYNTHESIS
// ============================================================================

function PlayWavetableAdv(AFreq: Single; const APreset: string): Integer;
begin
  // Wavetable presets configured per-Part (ConfigureWavetableVoice); exact Hz.
  Result := PlayOnPart(psWavetable, APreset, AFreq, 0.8);
end;

procedure PlayWavetable(AFreq: Single; const APreset: string);
begin
  PlayWavetableAdv(AFreq, APreset);
end;

procedure PlaySerum(AFreq: Single);
begin
  PlayWavetable(AFreq, 'basic');
end;

procedure PlaySuperSaw(AFreq: Single);
begin
  PlayWavetable(AFreq, 'supersaw');
end;

procedure PlayPWM(AFreq: Single);
begin
  PlayWavetable(AFreq, 'pwm');
end;

procedure PlayWasp(AFreq: Single);
begin
  // Wasp-style harsh digital wavetable
  PlayWavetable(AFreq, 'basic');
end;

procedure PlayPPG(AFreq: Single);
begin
  // PPG-style wavetable (use basic for now)
  PlayWavetable(AFreq, 'basic');
end;

procedure PlayScaleWavetable(ABaseFreq: Single; const APreset: string);
const
  MAJOR_SCALE: array[0..7] of Integer = (0, 2, 4, 5, 7, 9, 11, 12);
var
  I: Integer;
begin
  for I := 0 to 7 do
    PlayWavetable(ABaseFreq * Power(2.0, MAJOR_SCALE[I] / 12.0), APreset);
end;

// ============================================================================
// WAVETABLE LOADING API
// ============================================================================

function LoadWavetableFile(const AFilename: string): Boolean;
var
  WT: TWavetable;
begin
  WT := TSedaiWavetableLoader.LoadWavetable(AFilename);
  Result := WT.IsLoaded;
end;

function LoadWavetableDirectory(const APath: string): Integer;
var
  Names: TStringArray;
begin
  Names := TSedaiWavetableLoader.LoadWavetableDirectory(APath);
  Result := Length(Names);
end;

function GetLoadedWavetableCount: Integer;
begin
  Result := TSedaiWavetableLoader.GetCacheCount;
end;

function GetLoadedWavetableName(AIndex: Integer): string;
begin
  Result := TSedaiWavetableLoader.GetCachedName(AIndex);
end;

procedure ClearWavetableCache;
begin
  TSedaiWavetableLoader.ClearCache;
end;

function GetLoadedWavetables: TStringArray;
var
  I, Count: Integer;
begin
  Count := TSedaiWavetableLoader.GetCacheCount;
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := TSedaiWavetableLoader.GetCachedName(I);
end;

function IsWavetableLoaded(const AName: string): Boolean;
begin
  Result := TSedaiWavetableLoader.IsCached(AName);
end;

function GetWavetableCacheInfo: string;
begin
  Result := TSedaiWavetableLoader.GetCacheInfo;
end;

procedure PlayLoadedWavetable(AFreq: Single; const AWavetableName: string);
begin
  // For now, just play a basic wavetable
  // In a full implementation, this would load the wavetable from cache
  PlayWavetable(AFreq, 'basic');
end;

function PlayLoadedWavetableAdv(AFreq: Single; const AWavetableName: string): Integer;
begin
  Result := PlayWavetableAdv(AFreq, 'basic');
end;

procedure ListLoadedWavetables;
var
  I, Count: Integer;
begin
  Count := TSedaiWavetableLoader.GetCacheCount;
  WriteLn('Loaded Wavetables: ', Count);
  for I := 0 to Count - 1 do
    WriteLn('  ', I, ': ', TSedaiWavetableLoader.GetCachedName(I));
end;

procedure PrintWavetableInfo(const AFilename: string);
var
  Info: TWavetableInfo;
begin
  Info := TSedaiWavetableLoader.GetWavetableInfo(AFilename);
  WriteLn('Wavetable Info for: ', AFilename);
  WriteLn('  Name: ', Info.Name);
  WriteLn('  Format: ', TSedaiWavetableLoader.FormatToString(Info.Format));
  WriteLn('  File Size: ', Info.FileSize, ' bytes');
  WriteLn('  Frame Count: ', Info.FrameCount);
  WriteLn('  Sample Rate: ', Info.SampleRate);
  WriteLn('  Valid: ', Info.IsValid);
end;

procedure ScanWavetableDirectory(const APath: string);
var
  Files: TStringArray;
  I: Integer;
  Info: TWavetableInfo;
begin
  if not DirectoryExists(APath) then
  begin
    WriteLn('ERROR: Directory not found: ', APath);
    Exit;
  end;

  Files := TSedaiWavetableLoader.ScanWavetableDirectory(APath);
  WriteLn('Scanning wavetable directory: ', APath);
  WriteLn('Found ', Length(Files), ' wavetable files:');

  for I := 0 to Length(Files) - 1 do
  begin
    Info := TSedaiWavetableLoader.GetWavetableInfo(Files[I]);
    WriteLn('  ', ExtractFileName(Files[I]), ' (',
            TSedaiWavetableLoader.FormatToString(Info.Format),
            ', ', Info.FrameCount, ' frames)');
  end;
end;

function GetWavetableFormats: TStringArray;
begin
  Result := TSedaiWavetableLoader.GetSupportedExtensions;
end;

procedure PlayCustomWavetable(AFreq: Single; const ACustomWavetable: TWavetable);
begin
  // Custom TWavetable data loading is deferred to A4; play the basic table.
  PlayWavetable(AFreq, 'basic');
end;

function PlayCustomWavetableAdv(AFreq: Single; const ACustomWavetable: TWavetable): Integer;
begin
  Result := PlayWavetableAdv(AFreq, 'basic');
end;

// ============================================================================
// MIDI INTEGRATION CALLBACKS
// ============================================================================

procedure RegisterMidiUpdateCallback(ACallback: TMIDIUpdateProc);
begin
  GMIDIUpdateCallback := ACallback;
end;

procedure UnregisterMidiUpdateCallback;
begin
  GMIDIUpdateCallback := nil;
end;

// ============================================================================
// VOICE CONTROL
// ============================================================================

procedure NoteOff(AVoiceIndex: Integer);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V <> nil then
    V.NoteOff;   // releases envelopes (and the FM source) internally
end;

procedure NoteRelease(AVoiceIndex: Integer);
begin
  NoteOff(AVoiceIndex);
end;

procedure SetVoicePan(AVoiceIndex: Integer; APan: Single);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V = nil then Exit;
  if APan < -1 then APan := -1;
  if APan > 1 then APan := 1;
  V.Pan := APan;
end;

procedure SetVoiceFrequency(AVoiceIndex: Integer; AFreq: Single);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V <> nil then
    V.SetExplicitFrequency(AFreq);  // exact Hz on oscillator/wavetable sources
end;

procedure SetVoiceAmplitude(AVoiceIndex: Integer; AAmplitude: Single);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V = nil then Exit;
  if AAmplitude < 0 then AAmplitude := 0;
  if AAmplitude > 1 then AAmplitude := 1;
  V.OutputLevel := AAmplitude;
end;

procedure SetVoiceADSR(AVoiceIndex: Integer; AAttack, ADecay, ASustain, ARelease: Single);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V <> nil then
    V.SetEnvelopeADSR(0, AAttack, ADecay, ASustain, ARelease);  // amp envelope
end;

procedure RetriggerVoice(AVoiceIndex: Integer);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if (V <> nil) and V.IsActive then
    V.Envelopes[0].Trigger;
end;

procedure RetriggerVoiceHard(AVoiceIndex: Integer);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V = nil then Exit;

  // Hard reset of the amp envelope then re-trigger (SID-style for trills).
  V.Envelopes[0].Reset;
  V.Envelopes[0].Trigger;
end;

procedure SetVoicePulseWidth(AVoiceIndex: Integer; APulseWidth: Single);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V = nil then Exit;

  if APulseWidth < 0.0 then APulseWidth := 0.0;
  if APulseWidth > 1.0 then APulseWidth := 1.0;

  V.SetOscillatorPulseWidth(0, APulseWidth);
end;

procedure SetVoiceFilter(AVoiceIndex: Integer; AEnabled: Boolean;
  AFilterType: TFilterType; AFreq, AQ: Single; ASlope: TFilterSlope);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V = nil then Exit;

  V.FilterEnabled := AEnabled;
  V.SetFilterType(AFilterType);
  V.SetFilterCutoff(AFreq);
  V.SetFilterResonance(AQ);
  V.Filter.Slope := ASlope;
end;

procedure SetVoiceFilterEnabled(AVoiceIndex: Integer; AEnabled: Boolean);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V <> nil then
    V.FilterEnabled := AEnabled;
end;

procedure SetVoiceFilterParams(AVoiceIndex: Integer; AFreq, AQ: Single);
var
  V: TSedaiVoice;
begin
  V := HandleVoice(AVoiceIndex);
  if V = nil then Exit;
  V.SetFilterCutoff(AFreq);
  V.SetFilterResonance(AQ);
end;

// ============================================================================
// SYSTEM CONTROL
// ============================================================================

procedure StopAll;
var
  I: Integer;
begin
  if not Assigned(GEngine) then Exit;
  for I := 0 to GEngine.PartCount - 1 do
    GEngine.GetPart(I).AllSoundOff;   // immediate
end;

procedure SmoothStopAll(AFadeTimeMs: Integer);
var
  I: Integer;
begin
  if not Assigned(GEngine) then Exit;
  // Release all notes; voices fade out on their own release stage.
  for I := 0 to GEngine.PartCount - 1 do
    GEngine.GetPart(I).AllNotesOff;
end;

procedure PrintStatus;
begin
  WriteLn('=== Sedai Audio Foundation Status ===');
  WriteLn('Initialized: ', GInitialized);
  WriteLn('Sample Rate: ', GSampleRate);
  WriteLn('Max Voices: ', GMaxVoices);
  WriteLn('Active Voices: ', GetActiveVoices);
  WriteLn('Master Volume: ', GMasterVolume:0:2);
end;

// ============================================================================
// MIDI UTILITIES
// ============================================================================

function MIDINoteToFreq(ANote: Byte): Single;
begin
  // A4 = MIDI note 69 = 440 Hz
  Result := 440.0 * Power(2.0, (ANote - 69) / 12.0);
end;

function FreqToMIDINote(AFreq: Single): Byte;
var
  Note: Integer;
begin
  if AFreq <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  Note := Round(69 + 12 * Log2(AFreq / 440.0));
  if Note < 0 then Note := 0;
  if Note > 127 then Note := 127;
  Result := Note;
end;

function MIDIVelocityToAmplitude(AVelocity: Byte): Single;
begin
  // Logarithmic curve for natural dynamics
  Result := Power(AVelocity / 127.0, 0.5);
end;

function MIDIAmplitudeToVelocity(AAmplitude: Single): Byte;
var
  Vel: Integer;
begin
  if AAmplitude <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  if AAmplitude >= 1 then
  begin
    Result := 127;
    Exit;
  end;
  // Inverse of logarithmic curve
  Vel := Round(127.0 * Power(AAmplitude, 2.0));
  if Vel < 0 then Vel := 0;
  if Vel > 127 then Vel := 127;
  Result := Vel;
end;

function MIDIPanToSedai(APan: Byte): Single;
begin
  // MIDI pan: 0=left, 64=center, 127=right
  // Sedai pan: -1.0=left, 0.0=center, 1.0=right
  Result := (APan - 64) / 64.0;
  if Result < -1.0 then Result := -1.0;
  if Result > 1.0 then Result := 1.0;
end;

function SedaiPanToMIDI(APan: Single): Byte;
var
  Pan: Integer;
begin
  // Sedai pan: -1.0=left, 0.0=center, 1.0=right
  // MIDI pan: 0=left, 64=center, 127=right
  Pan := Round(APan * 64.0 + 64.0);
  if Pan < 0 then Pan := 0;
  if Pan > 127 then Pan := 127;
  Result := Pan;
end;

function MIDINoteToName(ANote: Byte): string;
const
  NOTE_NAMES: array[0..11] of string = ('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B');
var
  NoteName: Integer;
  Octave: Integer;
begin
  NoteName := ANote mod 12;
  Octave := (ANote div 12) - 1;
  Result := NOTE_NAMES[NoteName] + IntToStr(Octave);
end;

function MIDINoteToOctave(ANote: Byte): Integer;
begin
  // MIDI note 0 = C-1, note 12 = C0, note 24 = C1, etc.
  Result := (ANote div 12) - 1;
end;

// ============================================================================
// MIDI VOICE MANAGEMENT
// ============================================================================

procedure InitializeMIDIVoicePool;
var
  I: Integer;
begin
  if GMIDIVoicePoolInitialized then Exit;

  for I := 0 to MIDI_MAX_VOICES - 1 do
  begin
    GMIDIVoicePool[I].IsAllocated := False;
    GMIDIVoicePool[I].IsPlaying := False;
    GMIDIVoicePool[I].Frequency := 440.0;
    GMIDIVoicePool[I].Amplitude := 0.8;
    GMIDIVoicePool[I].Pan := 0.0;
    GMIDIVoicePool[I].WavetableType := 'basic';
    GMIDIVoicePool[I].SynthVoiceIndex := -1;
    GMIDIVoicePool[I].NoteNumber := 69;
    GMIDIVoicePool[I].Velocity := 100;
  end;

  GMIDIVoicePoolInitialized := True;
end;

function MIDIAllocateVoice: Integer;
var
  I: Integer;
begin
  Result := -1;
  InitializeMIDIVoicePool;

  for I := 0 to MIDI_MAX_VOICES - 1 do
  begin
    if not GMIDIVoicePool[I].IsAllocated then
    begin
      GMIDIVoicePool[I].IsAllocated := True;
      GMIDIVoicePool[I].IsPlaying := False;
      GMIDIVoicePool[I].Frequency := 440.0;
      GMIDIVoicePool[I].Amplitude := 0.8;
      GMIDIVoicePool[I].Pan := 0.0;
      GMIDIVoicePool[I].WavetableType := 'basic';
      GMIDIVoicePool[I].SynthVoiceIndex := -1;
      Result := I;
      Exit;
    end;
  end;
end;

function MIDIIsVoiceActive(AVoiceIndex: Integer): Boolean;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := False
  else
    Result := GMIDIVoicePool[AVoiceIndex].IsAllocated and
              GMIDIVoicePool[AVoiceIndex].IsPlaying;
end;

function MIDIGetFreeVoiceCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  InitializeMIDIVoicePool;

  for I := 0 to MIDI_MAX_VOICES - 1 do
  begin
    if not GMIDIVoicePool[I].IsAllocated then
      Inc(Result);
  end;
end;

procedure MIDIVoiceOn(AVoiceIndex: Integer);
var
  ASynthVoice: Integer;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;
  if not GMIDIVoicePool[AVoiceIndex].IsAllocated then Exit;

  with GMIDIVoicePool[AVoiceIndex] do
  begin
    ASynthVoice := PlayWavetableAdv(Frequency, WavetableType);
    if ASynthVoice >= 0 then
    begin
      SynthVoiceIndex := ASynthVoice;
      SetVoiceAmplitude(ASynthVoice, Amplitude);
      SetVoicePan(ASynthVoice, Pan);
      IsPlaying := True;
    end;
  end;
end;

procedure MIDIVoiceOff(AVoiceIndex: Integer);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;

  with GMIDIVoicePool[AVoiceIndex] do
  begin
    if IsPlaying and (SynthVoiceIndex >= 0) then
    begin
      NoteOff(SynthVoiceIndex);
      IsPlaying := False;
    end;
  end;
end;

procedure MIDIReleaseVoice(AVoiceIndex: Integer);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;

  MIDIVoiceOff(AVoiceIndex);
  GMIDIVoicePool[AVoiceIndex].IsAllocated := False;
  GMIDIVoicePool[AVoiceIndex].SynthVoiceIndex := -1;
end;

procedure MIDIReleaseAllVoices;
var
  I: Integer;
begin
  for I := 0 to MIDI_MAX_VOICES - 1 do
    MIDIReleaseVoice(I);
end;

procedure MIDISetVoiceFrequency(AVoiceIndex: Integer; AFrequency: Single);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;
  GMIDIVoicePool[AVoiceIndex].Frequency := AFrequency;
end;

procedure MIDISetVoiceAmplitude(AVoiceIndex: Integer; AAmplitude: Single);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;
  if AAmplitude < 0.0 then AAmplitude := 0.0;
  if AAmplitude > 1.0 then AAmplitude := 1.0;
  GMIDIVoicePool[AVoiceIndex].Amplitude := AAmplitude;
end;

procedure MIDISetVoiceWavetable(AVoiceIndex: Integer; const AWavetableType: string);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;
  GMIDIVoicePool[AVoiceIndex].WavetableType := AWavetableType;
end;

procedure MIDISetVoicePan(AVoiceIndex: Integer; APan: Single);
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then Exit;
  if APan < -1.0 then APan := -1.0;
  if APan > 1.0 then APan := 1.0;

  GMIDIVoicePool[AVoiceIndex].Pan := APan;

  with GMIDIVoicePool[AVoiceIndex] do
  begin
    if IsPlaying and (SynthVoiceIndex >= 0) then
      SetVoicePan(SynthVoiceIndex, APan);
  end;
end;

function MIDIGetVoiceFrequency(AVoiceIndex: Integer): Single;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := 0.0
  else
    Result := GMIDIVoicePool[AVoiceIndex].Frequency;
end;

function MIDIGetVoiceAmplitude(AVoiceIndex: Integer): Single;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := 0.0
  else
    Result := GMIDIVoicePool[AVoiceIndex].Amplitude;
end;

function MIDIGetVoiceWavetable(AVoiceIndex: Integer): string;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := ''
  else
    Result := GMIDIVoicePool[AVoiceIndex].WavetableType;
end;

function MIDIGetVoicePan(AVoiceIndex: Integer): Single;
begin
  if (AVoiceIndex < 0) or (AVoiceIndex >= MIDI_MAX_VOICES) then
    Result := 0.0
  else
    Result := GMIDIVoicePool[AVoiceIndex].Pan;
end;

function MIDIPlayNote(ANote, AVelocity: Byte; const APreset: string): Integer;
begin
  Result := MIDIAllocateVoice;
  if Result < 0 then Exit;

  with GMIDIVoicePool[Result] do
  begin
    NoteNumber := ANote;
    Velocity := AVelocity;
    Frequency := MIDINoteToFreq(ANote);
    Amplitude := MIDIVelocityToAmplitude(AVelocity);
    WavetableType := APreset;
  end;

  MIDIVoiceOn(Result);
end;

function MIDIPlayNoteWithFreq(AFrequency: Single; AVelocity: Byte; const APreset: string): Integer;
begin
  Result := MIDIAllocateVoice;
  if Result < 0 then Exit;

  with GMIDIVoicePool[Result] do
  begin
    NoteNumber := FreqToMIDINote(AFrequency);
    Velocity := AVelocity;
    Frequency := AFrequency;
    Amplitude := MIDIVelocityToAmplitude(AVelocity);
    WavetableType := APreset;
  end;

  MIDIVoiceOn(Result);
end;

// ============================================================================
// CHORD AND SCALE HELPERS
// ============================================================================

procedure PlayChordWavetable(ANote1, ANote2, ANote3: Single; const APreset: string);
begin
  PlayWavetable(ANote1, APreset);
  PlayWavetable(ANote2, APreset);
  PlayWavetable(ANote3, APreset);
end;

procedure PlayChordClassic(ANote1, ANote2, ANote3: Single; const APreset: string);
begin
  PlayClassic(ANote1, APreset);
  PlayClassic(ANote2, APreset);
  PlayClassic(ANote3, APreset);
end;

procedure PlayChordFM(ANote1, ANote2, ANote3: Single; const APreset: string);
begin
  PlayFM(ANote1, APreset);
  PlayFM(ANote2, APreset);
  PlayFM(ANote3, APreset);
end;

procedure PlayScaleClassic(ABaseFreq: Single; const APreset: string);
const
  MAJOR_SCALE: array[0..7] of Integer = (0, 2, 4, 5, 7, 9, 11, 12);
var
  I: Integer;
begin
  for I := 0 to 7 do
    PlayClassic(ABaseFreq * Power(2.0, MAJOR_SCALE[I] / 12.0), APreset);
end;

procedure PlayScaleFM(ABaseFreq: Single; const APreset: string);
const
  MAJOR_SCALE: array[0..7] of Integer = (0, 2, 4, 5, 7, 9, 11, 12);
var
  I: Integer;
begin
  for I := 0 to 7 do
    PlayFM(ABaseFreq * Power(2.0, MAJOR_SCALE[I] / 12.0), APreset);
end;

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

procedure Beep(AFreq: Single; ADurationMs: Integer; AAmplitude: Single);
var
  VoiceIdx: Integer;
begin
  VoiceIdx := PlayClassicAdv(AFreq, 'sine');
  if VoiceIdx >= 0 then
  begin
    SetVoiceAmplitude(VoiceIdx, AAmplitude);
    // Note: In real implementation, a timer would release the note after ADurationMs
    // For now, user must call StopAll or wait for natural decay
  end;
end;

procedure PlayNote(AChannel: Integer; AFreq: Single; ADurationMs: Integer;
  AAmplitude: Single; const APreset: string);
var
  VoiceIdx: Integer;
begin
  // AChannel is a hint for voice allocation (not strictly enforced)
  VoiceIdx := PlayClassicAdv(AFreq, APreset);
  if VoiceIdx >= 0 then
    SetVoiceAmplitude(VoiceIdx, AAmplitude);
end;

procedure PlayOnVoice(AVoiceIndex: Integer; AFreq: Single; const APreset: string);
begin
  // The Part model pool-allocates voices, so targeting a fixed voice slot no
  // longer applies; play the note on the matching classic instrument Part.
  PlayClassic(AFreq, APreset);
end;

procedure ReleaseVoice(AVoiceIndex: Integer);
begin
  NoteOff(AVoiceIndex);
end;

function PlayWavetableMIDI(AFrequency: Single; const AWavetableType: string;
  AVelocity: Byte; APan: Byte): Integer;
var
  VoiceIdx: Integer;
  Amp: Single;
begin
  Result := -1;

  // Allocate a MIDI voice
  VoiceIdx := MIDIAllocateVoice;
  if VoiceIdx < 0 then Exit;

  // Convert MIDI values
  Amp := MIDIVelocityToAmplitude(AVelocity);

  // Configure the MIDI voice
  with GMIDIVoicePool[VoiceIdx] do
  begin
    Frequency := AFrequency;
    Amplitude := Amp;
    Pan := MIDIPanToSedai(APan);
    WavetableType := AWavetableType;
    NoteNumber := FreqToMIDINote(AFrequency);
    Velocity := AVelocity;
  end;

  // Start playback
  MIDIVoiceOn(VoiceIdx);
  Result := VoiceIdx;
end;

// ============================================================================
// FINALIZATION
// ============================================================================

initialization
  // Engine is created lazily by InitAudio; nothing to pre-initialize here.

finalization
  if GInitialized then
    ShutdownAudio;

end.
