{*
 * Sedai Audio Foundation - Voice
 *
 * TSedaiVoice represents a single synthesizer voice with complete
 * generator-modulator-processor chain. Supports multiple oscillators,
 * envelopes, and filter routing.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiVoice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode,
  SedaiOscillator, SedaiEnvelope, SedaiLFO, SedaiFilter,
  SedaiFMOperator, SedaiWavetableGenerator, SedaiAdditiveGenerator,
  SedaiSamplePlayer, SedaiKarplusGenerator, SedaiModulationMatrix;

const
  MAX_OSCILLATORS = 3;
  MAX_ENVELOPES = 3;
  DEFAULT_LFOS = 2;   // built-in LFOs (mstLFO1/mstLFO2); the pool grows via AddLFO

type
  // The signal source a voice generates from. A voice is "universal": it can be
  // a classic oscillator stack, an FM synth, or a wavetable generator. The rest
  // of the chain (envelopes, filter, amp, pan) is shared across all source types.
  TVoiceSourceType = (vstOscillators, vstFM, vstWavetable, vstAdditive, vstSample,
    vstKarplus);

  // How the three oscillators combine in the vstOscillators source.
  TVoiceOscMode = (
    vomMix,      // sum of enabled oscillators (classic subtractive)
    vomRingMod,  // osc1 x osc2 (ring modulation), plus osc3 if enabled
    vomSync      // osc2/osc3 hard-synced to osc1 (sync lead)
  );

  { TSedaiVoice }
  // Single synthesizer voice with generator/modulator/processor chain
  TSedaiVoice = class(TSedaiSignalNode)
  private
    // Voice state
    FState: TVoiceState;
    FSourceType: TVoiceSourceType;  // which generator drives this voice
    FFMSynth: TSedaiFMSynth;        // created on demand when SourceType = vstFM
    FWTGenerator: TSedaiWavetableGenerator;  // created on demand for vstWavetable
    FAdditive: TSedaiAdditiveGenerator;      // created on demand for vstAdditive
    FSamplePlayer: TSedaiSamplePlayer;       // created on demand for vstSample
    FKarplus: TSedaiKarplusGenerator;        // created on demand for vstKarplus
    FNote: Byte;                  // MIDI note number
    FVelocity: Single;            // Note velocity (0.0 - 1.0)
    FGate: Boolean;               // Note gate (on/off)
    FStealPriority: Integer;      // Priority for voice stealing (lower = steal first)
    FAge: Cardinal;               // Voice age in samples (for oldest-voice stealing)

    // Pitch
    FBaseFrequency: Single;       // Base frequency from MIDI note
    FPitchBend: Single;           // Pitch bend amount (-1 to +1)
    FPitchBendRange: Integer;     // Pitch bend range in semitones
    FGlideTime: Single;           // Portamento time in seconds
    FGlideTarget: Single;         // Target frequency for glide
    FCurrentFrequency: Single;    // Current frequency (after glide)
    FGlideCoeff: Single;          // Glide coefficient

    // Generators
    FOscillators: array[0..MAX_OSCILLATORS-1] of TSedaiOscillator;
    FOscillatorLevels: array[0..MAX_OSCILLATORS-1] of Single;
    FOscillatorEnabled: array[0..MAX_OSCILLATORS-1] of Boolean;
    FOscillatorDetune: array[0..MAX_OSCILLATORS-1] of Single;  // In cents
    FOscMode: TVoiceOscMode;       // how the oscillators combine (mix/ring/sync)

    // Modulators
    FEnvelopes: array[0..MAX_ENVELOPES-1] of TSedaiEnvelope;
    // LFO pool: dynamic, starts with DEFAULT_LFOS and grows via AddLFO so a voice
    // can host an unlimited number of LFOs (vibrato + tremolo + auto-wah + ...).
    FLFOs: array of TSedaiLFO;
    FLFOValue: array of Single;  // last LFO outputs (for the mod matrix)

    // Modulation routing (env/LFO/velocity/keytrack -> pitch/cutoff/amp/...).
    // Layered ON TOP of the hardcoded core modulation; empty by default so the
    // voice sounds identical until routings are added.
    FModMatrix: TSedaiModulationMatrix;

    // Processor
    FFilter: TSedaiFilter;
    FFilterEnabled: Boolean;
    FFilterBaseCutoff: Single;      // Unmodulated cutoff; env/keytrack derive from this
    FFilterEnvelopeAmount: Single;  // How much envelope affects filter cutoff
    FFilterKeyTracking: Single;     // How much note pitch affects cutoff (0-1)

    // Output
    FOutputLevel: Single;         // Voice output level
    FPan: Single;                 // Pan position (-1 = left, +1 = right)

    procedure SetNote(AValue: Byte);
    procedure SetVelocity(AValue: Single);
    procedure SetPitchBend(AValue: Single);
    procedure SetGlideTime(AValue: Single);
    procedure SetOscMode(AValue: TVoiceOscMode);
    procedure UpdateOscRouting;    // wires hard-sync sources for vomSync

    procedure UpdateFrequency;
    procedure ProcessGlide;

    function GetOscillator(AIndex: Integer): TSedaiOscillator;
    function GetEnvelope(AIndex: Integer): TSedaiEnvelope;
    function GetLFO(AIndex: Integer): TSedaiLFO;

  public
    constructor Create; override;
    destructor Destroy; override;

    // Reset voice to initial state
    procedure Reset; override;

    // ========================================================================
    // NOTE CONTROL
    // ========================================================================

    // Trigger note on
    procedure NoteOn(ANote: Byte; AVelocity: Single);

    // Trigger note off
    procedure NoteOff;

    // Force release with custom time
    procedure ForceRelease(AReleaseTime: Single);

    // Kill voice immediately
    procedure Kill;

    // ========================================================================
    // PROCESSING
    // ========================================================================

    // Process a block of audio
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Process single sample (for sample-accurate modulation)
    function ProcessSample: Single;

    // ========================================================================
    // STATE QUERIES
    // ========================================================================

    // Check if voice is active (not idle)
    function IsActive: Boolean;

    // Check if voice is releasing
    function IsReleasing: Boolean;

    // Check if voice can be stolen
    function CanSteal: Boolean;

    // Get current output level (for quietest-voice stealing)
    function GetCurrentLevel: Single;

    // ========================================================================
    // OSCILLATOR CONFIGURATION
    // ========================================================================

    // Set oscillator waveform
    procedure SetOscillatorWaveform(AIndex: Integer; AWaveform: TWaveformType);

    // Set oscillator level
    procedure SetOscillatorLevel(AIndex: Integer; ALevel: Single);

    // Set oscillator detune
    procedure SetOscillatorDetune(AIndex: Integer; ACents: Single);

    // Enable/disable oscillator
    procedure SetOscillatorEnabled(AIndex: Integer; AEnabled: Boolean);

    // Set oscillator pulse width
    procedure SetOscillatorPulseWidth(AIndex: Integer; AWidth: Single);

    // How the 3 oscillators combine: mix (sum) / ring-mod / sync.
    property OscMode: TVoiceOscMode read FOscMode write SetOscMode;

    // ========================================================================
    // ENVELOPE CONFIGURATION
    // ========================================================================

    // Set ADSR for envelope
    procedure SetEnvelopeADSR(AIndex: Integer; AAttack, ADecay, ASustain, ARelease: Single);

    // Set envelope curve
    procedure SetEnvelopeCurve(AIndex: Integer; ACurve: TEnvelopeCurve);

    // ========================================================================
    // FILTER CONFIGURATION
    // ========================================================================

    // Set filter type
    procedure SetFilterType(AType: TFilterType);

    // Set filter cutoff
    procedure SetFilterCutoff(ACutoff: Single);

    // Set filter resonance
    procedure SetFilterResonance(AResonance: Single);

    // Set filter envelope amount
    procedure SetFilterEnvelopeAmount(AAmount: Single);

    // ========================================================================
    // SOURCE TYPE (universal voice: oscillators / FM / wavetable)
    // ========================================================================

    // Set an explicit base frequency (Hz), bypassing MIDI-note quantization.
    // Used for arbitrary / microtonal pitches; jumps immediately (no glide).
    // Affects oscillator and wavetable sources (the FM source is note-driven).
    procedure SetExplicitFrequency(AFreq: Single);

    // Select which generator drives the voice.
    procedure SetSourceType(AType: TVoiceSourceType);
    function GetSourceType: TVoiceSourceType;
    // Access the FM synth / wavetable generator (created on first use) so the
    // caller can configure presets. Returns nil for an out-of-range sample rate.
    function GetFMSynth: TSedaiFMSynth;
    function GetWavetableGenerator: TSedaiWavetableGenerator;
    function GetAdditiveGenerator: TSedaiAdditiveGenerator;
    function GetSamplePlayer: TSedaiSamplePlayer;
    function GetKarplusGenerator: TSedaiKarplusGenerator;

    // ========================================================================
    // MODULATION ROUTING (env / LFO / velocity / keytrack -> pitch / cutoff / amp)
    // ========================================================================

    // Add a modulation routing on top of the core modulation. Destination units:
    //   mdtOscAllPitch  -> semitones, applied to oscillator & wavetable pitch
    //   mdtFilterCutoff -> octaves, applied to the (already enveloped) cutoff
    //   mdtAmplitude    -> linear gain offset (output *= 1 + mod)
    // Sources: LFO1/2 and pitch-bend are bipolar (-1..1); envelopes, velocity
    // and keytrack are 0..1. Returns the slot index (-1 if the matrix is full).
    function AddModulation(ASource: TModSourceType; ADest: TModDestType;
      AAmount: Single; ABipolar: Boolean = True): Integer;
    procedure ClearModulation;

    // Append a new LFO to the voice's pool and return its index. The new LFO is
    // free-running at ARate Hz with the given waveform. No artificial ceiling.
    function AddLFO(ARate: Single; AWaveform: TWaveformType): Integer;
    // Trim the pool back to the built-in DEFAULT_LFOS (frees added LFOs).
    procedure ResetExtraLFOs;
    function LFOCount: Integer;
    // Route an LFO from the pool (index from AddLFO, or 0/1 for the built-ins) to
    // a destination. Same destination units as AddModulation. Returns slot index.
    function AddModulationLFO(ALFOIndex: Integer; ADest: TModDestType;
      AAmount: Single; ABipolar: Boolean = True): Integer;

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    // Voice state
    property State: TVoiceState read FState;
    property Note: Byte read FNote write SetNote;
    property Velocity: Single read FVelocity write SetVelocity;
    property Gate: Boolean read FGate;
    property Age: Cardinal read FAge;
    property StealPriority: Integer read FStealPriority write FStealPriority;

    // Pitch
    property PitchBend: Single read FPitchBend write SetPitchBend;
    property PitchBendRange: Integer read FPitchBendRange write FPitchBendRange;
    property GlideTime: Single read FGlideTime write SetGlideTime;
    property CurrentFrequency: Single read FCurrentFrequency;

    // Components
    property Oscillators[AIndex: Integer]: TSedaiOscillator read GetOscillator;
    property Envelopes[AIndex: Integer]: TSedaiEnvelope read GetEnvelope;
    property LFOs[AIndex: Integer]: TSedaiLFO read GetLFO;
    property Filter: TSedaiFilter read FFilter;
    property ModMatrix: TSedaiModulationMatrix read FModMatrix;

    // Filter
    property FilterEnabled: Boolean read FFilterEnabled write FFilterEnabled;
    property FilterEnvelopeAmount: Single read FFilterEnvelopeAmount write FFilterEnvelopeAmount;
    property FilterKeyTracking: Single read FFilterKeyTracking write FFilterKeyTracking;

    // Output
    property OutputLevel: Single read FOutputLevel write FOutputLevel;
    property Pan: Single read FPan write FPan;
  end;

implementation

{ TSedaiVoice }

constructor TSedaiVoice.Create;
var
  I: Integer;
begin
  inherited Create;

  FState := vsIdle;
  FSourceType := vstOscillators;
  FFMSynth := nil;
  FWTGenerator := nil;
  FAdditive := nil;
  FSamplePlayer := nil;
  FKarplus := nil;
  FNote := 60;  // Middle C
  FVelocity := 1.0;
  FGate := False;
  FStealPriority := 0;
  FAge := 0;

  FBaseFrequency := 261.63;  // C4
  FPitchBend := 0.0;
  FPitchBendRange := 2;
  FGlideTime := 0.0;
  FGlideTarget := FBaseFrequency;
  FCurrentFrequency := FBaseFrequency;
  FGlideCoeff := 1.0;

  // Create oscillators
  for I := 0 to MAX_OSCILLATORS - 1 do
  begin
    FOscillators[I] := TSedaiOscillator.Create;
    FOscillators[I].Waveform := wtSawtooth;
    FOscillatorLevels[I] := 1.0;
    FOscillatorEnabled[I] := (I = 0);  // Only first oscillator enabled by default
    FOscillatorDetune[I] := 0.0;
  end;
  FOscMode := vomMix;  // classic sum (no sync/ring) -> presets unchanged

  // Create envelopes
  for I := 0 to MAX_ENVELOPES - 1 do
  begin
    FEnvelopes[I] := TSedaiEnvelope.Create;
    // Envelope 0 = Amplitude, Envelope 1 = Filter, Envelope 2 = Pitch/Mod
    if I = 0 then
    begin
      FEnvelopes[I].AttackTime := 0.01;
      FEnvelopes[I].DecayTime := 0.1;
      FEnvelopes[I].SustainLevel := 0.7;
      FEnvelopes[I].ReleaseTime := 0.3;
    end
    else
    begin
      FEnvelopes[I].AttackTime := 0.05;
      FEnvelopes[I].DecayTime := 0.2;
      FEnvelopes[I].SustainLevel := 0.0;
      FEnvelopes[I].ReleaseTime := 0.5;
    end;
  end;

  // Create the built-in LFO pool (grows later via AddLFO)
  SetLength(FLFOs, DEFAULT_LFOS);
  SetLength(FLFOValue, DEFAULT_LFOS);
  for I := 0 to High(FLFOs) do
  begin
    FLFOs[I] := TSedaiLFO.Create;
    FLFOs[I].Rate := 5.0;
    FLFOs[I].Waveform := wtSine;
    FLFOValue[I] := 0.0;
  end;

  // Create filter
  FFilter := TSedaiFilter.Create;
  FFilter.FilterType := ftLowPass;
  FFilter.Cutoff := 5000.0;
  FFilter.Resonance := 0.2;
  FFilterBaseCutoff := 5000.0;
  FFilterEnabled := True;
  FFilterEnvelopeAmount := 0.5;
  FFilterKeyTracking := 0.5;

  // Modulation matrix (empty by default -> no extra modulation)
  FModMatrix := TSedaiModulationMatrix.Create;

  FOutputLevel := 1.0;
  FPan := 0.0;
end;

destructor TSedaiVoice.Destroy;
var
  I: Integer;
begin
  for I := 0 to MAX_OSCILLATORS - 1 do
    FOscillators[I].Free;

  for I := 0 to MAX_ENVELOPES - 1 do
    FEnvelopes[I].Free;

  for I := 0 to High(FLFOs) do
    FLFOs[I].Free;
  SetLength(FLFOs, 0);
  SetLength(FLFOValue, 0);

  FFilter.Free;
  FFMSynth.Free;        // nil-safe
  FWTGenerator.Free;    // nil-safe
  FAdditive.Free;       // nil-safe
  FSamplePlayer.Free;   // nil-safe (does not own the sample buffer)
  FKarplus.Free;        // nil-safe
  FModMatrix.Free;

  inherited Destroy;
end;

procedure TSedaiVoice.Reset;
var
  I: Integer;
begin
  inherited Reset;

  FState := vsIdle;
  FGate := False;
  FAge := 0;

  for I := 0 to MAX_OSCILLATORS - 1 do
    FOscillators[I].Reset;

  for I := 0 to MAX_ENVELOPES - 1 do
    FEnvelopes[I].Reset;

  for I := 0 to High(FLFOs) do
    FLFOs[I].Reset;

  FFilter.Reset;
  if Assigned(FFMSynth) then FFMSynth.Reset;
  if Assigned(FWTGenerator) then FWTGenerator.Reset;
  if Assigned(FAdditive) then FAdditive.Kill;   // resets envelope + phases
  if Assigned(FSamplePlayer) then FSamplePlayer.Stop;
  if Assigned(FKarplus) then FKarplus.Reset;
  FModMatrix.Reset;          // clears source/dest state, keeps the routings
  for I := 0 to High(FLFOValue) do
    FLFOValue[I] := 0.0;
end;

procedure TSedaiVoice.SetNote(AValue: Byte);
begin
  if AValue > 127 then
    AValue := 127;
  FNote := AValue;
  UpdateFrequency;
end;

procedure TSedaiVoice.SetVelocity(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 1.0 then
    AValue := 1.0;
  FVelocity := AValue;
end;

procedure TSedaiVoice.SetPitchBend(AValue: Single);
begin
  if AValue < -1.0 then
    AValue := -1.0
  else if AValue > 1.0 then
    AValue := 1.0;
  FPitchBend := AValue;
  UpdateFrequency;
end;

procedure TSedaiVoice.SetGlideTime(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 10.0 then
    AValue := 10.0;

  FGlideTime := AValue;

  // Calculate glide coefficient
  if (FGlideTime > 0) and (FSampleRate > 0) then
    FGlideCoeff := Exp(-1.0 / (FGlideTime * FSampleRate))
  else
    FGlideCoeff := 0.0;  // Instant
end;

procedure TSedaiVoice.SetOscMode(AValue: TVoiceOscMode);
begin
  FOscMode := AValue;
  UpdateOscRouting;
end;

procedure TSedaiVoice.UpdateOscRouting;
begin
  // Sync mode: osc1 and osc2 are slaves hard-synced to osc0 (the master).
  if FOscMode = vomSync then
  begin
    FOscillators[1].SyncTo(FOscillators[0]);
    FOscillators[2].SyncTo(FOscillators[0]);
  end
  else
  begin
    FOscillators[1].SyncSource := nil;
    FOscillators[1].HardSync := False;
    FOscillators[2].SyncSource := nil;
    FOscillators[2].HardSync := False;
  end;
end;

procedure TSedaiVoice.UpdateFrequency;
var
  PitchBendSemitones: Single;
begin
  // Convert MIDI note to frequency
  FBaseFrequency := MIDINoteToFrequency(FNote);

  // Apply pitch bend
  PitchBendSemitones := FPitchBend * FPitchBendRange;
  FGlideTarget := FBaseFrequency * Power(2.0, PitchBendSemitones / 12.0);

  // If no glide, jump immediately
  if FGlideTime <= 0 then
    FCurrentFrequency := FGlideTarget;
end;

procedure TSedaiVoice.ProcessGlide;
begin
  if FGlideTime > 0 then
  begin
    // Exponential glide
    FCurrentFrequency := FCurrentFrequency * FGlideCoeff +
                         FGlideTarget * (1.0 - FGlideCoeff);
  end
  else
    FCurrentFrequency := FGlideTarget;
end;

function TSedaiVoice.GetOscillator(AIndex: Integer): TSedaiOscillator;
begin
  if (AIndex >= 0) and (AIndex < MAX_OSCILLATORS) then
    Result := FOscillators[AIndex]
  else
    Result := nil;
end;

function TSedaiVoice.GetEnvelope(AIndex: Integer): TSedaiEnvelope;
begin
  if (AIndex >= 0) and (AIndex < MAX_ENVELOPES) then
    Result := FEnvelopes[AIndex]
  else
    Result := nil;
end;

function TSedaiVoice.GetLFO(AIndex: Integer): TSedaiLFO;
begin
  if (AIndex >= 0) and (AIndex <= High(FLFOs)) then
    Result := FLFOs[AIndex]
  else
    Result := nil;
end;

procedure TSedaiVoice.NoteOn(ANote: Byte; AVelocity: Single);
var
  I: Integer;
begin
  FNote := ANote;
  FVelocity := AVelocity;
  FGate := True;
  FAge := 0;

  // Set initial frequency (before glide)
  if FState = vsIdle then
    FCurrentFrequency := MIDINoteToFrequency(ANote);

  UpdateFrequency;

  // Update oscillator frequencies
  for I := 0 to MAX_OSCILLATORS - 1 do
  begin
    FOscillators[I].Frequency := FCurrentFrequency *
      Power(2.0, FOscillatorDetune[I] / 1200.0);
  end;

  // Trigger envelopes
  for I := 0 to MAX_ENVELOPES - 1 do
    FEnvelopes[I].Trigger;

  // Trigger LFOs (for key sync)
  for I := 0 to High(FLFOs) do
    FLFOs[I].Trigger;

  // FM source has its own per-operator envelopes — trigger them too.
  if (FSourceType = vstFM) and Assigned(FFMSynth) then
    FFMSynth.NoteOn(ANote, AVelocity);

  // Additive source has its own amplitude envelope.
  if (FSourceType = vstAdditive) and Assigned(FAdditive) then
    FAdditive.NoteOn(ANote, AVelocity);

  // Sample source: pitch from the current frequency, then (re)start playback.
  if (FSourceType = vstSample) and Assigned(FSamplePlayer) then
  begin
    FSamplePlayer.Frequency := FCurrentFrequency;
    FSamplePlayer.Play;
  end;

  // Karplus-Strong: pluck the string at the current frequency.
  if (FSourceType = vstKarplus) and Assigned(FKarplus) then
    FKarplus.NoteOn(FCurrentFrequency, AVelocity);

  FState := vsAttack;
end;

procedure TSedaiVoice.NoteOff;
var
  I: Integer;
begin
  FGate := False;

  // Release envelopes
  for I := 0 to MAX_ENVELOPES - 1 do
    FEnvelopes[I].Release;

  if (FSourceType = vstFM) and Assigned(FFMSynth) then
    FFMSynth.NoteOff;

  if (FSourceType = vstAdditive) and Assigned(FAdditive) then
    FAdditive.NoteOff;

  if (FSourceType = vstKarplus) and Assigned(FKarplus) then
    FKarplus.NoteOff;

  if FState <> vsIdle then
    FState := vsReleasing;
end;

procedure TSedaiVoice.ForceRelease(AReleaseTime: Single);
var
  I: Integer;
begin
  FGate := False;

  for I := 0 to MAX_ENVELOPES - 1 do
    FEnvelopes[I].ForceRelease(AReleaseTime);

  if FState <> vsIdle then
    FState := vsReleasing;
end;

procedure TSedaiVoice.Kill;
begin
  Reset;
end;

procedure TSedaiVoice.SetExplicitFrequency(AFreq: Single);
var
  I: Integer;
begin
  if AFreq <= 0.0 then Exit;

  // Pin all frequency-tracking state to the explicit value so ProcessGlide
  // holds it (glide coeff is irrelevant when target = current).
  FBaseFrequency := AFreq;
  FGlideTarget := AFreq;
  FCurrentFrequency := AFreq;

  for I := 0 to MAX_OSCILLATORS - 1 do
    FOscillators[I].Frequency := AFreq *
      Power(2.0, FOscillatorDetune[I] / 1200.0);

  if Assigned(FWTGenerator) then
    FWTGenerator.Frequency := AFreq;
end;

procedure TSedaiVoice.SetSourceType(AType: TVoiceSourceType);
begin
  FSourceType := AType;
end;

function TSedaiVoice.GetSourceType: TVoiceSourceType;
begin
  Result := FSourceType;
end;

function TSedaiVoice.GetFMSynth: TSedaiFMSynth;
begin
  if not Assigned(FFMSynth) then
  begin
    FFMSynth := TSedaiFMSynth.Create;
    if FSampleRate > 0 then
      FFMSynth.SetSampleRate(FSampleRate);
  end;
  Result := FFMSynth;
end;

function TSedaiVoice.GetWavetableGenerator: TSedaiWavetableGenerator;
begin
  if not Assigned(FWTGenerator) then
  begin
    FWTGenerator := TSedaiWavetableGenerator.Create;
    if FSampleRate > 0 then
      FWTGenerator.SetSampleRate(FSampleRate);
  end;
  Result := FWTGenerator;
end;

function TSedaiVoice.GetAdditiveGenerator: TSedaiAdditiveGenerator;
begin
  if not Assigned(FAdditive) then
  begin
    FAdditive := TSedaiAdditiveGenerator.Create;
    if FSampleRate > 0 then
      FAdditive.SetSampleRate(FSampleRate);
  end;
  Result := FAdditive;
end;

function TSedaiVoice.GetSamplePlayer: TSedaiSamplePlayer;
begin
  if not Assigned(FSamplePlayer) then
  begin
    FSamplePlayer := TSedaiSamplePlayer.Create;
    if FSampleRate > 0 then
      FSamplePlayer.SetSampleRate(FSampleRate);
  end;
  Result := FSamplePlayer;
end;

function TSedaiVoice.GetKarplusGenerator: TSedaiKarplusGenerator;
begin
  if not Assigned(FKarplus) then
  begin
    FKarplus := TSedaiKarplusGenerator.Create;
    if FSampleRate > 0 then
      FKarplus.SetSampleRate(FSampleRate);
  end;
  Result := FKarplus;
end;

function TSedaiVoice.AddModulation(ASource: TModSourceType; ADest: TModDestType;
  AAmount: Single; ABipolar: Boolean): Integer;
begin
  Result := FModMatrix.AddSlot(ASource, ADest, AAmount, ABipolar);
end;

procedure TSedaiVoice.ClearModulation;
begin
  FModMatrix.ClearAllSlots;
end;

function TSedaiVoice.AddLFO(ARate: Single; AWaveform: TWaveformType): Integer;
var
  LFO: TSedaiLFO;
begin
  LFO := TSedaiLFO.Create;
  // Sample rate first so SetRate computes the phase increment correctly.
  if FSampleRate > 0 then
    LFO.SetSampleRate(FSampleRate);
  LFO.Rate := ARate;
  LFO.Waveform := AWaveform;

  SetLength(FLFOs, Length(FLFOs) + 1);
  SetLength(FLFOValue, Length(FLFOValue) + 1);
  FLFOs[High(FLFOs)] := LFO;
  FLFOValue[High(FLFOValue)] := 0.0;
  Result := High(FLFOs);
end;

procedure TSedaiVoice.ResetExtraLFOs;
var
  I: Integer;
begin
  for I := DEFAULT_LFOS to High(FLFOs) do
    FLFOs[I].Free;
  SetLength(FLFOs, DEFAULT_LFOS);
  SetLength(FLFOValue, DEFAULT_LFOS);
end;

function TSedaiVoice.LFOCount: Integer;
begin
  Result := Length(FLFOs);
end;

function TSedaiVoice.AddModulationLFO(ALFOIndex: Integer; ADest: TModDestType;
  AAmount: Single; ABipolar: Boolean): Integer;
begin
  Result := FModMatrix.AddSlot(mstLFO, ADest, AAmount, ABipolar, ALFOIndex);
end;

function TSedaiVoice.ProcessSample: Single;
var
  I: Integer;
  SourceOut, FilteredOutput: Single;
  AmpEnv, FilterEnv, ModEnv: Single;
  FilterCutoff: Single;
  Finished, UseAmpEnv: Boolean;
  ModPitch, ModCutoff, ModAmp, PitchFactor, AmpFactor: Single;
  OscA, OscB: Single;
begin
  if FState = vsIdle then
  begin
    Result := 0.0;
    Exit;
  end;

  // Process glide
  ProcessGlide;

  // Process envelopes (always advance) — env0=amp, env1=filter, env2=mod
  AmpEnv := FEnvelopes[0].Process;
  FilterEnv := FEnvelopes[1].Process;
  ModEnv := FEnvelopes[2].Process;

  // Process LFOs (keep their outputs for the modulation matrix)
  for I := 0 to High(FLFOs) do
    FLFOValue[I] := FLFOs[I].Process;

  // ---- Modulation matrix (extra layer on top of the hardcoded core) ----
  // Empty by default -> all modulations are 0 -> sound is unchanged.
  ModPitch := 0.0;   // semitones
  ModCutoff := 0.0;  // octaves
  ModAmp := 0.0;     // linear gain offset
  if FModMatrix.SlotCount > 0 then
  begin
    FModMatrix.SetSourceValue(mstEnvelope1, AmpEnv);
    FModMatrix.SetSourceValue(mstEnvelope2, FilterEnv);
    FModMatrix.SetSourceValue(mstEnvelope3, ModEnv);
    // Publish every LFO to the indexed pool (mstLFO sources)...
    for I := 0 to High(FLFOValue) do
      FModMatrix.SetLFOValue(I, FLFOValue[I]);
    // ...and keep the fixed mstLFO1/mstLFO2 aliases for the first two.
    FModMatrix.SetSourceValue(mstLFO1, FLFOValue[0]);
    FModMatrix.SetSourceValue(mstLFO2, FLFOValue[1]);
    FModMatrix.SetSourceValue(mstVelocity, FVelocity);
    FModMatrix.SetSourceValue(mstPitchBend, FPitchBend);
    FModMatrix.SetSourceValue(mstKeytrack, (FNote - 60) / 63.0);
    FModMatrix.Process;
    ModPitch := FModMatrix.GetModulation(mdtOscAllPitch);
    ModCutoff := FModMatrix.GetModulation(mdtFilterCutoff);
    ModAmp := FModMatrix.GetModulation(mdtAmplitude);
  end;
  PitchFactor := Power(2.0, ModPitch / 12.0);

  // ---- Generate from the selected source ----
  UseAmpEnv := True;
  case FSourceType of
    vstFM:
      begin
        // FM synth has its own per-operator envelopes; the amp envelope is not
        // applied, and the voice ends when the FM synth reports finished.
        if Assigned(FFMSynth) then
          SourceOut := FFMSynth.GenerateSample
        else
          SourceOut := 0.0;
        Finished := (not Assigned(FFMSynth)) or FFMSynth.IsFinished;
        UseAmpEnv := False;
      end;
    vstWavetable:
      begin
        if Assigned(FWTGenerator) then
        begin
          FWTGenerator.Frequency := FCurrentFrequency * PitchFactor;
          SourceOut := FWTGenerator.GenerateSample;
        end
        else
          SourceOut := 0.0;
        Finished := FEnvelopes[0].IsFinished;
      end;
    vstAdditive:
      begin
        // Additive runs its own amplitude envelope; drive its pitch from the
        // voice's current frequency (so glide / explicit freq / pitch mod work).
        if Assigned(FAdditive) then
        begin
          FAdditive.Frequency := FCurrentFrequency * PitchFactor;
          SourceOut := FAdditive.GenerateSample;
        end
        else
          SourceOut := 0.0;
        Finished := (not Assigned(FAdditive)) or
                    ((not FAdditive.GateOpen) and (not FAdditive.Releasing));
        UseAmpEnv := False;
      end;
    vstSample:
      begin
        // Sample playback, repitched from the voice frequency; shaped by the
        // shared amp envelope (UseAmpEnv stays True). A one-shot ends the voice
        // when it stops; a looped sample rings until the release finishes.
        if Assigned(FSamplePlayer) then
        begin
          FSamplePlayer.Frequency := FCurrentFrequency * PitchFactor;
          SourceOut := FSamplePlayer.GenerateSample;
        end
        else
          SourceOut := 0.0;
        Finished := (not Assigned(FSamplePlayer)) or
                    (not FSamplePlayer.Playing) or FEnvelopes[0].IsFinished;
      end;
    vstKarplus:
      begin
        // Plucked string: self-decaying. Shaped by the shared amp envelope; the
        // voice ends when the string rings out OR the envelope finishes.
        if Assigned(FKarplus) then
          SourceOut := FKarplus.GenerateSample
        else
          SourceOut := 0.0;
        Finished := (not Assigned(FKarplus)) or
                    (not FKarplus.IsRinging) or FEnvelopes[0].IsFinished;
      end;
  else
    // vstOscillators: 3-oscillator source (mix / ring-mod / sync)
    begin
      // Set every oscillator's frequency. Ring-mod and sync need all three
      // running (a muted-in-the-mix oscillator can still be a ring operand or a
      // sync master), so set all three uniformly.
      for I := 0 to MAX_OSCILLATORS - 1 do
        FOscillators[I].Frequency := FCurrentFrequency *
          Power(2.0, FOscillatorDetune[I] / 1200.0) * PitchFactor;

      case FOscMode of
        vomRingMod:
          begin
            // Classic ring modulation: osc0 x osc1, plus osc2 if enabled.
            OscA := FOscillators[0].GenerateSample;
            OscB := FOscillators[1].GenerateSample;
            SourceOut := OscA * OscB * FOscillatorLevels[0];
            OscA := FOscillators[2].GenerateSample;  // keep osc2 advancing
            if FOscillatorEnabled[2] then
              SourceOut := SourceOut + OscA * FOscillatorLevels[2];
          end;
        vomSync:
          begin
            // osc1/osc2 hard-synced to osc0; generate all (master must advance),
            // sum the enabled ones.
            SourceOut := 0.0;
            for I := 0 to MAX_OSCILLATORS - 1 do
            begin
              OscA := FOscillators[I].GenerateSample;
              if FOscillatorEnabled[I] then
                SourceOut := SourceOut + OscA * FOscillatorLevels[I];
            end;
          end;
      else
        // vomMix: classic sum of enabled oscillators (behaviour unchanged).
        begin
          SourceOut := 0.0;
          for I := 0 to MAX_OSCILLATORS - 1 do
            if FOscillatorEnabled[I] then
              SourceOut := SourceOut + FOscillators[I].GenerateSample * FOscillatorLevels[I];
        end;
      end;
      Finished := FEnvelopes[0].IsFinished;
    end;
  end;

  // Apply filter (shared across all source types). The effective cutoff is
  // derived each sample from the STABLE base cutoff (not the filter's current,
  // already-modulated value) — otherwise the read-modify-write recursion makes
  // the cutoff run away to the rails and the biquad self-oscillates.
  if FFilterEnabled then
  begin
    FilterCutoff := FFilterBaseCutoff;
    FilterCutoff := FilterCutoff + (FilterCutoff * 4.0 * FilterEnv * FFilterEnvelopeAmount);
    FilterCutoff := FilterCutoff * Power(2.0, ((FNote - 60) / 12.0) * FFilterKeyTracking);
    // Matrix cutoff modulation (octaves)
    if ModCutoff <> 0.0 then
      FilterCutoff := FilterCutoff * Power(2.0, ModCutoff);
    if FilterCutoff < 20.0 then
      FilterCutoff := 20.0
    else if FilterCutoff > 20000.0 then
      FilterCutoff := 20000.0;
    FFilter.Cutoff := FilterCutoff;
    FilteredOutput := FFilter.ProcessSample(SourceOut, 0);
  end
  else
    FilteredOutput := SourceOut;

  // Apply amplitude (amp envelope only for non-FM sources)
  if UseAmpEnv then
    Result := FilteredOutput * AmpEnv * FVelocity * FOutputLevel
  else
    Result := FilteredOutput * FVelocity * FOutputLevel;

  // Matrix amplitude modulation (tremolo): linear gain offset, no phase flip.
  if ModAmp <> 0.0 then
  begin
    AmpFactor := 1.0 + ModAmp;
    if AmpFactor < 0.0 then
      AmpFactor := 0.0;
    Result := Result * AmpFactor;
  end;

  // Check if voice finished
  if Finished then
    FState := vsIdle;

  // Update age
  Inc(FAge);

  // Update state based on envelope
  if FGate then
  begin
    case FEnvelopes[0].State of
      esAttack: FState := vsAttack;
      esDecay, esSustain: FState := vsPlaying;
    end;
  end
  else if FEnvelopes[0].State = esRelease then
    FState := vsReleasing;
end;

procedure TSedaiVoice.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I: Integer;
  Sample: Single;
  LeftGain, RightGain: Single;
begin
  // Calculate pan gains
  LeftGain := Cos((FPan + 1.0) * PI / 4.0);
  RightGain := Sin((FPan + 1.0) * PI / 4.0);

  // Process each frame
  for I := 0 to AFrameCount - 1 do
  begin
    Sample := ProcessSample;

    // Output stereo (interleaved)
    AOutput[I * 2] := Sample * LeftGain;       // Left
    AOutput[I * 2 + 1] := Sample * RightGain;  // Right
  end;
end;

function TSedaiVoice.IsActive: Boolean;
begin
  Result := FState <> vsIdle;
end;

function TSedaiVoice.IsReleasing: Boolean;
begin
  Result := FState = vsReleasing;
end;

function TSedaiVoice.CanSteal: Boolean;
begin
  // Can steal if not in attack phase
  Result := (FState <> vsAttack) or (FAge > FSampleRate div 10);  // Allow steal after 100ms
end;

function TSedaiVoice.GetCurrentLevel: Single;
begin
  // Return amplitude envelope level for quietest-voice stealing
  Result := FEnvelopes[0].Level * FVelocity;
end;

procedure TSedaiVoice.SetOscillatorWaveform(AIndex: Integer; AWaveform: TWaveformType);
begin
  if (AIndex >= 0) and (AIndex < MAX_OSCILLATORS) then
    FOscillators[AIndex].Waveform := AWaveform;
end;

procedure TSedaiVoice.SetOscillatorLevel(AIndex: Integer; ALevel: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_OSCILLATORS) then
  begin
    if ALevel < 0.0 then
      ALevel := 0.0
    else if ALevel > 1.0 then
      ALevel := 1.0;
    FOscillatorLevels[AIndex] := ALevel;
  end;
end;

procedure TSedaiVoice.SetOscillatorDetune(AIndex: Integer; ACents: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_OSCILLATORS) then
    FOscillatorDetune[AIndex] := ACents;
end;

procedure TSedaiVoice.SetOscillatorEnabled(AIndex: Integer; AEnabled: Boolean);
begin
  if (AIndex >= 0) and (AIndex < MAX_OSCILLATORS) then
    FOscillatorEnabled[AIndex] := AEnabled;
end;

procedure TSedaiVoice.SetOscillatorPulseWidth(AIndex: Integer; AWidth: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_OSCILLATORS) then
    FOscillators[AIndex].PulseWidth := AWidth;
end;

procedure TSedaiVoice.SetEnvelopeADSR(AIndex: Integer; AAttack, ADecay, ASustain, ARelease: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_ENVELOPES) then
  begin
    FEnvelopes[AIndex].AttackTime := AAttack;
    FEnvelopes[AIndex].DecayTime := ADecay;
    FEnvelopes[AIndex].SustainLevel := ASustain;
    FEnvelopes[AIndex].ReleaseTime := ARelease;
  end;
end;

procedure TSedaiVoice.SetEnvelopeCurve(AIndex: Integer; ACurve: TEnvelopeCurve);
begin
  if (AIndex >= 0) and (AIndex < MAX_ENVELOPES) then
  begin
    FEnvelopes[AIndex].AttackCurve := ACurve;
    FEnvelopes[AIndex].DecayCurve := ACurve;
    FEnvelopes[AIndex].ReleaseCurve := ACurve;
  end;
end;

procedure TSedaiVoice.SetFilterType(AType: TFilterType);
begin
  FFilter.FilterType := AType;
end;

procedure TSedaiVoice.SetFilterCutoff(ACutoff: Single);
begin
  FFilterBaseCutoff := ACutoff;
  FFilter.Cutoff := ACutoff;
end;

procedure TSedaiVoice.SetFilterResonance(AResonance: Single);
begin
  FFilter.Resonance := AResonance;
end;

procedure TSedaiVoice.SetFilterEnvelopeAmount(AAmount: Single);
begin
  if AAmount < -1.0 then
    AAmount := -1.0
  else if AAmount > 1.0 then
    AAmount := 1.0;
  FFilterEnvelopeAmount := AAmount;
end;

end.
