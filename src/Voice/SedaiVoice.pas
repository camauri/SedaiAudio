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
  SedaiOscillator, SedaiEnvelope, SedaiLFO, SedaiFilter;

const
  MAX_OSCILLATORS = 3;
  MAX_ENVELOPES = 3;
  MAX_LFOS = 2;

type
  { TSedaiVoice }
  // Single synthesizer voice with generator/modulator/processor chain
  TSedaiVoice = class(TSedaiSignalNode)
  private
    // Voice state
    FState: TVoiceState;
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

    // Modulators
    FEnvelopes: array[0..MAX_ENVELOPES-1] of TSedaiEnvelope;
    FLFOs: array[0..MAX_LFOS-1] of TSedaiLFO;

    // Processor
    FFilter: TSedaiFilter;
    FFilterEnabled: Boolean;
    FFilterEnvelopeAmount: Single;  // How much envelope affects filter cutoff
    FFilterKeyTracking: Single;     // How much note pitch affects cutoff (0-1)

    // Output
    FOutputLevel: Single;         // Voice output level
    FPan: Single;                 // Pan position (-1 = left, +1 = right)

    procedure SetNote(AValue: Byte);
    procedure SetVelocity(AValue: Single);
    procedure SetPitchBend(AValue: Single);
    procedure SetGlideTime(AValue: Single);

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

  // Create LFOs
  for I := 0 to MAX_LFOS - 1 do
  begin
    FLFOs[I] := TSedaiLFO.Create;
    FLFOs[I].Rate := 5.0;
    FLFOs[I].Waveform := wtSine;
  end;

  // Create filter
  FFilter := TSedaiFilter.Create;
  FFilter.FilterType := ftLowPass;
  FFilter.Cutoff := 5000.0;
  FFilter.Resonance := 0.2;
  FFilterEnabled := True;
  FFilterEnvelopeAmount := 0.5;
  FFilterKeyTracking := 0.5;

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

  for I := 0 to MAX_LFOS - 1 do
    FLFOs[I].Free;

  FFilter.Free;

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

  for I := 0 to MAX_LFOS - 1 do
    FLFOs[I].Reset;

  FFilter.Reset;
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
  if (AIndex >= 0) and (AIndex < MAX_LFOS) then
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
  for I := 0 to MAX_LFOS - 1 do
    FLFOs[I].Trigger;

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

function TSedaiVoice.ProcessSample: Single;
var
  I: Integer;
  OscOutput, FilteredOutput: Single;
  AmpEnv, FilterEnv: Single;
  FilterCutoff: Single;
begin
  if FState = vsIdle then
  begin
    Result := 0.0;
    Exit;
  end;

  // Process glide
  ProcessGlide;

  // Update oscillator frequencies
  for I := 0 to MAX_OSCILLATORS - 1 do
  begin
    if FOscillatorEnabled[I] then
      FOscillators[I].Frequency := FCurrentFrequency *
        Power(2.0, FOscillatorDetune[I] / 1200.0);
  end;

  // Process envelopes
  AmpEnv := FEnvelopes[0].Process;
  FilterEnv := FEnvelopes[1].Process;
  FEnvelopes[2].Process;  // Mod envelope

  // Process LFOs
  for I := 0 to MAX_LFOS - 1 do
    FLFOs[I].Process;

  // Mix oscillators
  OscOutput := 0.0;
  for I := 0 to MAX_OSCILLATORS - 1 do
  begin
    if FOscillatorEnabled[I] then
      OscOutput := OscOutput + FOscillators[I].GenerateSample * FOscillatorLevels[I];
  end;

  // Apply filter
  if FFilterEnabled then
  begin
    // Calculate modulated cutoff
    FilterCutoff := FFilter.Cutoff;

    // Apply filter envelope
    FilterCutoff := FilterCutoff + (FilterCutoff * 4.0 * FilterEnv * FFilterEnvelopeAmount);

    // Apply key tracking
    FilterCutoff := FilterCutoff * Power(2.0, ((FNote - 60) / 12.0) * FFilterKeyTracking);

    // Clamp cutoff
    if FilterCutoff < 20.0 then
      FilterCutoff := 20.0
    else if FilterCutoff > 20000.0 then
      FilterCutoff := 20000.0;

    FFilter.Cutoff := FilterCutoff;
    FilteredOutput := FFilter.ProcessSample(OscOutput, 0);
  end
  else
    FilteredOutput := OscOutput;

  // Apply amplitude envelope
  Result := FilteredOutput * AmpEnv * FVelocity * FOutputLevel;

  // Check if voice finished
  if FEnvelopes[0].IsFinished then
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
