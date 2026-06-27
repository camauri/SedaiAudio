{*
 * Sedai Audio Foundation - Part / Instrument
 *
 * TSAFPart is a single monotimbral instrument: a pool of universal voices
 * (TSedaiVoice = oscillators / FM / wavetable) managed by a TSedaiVoiceManager,
 * configured from a simple preset, and rendered to a stereo-interleaved buffer.
 *
 * In the Part/Instrument architecture the Part owns monotimbrality (one
 * instrument for its whole voice pool, as TSedaiVoiceManager is designed);
 * multitimbrality lives one level up in TSAFEngine, which hosts many Parts and
 * routes each to its own mixer channel.
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiPart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes,
  SedaiOscillator, SedaiFilter, SedaiFMOperator, SedaiWavetableGenerator,
  SedaiVoice, SedaiVoiceManager;

type
  // Which generator the part's voices use. Mirrors TVoiceSourceType but is the
  // public, instrument-level selector exposed by the Part.
  TSAFPartSource = (psClassic, psFM, psWavetable);

  { TSAFPart }
  // A monotimbral instrument backed by a polyphonic universal-voice manager.
  TSAFPart = class
  private
    FManager: TSedaiVoiceManager;
    FSource: TSAFPartSource;
    FPreset: string;
    FSampleRate: Cardinal;
    FName: string;

    // Voice configuration callback (of object) applied to every voice in the
    // pool by FManager.ConfigureAllVoices. Reads FSource / FPreset.
    procedure ApplyToVoice(AVoice: TSedaiVoice);

  public
    constructor Create(AMaxVoices: Integer = DEFAULT_MAX_VOICES);
    destructor Destroy; override;

    procedure SetSampleRate(AValue: Cardinal);

    // Select the instrument: source generator + a named preset. Re-applies the
    // configuration to all voices in the pool.
    procedure SetInstrument(ASource: TSAFPartSource; const APreset: string);

    // Note control (forwarded to the voice manager).
    procedure NoteOn(ANote: Byte; AVelocity: Single);

    // Trigger a note at an explicit frequency (Hz). Returns the allocated pool
    // slot (-1 if none free), so callers can keep a stable handle to the voice.
    // Classic/wavetable sources get the exact frequency; FM stays note-driven.
    function NoteOnFreq(AFreq, AVelocity: Single): Integer;

    procedure NoteOff(ANote: Byte);
    procedure AllNotesOff;
    procedure AllSoundOff;

    // Render a block of audio into AOutput (stereo, interleaved L/R).
    procedure RenderBlock(AOutput: PSingle; AFrameCount: Integer);

    function ActiveVoiceCount: Integer;

    // Direct access for advanced setup.
    property VoiceManager: TSedaiVoiceManager read FManager;
    property Source: TSAFPartSource read FSource;
    property Preset: string read FPreset;
    property Name: string read FName write FName;
    property SampleRate: Cardinal read FSampleRate;
  end;

// Stand-alone preset configurators (reused by the facade/engine if needed).
procedure ConfigureClassicVoice(AVoice: TSedaiVoice; const APreset: string);
procedure ConfigureFMVoice(AVoice: TSedaiVoice; const APreset: string);
procedure ConfigureWavetableVoice(AVoice: TSedaiVoice; const APreset: string);

implementation

// ============================================================================
// PRESET CONFIGURATORS
// ============================================================================

procedure ConfigureClassicVoice(AVoice: TSedaiVoice; const APreset: string);
var
  P: string;
begin
  AVoice.SetSourceType(vstOscillators);
  P := LowerCase(APreset);

  // Sensible defaults (overridden per preset below).
  AVoice.SetOscillatorEnabled(0, True);
  AVoice.SetOscillatorWaveform(0, wtSawtooth);
  AVoice.FilterEnabled := True;
  AVoice.SetFilterType(ftLowPass);
  AVoice.SetFilterCutoff(2500.0);
  AVoice.SetFilterResonance(0.2);
  AVoice.SetEnvelopeADSR(0, 0.01, 0.1, 0.7, 0.3);

  if P = 'sine' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSine);
    AVoice.SetFilterCutoff(10000.0);
  end
  else if P = 'square' then
    AVoice.SetOscillatorWaveform(0, wtSquare)
  else if (P = 'saw') or (P = 'sawtooth') then
    AVoice.SetOscillatorWaveform(0, wtSawtooth)
  else if P = 'triangle' then
    AVoice.SetOscillatorWaveform(0, wtTriangle)
  else if P = 'pulse' then
    AVoice.SetOscillatorWaveform(0, wtPulse)
  else if P = 'noise' then
  begin
    AVoice.SetOscillatorWaveform(0, wtNoise);
    AVoice.FilterEnabled := False;
  end
  else if P = 'lead' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSawtooth);
    AVoice.SetFilterCutoff(2500.0);
    AVoice.SetFilterResonance(0.6);
    AVoice.SetEnvelopeADSR(0, 0.01, 0.1, 0.8, 0.2);
  end
  else if P = 'bass' then
  begin
    AVoice.SetOscillatorWaveform(0, wtSquare);
    AVoice.SetFilterCutoff(800.0);
    AVoice.SetFilterResonance(0.4);
    AVoice.SetEnvelopeADSR(0, 0.01, 0.2, 0.6, 0.15);
  end
  else if P = 'pad' then
  begin
    AVoice.SetOscillatorWaveform(0, wtTriangle);
    AVoice.SetFilterCutoff(1800.0);
    AVoice.SetEnvelopeADSR(0, 0.5, 0.5, 0.7, 1.0);
  end;
end;

procedure ConfigureFMVoice(AVoice: TSedaiVoice; const APreset: string);
var
  FM: TSedaiFMSynth;
  Op: TSedaiFMOperator;
  P: string;
  I: Integer;
  Trim: Single;
begin
  AVoice.SetSourceType(vstFM);
  FM := AVoice.GetFMSynth;   // created on demand
  if FM = nil then Exit;

  P := LowerCase(APreset);

  // Start from a clean slate: silence all operators, then build the preset.
  for I := 0 to 5 do
    FM.GetOperator(I).Level := 0.0;

  // Per-preset output trim (gain staging): the raw FM presets span ~0.17..2.9
  // peak, which clips and pins the shared master limiter. Each Trim = roughly
  // min(0.9/peak, 0.32/rms) measured offline (note 60, stable filter) → ~0.9
  // peak ceiling, consistent RMS where the preset isn't peak-limited.
  Trim := 1.0;

  if P = 'epiano' then
  begin
    Trim := 0.72;
    FM.Algorithm := 5;
    FM.FeedbackLevel := 0.3;
    Op := FM.GetOperator(0);
    Op.Ratio := 1.0;  Op.Level := 0.9;
    Op.AttackRate := 95; Op.Decay1Rate := 70; Op.SustainLevel := 0.3; Op.ReleaseRate := 60;
    Op := FM.GetOperator(1);
    Op.Ratio := 14.0; Op.Level := 0.5;
    Op.AttackRate := 95; Op.Decay1Rate := 85; Op.SustainLevel := 0.0; Op.ReleaseRate := 70;
  end
  else if P = 'brass' then
  begin
    Trim := 0.42;
    FM.Algorithm := 1;
    FM.FeedbackLevel := 0.5;
    Op := FM.GetOperator(0);
    Op.Ratio := 1.0; Op.Level := 0.9;
    Op.AttackRate := 80; Op.Decay1Rate := 50; Op.SustainLevel := 0.8; Op.ReleaseRate := 50;
    Op := FM.GetOperator(1);
    Op.Ratio := 1.0; Op.Level := 0.7;
    Op.AttackRate := 70; Op.Decay1Rate := 60; Op.SustainLevel := 0.5; Op.ReleaseRate := 50;
  end
  else if P = 'bell' then
  begin
    Trim := 0.47;
    FM.Algorithm := 1;
    FM.FeedbackLevel := 0.0;
    Op := FM.GetOperator(0);
    Op.Ratio := 1.0; Op.Level := 0.8;
    Op.AttackRate := 99; Op.Decay1Rate := 40; Op.SustainLevel := 0.0; Op.ReleaseRate := 30;
    Op := FM.GetOperator(1);
    Op.Ratio := 3.5; Op.Level := 0.9;
    Op.AttackRate := 99; Op.Decay1Rate := 50; Op.SustainLevel := 0.0; Op.ReleaseRate := 40;
  end
  else if P = 'organ' then
  begin
    Trim := 3.90;        // additive /6 → quiet; boosted back up
    FM.Algorithm := 32;  // all carriers (additive)
    FM.GetOperator(0).Ratio := 0.5; FM.GetOperator(0).Level := 0.6;
    FM.GetOperator(1).Ratio := 1.0; FM.GetOperator(1).Level := 0.8;
    FM.GetOperator(2).Ratio := 2.0; FM.GetOperator(2).Level := 0.5;
    FM.GetOperator(3).Ratio := 3.0; FM.GetOperator(3).Level := 0.3;
    FM.GetOperator(4).Ratio := 4.0; FM.GetOperator(4).Level := 0.2;
    FM.GetOperator(5).Ratio := 6.0; FM.GetOperator(5).Level := 0.1;
  end
  else if P = 'bass' then
  begin
    Trim := 0.31;
    FM.Algorithm := 1;
    FM.FeedbackLevel := 0.6;
    Op := FM.GetOperator(0);
    Op.Ratio := 1.0; Op.Level := 0.9;
    Op.AttackRate := 95; Op.Decay1Rate := 60; Op.SustainLevel := 0.5; Op.ReleaseRate := 70;
    Op := FM.GetOperator(1);
    Op.Ratio := 1.0; Op.Level := 0.8;
    Op.AttackRate := 90; Op.Decay1Rate := 70; Op.SustainLevel := 0.3; Op.ReleaseRate := 70;
  end
  else  // default: simple 2-op FM
  begin
    Trim := 0.36;
    FM.Algorithm := 1;
    FM.FeedbackLevel := 0.2;
    FM.GetOperator(0).Ratio := 1.0; FM.GetOperator(0).Level := 0.8;
    FM.GetOperator(1).Ratio := 2.0; FM.GetOperator(1).Level := 0.5;
  end;

  AVoice.OutputLevel := Trim;
end;

procedure ConfigureWavetableVoice(AVoice: TSedaiVoice; const APreset: string);
var
  WT: TSedaiWavetableGenerator;
  P: string;
  Trim: Single;
begin
  AVoice.SetSourceType(vstWavetable);
  WT := AVoice.GetWavetableGenerator;  // created on demand
  if WT = nil then Exit;

  P := LowerCase(APreset);

  // Per-preset output trim (gain staging, same scheme as the FM presets).
  Trim := 0.72;  // basic

  if P = 'pwm' then
  begin
    Trim := 0.49;
    WT.CreatePWMWavetable(64);
  end
  else if P = 'supersaw' then
  begin
    Trim := 0.66;
    WT.CreateSuperSawWavetable(32);
    WT.UnisonVoices := 7;
    WT.UnisonDetune := 20.0;
    WT.UnisonSpread := 0.7;
  end
  else
    WT.CreateBasicWavetable;

  // Wavetable voices use the shared amp envelope for their lifecycle.
  AVoice.SetEnvelopeADSR(0, 0.01, 0.2, 0.7, 0.3);
  AVoice.OutputLevel := Trim;
end;

// ============================================================================
// TSAFPart
// ============================================================================

constructor TSAFPart.Create(AMaxVoices: Integer);
begin
  inherited Create;

  FManager := TSedaiVoiceManager.Create;
  FManager.MaxVoices := AMaxVoices;
  FSource := psClassic;
  FPreset := 'saw';
  FSampleRate := SEDAI_DEFAULT_SAMPLE_RATE;
  FName := 'Part';

  // Apply the default instrument so the pool is immediately playable.
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

destructor TSAFPart.Destroy;
begin
  FManager.Free;
  inherited Destroy;
end;

procedure TSAFPart.SetSampleRate(AValue: Cardinal);
begin
  FSampleRate := AValue;
  FManager.SetSampleRate(AValue);
end;

procedure TSAFPart.ApplyToVoice(AVoice: TSedaiVoice);
begin
  case FSource of
    psFM:        ConfigureFMVoice(AVoice, FPreset);
    psWavetable: ConfigureWavetableVoice(AVoice, FPreset);
  else
    ConfigureClassicVoice(AVoice, FPreset);
  end;
end;

procedure TSAFPart.SetInstrument(ASource: TSAFPartSource; const APreset: string);
begin
  FSource := ASource;
  FPreset := APreset;
  FManager.ConfigureAllVoices(@ApplyToVoice);
end;

procedure TSAFPart.NoteOn(ANote: Byte; AVelocity: Single);
begin
  FManager.NoteOn(ANote, AVelocity);
end;

function TSAFPart.NoteOnFreq(AFreq, AVelocity: Single): Integer;
var
  Note: Integer;
  Slot: Integer;
  V: TSedaiVoice;
begin
  Result := -1;
  if AFreq <= 0.0 then Exit;

  // Nearest MIDI note (drives key-tracking / FM pitch); exact Hz applied below.
  Note := Round(69.0 + 12.0 * Log2(AFreq / 440.0));
  if Note < 0 then Note := 0;
  if Note > 127 then Note := 127;

  FManager.NoteOn(Note, AVelocity);
  Slot := FManager.LastVoiceIndex;
  if Slot < 0 then Exit;

  if FSource <> psFM then
  begin
    V := FManager.GetVoice(Slot);
    if V <> nil then
      V.SetExplicitFrequency(AFreq);
  end;

  Result := Slot;
end;

procedure TSAFPart.NoteOff(ANote: Byte);
begin
  FManager.NoteOff(ANote);
end;

procedure TSAFPart.AllNotesOff;
begin
  FManager.AllNotesOff;
end;

procedure TSAFPart.AllSoundOff;
begin
  FManager.AllSoundOff;
end;

procedure TSAFPart.RenderBlock(AOutput: PSingle; AFrameCount: Integer);
begin
  // VoiceManager outputs stereo interleaved; it clears AOutput internally.
  FManager.ProcessBlock(nil, AOutput, AFrameCount);
end;

function TSAFPart.ActiveVoiceCount: Integer;
begin
  Result := FManager.ActiveVoiceCount;
end;

end.
