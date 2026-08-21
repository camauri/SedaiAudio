// ============================================================================
// SedaiPatchInstruments — SAF's synthesis engines, patchable.
//
// The physical models and the FM operator were built over sessions 14 to 20 and
// lived only behind TSedaiPart and a .safinst file. A patch could not touch any
// of them. Now they are modules: pitch in, gate in, audio out, and every input
// on them can be driven by anything else in the graph — which is the point.
//
// All sample-first. Every generator in SAF overrides GenerateSample, so unlike
// the block-oriented effects these are mrBoth and may sit inside a feedback
// cycle. One wrapper covers them all because they share a base: Frequency,
// Amplitude, GenerateSample. Only excitation differs, and that is what the
// subclasses supply.
//
// NOT here yet, and deliberately rather than by oversight: additive, partial,
// wavetable and sample playback. Those four need DATA — harmonic tracks, a
// wavetable, a sample file — so they need a loading path in the patch file
// first. Wrapping them empty would give five silent modules and the appearance
// of coverage.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchInstruments;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, SedaiAudioTypes, SedaiPatchGraph, SedaiOscillator,
  SedaiKarplusGenerator, SedaiModalGenerator, SedaiBowedGenerator,
  SedaiReedGenerator, SedaiFMOperator, SedaiGranularGenerator, SedaiRandom;

type
  { TSedaiInstrumentModule }

  TSedaiInstrumentModule = class(TSedaiPatchModule)
  private
    FPitchIn, FGateIn, FAmpIn, FOut: TSedaiPatchPort;
    FLastGate: Single;
    FBaseFreq: Single;
  protected
    FGen: TSedaiSignalGenerator;
    function CreateGen: TSedaiSignalGenerator; virtual; abstract;
    // Excitation differs per family: a plucked string is struck once, a bowed
    // string is driven continuously. The subclass knows which.
    procedure TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single); virtual; abstract;
    procedure TriggerOff; virtual;
    function ConfigureGen(const AKey, AValue: string;
      AFloat: Single): Boolean; virtual;
    function ConfigKeys: string; override;
    // Called once per sample, after the pitch is set and before the generator
    // produces. It exists for engines that take an input as well as a note —
    // the FM operator's phase modulation is the only one today, and without it
    // an operator can only ever be a sine with an envelope on it.
    procedure BeforeSample(AIndex: Integer); virtual;
    // Engines with a random component seed themselves from the module's own
    // name, so a patch sounds the same whatever else was built first. Does
    // nothing for the engines that have none.
    procedure SeedGen(ASeed: QWord); virtual;
    // Output trim. The engines were each validated on their own and land as
    // much as 24 dB apart (measured at 220 Hz: FM operator RMS 0.458, bowed
    // string 0.030). Patched together the loud one buries the quiet one, so
    // each family carries a trim that brings a bare module to roughly the same
    // level as the others. It is the module's output attenuator, nothing more:
    // it only sets the DEFAULT of the amp input, which the patch still owns.
    function DefaultTrim: Single; virtual;
    property Generator: TSedaiSignalGenerator read FGen;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    procedure ResetState; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  TSedaiModKarplus = class(TSedaiInstrumentModule)
  protected
    function CreateGen: TSedaiSignalGenerator; override;
    procedure SeedGen(ASeed: QWord); override;
    procedure TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single); override;
  public
    constructor Create; override;
  end;

  { granular — a cloud of grains read from a recording.

    Deliberately NOT a TSedaiInstrumentModule: that base is shaped around a
    note, and a cloud is not one. It has no gate and no root pitch; it has a
    place in a recording, a density and a handful of spreads, and it runs
    continuously. Forcing it into the note shape would have cost it the one
    thing granular is for — that speed and pitch stop being the same knob. }
  TSedaiModGranular = class(TSedaiPatchModule)
  private
    FGen: TSedaiGranularGenerator;
    FPosIn, FDensIn, FSizeIn, FPitchIn: TSedaiPatchPort;
    FSpreadIn, FPSpreadIn, FPanIn, FAmpIn: TSedaiPatchPort;
    FOut, FOutR: TSedaiPatchPort;
    FLoaded: Boolean;
    FSeedGiven: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ConfigKeys: string; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  TSedaiModModal = class(TSedaiInstrumentModule)
  protected
    function CreateGen: TSedaiSignalGenerator; override;
    function DefaultTrim: Single; override;
    procedure TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single); override;
    procedure TriggerOff; override;
  public
    constructor Create; override;
  end;

  TSedaiModBowed = class(TSedaiInstrumentModule)
  protected
    function CreateGen: TSedaiSignalGenerator; override;
    function DefaultTrim: Single; override;
    procedure TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single); override;
    procedure TriggerOff; override;
  public
    constructor Create; override;
  end;

  TSedaiModReed = class(TSedaiInstrumentModule)
  protected
    function CreateGen: TSedaiSignalGenerator; override;
    function DefaultTrim: Single; override;
    procedure TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single); override;
    procedure TriggerOff; override;
  public
    constructor Create; override;
  end;

  { TSedaiModFMOp — one FM operator, with its phase modulation input exposed.

    That input is the whole technique. An operator whose phase nothing drives
    is a sine with an envelope; an operator driven by another operator is a
    DX7. The engine underneath always had it (SetModInput, summed into the
    phase before the sine is taken) — this module simply did not offer it.

    NOTE ON THE INDEX. There is no separate index control, and there should not
    be: the modulator's own output level IS the index, so `amp` on the
    modulating operator does that job. Patch an envelope or velocity into the
    modulator's `amp` and the tone gets brighter as it gets louder, which is
    what makes an FM electric piano behave like an instrument rather than a
    recording of one. }

  TSedaiModFMOp = class(TSedaiInstrumentModule)
  private
    FPhaseMIn: TSedaiPatchPort;
  protected
    function CreateGen: TSedaiSignalGenerator; override;
    function DefaultTrim: Single; override;
    function ConfigureGen(const AKey, AValue: string;
      AFloat: Single): Boolean; override;
    function ConfigKeys: string; override;
    procedure BeforeSample(AIndex: Integer); override;
    procedure TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single); override;
    procedure TriggerOff; override;
  public
    constructor Create; override;
  end;

function CreateInstrumentModuleByType(const ATypeName: string): TSedaiPatchModule;
function KnownInstrumentTypes: string;

implementation

{ TSedaiInstrumentModule }

constructor TSedaiInstrumentModule.Create;
begin
  inherited Create;
  Rate := mrBoth;
  FBaseFreq := 220.0;
  FLastGate := 0.0;
  FGen := CreateGen;
  FPitchIn := AddInput('pitch', prPitch, 0.0);
  FGateIn  := AddInput('gate', prGate, 0.0);
  FAmpIn   := AddInput('amp', prUnipolar, DefaultTrim);
  FAmpIn.Min := 0.0; FAmpIn.Max := 8.0;
  FOut     := AddOutput('out', prAudio);
end;

destructor TSedaiInstrumentModule.Destroy;
begin
  FGen.Free;
  inherited Destroy;
end;

procedure TSedaiInstrumentModule.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  SeedGen(SedaiSeedFromName(TypeName + '.' + ModuleName));
  inherited Prepare(ASampleRate, ABlockSize);
  if FGen <> nil then FGen.SetSampleRate(ASampleRate);
end;

procedure TSedaiInstrumentModule.ResetState;
begin
  inherited ResetState;
  FLastGate := 0.0;
  if FGen <> nil then FGen.Reset;
end;

procedure TSedaiInstrumentModule.TriggerOff;
begin
  // Plucked and struck families ignore this: they ring out on their own.
end;

function TSedaiInstrumentModule.DefaultTrim: Single;
begin
  Result := 1.0;
end;

procedure TSedaiInstrumentModule.SeedGen(ASeed: QWord);
begin
  // Most engines have nothing random in them.
end;

procedure TSedaiInstrumentModule.BeforeSample(AIndex: Integer);
begin
  // Most engines want nothing here.
end;

function TSedaiInstrumentModule.ConfigKeys: string;
begin
  Result := 'freq';
end;

function TSedaiInstrumentModule.ConfigureGen(const AKey, AValue: string;
  AFloat: Single): Boolean;
begin
  Result := False;
end;

function TSedaiInstrumentModule.Configure(const AKey, AValue: string): Boolean;
var
  F: Single;
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  F := 0.0;
  TryStrToFloat(Trim(AValue), F, FS);
  if SameText(AKey, 'freq') then
  begin
    FBaseFreq := F;
    Exit(True);
  end;
  Result := ConfigureGen(AKey, AValue, F);
  if not Result then Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiInstrumentModule.RenderSample(AIndex: Integer);
var
  Volts, Freq, G: Single;
  Note: Integer;
begin
  Volts := FPitchIn.Read(AIndex);
  Freq := FBaseFreq * Power(2.0, Volts);
  if Freq < 1.0 then Freq := 1.0;
  if Freq > FSR * 0.45 then Freq := FSR * 0.45;

  G := FGateIn.Read(AIndex);
  if (G > 0.5) and (FLastGate <= 0.5) then
  begin
    // Some engines want a MIDI note, some a frequency; hand over both and let
    // the subclass take what its API asks for.
    Note := Round(69.0 + 12.0 * Log2(Freq / 440.0));
    if Note < 0 then Note := 0 else if Note > 127 then Note := 127;
    TriggerOn(Freq, Note, 1.0);
  end
  else if (G <= 0.5) and (FLastGate > 0.5) then
    TriggerOff;
  FLastGate := G;

  // Pitch stays live between triggers, so a glide or an LFO on the pitch input
  // bends the note as it sounds instead of only setting it at the attack.
  FGen.Frequency := Freq;
  BeforeSample(AIndex);
  FOut.Write(AIndex, FGen.GenerateSample * FAmpIn.Read(AIndex));
end;

{ karplus — plucked string }

constructor TSedaiModKarplus.Create;
begin
  inherited Create;
  TypeName := 'karplus';
end;

function TSedaiModKarplus.CreateGen: TSedaiSignalGenerator;
begin
  Result := TSedaiKarplusGenerator.Create;
end;

procedure TSedaiModKarplus.SeedGen(ASeed: QWord);
begin
  // The noise burst that plucks the string.
  TSedaiKarplusGenerator(FGen).SetSeed(ASeed);
end;

procedure TSedaiModKarplus.TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single);
begin
  TSedaiKarplusGenerator(FGen).NoteOn(AFreq, AVelocity);
end;

{ modal — struck percussion }

constructor TSedaiModModal.Create;
begin
  inherited Create;
  TypeName := 'modal';
end;

function TSedaiModModal.DefaultTrim: Single;
begin
  Result := 1.25;   // measured 0.121
end;

function TSedaiModModal.CreateGen: TSedaiSignalGenerator;
begin
  Result := TSedaiModalGenerator.Create;
end;

procedure TSedaiModModal.TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single);
begin
  TSedaiModalGenerator(FGen).NoteOn(ANote, AVelocity);
end;

procedure TSedaiModModal.TriggerOff;
begin
  TSedaiModalGenerator(FGen).NoteOff;
end;

{ bowed — sustained string }

constructor TSedaiModBowed.Create;
begin
  inherited Create;
  TypeName := 'bowed';
end;

function TSedaiModBowed.DefaultTrim: Single;
begin
  Result := 5.0;   // measured 0.030
end;

function TSedaiModBowed.CreateGen: TSedaiSignalGenerator;
begin
  Result := TSedaiBowedGenerator.Create;
end;

procedure TSedaiModBowed.TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single);
begin
  TSedaiBowedGenerator(FGen).NoteOn(ANote, AVelocity);
end;

procedure TSedaiModBowed.TriggerOff;
begin
  TSedaiBowedGenerator(FGen).NoteOff;
end;

{ reed — sustained wind }

constructor TSedaiModReed.Create;
begin
  inherited Create;
  TypeName := 'reed';
end;

function TSedaiModReed.DefaultTrim: Single;
begin
  Result := 1.8;   // measured 0.085
end;

function TSedaiModReed.CreateGen: TSedaiSignalGenerator;
begin
  Result := TSedaiReedGenerator.Create;
end;

procedure TSedaiModReed.TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single);
begin
  TSedaiReedGenerator(FGen).NoteOn(ANote, AVelocity);
end;

procedure TSedaiModReed.TriggerOff;
begin
  TSedaiReedGenerator(FGen).NoteOff;
end;

{ fmop — a DX7-style operator }

constructor TSedaiModFMOp.Create;
begin
  inherited Create;
  TypeName := 'fmop';
  FPhaseMIn := AddInput('phasem', prAudio, 0.0);
end;

function TSedaiModFMOp.DefaultTrim: Single;
begin
  Result := 0.33;   // measured 0.458
end;

function TSedaiModFMOp.CreateGen: TSedaiSignalGenerator;
begin
  Result := TSedaiFMOperator.Create;
end;

function TSedaiModFMOp.ConfigKeys: string;
begin
  Result := 'detune, feedback, fixedfreq, ratio';
end;

function TSedaiModFMOp.ConfigureGen(const AKey, AValue: string;
  AFloat: Single): Boolean;
begin
  Result := True;
  // The ratio is what makes an operator a partial of the note rather than a
  // note of its own: 1 is the fundamental, 14 is the metallic tine of a
  // Rhodes, and a non-integer like 3.5 is a bell, because it is not a harmonic
  // of anything.
  if SameText(AKey, 'ratio') then TSedaiFMOperator(FGen).Ratio := AFloat
  else if SameText(AKey, 'detune') then TSedaiFMOperator(FGen).Detune := AFloat
  else if SameText(AKey, 'feedback') then TSedaiFMOperator(FGen).FeedbackLevel := AFloat
  else if SameText(AKey, 'fixedfreq') then
  begin
    TSedaiFMOperator(FGen).Fixed := True;
    TSedaiFMOperator(FGen).FixedFreq := AFloat;
  end
  else Result := inherited ConfigureGen(AKey, AValue, AFloat);
end;

procedure TSedaiModFMOp.BeforeSample(AIndex: Integer);
begin
  TSedaiFMOperator(FGen).SetModInput(0, FPhaseMIn.Read(AIndex));
end;

procedure TSedaiModFMOp.TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single);
begin
  TSedaiFMOperator(FGen).NoteOn(ANote, AVelocity);
end;

procedure TSedaiModFMOp.TriggerOff;
begin
  TSedaiFMOperator(FGen).NoteOff;
end;

{ TSedaiModGranular }

constructor TSedaiModGranular.Create;
begin
  inherited Create;
  TypeName := 'granular';
  Rate := mrBoth;
  FGen := TSedaiGranularGenerator.Create;
  FLoaded := False;
  FSeedGiven := False;

  // Where in the recording, 0..1. A knob, so an LFO or an envelope patched
  // here scrubs through the source — which is the gesture the technique is for.
  FPosIn := AddInput('pos', prUnipolar, 0.0);
  FPosIn.Min := 0.0; FPosIn.Max := 1.0;
  // Grains per second. Below about twenty you hear them one by one; above, they
  // fuse. Both are useful, so nothing here forces the choice.
  FDensIn := AddInput('dens', prUnipolar, 40.0);
  FDensIn.Min := 0.1; FDensIn.Max := 2000.0;
  FSizeIn := AddInput('size', prUnipolar, 50.0);      // milliseconds
  FSizeIn.Min := 1.0; FSizeIn.Max := 2000.0;
  // Volts per octave, like every other pitch input in the workbench, so the
  // same LFO that is vibrato on an oscillator is vibrato here.
  FPitchIn := AddInput('pitch', prPitch, 0.0);
  FSpreadIn := AddInput('spread', prUnipolar, 0.0);   // seconds, either side
  FSpreadIn.Min := 0.0; FSpreadIn.Max := 10.0;
  FPSpreadIn := AddInput('pspread', prUnipolar, 0.0); // cents, either side
  FPSpreadIn.Min := 0.0; FPSpreadIn.Max := 2400.0;
  FPanIn := AddInput('pan', prUnipolar, 0.0);         // 0..1 of the full width
  FPanIn.Min := 0.0; FPanIn.Max := 1.0;
  FAmpIn := AddInput('amp', prUnipolar, 1.0);
  FAmpIn.Min := 0.0; FAmpIn.Max := 4.0;

  FOut := AddOutput('out', prAudio);
  // The second channel is where the pan spread goes. A patch that wants one
  // signal simply never connects it.
  FOutR := AddOutput('outR', prAudio);
end;

destructor TSedaiModGranular.Destroy;
begin
  FGen.Free;
  inherited Destroy;
end;

function TSedaiModGranular.ConfigKeys: string;
begin
  Result := 'sample, seed, skirt, speed, window';
end;

function TSedaiModGranular.Configure(const AKey, AValue: string): Boolean;
var
  F: Single;
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings; FS.DecimalSeparator := '.';
  Result := True;
  if SameText(AKey, 'sample') then
  begin
    if not FGen.LoadSampleFromFile(Trim(AValue)) then
      raise Exception.CreateFmt('granular: cannot read the sample "%s"', [AValue]);
    FLoaded := True;
  end
  else if SameText(AKey, 'speed') then
  begin
    // How fast the head walks the recording. 1 is natural, 0 FREEZES it, and
    // negative runs it backwards. Freezing is the thing a sampler cannot do at
    // all, so it is a first-class value here and not an edge case.
    if not TryStrToFloat(Trim(AValue), F, FS) then Exit(False);
    FGen.Speed := F;
  end
  else if SameText(AKey, 'window') then
  begin
    if SameText(AValue, 'hann') then FGen.Window := gwHann
    else if SameText(AValue, 'triangle') then FGen.Window := gwTriangle
    else if SameText(AValue, 'tukey') then FGen.Window := gwTukey
    else Exit(False);
  end
  else if SameText(AKey, 'skirt') then
  begin
    if not TryStrToFloat(Trim(AValue), F, FS) then Exit(False);
    FGen.TukeySkirt := F;
  end
  else if SameText(AKey, 'seed') then
  begin
    FGen.SetSeed(QWord(StrToInt64Def(Trim(AValue), 0)));
    FSeedGiven := True;
  end
  else
    Result := False;
end;

procedure TSedaiModGranular.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  inherited Prepare(ASampleRate, ABlockSize);
  FGen.SetSampleRate(ASampleRate);
  // From the module's own name, so the cloud is the same whatever else the
  // program built first — unless the patch asked for a particular seed, in
  // which case it wins.
  if not FSeedGiven then
    FGen.SetSeed(SedaiSeedFromName('granular.' + ModuleName));
end;

procedure TSedaiModGranular.ResetState;
begin
  inherited ResetState;
  FGen.Reset;
end;

procedure TSedaiModGranular.RenderSample(AIndex: Integer);
var
  L, R: Single;
begin
  if not FLoaded then
  begin
    // No recording, no cloud. Silence rather than a guess: a granular module
    // with nothing to read is a patch that forgot its `sample=`.
    FOut.Write(AIndex, 0.0);
    FOutR.Write(AIndex, 0.0);
    Exit;
  end;
  FGen.Position := FPosIn.Read(AIndex);
  FGen.Density := FDensIn.Read(AIndex);
  FGen.GrainMs := FSizeIn.Read(AIndex);
  FGen.Pitch := Power(2.0, FPitchIn.Read(AIndex));
  FGen.PositionSpread := FSpreadIn.Read(AIndex);
  FGen.PitchSpread := FPSpreadIn.Read(AIndex);
  FGen.PanSpread := FPanIn.Read(AIndex);
  FGen.Amplitude := FAmpIn.Read(AIndex);
  FGen.GenerateStereo(L, R);
  FOut.Write(AIndex, L);
  FOutR.Write(AIndex, R);
end;

{ factory }

function CreateInstrumentModuleByType(const ATypeName: string): TSedaiPatchModule;
begin
  if SameText(ATypeName, 'granular') then Result := TSedaiModGranular.Create
  else if SameText(ATypeName, 'karplus') then Result := TSedaiModKarplus.Create
  else if SameText(ATypeName, 'modal') then Result := TSedaiModModal.Create
  else if SameText(ATypeName, 'bowed') then Result := TSedaiModBowed.Create
  else if SameText(ATypeName, 'reed') then Result := TSedaiModReed.Create
  else if SameText(ATypeName, 'fmop') then Result := TSedaiModFMOp.Create
  else Result := nil;
end;

function KnownInstrumentTypes: string;
begin
  Result := 'granular, karplus, modal, bowed, reed, fmop';
end;

end.
