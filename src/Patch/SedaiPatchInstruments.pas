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
  SedaiReedGenerator, SedaiFMOperator;

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
    function ConfigureGen(const AKey, AValue: string; AFloat: Single): Boolean; virtual;
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
    procedure TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single); override;
  public
    constructor Create; override;
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

  TSedaiModFMOp = class(TSedaiInstrumentModule)
  protected
    function CreateGen: TSedaiSignalGenerator; override;
    function DefaultTrim: Single; override;
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
end;

function TSedaiModFMOp.DefaultTrim: Single;
begin
  Result := 0.33;   // measured 0.458
end;

function TSedaiModFMOp.CreateGen: TSedaiSignalGenerator;
begin
  Result := TSedaiFMOperator.Create;
end;

procedure TSedaiModFMOp.TriggerOn(AFreq: Single; ANote: Integer; AVelocity: Single);
begin
  TSedaiFMOperator(FGen).NoteOn(ANote, AVelocity);
end;

procedure TSedaiModFMOp.TriggerOff;
begin
  TSedaiFMOperator(FGen).NoteOff;
end;

{ factory }

function CreateInstrumentModuleByType(const ATypeName: string): TSedaiPatchModule;
begin
  if SameText(ATypeName, 'karplus') then Result := TSedaiModKarplus.Create
  else if SameText(ATypeName, 'modal') then Result := TSedaiModModal.Create
  else if SameText(ATypeName, 'bowed') then Result := TSedaiModBowed.Create
  else if SameText(ATypeName, 'reed') then Result := TSedaiModReed.Create
  else if SameText(ATypeName, 'fmop') then Result := TSedaiModFMOp.Create
  else Result := nil;
end;

function KnownInstrumentTypes: string;
begin
  Result := 'karplus, modal, bowed, reed, fmop';
end;

end.
