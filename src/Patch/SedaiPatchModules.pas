// ============================================================================
// SedaiPatchModules — the first native modules for the patch graph.
//
// All of them are written SAMPLE-FIRST: RenderSample is the real DSP and
// RenderBlock is the loop it gets for free from TSedaiPatchModule. That is what
// lets one implementation serve both schedulers, so a module can sit inside a
// feedback cycle without a second code path (design notes, 6.1).
//
// Pitch inputs use VOLTS PER OCTAVE semantics: 1.0 = one octave. A constant
// offset is therefore a transposition and any modulator is automatically
// musical, which is Moog's 1 V/oct convention and the reason an LFO into pitch
// and an oscillator into pitch are the same patch at two frequencies.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchModules;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, SedaiPatchGraph;

type
  TSedaiOscShape = (osSaw, osSquare, osTriangle, osSine, osPulse);

  { TSedaiModOsc — the sound source }
  TSedaiModOsc = class(TSedaiPatchModule)
  private
    FPitchIn, FPwIn, FSyncIn, FOut: TSedaiPatchPort;
    FPhase: Double;
    FShape: TSedaiOscShape;
    FBaseFreq: Single;
    FLastSync: Single;
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModFilter — 2-pole state-variable, self-oscillates at high resonance }
  TSedaiModFilterMode = (fmLowpass, fmHighpass, fmBandpass);

  TSedaiModFilter = class(TSedaiPatchModule)
  private
    FIn, FCutoffIn, FResIn, FOut: TSedaiPatchPort;
    FLow, FBand: Single;
    FMode: TSedaiModFilterMode;
    FBaseCutoff: Single;
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModAmp — the obligatory VCA }
  TSedaiModAmp = class(TSedaiPatchModule)
  private
    FIn, FGainIn, FOut: TSedaiPatchPort;
  public
    constructor Create; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModEnv — ADSR, gate driven }
  TSedaiEnvStage = (esIdle, esAttack, esDecay, esSustain, esRelease);

  TSedaiModEnv = class(TSedaiPatchModule)
  private
    FGateIn, FOut: TSedaiPatchPort;
    FStage: TSedaiEnvStage;
    FLevel: Single;
    FA, FD, FS, FR: Single;      // seconds, seconds, level, seconds
    FGateWasOpen: Boolean;
    function RateFor(ASeconds: Single): Single;
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModLFO — same engine as the oscillator, just usually slower.
    Deliberately a separate module only for clarity in a patch file: patch its
    output into a pitch input and speed it up and it IS an FM operator. }
  TSedaiModLFO = class(TSedaiPatchModule)
  private
    FRateIn, FOut: TSedaiPatchPort;
    FPhase: Double;
    FShape: TSedaiOscShape;
    FBaseRate: Single;
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModNote — the "keyboard": what the player or the renderer drives }
  TSedaiModNote = class(TSedaiPatchModule)
  private
    FPitchOut, FGateOut: TSedaiPatchPort;
    FPitch, FGate: Single;
  public
    constructor Create; override;
    procedure RenderSample(AIndex: Integer); override;
    procedure SetNote(APitchVolts, AGate: Single);
    property Pitch: Single read FPitch write FPitch;
    property Gate: Single read FGate write FGate;
  end;

// Factory: maps the type name in a patch file to a class.
function CreateModuleByType(const ATypeName: string): TSedaiPatchModule;
function KnownModuleTypes: string;

implementation

const
  TWO_PI = 6.283185307179586;

function ShapeFromName(const AName: string; out AShape: TSedaiOscShape): Boolean;
begin
  Result := True;
  if SameText(AName, 'saw') then AShape := osSaw
  else if SameText(AName, 'square') then AShape := osSquare
  else if SameText(AName, 'triangle') or SameText(AName, 'tri') then AShape := osTriangle
  else if SameText(AName, 'sine') or SameText(AName, 'sin') then AShape := osSine
  else if SameText(AName, 'pulse') then AShape := osPulse
  else Result := False;
end;

// One shared naive generator. No band-limiting yet: P1 is about the graph, and
// an aliasing saw is honest about that rather than pretending otherwise.
function ShapeValue(AShape: TSedaiOscShape; APhase: Double; APw: Single): Single;
begin
  case AShape of
    osSaw:      Result := Single(2.0 * APhase - 1.0);
    osSquare:   if APhase < 0.5 then Result := 1.0 else Result := -1.0;
    osTriangle: if APhase < 0.5 then Result := Single(4.0 * APhase - 1.0)
                else Result := Single(3.0 - 4.0 * APhase);
    osSine:     Result := Single(Sin(TWO_PI * APhase));
    osPulse:    if APhase < APw then Result := 1.0 else Result := -1.0;
  else
    Result := 0.0;
  end;
end;

{ TSedaiModOsc }

constructor TSedaiModOsc.Create;
begin
  inherited Create;
  TypeName := 'osc';
  Rate := mrBoth;
  FShape := osSaw;
  FBaseFreq := 110.0;
  FPhase := 0.0;
  FLastSync := 0.0;
  FPitchIn := AddInput('pitch', prPitch, 0.0);
  FPwIn    := AddInput('pw', prUnipolar, 0.5);
  FSyncIn  := AddInput('sync', prGate, 0.0);
  FOut     := AddOutput('out', prAudio);
end;

function TSedaiModOsc.Configure(const AKey, AValue: string): Boolean;
var
  S: TSedaiOscShape;
begin
  Result := True;
  if SameText(AKey, 'shape') then
  begin
    if ShapeFromName(AValue, S) then FShape := S else Result := False;
  end
  else if SameText(AKey, 'freq') then
    FBaseFreq := StrToFloatDef(AValue, FBaseFreq)
  else
    Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModOsc.ResetState;
begin
  inherited ResetState;
  FPhase := 0.0;
  FLastSync := 0.0;
end;

procedure TSedaiModOsc.RenderSample(AIndex: Integer);
var
  Volts, Freq, Inc_, Sy: Single;
begin
  // 1 volt per octave: the base frequency shifted by 2^volts.
  Volts := FPitchIn.Read(AIndex);
  Freq := FBaseFreq * Power(2.0, Volts);
  if Freq < 0.0 then Freq := 0.0;
  if Freq > FSR * 0.5 then Freq := FSR * 0.5;

  // Hard sync on a rising edge, which only means resetting the phase.
  Sy := FSyncIn.Read(AIndex);
  if (Sy > 0.5) and (FLastSync <= 0.5) then FPhase := 0.0;
  FLastSync := Sy;

  Inc_ := Freq / FSR;
  FPhase := FPhase + Inc_;
  while FPhase >= 1.0 do FPhase := FPhase - 1.0;

  FOut.Write(AIndex, ShapeValue(FShape, FPhase, FPwIn.Read(AIndex)));
end;

{ TSedaiModFilter }

constructor TSedaiModFilter.Create;
begin
  inherited Create;
  TypeName := 'filter';
  Rate := mrBoth;
  FMode := fmLowpass;
  FBaseCutoff := 1000.0;
  FLow := 0.0; FBand := 0.0;
  FIn       := AddInput('in', prAudio, 0.0);
  FCutoffIn := AddInput('cutoff', prPitch, 0.0);
  FResIn    := AddInput('res', prUnipolar, 0.2);
  FOut      := AddOutput('out', prAudio);
end;

function TSedaiModFilter.Configure(const AKey, AValue: string): Boolean;
begin
  Result := True;
  if SameText(AKey, 'mode') then
  begin
    if SameText(AValue, 'lowpass') or SameText(AValue, 'lp') then FMode := fmLowpass
    else if SameText(AValue, 'highpass') or SameText(AValue, 'hp') then FMode := fmHighpass
    else if SameText(AValue, 'bandpass') or SameText(AValue, 'bp') then FMode := fmBandpass
    else Result := False;
  end
  else if SameText(AKey, 'cutoff') then
    FBaseCutoff := StrToFloatDef(AValue, FBaseCutoff)
  else
    Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModFilter.ResetState;
begin
  inherited ResetState;
  FLow := 0.0; FBand := 0.0;
end;

// Chamberlin state-variable filter. Chosen over a biquad because it is stable
// under per-sample modulation and self-oscillates cleanly at high resonance —
// which is exactly the case a feedback patch is built around.
procedure TSedaiModFilter.RenderSample(AIndex: Integer);
var
  Cut, F, Q, High_, X, Res: Single;
begin
  Cut := FBaseCutoff * Power(2.0, FCutoffIn.Read(AIndex));
  if Cut < 10.0 then Cut := 10.0;
  if Cut > FSR * 0.45 then Cut := FSR * 0.45;

  Res := FResIn.Read(AIndex);
  if Res < 0.0 then Res := 0.0;
  if Res > 1.0 then Res := 1.0;
  Q := 1.0 - Res * 0.99;              // 1.0 = no resonance, ~0.01 = screaming

  F := Single(2.0 * Sin(Pi * Cut / FSR));
  X := FIn.Read(AIndex);

  High_ := X - FLow - Q * FBand;
  FBand := FBand + F * High_;
  FLow := FLow + F * FBand;

  // Cheap safety net: a self-oscillating SVF driven hard can run away, and a
  // NaN would poison the whole graph from here on.
  if FLow > 8.0 then FLow := 8.0 else if FLow < -8.0 then FLow := -8.0;
  if FBand > 8.0 then FBand := 8.0 else if FBand < -8.0 then FBand := -8.0;

  case FMode of
    fmLowpass:  FOut.Write(AIndex, FLow);
    fmHighpass: FOut.Write(AIndex, High_);
    fmBandpass: FOut.Write(AIndex, FBand);
  end;
end;

{ TSedaiModAmp }

constructor TSedaiModAmp.Create;
begin
  inherited Create;
  TypeName := 'amp';
  Rate := mrBoth;
  FIn     := AddInput('in', prAudio, 0.0);
  // Default 0, NOT 1. A port's knob value and its patched sources SUM, which is
  // how a real VCA with an "initial gain" knob behaves. With a default of 1 the
  // amplifier could never be closed by an envelope: it would read 1 + env and
  // sit at up to double unity. An unpatched VCA is silent, as it should be.
  FGainIn := AddInput('gain', prUnipolar, 0.0);
  FOut    := AddOutput('out', prAudio);
end;

procedure TSedaiModAmp.RenderSample(AIndex: Integer);
begin
  FOut.Write(AIndex, FIn.Read(AIndex) * FGainIn.Read(AIndex));
end;

{ TSedaiModEnv }

constructor TSedaiModEnv.Create;
begin
  inherited Create;
  TypeName := 'env';
  Rate := mrBoth;
  FA := 0.005; FD := 0.120; FS := 0.7; FR := 0.200;
  FStage := esIdle;
  FLevel := 0.0;
  FGateWasOpen := False;
  FGateIn := AddInput('gate', prGate, 0.0);
  FOut    := AddOutput('out', prUnipolar);
end;

function TSedaiModEnv.Configure(const AKey, AValue: string): Boolean;
begin
  Result := True;
  if SameText(AKey, 'a') then FA := StrToFloatDef(AValue, FA)
  else if SameText(AKey, 'd') then FD := StrToFloatDef(AValue, FD)
  else if SameText(AKey, 's') then FS := StrToFloatDef(AValue, FS)
  else if SameText(AKey, 'r') then FR := StrToFloatDef(AValue, FR)
  else Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModEnv.ResetState;
begin
  inherited ResetState;
  FStage := esIdle;
  FLevel := 0.0;
  FGateWasOpen := False;
end;

function TSedaiModEnv.RateFor(ASeconds: Single): Single;
begin
  if ASeconds <= 0.0 then Result := 1.0
  else Result := 1.0 / (ASeconds * FSR);
end;

procedure TSedaiModEnv.RenderSample(AIndex: Integer);
var
  Gate: Single;
  Open: Boolean;
begin
  Gate := FGateIn.Read(AIndex);
  Open := Gate > 0.5;

  if Open and (not FGateWasOpen) then FStage := esAttack
  else if (not Open) and FGateWasOpen then FStage := esRelease;
  FGateWasOpen := Open;

  case FStage of
    esAttack:
      begin
        FLevel := FLevel + RateFor(FA);
        if FLevel >= 1.0 then begin FLevel := 1.0; FStage := esDecay; end;
      end;
    esDecay:
      begin
        FLevel := FLevel - RateFor(FD) * (1.0 - FS);
        if FLevel <= FS then begin FLevel := FS; FStage := esSustain; end;
      end;
    esSustain:
      FLevel := FS;
    esRelease:
      begin
        FLevel := FLevel - RateFor(FR) * FS;
        if FLevel <= 0.0 then begin FLevel := 0.0; FStage := esIdle; end;
      end;
    esIdle:
      FLevel := 0.0;
  end;

  FOut.Write(AIndex, FLevel);
end;

{ TSedaiModLFO }

constructor TSedaiModLFO.Create;
begin
  inherited Create;
  TypeName := 'lfo';
  Rate := mrBoth;
  FShape := osTriangle;
  FBaseRate := 5.0;
  FPhase := 0.0;
  FRateIn := AddInput('rate', prPitch, 0.0);
  FOut    := AddOutput('out', prBipolar);
end;

function TSedaiModLFO.Configure(const AKey, AValue: string): Boolean;
var
  S: TSedaiOscShape;
begin
  Result := True;
  if SameText(AKey, 'shape') then
  begin
    if ShapeFromName(AValue, S) then FShape := S else Result := False;
  end
  else if SameText(AKey, 'rate') then
    FBaseRate := StrToFloatDef(AValue, FBaseRate)
  else
    Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModLFO.ResetState;
begin
  inherited ResetState;
  FPhase := 0.0;
end;

procedure TSedaiModLFO.RenderSample(AIndex: Integer);
var
  R: Single;
begin
  // Same volts-per-octave rule as the oscillator, so an LFO can modulate an LFO.
  R := FBaseRate * Power(2.0, FRateIn.Read(AIndex));
  if R < 0.0 then R := 0.0;
  if R > FSR * 0.5 then R := FSR * 0.5;
  FPhase := FPhase + R / FSR;
  while FPhase >= 1.0 do FPhase := FPhase - 1.0;
  FOut.Write(AIndex, ShapeValue(FShape, FPhase, 0.5));
end;

{ TSedaiModNote }

constructor TSedaiModNote.Create;
begin
  inherited Create;
  TypeName := 'note';
  Rate := mrBoth;
  FPitch := 0.0;
  FGate := 0.0;
  FPitchOut := AddOutput('pitch', prPitch);
  FGateOut  := AddOutput('gate', prGate);
end;

procedure TSedaiModNote.RenderSample(AIndex: Integer);
begin
  FPitchOut.Write(AIndex, FPitch);
  FGateOut.Write(AIndex, FGate);
end;

procedure TSedaiModNote.SetNote(APitchVolts, AGate: Single);
begin
  FPitch := APitchVolts;
  FGate := AGate;
end;

{ factory }

function CreateModuleByType(const ATypeName: string): TSedaiPatchModule;
begin
  if SameText(ATypeName, 'osc') then Result := TSedaiModOsc.Create
  else if SameText(ATypeName, 'filter') then Result := TSedaiModFilter.Create
  else if SameText(ATypeName, 'amp') then Result := TSedaiModAmp.Create
  else if SameText(ATypeName, 'env') then Result := TSedaiModEnv.Create
  else if SameText(ATypeName, 'lfo') then Result := TSedaiModLFO.Create
  else if SameText(ATypeName, 'note') then Result := TSedaiModNote.Create
  else Result := nil;
end;

function KnownModuleTypes: string;
begin
  Result := 'osc, filter, amp, env, lfo, note';
end;

end.
