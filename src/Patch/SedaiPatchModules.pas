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
    FTri: Single;
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    function ConfigKeys: string; override;
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
    function ConfigKeys: string; override;
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
    function ConfigKeys: string; override;
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
    FStartPhase: Double;
    FShape: TSedaiOscShape;
    FBaseRate: Single;
    FTri: Single;
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    function ConfigKeys: string; override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModDelay — a delay line, and the first module with real latency.
    Its InternalDelay is what lets a feedback loop through it be advanced in
    chunks instead of one sample at a time. }
  TSedaiModDelay = class(TSedaiPatchModule)
  private
    FIn, FOut: TSedaiPatchPort;
    FLine: array of Single;
    FWrite: Integer;
    FLenSamples: Integer;
    FTimeSec: Single;
    procedure Rebuild;
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    function ConfigKeys: string; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
    function InternalDelay: Integer; override;
  end;

  { TSedaiModInput — audio coming IN from the host.

    This is what makes the design's claim true that "FX are the same machine":
    a patch whose source is incoming audio instead of an oscillator is not a
    second architecture, it is the same graph with a different first module. The
    host fills a block before each render; with nothing supplied the module is
    silent, so a patch that expects input and gets none fails audibly rather
    than mysteriously. }
  TSedaiModInput = class(TSedaiPatchModule)
  private
    FOut: TSedaiPatchPort;
    FBlock: array of Single;
    FCount: Integer;
    FChannel: Integer;
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    function ConfigKeys: string; override;
    property Channel: Integer read FChannel;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
    // Called by the host before Render. ACount frames of mono audio.
    procedure SetBlock(AData: PSingle; ACount: Integer);
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
  // Soft-saturation for the filter loop: untouched below SAT_T, then bending
  // smoothly and never exceeding SAT_T + SAT_H. Full scale audio is 1.0, so a
  // threshold of 2 leaves every normal signal bit-for-bit unchanged.
  SAT_T = 2.0;
  SAT_H = 6.0;

// Linear below SAT_T, asymptotic to SAT_T + SAT_H above it.
function Saturate(X: Single): Single;
var
  A: Single;
begin
  // A NaN fails every comparison, so this also catches one and returns 0.
  if (X > -SAT_T) and (X < SAT_T) then Exit(X);
  if not (X > -1.0e30) or not (X < 1.0e30) then Exit(0.0);
  A := Abs(X) - SAT_T;
  A := SAT_T + A / (1.0 + A / SAT_H);
  if X < 0 then Result := -A else Result := A;
end;

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

// PolyBLEP: the correction added around a discontinuity so that a naive saw or
// square stops folding its harmonics back down the spectrum. Two samples wide,
// which is enough to take the worst of the aliasing off without the cost of a
// wavetable or an oversampled path.
function PolyBlep(T, Dt: Double): Double;
begin
  if Dt <= 0.0 then Exit(0.0);
  if T < Dt then
  begin
    T := T / Dt;
    Result := T + T - T * T - 1.0;
  end
  else if T > 1.0 - Dt then
  begin
    T := (T - 1.0) / Dt;
    Result := T * T + T + T + 1.0;
  end
  else
    Result := 0.0;
end;

function WrapPhase(T: Double): Double;
begin
  Result := T;
  while Result >= 1.0 do Result := Result - 1.0;
  while Result < 0.0 do Result := Result + 1.0;
end;

// One shared generator, band-limited. ADt is the phase increment, i.e. the
// normalised frequency, which is what tells the correction how wide to be.
// ATri carries the leaky integrator state used to build a triangle out of the
// band-limited square — integrating a corrected square is what keeps the
// triangle clean too, instead of leaving it as the one aliasing shape.
function ShapeValue(AShape: TSedaiOscShape; APhase, ADt: Double; APw: Single;
                    var ATri: Single): Single;
var
  V: Double;
  Pw: Double;
begin
  case AShape of
    osSaw:
      begin
        V := 2.0 * APhase - 1.0;
        V := V - PolyBlep(APhase, ADt);
        Result := Single(V);
      end;
    osSquare:
      begin
        if APhase < 0.5 then V := 1.0 else V := -1.0;
        V := V + PolyBlep(APhase, ADt) - PolyBlep(WrapPhase(APhase + 0.5), ADt);
        Result := Single(V);
      end;
    osPulse:
      begin
        Pw := APw;
        if Pw < 0.01 then Pw := 0.01 else if Pw > 0.99 then Pw := 0.99;
        if APhase < Pw then V := 1.0 else V := -1.0;
        V := V + PolyBlep(APhase, ADt) - PolyBlep(WrapPhase(APhase - Pw), ADt);
        Result := Single(V);
      end;
    osTriangle:
      begin
        // band-limited square, then a leaky integrator, scaled back to +-1
        if APhase < 0.5 then V := 1.0 else V := -1.0;
        V := V + PolyBlep(APhase, ADt) - PolyBlep(WrapPhase(APhase + 0.5), ADt);
        ATri := Single(ADt * 4.0 * V + (1.0 - ADt * 8.0) * ATri);
        Result := ATri;
      end;
    osSine:
      Result := Single(Sin(TWO_PI * APhase));
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
  FTri := 0.0;
  FPitchIn := AddInput('pitch', prPitch, 0.0);
  FPwIn    := AddInput('pw', prUnipolar, 0.5);
  FPwIn.Min := 0.0; FPwIn.Max := 1.0;
  FSyncIn  := AddInput('sync', prGate, 0.0);
  FOut     := AddOutput('out', prAudio);
end;

function TSedaiModOsc.ConfigKeys: string;
begin
  Result := 'freq, shape';
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
  FTri := 0.0;
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

  FOut.Write(AIndex, ShapeValue(FShape, FPhase, Inc_, FPwIn.Read(AIndex), FTri));
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
  FResIn.Min := 0.0; FResIn.Max := 1.0;
  FOut      := AddOutput('out', prAudio);
end;

function TSedaiModFilter.ConfigKeys: string;
begin
  Result := 'cutoff, mode';
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

  // What bounds a real analogue filter is the amplifier in its loop running out
  // of headroom, not a hard limit on the state. So the state is soft-saturated —
  // but EXACTLY LINEAR below the threshold, which matters more than it sounds:
  // a saturator with no dead zone attenuates a little on every pass, and inside
  // a recursive loop that compounds into a real change of frequency response.
  // A first attempt without the threshold cost 12 dB on an ordinary patch.
  FLow := Saturate(FLow);
  FBand := Saturate(FBand);


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
  FGainIn.Min := 0.0; FGainIn.Max := 16.0;
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

function TSedaiModEnv.ConfigKeys: string;
begin
  Result := 'a, d, r, s';
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
  FStartPhase := 0.0;
  FPhase := 0.0;
  FTri := 0.0;
  FRateIn := AddInput('rate', prPitch, 0.0);
  FOut    := AddOutput('out', prBipolar);
end;

function TSedaiModLFO.ConfigKeys: string;
begin
  Result := 'phase, rate, shape';
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
  else if SameText(AKey, 'phase') then
  begin
    // In turns, 0..1. Two LFOs at the same rate a quarter turn apart are a
    // quadrature pair, which is how a source is made to circle a listener.
    FStartPhase := Frac(StrToFloatDef(AValue, 0.0));
    if FStartPhase < 0.0 then FStartPhase := FStartPhase + 1.0;
    FPhase := FStartPhase;
  end
  else
    Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModLFO.ResetState;
begin
  inherited ResetState;
  FPhase := FStartPhase;
  FTri := 0.0;
end;

procedure TSedaiModLFO.RenderSample(AIndex: Integer);
var
  R, Dt: Single;
begin
  // Same volts-per-octave rule as the oscillator, so an LFO can modulate an LFO.
  R := FBaseRate * Power(2.0, FRateIn.Read(AIndex));
  if R < 0.0 then R := 0.0;
  if R > FSR * 0.5 then R := FSR * 0.5;
  Dt := R / FSR;
  FPhase := FPhase + Dt;
  while FPhase >= 1.0 do FPhase := FPhase - 1.0;
  FOut.Write(AIndex, ShapeValue(FShape, FPhase, Dt, 0.5, FTri));
end;

{ TSedaiModInput }

constructor TSedaiModInput.Create;
begin
  inherited Create;
  TypeName := 'input';
  Rate := mrBoth;
  FCount := 0;
  FChannel := 0;
  FOut := AddOutput('out', prAudio);
end;

// `module l = input channel=0` / `module r = input channel=1`: a patch declares
// how many inputs it wants and which channel each takes, the same way `output`
// lines declare the outputs.
function TSedaiModInput.ConfigKeys: string;
begin
  Result := 'channel';
end;

function TSedaiModInput.Configure(const AKey, AValue: string): Boolean;
begin
  Result := True;
  if SameText(AKey, 'channel') then FChannel := StrToIntDef(AValue, 0)
  else Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModInput.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  inherited Prepare(ASampleRate, ABlockSize);
  if Length(FBlock) < ABlockSize then SetLength(FBlock, ABlockSize);
end;

procedure TSedaiModInput.ResetState;
var
  I: Integer;
begin
  inherited ResetState;
  for I := 0 to High(FBlock) do FBlock[I] := 0.0;
  FCount := 0;
end;

procedure TSedaiModInput.SetBlock(AData: PSingle; ACount: Integer);
var
  I: Integer;
begin
  if ACount > Length(FBlock) then SetLength(FBlock, ACount);
  FCount := ACount;
  if AData = nil then
  begin
    for I := 0 to ACount - 1 do FBlock[I] := 0.0;
    Exit;
  end;
  for I := 0 to ACount - 1 do FBlock[I] := AData[I];
end;

procedure TSedaiModInput.RenderSample(AIndex: Integer);
begin
  if AIndex < FCount then FOut.Write(AIndex, FBlock[AIndex])
                     else FOut.Write(AIndex, 0.0);
end;

{ TSedaiModDelay }

constructor TSedaiModDelay.Create;
begin
  inherited Create;
  TypeName := 'delay';
  Rate := mrBoth;
  FTimeSec := 0.010;
  FLenSamples := 441;
  FWrite := 0;
  FIn  := AddInput('in', prAudio, 0.0);
  FOut := AddOutput('out', prAudio);
end;

function TSedaiModDelay.ConfigKeys: string;
begin
  Result := 'time';
end;

function TSedaiModDelay.Configure(const AKey, AValue: string): Boolean;
begin
  Result := True;
  if SameText(AKey, 'time') then
  begin
    FTimeSec := StrToFloatDef(AValue, FTimeSec);
    Rebuild;
  end
  else
    Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModDelay.Rebuild;
begin
  // The delay time is fixed at load: InternalDelay has to be a constant the
  // scheduler can trust when it sizes a cycle's chunk. A modulated delay would
  // have to report its MINIMUM length instead, which is a P2 problem.
  FLenSamples := Round(FTimeSec * FSR);
  if FLenSamples < 1 then FLenSamples := 1;
  SetLength(FLine, FLenSamples);
  ResetState;
end;

procedure TSedaiModDelay.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  inherited Prepare(ASampleRate, ABlockSize);
  Rebuild;
end;

procedure TSedaiModDelay.ResetState;
var
  I: Integer;
begin
  inherited ResetState;
  for I := 0 to High(FLine) do FLine[I] := 0.0;
  FWrite := 0;
end;

function TSedaiModDelay.InternalDelay: Integer;
begin
  Result := FLenSamples;
end;

procedure TSedaiModDelay.RenderSample(AIndex: Integer);
var
  Old: Single;
begin
  Old := FLine[FWrite];                  // oldest sample: FLenSamples ago
  FLine[FWrite] := FIn.Read(AIndex);
  Inc(FWrite);
  if FWrite >= FLenSamples then FWrite := 0;
  FOut.Write(AIndex, Old);
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
  else if SameText(ATypeName, 'input') then Result := TSedaiModInput.Create
  else if SameText(ATypeName, 'delay') then Result := TSedaiModDelay.Create
  else if SameText(ATypeName, 'note') then Result := TSedaiModNote.Create
  else Result := nil;
end;

function KnownModuleTypes: string;
begin
  Result := 'osc, filter, amp, env, lfo, delay, input, note';
end;

end.
