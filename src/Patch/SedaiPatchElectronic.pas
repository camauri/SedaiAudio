// ============================================================================
// SedaiPatchElectronic — the modules that make it sound like that music.
//
// Aimed squarely at the lineage the workbench was asked for: Jarre, Kraftwerk,
// Moroder, early Battiato, Eno. What separates those records from a generic
// subtractive synth is not the filter — it is the SEQUENCER driving everything,
// random voltages from sample-and-hold, ring modulation for inharmonic metal,
// and portamento. So those are what is here.
//
// All sample-first, so any of them can sit inside a feedback cycle.
//
// NOT here, deliberately, because the port model already provides them:
//
//   attenuverter   a connection carries `amount`, and it may be negative, so
//                  inverting or scaling a modulation is already a patch-level
//                  act rather than a module.
//   CV mixer       several connections into one input sum, and the input's own
//                  value is the offset. That is an adder with an offset.
//   FM             patch an oscillator's output into another's pitch and raise
//                  its frequency. Exponential FM, which is exactly what the
//                  analogue instruments of that period actually did.
//
// This is Serge's patch-programmability argument: fewer module types with more
// inputs beats a catalogue of specialised boxes.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchElectronic;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, SedaiAudioTypes, SedaiPatchGraph, SedaiNoiseGenerator, SedaiRandom;

const
  SEDAI_SEQ_MAX_STEPS = 32;

type
  { TSedaiModSeq — a clock-driven step sequencer.

    Clock-driven rather than self-timed on purpose: that is what makes it a
    modular sequencer instead of an arpeggiator. Drive it from a square LFO and
    the LFO's rate is the tempo; drive it from another sequencer's gate and you
    get the polyrhythms those records are built on. }
  TSedaiModSeq = class(TSedaiPatchModule)
  private
    FClockIn, FResetIn, FCvOut, FGateOut: TSedaiPatchPort;
    FValues: array[0..SEDAI_SEQ_MAX_STEPS-1] of Single;
    FGates: array[0..SEDAI_SEQ_MAX_STEPS-1] of Boolean;
    FCount, FStep: Integer;
    FLastClock, FLastReset: Single;
    FGateSamples, FGateLen: Integer;
    function ParseList(const AText: string; AIsGate: Boolean): Boolean;
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    function ConfigKeys: string; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModSampleHold — the burbling random voltage.

    Noise into `in`, a clock into `trig`, and out comes a new random level on
    every edge. Jarre's signature, and the reason a sequencer and a noise source
    are worth having in the same box. }
  TSedaiModSampleHold = class(TSedaiPatchModule)
  private
    FIn, FTrigIn, FOut: TSedaiPatchPort;
    FHeld, FLastTrig: Single;
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModRing — a multiplier.

    Two audio signals multiplied give sum and difference frequencies and nothing
    else, so the result is inharmonic: bells, gongs, robot voices. With a
    unipolar control signal on one side it is also just a VCA, which is the same
    circuit doing a different job depending on what you patch into it. }
  TSedaiModRing = class(TSedaiPatchModule)
  private
    FA, FB, FOut: TSedaiPatchPort;
  public
    constructor Create; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModGlide — portamento, i.e. a slew limiter.

    The sliding bass line. Also useful on any control voltage: put it after a
    sample-and-hold and the random steps become random glides. }
  TSedaiModGlide = class(TSedaiPatchModule)
  private
    FIn, FTimeIn, FOut: TSedaiPatchPort;
    FState: Single;
    FStarted: Boolean;
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModNoise — wraps SAF's own noise generator.

    White, pink, brown, blue and violet were all written long ago and no patch
    could reach them. It has a per-sample entry point, so unlike the block
    oriented effects this one is mrBoth and may sit inside a feedback cycle. }
  TSedaiModNoise = class(TSedaiPatchModule)
  private
    FOut: TSedaiPatchPort;
    FGen: TSedaiNoiseGenerator;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    function ConfigKeys: string; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;


  { TSedaiModQuant — a pitch quantiser.

    The sequencer and the sample-and-hold both put arbitrary voltages on a
    pitch input, and arbitrary voltages are out of tune. A quantiser snaps to
    the nearest degree of a scale, which is what turns a random voltage from a
    noise source into a melody rather than a warble. }

  TSedaiModQuant = class(TSedaiPatchModule)
  private
    FIn, FOut: TSedaiPatchPort;
    FDegrees: array of Integer;   // semitones within an octave, ascending
    procedure SetScale(const AName: string);
  public
    constructor Create; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    function ConfigKeys: string; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModFollow — an envelope follower.

    Turns any signal into a control voltage tracking its loudness. It is the
    module that lets one sound steer another: a drum part opening the filter on
    a pad is a follower on the drums patched to the pad's cutoff. Attack and
    release are separate because a follower that rises as slowly as it falls is
    useless for anything percussive. }

  TSedaiModFollow = class(TSedaiPatchModule)
  private
    FIn, FAttIn, FRelIn, FOut: TSedaiPatchPort;
    FLevel: Single;
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModFold — a wavefolder.

    West coast rather than east coast: instead of removing harmonics with a
    filter, it ADDS them by reflecting the signal back on itself every time it
    passes a threshold. Drive it from an envelope and the timbre opens as the
    note gets louder, which is how a Buchla gets brighter without a filter. }

  TSedaiModFold = class(TSedaiPatchModule)
  private
    FIn, FFoldIn, FSymIn, FOut: TSedaiPatchPort;
  public
    constructor Create; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModLPG — a low-pass gate.

    The Buchla 292: one control opens BOTH the amplitude and the brightness at
    once, through a vactrol whose sluggishness is the whole character. A quiet
    note is also a dull note, which is what a struck physical object does and
    what a VCA alone never does. `resp` is the vactrol lag in seconds. }

  TSedaiModLPG = class(TSedaiPatchModule)
  private
    FIn, FCVIn, FRespIn, FOut: TSedaiPatchPort;
    FLag: Single;
    FZ1: Single;
  public
    constructor Create; override;
    procedure ResetState; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

function CreateElectronicModuleByType(const ATypeName: string): TSedaiPatchModule;
function KnownElectronicTypes: string;

implementation

{ TSedaiModSeq }

constructor TSedaiModSeq.Create;
var
  I: Integer;
begin
  inherited Create;
  TypeName := 'seq';
  Rate := mrBoth;
  FCount := 8;
  // Parked on the LAST step so the FIRST clock advances onto step 0. Starting
  // at 0 made the first clock play step 1 and shifted the whole sequence by
  // one, which is exactly what hardware sequencers do not do.
  FStep := SEDAI_SEQ_MAX_STEPS - 1;
  FLastClock := 0.0;
  FLastReset := 0.0;
  FGateLen := 1;
  FGateSamples := 0;
  for I := 0 to SEDAI_SEQ_MAX_STEPS - 1 do
  begin
    FValues[I] := 0.0;
    FGates[I] := True;
  end;
  FClockIn := AddInput('clock', prGate, 0.0);
  FResetIn := AddInput('reset', prGate, 0.0);
  FCvOut   := AddOutput('out', prPitch);
  FGateOut := AddOutput('gate', prGate);
end;

// values=0,3,7,12  or  gates=1,1,0,1
function TSedaiModSeq.ParseList(const AText: string; AIsGate: Boolean): Boolean;
var
  Parts: TStringArray;
  I: Integer;
  V: Single;
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Parts := AText.Split([','], TStringSplitOptions.ExcludeEmpty);
  if Length(Parts) = 0 then Exit(False);
  if Length(Parts) > SEDAI_SEQ_MAX_STEPS then SetLength(Parts, SEDAI_SEQ_MAX_STEPS);
  for I := 0 to High(Parts) do
  begin
    if not TryStrToFloat(Trim(Parts[I]), V, FS) then Exit(False);
    if AIsGate then FGates[I] := V >= 0.5
               else FValues[I] := V;
  end;
  FCount := Length(Parts);
  FStep := FCount - 1;
  Result := True;
end;

function TSedaiModSeq.ConfigKeys: string;
begin
  Result := 'gatems, gates, steps, values';
end;

function TSedaiModSeq.Configure(const AKey, AValue: string): Boolean;
var
  N: Integer;
begin
  Result := True;
  if SameText(AKey, 'values') then
    // Semitones, converted to volts per octave, so a sequence is written the way
    // it is thought about: 0,3,7,12 is a minor arpeggio.
    Result := ParseList(AValue, False)
  else if SameText(AKey, 'gates') then
    Result := ParseList(AValue, True)
  else if SameText(AKey, 'steps') then
  begin
    N := StrToIntDef(AValue, FCount);
    if (N >= 1) and (N <= SEDAI_SEQ_MAX_STEPS) then
    begin FCount := N; FStep := FCount - 1; end
    else Result := False;
  end
  else if SameText(AKey, 'gatems') then
    FGateLen := Max(1, Round(StrToFloatDef(AValue, 10.0) * 0.001 * FSR))
  else
    Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModSeq.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  inherited Prepare(ASampleRate, ABlockSize);
  if FGateLen <= 1 then FGateLen := Max(1, Round(0.010 * FSR));   // 10 ms default
end;

procedure TSedaiModSeq.ResetState;
begin
  inherited ResetState;
  FStep := FCount - 1;
  FLastClock := 0.0;
  FLastReset := 0.0;
  FGateSamples := 0;
end;

procedure TSedaiModSeq.RenderSample(AIndex: Integer);
var
  Clk, Rst: Single;
begin
  Rst := FResetIn.Read(AIndex);
  if (Rst > 0.5) and (FLastReset <= 0.5) then FStep := 0;
  FLastReset := Rst;

  Clk := FClockIn.Read(AIndex);
  if (Clk > 0.5) and (FLastClock <= 0.5) then
  begin
    Inc(FStep);
    if FStep >= FCount then FStep := 0;
    if FGates[FStep] then FGateSamples := FGateLen;
  end;
  FLastClock := Clk;

  FCvOut.Write(AIndex, FValues[FStep] / 12.0);   // semitones -> volts per octave
  if FGateSamples > 0 then
  begin
    Dec(FGateSamples);
    FGateOut.Write(AIndex, 1.0);
  end
  else
    FGateOut.Write(AIndex, 0.0);
end;

{ TSedaiModSampleHold }

constructor TSedaiModSampleHold.Create;
begin
  inherited Create;
  TypeName := 'sh';
  Rate := mrBoth;
  FHeld := 0.0;
  FLastTrig := 0.0;
  FIn     := AddInput('in', prBipolar, 0.0);
  FTrigIn := AddInput('trig', prGate, 0.0);
  FOut    := AddOutput('out', prBipolar);
end;

procedure TSedaiModSampleHold.ResetState;
begin
  inherited ResetState;
  FHeld := 0.0;
  FLastTrig := 0.0;
end;

procedure TSedaiModSampleHold.RenderSample(AIndex: Integer);
var
  T: Single;
begin
  T := FTrigIn.Read(AIndex);
  if (T > 0.5) and (FLastTrig <= 0.5) then FHeld := FIn.Read(AIndex);
  FLastTrig := T;
  FOut.Write(AIndex, FHeld);
end;

{ TSedaiModRing }

constructor TSedaiModRing.Create;
begin
  inherited Create;
  TypeName := 'ring';
  Rate := mrBoth;
  FA   := AddInput('a', prAudio, 0.0);
  FB   := AddInput('b', prAudio, 0.0);
  FOut := AddOutput('out', prAudio);
end;

procedure TSedaiModRing.RenderSample(AIndex: Integer);
begin
  FOut.Write(AIndex, FA.Read(AIndex) * FB.Read(AIndex));
end;

{ TSedaiModGlide }

constructor TSedaiModGlide.Create;
begin
  inherited Create;
  TypeName := 'glide';
  Rate := mrBoth;
  FState := 0.0;
  FStarted := False;
  FIn     := AddInput('in', prPitch, 0.0);
  FTimeIn := AddInput('time', prUnipolar, 0.08);   // seconds to cover the step
  FTimeIn.Min := 0.0; FTimeIn.Max := 10.0;
  FTimeIn.UnitLabel := 's';
  FOut    := AddOutput('out', prPitch);
end;

procedure TSedaiModGlide.ResetState;
begin
  inherited ResetState;
  FState := 0.0;
  FStarted := False;
end;

procedure TSedaiModGlide.RenderSample(AIndex: Integer);
var
  Target, T, Coeff: Single;
begin
  Target := FIn.Read(AIndex);
  if not FStarted then
  begin
    // Jump to the first value rather than sliding up from zero, which would put
    // an unwanted swoop on the very first note of every patch.
    FState := Target;
    FStarted := True;
  end;
  T := FTimeIn.Read(AIndex);
  if T <= 0.0 then
    FState := Target
  else
  begin
    // One-pole towards the target; T is roughly the time to cover the step.
    Coeff := 1.0 - Exp(-1.0 / (T * FSR));
    FState := FState + (Target - FState) * Coeff;
  end;
  FOut.Write(AIndex, FState);
end;

{ TSedaiModNoise }

constructor TSedaiModNoise.Create;
begin
  inherited Create;
  TypeName := 'noise';
  Rate := mrBoth;
  FGen := TSedaiNoiseGenerator.Create;
  FOut := AddOutput('out', prAudio);
end;

destructor TSedaiModNoise.Destroy;
begin
  FGen.Free;
  inherited Destroy;
end;

function TSedaiModNoise.ConfigKeys: string;
begin
  Result := 'color, seed, type';
end;

function TSedaiModNoise.Configure(const AKey, AValue: string): Boolean;
begin
  Result := True;
  if SameText(AKey, 'color') or SameText(AKey, 'type') then
  begin
    if SameText(AValue, 'white') then FGen.NoiseType := ntWhite
    else if SameText(AValue, 'pink') then FGen.NoiseType := ntPink
    else if SameText(AValue, 'brown') then FGen.NoiseType := ntBrown
    else if SameText(AValue, 'blue') then FGen.NoiseType := ntBlue
    else if SameText(AValue, 'violet') then FGen.NoiseType := ntViolet
    else Result := False;
  end
  else if SameText(AKey, 'seed') then
    FGen.SetSeed(StrToIntDef(AValue, 1))
  else
    Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModNoise.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  // Seeded from the module's OWN NAME, so this patch's noise depends on this
  // patch and on nothing else. Seeding from the global dispenser made the sound
  // depend on how many other things had been built first — measured: adding one
  // patch to the library changed the sound of the ones rendered after it.
  FGen.SetSeed(Cardinal(SedaiSeedFromName('noise.' + ModuleName)));
  inherited Prepare(ASampleRate, ABlockSize);
  FGen.SetSampleRate(ASampleRate);
end;

procedure TSedaiModNoise.ResetState;
begin
  inherited ResetState;
  FGen.Reset;
end;

procedure TSedaiModNoise.RenderSample(AIndex: Integer);
begin
  FOut.Write(AIndex, FGen.GenerateSample);
end;

{ factory }


{ TSedaiModQuant }

constructor TSedaiModQuant.Create;
begin
  inherited Create;
  TypeName := 'quant';
  Rate := mrBoth;
  SetScale('chromatic');
  FIn  := AddInput('in', prPitch, 0.0);
  FOut := AddOutput('out', prPitch);
end;

procedure TSedaiModQuant.SetScale(const AName: string);

  procedure Use(const A: array of Integer);
  var
    I: Integer;
  begin
    SetLength(FDegrees, Length(A));
    for I := 0 to High(A) do FDegrees[I] := A[I];
  end;

var
  N: string;
begin
  N := LowerCase(Trim(AName));
  if N = 'chromatic' then Use([0,1,2,3,4,5,6,7,8,9,10,11])
  else if N = 'major' then Use([0,2,4,5,7,9,11])
  else if N = 'minor' then Use([0,2,3,5,7,8,10])
  else if N = 'dorian' then Use([0,2,3,5,7,9,10])
  else if N = 'phrygian' then Use([0,1,3,5,7,8,10])
  else if N = 'pentatonic' then Use([0,2,4,7,9])
  else if N = 'minorpent' then Use([0,3,5,7,10])
  else if N = 'blues' then Use([0,3,5,6,7,10])
  else if N = 'whole' then Use([0,2,4,6,8,10])
  else if N = 'octave' then Use([0])
  else if N = 'fifth' then Use([0,7])
  else raise Exception.CreateFmt('unknown scale "%s" — one of: chromatic, '
    + 'major, minor, dorian, phrygian, pentatonic, minorpent, blues, whole, '
    + 'octave, fifth', [AName]);
end;

function TSedaiModQuant.ConfigKeys: string;
begin
  Result := 'scale';
end;

function TSedaiModQuant.Configure(const AKey, AValue: string): Boolean;
begin
  if SameText(AKey, 'scale') then
  begin
    SetScale(AValue);
    Exit(True);
  end;
  Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModQuant.RenderSample(AIndex: Integer);
var
  V, Semis, Best, D, BestD: Single;
  Oct, I: Integer;
begin
  // Volts per octave in, volts per octave out: quantising happens in semitones
  // because that is the unit a scale is written in, and the octave is carried
  // separately so the scale repeats correctly below zero as well as above.
  V := FIn.Read(AIndex);
  Semis := V * 12.0;
  Oct := Floor(Semis / 12.0);
  Semis := Semis - Oct * 12.0;

  Best := FDegrees[0];
  BestD := Abs(Semis - Best);
  for I := 1 to High(FDegrees) do
  begin
    D := Abs(Semis - FDegrees[I]);
    if D < BestD then begin BestD := D; Best := FDegrees[I]; end;
  end;
  // The octave above closes the scale: without it every note in the top gap
  // snaps down instead of up to the tonic.
  if Abs(Semis - 12.0) < BestD then Best := 12.0;

  FOut.Write(AIndex, (Oct * 12.0 + Best) / 12.0);
end;

{ TSedaiModFollow }

constructor TSedaiModFollow.Create;
begin
  inherited Create;
  TypeName := 'follow';
  Rate := mrBoth;
  FLevel := 0.0;
  FIn    := AddInput('in', prAudio, 0.0);
  FAttIn := AddInput('attack', prUnipolar, 0.005);
  FAttIn.Min := 0.0001; FAttIn.Max := 2.0;
  FRelIn := AddInput('release', prUnipolar, 0.120);
  FRelIn.Min := 0.0001; FRelIn.Max := 8.0;
  FOut   := AddOutput('out', prUnipolar);
end;

procedure TSedaiModFollow.ResetState;
begin
  inherited ResetState;
  FLevel := 0.0;
end;

procedure TSedaiModFollow.RenderSample(AIndex: Integer);
var
  X, T, C: Single;
begin
  X := Abs(FIn.Read(AIndex));
  // Separate coefficients: a follower that falls as slowly as it rises smears
  // every transient it was supposed to detect.
  if X > FLevel then T := FAttIn.Read(AIndex) else T := FRelIn.Read(AIndex);
  if T < 0.0001 then T := 0.0001;
  C := 1.0 - Exp(-1.0 / (T * FSR));
  FLevel := FLevel + C * (X - FLevel);
  FOut.Write(AIndex, FLevel);
end;

{ TSedaiModFold }

constructor TSedaiModFold.Create;
begin
  inherited Create;
  TypeName := 'fold';
  Rate := mrBoth;
  FIn    := AddInput('in', prAudio, 0.0);
  FFoldIn := AddInput('fold', prUnipolar, 1.0);
  FFoldIn.Min := 0.0; FFoldIn.Max := 16.0;
  FSymIn := AddInput('sym', prBipolar, 0.0);
  FSymIn.Min := -1.0; FSymIn.Max := 1.0;
  FOut   := AddOutput('out', prAudio);
end;

procedure TSedaiModFold.RenderSample(AIndex: Integer);
var
  X: Single;
  Guard: Integer;
begin
  X := FIn.Read(AIndex) * FFoldIn.Read(AIndex) + FSymIn.Read(AIndex);
  // Reflect about +-1 until the value is back inside. Bounded rather than
  // while(true): a runaway drive would otherwise spin here forever, and a
  // signal that needs more than 32 reflections is already past anything useful.
  Guard := 0;
  while ((X > 1.0) or (X < -1.0)) and (Guard < 32) do
  begin
    if X > 1.0 then X := 2.0 - X else X := -2.0 - X;
    Inc(Guard);
  end;
  if X > 1.0 then X := 1.0 else if X < -1.0 then X := -1.0;
  FOut.Write(AIndex, X);
end;

{ TSedaiModLPG }

constructor TSedaiModLPG.Create;
begin
  inherited Create;
  TypeName := 'lpg';
  Rate := mrBoth;
  FLag := 0.0;
  FZ1 := 0.0;
  FIn     := AddInput('in', prAudio, 0.0);
  FCVIn   := AddInput('cv', prUnipolar, 0.0);
  FCVIn.Min := 0.0; FCVIn.Max := 1.0;
  FRespIn := AddInput('resp', prUnipolar, 0.020);
  FRespIn.Min := 0.0; FRespIn.Max := 1.0;
  FOut    := AddOutput('out', prAudio);
end;

procedure TSedaiModLPG.ResetState;
begin
  inherited ResetState;
  FLag := 0.0;
  FZ1 := 0.0;
end;

procedure TSedaiModLPG.RenderSample(AIndex: Integer);
var
  CV, T, C, Cut, G, A: Single;
begin
  CV := FCVIn.Read(AIndex);
  if CV < 0.0 then CV := 0.0 else if CV > 1.0 then CV := 1.0;

  // The vactrol. Its lag is not a defect to be minimised — it is the reason a
  // low-pass gate sounds like something being struck rather than switched.
  T := FRespIn.Read(AIndex);
  if T < 0.0001 then FLag := CV
  else
  begin
    C := 1.0 - Exp(-1.0 / (T * FSR));
    FLag := FLag + C * (CV - FLag);
  end;

  // One control, both parameters: quieter is also duller, which is what a
  // struck object does and what a VCA on its own never does.
  Cut := 40.0 + FLag * FLag * 9000.0;
  if Cut > FSR * 0.45 then Cut := FSR * 0.45;
  A := 1.0 - Exp(-2.0 * Pi * Cut / FSR);
  FZ1 := FZ1 + A * (FIn.Read(AIndex) - FZ1);

  G := FLag * FLag;
  FOut.Write(AIndex, FZ1 * G);
end;

function CreateElectronicModuleByType(const ATypeName: string): TSedaiPatchModule;
begin
  if SameText(ATypeName, 'seq') then Result := TSedaiModSeq.Create
  else if SameText(ATypeName, 'sh') then Result := TSedaiModSampleHold.Create
  else if SameText(ATypeName, 'ring') then Result := TSedaiModRing.Create
  else if SameText(ATypeName, 'glide') then Result := TSedaiModGlide.Create
  else if SameText(ATypeName, 'noise') then Result := TSedaiModNoise.Create
  else if SameText(ATypeName, 'quant') then Result := TSedaiModQuant.Create
  else if SameText(ATypeName, 'follow') then Result := TSedaiModFollow.Create
  else if SameText(ATypeName, 'fold') then Result := TSedaiModFold.Create
  else if SameText(ATypeName, 'lpg') then Result := TSedaiModLPG.Create
  else Result := nil;
end;

function KnownElectronicTypes: string;
begin
  Result := 'seq, sh, ring, glide, noise, quant, follow, fold, lpg';
end;

end.
