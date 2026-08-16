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
  SysUtils, Math, SedaiAudioTypes, SedaiPatchGraph, SedaiNoiseGenerator;

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
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
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

function CreateElectronicModuleByType(const ATypeName: string): TSedaiPatchModule;
begin
  if SameText(ATypeName, 'seq') then Result := TSedaiModSeq.Create
  else if SameText(ATypeName, 'sh') then Result := TSedaiModSampleHold.Create
  else if SameText(ATypeName, 'ring') then Result := TSedaiModRing.Create
  else if SameText(ATypeName, 'glide') then Result := TSedaiModGlide.Create
  else if SameText(ATypeName, 'noise') then Result := TSedaiModNoise.Create
  else Result := nil;
end;

function KnownElectronicTypes: string;
begin
  Result := 'seq, sh, ring, glide, noise';
end;

end.
