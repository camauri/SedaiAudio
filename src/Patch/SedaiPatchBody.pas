// ============================================================================
// SedaiPatchBody — the body stage: what the sound is radiating THROUGH.
//
// These came out of the workbench work (source -> body -> radiation): a plate,
// a tube, a violin's box are not effects applied to a finished sound, they are
// the thing that turns an excitation into an instrument. Both units here expose
// ProcessSample, so unlike the block-oriented bridge they are native modules and
// may sit inside a feedback cycle — which matters here more than elsewhere,
// because an excitation feeding a body that feeds back into the excitation IS
// the physical model.
//
//   formant  a formant body: violin, viola, cello, sax, clarinet
//   tube     a resonant tube, full or odd harmonics, tuned in Hz
//
// The tube's frequency is a PORT, so the body can be tuned while it sounds.
// That is not physical for a real instrument and is exactly the kind of thing
// the pioneers did the moment they had a patch cord in hand.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchBody;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, SedaiAudioTypes, SedaiPatchGraph, SedaiFormantBody,
  SedaiTubeResonator;

type
  { TSedaiModFormant }

  TSedaiModFormant = class(TSedaiPatchModule)
  private
    FBody: TSedaiFormantBody;
    FIn, FMixIn, FOut: TSedaiPatchPort;
    FKind: TFormantBodyKind;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    function Configure(const AKey, AValue: string): Boolean; override;
    function ConfigKeys: string; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModTube }

  TSedaiModTube = class(TSedaiPatchModule)
  private
    FTube: TSedaiTubeResonator;
    FIn, FFreqIn, FResIn, FMixIn, FOut: TSedaiPatchPort;
    FLastFreq, FLastRes: Single;
    FMode: TTubeMode;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    function Configure(const AKey, AValue: string): Boolean; override;
    function ConfigKeys: string; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

function CreateBodyModuleByType(const ATypeName: string): TSedaiPatchModule;
function KnownBodyTypes: string;

implementation

{ TSedaiModFormant }

constructor TSedaiModFormant.Create;
begin
  inherited Create;
  TypeName := 'formant';
  Rate := mrBoth;
  FKind := fbViolin;
  FBody := TSedaiFormantBody.Create;
  FIn    := AddInput('in', prAudio, 0.0);
  FMixIn := AddInput('mix', prUnipolar, 1.0);
  FMixIn.Min := 0.0; FMixIn.Max := 1.0;
  FOut   := AddOutput('out', prAudio);
end;

destructor TSedaiModFormant.Destroy;
begin
  FBody.Free;
  inherited Destroy;
end;

procedure TSedaiModFormant.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  inherited Prepare(ASampleRate, ABlockSize);
  FBody.SetSampleRate(ASampleRate);
  FBody.SetBody(FKind);
  // The unit has a dry/wet of its own and it starts dry. Drive it fully wet and
  // let the module's `mix` port be the single place the blend happens, the same
  // convention the block bridge uses — otherwise two mix controls multiply and
  // the module reads as a pass-through.
  FBody.SetMix(1.0);
end;

function TSedaiModFormant.ConfigKeys: string;
begin
  Result := 'body, kind';
end;

function TSedaiModFormant.Configure(const AKey, AValue: string): Boolean;
var
  V: string;
begin
  if SameText(AKey, 'kind') or SameText(AKey, 'body') then
  begin
    V := LowerCase(Trim(AValue));
    if V = 'none' then FKind := fbNone
    else if V = 'violin' then FKind := fbViolin
    else if V = 'viola' then FKind := fbViola
    else if V = 'cello' then FKind := fbCello
    else if V = 'sax' then FKind := fbSax
    else if V = 'clarinet' then FKind := fbClarinet
    else raise Exception.CreateFmt('unknown body "%s" — one of: none, violin, '
      + 'viola, cello, sax, clarinet', [AValue]);
    FBody.SetBody(FKind);
    Exit(True);
  end;
  Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModFormant.RenderSample(AIndex: Integer);
var
  Dry, Wet, Mix: Single;
begin
  Dry := FIn.Read(AIndex);
  Wet := FBody.ProcessSample(Dry);
  Mix := FMixIn.Read(AIndex);
  if Mix < 0.0 then Mix := 0.0 else if Mix > 1.0 then Mix := 1.0;
  FOut.Write(AIndex, Dry * (1.0 - Mix) + Wet * Mix);
end;

{ TSedaiModTube }

constructor TSedaiModTube.Create;
begin
  inherited Create;
  TypeName := 'tube';
  Rate := mrBoth;
  FMode := tmFull;
  FLastFreq := -1.0;
  FLastRes := -1.0;
  FTube := TSedaiTubeResonator.Create;
  FIn     := AddInput('in', prAudio, 0.0);
  FFreqIn := AddInput('freq', prUnipolar, 220.0);
  FFreqIn.Min := 20.0; FFreqIn.Max := 8000.0;
  FResIn  := AddInput('res', prUnipolar, 0.9);
  FResIn.Min := 0.0; FResIn.Max := 0.999;
  FMixIn  := AddInput('mix', prUnipolar, 1.0);
  FMixIn.Min := 0.0; FMixIn.Max := 1.0;
  FOut    := AddOutput('out', prAudio);
end;

destructor TSedaiModTube.Destroy;
begin
  FTube.Free;
  inherited Destroy;
end;

procedure TSedaiModTube.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  inherited Prepare(ASampleRate, ABlockSize);
  FTube.SetSampleRate(ASampleRate);
  FTube.SetMode(FMode);
  FTube.SetMix(1.0);          // see the note in TSedaiModFormant.Prepare
  FLastFreq := -1.0;
  FLastRes := -1.0;
end;

function TSedaiModTube.ConfigKeys: string;
begin
  Result := 'mode';
end;

function TSedaiModTube.Configure(const AKey, AValue: string): Boolean;
var
  V: string;
begin
  if SameText(AKey, 'mode') then
  begin
    V := LowerCase(Trim(AValue));
    if V = 'full' then FMode := tmFull
    else if V = 'odd' then FMode := tmOdd
    else raise Exception.CreateFmt('unknown tube mode "%s" — full or odd', [AValue]);
    FTube.SetMode(FMode);
    Exit(True);
  end;
  Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModTube.RenderSample(AIndex: Integer);
var
  Dry, Wet, Mix, F, R: Single;
begin
  // Only retune when the value actually moved: recomputing the delay length
  // every sample would be wasted work on the common case of a fixed body.
  F := FFreqIn.Read(AIndex);
  if F < 20.0 then F := 20.0 else if F > FSR * 0.45 then F := FSR * 0.45;
  if F <> FLastFreq then
  begin
    FTube.SetFrequency(F);
    FLastFreq := F;
  end;
  R := FResIn.Read(AIndex);
  if R < 0.0 then R := 0.0 else if R > 0.999 then R := 0.999;
  if R <> FLastRes then
  begin
    FTube.SetResonance(R);
    FLastRes := R;
  end;

  Dry := FIn.Read(AIndex);
  Wet := FTube.ProcessSample(Dry);
  Mix := FMixIn.Read(AIndex);
  if Mix < 0.0 then Mix := 0.0 else if Mix > 1.0 then Mix := 1.0;
  FOut.Write(AIndex, Dry * (1.0 - Mix) + Wet * Mix);
end;

{ factory }

function CreateBodyModuleByType(const ATypeName: string): TSedaiPatchModule;
begin
  if SameText(ATypeName, 'formant') then Result := TSedaiModFormant.Create
  else if SameText(ATypeName, 'tube') then Result := TSedaiModTube.Create
  else Result := nil;
end;

function KnownBodyTypes: string;
begin
  Result := 'formant, tube';
end;

end.
