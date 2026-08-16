// ============================================================================
// SedaiPatchLegacy — the bridge that makes SAF's existing DSP patchable.
//
// SAF already carries 22 block-oriented units — delay, chorus, flanger, phaser,
// reverb, convolver, compressor, limiter, EQ, distortion and the rest — all of
// them descendants of TSedaiSignalNode with a ProcessBlock. This wraps them as
// patch modules so a patch can reach them, WITHOUT changing a single signature
// in any of them. That was the whole promise of extending the existing
// architecture rather than growing a parallel one.
//
// Two things the wrapper has to reconcile:
//
//   * The legacy contract is INTERLEAVED STEREO; a patch port carries one mono
//     signal. So the wrapper fans the input out to both channels, calls the
//     unit, and takes the left channel back. A stereo-aware bridge is a later
//     job, when ports themselves grow a channel count.
//
//   * The legacy contract is BLOCK-ORIENTED, so every wrapped unit declares
//     itself mrBlockOnly. The graph then refuses to put one inside a feedback
//     cycle, loudly and at compile time, instead of driving it a sample at a
//     time — which measured 29% slower and is the reason that path was made
//     unreachable rather than merely discouraged.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchLegacy;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SedaiAudioTypes, SedaiSignalNode, SedaiPatchGraph,
  SedaiDelay, SedaiChorus, SedaiFlanger, SedaiPhaser, SedaiReverb,
  SedaiCompressor, SedaiLimiter, SedaiDistortion, SedaiEQ;

type
  { TSedaiLegacyModule }

  TSedaiLegacyModule = class(TSedaiPatchModule)
  private
    FUnit: TSedaiSignalNode;
    FIn, FInR, FMixIn, FOut, FOutR: TSedaiPatchPort;
    FScratchIn, FScratchOut: array of Single;
  protected
    // Subclasses build their unit here and answer Configure keys.
    function CreateUnit: TSedaiSignalNode; virtual; abstract;
    function ConfigureUnit(const AKey, AValue: string; AFloat: Single): Boolean; virtual;
    property WrappedUnit: TSedaiSignalNode read FUnit;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    procedure ResetState; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    // Never called: the graph refuses a block-only module inside a cycle, which
    // is the only situation that would ask for it. Raising beats returning
    // silence if that guarantee ever breaks.
    procedure RenderSample(AIndex: Integer); override;
    procedure RenderBlock(ACount: Integer); override;
  end;

  TSedaiLegacyDelay = class(TSedaiLegacyModule)
  protected
    function CreateUnit: TSedaiSignalNode; override;
    function ConfigureUnit(const AKey, AValue: string; AFloat: Single): Boolean; override;
  public
    constructor Create; override;
  end;

  TSedaiLegacyChorus = class(TSedaiLegacyModule)
  protected
    function CreateUnit: TSedaiSignalNode; override;
    function ConfigureUnit(const AKey, AValue: string; AFloat: Single): Boolean; override;
  public
    constructor Create; override;
  end;

  TSedaiLegacyFlanger = class(TSedaiLegacyModule)
  protected
    function CreateUnit: TSedaiSignalNode; override;
  public
    constructor Create; override;
  end;

  TSedaiLegacyPhaser = class(TSedaiLegacyModule)
  protected
    function CreateUnit: TSedaiSignalNode; override;
  public
    constructor Create; override;
  end;

  TSedaiLegacyReverb = class(TSedaiLegacyModule)
  protected
    function CreateUnit: TSedaiSignalNode; override;
    function ConfigureUnit(const AKey, AValue: string; AFloat: Single): Boolean; override;
  public
    constructor Create; override;
  end;

  TSedaiLegacyCompressor = class(TSedaiLegacyModule)
  protected
    function CreateUnit: TSedaiSignalNode; override;
    function ConfigureUnit(const AKey, AValue: string; AFloat: Single): Boolean; override;
  public
    constructor Create; override;
  end;

  TSedaiLegacyLimiter = class(TSedaiLegacyModule)
  protected
    function CreateUnit: TSedaiSignalNode; override;
  public
    constructor Create; override;
  end;

  TSedaiLegacyDistortion = class(TSedaiLegacyModule)
  protected
    function CreateUnit: TSedaiSignalNode; override;
    function ConfigureUnit(const AKey, AValue: string; AFloat: Single): Boolean; override;
  public
    constructor Create; override;
  end;

// Factory for the bridged types, consulted after the native ones.
function CreateLegacyModuleByType(const ATypeName: string): TSedaiPatchModule;
function KnownLegacyTypes: string;

implementation

{ TSedaiLegacyModule }

constructor TSedaiLegacyModule.Create;
begin
  inherited Create;
  Rate := mrBlockOnly;             // the whole point of the supports flag
  FUnit := CreateUnit;
  FIn    := AddInput('in', prAudio, 0.0);
  // Right side. Left unpatched it follows the left, so a mono patch reads and
  // behaves exactly as before this port existed.
  FInR   := AddInput('inR', prAudio, 0.0);
  // A dry/wet control the wrapper provides itself, because not every legacy
  // unit has one and a patch should not have to know which do.
  FMixIn := AddInput('mix', prUnipolar, 1.0);
  FMixIn.Min := 0.0; FMixIn.Max := 1.0;
  FOut   := AddOutput('out', prAudio);
  FOutR  := AddOutput('outR', prAudio);
end;

destructor TSedaiLegacyModule.Destroy;
begin
  FUnit.Free;
  inherited Destroy;
end;

procedure TSedaiLegacyModule.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  inherited Prepare(ASampleRate, ABlockSize);
  if FUnit <> nil then
  begin
    FUnit.SetSampleRate(ASampleRate);
    FUnit.SetBlockSize(ABlockSize);
  end;
  // Interleaved stereo, which is what the legacy contract expects.
  if Length(FScratchIn) < ABlockSize * 2 then
  begin
    SetLength(FScratchIn, ABlockSize * 2);
    SetLength(FScratchOut, ABlockSize * 2);
  end;
end;

procedure TSedaiLegacyModule.ResetState;
begin
  inherited ResetState;
  if FUnit <> nil then FUnit.Reset;
end;

function TSedaiLegacyModule.ConfigureUnit(const AKey, AValue: string;
  AFloat: Single): Boolean;
begin
  Result := False;
end;

function TSedaiLegacyModule.Configure(const AKey, AValue: string): Boolean;
var
  F: Single;
begin
  F := StrToFloatDef(AValue, 0.0);
  Result := ConfigureUnit(AKey, AValue, F);
  if not Result then Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiLegacyModule.RenderSample(AIndex: Integer);
begin
  raise Exception.CreateFmt(
    'module "%s" (%s) is block-only and was asked for a single sample; the ' +
    'scheduler should have refused it inside a feedback cycle',
    [ModuleName, TypeName]);
end;

procedure TSedaiLegacyModule.RenderBlock(ACount: Integer);
var
  I: Integer;
  Dry, Wet, Mix: Single;
begin
  if (FUnit = nil) or (ACount <= 0) then Exit;
  if Length(FScratchIn) < ACount * 2 then
  begin
    SetLength(FScratchIn, ACount * 2);
    SetLength(FScratchOut, ACount * 2);
  end;

  // These units are stereo internally. The bridge used to fan mono in and throw
  // the right channel away, which silently flattened every stereo chorus and
  // reverb in the library. Now the right side is its own port, normalled to the
  // left: patch nothing and the behaviour is the old mono one exactly, patch
  // something and the unit is stereo end to end.
  if FInR.LinkCount > 0 then
    for I := 0 to ACount - 1 do
    begin
      FScratchIn[I * 2] := FIn.Read(I);
      FScratchIn[I * 2 + 1] := FInR.Read(I);
    end
  else
    for I := 0 to ACount - 1 do
    begin
      Dry := FIn.Read(I);
      FScratchIn[I * 2] := Dry;
      FScratchIn[I * 2 + 1] := Dry;
    end;

  FUnit.ProcessBlock(@FScratchIn[0], @FScratchOut[0], ACount);

  for I := 0 to ACount - 1 do
  begin
    Mix := FMixIn.Read(I);
    if Mix < 0.0 then Mix := 0.0 else if Mix > 1.0 then Mix := 1.0;
    Dry := FScratchIn[I * 2];
    Wet := FScratchOut[I * 2];
    FOut.Write(I, Dry * (1.0 - Mix) + Wet * Mix);
    Dry := FScratchIn[I * 2 + 1];
    Wet := FScratchOut[I * 2 + 1];
    FOutR.Write(I, Dry * (1.0 - Mix) + Wet * Mix);
  end;
end;

{ delay }

constructor TSedaiLegacyDelay.Create;
begin
  inherited Create;
  TypeName := 'sdelay';
end;

function TSedaiLegacyDelay.CreateUnit: TSedaiSignalNode;
begin
  Result := TSedaiDelay.Create;
end;

function TSedaiLegacyDelay.ConfigureUnit(const AKey, AValue: string;
  AFloat: Single): Boolean;
begin
  Result := True;
  if SameText(AKey, 'time') then TSedaiDelay(WrappedUnit).DelayTime := AFloat
  else if SameText(AKey, 'feedback') then TSedaiDelay(WrappedUnit).Feedback := AFloat
  else if SameText(AKey, 'moddepth') then TSedaiDelay(WrappedUnit).ModDepth := AFloat
  else if SameText(AKey, 'modrate') then TSedaiDelay(WrappedUnit).ModRate := AFloat
  else Result := False;
end;

{ chorus }

constructor TSedaiLegacyChorus.Create;
begin
  inherited Create;
  TypeName := 'schorus';
end;

function TSedaiLegacyChorus.CreateUnit: TSedaiSignalNode;
begin
  Result := TSedaiChorus.Create;
end;

function TSedaiLegacyChorus.ConfigureUnit(const AKey, AValue: string;
  AFloat: Single): Boolean;
begin
  Result := True;
  if SameText(AKey, 'rate') then TSedaiChorus(WrappedUnit).Rate := AFloat
  else if SameText(AKey, 'depth') then TSedaiChorus(WrappedUnit).Depth := AFloat
  else if SameText(AKey, 'voices') then TSedaiChorus(WrappedUnit).Voices := Round(AFloat)
  else Result := False;
end;

{ flanger }

constructor TSedaiLegacyFlanger.Create;
begin
  inherited Create;
  TypeName := 'sflanger';
end;

function TSedaiLegacyFlanger.CreateUnit: TSedaiSignalNode;
begin
  Result := TSedaiFlanger.Create;
end;

{ phaser }

constructor TSedaiLegacyPhaser.Create;
begin
  inherited Create;
  TypeName := 'sphaser';
end;

function TSedaiLegacyPhaser.CreateUnit: TSedaiSignalNode;
begin
  Result := TSedaiPhaser.Create;
end;

{ reverb }

constructor TSedaiLegacyReverb.Create;
begin
  inherited Create;
  TypeName := 'sreverb';
end;

function TSedaiLegacyReverb.CreateUnit: TSedaiSignalNode;
begin
  Result := TSedaiReverb.Create;
end;

function TSedaiLegacyReverb.ConfigureUnit(const AKey, AValue: string;
  AFloat: Single): Boolean;
begin
  Result := True;
  if SameText(AKey, 'size') then TSedaiReverb(WrappedUnit).RoomSize := AFloat
  else if SameText(AKey, 'damping') then TSedaiReverb(WrappedUnit).Damping := AFloat
  else if SameText(AKey, 'width') then TSedaiReverb(WrappedUnit).Width := AFloat
  else Result := False;
end;

{ compressor }

constructor TSedaiLegacyCompressor.Create;
begin
  inherited Create;
  TypeName := 'scomp';
end;

function TSedaiLegacyCompressor.CreateUnit: TSedaiSignalNode;
begin
  Result := TSedaiCompressor.Create;
end;

function TSedaiLegacyCompressor.ConfigureUnit(const AKey, AValue: string;
  AFloat: Single): Boolean;
begin
  Result := True;
  if SameText(AKey, 'threshold') then TSedaiCompressor(WrappedUnit).Threshold := AFloat
  else if SameText(AKey, 'ratio') then TSedaiCompressor(WrappedUnit).Ratio := AFloat
  else Result := False;
end;

{ limiter }

constructor TSedaiLegacyLimiter.Create;
begin
  inherited Create;
  TypeName := 'slimiter';
end;

function TSedaiLegacyLimiter.CreateUnit: TSedaiSignalNode;
begin
  Result := TSedaiLimiter.Create;
end;

{ distortion }

constructor TSedaiLegacyDistortion.Create;
begin
  inherited Create;
  TypeName := 'sdist';
end;

function TSedaiLegacyDistortion.CreateUnit: TSedaiSignalNode;
begin
  Result := TSedaiDistortion.Create;
end;

function TSedaiLegacyDistortion.ConfigureUnit(const AKey, AValue: string;
  AFloat: Single): Boolean;
begin
  Result := True;
  if SameText(AKey, 'drive') then TSedaiDistortion(WrappedUnit).Drive := AFloat
  else if SameText(AKey, 'tone') then TSedaiDistortion(WrappedUnit).Tone := AFloat
  else if SameText(AKey, 'gain') then TSedaiDistortion(WrappedUnit).OutputGain := AFloat
  else Result := False;
end;

{ factory }

function CreateLegacyModuleByType(const ATypeName: string): TSedaiPatchModule;
begin
  if SameText(ATypeName, 'sdelay') then Result := TSedaiLegacyDelay.Create
  else if SameText(ATypeName, 'schorus') then Result := TSedaiLegacyChorus.Create
  else if SameText(ATypeName, 'sflanger') then Result := TSedaiLegacyFlanger.Create
  else if SameText(ATypeName, 'sphaser') then Result := TSedaiLegacyPhaser.Create
  else if SameText(ATypeName, 'sreverb') then Result := TSedaiLegacyReverb.Create
  else if SameText(ATypeName, 'scomp') then Result := TSedaiLegacyCompressor.Create
  else if SameText(ATypeName, 'slimiter') then Result := TSedaiLegacyLimiter.Create
  else if SameText(ATypeName, 'sdist') then Result := TSedaiLegacyDistortion.Create
  else Result := nil;
end;

function KnownLegacyTypes: string;
begin
  Result := 'sdelay, schorus, sflanger, sphaser, sreverb, scomp, slimiter, sdist';
end;

end.
