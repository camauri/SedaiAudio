// ============================================================================
// SedaiPatchSpace — the spatial stage of the chain.
//
// This is the step you described: source, then processing, then a decision about
// how the sound occupies space, then the mixer. Up to here every port carried
// one mono signal and that stays true — these modules are where a mono signal
// becomes a pair, and they are the only place in the graph where that happens.
//
//   pan     mono -> pair. Constant power. What you reach for most of the time.
//   width   pair -> pair. Mid/side. Narrows or widens what is already a pair.
//   space   mono -> pair. A position in a room relative to a listener: distance
//           attenuation, interaural time and level difference, Doppler.
//
// All three are sample-first, which is the point of doing it this way rather
// than as a block effect: `x` and `z` are ports like any other, so a sound can
// be MOVED by an LFO or an envelope while it sounds. A source circling the
// listener is an LFO on x and a quarter-phase LFO on z, and nothing else.
//
// On the mixer: there is deliberately no mixer module. Summing is what a port
// already does — several links into one input, each with its own `amount` — and
// aux sends are just more links from the same output. Inserts are modules in
// series. What TSedaiMixer adds beyond that is solo, mute and metering, which
// are things a user interface does, not things a signal graph does. Adding a
// mixer module would be adding a second way to do what the wires already do.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchSpace;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Math, SedaiAudioTypes, SedaiPatchGraph, SedaiSpatialAudio;

type
  { TSedaiModPan — mono in, stereo out, constant power }

  TSedaiModPan = class(TSedaiPatchModule)
  private
    FIn, FPanIn, FOut, FOutR: TSedaiPatchPort;
  public
    constructor Create; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModWidth — stereo in, stereo out, mid/side }

  TSedaiModWidth = class(TSedaiPatchModule)
  private
    FIn, FInR, FWidthIn, FOut, FOutR: TSedaiPatchPort;
  public
    constructor Create; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

  { TSedaiModSpace — a position in a room }

  TSedaiModSpace = class(TSedaiPatchModule)
  private
    FListener: TSedaiSpatialListener;
    FProc: TSedaiSpatialProcessor;
    FIn, FXIn, FYIn, FZIn, FOut, FOutR: TSedaiPatchPort;
    FRefDist, FMaxDist, FRolloff: Single;
    FDoppler: Boolean;
    procedure Rebuild;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    procedure ResetState; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    procedure RenderSample(AIndex: Integer); override;
  end;

function CreateSpaceModuleByType(const ATypeName: string): TSedaiPatchModule;
function KnownSpaceTypes: string;

implementation

{ TSedaiModPan }

constructor TSedaiModPan.Create;
begin
  inherited Create;
  TypeName := 'pan';
  Rate := mrBoth;
  FIn    := AddInput('in', prAudio, 0.0);
  FPanIn := AddInput('pan', prBipolar, 0.0);
  FPanIn.Min := -1.0; FPanIn.Max := 1.0;
  FOut   := AddOutput('out', prAudio);
  FOutR  := AddOutput('outR', prAudio);
end;

procedure TSedaiModPan.RenderSample(AIndex: Integer);
var
  P, A, S, C: Single;
begin
  P := FPanIn.Read(AIndex);
  if P < -1.0 then P := -1.0 else if P > 1.0 then P := 1.0;
  // Constant power: the pair keeps the same energy across the sweep, so a pan
  // does not sound like a dip in the middle.
  A := (P + 1.0) * (PI * 0.25);
  SinCos(A, S, C);
  FOut.Write(AIndex, FIn.Read(AIndex) * C);
  FOutR.Write(AIndex, FIn.Read(AIndex) * S);
end;

{ TSedaiModWidth }

constructor TSedaiModWidth.Create;
begin
  inherited Create;
  TypeName := 'width';
  Rate := mrBoth;
  FIn      := AddInput('in', prAudio, 0.0);
  FInR     := AddInput('inR', prAudio, 0.0);
  FWidthIn := AddInput('width', prUnipolar, 1.0);
  FWidthIn.Min := 0.0; FWidthIn.Max := 2.0;
  FOut     := AddOutput('out', prAudio);
  FOutR    := AddOutput('outR', prAudio);
end;

procedure TSedaiModWidth.RenderSample(AIndex: Integer);
var
  L, R, M, S, W: Single;
begin
  L := FIn.Read(AIndex);
  R := FInR.Read(AIndex);
  W := FWidthIn.Read(AIndex);
  if W < 0.0 then W := 0.0 else if W > 2.0 then W := 2.0;
  // 0 collapses to mono, 1 is unchanged, 2 doubles the side. Scaling the side
  // and leaving the mid alone is what keeps a mono fold intact: whatever the
  // width, summing L+R returns the same mid.
  M := (L + R) * 0.5;
  S := (L - R) * 0.5 * W;
  FOut.Write(AIndex, M + S);
  FOutR.Write(AIndex, M - S);
end;

{ TSedaiModSpace }

constructor TSedaiModSpace.Create;
begin
  inherited Create;
  TypeName := 'space';
  Rate := mrBoth;
  FRefDist := 1.0;
  FMaxDist := 50.0;
  FRolloff := 1.0;
  FDoppler := False;
  FIn   := AddInput('in', prAudio, 0.0);
  // Metres, listener at the origin looking down -z: x to the right, y up,
  // z forward. Ports, not settings, so the source can be moved while it sounds.
  FXIn  := AddInput('x', prBipolar, 0.0);
  FXIn.Min := -100.0; FXIn.Max := 100.0;
  FYIn  := AddInput('y', prBipolar, 0.0);
  FYIn.Min := -100.0; FYIn.Max := 100.0;
  FZIn  := AddInput('z', prBipolar, -1.0);
  FZIn.Min := -100.0; FZIn.Max := 100.0;
  FOut  := AddOutput('out', prAudio);
  FOutR := AddOutput('outR', prAudio);
end;

destructor TSedaiModSpace.Destroy;
begin
  FProc.Free;
  FListener.Free;
  inherited Destroy;
end;

procedure TSedaiModSpace.Rebuild;
begin
  // The processor takes its sample rate from the listener at construction, so
  // a rate change means building both again rather than poking at them.
  FreeAndNil(FProc);
  FreeAndNil(FListener);
  // Round, not Integer(): FSR is a Single, and Integer(aSingle) in Object
  // Pascal reinterprets the bit pattern rather than converting the value.
  FListener := TSedaiSpatialListener.Create(Round(FSR));
  FProc := TSedaiSpatialProcessor.Create(FListener);
  FProc.SetDistanceParams(FRefDist, FMaxDist, FRolloff);
  FProc.DopplerEnabled := FDoppler;
end;

procedure TSedaiModSpace.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  inherited Prepare(ASampleRate, ABlockSize);
  Rebuild;
end;

procedure TSedaiModSpace.ResetState;
begin
  inherited ResetState;
  if FProc <> nil then Rebuild;
end;

function TSedaiModSpace.Configure(const AKey, AValue: string): Boolean;
var
  F: Single;
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  F := 0.0;
  TryStrToFloat(Trim(AValue), F, FS);
  if SameText(AKey, 'ref') then begin FRefDist := F; Exit(True); end;
  if SameText(AKey, 'max') then begin FMaxDist := F; Exit(True); end;
  if SameText(AKey, 'rolloff') then begin FRolloff := F; Exit(True); end;
  if SameText(AKey, 'doppler') then
  begin
    FDoppler := SameText(Trim(AValue), 'on') or SameText(Trim(AValue), 'true')
                or (F <> 0.0);
    Exit(True);
  end;
  Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModSpace.RenderSample(AIndex: Integer);
var
  L, R: Single;
begin
  FProc.SetPosition(FXIn.Read(AIndex), FYIn.Read(AIndex), FZIn.Read(AIndex));
  FProc.Process(FIn.Read(AIndex), L, R);
  FOut.Write(AIndex, L);
  FOutR.Write(AIndex, R);
end;

{ factory }

function CreateSpaceModuleByType(const ATypeName: string): TSedaiPatchModule;
begin
  if SameText(ATypeName, 'pan') then Result := TSedaiModPan.Create
  else if SameText(ATypeName, 'width') then Result := TSedaiModWidth.Create
  else if SameText(ATypeName, 'space') then Result := TSedaiModSpace.Create
  else Result := nil;
end;

function KnownSpaceTypes: string;
begin
  Result := 'pan, width, space';
end;

end.
