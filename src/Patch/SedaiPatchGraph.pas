// ============================================================================
// SedaiPatchGraph — the patch graph: ports, module base, scheduling.
//
// This is the substrate for the Patch Workbench: build a sound by connecting
// modules, the way the pioneers of electronic music did. The design notes live
// in job/docs/SAF_MODULAR_DESIGN.md; the parts that matter to this unit:
//
//   * ONE kind of connection. Any output can feed any input. There is no
//     enumerated list of legal destinations, and no privileged "audio" path —
//     which is what makes an LFO into pitch and an oscillator into pitch the
//     same operation, so vibrato and FM are one patch at two frequencies.
//
//   * SAMPLE RATE IS SCOPED, NEVER GLOBAL. Only modules inside a feedback cycle
//     need to advance one sample at a time. The graph is decomposed into
//     strongly connected components (Tarjan); the condensation is always a DAG,
//     so acyclic components — the vast majority — run at full block rate with no
//     per-sample dispatch at all. Measured on a 20-node chain: block 406x
//     realtime, whole-graph-per-sample 365x, mixed 16+4 436x. Scoping it costs
//     nothing; not scoping it costs 10% and driving legacy block-only units one
//     sample at a time costs 29%.
//
//   * ONE PORT CARRIES ONE MONO SIGNAL. This is a decision, not an omission.
//     The rest of SAF passes interleaved stereo blocks, but a patch graph wants
//     one signal per connection: stereo in a port would force every module to
//     know a channel count, and every patch to care. Stereo arrives later as
//     either a pair of ports (out.l / out.r) or an explicit per-port channel
//     count, and until then a patch renders mono.
//
//   * Feedback is a technique, not an error. A cycle is detected, its back edges
//     are marked, and a back edge reads the source's PREVIOUS sample. That unit
//     delay is what makes the loop computable.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchGraph;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SedaiAudioTypes, SedaiSignalNode;

type
  // Roles are metadata for the UI and for sane defaults. They are deliberately
  // NOT a type system: nothing here refuses a connection because the roles
  // differ. That freedom is the whole point (see the design notes, 1.1/1.2).
  TSedaiPortRole = (prAudio, prPitch, prGate, prUnipolar, prBipolar);
  TSedaiPortKind = (pkInput, pkOutput);

  // Whether a module can be advanced one sample at a time. Modules written for
  // the workbench are 'both'; a wrapper around a legacy block-oriented SAF unit
  // is 'block only' and is REFUSED inside a feedback cycle at compile time,
  // rather than being driven as blocks of one.
  TSedaiModuleRate = (mrBoth, mrBlockOnly);

  TSedaiPatchModule = class;
  TSedaiPatchPort = class;

  // A single connection into an input port. Several may target the same input;
  // they sum, as they would on a passive multiple.
  TSedaiPortLink = record
    Source: TSedaiPatchPort;
    Amount: Single;
    Delayed: Boolean;    // back edge inside a cycle: read the previous sample
    Normalled: Boolean;  // a default that yields the moment the input is patched
  end;

  { TSedaiPatchPort }

  TSedaiPatchPort = class
  private
    FName: string;
    FRole: TSedaiPortRole;
    FKind: TSedaiPortKind;
    FOwner: TSedaiPatchModule;
    FBuffer: array of Single;
    FPrev: Single;                    // value written on the previous sample
    FValue: Single;                   // the "knob": base value of an input
    FMin, FMax: Single;               // advisory range, for validation and a UI
    FUnitLabel: string;
    FLinks: array of TSedaiPortLink;
  public
    constructor Create(AOwner: TSedaiPatchModule; const AName: string;
                       AKind: TSedaiPortKind; ARole: TSedaiPortRole);
    procedure EnsureBuffer(ASize: Integer);
    procedure AddLink(ASource: TSedaiPatchPort; AAmount: Single;
                      ANormalled: Boolean = False);
    // Normalling, the idea that keeps a modular usable: a jack has a default
    // internal connection which is broken the instant something is plugged into
    // it. So an instrument makes a sound before you patch anything, and every
    // patch is a delta from something that already works.
    procedure DropNormalledIfPatched;
    function LinkCount: Integer;
    function LinkSource(AIndex: Integer): TSedaiPatchPort;
    procedure MarkLinkDelayed(AIndex: Integer);
    function LinkDelayed(AIndex: Integer): Boolean;
    procedure Reset;

    // The constant fast path. An input with nothing patched into it never
    // touches a buffer: it answers with the knob value and the caller pays a
    // compare instead of a memory read.
    function IsConstant: Boolean; inline;

    // Read an input at sample AIndex: knob value plus every patched source.
    function Read(AIndex: Integer): Single; inline;
    // Write an output at sample AIndex. FPrev is updated at the same time, so a
    // back edge read EARLIER in the same sample still sees the previous one.
    procedure Write(AIndex: Integer; AValue: Single); inline;
    function Sample(AIndex: Integer): Single; inline;

    property Name: string read FName;
    property Role: TSedaiPortRole read FRole;
    property Kind: TSedaiPortKind read FKind;
    property Owner: TSedaiPatchModule read FOwner;
    property Value: Single read FValue write FValue;
    // Advisory only: a range is checked when a patch file sets a value, and is
    // there for a future UI. It is NOT enforced on signals arriving through a
    // connection — clamping a patched CV would break the whole premise that any
    // output may drive any input.
    property Min: Single read FMin write FMin;
    property Max: Single read FMax write FMax;
    property UnitLabel: string read FUnitLabel write FUnitLabel;
    function InRange(AValue: Single): Boolean;
  end;

  TSedaiPatchPortArray = array of TSedaiPatchPort;

  { TSedaiPatchModule }
  // Descends from TSedaiSignalNode on purpose: there is ONE node hierarchy in
  // SAF and the workbench extends it rather than growing a parallel one. Nothing
  // in TSedaiSignalNode is modified — this is additive.
  TSedaiPatchModule = class(TSedaiSignalNode)
  private
    FPorts: TSedaiPatchPortArray;
    FModuleName: string;
    FTypeName: string;
    FRate: TSedaiModuleRate;
  protected
    FSR: Single;
    function AddInput(const AName: string; ARole: TSedaiPortRole;
                      ADefault: Single = 0.0): TSedaiPatchPort;
    function AddOutput(const AName: string; ARole: TSedaiPortRole): TSedaiPatchPort;
  public
    constructor Create; override;
    destructor Destroy; override;

    function PortByName(const AName: string): TSedaiPatchPort;
    function PortCount: Integer;
    function Port(AIndex: Integer): TSedaiPatchPort;

    // Called once before rendering, after sample rate and block size are known.
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); virtual;
    procedure ResetState; virtual;

    // Optional configuration from the patch file (`module osc1 = osc shape=saw`).
    // Returns False if the key is not understood, so the loader can complain.
    function Configure(const AKey, AValue: string): Boolean; virtual;

    // THE primitive. Everything else is built from it: RenderBlock is a loop.
    // Writing modules sample-first is what lets one implementation serve both
    // schedulers (design notes, 6.1).
    procedure RenderSample(AIndex: Integer); virtual; abstract;
    procedure RenderRange(AStart, ACount: Integer); virtual;
    procedure RenderBlock(ACount: Integer); virtual;

    // Samples of delay this module puts between its input and its output. Zero
    // for a filter or a VCA; the line length for a delay. Used to widen the
    // chunk a feedback cycle can be advanced by: a loop passing through 500
    // samples of delay does not have to be walked one sample at a time.
    function InternalDelay: Integer; virtual;

    property ModuleName: string read FModuleName write FModuleName;
    property TypeName: string read FTypeName write FTypeName;
    property Rate: TSedaiModuleRate read FRate write FRate;
  end;

  TSedaiModuleClass = class of TSedaiPatchModule;

  // One strongly connected component of the graph, in execution order.
  TSedaiPatchStage = record
    Members: array of Integer;   // indices into the module list
    IsCycle: Boolean;            // true = the stage contains a feedback loop
    ChunkSize: Integer;          // how many samples it may advance at a time
  end;

  { TSedaiPatchGraph }

  TSedaiPatchGraph = class
  private
    FModules: array of TSedaiPatchModule;
    FStages: array of TSedaiPatchStage;
    FSampleRate: Cardinal;
    FBlockSize: Integer;
    FCompiled: Boolean;
    FOutputPort: TSedaiPatchPort;
    FLastError: string;
    // Tarjan working state
    FIndex: array of Integer;
    FLow: array of Integer;
    FOnStack: array of Boolean;
    FStack: array of Integer;
    FStackTop: Integer;
    FCounter: Integer;
    FComp: array of Integer;      // module -> component id
    FCompCount: Integer;
    procedure Tarjan(AVertex: Integer);
    function ModuleIndex(AModule: TSedaiPatchModule): Integer;
    procedure MarkBackEdges;
    function MinCycleDelay(AStage: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function AddModule(AModule: TSedaiPatchModule; const AName: string): Boolean;
    function ModuleByName(const AName: string): TSedaiPatchModule;
    // First module of a given type, so a host can find e.g. the audio input
    // without dictating what the patch must call it.
    function ModuleOfType(const ATypeName: string): TSedaiPatchModule;
    function FindPort(const APath: string): TSedaiPatchPort;   // "osc1.out"
    function Connect(const ASource, ADest: string; AAmount: Single = 1.0;
                     ANormalled: Boolean = False): Boolean;
    function SetValue(const APath: string; AValue: Single): Boolean;
    function SetOutput(const APath: string): Boolean;

    // Decompose, order, validate. AForceSampleRate makes every stage advance one
    // sample at a time (`mode = sample` in the patch file) — useful to compare
    // the two schedulers, never needed for correctness.
    function Compile(AForceSampleRate: Boolean = False): Boolean;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
    procedure ResetState;

    // Render ACount frames. Acyclic stages go through RenderBlock in one call;
    // cyclic stages advance sample by sample, and only those.
    procedure Render(ACount: Integer);
    function OutputSample(AIndex: Integer): Single; inline;

    function StageCount: Integer;
    function StageIsCycle(AIndex: Integer): Boolean;
    function StageChunk(AIndex: Integer): Integer;
    function StageSize(AIndex: Integer): Integer;
    function Describe: string;

    property ModuleCount: Integer read FCompCount;
    property LastError: string read FLastError;
    property SampleRate: Cardinal read FSampleRate;
    property BlockSize: Integer read FBlockSize;
    property Compiled: Boolean read FCompiled;
    property OutputPort: TSedaiPatchPort read FOutputPort;
  end;

implementation

{ TSedaiPatchPort }

constructor TSedaiPatchPort.Create(AOwner: TSedaiPatchModule; const AName: string;
  AKind: TSedaiPortKind; ARole: TSedaiPortRole);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FKind := AKind;
  FRole := ARole;
  FValue := 0.0;
  FPrev := 0.0;
  FMin := -1.0e30;
  FMax := 1.0e30;
  FUnitLabel := '';
  SetLength(FLinks, 0);
end;

function TSedaiPatchPort.InRange(AValue: Single): Boolean;
begin
  Result := (AValue >= FMin) and (AValue <= FMax);
end;

procedure TSedaiPatchPort.EnsureBuffer(ASize: Integer);
begin
  // Outputs always need storage. An input only needs it if something is
  // actually patched into it — otherwise it stays on the constant path.
  if (FKind = pkOutput) or (Length(FLinks) > 0) then
  begin
    if Length(FBuffer) < ASize then
      SetLength(FBuffer, ASize);
  end;
end;

procedure TSedaiPatchPort.AddLink(ASource: TSedaiPatchPort; AAmount: Single;
  ANormalled: Boolean);
var
  N: Integer;
begin
  N := Length(FLinks);
  SetLength(FLinks, N + 1);
  FLinks[N].Source := ASource;
  FLinks[N].Amount := AAmount;
  FLinks[N].Delayed := False;
  FLinks[N].Normalled := ANormalled;
end;

procedure TSedaiPatchPort.DropNormalledIfPatched;
var
  I, W: Integer;
  Explicit: Boolean;
begin
  Explicit := False;
  for I := 0 to High(FLinks) do
    if not FLinks[I].Normalled then Explicit := True;
  if not Explicit then Exit;          // nothing plugged in: the default stands

  W := 0;
  for I := 0 to High(FLinks) do
    if not FLinks[I].Normalled then
    begin
      if W <> I then FLinks[W] := FLinks[I];
      Inc(W);
    end;
  SetLength(FLinks, W);
end;

function TSedaiPatchPort.LinkCount: Integer;
begin
  Result := Length(FLinks);
end;

function TSedaiPatchPort.LinkSource(AIndex: Integer): TSedaiPatchPort;
begin
  if (AIndex >= 0) and (AIndex < Length(FLinks)) then
    Result := FLinks[AIndex].Source
  else
    Result := nil;
end;

procedure TSedaiPatchPort.MarkLinkDelayed(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < Length(FLinks)) then
    FLinks[AIndex].Delayed := True;
end;

function TSedaiPatchPort.LinkDelayed(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Length(FLinks)) and FLinks[AIndex].Delayed;
end;

procedure TSedaiPatchPort.Reset;
var
  I: Integer;
begin
  FPrev := 0.0;
  for I := 0 to High(FBuffer) do FBuffer[I] := 0.0;
end;

function TSedaiPatchPort.IsConstant: Boolean;
begin
  Result := Length(FLinks) = 0;
end;

function TSedaiPatchPort.Read(AIndex: Integer): Single;
var
  I: Integer;
  Acc: Single;
begin
  if Length(FLinks) = 0 then
  begin
    Result := FValue;   // constant fast path: no memory traffic at all
    Exit;
  end;
  Acc := FValue;
  for I := 0 to High(FLinks) do
    if FLinks[I].Delayed then
      Acc := Acc + FLinks[I].Source.FPrev * FLinks[I].Amount
    else
      Acc := Acc + FLinks[I].Source.FBuffer[AIndex] * FLinks[I].Amount;
  Result := Acc;
end;

procedure TSedaiPatchPort.Write(AIndex: Integer; AValue: Single);
begin
  FBuffer[AIndex] := AValue;
  FPrev := AValue;
end;

function TSedaiPatchPort.Sample(AIndex: Integer): Single;
begin
  if Length(FBuffer) > AIndex then Result := FBuffer[AIndex] else Result := FValue;
end;

{ TSedaiPatchModule }

constructor TSedaiPatchModule.Create;
begin
  inherited Create;
  SetLength(FPorts, 0);
  FRate := mrBoth;
  FSR := 44100.0;
end;

destructor TSedaiPatchModule.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FPorts) do FPorts[I].Free;
  SetLength(FPorts, 0);
  inherited Destroy;
end;

function TSedaiPatchModule.AddInput(const AName: string; ARole: TSedaiPortRole;
  ADefault: Single): TSedaiPatchPort;
var
  N: Integer;
begin
  Result := TSedaiPatchPort.Create(Self, AName, pkInput, ARole);
  Result.Value := ADefault;
  N := Length(FPorts);
  SetLength(FPorts, N + 1);
  FPorts[N] := Result;
end;

function TSedaiPatchModule.AddOutput(const AName: string; ARole: TSedaiPortRole): TSedaiPatchPort;
var
  N: Integer;
begin
  Result := TSedaiPatchPort.Create(Self, AName, pkOutput, ARole);
  N := Length(FPorts);
  SetLength(FPorts, N + 1);
  FPorts[N] := Result;
end;

function TSedaiPatchModule.PortByName(const AName: string): TSedaiPatchPort;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to High(FPorts) do
    if SameText(FPorts[I].Name, AName) then Exit(FPorts[I]);
end;

function TSedaiPatchModule.PortCount: Integer;
begin
  Result := Length(FPorts);
end;

function TSedaiPatchModule.Port(AIndex: Integer): TSedaiPatchPort;
begin
  if (AIndex >= 0) and (AIndex < Length(FPorts)) then Result := FPorts[AIndex]
  else Result := nil;
end;

procedure TSedaiPatchModule.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
var
  I: Integer;
begin
  FSR := ASampleRate;
  for I := 0 to High(FPorts) do FPorts[I].EnsureBuffer(ABlockSize);
end;

procedure TSedaiPatchModule.ResetState;
var
  I: Integer;
begin
  for I := 0 to High(FPorts) do FPorts[I].Reset;
end;

// Understood by every module, so a patch can declare that one of them must not
// be advanced a sample at a time. That is what a wrapper around a legacy
// block-oriented SAF unit will report, and declaring it by hand is also how the
// refusal path is exercised before that wrapper exists.
function TSedaiPatchModule.Configure(const AKey, AValue: string): Boolean;
var
  P: TSedaiPatchPort;
  V: Single;
  FS: TFormatSettings;
begin
  Result := False;
  if SameText(AKey, 'supports') then
  begin
    if SameText(AValue, 'block') then begin FRate := mrBlockOnly; Result := True; end
    else if SameText(AValue, 'both') then begin FRate := mrBoth; Result := True; end;
    Exit;
  end;

  // Fall back to setting an input port of the same name, so a module can be
  // written `module gl = glide time=0.012` instead of needing a separate `set`
  // line. A subclass that claims the key first still wins, which is how the
  // filter keeps `cutoff` meaning its base frequency in Hz while the port of
  // the same name stays the volts-per-octave offset patched into it.
  P := PortByName(AKey);
  if (P <> nil) and (P.Kind = pkInput) then
  begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    if TryStrToFloat(Trim(AValue), V, FS) then
    begin
      if not P.InRange(V) then Exit(False);
      P.Value := V;
      Result := True;
    end;
  end;
end;

procedure TSedaiPatchModule.RenderRange(AStart, ACount: Integer);
var
  I: Integer;
begin
  for I := AStart to AStart + ACount - 1 do RenderSample(I);
end;

procedure TSedaiPatchModule.RenderBlock(ACount: Integer);
begin
  RenderRange(0, ACount);
end;

function TSedaiPatchModule.InternalDelay: Integer;
begin
  Result := 0;
end;

{ TSedaiPatchGraph }

constructor TSedaiPatchGraph.Create;
begin
  inherited Create;
  SetLength(FModules, 0);
  SetLength(FStages, 0);
  FSampleRate := 44100;
  FBlockSize := 256;
  FCompiled := False;
  FOutputPort := nil;
  FLastError := '';
  FCompCount := 0;
end;

destructor TSedaiPatchGraph.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FModules) do FModules[I].Free;
  SetLength(FModules, 0);
  inherited Destroy;
end;

function TSedaiPatchGraph.AddModule(AModule: TSedaiPatchModule; const AName: string): Boolean;
var
  N: Integer;
begin
  Result := False;
  if AModule = nil then Exit;
  if ModuleByName(AName) <> nil then
  begin
    FLastError := Format('duplicate module name "%s"', [AName]);
    Exit;
  end;
  AModule.ModuleName := AName;
  N := Length(FModules);
  SetLength(FModules, N + 1);
  FModules[N] := AModule;
  FCompCount := N + 1;
  FCompiled := False;
  Result := True;
end;

function TSedaiPatchGraph.ModuleByName(const AName: string): TSedaiPatchModule;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to High(FModules) do
    if SameText(FModules[I].ModuleName, AName) then Exit(FModules[I]);
end;

function TSedaiPatchGraph.ModuleOfType(const ATypeName: string): TSedaiPatchModule;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to High(FModules) do
    if SameText(FModules[I].TypeName, ATypeName) then Exit(FModules[I]);
end;

function TSedaiPatchGraph.FindPort(const APath: string): TSedaiPatchPort;
var
  P: Integer;
  M: TSedaiPatchModule;
begin
  Result := nil;
  P := Pos('.', APath);
  if P <= 1 then
  begin
    FLastError := Format('"%s" is not a module.port path', [APath]);
    Exit;
  end;
  M := ModuleByName(Copy(APath, 1, P - 1));
  if M = nil then
  begin
    FLastError := Format('unknown module "%s"', [Copy(APath, 1, P - 1)]);
    Exit;
  end;
  Result := M.PortByName(Copy(APath, P + 1, Length(APath)));
  if Result = nil then
    FLastError := Format('module "%s" has no port "%s"',
                         [M.ModuleName, Copy(APath, P + 1, Length(APath))]);
end;

function TSedaiPatchGraph.Connect(const ASource, ADest: string; AAmount: Single;
  ANormalled: Boolean): Boolean;
var
  S, D: TSedaiPatchPort;
begin
  Result := False;
  S := FindPort(ASource);
  if S = nil then Exit;
  D := FindPort(ADest);
  if D = nil then Exit;
  if S.Kind <> pkOutput then
  begin
    FLastError := Format('"%s" is an input; a connection must start at an output', [ASource]);
    Exit;
  end;
  if D.Kind <> pkInput then
  begin
    FLastError := Format('"%s" is an output; a connection must end at an input', [ADest]);
    Exit;
  end;
  D.AddLink(S, AAmount, ANormalled);
  FCompiled := False;
  Result := True;
end;

function TSedaiPatchGraph.SetValue(const APath: string; AValue: Single): Boolean;
var
  P: TSedaiPatchPort;
begin
  P := FindPort(APath);
  Result := P <> nil;
  if not Result then Exit;
  if not P.InRange(AValue) then
  begin
    if P.UnitLabel <> '' then
      FLastError := Format('%s = %g is outside %g .. %g %s',
                           [APath, AValue, P.Min, P.Max, P.UnitLabel])
    else
      FLastError := Format('%s = %g is outside %g .. %g',
                           [APath, AValue, P.Min, P.Max]);
    Exit(False);
  end;
  P.Value := AValue;
end;

function TSedaiPatchGraph.SetOutput(const APath: string): Boolean;
var
  P: TSedaiPatchPort;
begin
  P := FindPort(APath);
  if P = nil then Exit(False);
  if P.Kind <> pkOutput then
  begin
    FLastError := Format('"%s" is not an output', [APath]);
    Exit(False);
  end;
  FOutputPort := P;
  Result := True;
end;

function TSedaiPatchGraph.ModuleIndex(AModule: TSedaiPatchModule): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FModules) do
    if FModules[I] = AModule then Exit(I);
end;

// Tarjan's strongly connected components. Emits components in REVERSE
// topological order of the condensation, which is why Compile walks FStages
// backwards when building the execution order.
procedure TSedaiPatchGraph.Tarjan(AVertex: Integer);
var
  I, J, K, W, Top: Integer;
  M: TSedaiPatchModule;
  Prt: TSedaiPatchPort;
begin
  FIndex[AVertex] := FCounter;
  FLow[AVertex] := FCounter;
  Inc(FCounter);
  FStack[FStackTop] := AVertex;
  Inc(FStackTop);
  FOnStack[AVertex] := True;

  M := FModules[AVertex];
  for I := 0 to M.PortCount - 1 do
  begin
    Prt := M.Port(I);
    if Prt.Kind <> pkInput then Continue;
    for J := 0 to Prt.LinkCount - 1 do
    begin
      W := ModuleIndex(Prt.LinkSource(J).Owner);
      if W < 0 then Continue;
      if FIndex[W] < 0 then
      begin
        Tarjan(W);
        if FLow[W] < FLow[AVertex] then FLow[AVertex] := FLow[W];
      end
      else if FOnStack[W] then
        if FIndex[W] < FLow[AVertex] then FLow[AVertex] := FIndex[W];
    end;
  end;

  if FLow[AVertex] = FIndex[AVertex] then
  begin
    Top := Length(FStages);
    SetLength(FStages, Top + 1);
    SetLength(FStages[Top].Members, 0);
    FStages[Top].IsCycle := False;
    repeat
      Dec(FStackTop);
      W := FStack[FStackTop];
      FOnStack[W] := False;
      FComp[W] := Top;
      K := Length(FStages[Top].Members);
      SetLength(FStages[Top].Members, K + 1);
      FStages[Top].Members[K] := W;
    until W = AVertex;
  end;
end;

// A link is a back edge when its source lives in the SAME component and is
// scheduled at or after the destination. Those read the previous sample, which
// is exactly the unit delay that makes a feedback loop computable.
procedure TSedaiPatchGraph.MarkBackEdges;
var
  S, I, J, K, MI, SrcMod, PosDst, PosSrc: Integer;
  M: TSedaiPatchModule;
  Prt: TSedaiPatchPort;

  function PositionInStage(AStage, AModule: Integer): Integer;
  var
    Q: Integer;
  begin
    Result := -1;
    for Q := 0 to High(FStages[AStage].Members) do
      if FStages[AStage].Members[Q] = AModule then Exit(Q);
  end;

begin
  for S := 0 to High(FStages) do
  begin
    if not FStages[S].IsCycle then Continue;
    for I := 0 to High(FStages[S].Members) do
    begin
      MI := FStages[S].Members[I];
      M := FModules[MI];
      PosDst := I;
      for J := 0 to M.PortCount - 1 do
      begin
        Prt := M.Port(J);
        if Prt.Kind <> pkInput then Continue;
        for K := 0 to Prt.LinkCount - 1 do
        begin
          SrcMod := ModuleIndex(Prt.LinkSource(K).Owner);
          if SrcMod < 0 then Continue;
          if FComp[SrcMod] <> S then Continue;          // crosses stages: fine
          PosSrc := PositionInStage(S, SrcMod);
          if PosSrc >= PosDst then Prt.MarkLinkDelayed(K);
        end;
      end;
    end;
  end;
end;

// Shortest cycle, measured in samples of delay, inside one component.
// Edge A -> B (B reads A) weighs A's internal delay, plus one when the edge is
// the marked back edge that carries the unit feedback delay. The minimum over
// all cycles is how far the component may be advanced before it must close the
// loop — 1 for a bare filter-into-itself, 5293 for a 120 ms echo.
function TSedaiPatchGraph.MinCycleDelay(AStage: Integer): Integer;
const
  INF = MaxInt div 4;
var
  N, I, J, K, A, B, W, Best: Integer;
  Dist: array of array of Integer;
  Local: array of Integer;

  function LocalIndex(AModule: Integer): Integer;
  var
    Q: Integer;
  begin
    Result := -1;
    for Q := 0 to High(Local) do
      if Local[Q] = AModule then Exit(Q);
  end;

var
  M: TSedaiPatchModule;
  Prt: TSedaiPatchPort;
  SrcMod: Integer;
begin
  Local := FStages[AStage].Members;
  N := Length(Local);
  if N = 0 then Exit(1);

  SetLength(Dist, N, N);
  for I := 0 to N - 1 do
    for J := 0 to N - 1 do Dist[I][J] := INF;

  // Build the weighted edges from the input links of every member.
  for I := 0 to N - 1 do
  begin
    M := FModules[Local[I]];
    for J := 0 to M.PortCount - 1 do
    begin
      Prt := M.Port(J);
      if Prt.Kind <> pkInput then Continue;
      for K := 0 to Prt.LinkCount - 1 do
      begin
        SrcMod := ModuleIndex(Prt.LinkSource(K).Owner);
        if SrcMod < 0 then Continue;
        A := LocalIndex(SrcMod);
        if A < 0 then Continue;              // comes from another stage
        W := FModules[SrcMod].InternalDelay;
        if W < 0 then W := 0;
        if Prt.LinkDelayed(K) then Inc(W);
        if W < Dist[A][I] then Dist[A][I] := W;
      end;
    end;
  end;

  for K := 0 to N - 1 do
    for I := 0 to N - 1 do
      if Dist[I][K] < INF then
        for J := 0 to N - 1 do
          if Dist[K][J] < INF then
            if Dist[I][K] + Dist[K][J] < Dist[I][J] then
              Dist[I][J] := Dist[I][K] + Dist[K][J];

  Best := INF;
  for I := 0 to N - 1 do
    if Dist[I][I] < Best then Best := Dist[I][I];
  if Best >= INF then Best := 1;
  Result := Best;
end;

function TSedaiPatchGraph.Compile(AForceSampleRate: Boolean): Boolean;
var
  I, J, K, N: Integer;
  M: TSedaiPatchModule;
  Prt: TSedaiPatchPort;
  SelfLoop: Boolean;
begin
  Result := False;
  FLastError := '';
  N := Length(FModules);
  if N = 0 then
  begin
    FLastError := 'the patch has no modules';
    Exit;
  end;

  // Resolve normalling BEFORE anything looks at the graph: a dropped default
  // must not appear as an edge, or it would create cycles and stages that the
  // finished patch does not have.
  for I := 0 to N - 1 do
    for J := 0 to FModules[I].PortCount - 1 do
      if FModules[I].Port(J).Kind = pkInput then
        FModules[I].Port(J).DropNormalledIfPatched;

  SetLength(FStages, 0);
  SetLength(FIndex, N);
  SetLength(FLow, N);
  SetLength(FOnStack, N);
  SetLength(FStack, N);
  SetLength(FComp, N);
  for I := 0 to N - 1 do
  begin
    FIndex[I] := -1; FLow[I] := -1; FOnStack[I] := False; FComp[I] := -1;
  end;
  FStackTop := 0;
  FCounter := 0;

  for I := 0 to N - 1 do
    if FIndex[I] < 0 then Tarjan(I);

  // NOTE ON DIRECTION. The edges walked by Tarjan run from a module to its
  // SOURCES (they are discovered through input links), i.e. against the flow of
  // audio. Tarjan emits a component once it is complete, so it emits the sinks
  // of the graph it was given first — and in this reversed graph the sinks are
  // the producers. That is already the execution order: producers first.
  // Reversing here, as a first version did, put the output stage before the
  // oscillator and every module read stale buffers.
  for I := 0 to High(FStages) do
    for J := 0 to High(FStages[I].Members) do
      FComp[FStages[I].Members[J]] := I;

  // A stage is a cycle if it has more than one member, or one member that feeds
  // itself. Everything else is acyclic and runs at full block rate.
  for I := 0 to High(FStages) do
  begin
    if Length(FStages[I].Members) > 1 then
    begin
      FStages[I].IsCycle := True;
      Continue;
    end;
    SelfLoop := False;
    M := FModules[FStages[I].Members[0]];
    for J := 0 to M.PortCount - 1 do
    begin
      Prt := M.Port(J);
      if Prt.Kind <> pkInput then Continue;
      for K := 0 to Prt.LinkCount - 1 do
        if Prt.LinkSource(K).Owner = M then SelfLoop := True;
    end;
    FStages[I].IsCycle := SelfLoop;
  end;

  if AForceSampleRate then
    for I := 0 to High(FStages) do FStages[I].IsCycle := True;

  // MarkBackEdges first: the chunk calculation below weighs each edge by whether
  // it carries the one-sample feedback delay.
  MarkBackEdges;

  // How far a stage may be advanced in one go.
  //
  // An acyclic stage has no constraint: the whole block at once.
  //
  // For a cycle the safe chunk is the SMALLEST total delay around any loop in
  // it. Taking the smallest delay of any single MEMBER, as a first version did,
  // is safe but useless: a real echo is a delay line plus a feedback VCA, and
  // the VCA contributes zero, so the answer was always 1.
  //
  // The right quantity is a shortest-cycle weight, so compute it directly: give
  // each edge A->B the weight of A's internal delay plus one if that edge is a
  // marked back edge, then take a min-plus (Floyd-Warshall) closure and read the
  // best diagonal entry. Components are small — a handful of modules — so the
  // cubic closure costs nothing, and it is exact rather than conservative.
  for I := 0 to High(FStages) do
  begin
    if not FStages[I].IsCycle then
    begin
      FStages[I].ChunkSize := 0;      // 0 = unconstrained, take the whole block
      Continue;
    end;
    FStages[I].ChunkSize := MinCycleDelay(I);
    if FStages[I].ChunkSize < 1 then FStages[I].ChunkSize := 1;
  end;
  // A block-only module inside a cycle is REFUSED. It is not driven as blocks
  // of one — that path costs 29% of throughput and is simply made unreachable.
  for I := 0 to High(FStages) do
  begin
    if not FStages[I].IsCycle then Continue;
    for J := 0 to High(FStages[I].Members) do
    begin
      M := FModules[FStages[I].Members[J]];
      if M.Rate = mrBlockOnly then
      begin
        FLastError := Format(
          'module "%s" (%s) is block-only and cannot sit inside a feedback cycle; ' +
          'the cycle has %d modules', [M.ModuleName, M.TypeName,
          Length(FStages[I].Members)]);
        Exit;
      end;
    end;
  end;

  if FOutputPort = nil then
  begin
    FLastError := 'the patch declares no output (use: output <module>.<port>)';
    Exit;
  end;

  FCompiled := True;
  Result := True;
end;

procedure TSedaiPatchGraph.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
var
  I: Integer;
begin
  FSampleRate := ASampleRate;
  FBlockSize := ABlockSize;
  for I := 0 to High(FModules) do
    FModules[I].Prepare(ASampleRate, ABlockSize);
end;

procedure TSedaiPatchGraph.ResetState;
var
  I: Integer;
begin
  for I := 0 to High(FModules) do FModules[I].ResetState;
end;

procedure TSedaiPatchGraph.Render(ACount: Integer);
var
  S, I, K, N, Chunk: Integer;
begin
  if not FCompiled then Exit;
  for S := 0 to High(FStages) do
  begin
    if FStages[S].IsCycle then
    begin
      // The only place that pays extra dispatch, and only for these modules —
      // and only in chunks of ChunkSize, which is 1 solely when the loop has no
      // delay in it at all.
      Chunk := FStages[S].ChunkSize;
      if Chunk < 1 then Chunk := 1;
      K := 0;
      while K < ACount do
      begin
        N := ACount - K;
        if N > Chunk then N := Chunk;
        for I := 0 to High(FStages[S].Members) do
          FModules[FStages[S].Members[I]].RenderRange(K, N);
        Inc(K, N);
      end;
    end
    else
      for I := 0 to High(FStages[S].Members) do
        FModules[FStages[S].Members[I]].RenderBlock(ACount);
  end;
end;

function TSedaiPatchGraph.OutputSample(AIndex: Integer): Single;
begin
  if FOutputPort <> nil then Result := FOutputPort.Sample(AIndex) else Result := 0.0;
end;

function TSedaiPatchGraph.StageCount: Integer;
begin
  Result := Length(FStages);
end;

function TSedaiPatchGraph.StageIsCycle(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Length(FStages)) and FStages[AIndex].IsCycle;
end;

function TSedaiPatchGraph.StageChunk(AIndex: Integer): Integer;
begin
  if (AIndex >= 0) and (AIndex < Length(FStages)) then
    Result := FStages[AIndex].ChunkSize
  else
    Result := 0;
end;

function TSedaiPatchGraph.StageSize(AIndex: Integer): Integer;
begin
  if (AIndex >= 0) and (AIndex < Length(FStages)) then
    Result := Length(FStages[AIndex].Members)
  else
    Result := 0;
end;

function TSedaiPatchGraph.Describe: string;
var
  S, I, NCyc, NMod: Integer;
  Line: string;
begin
  Result := '';
  NCyc := 0; NMod := 0;
  for S := 0 to High(FStages) do
  begin
    Line := '';
    for I := 0 to High(FStages[S].Members) do
    begin
      if Line <> '' then Line := Line + ' + ';
      Line := Line + FModules[FStages[S].Members[I]].ModuleName;
      Inc(NMod);
    end;
    if FStages[S].IsCycle then
    begin
      Inc(NCyc);
      if FStages[S].ChunkSize > 1 then
        Result := Result + Format('  stage %d  [chunk %d]     %s'#10,
                                  [S, FStages[S].ChunkSize, Line])
      else
        Result := Result + Format('  stage %d  [per sample]  %s'#10, [S, Line]);
    end
    else
      Result := Result + Format('  stage %d  [block]       %s'#10, [S, Line]);
  end;
  Result := Result + Format('  %d modules in %d stages, %d of which need sample rate'#10,
                            [NMod, Length(FStages), NCyc]);
end;

end.
