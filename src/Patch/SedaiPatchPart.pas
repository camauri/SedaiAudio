// ============================================================================
// SedaiPatchPart — the whole instrument library, patchable.
//
// SedaiPatchInstruments wraps five generators that make sound from nothing.
// The other six techniques — classic, FM, wavetable, additive, partial, sample,
// SID — need DATA: a table, harmonic tracks, a recording. That data already has
// a home, TSAFPart plus the .safinst registry, which is exactly the pairing the
// preset system was built for. So rather than teach the patch file to parse
// harmonic tracks, this module hands the job to the machinery that already does
// it, and one module type reaches all eleven techniques and all nine libraries.
//
//   module s = inst instrument="Drawbar Organ"
//   module s = inst library=library/orchestra.safinst instrument=Violin
//   module s = inst source=psAdditive preset=strings
//
// Two consequences worth stating rather than discovering:
//
//   BLOCK-ONLY. A Part renders a block at a time, so it declares supports=block
//   and the graph refuses to put it in a feedback cycle. Gate edges are still
//   sample-accurate — the block is split at each edge — but a Part cannot be
//   advanced one sample at a time and this does not pretend otherwise.
//
//   PITCH IS LATCHED AT THE TRIGGER. A Part hands the frequency to the voice at
//   note-on; Karplus in particular bakes it into the delay line right there. So
//   an LFO on the pitch input of an inst module sets the note, it does not bend
//   it. The five native instrument modules do bend — use those when the pitch
//   has to move while the note sounds.
//
// The Part is created with ONE voice. Polyphony belongs to the patch voice pool,
// which already runs N independent graphs; a polyphonic Part inside each of them
// would be polyphony twice over.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchPart;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math, SedaiAudioTypes, SedaiPatchGraph, SedaiPart,
  SedaiInstrumentPreset;

type
  { TSedaiModPart }

  TSedaiModPart = class(TSedaiPatchModule)
  private
    FPart: TSAFPart;
    FPitchIn, FGateIn, FAmpIn, FOut, FOutR: TSedaiPatchPort;
    FScratch: array of Single;
    FLastGate: Single;
    FBaseFreq: Single;
    FSounding: Boolean;
    FSource: TSAFPartSource;
    FPreset: string;
    FInstrument: string;
    FDirty: Boolean;
    procedure ApplySelection;
    procedure RenderSegment(AFrom, ACount: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer); override;
    procedure ResetState; override;
    function Configure(const AKey, AValue: string): Boolean; override;
    procedure RenderSample(AIndex: Integer); override;
    procedure RenderBlock(ACount: Integer); override;
  end;

function CreatePartModuleByType(const ATypeName: string): TSedaiPatchModule;
function KnownPartTypes: string;
// Libraries are loaded into the global registry once each, however many voices
// or patches ask for them.
function EnsureLibraryLoaded(const APath: string): Boolean;
// The engine's own name mapper is private to its unit, and its names carry the
// 'ps' prefix from the enum. A patch file should read like a patch file, so
// this takes 'additive' as readily as 'psAdditive'.
function PatchTechniqueFromName(const AName: string; out ASource: TSAFPartSource): Boolean;

implementation

var
  GLoaded: TStringList = nil;

function EnsureLibraryLoaded(const APath: string): Boolean;
var
  Full: string;
begin
  Full := ExpandFileName(APath);
  if GLoaded = nil then
  begin
    GLoaded := TStringList.Create;
    GLoaded.Sorted := True;
    GLoaded.Duplicates := dupIgnore;
  end;
  if GLoaded.IndexOf(Full) >= 0 then Exit(True);
  if not FileExists(Full) then Exit(False);
  Result := InstrumentRegistry.LoadFromFile(Full) > 0;
  if Result then GLoaded.Add(Full);
end;

function PatchTechniqueFromName(const AName: string; out ASource: TSAFPartSource): Boolean;
var
  N: string;
begin
  N := LowerCase(Trim(AName));
  if Copy(N, 1, 2) = 'ps' then N := Copy(N, 3, MaxInt);
  Result := True;
  if N = 'classic' then ASource := psClassic
  else if N = 'fm' then ASource := psFM
  else if N = 'wavetable' then ASource := psWavetable
  else if N = 'additive' then ASource := psAdditive
  else if N = 'sample' then ASource := psSample
  else if N = 'karplus' then ASource := psKarplus
  else if N = 'sid' then ASource := psSID
  else if N = 'partial' then ASource := psPartial
  else if N = 'reed' then ASource := psReed
  else if N = 'bowed' then ASource := psBowed
  else if N = 'modal' then ASource := psModal
  else Result := False;
end;

{ TSedaiModPart }

constructor TSedaiModPart.Create;
begin
  inherited Create;
  TypeName := 'inst';
  Rate := mrBlockOnly;
  FBaseFreq := 220.0;
  FSource := psClassic;
  FPreset := 'Saw';
  FInstrument := '';
  FDirty := True;
  FPart := TSAFPart.Create(1);
  FPitchIn := AddInput('pitch', prPitch, 0.0);
  FGateIn  := AddInput('gate', prGate, 0.0);
  // Output trim. Measured across the built-in library a bare Part peaks between
  // 1.01 and 1.47, so at unity a plain `module s = inst` would clip before the
  // patch had done anything. 0.6 puts the whole library under full scale and
  // near the level of the native instrument modules; it only sets the DEFAULT
  // of amp, which the patch still owns.
  FAmpIn   := AddInput('amp', prUnipolar, 0.6);
  FAmpIn.Min := 0.0; FAmpIn.Max := 8.0;
  FOut     := AddOutput('out', prAudio);
  FOutR    := AddOutput('outR', prAudio);
end;

destructor TSedaiModPart.Destroy;
begin
  FPart.Free;
  inherited Destroy;
end;

procedure TSedaiModPart.ApplySelection;
begin
  FDirty := False;
  // A named instrument wins: it carries its own technique and parameter block,
  // so setting a source alongside it would only be a way to contradict it.
  if FInstrument <> '' then
  begin
    if InstrumentRegistry.ApplyToPartByName(FInstrument, FPart) then Exit;
    raise Exception.CreateFmt(
      'unknown instrument "%s" — load its library first (library=<path.safinst>)',
      [FInstrument]);
  end;
  FPart.SetInstrument(FSource, FPreset);
end;

procedure TSedaiModPart.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
begin
  inherited Prepare(ASampleRate, ABlockSize);
  FPart.SetSampleRate(ASampleRate);
  if ABlockSize > 0 then SetLength(FScratch, ABlockSize * 2);
  // After the sample rate, because a preset's times are in seconds.
  ApplySelection;
end;

procedure TSedaiModPart.ResetState;
begin
  inherited ResetState;
  FLastGate := 0.0;
  FSounding := False;
  FPart.AllNotesOff;
end;

function TSedaiModPart.Configure(const AKey, AValue: string): Boolean;
var
  V: string;
  F: Single;
  FS: TFormatSettings;
begin
  V := Trim(AValue);
  // Quotes are how an instrument name with a space survives the parser.
  if (Length(V) >= 2) and (V[1] = '"') and (V[Length(V)] = '"') then
    V := Copy(V, 2, Length(V) - 2);

  if SameText(AKey, 'library') then
  begin
    if not EnsureLibraryLoaded(V) then
      raise Exception.CreateFmt('cannot load instrument library "%s"', [V]);
    Exit(True);
  end;
  if SameText(AKey, 'instrument') then
  begin
    FInstrument := V; FDirty := True; Exit(True);
  end;
  if SameText(AKey, 'source') then
  begin
    if not PatchTechniqueFromName(V, FSource) then
      raise Exception.CreateFmt('unknown technique "%s" — one of: classic, fm, '
        + 'wavetable, additive, sample, karplus, sid, partial, reed, bowed, modal',
        [V]);
    FDirty := True; Exit(True);
  end;
  if SameText(AKey, 'preset') then
  begin
    FPreset := V; FDirty := True; Exit(True);
  end;
  if SameText(AKey, 'freq') then
  begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := '.';
    if TryStrToFloat(V, F, FS) then FBaseFreq := F;
    Exit(True);
  end;
  Result := inherited Configure(AKey, AValue);
end;

procedure TSedaiModPart.RenderSample(AIndex: Integer);
begin
  raise Exception.Create('inst is block-only; the graph should never single-step it');
end;

procedure TSedaiModPart.RenderSegment(AFrom, ACount: Integer);
var
  I: Integer;
  A: Single;
begin
  if ACount <= 0 then Exit;
  FPart.RenderBlock(@FScratch[0], ACount);
  for I := 0 to ACount - 1 do
  begin
    A := FAmpIn.Read(AFrom + I);
    FOut.Write(AFrom + I, FScratch[I * 2] * A);
    FOutR.Write(AFrom + I, FScratch[I * 2 + 1] * A);
  end;
end;

procedure TSedaiModPart.RenderBlock(ACount: Integer);
var
  I, SegStart: Integer;
  G, Volts, Freq: Single;
begin
  if ACount <= 0 then Exit;
  if Length(FScratch) < ACount * 2 then SetLength(FScratch, ACount * 2);
  if FDirty then ApplySelection;

  // Split the block at every gate edge, so a note starts on the sample it was
  // asked for rather than on the next block boundary. A Part cannot be stepped
  // one sample at a time, but it can be rendered in pieces.
  SegStart := 0;
  for I := 0 to ACount - 1 do
  begin
    G := FGateIn.Read(I);
    if (G > 0.5) <> (FLastGate > 0.5) then
    begin
      RenderSegment(SegStart, I - SegStart);
      SegStart := I;
      if G > 0.5 then
      begin
        Volts := FPitchIn.Read(I);
        Freq := FBaseFreq * Power(2.0, Volts);
        if Freq < 1.0 then Freq := 1.0
        else if Freq > FSR * 0.45 then Freq := FSR * 0.45;
        FPart.NoteOnFreq(Freq, 1.0);
        FSounding := True;
      end
      else if FSounding then
      begin
        // One voice, so there is exactly one note to release and no bookkeeping
        // is needed to say which.
        FPart.AllNotesOff;
        FSounding := False;
      end;
    end;
    FLastGate := G;
  end;
  RenderSegment(SegStart, ACount - SegStart);
end;

{ factory }

function CreatePartModuleByType(const ATypeName: string): TSedaiPatchModule;
begin
  if SameText(ATypeName, 'inst') then Result := TSedaiModPart.Create
  else Result := nil;
end;

function KnownPartTypes: string;
begin
  Result := 'inst (instrument=<name> | source=<technique> preset=<key>, library=<path.safinst>)';
end;

initialization

finalization
  FreeAndNil(GLoaded);

end.
