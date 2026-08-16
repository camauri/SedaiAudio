// ============================================================================
// SedaiPatchVoices — a patch is a VOICE TEMPLATE; this plays several at once.
//
// The pool holds N independent graphs built from the same patch text, each with
// its own module instances and therefore its own oscillator phases, filter state
// and envelope. That is the whole reason a chord does not sound like one note
// three times as loud: nothing is shared, so nothing phase-locks.
//
// Building N graphs from the same source is deliberate rather than cloning a
// compiled one: a module carries DSP state, and a deep copy would have to know
// how to duplicate each kind. Parsing is cheap and happens once at load.
//
// Voice stealing is by age. A voice is free again once its gate is closed AND
// its output has fallen below the silence floor for a whole block — which asks
// the audio rather than the envelope, so it works for any patch, including ones
// whose tail is a delay or a resonant filter ringing rather than an ADSR.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchVoices;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SedaiPatchGraph, SedaiPatchModules, SedaiPatchFile;

const
  SEDAI_PATCH_SILENCE = 1.0e-4;   // -80 dB: below this a released voice is done

type
  TSedaiPatchVoice = class
  private
    FGraph: TSedaiPatchGraph;
    FNote: TSedaiModNote;
    FActive: Boolean;
    FGateOpen: Boolean;
    FMidiNote: Integer;
    FAge: QWord;
    FQuietBlocks: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    property Graph: TSedaiPatchGraph read FGraph;
    property Active: Boolean read FActive;
    property MidiNote: Integer read FMidiNote;
  end;

  { TSedaiPatchVoicePool }

  TSedaiPatchVoicePool = class
  private
    FVoices: array of TSedaiPatchVoice;
    FMix: array of array of Single;   // [channel][sample]
    FSampleRate: Cardinal;
    FBlockSize: Integer;
    FClock: QWord;
    FLastError: string;
    FForceSampleRate: Boolean;
    FMasterGain: Single;
    function FindFree: Integer;
    function FindOldest: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    // Build APolyphony independent instances of the same patch file.
    function LoadFromFile(const AFilename: string; APolyphony: Integer): Boolean;
    procedure Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
    procedure Reset;

    // MIDI note numbers; 60 = middle C, which the patch's own base frequency
    // decides the pitch of. Pitch reaches the graph as volts per octave.
    procedure NoteOn(AMidiNote: Integer);
    procedure NoteOff(AMidiNote: Integer);
    procedure AllNotesOff;

    // Render ACount frames of the mix. Only voices that are actually sounding
    // are walked, so an idle pool costs nothing.
    procedure Render(ACount: Integer);
    function MixSample(AChannel, AIndex: Integer): Single; inline;
    function OutputCount: Integer;
    function ActiveVoices: Integer;

    function Describe: string;

    function VoiceCount: Integer;
    property LastError: string read FLastError;
    property MasterGain: Single read FMasterGain write FMasterGain;
  end;

implementation

{ TSedaiPatchVoice }

constructor TSedaiPatchVoice.Create;
begin
  inherited Create;
  FGraph := TSedaiPatchGraph.Create;
  FNote := nil;
  FActive := False;
  FGateOpen := False;
  FMidiNote := -1;
  FAge := 0;
  FQuietBlocks := 0;
end;

destructor TSedaiPatchVoice.Destroy;
begin
  FGraph.Free;
  inherited Destroy;
end;

{ TSedaiPatchVoicePool }

constructor TSedaiPatchVoicePool.Create;
begin
  inherited Create;
  SetLength(FVoices, 0);
  FSampleRate := 44100;
  FBlockSize := 256;
  FClock := 0;
  FMasterGain := 1.0;
  FForceSampleRate := False;
end;

destructor TSedaiPatchVoicePool.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do FVoices[I].Free;
  SetLength(FVoices, 0);
  inherited Destroy;
end;

function TSedaiPatchVoicePool.LoadFromFile(const AFilename: string;
  APolyphony: Integer): Boolean;
var
  I: Integer;
  Res: TSedaiPatchLoadResult;
  V: TSedaiPatchVoice;
  M: TSedaiPatchModule;
begin
  Result := False;
  FLastError := '';
  if APolyphony < 1 then APolyphony := 1;

  for I := 0 to High(FVoices) do FVoices[I].Free;
  SetLength(FVoices, 0);

  for I := 0 to APolyphony - 1 do
  begin
    V := TSedaiPatchVoice.Create;
    Res := LoadPatchFromFile(V.FGraph, AFilename);
    if not Res.Success then
    begin
      if Res.ErrorLine > 0 then
        FLastError := Format('%s:%d: %s', [AFilename, Res.ErrorLine, Res.ErrorText])
      else
        FLastError := Res.ErrorText;
      V.Free;
      Exit;
    end;
    FForceSampleRate := Res.ForceSampleRate;
    if not V.FGraph.Compile(FForceSampleRate) then
    begin
      FLastError := V.FGraph.LastError;
      V.Free;
      Exit;
    end;
    M := V.FGraph.ModuleByName('note');
    if M is TSedaiModNote then V.FNote := TSedaiModNote(M);

    SetLength(FVoices, Length(FVoices) + 1);
    FVoices[High(FVoices)] := V;
  end;

  if (Length(FVoices) > 0) and (FVoices[0].FNote = nil) then
    // Not fatal: a drone patch with no keyboard is perfectly legitimate, it just
    // cannot be played from one.
    FLastError := 'note: the patch has no module named "note", so it cannot be played by note';

  Result := True;
end;

procedure TSedaiPatchVoicePool.Prepare(ASampleRate: Cardinal; ABlockSize: Integer);
var
  I: Integer;
begin
  FSampleRate := ASampleRate;
  FBlockSize := ABlockSize;
  SetLength(FMix, OutputCount);
  for I := 0 to High(FMix) do
    if Length(FMix[I]) < ABlockSize then SetLength(FMix[I], ABlockSize);
  for I := 0 to High(FVoices) do
  begin
    FVoices[I].FGraph.Prepare(ASampleRate, ABlockSize);
    FVoices[I].FGraph.ResetState;
  end;
end;

procedure TSedaiPatchVoicePool.Reset;
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
  begin
    FVoices[I].FGraph.ResetState;
    FVoices[I].FActive := False;
    FVoices[I].FGateOpen := False;
    FVoices[I].FMidiNote := -1;
    FVoices[I].FQuietBlocks := 0;
  end;
  FClock := 0;
end;

function TSedaiPatchVoicePool.FindFree: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FVoices) do
    if not FVoices[I].FActive then Exit(I);
end;

function TSedaiPatchVoicePool.FindOldest: Integer;
var
  I: Integer;
  Best: QWord;
begin
  Result := 0;
  if Length(FVoices) = 0 then Exit(-1);
  Best := FVoices[0].FAge;
  for I := 1 to High(FVoices) do
    if FVoices[I].FAge < Best then
    begin
      Best := FVoices[I].FAge;
      Result := I;
    end;
end;

procedure TSedaiPatchVoicePool.NoteOn(AMidiNote: Integer);
var
  Idx: Integer;
begin
  if Length(FVoices) = 0 then Exit;
  Idx := FindFree;
  if Idx < 0 then
  begin
    Idx := FindOldest;                   // steal by age
    FVoices[Idx].FGraph.ResetState;      // a stolen voice starts clean
  end;
  Inc(FClock);
  FVoices[Idx].FActive := True;
  FVoices[Idx].FGateOpen := True;
  FVoices[Idx].FMidiNote := AMidiNote;
  FVoices[Idx].FAge := FClock;
  FVoices[Idx].FQuietBlocks := 0;
  if FVoices[Idx].FNote <> nil then
    // 60 = the patch's own base frequency; a semitone is 1/12 of a volt.
    FVoices[Idx].FNote.SetNote((AMidiNote - 60) / 12.0, 1.0);
end;

procedure TSedaiPatchVoicePool.NoteOff(AMidiNote: Integer);
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
    if FVoices[I].FActive and FVoices[I].FGateOpen and
       (FVoices[I].FMidiNote = AMidiNote) then
    begin
      FVoices[I].FGateOpen := False;
      if FVoices[I].FNote <> nil then
        FVoices[I].FNote.SetNote((AMidiNote - 60) / 12.0, 0.0);
    end;
end;

procedure TSedaiPatchVoicePool.AllNotesOff;
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
    if FVoices[I].FActive and FVoices[I].FGateOpen then
    begin
      FVoices[I].FGateOpen := False;
      if FVoices[I].FNote <> nil then
        FVoices[I].FNote.SetNote(FVoices[I].FNote.Pitch, 0.0);
    end;
end;

procedure TSedaiPatchVoicePool.Render(ACount: Integer);
var
  I, K, C, NCh: Integer;
  V: TSedaiPatchVoice;
  S, Peak: Single;
begin
  NCh := OutputCount;
  if Length(FMix) < NCh then SetLength(FMix, NCh);
  for C := 0 to NCh - 1 do
  begin
    if Length(FMix[C]) < ACount then SetLength(FMix[C], ACount);
    for K := 0 to ACount - 1 do FMix[C][K] := 0.0;
  end;

  for I := 0 to High(FVoices) do
  begin
    V := FVoices[I];
    if not V.FActive then Continue;      // an idle voice costs nothing

    V.FGraph.Render(ACount);
    Peak := 0.0;
    for C := 0 to NCh - 1 do
      for K := 0 to ACount - 1 do
      begin
        S := V.FGraph.OutputSample(C, K);
        if Abs(S) > Peak then Peak := Abs(S);
        FMix[C][K] := FMix[C][K] + S;
      end;

    // Retire the voice when the gate is shut and the sound has actually gone,
    // measured on the output rather than guessed from the envelope.
    if (not V.FGateOpen) and (Peak < SEDAI_PATCH_SILENCE) then
    begin
      Inc(V.FQuietBlocks);
      if V.FQuietBlocks >= 2 then
      begin
        V.FActive := False;
        V.FMidiNote := -1;
        V.FGraph.ResetState;
      end;
    end
    else
      V.FQuietBlocks := 0;
  end;

  if FMasterGain <> 1.0 then
    for C := 0 to NCh - 1 do
      for K := 0 to ACount - 1 do FMix[C][K] := FMix[C][K] * FMasterGain;
end;

function TSedaiPatchVoicePool.MixSample(AChannel, AIndex: Integer): Single;
begin
  if (AChannel >= 0) and (AChannel < Length(FMix)) and
     (AIndex >= 0) and (AIndex < Length(FMix[AChannel])) then
    Result := FMix[AChannel][AIndex]
  else
    Result := 0.0;
end;

// Every voice is the same patch, so they all declare the same channel count.
function TSedaiPatchVoicePool.OutputCount: Integer;
begin
  if Length(FVoices) = 0 then Result := 1
  else Result := FVoices[0].FGraph.OutputCount;
end;

function TSedaiPatchVoicePool.VoiceCount: Integer;
begin
  Result := Length(FVoices);
end;

function TSedaiPatchVoicePool.ActiveVoices: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FVoices) do
    if FVoices[I].FActive then Inc(Result);
end;

function TSedaiPatchVoicePool.Describe: string;
begin
  if Length(FVoices) = 0 then Exit('  (no voices)');
  Result := Format('  %d voices, each an independent instance of the patch'#10,
                   [Length(FVoices)]) + FVoices[0].FGraph.Describe;
end;

end.
