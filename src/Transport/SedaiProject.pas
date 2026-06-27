{*
 * Sedai Audio Foundation - Project
 *
 * TSedaiProject represents a complete DAW project with tracks,
 * mixer, transport, and project-wide settings.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject,
  SedaiTransport, SedaiTrack, SedaiClip, SedaiMixer, SedaiMixerChannel,
  SedaiBus, SedaiAudioBuffer;

const
  MAX_TRACKS = 256;
  MAX_INSTRUMENTS = 64;

type
  // Project state
  TProjectState = (
    psNew,                // New unsaved project
    psSaved,              // Saved, no changes
    psModified            // Has unsaved changes
  );

  // Undo action type
  TUndoActionType = (
    uatClipAdd,
    uatClipRemove,
    uatClipMove,
    uatClipSplit,
    uatTrackAdd,
    uatTrackRemove,
    uatParameterChange,
    uatAutomationChange
  );

  // Undo action record
  TUndoAction = record
    ActionType: TUndoActionType;
    Description: string;
    TrackIndex: Integer;
    ClipIndex: Integer;
    OldValue: Variant;
    NewValue: Variant;
    Timestamp: TDateTime;
  end;

  { TSedaiProject }
  // Complete DAW project
  TSedaiProject = class(TSedaiAudioObject)
  private
    // Project info
    FName: string;
    FFilePath: string;
    FState: TProjectState;
    FCreated: TDateTime;
    FModified: TDateTime;

    // Components
    FTransport: TSedaiTransport;
    FMixer: TSedaiMixer;

    // Tracks
    FTracks: array[0..MAX_TRACKS-1] of TSedaiTrack;
    FTrackCount: Integer;

    // Undo/Redo
    FUndoStack: array of TUndoAction;
    FUndoCount: Integer;
    FUndoPosition: Integer;  // Current position in stack
    FMaxUndoLevels: Integer;

    // Project settings
    FBitDepth: Integer;          // 16, 24, or 32
    FProjectLength: Int64;       // Project length in samples

    // Processing buffers
    FMasterBuffer: array of Single;
    FProjectBlockSize: Integer;

    procedure MarkModified;
    procedure EnsureBufferSize(AFrameCount: Integer);

    // Native .safproj text format (the default Save/Load implementation).
    function SaveNative(APath: string): Boolean;
    function LoadNative(APath: string): Boolean;

    // Apply (redo) or revert (undo) a single undo action. Value-based actions
    // (clip move) reverse fully from Old/NewValue; structural actions that need
    // the affected object retained are not yet applied (thin TUndoAction).
    function ApplyUndoAction(const AAction: TUndoAction; AUndo: Boolean): Boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure SampleRateChanged; override;

    // ========================================================================
    // PROJECT MANAGEMENT
    // ========================================================================

    // New project
    procedure NewProject(AName: string = 'Untitled');

    // Save/Load (stub - actual implementation would use file I/O)
    function SaveProject(APath: string): Boolean;
    function LoadProject(APath: string): Boolean;

    // ========================================================================
    // TRACK MANAGEMENT
    // ========================================================================

    // Add tracks
    function AddAudioTrack(AName: string = ''): TSedaiAudioTrack;
    function AddMIDITrack(AName: string = ''): TSedaiMIDITrack;

    // Remove track
    procedure RemoveTrack(AIndex: Integer);

    // Get track
    function GetTrack(AIndex: Integer): TSedaiTrack;

    // Move track (reorder)
    procedure MoveTrack(AFromIndex, AToIndex: Integer);

    // ========================================================================
    // PLAYBACK
    // ========================================================================

    // Process audio for playback
    procedure ProcessBlock(AOutput: PSingle; AFrameCount: Integer);

    // ========================================================================
    // UNDO/REDO
    // ========================================================================

    // Record action for undo
    procedure RecordUndoAction(AType: TUndoActionType; ADescription: string;
                               ATrackIdx, AClipIdx: Integer;
                               AOldValue, ANewValue: Variant);

    // Undo last action
    function Undo: Boolean;

    // Redo last undone action
    function Redo: Boolean;

    // Check if undo/redo available
    function CanUndo: Boolean;
    function CanRedo: Boolean;

    // Get undo/redo descriptions
    function GetUndoDescription: string;
    function GetRedoDescription: string;

    // Clear undo history
    procedure ClearUndoHistory;

    // ========================================================================
    // UTILITY
    // ========================================================================

    // Get project duration in seconds
    function GetDurationSeconds: Double;

    // Set project length from duration
    procedure SetDurationSeconds(ASeconds: Double);

    // Get total clip count across all tracks
    function GetTotalClipCount: Integer;

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    property Name: string read FName write FName;
    property FilePath: string read FFilePath;
    property State: TProjectState read FState;
    property Created: TDateTime read FCreated;
    property Modified: TDateTime read FModified;

    property Transport: TSedaiTransport read FTransport;
    property Mixer: TSedaiMixer read FMixer;

    property TrackCount: Integer read FTrackCount;
    property BitDepth: Integer read FBitDepth write FBitDepth;
    property ProjectLength: Int64 read FProjectLength write FProjectLength;
    property MaxUndoLevels: Integer read FMaxUndoLevels write FMaxUndoLevels;
  end;

implementation

{ TSedaiProject }

constructor TSedaiProject.Create;
var
  I: Integer;
begin
  inherited Create;

  FName := 'Untitled';
  FFilePath := '';
  FState := psNew;
  FCreated := Now;
  FModified := Now;

  // Create transport
  FTransport := TSedaiTransport.Create;

  // Create mixer
  FMixer := TSedaiMixer.Create;

  // Initialize tracks
  FTrackCount := 0;
  for I := 0 to MAX_TRACKS - 1 do
    FTracks[I] := nil;

  // Undo system
  FUndoCount := 0;
  FUndoPosition := 0;
  FMaxUndoLevels := 100;

  // Settings
  FBitDepth := 24;
  FProjectLength := Round(FSampleRate * 300);  // 5 minutes default

  FProjectBlockSize := 0;
end;

destructor TSedaiProject.Destroy;
var
  I: Integer;
begin
  // Free tracks
  for I := 0 to MAX_TRACKS - 1 do
    if Assigned(FTracks[I]) then
      FTracks[I].Free;

  FMixer.Free;
  FTransport.Free;

  SetLength(FUndoStack, 0);
  SetLength(FMasterBuffer, 0);

  inherited Destroy;
end;

procedure TSedaiProject.Reset;
begin
  inherited Reset;

  FTransport.Reset;
  FMixer.Reset;
end;

procedure TSedaiProject.SampleRateChanged;
var
  I: Integer;
  Rate: Cardinal;
begin
  inherited SampleRateChanged;
  Rate := SampleRate;

  FTransport.SetSampleRate(Rate);
  FMixer.SetSampleRate(Rate);

  for I := 0 to MAX_TRACKS - 1 do
    if Assigned(FTracks[I]) then
      FTracks[I].SetSampleRate(Rate);
end;

procedure TSedaiProject.MarkModified;
begin
  FState := psModified;
  FModified := Now;
end;

procedure TSedaiProject.EnsureBufferSize(AFrameCount: Integer);
begin
  if FProjectBlockSize < AFrameCount then
  begin
    FProjectBlockSize := AFrameCount;
    SetLength(FMasterBuffer, AFrameCount * 2);
  end;
end;

procedure TSedaiProject.NewProject(AName: string);
var
  I: Integer;
begin
  // Clear existing tracks
  for I := 0 to MAX_TRACKS - 1 do
  begin
    if Assigned(FTracks[I]) then
    begin
      FTracks[I].Free;
      FTracks[I] := nil;
    end;
  end;
  FTrackCount := 0;

  // Reset mixer
  FMixer.Free;
  FMixer := TSedaiMixer.Create;
  FMixer.SetSampleRate(FSampleRate);

  // Reset transport
  FTransport.Reset;

  // Clear undo
  ClearUndoHistory;

  // Set project info
  FName := AName;
  FFilePath := '';
  FState := psNew;
  FCreated := Now;
  FModified := Now;
end;

// Locale-independent float format for the file (always '.' decimal separator).
function ProjectFloatFmt: TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DecimalSeparator := '.';
  Result.ThousandSeparator := #0;
end;

function TSedaiProject.SaveNative(APath: string): Boolean;
var
  L: TStringList;
  Fmt: TFormatSettings;
  I, J, K: Integer;
  Trk: TSedaiTrack;
  Ch: TSedaiMixerChannel;
  AClip: TSedaiAudioClip;
  MClip: TSedaiMIDIClip;
  Clip: TSedaiClip;
  Note: TMIDINoteEvent;

  function F(V: Single): string;
  begin
    Result := FloatToStr(V, Fmt);
  end;

begin
  Result := False;
  Fmt := ProjectFloatFmt;
  L := TStringList.Create;
  try
    // Header + project settings
    L.Add('SAFPROJECT 1');
    L.Add('NAME ' + FName);
    L.Add('TEMPO ' + F(FTransport.BaseTempo));
    L.Add(Format('TIMESIG %d %d', [FTransport.TimeSignature.Numerator,
                                    FTransport.TimeSignature.Denominator]));
    L.Add('LENGTH ' + IntToStr(FProjectLength));
    L.Add('BITDEPTH ' + IntToStr(FBitDepth));

    // Mixer channels (settings restored after the tracks recreate them)
    for I := 0 to MAX_MIXER_CHANNELS - 1 do
    begin
      Ch := FMixer.GetChannel(I);
      if Assigned(Ch) then
        L.Add(Format('CH %d %s %s %d',
          [I, F(Ch.Volume), F(Ch.Pan), Ord(Ch.Muted)]));
    end;

    // Tracks (name last so it may contain spaces)
    for I := 0 to MAX_TRACKS - 1 do
    begin
      Trk := FTracks[I];
      if not Assigned(Trk) then Continue;

      if Trk is TSedaiMIDITrack then
        L.Add(Format('TRACK midi %d %d %s',
          [Trk.MixerChannelIndex, Ord(Trk.Muted), Trk.Name]))
      else
        L.Add(Format('TRACK audio %d %d %s',
          [Trk.MixerChannelIndex, Ord(Trk.Muted), Trk.Name]));

      for J := 0 to MAX_CLIPS_PER_TRACK - 1 do
      begin
        Clip := Trk.GetClip(J);
        if not Assigned(Clip) then Continue;

        if Clip is TSedaiAudioClip then
        begin
          AClip := TSedaiAudioClip(Clip);
          // Audio samples are not embedded yet (no source-path field) -> metadata only.
          L.Add(Format('ACLIP %d %d %d %s',
            [AClip.StartPosition, AClip.ClipLength, AClip.SourceOffset, F(AClip.Gain)]));
        end
        else if Clip is TSedaiMIDIClip then
        begin
          MClip := TSedaiMIDIClip(Clip);
          L.Add(Format('MCLIP %d %d %d',
            [MClip.StartPosition, MClip.ClipLength, MClip.NoteCount]));
          for K := 0 to MClip.NoteCount - 1 do
          begin
            Note := MClip.GetNote(K);
            L.Add(Format('NOTE %d %d %d %d %d',
              [Note.Position, Note.Duration, Note.Note, Note.Velocity, Note.Channel]));
          end;
        end;
      end;
      L.Add('ENDTRACK');
    end;
    L.Add('END');

    L.SaveToFile(APath);
    Result := True;
  finally
    L.Free;
  end;
end;

function TSedaiProject.LoadNative(APath: string): Boolean;
var
  L: TStringList;
  Fmt: TFormatSettings;
  LineNo, P: Integer;
  Line, Tok, Rest: string;
  ATrk: TSedaiAudioTrack;
  MTrk: TSedaiMIDITrack;
  CurTrack: TSedaiTrack;
  AClip: TSedaiAudioClip;
  MClip: TSedaiMIDIClip;
  ChIdx: Integer;
  Ch: TSedaiMixerChannel;
  // Pending channel settings, applied after tracks recreate the channels.
  ChI: array of Integer;
  ChV, ChP: array of Single;
  ChM: array of Boolean;
  ChN: Integer;
  // Scratch locals: NextXxx have side effects, so multi-arg calls must read each
  // token into a local first (argument evaluation order is unspecified in FPC).
  sv1, sv2, sv3, sv4, sv5: Int64;

  // Pop the next whitespace-delimited token from Rest.
  function NextTok: string;
  var Sp: Integer;
  begin
    Rest := TrimLeft(Rest);
    Sp := Pos(' ', Rest);
    if Sp = 0 then
    begin
      Result := Rest; Rest := '';
    end
    else
    begin
      Result := Copy(Rest, 1, Sp - 1);
      Rest := Copy(Rest, Sp + 1, MaxInt);
    end;
  end;

  function NextInt: Integer;
  begin
    Result := StrToIntDef(NextTok, 0);
  end;

  function NextI64: Int64;
  begin
    Result := StrToInt64Def(NextTok, 0);
  end;

  function NextFloat: Single;
  begin
    Result := StrToFloatDef(NextTok, 0.0, Fmt);
  end;

begin
  Result := False;
  if not FileExists(APath) then Exit;

  Fmt := ProjectFloatFmt;
  ChN := 0;
  CurTrack := nil;
  MClip := nil;

  L := TStringList.Create;
  try
    L.LoadFromFile(APath);
    if (L.Count = 0) or (Copy(L[0], 1, 10) <> 'SAFPROJECT') then Exit;

    NewProject('Untitled');  // clears tracks/mixer/undo

    for LineNo := 1 to L.Count - 1 do
    begin
      Line := Trim(L[LineNo]);
      if Line = '' then Continue;

      Rest := Line;
      Tok := NextTok;

      if Tok = 'NAME' then
        FName := Rest
      else if Tok = 'TEMPO' then
        FTransport.BaseTempo := NextFloat
      else if Tok = 'TIMESIG' then
      begin
        sv1 := NextInt; sv2 := NextInt;
        FTransport.SetTimeSignature(sv1, sv2);
      end
      else if Tok = 'LENGTH' then
      begin
        FProjectLength := NextI64;
        FTransport.Length := FProjectLength;
      end
      else if Tok = 'BITDEPTH' then
        FBitDepth := NextInt
      else if Tok = 'CH' then
      begin
        SetLength(ChI, ChN + 1); SetLength(ChV, ChN + 1);
        SetLength(ChP, ChN + 1); SetLength(ChM, ChN + 1);
        ChI[ChN] := NextInt; ChV[ChN] := NextFloat;
        ChP[ChN] := NextFloat; ChM[ChN] := NextInt <> 0;
        Inc(ChN);
      end
      else if Tok = 'TRACK' then
      begin
        Tok := NextTok;          // type
        ChIdx := NextInt;        // (declared channel; recreated tracks reassign)
        P := NextInt;            // mute
        if Tok = 'midi' then
        begin
          MTrk := AddMIDITrack(Rest);
          CurTrack := MTrk;
        end
        else
        begin
          ATrk := AddAudioTrack(Rest);
          CurTrack := ATrk;
        end;
        if Assigned(CurTrack) then CurTrack.Muted := P <> 0;
      end
      else if Tok = 'ACLIP' then
      begin
        if Assigned(CurTrack) then
        begin
          AClip := TSedaiAudioClip.Create;
          AClip.StartPosition := NextI64;
          AClip.ClipLength := NextI64;
          AClip.SourceOffset := NextI64;
          AClip.Gain := NextFloat;
          CurTrack.AddClip(AClip);
        end;
      end
      else if Tok = 'MCLIP' then
      begin
        if Assigned(CurTrack) then
        begin
          MClip := TSedaiMIDIClip.Create;
          MClip.StartPosition := NextI64;
          MClip.ClipLength := NextI64;
          NextInt;  // note count (informational; notes follow)
          CurTrack.AddClip(MClip);
        end;
      end
      else if Tok = 'NOTE' then
      begin
        if Assigned(MClip) then
        begin
          sv1 := NextI64;  // position
          sv2 := NextI64;  // duration
          sv3 := NextInt;  // note
          sv4 := NextInt;  // velocity
          sv5 := NextInt;  // channel
          MClip.AddNote(sv1, sv2, sv3, sv4, sv5);
        end;
      end
      else if Tok = 'ENDTRACK' then
      begin
        CurTrack := nil;
        MClip := nil;
      end;
    end;

    // Apply saved channel settings now that the tracks recreated the channels.
    for P := 0 to ChN - 1 do
    begin
      Ch := FMixer.GetChannel(ChI[P]);
      if Assigned(Ch) then
      begin
        Ch.Volume := ChV[P];
        Ch.Pan := ChP[P];
        Ch.Muted := ChM[P];
      end;
    end;

    FFilePath := APath;
    FState := psSaved;
    Result := True;
  finally
    L.Free;
  end;
end;

function TSedaiProject.SaveProject(APath: string): Boolean;
begin
  // Format dispatch by extension (the seam for multi-format: .mid SMF, Dawproject
  // XML, etc. can plug in here). Default is the native .safproj text format.
  Result := SaveNative(APath);
  if Result then
  begin
    FFilePath := APath;
    FState := psSaved;
    FModified := Now;
  end;
end;

function TSedaiProject.LoadProject(APath: string): Boolean;
begin
  Result := LoadNative(APath);
end;

function TSedaiProject.AddAudioTrack(AName: string): TSedaiAudioTrack;
var
  I: Integer;
  Track: TSedaiAudioTrack;
  Channel: TSedaiMixerChannel;
begin
  Result := nil;

  // Find empty slot
  for I := 0 to MAX_TRACKS - 1 do
  begin
    if FTracks[I] = nil then
    begin
      // Create track
      Track := TSedaiAudioTrack.Create;
      Track.SetSampleRate(FSampleRate);
      Track.Index := I;

      if AName = '' then
        Track.Name := Format('Audio %d', [FTrackCount + 1])
      else
        Track.Name := AName;

      FTracks[I] := Track;
      Inc(FTrackCount);

      // Create corresponding mixer channel
      Channel := FMixer.AddChannel(Track.Name);
      if Assigned(Channel) then
        Track.MixerChannelIndex := Channel.Index;

      MarkModified;
      Result := Track;
      Exit;
    end;
  end;
end;

function TSedaiProject.AddMIDITrack(AName: string): TSedaiMIDITrack;
var
  I: Integer;
  Track: TSedaiMIDITrack;
  Channel: TSedaiMixerChannel;
begin
  Result := nil;

  for I := 0 to MAX_TRACKS - 1 do
  begin
    if FTracks[I] = nil then
    begin
      Track := TSedaiMIDITrack.Create;
      Track.SetSampleRate(FSampleRate);
      Track.Index := I;

      if AName = '' then
        Track.Name := Format('MIDI %d', [FTrackCount + 1])
      else
        Track.Name := AName;

      FTracks[I] := Track;
      Inc(FTrackCount);

      // Create corresponding mixer channel
      Channel := FMixer.AddChannel(Track.Name);
      if Assigned(Channel) then
        Track.MixerChannelIndex := Channel.Index;

      MarkModified;
      Result := Track;
      Exit;
    end;
  end;
end;

procedure TSedaiProject.RemoveTrack(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= MAX_TRACKS) then Exit;
  if FTracks[AIndex] = nil then Exit;

  // Remove mixer channel
  if FTracks[AIndex].MixerChannelIndex >= 0 then
    FMixer.RemoveChannel(FTracks[AIndex].MixerChannelIndex);

  // Free track
  FTracks[AIndex].Free;
  FTracks[AIndex] := nil;
  Dec(FTrackCount);

  MarkModified;
end;

function TSedaiProject.GetTrack(AIndex: Integer): TSedaiTrack;
begin
  if (AIndex >= 0) and (AIndex < MAX_TRACKS) then
    Result := FTracks[AIndex]
  else
    Result := nil;
end;

procedure TSedaiProject.MoveTrack(AFromIndex, AToIndex: Integer);
var
  Track: TSedaiTrack;
  I: Integer;
begin
  if (AFromIndex < 0) or (AFromIndex >= MAX_TRACKS) then Exit;
  if (AToIndex < 0) or (AToIndex >= MAX_TRACKS) then Exit;
  if FTracks[AFromIndex] = nil then Exit;
  if AFromIndex = AToIndex then Exit;

  Track := FTracks[AFromIndex];

  // Shift tracks
  if AFromIndex < AToIndex then
  begin
    for I := AFromIndex to AToIndex - 1 do
      FTracks[I] := FTracks[I + 1];
  end
  else
  begin
    for I := AFromIndex downto AToIndex + 1 do
      FTracks[I] := FTracks[I - 1];
  end;

  FTracks[AToIndex] := Track;
  Track.Index := AToIndex;

  MarkModified;
end;

procedure TSedaiProject.ProcessBlock(AOutput: PSingle; AFrameCount: Integer);
var
  I, J, Ch: Integer;
  Position: Int64;
  TrackInputs: array of PSingle;
  TrackBuffers: array of array of Single;
begin
  EnsureBufferSize(AFrameCount);

  // Get current position
  Position := FTransport.Position;

  // Inputs are indexed by MIXER CHANNEL SLOT (each track routes to its own
  // channel via MixerChannelIndex), not by a compacted track counter — the
  // mixer reads AInputs[channelSlot]. TrackBuffers holds the per-track storage.
  SetLength(TrackInputs, MAX_MIXER_CHANNELS);
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
    TrackInputs[I] := nil;
  SetLength(TrackBuffers, FTrackCount);

  // Render each track into its channel slot: audio tracks read their clips,
  // MIDI tracks play their clips through their instrument.
  J := 0;
  for I := 0 to MAX_TRACKS - 1 do
  begin
    if not Assigned(FTracks[I]) then Continue;
    if not ((FTracks[I] is TSedaiAudioTrack) or (FTracks[I] is TSedaiMIDITrack)) then
      Continue;

    Ch := FTracks[I].MixerChannelIndex;
    if (Ch < 0) or (Ch >= MAX_MIXER_CHANNELS) then Continue;

    SetLength(TrackBuffers[J], AFrameCount * 2);
    if FTracks[I] is TSedaiAudioTrack then
      FTracks[I].ReadAudio(Position, @TrackBuffers[J][0], AFrameCount, 2)
    else
      TSedaiMIDITrack(FTracks[I]).RenderInstrument(Position, @TrackBuffers[J][0], AFrameCount);

    TrackInputs[Ch] := @TrackBuffers[J][0];
    Inc(J);
  end;

  // Process through mixer
  if J > 0 then
    FMixer.ProcessBlock(TrackInputs, AOutput, AFrameCount)
  else
  begin
    // No tracks - silence
    for I := 0 to AFrameCount * 2 - 1 do
      AOutput[I] := 0.0;
  end;

  // Advance transport
  FTransport.AdvanceSamples(AFrameCount);
end;

procedure TSedaiProject.RecordUndoAction(AType: TUndoActionType;
  ADescription: string; ATrackIdx, AClipIdx: Integer;
  AOldValue, ANewValue: Variant);
begin
  // Truncate redo history
  if FUndoPosition < FUndoCount then
    FUndoCount := FUndoPosition;

  // Check max levels
  if FUndoCount >= FMaxUndoLevels then
  begin
    // Remove oldest action
    Move(FUndoStack[1], FUndoStack[0], (FMaxUndoLevels - 1) * SizeOf(TUndoAction));
    Dec(FUndoCount);
    Dec(FUndoPosition);
  end;

  // Add new action
  Inc(FUndoCount);
  SetLength(FUndoStack, FUndoCount);

  with FUndoStack[FUndoCount - 1] do
  begin
    ActionType := AType;
    Description := ADescription;
    TrackIndex := ATrackIdx;
    ClipIndex := AClipIdx;
    OldValue := AOldValue;
    NewValue := ANewValue;
    Timestamp := Now;
  end;

  FUndoPosition := FUndoCount;
end;

function TSedaiProject.ApplyUndoAction(const AAction: TUndoAction; AUndo: Boolean): Boolean;
var
  Trk: TSedaiTrack;
  Clip: TSedaiClip;
  Pos: Int64;
begin
  Result := False;

  case AAction.ActionType of
    uatClipMove:
      begin
        Trk := GetTrack(AAction.TrackIndex);
        if Trk = nil then Exit;
        Clip := Trk.GetClip(AAction.ClipIndex);
        if Clip = nil then Exit;
        // Undo restores the old position; redo re-applies the new one.
        if AUndo then
          Pos := AAction.OldValue
        else
          Pos := AAction.NewValue;
        Clip.MoveTo(Pos);
        Result := True;
      end;
    // uatClipAdd/uatClipRemove/uatTrackAdd/uatTrackRemove/uatClipSplit and
    // parameter/automation changes need the affected object (or a parameter
    // target) retained in the action — extend TUndoAction to apply those.
  end;
end;

function TSedaiProject.Undo: Boolean;
begin
  Result := False;

  if not CanUndo then Exit;

  Dec(FUndoPosition);
  ApplyUndoAction(FUndoStack[FUndoPosition], True);  // revert

  MarkModified;
  Result := True;
end;

function TSedaiProject.Redo: Boolean;
begin
  Result := False;

  if not CanRedo then Exit;

  ApplyUndoAction(FUndoStack[FUndoPosition], False);  // re-apply
  Inc(FUndoPosition);

  MarkModified;
  Result := True;
end;

function TSedaiProject.CanUndo: Boolean;
begin
  Result := FUndoPosition > 0;
end;

function TSedaiProject.CanRedo: Boolean;
begin
  Result := FUndoPosition < FUndoCount;
end;

function TSedaiProject.GetUndoDescription: string;
begin
  if CanUndo then
    Result := FUndoStack[FUndoPosition - 1].Description
  else
    Result := '';
end;

function TSedaiProject.GetRedoDescription: string;
begin
  if CanRedo then
    Result := FUndoStack[FUndoPosition].Description
  else
    Result := '';
end;

procedure TSedaiProject.ClearUndoHistory;
begin
  FUndoCount := 0;
  FUndoPosition := 0;
  SetLength(FUndoStack, 0);
end;

function TSedaiProject.GetDurationSeconds: Double;
begin
  if FSampleRate > 0 then
    Result := FProjectLength / FSampleRate
  else
    Result := 0;
end;

procedure TSedaiProject.SetDurationSeconds(ASeconds: Double);
begin
  FProjectLength := Round(ASeconds * FSampleRate);
  FTransport.Length := FProjectLength;
end;

function TSedaiProject.GetTotalClipCount: Integer;
var
  I: Integer;
begin
  Result := 0;

  for I := 0 to MAX_TRACKS - 1 do
    if Assigned(FTracks[I]) then
      Result := Result + FTracks[I].ClipCount;
end;

end.
