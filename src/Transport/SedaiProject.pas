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

function TSedaiProject.SaveProject(APath: string): Boolean;
begin
  // TODO: Implement actual project saving
  // This would serialize all project data to a file

  FFilePath := APath;
  FState := psSaved;
  FModified := Now;
  Result := True;
end;

function TSedaiProject.LoadProject(APath: string): Boolean;
begin
  // TODO: Implement actual project loading
  // This would deserialize project data from a file

  FFilePath := APath;
  FState := psSaved;
  Result := True;
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
  I, J: Integer;
  Position: Int64;
  TrackInputs: array of PSingle;
  TrackBuffers: array of array of Single;
begin
  EnsureBufferSize(AFrameCount);

  // Get current position
  Position := FTransport.Position;

  // Allocate track input pointers
  SetLength(TrackInputs, FTrackCount);
  SetLength(TrackBuffers, FTrackCount);

  // Read audio from each track
  J := 0;
  for I := 0 to MAX_TRACKS - 1 do
  begin
    if Assigned(FTracks[I]) and (FTracks[I] is TSedaiAudioTrack) then
    begin
      SetLength(TrackBuffers[J], AFrameCount * 2);

      // Read track audio
      FTracks[I].ReadAudio(Position, @TrackBuffers[J][0], AFrameCount, 2);
      TrackInputs[J] := @TrackBuffers[J][0];
      Inc(J);
    end;
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

function TSedaiProject.Undo: Boolean;
begin
  Result := False;

  if not CanUndo then Exit;

  Dec(FUndoPosition);

  // TODO: Actually apply the undo action based on type
  // For now, just track the position

  MarkModified;
  Result := True;
end;

function TSedaiProject.Redo: Boolean;
begin
  Result := False;

  if not CanRedo then Exit;

  // TODO: Actually apply the redo action based on type

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
