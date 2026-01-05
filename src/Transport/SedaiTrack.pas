{*
 * Sedai Audio Foundation - Track
 *
 * TSedaiTrack represents a track in the project timeline,
 * containing clips and routing to mixer channels.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiTrack;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject,
  SedaiClip, SedaiAudioBuffer;

const
  MAX_CLIPS_PER_TRACK = 1024;
  MAX_AUTOMATION_LANES = 32;

type
  // Clip array type
  TSedaiClipArray = array of TSedaiClip;

  // Track type
  TTrackType = (
    ttAudio,              // Audio track
    ttMIDI,               // MIDI/Instrument track
    ttGroup,              // Group/folder track
    ttMaster              // Master track
  );

  // Automation point
  TAutomationPoint = record
    Position: Int64;      // Sample position
    Value: Single;        // Normalized value (0-1)
    Curve: Single;        // Curve type (-1 to +1, 0 = linear)
  end;

  // Automation lane
  TAutomationLane = record
    ParameterName: string;
    ParameterID: Integer;
    Points: array of TAutomationPoint;
    PointCount: Integer;
    Enabled: Boolean;
    Visible: Boolean;
  end;

  // Forward declaration
  TSedaiTrack = class;

  // Track event callback
  TTrackClipEvent = procedure(ATrack: TSedaiTrack; AClip: TSedaiClip) of object;

  { TSedaiTrack }
  // Timeline track containing clips
  TSedaiTrack = class(TSedaiAudioObject)
  private
    FTrackType: TTrackType;
    FName: string;
    FColor: Cardinal;
    FIndex: Integer;

    // Clips
    FClips: array[0..MAX_CLIPS_PER_TRACK-1] of TSedaiClip;
    FClipCount: Integer;

    // Automation
    FAutomationLanes: array[0..MAX_AUTOMATION_LANES-1] of TAutomationLane;
    FAutomationLaneCount: Integer;

    // Track state
    FMuted: Boolean;
    FSoloed: Boolean;
    FArmed: Boolean;               // Record armed
    FMonitorInput: Boolean;        // Input monitoring

    // Routing
    FMixerChannelIndex: Integer;   // Target mixer channel
    FInputSource: Integer;         // Input source (audio device input or MIDI)

    // Height (for UI)
    FHeight: Integer;
    FExpanded: Boolean;

    // Events
    FOnClipAdded: TTrackClipEvent;
    FOnClipRemoved: TTrackClipEvent;

    function GetClipAtPosition(APosition: Int64): TSedaiClip;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;

    // ========================================================================
    // CLIP MANAGEMENT
    // ========================================================================

    // Add clip to track
    function AddClip(AClip: TSedaiClip): Integer;

    // Remove clip
    procedure RemoveClip(AIndex: Integer);
    procedure RemoveClipObj(AClip: TSedaiClip);

    // Clear all clips
    procedure ClearClips;

    // Get clip by index
    function GetClip(AIndex: Integer): TSedaiClip;

    // Find clips at position
    function GetClipsAtPosition(APosition: Int64): TSedaiClipArray;

    // Find clips in range
    function GetClipsInRange(AStart, AEnd: Int64): TSedaiClipArray;

    // Check for overlapping clips
    function HasOverlappingClips(AStart, AEnd: Int64; AExclude: TSedaiClip = nil): Boolean;

    // Split clip at position
    function SplitClipAtPosition(APosition: Int64): Boolean;

    // ========================================================================
    // AUTOMATION
    // ========================================================================

    // Add automation lane
    function AddAutomationLane(AParameterName: string; AParameterID: Integer): Integer;

    // Remove automation lane
    procedure RemoveAutomationLane(AIndex: Integer);

    // Add automation point
    function AddAutomationPoint(ALaneIndex: Integer; APosition: Int64;
                                 AValue: Single; ACurve: Single = 0): Integer;

    // Remove automation point
    procedure RemoveAutomationPoint(ALaneIndex, APointIndex: Integer);

    // Get automation value at position
    function GetAutomationValue(ALaneIndex: Integer; APosition: Int64): Single;

    // Clear automation lane
    procedure ClearAutomationLane(ALaneIndex: Integer);

    // ========================================================================
    // AUDIO PROCESSING
    // ========================================================================

    // Read audio from all clips at position
    procedure ReadAudio(APosition: Int64; AOutput: PSingle;
                        AFrameCount: Integer; AChannels: Integer);

    // Get MIDI events in range (for MIDI tracks)
    function GetMIDIEventsInRange(AStart, AEnd: Int64): TMIDINoteEventArray;

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    property TrackType: TTrackType read FTrackType write FTrackType;
    property Name: string read FName write FName;
    property Color: Cardinal read FColor write FColor;
    property Index: Integer read FIndex write FIndex;

    property ClipCount: Integer read FClipCount;
    property AutomationLaneCount: Integer read FAutomationLaneCount;

    property Muted: Boolean read FMuted write FMuted;
    property Soloed: Boolean read FSoloed write FSoloed;
    property Armed: Boolean read FArmed write FArmed;
    property MonitorInput: Boolean read FMonitorInput write FMonitorInput;

    property MixerChannelIndex: Integer read FMixerChannelIndex write FMixerChannelIndex;
    property InputSource: Integer read FInputSource write FInputSource;

    property Height: Integer read FHeight write FHeight;
    property Expanded: Boolean read FExpanded write FExpanded;

    property OnClipAdded: TTrackClipEvent read FOnClipAdded write FOnClipAdded;
    property OnClipRemoved: TTrackClipEvent read FOnClipRemoved write FOnClipRemoved;
  end;

  { TSedaiAudioTrack }
  // Audio track specialization
  TSedaiAudioTrack = class(TSedaiTrack)
  private
    FRecordBuffer: TSedaiAudioBuffer;
    FRecording: Boolean;
    FRecordStartPos: Int64;

  public
    constructor Create; override;
    destructor Destroy; override;

    // Recording
    procedure StartRecording(APosition: Int64);
    function StopRecording: TSedaiAudioClip;
    procedure WriteRecordBuffer(AInput: PSingle; AFrameCount: Integer);

    property Recording: Boolean read FRecording;
    property RecordStartPos: Int64 read FRecordStartPos;
  end;

  { TSedaiMIDITrack }
  // MIDI track specialization
  TSedaiMIDITrack = class(TSedaiTrack)
  private
    FInstrumentIndex: Integer;     // Index of instrument in instrument list
    FMIDIChannel: Integer;         // Output MIDI channel

    // Recording state
    FRecordingNotes: array of TMIDINoteEvent;
    FRecordingNoteCount: Integer;
    FRecording: Boolean;
    FRecordStartPos: Int64;

  public
    constructor Create; override;

    // Recording
    procedure StartRecording(APosition: Int64);
    function StopRecording: TSedaiMIDIClip;
    procedure RecordNoteOn(APosition: Int64; ANote, AVelocity: Byte);
    procedure RecordNoteOff(APosition: Int64; ANote: Byte);

    property InstrumentIndex: Integer read FInstrumentIndex write FInstrumentIndex;
    property MIDIChannel: Integer read FMIDIChannel write FMIDIChannel;
    property Recording: Boolean read FRecording;
  end;

implementation

{ TSedaiTrack }

constructor TSedaiTrack.Create;
var
  I: Integer;
begin
  inherited Create;

  FTrackType := ttAudio;
  FName := 'Track';
  FColor := $FF808080;
  FIndex := 0;

  FClipCount := 0;
  for I := 0 to MAX_CLIPS_PER_TRACK - 1 do
    FClips[I] := nil;

  FAutomationLaneCount := 0;
  for I := 0 to MAX_AUTOMATION_LANES - 1 do
  begin
    FAutomationLanes[I].ParameterName := '';
    FAutomationLanes[I].ParameterID := 0;
    FAutomationLanes[I].PointCount := 0;
    FAutomationLanes[I].Enabled := True;
    FAutomationLanes[I].Visible := False;
  end;

  FMuted := False;
  FSoloed := False;
  FArmed := False;
  FMonitorInput := False;

  FMixerChannelIndex := -1;
  FInputSource := 0;

  FHeight := 80;
  FExpanded := True;
end;

destructor TSedaiTrack.Destroy;
begin
  ClearClips;
  inherited Destroy;
end;

procedure TSedaiTrack.Reset;
begin
  inherited Reset;
end;

function TSedaiTrack.GetClipAtPosition(APosition: Int64): TSedaiClip;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to FClipCount - 1 do
  begin
    if Assigned(FClips[I]) and FClips[I].ContainsPosition(APosition) then
    begin
      Result := FClips[I];
      Exit;
    end;
  end;
end;

function TSedaiTrack.AddClip(AClip: TSedaiClip): Integer;
var
  I: Integer;
begin
  Result := -1;

  // Find empty slot
  for I := 0 to MAX_CLIPS_PER_TRACK - 1 do
  begin
    if FClips[I] = nil then
    begin
      FClips[I] := AClip;
      Inc(FClipCount);
      Result := I;

      if Assigned(FOnClipAdded) then
        FOnClipAdded(Self, AClip);

      Exit;
    end;
  end;
end;

procedure TSedaiTrack.RemoveClip(AIndex: Integer);
var
  Clip: TSedaiClip;
begin
  if (AIndex < 0) or (AIndex >= MAX_CLIPS_PER_TRACK) then Exit;
  if FClips[AIndex] = nil then Exit;

  Clip := FClips[AIndex];
  FClips[AIndex] := nil;
  Dec(FClipCount);

  if Assigned(FOnClipRemoved) then
    FOnClipRemoved(Self, Clip);
end;

procedure TSedaiTrack.RemoveClipObj(AClip: TSedaiClip);
var
  I: Integer;
begin
  for I := 0 to MAX_CLIPS_PER_TRACK - 1 do
  begin
    if FClips[I] = AClip then
    begin
      RemoveClip(I);
      Exit;
    end;
  end;
end;

procedure TSedaiTrack.ClearClips;
var
  I: Integer;
begin
  for I := 0 to MAX_CLIPS_PER_TRACK - 1 do
  begin
    if Assigned(FClips[I]) then
    begin
      FClips[I].Free;
      FClips[I] := nil;
    end;
  end;
  FClipCount := 0;
end;

function TSedaiTrack.GetClip(AIndex: Integer): TSedaiClip;
begin
  if (AIndex >= 0) and (AIndex < MAX_CLIPS_PER_TRACK) then
    Result := FClips[AIndex]
  else
    Result := nil;
end;

function TSedaiTrack.GetClipsAtPosition(APosition: Int64): TSedaiClipArray;
var
  I, Count: Integer;
begin
  SetLength(Result, FClipCount);
  Count := 0;

  for I := 0 to MAX_CLIPS_PER_TRACK - 1 do
  begin
    if Assigned(FClips[I]) and FClips[I].ContainsPosition(APosition) then
    begin
      Result[Count] := FClips[I];
      Inc(Count);
    end;
  end;

  SetLength(Result, Count);
end;

function TSedaiTrack.GetClipsInRange(AStart, AEnd: Int64): TSedaiClipArray;
var
  I, Count: Integer;
begin
  SetLength(Result, FClipCount);
  Count := 0;

  for I := 0 to MAX_CLIPS_PER_TRACK - 1 do
  begin
    if Assigned(FClips[I]) then
    begin
      // Check if clip overlaps with range
      if not ((FClips[I].EndPosition <= AStart) or (FClips[I].StartPosition >= AEnd)) then
      begin
        Result[Count] := FClips[I];
        Inc(Count);
      end;
    end;
  end;

  SetLength(Result, Count);
end;

function TSedaiTrack.HasOverlappingClips(AStart, AEnd: Int64; AExclude: TSedaiClip): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to MAX_CLIPS_PER_TRACK - 1 do
  begin
    if Assigned(FClips[I]) and (FClips[I] <> AExclude) then
    begin
      if not ((FClips[I].EndPosition <= AStart) or (FClips[I].StartPosition >= AEnd)) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

function TSedaiTrack.SplitClipAtPosition(APosition: Int64): Boolean;
var
  I: Integer;
  Clip, NewClip: TSedaiClip;
begin
  Result := False;

  for I := 0 to MAX_CLIPS_PER_TRACK - 1 do
  begin
    if Assigned(FClips[I]) and FClips[I].ContainsPosition(APosition) then
    begin
      Clip := FClips[I];
      NewClip := Clip.Split(APosition);

      if Assigned(NewClip) then
      begin
        AddClip(NewClip);
        Result := True;
      end;

      Exit;
    end;
  end;
end;

function TSedaiTrack.AddAutomationLane(AParameterName: string;
  AParameterID: Integer): Integer;
begin
  Result := -1;

  if FAutomationLaneCount >= MAX_AUTOMATION_LANES then Exit;

  Result := FAutomationLaneCount;
  Inc(FAutomationLaneCount);

  FAutomationLanes[Result].ParameterName := AParameterName;
  FAutomationLanes[Result].ParameterID := AParameterID;
  FAutomationLanes[Result].PointCount := 0;
  FAutomationLanes[Result].Enabled := True;
  FAutomationLanes[Result].Visible := True;
end;

procedure TSedaiTrack.RemoveAutomationLane(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FAutomationLaneCount) then Exit;

  SetLength(FAutomationLanes[AIndex].Points, 0);

  for I := AIndex to FAutomationLaneCount - 2 do
    FAutomationLanes[I] := FAutomationLanes[I + 1];

  Dec(FAutomationLaneCount);
end;

function TSedaiTrack.AddAutomationPoint(ALaneIndex: Integer; APosition: Int64;
  AValue: Single; ACurve: Single): Integer;
begin
  Result := -1;

  if (ALaneIndex < 0) or (ALaneIndex >= FAutomationLaneCount) then Exit;

  with FAutomationLanes[ALaneIndex] do
  begin
    Result := PointCount;
    Inc(PointCount);
    SetLength(Points, PointCount);

    Points[Result].Position := APosition;
    Points[Result].Value := AValue;
    Points[Result].Curve := ACurve;
  end;
end;

procedure TSedaiTrack.RemoveAutomationPoint(ALaneIndex, APointIndex: Integer);
var
  I: Integer;
begin
  if (ALaneIndex < 0) or (ALaneIndex >= FAutomationLaneCount) then Exit;

  with FAutomationLanes[ALaneIndex] do
  begin
    if (APointIndex < 0) or (APointIndex >= PointCount) then Exit;

    for I := APointIndex to PointCount - 2 do
      Points[I] := Points[I + 1];

    Dec(PointCount);
    SetLength(Points, PointCount);
  end;
end;

function TSedaiTrack.GetAutomationValue(ALaneIndex: Integer; APosition: Int64): Single;
var
  I: Integer;
  P1, P2: TAutomationPoint;
  T: Single;
begin
  Result := 0.5;  // Default

  if (ALaneIndex < 0) or (ALaneIndex >= FAutomationLaneCount) then Exit;
  if not FAutomationLanes[ALaneIndex].Enabled then Exit;

  with FAutomationLanes[ALaneIndex] do
  begin
    if PointCount = 0 then Exit;

    // Before first point
    if APosition <= Points[0].Position then
    begin
      Result := Points[0].Value;
      Exit;
    end;

    // After last point
    if APosition >= Points[PointCount - 1].Position then
    begin
      Result := Points[PointCount - 1].Value;
      Exit;
    end;

    // Find surrounding points
    for I := 0 to PointCount - 2 do
    begin
      if (APosition >= Points[I].Position) and (APosition < Points[I + 1].Position) then
      begin
        P1 := Points[I];
        P2 := Points[I + 1];

        // Linear interpolation (curve parameter ignored for now)
        T := (APosition - P1.Position) / (P2.Position - P1.Position);
        Result := P1.Value + (P2.Value - P1.Value) * T;
        Exit;
      end;
    end;
  end;
end;

procedure TSedaiTrack.ClearAutomationLane(ALaneIndex: Integer);
begin
  if (ALaneIndex < 0) or (ALaneIndex >= FAutomationLaneCount) then Exit;

  FAutomationLanes[ALaneIndex].PointCount := 0;
  SetLength(FAutomationLanes[ALaneIndex].Points, 0);
end;

procedure TSedaiTrack.ReadAudio(APosition: Int64; AOutput: PSingle;
  AFrameCount: Integer; AChannels: Integer);
var
  I, J: Integer;
  Clips: TSedaiClipArray;
  TempBuffer: array of Single;
  AudioClip: TSedaiAudioClip;
begin
  // Clear output
  for I := 0 to AFrameCount * AChannels - 1 do
    AOutput[I] := 0.0;

  if FMuted then Exit;

  // Get clips in range
  Clips := GetClipsInRange(APosition, APosition + AFrameCount);

  if Length(Clips) = 0 then Exit;

  // Allocate temp buffer
  SetLength(TempBuffer, AFrameCount * AChannels);

  // Mix all clips
  for I := 0 to High(Clips) do
  begin
    if Clips[I] is TSedaiAudioClip then
    begin
      AudioClip := TSedaiAudioClip(Clips[I]);

      // Read audio from clip
      AudioClip.ReadAudio(APosition, @TempBuffer[0], AFrameCount, AChannels);

      // Mix into output
      for J := 0 to AFrameCount * AChannels - 1 do
        AOutput[J] := AOutput[J] + TempBuffer[J];
    end;
  end;
end;

function TSedaiTrack.GetMIDIEventsInRange(AStart, AEnd: Int64): TMIDINoteEventArray;
var
  I, J, Count: Integer;
  Clips: TSedaiClipArray;
  MIDIClip: TSedaiMIDIClip;
  Note: TMIDINoteEvent;
begin
  SetLength(Result, 0);
  Count := 0;

  Clips := GetClipsInRange(AStart, AEnd);

  for I := 0 to High(Clips) do
  begin
    if Clips[I] is TSedaiMIDIClip then
    begin
      MIDIClip := TSedaiMIDIClip(Clips[I]);

      for J := 0 to MIDIClip.NoteCount - 1 do
      begin
        Note := MIDIClip.GetNote(J);
        // Adjust note position to timeline position
        Note.Position := Note.Position + MIDIClip.StartPosition;

        if (Note.Position >= AStart) and (Note.Position < AEnd) then
        begin
          Inc(Count);
          SetLength(Result, Count);
          Result[Count - 1] := Note;
        end;
      end;
    end;
  end;
end;

{ TSedaiAudioTrack }

constructor TSedaiAudioTrack.Create;
begin
  inherited Create;

  FTrackType := ttAudio;
  FRecordBuffer := TSedaiAudioBuffer.Create;
  FRecording := False;
  FRecordStartPos := 0;
end;

destructor TSedaiAudioTrack.Destroy;
begin
  FRecordBuffer.Free;
  inherited Destroy;
end;

procedure TSedaiAudioTrack.StartRecording(APosition: Int64);
begin
  FRecording := True;
  FRecordStartPos := APosition;

  // Allocate buffer for recording (1 minute initial)
  FRecordBuffer.Allocate(2, Round(FSampleRate * 60));
  FRecordBuffer.Clear;
end;

function TSedaiAudioTrack.StopRecording: TSedaiAudioClip;
var
  NewClip: TSedaiAudioClip;
begin
  Result := nil;

  if not FRecording then Exit;

  FRecording := False;

  // Create new clip from recorded audio
  NewClip := TSedaiAudioClip.Create;
  NewClip.Name := Format('Recording %d', [FClipCount + 1]);
  NewClip.StartPosition := FRecordStartPos;
  NewClip.LoadBuffer(FRecordBuffer, True);  // Transfer ownership

  // Create new record buffer for next recording
  FRecordBuffer := TSedaiAudioBuffer.Create;

  // Add clip to track
  AddClip(NewClip);

  Result := NewClip;
end;

procedure TSedaiAudioTrack.WriteRecordBuffer(AInput: PSingle; AFrameCount: Integer);
var
  I: Integer;
  CurrentSize: Integer;
begin
  if not FRecording then Exit;

  // TODO: Implement proper circular buffer recording
  // For now, just a simple approach
  CurrentSize := FRecordBuffer.SampleCount;

  // Expand buffer if needed
  if CurrentSize < Round(FSampleRate * 600) then  // Max 10 minutes
  begin
    // Copy samples to buffer
    for I := 0 to AFrameCount - 1 do
    begin
      // TODO: Write to correct position in buffer
    end;
  end;
end;

{ TSedaiMIDITrack }

constructor TSedaiMIDITrack.Create;
begin
  inherited Create;

  FTrackType := ttMIDI;
  FInstrumentIndex := -1;
  FMIDIChannel := 0;
  FRecording := False;
  FRecordStartPos := 0;
  FRecordingNoteCount := 0;
end;

procedure TSedaiMIDITrack.StartRecording(APosition: Int64);
begin
  FRecording := True;
  FRecordStartPos := APosition;
  FRecordingNoteCount := 0;
  SetLength(FRecordingNotes, 0);
end;

function TSedaiMIDITrack.StopRecording: TSedaiMIDIClip;
var
  NewClip: TSedaiMIDIClip;
  I: Integer;
begin
  Result := nil;

  if not FRecording then Exit;

  FRecording := False;

  if FRecordingNoteCount = 0 then Exit;

  // Create new MIDI clip from recorded notes
  NewClip := TSedaiMIDIClip.Create;
  NewClip.Name := Format('MIDI Recording %d', [FClipCount + 1]);
  NewClip.StartPosition := FRecordStartPos;

  // Add recorded notes
  for I := 0 to FRecordingNoteCount - 1 do
  begin
    with FRecordingNotes[I] do
      NewClip.AddNote(Position - FRecordStartPos, Duration, Note, Velocity, Channel);
  end;

  // Clear recording buffer
  FRecordingNoteCount := 0;
  SetLength(FRecordingNotes, 0);

  // Add clip to track
  AddClip(NewClip);

  Result := NewClip;
end;

procedure TSedaiMIDITrack.RecordNoteOn(APosition: Int64; ANote, AVelocity: Byte);
begin
  if not FRecording then Exit;

  Inc(FRecordingNoteCount);
  SetLength(FRecordingNotes, FRecordingNoteCount);

  with FRecordingNotes[FRecordingNoteCount - 1] do
  begin
    Position := APosition;
    Duration := 0;  // Will be set on note off
    Note := ANote;
    Velocity := AVelocity;
    Channel := FMIDIChannel;
  end;
end;

procedure TSedaiMIDITrack.RecordNoteOff(APosition: Int64; ANote: Byte);
var
  I: Integer;
begin
  if not FRecording then Exit;

  // Find matching note on and set duration
  for I := FRecordingNoteCount - 1 downto 0 do
  begin
    if (FRecordingNotes[I].Note = ANote) and (FRecordingNotes[I].Duration = 0) then
    begin
      FRecordingNotes[I].Duration := APosition - FRecordingNotes[I].Position;
      Exit;
    end;
  end;
end;

end.
