{*
 * Sedai Audio Foundation - MIDI Player
 *
 * TSedaiMIDIPlayer provides playback of Standard MIDI Files (SMF).
 * Supports Format 0 and 1, tempo changes, and all standard events.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiMIDIPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject;

const
  MIDI_MAX_CHANNELS = 16;
  MIDI_MAX_TRACKS = 256;

type
  // MIDI event types
  TMIDIEventType = (
    metNoteOff = $80,
    metNoteOn = $90,
    metPolyPressure = $A0,
    metControlChange = $B0,
    metProgramChange = $C0,
    metChannelPressure = $D0,
    metPitchBend = $E0,
    metSysEx = $F0,
    metMeta = $FF
  );

  // MIDI meta event types
  TMIDIMetaType = (
    mmtSequenceNumber = $00,
    mmtText = $01,
    mmtCopyright = $02,
    mmtTrackName = $03,
    mmtInstrumentName = $04,
    mmtLyric = $05,
    mmtMarker = $06,
    mmtCuePoint = $07,
    mmtChannelPrefix = $20,
    mmtEndOfTrack = $2F,
    mmtTempo = $51,
    mmtSMPTEOffset = $54,
    mmtTimeSignature = $58,
    mmtKeySignature = $59,
    mmtSequencerSpecific = $7F
  );

  // Single MIDI event
  TMIDIEvent = record
    DeltaTime: Cardinal;          // Delta time in ticks
    AbsoluteTime: Cardinal;       // Absolute time in ticks
    EventType: Byte;
    Channel: Byte;
    Data1: Byte;
    Data2: Byte;
    MetaType: Byte;
    MetaData: TBytes;
  end;

  // MIDI track
  TMIDITrack = record
    Events: array of TMIDIEvent;
    EventCount: Integer;
    CurrentEvent: Integer;
    Name: string;
  end;

  // Channel state
  TMIDIChannelState = record
    ProgramNum: Byte;             // Current program/patch
    Volume: Byte;                 // CC 7
    Pan: Byte;                    // CC 10
    Expression: Byte;             // CC 11
    ModWheel: Byte;               // CC 1
    PitchBend: SmallInt;          // -8192 to +8191
    Sustain: Boolean;             // CC 64
    Notes: array[0..127] of Byte; // Active note velocities
  end;

  // Note event callback
  TMIDINoteEvent = procedure(AChannel, ANote, AVelocity: Byte; ANoteOn: Boolean) of object;
  TMIDIControlEvent = procedure(AChannel, AController, AValue: Byte) of object;
  TMIDIProgramEvent = procedure(AChannel, AProgram: Byte) of object;
  TMIDIPitchBendEvent = procedure(AChannel: Byte; ABend: SmallInt) of object;

  { TSedaiMIDIPlayer }
  // Standard MIDI File player
  TSedaiMIDIPlayer = class(TSedaiAudioObject)
  private
    // File info
    FLoaded: Boolean;
    FFormat: Integer;             // 0, 1, or 2
    FTrackCount: Integer;
    FTicksPerQuarter: Integer;    // Division (PPQ)

    // Tracks
    FTracks: array of TMIDITrack;

    // Playback state
    FPlaying: Boolean;
    FPaused: Boolean;
    FCurrentTick: Cardinal;       // Current position in ticks
    FTempo: Cardinal;             // Microseconds per quarter note
    FSamplesPerTick: Double;      // Samples per MIDI tick
    FSampleAccum: Double;         // Accumulated samples

    // Channel states
    FChannels: array[0..MIDI_MAX_CHANNELS-1] of TMIDIChannelState;

    // Events
    FOnNoteEvent: TMIDINoteEvent;
    FOnControlEvent: TMIDIControlEvent;
    FOnProgramEvent: TMIDIProgramEvent;
    FOnPitchBendEvent: TMIDIPitchBendEvent;

    // Metadata
    FSongName: string;
    FCopyright: string;

    procedure UpdateTiming;
    procedure ProcessEvent(const AEvent: TMIDIEvent);
    procedure ProcessMetaEvent(const AEvent: TMIDIEvent);
    function ReadVariableLength(AStream: TStream): Cardinal;
    procedure InitChannels;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure SampleRateChanged; override;

    // Load MIDI file
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;

    // Playback control
    procedure Play;
    procedure Stop;
    procedure Pause;
    procedure Rewind;
    procedure SetPosition(ATick: Cardinal);

    // Advance playback by samples
    procedure AdvanceSamples(ASampleCount: Integer);

    // Get playback info
    function GetDurationTicks: Cardinal;
    function GetDurationSeconds: Double;
    function GetPositionSeconds: Double;

    // Get channel state
    function GetChannelState(AChannel: Integer): TMIDIChannelState;

    // Properties
    property Loaded: Boolean read FLoaded;
    property Playing: Boolean read FPlaying;
    property Paused: Boolean read FPaused;
    property Format: Integer read FFormat;
    property TrackCount: Integer read FTrackCount;
    property TicksPerQuarter: Integer read FTicksPerQuarter;
    property CurrentTick: Cardinal read FCurrentTick;
    property Tempo: Cardinal read FTempo;
    property SongName: string read FSongName;
    property Copyright: string read FCopyright;

    // Events
    property OnNoteEvent: TMIDINoteEvent read FOnNoteEvent write FOnNoteEvent;
    property OnControlEvent: TMIDIControlEvent read FOnControlEvent write FOnControlEvent;
    property OnProgramEvent: TMIDIProgramEvent read FOnProgramEvent write FOnProgramEvent;
    property OnPitchBendEvent: TMIDIPitchBendEvent read FOnPitchBendEvent write FOnPitchBendEvent;
  end;

implementation

{ TSedaiMIDIPlayer }

constructor TSedaiMIDIPlayer.Create;
begin
  inherited Create;

  FLoaded := False;
  FPlaying := False;
  FPaused := False;

  FFormat := 0;
  FTrackCount := 0;
  FTicksPerQuarter := 480;

  FCurrentTick := 0;
  FTempo := 500000;  // 120 BPM default
  FSampleAccum := 0;

  InitChannels;
  UpdateTiming;
end;

destructor TSedaiMIDIPlayer.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FTracks) do
    SetLength(FTracks[I].Events, 0);
  SetLength(FTracks, 0);

  inherited Destroy;
end;

procedure TSedaiMIDIPlayer.InitChannels;
var
  I, J: Integer;
begin
  for I := 0 to MIDI_MAX_CHANNELS - 1 do
  begin
    FChannels[I].ProgramNum := 0;
    FChannels[I].Volume := 100;
    FChannels[I].Pan := 64;
    FChannels[I].Expression := 127;
    FChannels[I].ModWheel := 0;
    FChannels[I].PitchBend := 0;
    FChannels[I].Sustain := False;

    for J := 0 to 127 do
      FChannels[I].Notes[J] := 0;
  end;
end;

procedure TSedaiMIDIPlayer.UpdateTiming;
begin
  // Calculate samples per tick
  // tempo = microseconds per quarter note
  // ticks per quarter = division
  // samples per tick = (tempo / 1000000) * sample_rate / ticks_per_quarter

  if (FTicksPerQuarter > 0) and (FSampleRate > 0) then
    FSamplesPerTick := (FTempo / 1000000.0) * FSampleRate / FTicksPerQuarter
  else
    FSamplesPerTick := 1;
end;

procedure TSedaiMIDIPlayer.Reset;
var
  I: Integer;
begin
  inherited Reset;

  FCurrentTick := 0;
  FSampleAccum := 0;
  FPlaying := False;
  FPaused := False;

  // Reset track positions
  for I := 0 to FTrackCount - 1 do
    FTracks[I].CurrentEvent := 0;

  InitChannels;
end;

procedure TSedaiMIDIPlayer.SampleRateChanged;
begin
  inherited SampleRateChanged;
  UpdateTiming;
end;

function TSedaiMIDIPlayer.ReadVariableLength(AStream: TStream): Cardinal;
var
  B: Byte;
begin
  Result := 0;

  repeat
    if AStream.Read(B, 1) <> 1 then Break;
    Result := (Result shl 7) or (B and $7F);
  until (B and $80) = 0;
end;

function TSedaiMIDIPlayer.LoadFromFile(const AFileName: string): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;

  if not FileExists(AFileName) then Exit;

  try
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TSedaiMIDIPlayer.LoadFromStream(AStream: TStream): Boolean;
var
  Header: array[0..3] of Char;
  ChunkSize: Cardinal;
  Format16, TrackCount16, Division16: Word;
  I, J: Integer;
  TrackHeader: array[0..3] of Char;
  TrackSize: Cardinal;
  TrackEnd: Int64;
  DeltaTime, AbsTime: Cardinal;
  EventByte, RunningStatus: Byte;
  Channel, Data1, Data2: Byte;
  MetaType: Byte;
  MetaLen: Cardinal;
  TempEvent: TMIDIEvent;

  function ReadBigEndian16: Word;
  var
    B1, B2: Byte;
  begin
    AStream.Read(B1, 1);
    AStream.Read(B2, 1);
    Result := (B1 shl 8) or B2;
  end;

  function ReadBigEndian32: Cardinal;
  var
    B1, B2, B3, B4: Byte;
  begin
    AStream.Read(B1, 1);
    AStream.Read(B2, 1);
    AStream.Read(B3, 1);
    AStream.Read(B4, 1);
    Result := (B1 shl 24) or (B2 shl 16) or (B3 shl 8) or B4;
  end;

begin
  Result := False;
  FLoaded := False;

  try
    // Read header chunk
    AStream.Read(Header, 4);
    if (Header[0] <> 'M') or (Header[1] <> 'T') or
       (Header[2] <> 'h') or (Header[3] <> 'd') then
      Exit;

    ChunkSize := ReadBigEndian32;
    if ChunkSize < 6 then Exit;

    Format16 := ReadBigEndian16;
    TrackCount16 := ReadBigEndian16;
    Division16 := ReadBigEndian16;

    FFormat := Format16;
    FTrackCount := TrackCount16;

    // Parse division
    if (Division16 and $8000) = 0 then
      // Ticks per quarter note
      FTicksPerQuarter := Division16
    else
      // SMPTE format (not fully supported)
      FTicksPerQuarter := 480;

    // Skip any extra header bytes
    if ChunkSize > 6 then
      AStream.Seek(ChunkSize - 6, soCurrent);

    // Allocate tracks
    SetLength(FTracks, FTrackCount);

    // Read tracks
    for I := 0 to FTrackCount - 1 do
    begin
      // Read track header
      AStream.Read(TrackHeader, 4);
      if (TrackHeader[0] <> 'M') or (TrackHeader[1] <> 'T') or
         (TrackHeader[2] <> 'r') or (TrackHeader[3] <> 'k') then
        Exit;

      TrackSize := ReadBigEndian32;
      TrackEnd := AStream.Position + TrackSize;

      FTracks[I].EventCount := 0;
      FTracks[I].CurrentEvent := 0;
      FTracks[I].Name := '';
      SetLength(FTracks[I].Events, 0);

      AbsTime := 0;
      RunningStatus := 0;

      // Read track events
      while AStream.Position < TrackEnd do
      begin
        // Read delta time
        DeltaTime := ReadVariableLength(AStream);
        AbsTime := AbsTime + DeltaTime;

        // Read event type
        AStream.Read(EventByte, 1);

        // Check for running status
        if (EventByte and $80) = 0 then
        begin
          // Use running status
          Data1 := EventByte;
          EventByte := RunningStatus;
        end
        else
        begin
          // New status
          if (EventByte and $F0) <> $F0 then
            RunningStatus := EventByte;

          // Read first data byte
          AStream.Read(Data1, 1);
        end;

        // Initialize event
        TempEvent.DeltaTime := DeltaTime;
        TempEvent.AbsoluteTime := AbsTime;
        TempEvent.EventType := EventByte and $F0;
        TempEvent.Channel := EventByte and $0F;
        TempEvent.Data1 := Data1;
        TempEvent.Data2 := 0;
        TempEvent.MetaType := 0;
        SetLength(TempEvent.MetaData, 0);

        case EventByte and $F0 of
          $80, $90, $A0, $B0, $E0:  // Two data bytes
            AStream.Read(TempEvent.Data2, 1);

          $C0, $D0:  // One data byte
            ;  // Data1 already read

          $F0:  // System events
            begin
              case EventByte of
                $F0:  // SysEx
                  begin
                    MetaLen := ReadVariableLength(AStream);
                    SetLength(TempEvent.MetaData, MetaLen);
                    if MetaLen > 0 then
                      AStream.Read(TempEvent.MetaData[0], MetaLen);
                  end;

                $FF:  // Meta event
                  begin
                    TempEvent.EventType := $FF;
                    TempEvent.MetaType := Data1;
                    MetaLen := ReadVariableLength(AStream);
                    SetLength(TempEvent.MetaData, MetaLen);
                    if MetaLen > 0 then
                      AStream.Read(TempEvent.MetaData[0], MetaLen);

                    // Extract useful meta info
                    case Data1 of
                      $03:  // Track name
                        begin
                          SetLength(FTracks[I].Name, MetaLen);
                          if MetaLen > 0 then
                            Move(TempEvent.MetaData[0], FTracks[I].Name[1], MetaLen);
                          if FSongName = '' then
                            FSongName := FTracks[I].Name;
                        end;

                      $02:  // Copyright
                        begin
                          SetLength(FCopyright, MetaLen);
                          if MetaLen > 0 then
                            Move(TempEvent.MetaData[0], FCopyright[1], MetaLen);
                        end;

                      $51:  // Tempo
                        if MetaLen = 3 then
                          FTempo := (TempEvent.MetaData[0] shl 16) or
                                    (TempEvent.MetaData[1] shl 8) or
                                    TempEvent.MetaData[2];
                    end;
                  end;

                $F7:  // Escape
                  begin
                    MetaLen := ReadVariableLength(AStream);
                    AStream.Seek(MetaLen, soCurrent);
                  end;

                else
                  ;  // Other system common messages
              end;
            end;
        end;

        // Add event to track
        Inc(FTracks[I].EventCount);
        SetLength(FTracks[I].Events, FTracks[I].EventCount);
        FTracks[I].Events[FTracks[I].EventCount - 1] := TempEvent;
      end;

      // Ensure we're at the expected position
      AStream.Position := TrackEnd;
    end;

    FLoaded := True;
    Result := True;

    UpdateTiming;
    Reset;

  except
    FLoaded := False;
    Result := False;
  end;
end;

procedure TSedaiMIDIPlayer.ProcessEvent(const AEvent: TMIDIEvent);
var
  Ch: Byte;
begin
  Ch := AEvent.Channel;

  case AEvent.EventType of
    $80:  // Note Off
      begin
        FChannels[Ch].Notes[AEvent.Data1] := 0;
        if Assigned(FOnNoteEvent) then
          FOnNoteEvent(Ch, AEvent.Data1, 0, False);
      end;

    $90:  // Note On
      begin
        if AEvent.Data2 = 0 then
        begin
          // Velocity 0 = Note Off
          FChannels[Ch].Notes[AEvent.Data1] := 0;
          if Assigned(FOnNoteEvent) then
            FOnNoteEvent(Ch, AEvent.Data1, 0, False);
        end
        else
        begin
          FChannels[Ch].Notes[AEvent.Data1] := AEvent.Data2;
          if Assigned(FOnNoteEvent) then
            FOnNoteEvent(Ch, AEvent.Data1, AEvent.Data2, True);
        end;
      end;

    $B0:  // Control Change
      begin
        case AEvent.Data1 of
          1: FChannels[Ch].ModWheel := AEvent.Data2;
          7: FChannels[Ch].Volume := AEvent.Data2;
          10: FChannels[Ch].Pan := AEvent.Data2;
          11: FChannels[Ch].Expression := AEvent.Data2;
          64: FChannels[Ch].Sustain := AEvent.Data2 >= 64;
        end;

        if Assigned(FOnControlEvent) then
          FOnControlEvent(Ch, AEvent.Data1, AEvent.Data2);
      end;

    $C0:  // Program Change
      begin
        FChannels[Ch].ProgramNum := AEvent.Data1;
        if Assigned(FOnProgramEvent) then
          FOnProgramEvent(Ch, AEvent.Data1);
      end;

    $E0:  // Pitch Bend
      begin
        FChannels[Ch].PitchBend := (AEvent.Data1 or (AEvent.Data2 shl 7)) - 8192;
        if Assigned(FOnPitchBendEvent) then
          FOnPitchBendEvent(Ch, FChannels[Ch].PitchBend);
      end;

    $FF:  // Meta Event
      ProcessMetaEvent(AEvent);
  end;
end;

procedure TSedaiMIDIPlayer.ProcessMetaEvent(const AEvent: TMIDIEvent);
begin
  case AEvent.MetaType of
    $51:  // Tempo
      begin
        if Length(AEvent.MetaData) = 3 then
        begin
          FTempo := (AEvent.MetaData[0] shl 16) or
                    (AEvent.MetaData[1] shl 8) or
                    AEvent.MetaData[2];
          UpdateTiming;
        end;
      end;

    $2F:  // End of Track
      ;  // Handled in AdvanceSamples
  end;
end;

procedure TSedaiMIDIPlayer.Play;
begin
  if not FLoaded then Exit;

  if FPaused then
    FPaused := False
  else
    Reset;

  FPlaying := True;
end;

procedure TSedaiMIDIPlayer.Stop;
begin
  FPlaying := False;
  FPaused := False;
  Reset;
end;

procedure TSedaiMIDIPlayer.Pause;
begin
  if FPlaying then
  begin
    FPlaying := False;
    FPaused := True;
  end
  else if FPaused then
  begin
    FPaused := False;
    FPlaying := True;
  end;
end;

procedure TSedaiMIDIPlayer.Rewind;
begin
  Reset;
end;

procedure TSedaiMIDIPlayer.SetPosition(ATick: Cardinal);
var
  I: Integer;
begin
  FCurrentTick := ATick;
  FSampleAccum := 0;

  // Reset track positions
  for I := 0 to FTrackCount - 1 do
    FTracks[I].CurrentEvent := 0;

  // Skip events up to current tick
  // (Would need proper implementation for seeking)
end;

procedure TSedaiMIDIPlayer.AdvanceSamples(ASampleCount: Integer);
var
  I: Integer;
  TracksFinished: Integer;
begin
  if not FPlaying then Exit;

  FSampleAccum := FSampleAccum + ASampleCount;

  while FSampleAccum >= FSamplesPerTick do
  begin
    FSampleAccum := FSampleAccum - FSamplesPerTick;
    Inc(FCurrentTick);

    TracksFinished := 0;

    // Process events for all tracks
    for I := 0 to FTrackCount - 1 do
    begin
      while FTracks[I].CurrentEvent < FTracks[I].EventCount do
      begin
        if FTracks[I].Events[FTracks[I].CurrentEvent].AbsoluteTime <= FCurrentTick then
        begin
          ProcessEvent(FTracks[I].Events[FTracks[I].CurrentEvent]);
          Inc(FTracks[I].CurrentEvent);
        end
        else
          Break;
      end;

      // Check if track finished
      if FTracks[I].CurrentEvent >= FTracks[I].EventCount then
        Inc(TracksFinished);
    end;

    // Stop if all tracks finished
    if TracksFinished = FTrackCount then
    begin
      FPlaying := False;
      Break;
    end;
  end;
end;

function TSedaiMIDIPlayer.GetDurationTicks: Cardinal;
var
  I: Integer;
  MaxTick: Cardinal;
begin
  MaxTick := 0;

  for I := 0 to FTrackCount - 1 do
    if FTracks[I].EventCount > 0 then
      if FTracks[I].Events[FTracks[I].EventCount - 1].AbsoluteTime > MaxTick then
        MaxTick := FTracks[I].Events[FTracks[I].EventCount - 1].AbsoluteTime;

  Result := MaxTick;
end;

function TSedaiMIDIPlayer.GetDurationSeconds: Double;
begin
  // Simplified - assumes constant tempo
  Result := GetDurationTicks * (FTempo / 1000000.0) / FTicksPerQuarter;
end;

function TSedaiMIDIPlayer.GetPositionSeconds: Double;
begin
  Result := FCurrentTick * (FTempo / 1000000.0) / FTicksPerQuarter;
end;

function TSedaiMIDIPlayer.GetChannelState(AChannel: Integer): TMIDIChannelState;
begin
  if (AChannel >= 0) and (AChannel < MIDI_MAX_CHANNELS) then
    Result := FChannels[AChannel]
  else
  begin
    FillChar(Result, SizeOf(Result), 0);
  end;
end;

end.
