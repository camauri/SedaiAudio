{*
 * Sedai Audio Foundation - Clip
 *
 * TSedaiClip represents an audio or MIDI region on the timeline.
 * Supports start/end, offset, fade in/out, and time stretching.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiClip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiAudioBuffer;

type
  // Integer array for note indices
  TIntegerDynArray = array of Integer;

  // Clip type
  TClipType = (
    ctAudio,              // Audio clip
    ctMIDI,               // MIDI clip
    ctAutomation          // Automation clip
  );

  // Fade curve type
  TFadeCurve = (
    fcLinear,             // Linear fade
    fcLogarithmic,        // Logarithmic (natural)
    fcExponential,        // Exponential
    fcSCurve,             // S-curve (smooth)
    fcEqualPower          // Equal power (crossfade)
  );

  // Time stretch mode
  TStretchMode = (
    smOff,                // No stretching
    smRepitch,            // Repitch (like tape)
    smElastic,            // Time stretch (preserve pitch)
    smSlice               // Beat slice
  );

  { TSedaiClip }
  // Base clip class
  TSedaiClip = class(TSedaiAudioObject)
  private
    FClipType: TClipType;
    FName: string;
    FColor: Cardinal;

    // Timeline position
    FStartPosition: Int64;        // Start on timeline (samples)
    FEndPosition: Int64;          // End on timeline (samples)

    // Source offset
    FSourceOffset: Int64;         // Offset into source (samples)

    // Fades
    FFadeInLength: Integer;       // Samples
    FFadeOutLength: Integer;      // Samples
    FFadeInCurve: TFadeCurve;
    FFadeOutCurve: TFadeCurve;

    // Gain
    FGain: Single;                // Linear gain (1.0 = unity)

    // Mute/Lock
    FMuted: Boolean;
    FLocked: Boolean;

    function GetLength: Int64;
    procedure SetClipLength(AValue: Int64);
    function GetFadeGain(APosition: Int64): Single;

  protected
    function CalculateFadeCurve(ANormalizedPos: Single; ACurve: TFadeCurve;
                                 AFadeIn: Boolean): Single;

  public
    constructor Create; override;

    procedure Reset; override;

    // Check if position is within clip
    function ContainsPosition(APosition: Int64): Boolean;

    // Get position relative to clip start
    function GetRelativePosition(ATimelinePos: Int64): Int64;

    // Get source position (with offset)
    function GetSourcePosition(ATimelinePos: Int64): Int64;

    // Move clip
    procedure MoveTo(APosition: Int64);
    procedure MoveBy(ADelta: Int64);

    // Trim clip
    procedure TrimStart(ANewStart: Int64);
    procedure TrimEnd(ANewEnd: Int64);

    // Split clip at position (returns new clip for right side)
    function Split(APosition: Int64): TSedaiClip; virtual;

    // Properties
    property ClipType: TClipType read FClipType;
    property Name: string read FName write FName;
    property Color: Cardinal read FColor write FColor;

    property StartPosition: Int64 read FStartPosition write FStartPosition;
    property EndPosition: Int64 read FEndPosition write FEndPosition;
    property ClipLength: Int64 read GetLength write SetClipLength;
    property SourceOffset: Int64 read FSourceOffset write FSourceOffset;

    property FadeInLength: Integer read FFadeInLength write FFadeInLength;
    property FadeOutLength: Integer read FFadeOutLength write FFadeOutLength;
    property FadeInCurve: TFadeCurve read FFadeInCurve write FFadeInCurve;
    property FadeOutCurve: TFadeCurve read FFadeOutCurve write FFadeOutCurve;

    property Gain: Single read FGain write FGain;
    property Muted: Boolean read FMuted write FMuted;
    property Locked: Boolean read FLocked write FLocked;
  end;

  { TSedaiAudioClip }
  // Audio clip with waveform data
  TSedaiAudioClip = class(TSedaiClip)
  private
    FAudioBuffer: TSedaiAudioBuffer;
    FOwnsBuffer: Boolean;

    // Time stretch
    FStretchMode: TStretchMode;
    FStretchRatio: Double;        // 1.0 = original, 2.0 = double length
    FPitchShift: Single;          // Semitones

    // Warp markers (for elastic stretch)
    FWarpMarkers: array of record
      SourcePos: Int64;           // Position in source
      TimelinePos: Int64;         // Position on timeline
    end;
    FWarpMarkerCount: Integer;

  public
    constructor Create; override;
    destructor Destroy; override;

    // Load audio buffer
    procedure LoadBuffer(ABuffer: TSedaiAudioBuffer; ATakeOwnership: Boolean = False);
    procedure ClearBuffer;

    // Warp marker management
    function AddWarpMarker(ASourcePos, ATimelinePos: Int64): Integer;
    procedure RemoveWarpMarker(AIndex: Integer);
    procedure ClearWarpMarkers;

    // Read audio at timeline position
    procedure ReadAudio(ATimelinePos: Int64; AOutput: PSingle;
                        AFrameCount: Integer; AChannels: Integer);

    // Get source sample at position (with interpolation)
    function GetSampleAt(ASourcePos: Double; AChannel: Integer): Single;

    // Split override
    function Split(APosition: Int64): TSedaiClip; override;

    // Properties
    property AudioBuffer: TSedaiAudioBuffer read FAudioBuffer;
    property StretchMode: TStretchMode read FStretchMode write FStretchMode;
    property StretchRatio: Double read FStretchRatio write FStretchRatio;
    property PitchShift: Single read FPitchShift write FPitchShift;
    property WarpMarkerCount: Integer read FWarpMarkerCount;
  end;

  // MIDI note event
  TMIDINoteEvent = record
    Position: Int64;              // Tick position
    Duration: Int64;              // Duration in ticks
    Note: Byte;                   // MIDI note number
    Velocity: Byte;               // Note velocity
    Channel: Byte;                // MIDI channel
  end;

  // Array of MIDI note events
  TMIDINoteEventArray = array of TMIDINoteEvent;

  // MIDI CC event
  TMIDICCEvent = record
    Position: Int64;              // Tick position
    Controller: Byte;             // CC number
    Value: Byte;                  // CC value
    Channel: Byte;                // MIDI channel
  end;

  { TSedaiMIDIClip }
  // MIDI clip with note events
  TSedaiMIDIClip = class(TSedaiClip)
  private
    FNotes: array of TMIDINoteEvent;
    FNoteCount: Integer;

    FCCEvents: array of TMIDICCEvent;
    FCCEventCount: Integer;

    FTicksPerBeat: Integer;

  public
    constructor Create; override;
    destructor Destroy; override;

    // Note management
    function AddNote(APosition: Int64; ADuration: Int64; ANote, AVelocity: Byte;
                     AChannel: Byte = 0): Integer;
    procedure RemoveNote(AIndex: Integer);
    procedure ClearNotes;
    function GetNote(AIndex: Integer): TMIDINoteEvent;

    // CC event management
    function AddCCEvent(APosition: Int64; AController, AValue: Byte;
                        AChannel: Byte = 0): Integer;
    procedure RemoveCCEvent(AIndex: Integer);
    procedure ClearCCEvents;
    function GetCCEvent(AIndex: Integer): TMIDICCEvent;

    // Query notes at position
    function GetNotesAtPosition(APosition: Int64): TIntegerDynArray;
    function GetActiveNotesAtPosition(APosition: Int64): TIntegerDynArray;

    // Quantize notes
    procedure QuantizeNotes(AGridSize: Integer; AStrength: Single = 1.0);

    // Transpose
    procedure Transpose(ASemitones: Integer);

    // Split override
    function Split(APosition: Int64): TSedaiClip; override;

    // Properties
    property NoteCount: Integer read FNoteCount;
    property CCEventCount: Integer read FCCEventCount;
    property TicksPerBeat: Integer read FTicksPerBeat write FTicksPerBeat;
  end;

implementation

{ TSedaiClip }

constructor TSedaiClip.Create;
begin
  inherited Create;

  FClipType := ctAudio;
  FName := 'Clip';
  FColor := $FF808080;

  FStartPosition := 0;
  FEndPosition := Round(FSampleRate);  // 1 second default

  FSourceOffset := 0;

  FFadeInLength := 0;
  FFadeOutLength := 0;
  FFadeInCurve := fcLinear;
  FFadeOutCurve := fcLinear;

  FGain := 1.0;
  FMuted := False;
  FLocked := False;
end;

procedure TSedaiClip.Reset;
begin
  inherited Reset;
end;

function TSedaiClip.GetLength: Int64;
begin
  Result := FEndPosition - FStartPosition;
end;

procedure TSedaiClip.SetClipLength(AValue: Int64);
begin
  if AValue < 1 then AValue := 1;
  FEndPosition := FStartPosition + AValue;
end;

function TSedaiClip.GetFadeGain(APosition: Int64): Single;
var
  RelPos: Int64;
  NormPos: Single;
begin
  Result := 1.0;
  RelPos := APosition - FStartPosition;

  // Fade in
  if (FFadeInLength > 0) and (RelPos < FFadeInLength) then
  begin
    NormPos := RelPos / FFadeInLength;
    Result := CalculateFadeCurve(NormPos, FFadeInCurve, True);
  end;

  // Fade out
  if (FFadeOutLength > 0) and (RelPos > GetLength - FFadeOutLength) then
  begin
    NormPos := (GetLength - RelPos) / FFadeOutLength;
    Result := Result * CalculateFadeCurve(NormPos, FFadeOutCurve, False);
  end;
end;

function TSedaiClip.CalculateFadeCurve(ANormalizedPos: Single; ACurve: TFadeCurve;
  AFadeIn: Boolean): Single;
begin
  case ACurve of
    fcLinear:
      Result := ANormalizedPos;

    fcLogarithmic:
      Result := Ln(1 + ANormalizedPos * (Exp(1) - 1));

    fcExponential:
      Result := (Exp(ANormalizedPos) - 1) / (Exp(1) - 1);

    fcSCurve:
      Result := (1 - Cos(ANormalizedPos * PI)) * 0.5;

    fcEqualPower:
      begin
        if AFadeIn then
          Result := Sin(ANormalizedPos * PI * 0.5)
        else
          Result := Cos((1 - ANormalizedPos) * PI * 0.5);
      end;

    else
      Result := ANormalizedPos;
  end;
end;

function TSedaiClip.ContainsPosition(APosition: Int64): Boolean;
begin
  Result := (APosition >= FStartPosition) and (APosition < FEndPosition);
end;

function TSedaiClip.GetRelativePosition(ATimelinePos: Int64): Int64;
begin
  Result := ATimelinePos - FStartPosition;
end;

function TSedaiClip.GetSourcePosition(ATimelinePos: Int64): Int64;
begin
  Result := GetRelativePosition(ATimelinePos) + FSourceOffset;
end;

procedure TSedaiClip.MoveTo(APosition: Int64);
var
  Len: Int64;
begin
  if FLocked then Exit;

  Len := GetLength;
  FStartPosition := APosition;
  FEndPosition := APosition + Len;
end;

procedure TSedaiClip.MoveBy(ADelta: Int64);
begin
  if FLocked then Exit;

  FStartPosition := FStartPosition + ADelta;
  FEndPosition := FEndPosition + ADelta;
end;

procedure TSedaiClip.TrimStart(ANewStart: Int64);
var
  Delta: Int64;
begin
  if FLocked then Exit;
  if ANewStart >= FEndPosition then Exit;

  Delta := ANewStart - FStartPosition;
  FSourceOffset := FSourceOffset + Delta;
  FStartPosition := ANewStart;
end;

procedure TSedaiClip.TrimEnd(ANewEnd: Int64);
begin
  if FLocked then Exit;
  if ANewEnd <= FStartPosition then Exit;

  FEndPosition := ANewEnd;
end;

function TSedaiClip.Split(APosition: Int64): TSedaiClip;
begin
  Result := nil;  // Override in subclasses
end;

{ TSedaiAudioClip }

constructor TSedaiAudioClip.Create;
begin
  inherited Create;

  FClipType := ctAudio;
  FAudioBuffer := nil;
  FOwnsBuffer := False;

  FStretchMode := smOff;
  FStretchRatio := 1.0;
  FPitchShift := 0.0;

  FWarpMarkerCount := 0;
end;

destructor TSedaiAudioClip.Destroy;
begin
  if FOwnsBuffer and Assigned(FAudioBuffer) then
    FAudioBuffer.Free;

  SetLength(FWarpMarkers, 0);
  inherited Destroy;
end;

procedure TSedaiAudioClip.LoadBuffer(ABuffer: TSedaiAudioBuffer; ATakeOwnership: Boolean);
begin
  if FOwnsBuffer and Assigned(FAudioBuffer) then
    FAudioBuffer.Free;

  FAudioBuffer := ABuffer;
  FOwnsBuffer := ATakeOwnership;

  if Assigned(FAudioBuffer) then
    ClipLength := FAudioBuffer.SampleCount;
end;

procedure TSedaiAudioClip.ClearBuffer;
begin
  if FOwnsBuffer and Assigned(FAudioBuffer) then
    FAudioBuffer.Free;

  FAudioBuffer := nil;
  FOwnsBuffer := False;
end;

function TSedaiAudioClip.AddWarpMarker(ASourcePos, ATimelinePos: Int64): Integer;
begin
  Result := FWarpMarkerCount;
  Inc(FWarpMarkerCount);
  SetLength(FWarpMarkers, FWarpMarkerCount);

  FWarpMarkers[Result].SourcePos := ASourcePos;
  FWarpMarkers[Result].TimelinePos := ATimelinePos;
end;

procedure TSedaiAudioClip.RemoveWarpMarker(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FWarpMarkerCount) then Exit;

  for I := AIndex to FWarpMarkerCount - 2 do
    FWarpMarkers[I] := FWarpMarkers[I + 1];

  Dec(FWarpMarkerCount);
  SetLength(FWarpMarkers, FWarpMarkerCount);
end;

procedure TSedaiAudioClip.ClearWarpMarkers;
begin
  FWarpMarkerCount := 0;
  SetLength(FWarpMarkers, 0);
end;

procedure TSedaiAudioClip.ReadAudio(ATimelinePos: Int64; AOutput: PSingle;
  AFrameCount: Integer; AChannels: Integer);
var
  I, Ch: Integer;
  SourcePos: Double;
  TimelinePos: Int64;
  FadeGain, Sample: Single;
begin
  if FAudioBuffer = nil then
  begin
    for I := 0 to AFrameCount * AChannels - 1 do
      AOutput[I] := 0.0;
    Exit;
  end;

  for I := 0 to AFrameCount - 1 do
  begin
    TimelinePos := ATimelinePos + I;

    if ContainsPosition(TimelinePos) and not FMuted then
    begin
      // Calculate source position (with stretch)
      case FStretchMode of
        smOff, smRepitch:
          SourcePos := GetSourcePosition(TimelinePos) * FStretchRatio;

        smElastic:
          // TODO: Implement elastic time stretch
          SourcePos := GetSourcePosition(TimelinePos);

        smSlice:
          // TODO: Implement beat slicing
          SourcePos := GetSourcePosition(TimelinePos);

        else
          SourcePos := GetSourcePosition(TimelinePos);
      end;

      // Get fade gain
      FadeGain := GetFadeGain(TimelinePos) * FGain;

      // Read samples
      for Ch := 0 to AChannels - 1 do
      begin
        Sample := GetSampleAt(SourcePos, Ch mod FAudioBuffer.Channels);
        AOutput[I * AChannels + Ch] := Sample * FadeGain;
      end;
    end
    else
    begin
      // Outside clip or muted
      for Ch := 0 to AChannels - 1 do
        AOutput[I * AChannels + Ch] := 0.0;
    end;
  end;
end;

function TSedaiAudioClip.GetSampleAt(ASourcePos: Double; AChannel: Integer): Single;
var
  Index0, Index1: Integer;
  Frac: Single;
begin
  Result := 0.0;

  if FAudioBuffer = nil then Exit;
  if ASourcePos < 0 then Exit;
  if ASourcePos >= FAudioBuffer.SampleCount then Exit;

  // Linear interpolation
  Index0 := Trunc(ASourcePos);
  Index1 := Index0 + 1;
  Frac := ASourcePos - Index0;

  if Index1 >= FAudioBuffer.SampleCount then
    Index1 := FAudioBuffer.SampleCount - 1;

  Result := FAudioBuffer.GetSample(AChannel, Index0) * (1.0 - Frac) +
            FAudioBuffer.GetSample(AChannel, Index1) * Frac;
end;

function TSedaiAudioClip.Split(APosition: Int64): TSedaiClip;
var
  NewClip: TSedaiAudioClip;
begin
  Result := nil;

  if not ContainsPosition(APosition) then Exit;
  if FLocked then Exit;

  NewClip := TSedaiAudioClip.Create;
  NewClip.FName := FName + ' (split)';
  NewClip.FColor := FColor;
  NewClip.FStartPosition := APosition;
  NewClip.FEndPosition := FEndPosition;
  NewClip.FSourceOffset := GetSourcePosition(APosition);
  NewClip.FAudioBuffer := FAudioBuffer;
  NewClip.FOwnsBuffer := False;
  NewClip.FGain := FGain;
  NewClip.FStretchMode := FStretchMode;
  NewClip.FStretchRatio := FStretchRatio;
  NewClip.FPitchShift := FPitchShift;

  // Adjust original clip
  FEndPosition := APosition;

  Result := NewClip;
end;

{ TSedaiMIDIClip }

constructor TSedaiMIDIClip.Create;
begin
  inherited Create;

  FClipType := ctMIDI;
  FNoteCount := 0;
  FCCEventCount := 0;
  FTicksPerBeat := 960;
end;

destructor TSedaiMIDIClip.Destroy;
begin
  SetLength(FNotes, 0);
  SetLength(FCCEvents, 0);
  inherited Destroy;
end;

function TSedaiMIDIClip.AddNote(APosition: Int64; ADuration: Int64;
  ANote, AVelocity: Byte; AChannel: Byte): Integer;
begin
  Result := FNoteCount;
  Inc(FNoteCount);
  SetLength(FNotes, FNoteCount);

  FNotes[Result].Position := APosition;
  FNotes[Result].Duration := ADuration;
  FNotes[Result].Note := ANote;
  FNotes[Result].Velocity := AVelocity;
  FNotes[Result].Channel := AChannel;
end;

procedure TSedaiMIDIClip.RemoveNote(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FNoteCount) then Exit;

  for I := AIndex to FNoteCount - 2 do
    FNotes[I] := FNotes[I + 1];

  Dec(FNoteCount);
  SetLength(FNotes, FNoteCount);
end;

procedure TSedaiMIDIClip.ClearNotes;
begin
  FNoteCount := 0;
  SetLength(FNotes, 0);
end;

function TSedaiMIDIClip.GetNote(AIndex: Integer): TMIDINoteEvent;
begin
  if (AIndex >= 0) and (AIndex < FNoteCount) then
    Result := FNotes[AIndex]
  else
  begin
    Result.Position := 0;
    Result.Duration := 0;
    Result.Note := 0;
    Result.Velocity := 0;
    Result.Channel := 0;
  end;
end;

function TSedaiMIDIClip.AddCCEvent(APosition: Int64; AController, AValue: Byte;
  AChannel: Byte): Integer;
begin
  Result := FCCEventCount;
  Inc(FCCEventCount);
  SetLength(FCCEvents, FCCEventCount);

  FCCEvents[Result].Position := APosition;
  FCCEvents[Result].Controller := AController;
  FCCEvents[Result].Value := AValue;
  FCCEvents[Result].Channel := AChannel;
end;

procedure TSedaiMIDIClip.RemoveCCEvent(AIndex: Integer);
var
  I: Integer;
begin
  if (AIndex < 0) or (AIndex >= FCCEventCount) then Exit;

  for I := AIndex to FCCEventCount - 2 do
    FCCEvents[I] := FCCEvents[I + 1];

  Dec(FCCEventCount);
  SetLength(FCCEvents, FCCEventCount);
end;

procedure TSedaiMIDIClip.ClearCCEvents;
begin
  FCCEventCount := 0;
  SetLength(FCCEvents, 0);
end;

function TSedaiMIDIClip.GetCCEvent(AIndex: Integer): TMIDICCEvent;
begin
  if (AIndex >= 0) and (AIndex < FCCEventCount) then
    Result := FCCEvents[AIndex]
  else
  begin
    Result.Position := 0;
    Result.Controller := 0;
    Result.Value := 0;
    Result.Channel := 0;
  end;
end;

function TSedaiMIDIClip.GetNotesAtPosition(APosition: Int64): TIntegerDynArray;
var
  I, Count: Integer;
begin
  SetLength(Result, FNoteCount);
  Count := 0;

  for I := 0 to FNoteCount - 1 do
  begin
    if FNotes[I].Position = APosition then
    begin
      Result[Count] := I;
      Inc(Count);
    end;
  end;

  SetLength(Result, Count);
end;

function TSedaiMIDIClip.GetActiveNotesAtPosition(APosition: Int64): TIntegerDynArray;
var
  I, Count: Integer;
begin
  SetLength(Result, FNoteCount);
  Count := 0;

  for I := 0 to FNoteCount - 1 do
  begin
    if (APosition >= FNotes[I].Position) and
       (APosition < FNotes[I].Position + FNotes[I].Duration) then
    begin
      Result[Count] := I;
      Inc(Count);
    end;
  end;

  SetLength(Result, Count);
end;

procedure TSedaiMIDIClip.QuantizeNotes(AGridSize: Integer; AStrength: Single);
var
  I: Integer;
  OrigPos, QuantPos: Int64;
begin
  if AGridSize <= 0 then Exit;
  if AStrength < 0 then AStrength := 0;
  if AStrength > 1 then AStrength := 1;

  for I := 0 to FNoteCount - 1 do
  begin
    OrigPos := FNotes[I].Position;
    QuantPos := Round(OrigPos / AGridSize) * AGridSize;
    FNotes[I].Position := Round(OrigPos + (QuantPos - OrigPos) * AStrength);
  end;
end;

procedure TSedaiMIDIClip.Transpose(ASemitones: Integer);
var
  I: Integer;
  NewNote: Integer;
begin
  for I := 0 to FNoteCount - 1 do
  begin
    NewNote := FNotes[I].Note + ASemitones;
    if NewNote < 0 then NewNote := 0;
    if NewNote > 127 then NewNote := 127;
    FNotes[I].Note := NewNote;
  end;
end;

function TSedaiMIDIClip.Split(APosition: Int64): TSedaiClip;
var
  NewClip: TSedaiMIDIClip;
  I: Integer;
  RelPos: Int64;
begin
  Result := nil;

  if not ContainsPosition(APosition) then Exit;
  if FLocked then Exit;

  NewClip := TSedaiMIDIClip.Create;
  NewClip.FName := FName + ' (split)';
  NewClip.FColor := FColor;
  NewClip.FStartPosition := APosition;
  NewClip.FEndPosition := FEndPosition;
  NewClip.FTicksPerBeat := FTicksPerBeat;

  // Copy notes that start after split point
  RelPos := GetRelativePosition(APosition);
  for I := 0 to FNoteCount - 1 do
  begin
    if FNotes[I].Position >= RelPos then
      NewClip.AddNote(FNotes[I].Position - RelPos, FNotes[I].Duration,
                      FNotes[I].Note, FNotes[I].Velocity, FNotes[I].Channel);
  end;

  // Remove notes from original that are now in new clip
  I := 0;
  while I < FNoteCount do
  begin
    if FNotes[I].Position >= RelPos then
      RemoveNote(I)
    else
      Inc(I);
  end;

  // Adjust original clip
  FEndPosition := APosition;

  Result := NewClip;
end;

end.
