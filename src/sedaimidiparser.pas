{*
 * Sedai Audio Foundation - Professional audio synthesis library
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    You may redistribute and/or modify it under the terms of the GNU GPL v3
 *    as published by the Free Software Foundation.
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

unit SedaiMIDIParser;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, SedaiMIDITypes;

type

  { TSedaiMIDIParser }

  TSedaiMIDIParser = class
  private
    class function ValidateMIDITiming(const AMIDIFile: TMIDIFile): Boolean;
  public
    // Main loading function
    class function LoadMIDIFile(const AFilename: string): TMIDIFile;

    // Parsing components
    class function ParseMIDIHeader(AStream: TStream): TMIDIHeader;
    class function ParseMIDITrack(AStream: TStream): TMIDITrack;
    class function ParseVariableLength(AStream: TStream): Cardinal;
    class function ParseMIDIEvent(AStream: TStream; var ARunningStatus: Byte): TMIDIEvent;

    // Meta event parsing
    class procedure ParseMetaEvent(AStream: TStream; var AEvent: TMIDIEvent);
    class procedure ParseSysExEvent(AStream: TStream; var AEvent: TMIDIEvent);

    // Utility functions
    class function ValidateMIDIFile(const AFilename: string): Boolean;
    class function GetMIDIFileInfo(const AFilename: string): string;
    class procedure CalculateAbsoluteTimes(var AMIDIFile: TMIDIFile);
    class procedure BuildTempoMap(var AMIDIFile: TMIDIFile);

    // Debug functions
    class procedure PrintMIDIFileInfo(const AMIDIFile: TMIDIFile);
    class procedure PrintTrackEvents(const ATrack: TMIDITrack; AMaxEvents: Integer = 20);
  end;

implementation

// Load complete MIDI file
class function TSedaiMIDIParser.LoadMIDIFile(const AFilename: string): TMIDIFile;
var
  AStream: TFileStream;
  i: Integer;
begin
  FillChar(Result, SizeOf(TMIDIFile), 0);
  Result.Filename := AFilename;
  Result.IsLoaded := False;

  if not FileExists(AFilename) then
  begin
    WriteLn('ERROR: MIDI file not found: ', AFilename);
    Exit;
  end;

  WriteLn('Loading MIDI file: ', ExtractFileName(AFilename));

  try
    AStream := TFileStream.Create(AFilename, fmOpenRead);
    try
      // Parse header
      Result.Header := ParseMIDIHeader(AStream);
      if Result.Header.TrackCount = 0 then
      begin
        WriteLn('ERROR: Invalid MIDI file header');
        Exit;
      end;

      WriteLn('  Format: ', Result.Header.Format);
      WriteLn('  Tracks: ', Result.Header.TrackCount);
      WriteLn('  Division: ', Result.Header.Division, ' ticks/quarter');

      // Parse tracks
      SetLength(Result.Tracks, Result.Header.TrackCount);
      for i := 0 to Result.Header.TrackCount - 1 do
      begin
        WriteLn('  Parsing track ', i + 1, '...');
        Result.Tracks[i] := ParseMIDITrack(AStream);
        WriteLn('    Events: ', Result.Tracks[i].EventCount);
        if Result.Tracks[i].Name <> '' then
          WriteLn('    Name: ', Result.Tracks[i].Name);
      end;

      // Post-processing
      CalculateAbsoluteTimes(Result);
      BuildTempoMap(Result);

      // NEW: Timing validation
      ValidateMIDITiming(Result);

      Result.IsLoaded := True;
      WriteLn('MIDI file loaded successfully!');

    finally
      AStream.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR loading MIDI file: ', E.Message);
      Result.IsLoaded := False;
    end;
  end;
end;

// Parse MIDI header chunk
class function TSedaiMIDIParser.ParseMIDIHeader(AStream: TStream): TMIDIHeader;
var
  AChunkType: array[0..3] of Char;
  AChunkLength: Cardinal;
  AFormat, ATrackCount, ADivision: Word;
begin
  FillChar(Result, SizeOf(TMIDIHeader), 0);

  // Read chunk type (should be "MThd")
  AStream.ReadBuffer(AChunkType, 4);
  if not ((AChunkType[0] = 'M') and (AChunkType[1] = 'T') and
          (AChunkType[2] = 'h') and (AChunkType[3] = 'd')) then
  begin
    WriteLn('ERROR: Invalid MIDI header chunk');
    Exit;
  end;

  // Read chunk length (should be 6)
  AStream.ReadBuffer(AChunkLength, 4);
  AChunkLength := BEtoN(AChunkLength); // Big endian to native
  if AChunkLength <> 6 then
  begin
    WriteLn('ERROR: Invalid MIDI header length: ', AChunkLength);
    Exit;
  end;

  // Read header data
  AStream.ReadBuffer(AFormat, 2);
  AStream.ReadBuffer(ATrackCount, 2);
  AStream.ReadBuffer(ADivision, 2);

  // Convert from big endian
  Result.Format := BEtoN(AFormat);
  Result.TrackCount := BEtoN(ATrackCount);
  Result.Division := BEtoN(ADivision);
end;

// Parse a single MIDI track
class function TSedaiMIDIParser.ParseMIDITrack(AStream: TStream): TMIDITrack;
var
  AChunkType: array[0..3] of Char;
  AChunkLength: Cardinal;
  AStartPos, AEndPos: Int64;
  AEvent: TMIDIEvent;
  ARunningStatus: Byte;
  AEvents: array of TMIDIEvent;
  AEventCount: Integer;
  i: Integer;
begin
  FillChar(Result, SizeOf(TMIDITrack), 0);
  ARunningStatus := 0;
  AEventCount := 0;
  SetLength(AEvents, 1000); // Initial capacity

  // Read track chunk header
  AStream.ReadBuffer(AChunkType, 4);
  if not ((AChunkType[0] = 'M') and (AChunkType[1] = 'T') and
          (AChunkType[2] = 'r') and (AChunkType[3] = 'k')) then
  begin
    WriteLn('ERROR: Invalid track chunk type');
    Exit;
  end;

  AStream.ReadBuffer(AChunkLength, 4);
  AChunkLength := BEtoN(AChunkLength);
  Result.Length := AChunkLength;

  AStartPos := AStream.Position;
  AEndPos := AStartPos + AChunkLength;

  // Parse events
  while AStream.Position < AEndPos do
  begin
    AEvent := ParseMIDIEvent(AStream, ARunningStatus);

    // Expand array if needed
    if AEventCount >= Length(AEvents) then
      SetLength(AEvents, Length(AEvents) * 2);

    AEvents[AEventCount] := AEvent;
    Inc(AEventCount);

    // Stop at end of track
    if (AEvent.MessageType = mtSystem) and (AEvent.Data1 = $2F) then
      Break;
  end;

  // Copy to final array
  Result.EventCount := AEventCount;
  SetLength(Result.Events, AEventCount);
  for i := 0 to AEventCount - 1 do
    Result.Events[i] := AEvents[i];
end;

// Parse variable length quantity (VLQ)
class function TSedaiMIDIParser.ParseVariableLength(AStream: TStream): Cardinal;
var
  AByte: Byte;
  AResult: Cardinal;
begin
  AResult := 0;

  repeat
    if AStream.Read(AByte, 1) <> 1 then
    begin
      WriteLn('ERROR: Unexpected end of stream while reading VLQ');
      Result := 0;
      Exit;
    end;

    // FIXED: Correct calculation of Variable Length Quantity
    AResult := (AResult shl 7) or (AByte and $7F);

    // Check for runaway VLQ (max 4 bytes per MIDI spec)
    if AResult > $0FFFFFFF then
    begin
      WriteLn('ERROR: VLQ overflow - corrupted MIDI file');
      Result := 0;
      Exit;
    end;

  until (AByte and $80) = 0;  // Continue until MSB is 0

  Result := AResult;
end;

// Parse a single MIDI event
class function TSedaiMIDIParser.ParseMIDIEvent(AStream: TStream; var ARunningStatus: Byte): TMIDIEvent;
var
  AStatusByte: Byte;
  AData1, AData2: Byte;
  ADeltaTime: Cardinal;
begin
  FillChar(Result, SizeOf(TMIDIEvent), 0);

  // FIXED: Parse delta time with error checking
  ADeltaTime := ParseVariableLength(AStream);
  Result.DeltaTime := ADeltaTime;

  // DEBUG: Log anomalous delta times
  if ADeltaTime > 10000 then  // Delta time molto grande (più di ~5 secondi a 480 PPQN)
    WriteLn('WARNING: Large delta time: ', ADeltaTime, ' ticks');

  // Read status byte
  if AStream.Read(AStatusByte, 1) <> 1 then
  begin
    WriteLn('ERROR: Cannot read status byte');
    Exit;
  end;

  // Handle running status
  if (AStatusByte and $80) = 0 then
  begin
    // Running status - use previous status
    Result.IsRunningStatus := True;
    AStatusByte := ARunningStatus;
    AStream.Seek(-1, soCurrent); // Back up one byte
  end
  else
  begin
    ARunningStatus := AStatusByte;
    Result.IsRunningStatus := False;
  end;

  // FIXED: Correct interpretation of message type
  if AStatusByte < $F0 then
  begin
    // Channel messages (0x8n to 0xEn)
    Result.MessageType := TMIDIMessageType(AStatusByte and $F0);
    Result.Channel := AStatusByte and $0F;
  end
  else
  begin
    // System messages (0xFn)
    Result.MessageType := mtSystem;
    Result.Channel := 0;
  end;

  // Parse data bytes based on message type
  case Result.MessageType of
    mtNoteOff, mtNoteOn, mtAftertouch, mtController, mtPitchBend:
      begin
        if AStream.Read(AData1, 1) <> 1 then Exit;
        if AStream.Read(AData2, 1) <> 1 then Exit;
        Result.Data1 := AData1;
        Result.Data2 := AData2;
      end;

    mtProgram, mtPressure:
      begin
        if AStream.Read(AData1, 1) <> 1 then Exit;
        Result.Data1 := AData1;
        Result.Data2 := 0;
      end;

    mtSystem:
      begin
        case AStatusByte of
          $F0: ParseSysExEvent(AStream, Result);  // SysEx
          $FF: ParseMetaEvent(AStream, Result);   // Meta event
        else
          // Other system messages
          Result.Data1 := AStatusByte;
          Result.Data2 := 0;
        end;
      end;
  end;
end;

// Parse meta event (tempo, time signature, etc.)
class procedure TSedaiMIDIParser.ParseMetaEvent(AStream: TStream; var AEvent: TMIDIEvent);
var
  AMetaType: Byte;
  ALength: Cardinal;
  AData: array of Byte;
  AText: string;
  i: Integer;
begin
  AStream.ReadBuffer(AMetaType, 1);
  ALength := ParseVariableLength(AStream);

  AEvent.Data1 := AMetaType;
  AEvent.Data2 := ALength;

  if ALength > 0 then
  begin
    SetLength(AData, ALength);
    AStream.ReadBuffer(AData[0], ALength);

    // Handle common meta events
    case AMetaType of
      $03: // Track name
        begin
          SetLength(AText, ALength);
          for i := 0 to ALength - 1 do
            AText[i + 1] := Chr(AData[i]);
          // Store track name (would need track reference)
        end;
      $51: // Set tempo
        begin
          if ALength = 3 then
          begin
            // Microseconds per quarter note
            // Store in some global tempo map
          end;
        end;
    end;
  end;
end;

// Parse system exclusive event
class procedure TSedaiMIDIParser.ParseSysExEvent(AStream: TStream; var AEvent: TMIDIEvent);
var
  ALength: Cardinal;
begin
  ALength := ParseVariableLength(AStream);
  AEvent.Data1 := $F0;
  AEvent.Data2 := ALength;

  // Skip SysEx data for now
  AStream.Seek(ALength, soCurrent);
end;

// Validate MIDI file format
class function TSedaiMIDIParser.ValidateMIDIFile(const AFilename: string): Boolean;
var
  AStream: TFileStream;
  AHeader: array[0..3] of Char;
begin
  Result := False;

  if not FileExists(AFilename) then Exit;

  try
    AStream := TFileStream.Create(AFilename, fmOpenRead);
    try
      if AStream.Size < 14 then Exit; // Too small for MIDI header

      AStream.ReadBuffer(AHeader, 4);
      Result := (AHeader[0] = 'M') and (AHeader[1] = 'T') and
                (AHeader[2] = 'h') and (AHeader[3] = 'd');
    finally
      AStream.Free;
    end;
  except
    Result := False;
  end;
end;

// Get MIDI file information
class function TSedaiMIDIParser.GetMIDIFileInfo(const AFilename: string): string;
var
  AMIDIFile: TMIDIFile;
begin
  if not ValidateMIDIFile(AFilename) then
  begin
    Result := 'Invalid MIDI file';
    Exit;
  end;

  AMIDIFile := LoadMIDIFile(AFilename);
  if AMIDIFile.IsLoaded then
  begin
    Result := Format('MIDI File: Format %d, %d tracks, %d ticks/quarter',
      [AMIDIFile.Header.Format, AMIDIFile.Header.TrackCount, AMIDIFile.Header.Division]);
  end
  else
    Result := 'Failed to load MIDI file';
end;

// Calculate absolute times for all events
class procedure TSedaiMIDIParser.CalculateAbsoluteTimes(var AMIDIFile: TMIDIFile);
var
  i, j: Integer;
  ACurrentTime: Cardinal;
  AMaxDelta: Cardinal;
begin
  WriteLn('Calculating absolute times for MIDI events...');

  for i := 0 to Length(AMIDIFile.Tracks) - 1 do
  begin
    ACurrentTime := 0;
    AMaxDelta := 0;

    WriteLn('  Track ', i, ': Processing ', AMIDIFile.Tracks[i].EventCount, ' events');

    for j := 0 to AMIDIFile.Tracks[i].EventCount - 1 do
    begin
      with AMIDIFile.Tracks[i].Events[j] do
      begin
        // Track largest delta time for debugging
        if DeltaTime > AMaxDelta then
          AMaxDelta := DeltaTime;

        ACurrentTime := ACurrentTime + DeltaTime;
        AbsoluteTime := ACurrentTime;

        // DEBUG: Log first few events of each track
        if j < 5 then
          WriteLn(Format('    Event %d: Delta=%d, Absolute=%d, Type=%s',
            [j, DeltaTime, AbsoluteTime, MIDIMessageTypeToString(MessageType)]));
      end;
    end;

    WriteLn(Format('    Track %d: Final time=%d, Max delta=%d', [i, ACurrentTime, AMaxDelta]));

    if ACurrentTime > AMIDIFile.TotalTicks then
      AMIDIFile.TotalTicks := ACurrentTime;
  end;

  WriteLn('  Total file length: ', AMIDIFile.TotalTicks, ' ticks');
end;

// 4. ADD validation function for debug:
class function TSedaiMIDIParser.ValidateMIDITiming(const AMIDIFile: TMIDIFile): Boolean;
var
  i, j: Integer;
  AEventCount: Integer;
  ANoteOnCount, ANoteOffCount: Integer;
begin
  Result := True;
  WriteLn('=== MIDI Timing Validation ===');

  AEventCount := 0;
  ANoteOnCount := 0;
  ANoteOffCount := 0;

  for i := 0 to Length(AMIDIFile.Tracks) - 1 do
  begin
    for j := 0 to AMIDIFile.Tracks[i].EventCount - 1 do
    begin
      Inc(AEventCount);

      with AMIDIFile.Tracks[i].Events[j] do
      begin
        // Count note events
        if MessageType = mtNoteOn then Inc(ANoteOnCount);
        if MessageType = mtNoteOff then Inc(ANoteOffCount);

        // Check for timing anomalies
        if DeltaTime > 50000 then  // More than ~26 seconds at 480 PPQN, 120 BPM
        begin
          WriteLn('WARNING: Suspicious delta time in track ', i, ' event ', j, ': ', DeltaTime);
          // Don't fail, but warn
        end;

        if AbsoluteTime > AMIDIFile.TotalTicks then
        begin
          WriteLn('ERROR: Event absolute time exceeds total ticks');
          Result := False;
        end;
      end;
    end;
  end;

  WriteLn('Validation results:');
  WriteLn('  Total events: ', AEventCount);
  WriteLn('  Note ON events: ', ANoteOnCount);
  WriteLn('  Note OFF events: ', ANoteOffCount);
  WriteLn('  File duration: ', AMIDIFile.TotalTicks, ' ticks');
  WriteLn('  Estimated duration: ', (AMIDIFile.TotalTicks / 480.0 / 2.0):0:1, ' seconds (at 120 BPM)');

  if ANoteOnCount = 0 then
  begin
    WriteLn('WARNING: No Note ON events found - this may not be a musical MIDI file');
  end;

  WriteLn('=== End Validation ===');
end;

// Build tempo map for timing calculations
class procedure TSedaiMIDIParser.BuildTempoMap(var AMIDIFile: TMIDIFile);
var
  i, j: Integer;
  ATempoCount: Integer;
begin
  // Default tempo: 120 BPM = 500,000 microseconds per quarter note
  ATempoCount := 1;
  SetLength(AMIDIFile.TempoMap, 1);
  AMIDIFile.TempoMap[0].Tick := 0;
  AMIDIFile.TempoMap[0].MicrosecondsPerQuarter := 500000;

  // Scan for tempo changes (meta event $51)
  for i := 0 to Length(AMIDIFile.Tracks) - 1 do
  begin
    for j := 0 to AMIDIFile.Tracks[i].EventCount - 1 do
    begin
      with AMIDIFile.Tracks[i].Events[j] do
      begin
        if (MessageType = mtSystem) and (Data1 = $51) then
        begin
          // Found tempo change - would need to parse the actual tempo value
          // For now, keep default tempo
        end;
      end;
    end;
  end;
end;

// Debug: Print MIDI file information
class procedure TSedaiMIDIParser.PrintMIDIFileInfo(const AMIDIFile: TMIDIFile);
var
  i: Integer;
begin
  WriteLn('=== MIDI File Information ===');
  WriteLn('File: ', ExtractFileName(AMIDIFile.Filename));
  WriteLn('Format: ', AMIDIFile.Header.Format);
  WriteLn('Tracks: ', AMIDIFile.Header.TrackCount);
  WriteLn('Division: ', AMIDIFile.Header.Division, ' ticks/quarter');
  WriteLn('Total Ticks: ', AMIDIFile.TotalTicks);
  WriteLn('Loaded: ', BoolToStr(AMIDIFile.IsLoaded, True));
  WriteLn('');

  for i := 0 to Length(AMIDIFile.Tracks) - 1 do
  begin
    WriteLn('Track ', i, ': ', AMIDIFile.Tracks[i].EventCount, ' events');
    if AMIDIFile.Tracks[i].Name <> '' then
      WriteLn('  Name: ', AMIDIFile.Tracks[i].Name);
  end;
  WriteLn('============================');
end;

// Debug: Print track events
class procedure TSedaiMIDIParser.PrintTrackEvents(const ATrack: TMIDITrack; AMaxEvents: Integer);
var
  i, ACount: Integer;
begin
  WriteLn('=== Track Events ===');
  ACount := ATrack.EventCount;
  if ACount > AMaxEvents then ACount := AMaxEvents;

  for i := 0 to ACount - 1 do
  begin
    with ATrack.Events[i] do
    begin
      WriteLn(Format('  %6d: %s Ch:%d Data:%d,%d',
        [AbsoluteTime, MIDIMessageTypeToString(MessageType), Channel, Data1, Data2]));
    end;
  end;

  if ATrack.EventCount > AMaxEvents then
    WriteLn('  ... and ', ATrack.EventCount - AMaxEvents, ' more events');
  WriteLn('==================');
end;

end.
