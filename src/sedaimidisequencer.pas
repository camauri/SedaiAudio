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

unit SedaiMIDISequencer;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, SedaiMIDITypes, SedaiAudioFoundation, SedaiAudioTypes;

type
  TSedaiMIDISequencer = class
  private
    FSequencer: TMIDISequencer;
    FIsInitialized: Boolean;

    // Internal methods
    procedure ProcessMIDIEvent(const AEvent: TMIDIEvent);
    procedure HandleNoteOn(AChannel, ANote, AVelocity: Byte);
    procedure HandleNoteOff(AChannel, ANote: Byte);
    procedure HandleProgramChange(AChannel, AProgramNum: Byte);
    procedure HandleController(AChannel, AController, AValue: Byte);
    function FindFreeNoteSlot: Integer;
    function FindActiveNote(AChannel, ANote: Byte): Integer;

  public
    constructor Create;
    destructor Destroy; override;

    // File operations
    function LoadMIDIFile(const AFilename: string): Boolean;
    procedure UnloadMIDIFile;

    // Playback control
    procedure Play;
    procedure Stop;
    procedure Pause;
    procedure SetPosition(ATick: Cardinal);

    // Update - call this regularly from your audio callback
    procedure Update(ADeltaTimeSeconds: Single);

    // Status
    function GetPlaybackState: TMIDIPlaybackState;
    function GetCurrentTick: Cardinal;
    function GetTotalTicks: Cardinal;
    function GetProgressPercent: Single;
    function GetActiveNoteCount: Integer;

    // Channel configuration
    procedure SetChannelWavetable(AChannel: Integer; const AWavetableType: string);
    procedure SetChannelVolume(AChannel: Integer; AVolume: Single);
    procedure SetChannelMute(AChannel: Integer; AMuted: Boolean);

    // Information
    function GetMIDIFileInfo: string;
    function GetChannelInfo(AChannel: Integer): string;
    procedure PrintSequencerStatus;

    // Tempo control
    procedure SetTempo(ATempoPercent: Single); // 100% = normal, 50% = half speed, etc.
    function GetCurrentTempo: Cardinal; // Microseconds per quarter note
  end;

implementation

uses
  SedaiMIDIParser;

function MIDINoteToNoteName(ANoteNumber: Byte): string;
var
  ANoteNames: array[0..11] of string;
  ANoteName: string;
  AOctave: Integer;
begin
  // Initialize note names array
  ANoteNames[0] := 'C';   ANoteNames[1] := 'C#';  ANoteNames[2] := 'D';
  ANoteNames[3] := 'D#';  ANoteNames[4] := 'E';   ANoteNames[5] := 'F';
  ANoteNames[6] := 'F#';  ANoteNames[7] := 'G';   ANoteNames[8] := 'G#';
  ANoteNames[9] := 'A';   ANoteNames[10] := 'A#'; ANoteNames[11] := 'B';

  ANoteName := ANoteNames[ANoteNumber mod 12];
  AOctave := (ANoteNumber div 12) - 1;
  Result := ANoteName + IntToStr(AOctave);
end;

constructor TSedaiMIDISequencer.Create;
var
  i: Integer;
begin
  inherited Create;

  FillChar(FSequencer, SizeOf(TMIDISequencer), 0);
  FSequencer.PlaybackState := mpsStop;
  FSequencer.CurrentTempo := 500000; // 120 BPM default

  // Initialize channel states
  for i := 0 to 15 do
  begin
    with FSequencer.ChannelStates[i] do
    begin
      ProgramNumber := 0;    // Piano
      Volume := 100;         // Full volume
      Pan := 64;             // Center
      PitchBend := 0;        // No bend
      Modulation := 0;       // No modulation
      IsMuted := False;

      // Default wavetable assignments
      case i of
        9: WavetableType := 'ppg';      // Drum channel gets digital
        0..7: WavetableType := 'serum'; // Standard channels get Serum
      else
        WavetableType := 'wasp';        // Others get Wasp
      end;
    end;
  end;

  // Initialize note tracking
  for i := 0 to 127 do
  begin
    FSequencer.ActiveNotes[i].IsActive := False;
    FSequencer.ActiveNotes[i].VoiceIndex := -1;
  end;

  FIsInitialized := True;
  WriteLn('SEDAI MIDI Sequencer initialized');
end;

destructor TSedaiMIDISequencer.Destroy;
begin
  Stop;
  UnloadMIDIFile;
  inherited Destroy;
end;

// Load MIDI file
function TSedaiMIDISequencer.LoadMIDIFile(const AFilename: string): Boolean;
begin
  Result := False;

  if not FIsInitialized then
  begin
    WriteLn('ERROR: Sequencer not initialized');
    Exit;
  end;

  // Stop current playback
  Stop;

  WriteLn('Loading MIDI file into sequencer: ', ExtractFileName(AFilename));

  // Load the MIDI file
  FSequencer.MIDIFile := TSedaiMIDIParser.LoadMIDIFile(AFilename);
  Result := FSequencer.MIDIFile.IsLoaded;

  if Result then
  begin
    // Initialize playback state
    FSequencer.CurrentTick := 0;
    SetLength(FSequencer.TrackPositions, FSequencer.MIDIFile.Header.TrackCount);
    FillChar(FSequencer.TrackPositions[0], Length(FSequencer.TrackPositions) * SizeOf(Integer), 0);

    // FIXED: Correct calculation of ticks per second
    // Default: 120 BPM = 500000 microseconds per quarter note
    // TicksPerSecond = (1000000 / microseconds_per_quarter) * ticks_per_quarter
    FSequencer.TicksPerSecond := (1000000.0 / FSequencer.CurrentTempo) * FSequencer.MIDIFile.Header.Division;

    WriteLn('MIDI file loaded successfully!');
    WriteLn('  Total ticks: ', FSequencer.MIDIFile.TotalTicks);
    WriteLn('  Division: ', FSequencer.MIDIFile.Header.Division, ' ticks/quarter');
    WriteLn('  Tempo: ', FSequencer.CurrentTempo, ' μs/quarter (', Round(60000000 / FSequencer.CurrentTempo), ' BPM)');
    WriteLn('  Ticks per second: ', FSequencer.TicksPerSecond:0:1);
    WriteLn('  Estimated duration: ', (FSequencer.MIDIFile.TotalTicks / FSequencer.TicksPerSecond):0:1, ' seconds');
  end
  else
    WriteLn('ERROR: Failed to load MIDI file');
end;

procedure TSedaiMIDISequencer.UnloadMIDIFile;
var
  i: Integer;
begin
  Stop;

  // Release all active notes
  for i := 0 to 127 do
  begin
    if FSequencer.ActiveNotes[i].IsActive then
    begin
      NoteOff(FSequencer.ActiveNotes[i].VoiceIndex);
      FSequencer.ActiveNotes[i].IsActive := False;
    end;
  end;

  // Clear MIDI data
  FillChar(FSequencer.MIDIFile, SizeOf(TMIDIFile), 0);
  SetLength(FSequencer.TrackPositions, 0);

  WriteLn('MIDI file unloaded');
end;

// Playback control
procedure TSedaiMIDISequencer.Play;
begin
  if not FSequencer.MIDIFile.IsLoaded then
  begin
    WriteLn('ERROR: No MIDI file loaded');
    Exit;
  end;

  if FSequencer.PlaybackState = mpsPause then
  begin
    WriteLn('Resuming MIDI playback');
  end
  else
  begin
    WriteLn('Starting MIDI playback');
    FSequencer.CurrentTick := 0;
    FillChar(FSequencer.TrackPositions[0], Length(FSequencer.TrackPositions) * SizeOf(Integer), 0);
  end;

  FSequencer.PlaybackState := mpsPlay;
  FSequencer.LastUpdateTime := GetTickCount64;
  FSequencer.AccumulatedTime := 0.0;
end;

procedure TSedaiMIDISequencer.Stop;
var
  i: Integer;
begin
  if FSequencer.PlaybackState = mpsStop then Exit;

  WriteLn('Stopping MIDI playback');
  FSequencer.PlaybackState := mpsStop;
  FSequencer.CurrentTick := 0;

  // Stop all playing notes
  for i := 0 to 127 do
  begin
    if FSequencer.ActiveNotes[i].IsActive then
    begin
      NoteOff(FSequencer.ActiveNotes[i].VoiceIndex);
      FSequencer.ActiveNotes[i].IsActive := False;
    end;
  end;

  // Reset track positions
  if Length(FSequencer.TrackPositions) > 0 then
    FillChar(FSequencer.TrackPositions[0], Length(FSequencer.TrackPositions) * SizeOf(Integer), 0);
end;

procedure TSedaiMIDISequencer.Pause;
begin
  if FSequencer.PlaybackState = mpsPlay then
  begin
    WriteLn('Pausing MIDI playback');
    FSequencer.PlaybackState := mpsPause;
  end;
end;

procedure TSedaiMIDISequencer.SetPosition(ATick: Cardinal);
var
  i, j: Integer;
begin
  FSequencer.CurrentTick := ATick;

  // Find appropriate track positions for this tick
  for i := 0 to Length(FSequencer.TrackPositions) - 1 do
  begin
    FSequencer.TrackPositions[i] := 0;
    for j := 0 to FSequencer.MIDIFile.Tracks[i].EventCount - 1 do
    begin
      if FSequencer.MIDIFile.Tracks[i].Events[j].AbsoluteTime <= ATick then
        FSequencer.TrackPositions[i] := j + 1
      else
        Break;
    end;
  end;

  WriteLn('Set MIDI position to tick: ', ATick);
end;

// Main update function - call this regularly
procedure TSedaiMIDISequencer.Update(ADeltaTimeSeconds: Single);
var
  ATicksToAdvance: Double;
  ANewTick: Cardinal;
  i, j: Integer;
  AEvent: TMIDIEvent;
  AProcessedEvents: Integer;
  AActiveCount: Integer;
begin
  if (FSequencer.PlaybackState <> mpsPlay) or (not FSequencer.MIDIFile.IsLoaded) then
    Exit;

  // FIXED: Use ONLY the existing parameter, but limit it
  ATicksToAdvance := ADeltaTimeSeconds * FSequencer.TicksPerSecond;

  // Limit advancement to avoid jumps
  if ATicksToAdvance > (FSequencer.TicksPerSecond * 0.05) then  // Max 50ms
    ATicksToAdvance := FSequencer.TicksPerSecond * 0.05;

  // Only advance if we have at least 0.5 tick
  if ATicksToAdvance >= 0.5 then
  begin
    ANewTick := FSequencer.CurrentTick + Round(ATicksToAdvance);

    // End of file check
    if ANewTick >= FSequencer.MIDIFile.TotalTicks then
    begin
      WriteLn('MIDI playback finished - reached end of file');
      FSequencer.PlaybackState := mpsStop;
      Exit;
    end;

    // DEBUG: Less frequent progress updates
    if (ANewTick mod (FSequencer.MIDIFile.Header.Division * 2)) = 0 then
      WriteLn('MIDI: Tick ', ANewTick, '/', FSequencer.MIDIFile.TotalTicks,
              ' (', (ANewTick * 100.0 / FSequencer.MIDIFile.TotalTicks):0:1, '%)');

    // Process events
    AProcessedEvents := 0;
    for i := 0 to Length(FSequencer.TrackPositions) - 1 do
    begin
      j := FSequencer.TrackPositions[i];

      while (j < FSequencer.MIDIFile.Tracks[i].EventCount) and (AProcessedEvents < 20) do
      begin
        AEvent := FSequencer.MIDIFile.Tracks[i].Events[j];

        if AEvent.AbsoluteTime <= ANewTick then
        begin
          ProcessMIDIEvent(AEvent);
          Inc(FSequencer.TrackPositions[i]);
          Inc(AProcessedEvents);
          Inc(j);
        end
        else
          Break;
      end;
    end;

    FSequencer.CurrentTick := ANewTick;

    // Debug active notes every beat
    if (FSequencer.CurrentTick mod FSequencer.MIDIFile.Header.Division) = 0 then
    begin
      AActiveCount := GetActiveNoteCount;
      if AActiveCount > 0 then
        WriteLn(Format('Active notes at tick %d: %d notes playing', [FSequencer.CurrentTick, AActiveCount]));
    end;

    if AProcessedEvents > 8 then
      WriteLn('Processed ', AProcessedEvents, ' events at tick ', FSequencer.CurrentTick);
  end;
end;

// Process individual MIDI event
procedure TSedaiMIDISequencer.ProcessMIDIEvent(const AEvent: TMIDIEvent);
begin
  case AEvent.MessageType of
    mtNoteOn:
      begin
        if AEvent.Data2 > 0 then // Velocity > 0
          HandleNoteOn(AEvent.Channel, AEvent.Data1, AEvent.Data2)
        else
          HandleNoteOff(AEvent.Channel, AEvent.Data1); // Velocity 0 = note off
      end;

    mtNoteOff:
      HandleNoteOff(AEvent.Channel, AEvent.Data1);

    mtProgram:
      HandleProgramChange(AEvent.Channel, AEvent.Data1);

    mtController:
      HandleController(AEvent.Channel, AEvent.Data1, AEvent.Data2);

    mtPitchBend:
      begin
        // Pitch bend: combine Data1 and Data2 into 14-bit value
        FSequencer.ChannelStates[AEvent.Channel].PitchBend :=
          (AEvent.Data2 shl 7) or AEvent.Data1 - 8192; // Convert to signed
      end;
  end;
end;

// Handle note on
procedure TSedaiMIDISequencer.HandleNoteOn(AChannel, ANote, AVelocity: Byte);
var
  ANoteSlot: Integer;
  AVoiceIndex: Integer;
  AFrequency: Single;
  AAmplitude: Single;
  AWavetableType: string;
  APitchBendMultiplier: Single;
begin
  if FSequencer.ChannelStates[AChannel].IsMuted then Exit;

  ANoteSlot := FindFreeNoteSlot;
  if ANoteSlot < 0 then
  begin
    WriteLn('WARNING: No free note slots available');
    Exit;
  end;

  // Calculate base frequency
  AFrequency := MIDINoteToFrequency(ANote);

  // ENHANCEMENT: Apply pitch bend at note-on time
  APitchBendMultiplier := MIDIPitchBendToFrequencyMultiplier(
    FSequencer.ChannelStates[AChannel].PitchBend, 2.0); // 2 semitone range
  AFrequency := AFrequency * APitchBendMultiplier;

  // ENHANCEMENT: Velocity-sensitive amplitude with better curve
  AAmplitude := (MIDIVelocityToAmplitude(AVelocity) * 0.7) *
                (FSequencer.ChannelStates[AChannel].Volume / 127.0);

  AWavetableType := FSequencer.ChannelStates[AChannel].WavetableType;
  AVoiceIndex := PlayWavetableAdv(AFrequency, AWavetableType);

  if AVoiceIndex >= 0 then
  begin
    SetVoicePan(AVoiceIndex, MIDIPanToSedaiPan(FSequencer.ChannelStates[AChannel].Pan));

    with FSequencer.ActiveNotes[ANoteSlot] do
    begin
      IsActive := True;
      Channel := AChannel;
      Note := ANote;
      Velocity := AVelocity;
      VoiceIndex := AVoiceIndex;
      StartTime := FSequencer.CurrentTick;
    end;

    // Log NOTE ON - every note for debug
    WriteLn(Format('NOTE ON:  Ch:%d Note:%d (%s) Vel:%d Voice:%d Tick:%d',
      [AChannel, ANote, MIDINoteToNoteName(ANote), AVelocity, AVoiceIndex, FSequencer.CurrentTick]));
  end;
end;

// FIX HandleNoteOff with debug:
procedure TSedaiMIDISequencer.HandleNoteOff(AChannel, ANote: Byte);
var
  ANoteIndex: Integer;
  ANoteDuration: Cardinal;
begin
  ANoteIndex := FindActiveNote(AChannel, ANote);
  if ANoteIndex >= 0 then
  begin
    ANoteDuration := FSequencer.CurrentTick - FSequencer.ActiveNotes[ANoteIndex].StartTime;

    NoteOff(FSequencer.ActiveNotes[ANoteIndex].VoiceIndex);
    FSequencer.ActiveNotes[ANoteIndex].IsActive := False;

    // Log NOTE OFF - every note for debug
    WriteLn(Format('NOTE OFF: Ch:%d Note:%d (%s) Voice:%d Duration:%d ticks (%.2fs)',
      [AChannel, ANote, MIDINoteToNoteName(ANote),
       FSequencer.ActiveNotes[ANoteIndex].VoiceIndex,
       ANoteDuration, (ANoteDuration / FSequencer.TicksPerSecond)]));
  end
  else
  begin
    WriteLn(Format('WARNING: NOTE OFF without NOTE ON - Ch:%d Note:%d (%s)',
      [AChannel, ANote, MIDINoteToNoteName(ANote)]));
  end;
end;

// Handle program change
procedure TSedaiMIDISequencer.HandleProgramChange(AChannel, AProgramNum: Byte);
begin
  FSequencer.ChannelStates[AChannel].ProgramNumber := AProgramNum;

  // Map MIDI programs to wavetable types
  case AProgramNum of
    0..7:   FSequencer.ChannelStates[AChannel].WavetableType := 'serum';  // Piano family
    8..15:  FSequencer.ChannelStates[AChannel].WavetableType := 'wasp';   // Chromatic
    16..23: FSequencer.ChannelStates[AChannel].WavetableType := 'ppg';    // Organ
    24..31: FSequencer.ChannelStates[AChannel].WavetableType := 'serum';  // Guitar
    32..39: FSequencer.ChannelStates[AChannel].WavetableType := 'wasp';   // Bass
    40..47: FSequencer.ChannelStates[AChannel].WavetableType := 'serum';  // Strings
    48..55: FSequencer.ChannelStates[AChannel].WavetableType := 'wasp';   // Ensemble
    56..63: FSequencer.ChannelStates[AChannel].WavetableType := 'ppg';    // Brass
    64..71: FSequencer.ChannelStates[AChannel].WavetableType := 'serum';  // Reed
    72..79: FSequencer.ChannelStates[AChannel].WavetableType := 'ppg';    // Pipe
    80..87: FSequencer.ChannelStates[AChannel].WavetableType := 'wasp';   // Synth Lead
    88..95: FSequencer.ChannelStates[AChannel].WavetableType := 'serum';  // Synth Pad
    96..103: FSequencer.ChannelStates[AChannel].WavetableType := 'ppg';   // Synth Effects
    104..111: FSequencer.ChannelStates[AChannel].WavetableType := 'wasp'; // Ethnic
    112..119: FSequencer.ChannelStates[AChannel].WavetableType := 'ppg';  // Percussive
    120..127: FSequencer.ChannelStates[AChannel].WavetableType := 'serum';// Sound effects
  end;

  WriteLn(Format('Program Change: Ch:%d Program:%d -> %s',
    [AChannel, AProgramNum, FSequencer.ChannelStates[AChannel].WavetableType]));
end;

// Handle controller change
procedure TSedaiMIDISequencer.HandleController(AChannel, AController, AValue: Byte);
begin
  case AController of
    1:  begin
          // Modulation wheel (CC1) - stored for future use
          FSequencer.ChannelStates[AChannel].Modulation := AValue;
          // Could be mapped to vibrato depth, filter modulation, etc.
        end;
    7:  begin
          // Main volume (CC7)
          FSequencer.ChannelStates[AChannel].Volume := AValue;
        end;
    10: begin
          // Pan (CC10)
          FSequencer.ChannelStates[AChannel].Pan := AValue;
        end;
    11: begin
          // Expression (CC11) - similar to volume but for dynamics
          // Map to volume for now
          FSequencer.ChannelStates[AChannel].Volume :=
            Round((FSequencer.ChannelStates[AChannel].Volume / 127.0) * AValue);
        end;
    64: begin
          // Sustain pedal (CC64)
          // Values 0-63 = off, 64-127 = on
          // NOTE: Full sustain pedal support would require holding notes
          // Currently just logged for future implementation
        end;
    // Add more controllers as needed
  end;
end;

// Find free note slot
function TSedaiMIDISequencer.FindFreeNoteSlot: Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to 127 do
  begin
    if not FSequencer.ActiveNotes[i].IsActive then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

// Find active note
function TSedaiMIDISequencer.FindActiveNote(AChannel, ANote: Byte): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to 127 do
  begin
    with FSequencer.ActiveNotes[i] do
    begin
      if IsActive and (Channel = AChannel) and (Note = ANote) then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
end;

// Status functions
function TSedaiMIDISequencer.GetPlaybackState: TMIDIPlaybackState;
begin
  Result := FSequencer.PlaybackState;
end;

function TSedaiMIDISequencer.GetCurrentTick: Cardinal;
begin
  Result := FSequencer.CurrentTick;
end;

function TSedaiMIDISequencer.GetTotalTicks: Cardinal;
begin
  if FSequencer.MIDIFile.IsLoaded then
    Result := FSequencer.MIDIFile.TotalTicks
  else
    Result := 0;
end;

function TSedaiMIDISequencer.GetProgressPercent: Single;
begin
  if FSequencer.MIDIFile.TotalTicks > 0 then
    Result := (FSequencer.CurrentTick / FSequencer.MIDIFile.TotalTicks) * 100.0
  else
    Result := 0.0;
end;

function TSedaiMIDISequencer.GetActiveNoteCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to 127 do
    if FSequencer.ActiveNotes[i].IsActive then
      Inc(Result);
end;

// Channel configuration
procedure TSedaiMIDISequencer.SetChannelWavetable(AChannel: Integer; const AWavetableType: string);
begin
  if (AChannel >= 0) and (AChannel <= 15) then
  begin
    FSequencer.ChannelStates[AChannel].WavetableType := AWavetableType;
    WriteLn('Channel ', AChannel, ' wavetable set to: ', AWavetableType);
  end;
end;

procedure TSedaiMIDISequencer.SetChannelVolume(AChannel: Integer; AVolume: Single);
begin
  if (AChannel >= 0) and (AChannel <= 15) then
  begin
    FSequencer.ChannelStates[AChannel].Volume := Round(AVolume * 127);
    if FSequencer.ChannelStates[AChannel].Volume > 127 then
      FSequencer.ChannelStates[AChannel].Volume := 127;
  end;
end;

procedure TSedaiMIDISequencer.SetChannelMute(AChannel: Integer; AMuted: Boolean);
begin
  if (AChannel >= 0) and (AChannel <= 15) then
  begin
    FSequencer.ChannelStates[AChannel].IsMuted := AMuted;
    WriteLn('Channel ', AChannel, ' muted: ', BoolToStr(AMuted, True));
  end;
end;

// Information
function TSedaiMIDISequencer.GetMIDIFileInfo: string;
begin
  if FSequencer.MIDIFile.IsLoaded then
  begin
    Result := Format('MIDI: %s, Format %d, %d tracks, %d/%d ticks (%.1f%%)',
      [ExtractFileName(FSequencer.MIDIFile.Filename),
       FSequencer.MIDIFile.Header.Format,
       FSequencer.MIDIFile.Header.TrackCount,
       FSequencer.CurrentTick,
       FSequencer.MIDIFile.TotalTicks,
       GetProgressPercent]);
  end
  else
    Result := 'No MIDI file loaded';
end;

function TSedaiMIDISequencer.GetChannelInfo(AChannel: Integer): string;
begin
  if (AChannel >= 0) and (AChannel <= 15) then
  begin
    with FSequencer.ChannelStates[AChannel] do
    begin
      Result := Format('Ch %d: Prog:%d Vol:%d Pan:%d WT:%s Mute:%s',
        [AChannel, ProgramNumber, Volume, Pan, WavetableType, BoolToStr(IsMuted, True)]);
    end;
  end
  else
    Result := 'Invalid channel';
end;

procedure TSedaiMIDISequencer.PrintSequencerStatus;
var
  i: Integer;
  AStateStr: string;
begin
  WriteLn('=== MIDI Sequencer Status ===');

  case FSequencer.PlaybackState of
    mpsStop: AStateStr := 'Stopped';
    mpsPlay: AStateStr := 'Playing';
    mpsPause: AStateStr := 'Paused';
  end;

  WriteLn('State: ', AStateStr);
  WriteLn(GetMIDIFileInfo);
  WriteLn('Active Notes: ', GetActiveNoteCount);
  WriteLn('Tempo: ', FSequencer.CurrentTempo, ' μs/quarter (',
          Round(60000000 / FSequencer.CurrentTempo), ' BPM)');
  WriteLn('Ticks/Second: ', FSequencer.TicksPerSecond:0:1);
  WriteLn('');

  WriteLn('Channel States:');
  for i := 0 to 15 do
    WriteLn('  ', GetChannelInfo(i));

  WriteLn('============================');
end;

// Tempo control
procedure TSedaiMIDISequencer.SetTempo(ATempoPercent: Single);
begin
  if ATempoPercent <= 0.0 then ATempoPercent := 1.0;

  // Adjust current tempo by percentage
  FSequencer.CurrentTempo := Round(500000 / ATempoPercent); // Base: 120 BPM

  // Recalculate ticks per second
  if FSequencer.MIDIFile.IsLoaded then
    FSequencer.TicksPerSecond := (1000000.0 / FSequencer.CurrentTempo) * FSequencer.MIDIFile.Header.Division;

  WriteLn('Tempo set to ', ATempoPercent * 100:0:0, '% (',
          Round(60000000 / FSequencer.CurrentTempo), ' BPM)');
end;

function TSedaiMIDISequencer.GetCurrentTempo: Cardinal;
begin
  Result := FSequencer.CurrentTempo;
end;

end.
