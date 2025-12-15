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

unit SedaiMIDITypes;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math;

type
  // MIDI message types
  TMIDIMessageType = (
    mtNoteOff     = $80,
    mtNoteOn      = $90,
    mtAftertouch  = $A0,
    mtController  = $B0,
    mtProgram     = $C0,
    mtPressure    = $D0,
    mtPitchBend   = $E0,
    mtSystem      = $F0
  );

  // MIDI event structure
  TMIDIEvent = record
    DeltaTime: Cardinal;      // Ticks since last event
    AbsoluteTime: Cardinal;   // Absolute time in ticks
    MessageType: TMIDIMessageType;
    Channel: Byte;            // 0-15
    Data1: Byte;              // Note number, controller, etc.
    Data2: Byte;              // Velocity, value, etc.
    IsRunningStatus: Boolean;
  end;

  // MIDI track
  TMIDITrack = record
    Name: string;
    Events: array of TMIDIEvent;
    EventCount: Integer;
    Length: Cardinal;         // Track length in bytes
  end;

  // MIDI file header
  TMIDIHeader = record
    Format: Word;             // 0, 1, or 2
    TrackCount: Word;         // Number of tracks
    Division: Word;           // Time division (ticks per quarter note)
  end;

  // Complete MIDI file
  TMIDIFile = record
    Header: TMIDIHeader;
    Tracks: array of TMIDITrack;
    IsLoaded: Boolean;
    Filename: string;
    TotalTicks: Cardinal;
    TempoMap: array of record
      Tick: Cardinal;
      MicrosecondsPerQuarter: Cardinal;
    end;
  end;

  // MIDI playback state
  TMIDIPlaybackState = (mpsStop, mpsPlay, mpsPause);

  // Note tracking for polyphony
  TMIDINote = record
    IsActive: Boolean;
    Channel: Byte;
    Note: Byte;
    Velocity: Byte;
    VoiceIndex: Integer;      // SEDAI voice index
    StartTime: Cardinal;
  end;

  // MIDI channel state
  TMIDIChannelState = record
    ProgramNumber: Byte;      // Current instrument (0-127)
    Volume: Byte;             // Channel volume (0-127)
    Pan: Byte;                // Pan position (0=left, 64=center, 127=right)
    PitchBend: SmallInt;      // Pitch bend (-8192 to +8191)
    Modulation: Byte;         // Modulation wheel (0-127)
    IsMuted: Boolean;
    WavetableType: string;    // SEDAI wavetable type for this channel
  end;

  // MIDI sequencer state
  TMIDISequencer = record
    MIDIFile: TMIDIFile;
    PlaybackState: TMIDIPlaybackState;
    CurrentTick: Cardinal;
    TicksPerSecond: Single;   // Calculated from tempo and division
    CurrentTempo: Cardinal;   // Microseconds per quarter note

    // Playback tracking
    TrackPositions: array of Integer; // Current event index per track
    ActiveNotes: array[0..127] of TMIDINote; // Max 128 polyphonic notes
    ChannelStates: array[0..15] of TMIDIChannelState; // 16 MIDI channels

    // Timing
    LastUpdateTime: QWord;    // System time of last update
    AccumulatedTime: Single;  // Accumulated time for tick calculation
  end;

// Helper functions
function CreateMIDIEvent(ADeltaTime: Cardinal; AMsgType: TMIDIMessageType;
                        AChannel, AData1, AData2: Byte): TMIDIEvent;
function MIDIMessageTypeToString(AMsgType: TMIDIMessageType): string;
function MIDINoteToFrequency(ANoteNumber: Byte): Single;
function FrequencyToMIDINote(AFrequency: Single): Byte;
function MIDIVelocityToAmplitude(AVelocity: Byte): Single;
function MIDIPanToSedaiPan(APan: Byte): Single; // Convert 0-127 to -1.0 to 1.0
function MIDIPitchBendToFrequencyMultiplier(APitchBend: SmallInt; ABendRangeSemitones: Single = 2.0): Single;
function MIDIModulationToDepth(AModulation: Byte): Single;
function MIDIVelocityToFilterCutoff(AVelocity: Byte): Single;

implementation

function CreateMIDIEvent(ADeltaTime: Cardinal; AMsgType: TMIDIMessageType;
                        AChannel, AData1, AData2: Byte): TMIDIEvent;
begin
  Result.DeltaTime := ADeltaTime;
  Result.AbsoluteTime := 0; // Will be calculated during parsing
  Result.MessageType := AMsgType;
  Result.Channel := AChannel;
  Result.Data1 := AData1;
  Result.Data2 := AData2;
  Result.IsRunningStatus := False;
end;

function MIDIMessageTypeToString(AMsgType: TMIDIMessageType): string;
begin
  case AMsgType of
    mtNoteOff: Result := 'Note Off';
    mtNoteOn: Result := 'Note On';
    mtAftertouch: Result := 'Aftertouch';
    mtController: Result := 'Controller';
    mtProgram: Result := 'Program Change';
    mtPressure: Result := 'Channel Pressure';
    mtPitchBend: Result := 'Pitch Bend';
    mtSystem: Result := 'System';
  else
    Result := 'Unknown';
  end;
end;

// Convert MIDI note number to frequency (A4 = 440 Hz)
function MIDINoteToFrequency(ANoteNumber: Byte): Single;
begin
  // MIDI note 69 = A4 = 440 Hz
  Result := 440.0 * Power(2.0, (ANoteNumber - 69) / 12.0);
end;

// Convert frequency to MIDI note number
function FrequencyToMIDINote(AFrequency: Single): Byte;
var
  ANoteFloat: Single;
begin
  ANoteFloat := 69.0 + 12.0 * Log2(AFrequency / 440.0);
  Result := Round(ANoteFloat);
  if Result > 127 then Result := 127;
end;

// Convert MIDI velocity (0-127) to amplitude (0.0-1.0)
function MIDIVelocityToAmplitude(AVelocity: Byte): Single;
begin
  // Use velocity curve that feels more natural
  Result := Power(AVelocity / 127.0, 0.7);
end;

// Convert MIDI pan (0-127) to SEDAI pan (-1.0 to 1.0)
function MIDIPanToSedaiPan(APan: Byte): Single;
begin
  Result := (APan - 64) / 64.0;
  if Result < -1.0 then Result := -1.0;
  if Result > 1.0 then Result := 1.0;
end;

// Convert MIDI pitch bend (-8192 to +8191) to frequency multiplier
// ABendRangeSemitones: typically 2.0 semitones (whole tone) but can be adjusted
function MIDIPitchBendToFrequencyMultiplier(APitchBend: SmallInt; ABendRangeSemitones: Single): Single;
var
  ABendNormalized: Single;
  ASemitones: Single;
begin
  // Normalize pitch bend to -1.0 to +1.0
  ABendNormalized := APitchBend / 8192.0;

  // Convert to semitones
  ASemitones := ABendNormalized * ABendRangeSemitones;

  // Convert semitones to frequency multiplier
  // Frequency ratio = 2^(semitones/12)
  Result := Power(2.0, ASemitones / 12.0);
end;

// Convert MIDI modulation wheel (0-127) to depth (0.0-1.0)
function MIDIModulationToDepth(AModulation: Byte): Single;
begin
  Result := AModulation / 127.0;
end;

// Convert MIDI velocity to filter cutoff (velocity-sensitive brightness)
function MIDIVelocityToFilterCutoff(AVelocity: Byte): Single;
begin
  // Map velocity to filter cutoff: 0.3 to 1.0
  // Softer notes are darker, harder notes are brighter
  Result := 0.3 + (AVelocity / 127.0) * 0.7;
end;

end.
