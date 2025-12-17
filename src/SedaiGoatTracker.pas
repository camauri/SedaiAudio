{*
 * Sedai Audio Foundation - GoatTracker .sng Parser and Player
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * Parses and plays GoatTracker v2 song files (.sng format)
 * Based on GoatTracker file format specification (GTS5)
 *
 * Format reference: ChiptuneSAK documentation
 * https://chiptunesak.readthedocs.io/en/latest/goattracker.html
 *
 * This program is dual-licensed:
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *}

unit SedaiGoatTracker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiSIDEvo;

const
  // GoatTracker signature
  GT_SIGNATURE = 'GTS5';

  // Maximum limits
  GT_MAX_INSTRUMENTS = 64;
  GT_MAX_PATTERNS = 208;    // $00-$CF are pattern numbers
  GT_MAX_PATTERN_LEN = 128;
  GT_MAX_TABLE_SIZE = 256;
  GT_MAX_SUBTUNES = 32;
  GT_MAX_ORDERLIST_LEN = 256;

  // Instrument size in bytes
  GT_INSTRUMENT_SIZE = 25;
  GT_INSTRUMENT_NAME_LEN = 16;

  // Pattern note values
  GT_NOTE_FIRST = $60;      // C-0
  GT_NOTE_LAST = $BC;       // G#7
  GT_NOTE_REST = $BD;       // Rest (no new note)
  GT_NOTE_KEYOFF = $BE;     // Key off (gate off)
  GT_NOTE_KEYON = $BF;      // Key on (gate on, no new note)
  GT_PATTERN_END = $FF;     // End of pattern marker

  // Orderlist values
  GT_ORD_PATTERN_MAX = $CF; // $00-$CF are pattern numbers
  GT_ORD_REPEAT_MIN = $D0;  // $D0-$DF are repeat commands
  GT_ORD_REPEAT_MAX = $DF;
  GT_ORD_TRANSPOSE_MIN = $E0; // $E0-$FE are transpose commands
  GT_ORD_TRANSPOSE_MAX = $FE;
  GT_ORD_RST = $FF;         // End mark with restart position

  // Waveform bits
  GT_WAVE_TRIANGLE = $10;
  GT_WAVE_SAWTOOTH = $20;
  GT_WAVE_PULSE = $40;
  GT_WAVE_NOISE = $80;
  GT_WAVE_GATE = $01;
  GT_WAVE_SYNC = $02;
  GT_WAVE_RING = $04;
  GT_WAVE_TEST = $08;

  // Table commands
  GT_TBL_END = $00;         // End of table
  GT_TBL_JUMP = $FF;        // Jump command

  // Wavetable special commands ($F0-$FE) - from gcommon.h
  GT_WAVECMD = $F0;         // Command marker (commands are $F0-$FE)
  GT_CMD_DONOTHING = 0;     // $F0 - Do nothing
  GT_CMD_PORTAUP = 1;       // $F1 - Portamento up using speedtable
  GT_CMD_PORTADOWN = 2;     // $F2 - Portamento down using speedtable
  GT_CMD_TONEPORTA = 3;     // $F3 - Tone portamento (slide to note)
  GT_CMD_VIBRATO = 4;       // $F4 - Vibrato using speedtable
  GT_CMD_SETAD = 5;         // $F5 - Set Attack/Decay
  GT_CMD_SETSR = 6;         // $F6 - Set Sustain/Release
  GT_CMD_SETWAVE = 7;       // $F7 - Set waveform
  GT_CMD_SETWAVEPTR = 8;    // $F8 - Set wave table pointer (not used in wavetable)
  GT_CMD_SETPULSEPTR = 9;   // $F9 - Set pulse table pointer
  GT_CMD_SETFILTERPTR = 10; // $FA - Set filter table pointer
  GT_CMD_SETFILTERCTRL = 11;// $FB - Set filter control
  GT_CMD_SETFILTERCUTOFF = 12; // $FC - Set filter cutoff
  GT_CMD_SETMASTERVOL = 13; // $FD - Set master volume
  GT_CMD_FUNKTEMPO = 14;    // $FE - Funk tempo (not used in wavetable)

  // Default tempo (frames per row)
  GT_DEFAULT_TEMPO = 6;

  // Frame rate
  GT_FRAMERATE_PAL = 50;
  GT_FRAMERATE_NTSC = 60;

type
  // GoatTracker Instrument (25 bytes)
  TGTInstrument = record
    AttackDecay: Byte;      // Offset +0: ADSR Attack/Decay
    SustainRelease: Byte;   // Offset +1: ADSR Sustain/Release
    WavePointer: Byte;      // Offset +2: Pointer to wavetable
    PulsePointer: Byte;     // Offset +3: Pointer to pulsetable
    FilterPointer: Byte;    // Offset +4: Pointer to filtertable
    VibratoSpeedPtr: Byte;  // Offset +5: Vibrato speedtable pointer
    VibratoDelay: Byte;     // Offset +6: Vibrato delay
    GateOffTimer: Byte;     // Offset +7: Hard restart timer
    HRFirstWave: Byte;      // Offset +8: Hard restart / 1st frame waveform
    Name: string[16];       // Offset +9-24: Instrument name (16 bytes)
  end;

  // GoatTracker Pattern Row (4 bytes)
  TGTPatternRow = record
    Note: Byte;             // Note ($60-$BC) or command ($BD-$BF, $FF)
    Instrument: Byte;       // Instrument number (0-63)
    Command: Byte;          // Effect command (0-$F)
    CommandData: Byte;      // Effect parameter
  end;

  // GoatTracker Pattern
  TGTPattern = record
    Length: Integer;
    Rows: array[0..GT_MAX_PATTERN_LEN-1] of TGTPatternRow;
  end;

  // Table entry (left + right column)
  TGTTableEntry = record
    Left: Byte;
    Right: Byte;
  end;

  // Table
  TGTTable = record
    Size: Integer;
    Entries: array[0..GT_MAX_TABLE_SIZE-1] of TGTTableEntry;
  end;

  // Orderlist for one channel
  TGTOrderlist = record
    Length: Integer;  // Not counting restart
    Data: array[0..GT_MAX_ORDERLIST_LEN-1] of Byte;
    RestartPos: Byte;
  end;

  // Channel state during playback
  TGTChannelState = record
    // Orderlist position
    OrderPos: Integer;
    PatternNum: Integer;
    PatternRow: Integer;
    Transpose: Integer;

    // Timing
    TickCounter: Integer;     // Counts down to next row

    // Instrument state
    CurrentInstrument: Integer;
    WaveTablePos: Integer;
    PulseTablePos: Integer;
    FilterTablePos: Integer;
    WaveTableDelay: Integer;
    PulseTableDelay: Integer;  // Not used anymore - using PulseModTime instead

    // Pulse modulation state (from gplay.c)
    CurrentPulse: Word;       // Current 12-bit pulse width value
    PulseModTime: Integer;    // Modulation duration counter

    // Voice state
    CurrentNote: Integer;     // Current note (0-95) - target note for portamento
    LastNote: Integer;        // Last played note (for frequency calculations)
    GateOn: Boolean;
    HardRestartTimer: Integer;

    // Vibrato state (from instrument)
    VibratoDelayCounter: Integer;
    VibratoPhase: Integer;
    VibratoSpeed: Integer;
    VibratoDepth: Integer;

    // Wavetable vibrato/command state (from gplay.c vibtime)
    VibTime: Byte;            // Vibrato time counter for CMD_VIBRATO

    // Current frequency register value
    FreqReg: Word;
  end;

  // Song info
  TGTSongInfo = record
    Title: string;
    Author: string;
    Copyright: string;
    SubtuneCount: Integer;
  end;

  { TSedaiGoatTracker }
  TSedaiGoatTracker = class
  private
    FSIDEvo: TSedaiSIDEvo;
    FOwnsEvo: Boolean;

    // Song data
    FInfo: TGTSongInfo;
    FInstruments: array[1..GT_MAX_INSTRUMENTS] of TGTInstrument;
    FInstrumentCount: Integer;
    FPatterns: array[0..GT_MAX_PATTERNS-1] of TGTPattern;
    FPatternCount: Integer;

    // Tables
    FWaveTable: TGTTable;
    FPulseTable: TGTTable;
    FFilterTable: TGTTable;
    FSpeedTable: TGTTable;

    // Orderlists per subtune, per channel
    FOrderLists: array[0..GT_MAX_SUBTUNES-1, 0..2] of TGTOrderlist;

    // Playback state
    FChannels: array[0..2] of TGTChannelState;
    FFrameRate: Integer;
    FTempo: Integer;          // Frames per row
    FIsPlaying: Boolean;
    FCurrentSubtune: Integer;
    FFrameCounter: Integer;
    FLooping: Boolean;        // If false, stop at end instead of looping
    FSongFinished: Boolean;   // True when song reached end (and not looping)
    FDebugJumpCount: Integer; // DEBUG: count wave jumps on channel 0
    FVerbose: Boolean;        // If true, print pattern commands as they execute
    FWaveVerbose: Boolean;    // If true, print wavetable commands as they execute

    // Voice muting
    FVoiceMuted: array[0..2] of Boolean;

    // Global filter state (filter is global, not per-channel)
    FFilterPtr: Integer;        // Position in filter table
    FFilterCutoff: Word;        // Current 11-bit cutoff value
    FFilterCtrl: Byte;          // Filter control (resonance + channel routing)
    FFilterType: Byte;          // Filter type (LP/BP/HP)
    FFilterModTime: Integer;    // Modulation duration counter

    // Parsing helpers
    function ReadString32(AStream: TStream): string;
    function ReadByte(AStream: TStream): Byte;

    // Playback helpers
    function NoteToFreqReg(ANote: Integer): Word;
    function FreqRegToHz(AFreqReg: Word): Single;
    procedure ProcessChannel(AChannel: Integer);
    procedure ProcessRow(AChannel: Integer);
    procedure ProcessWaveTable(AChannel: Integer);
    procedure ProcessPulseTable(AChannel: Integer);
    procedure ProcessFilterTable;  // Global filter - processed once per frame
    procedure ApplyInstrument(AChannel: Integer; AInstrNum: Integer);
    procedure TriggerNote(AChannel: Integer; ANote: Integer);
    procedure ProcessCommand(AChannel: Integer; ACmd, AData: Byte);
    procedure SetVoiceWaveform(AChannel: Integer; AWave: Byte);
    procedure AdvanceOrderlist(AChannel: Integer);
    procedure ProcessWaveCommand(AChannel: Integer; ACmd, AParam: Byte);
    function GetSpeedFromTable(AParam: Byte; ALastNote: Integer): Word;

  public
    constructor Create; overload;
    constructor Create(ASIDEvo: TSedaiSIDEvo); overload;
    destructor Destroy; override;

    // Loading
    function LoadFromFile(const AFilename: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;

    // Playback control
    procedure Play(ASubtune: Integer = 0);
    procedure Stop;
    procedure Pause;
    procedure Resume;

    // Called every frame (50/60 Hz)
    procedure Update;

    // Info
    property Info: TGTSongInfo read FInfo;
    property FrameRate: Integer read FFrameRate write FFrameRate;
    property IsPlaying: Boolean read FIsPlaying;
    property InstrumentCount: Integer read FInstrumentCount;
    property PatternCount: Integer read FPatternCount;
    property Tempo: Integer read FTempo write FTempo;

    // Looping control
    property Looping: Boolean read FLooping write FLooping;
    property SongFinished: Boolean read FSongFinished;

    // Verbose mode - prints commands as they execute
    property Verbose: Boolean read FVerbose write FVerbose;         // Pattern commands (V key)
    property WaveVerbose: Boolean read FWaveVerbose write FWaveVerbose;  // Wavetable commands (W key)

    // Voice muting
    procedure SetVoiceMute(AVoice: Integer; AMuted: Boolean);
    function IsVoiceMuted(AVoice: Integer): Boolean;

    // Debug
    procedure PrintSongInfo;
    procedure PrintInstrument(AIndex: Integer);
    procedure PrintPattern(AIndex: Integer);
    procedure PrintOrderlist(ASubtune, AChannel: Integer);
    procedure PrintWaveTable;
    procedure PrintSpeedTable;
    procedure PrintAllPatternsWithCommand(ACmd: Byte);  // Find patterns using specific command
    procedure PrintAllCommandsUsed;  // Summary of all commands used in song
  end;

implementation

const
  // Note frequency table (C-0 to B-7, 96 notes)
  // SID frequency register values for PAL clock (985248 Hz)
  NoteFreqTable: array[0..95] of Word = (
    // Octave 0
    $0117, $0127, $0139, $014B, $015F, $0174, $018A, $01A1, $01BA, $01D4, $01F0, $020E,
    // Octave 1
    $022D, $024E, $0271, $0296, $02BE, $02E7, $0314, $0342, $0374, $03A9, $03E0, $041B,
    // Octave 2
    $045A, $049C, $04E2, $052D, $057B, $05CF, $0627, $0685, $06E8, $0751, $07C1, $0837,
    // Octave 3
    $08B4, $0939, $09C5, $0A5A, $0AF7, $0B9D, $0C4E, $0D0A, $0DD0, $0EA3, $0F82, $106E,
    // Octave 4
    $1168, $1271, $138A, $14B3, $15EE, $173B, $189C, $1A13, $1BA1, $1D46, $1F04, $20DC,
    // Octave 5
    $22D0, $24E2, $2714, $2967, $2BDD, $2E76, $3139, $3426, $3742, $3A8D, $3E08, $41B8,
    // Octave 6
    $45A1, $49C5, $4E28, $52CD, $57BA, $5CEB, $6272, $684C, $6E84, $751A, $7C10, $8371,
    // Octave 7
    $8B42, $9389, $9C4F, $A59B, $AF74, $B9D6, $C4E3, $D099, $DD08, $EA33, $F820, $FFFF
  );

{ TSedaiGoatTracker }

constructor TSedaiGoatTracker.Create;
begin
  inherited Create;
  FSIDEvo := TSedaiSIDEvo.Create;
  FSIDEvo.Initialize(8);
  FSIDEvo.SetSIDMode;
  FOwnsEvo := True;

  FFrameRate := GT_FRAMERATE_PAL;
  FTempo := GT_DEFAULT_TEMPO;
  FIsPlaying := False;
  FCurrentSubtune := 0;
  FFrameCounter := 0;
  FLooping := False;        // Default: play once, don't loop
  FSongFinished := False;

  FillChar(FVoiceMuted, SizeOf(FVoiceMuted), 0);
end;

constructor TSedaiGoatTracker.Create(ASIDEvo: TSedaiSIDEvo);
begin
  inherited Create;
  FSIDEvo := ASIDEvo;
  FOwnsEvo := False;

  FFrameRate := GT_FRAMERATE_PAL;
  FTempo := GT_DEFAULT_TEMPO;
  FIsPlaying := False;
  FCurrentSubtune := 0;
  FFrameCounter := 0;
  FLooping := False;
  FSongFinished := False;

  FillChar(FVoiceMuted, SizeOf(FVoiceMuted), 0);
end;

destructor TSedaiGoatTracker.Destroy;
begin
  Stop;

  if FOwnsEvo and Assigned(FSIDEvo) then
  begin
    FSIDEvo.Shutdown;
    FreeAndNil(FSIDEvo);
  end;

  inherited Destroy;
end;

function TSedaiGoatTracker.ReadString32(AStream: TStream): string;
var
  ABuf: array[0..31] of Char;
  i: Integer;
begin
  FillChar(ABuf, 32, 0);
  AStream.Read(ABuf, 32);

  // Find null terminator
  Result := '';
  for i := 0 to 31 do
  begin
    if ABuf[i] = #0 then Break;
    Result := Result + ABuf[i];
  end;
  Result := Trim(Result);
end;

function TSedaiGoatTracker.ReadByte(AStream: TStream): Byte;
begin
  Result := 0;
  AStream.Read(Result, 1);
end;

function TSedaiGoatTracker.LoadFromFile(const AFilename: string): Boolean;
var
  AStream: TFileStream;
begin
  Result := False;

  if not FileExists(AFilename) then
  begin
    WriteLn('GoatTracker: File not found: ', AFilename);
    Exit;
  end;

  try
    AStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadFromStream(AStream);
    finally
      AStream.Free;
    end;
  except
    on E: Exception do
      WriteLn('GoatTracker: Error loading file: ', E.Message);
  end;
end;

function TSedaiGoatTracker.LoadFromStream(AStream: TStream): Boolean;
var
  ASig: array[0..3] of Char;
  ASubtuneCount: Byte;
  i, j, k: Integer;
  AOrderLen: Byte;
  APatternLen: Byte;
  ATableSize: Byte;
  ANameBuf: array[0..15] of Char;
begin
  Result := False;

  // Read and verify signature
  AStream.Read(ASig, 4);
  if ASig <> GT_SIGNATURE then
  begin
    WriteLn('GoatTracker: Invalid signature (expected GTS5, got ', ASig, ')');
    Exit;
  end;

  // Read header (3 x 32-byte strings)
  FInfo.Title := ReadString32(AStream);
  FInfo.Author := ReadString32(AStream);
  FInfo.Copyright := ReadString32(AStream);
  ASubtuneCount := ReadByte(AStream);
  FInfo.SubtuneCount := ASubtuneCount;

  // Read orderlists for each subtune and channel
  // Order: subtune0-ch0, subtune0-ch1, subtune0-ch2, subtune1-ch0, ...
  // Format from GoatTracker source (gsong.c):
  //   LEN byte + (LEN + 1) bytes of data
  //   The extra byte is the restart position after RST marker
  for i := 0 to ASubtuneCount - 1 do
  begin
    for j := 0 to 2 do  // 3 channels
    begin
      // Read orderlist length (entries count, NOT including restart pos byte)
      AOrderLen := ReadByte(AStream);
      FOrderLists[i, j].Length := AOrderLen;

      // Read (length + 1) bytes of orderlist data
      // The +1 is for the restart position byte after RST marker
      for k := 0 to AOrderLen do
        FOrderLists[i, j].Data[k] := ReadByte(AStream);

      // Find restart position (byte after RST marker $FF)
      FOrderLists[i, j].RestartPos := 0;
      for k := 0 to AOrderLen - 1 do
      begin
        if FOrderLists[i, j].Data[k] = GT_ORD_RST then
        begin
          // The restart position is stored after the RST marker
          FOrderLists[i, j].RestartPos := FOrderLists[i, j].Data[k + 1];
          // Update length to be the position of RST (actual playable entries)
          FOrderLists[i, j].Length := k;
          Break;
        end;
      end;
    end;
  end;

  // Read instruments
  FInstrumentCount := ReadByte(AStream);

  for i := 1 to FInstrumentCount do
  begin
    // Read 25 bytes per instrument
    FInstruments[i].AttackDecay := ReadByte(AStream);
    FInstruments[i].SustainRelease := ReadByte(AStream);
    FInstruments[i].WavePointer := ReadByte(AStream);
    FInstruments[i].PulsePointer := ReadByte(AStream);
    FInstruments[i].FilterPointer := ReadByte(AStream);
    FInstruments[i].VibratoSpeedPtr := ReadByte(AStream);
    FInstruments[i].VibratoDelay := ReadByte(AStream);
    FInstruments[i].GateOffTimer := ReadByte(AStream);
    FInstruments[i].HRFirstWave := ReadByte(AStream);

    // Read 16-byte instrument name
    FillChar(ANameBuf, 16, 0);
    AStream.Read(ANameBuf, 16);
    FInstruments[i].Name := '';
    for j := 0 to 15 do
    begin
      if ANameBuf[j] = #0 then Break;
      FInstruments[i].Name := FInstruments[i].Name + ANameBuf[j];
    end;
  end;

  // Read wavetable
  ATableSize := ReadByte(AStream);
  FWaveTable.Size := ATableSize;
  for i := 0 to ATableSize - 1 do
    FWaveTable.Entries[i].Left := ReadByte(AStream);
  for i := 0 to ATableSize - 1 do
    FWaveTable.Entries[i].Right := ReadByte(AStream);

  // Read pulsetable
  ATableSize := ReadByte(AStream);
  FPulseTable.Size := ATableSize;
  for i := 0 to ATableSize - 1 do
    FPulseTable.Entries[i].Left := ReadByte(AStream);
  for i := 0 to ATableSize - 1 do
    FPulseTable.Entries[i].Right := ReadByte(AStream);

  // Read filtertable
  ATableSize := ReadByte(AStream);
  FFilterTable.Size := ATableSize;
  for i := 0 to ATableSize - 1 do
    FFilterTable.Entries[i].Left := ReadByte(AStream);
  for i := 0 to ATableSize - 1 do
    FFilterTable.Entries[i].Right := ReadByte(AStream);

  // Read speedtable
  ATableSize := ReadByte(AStream);
  FSpeedTable.Size := ATableSize;
  for i := 0 to ATableSize - 1 do
    FSpeedTable.Entries[i].Left := ReadByte(AStream);
  for i := 0 to ATableSize - 1 do
    FSpeedTable.Entries[i].Right := ReadByte(AStream);

  // Read patterns
  FPatternCount := ReadByte(AStream);

  for i := 0 to FPatternCount - 1 do
  begin
    APatternLen := ReadByte(AStream);
    FPatterns[i].Length := APatternLen;

    for j := 0 to APatternLen - 1 do
    begin
      FPatterns[i].Rows[j].Note := ReadByte(AStream);
      FPatterns[i].Rows[j].Instrument := ReadByte(AStream);
      FPatterns[i].Rows[j].Command := ReadByte(AStream);
      FPatterns[i].Rows[j].CommandData := ReadByte(AStream);
    end;
  end;

  Result := True;
end;

function TSedaiGoatTracker.NoteToFreqReg(ANote: Integer): Word;
begin
  if ANote < 0 then ANote := 0;
  if ANote > 95 then ANote := 95;
  Result := NoteFreqTable[ANote];
end;

function TSedaiGoatTracker.FreqRegToHz(AFreqReg: Word): Single;
begin
  // Freq = FreqReg * PAL_Clock / 16777216
  Result := (AFreqReg * 985248.0) / 16777216.0;
end;

procedure TSedaiGoatTracker.SetVoiceWaveform(AChannel: Integer; AWave: Byte);
var
  ASIDWave: Byte;
begin
  ASIDWave := SIDEVO_WAVE_NONE;

  if (AWave and GT_WAVE_NOISE) <> 0 then
    ASIDWave := SIDEVO_WAVE_NOISE
  else
  begin
    if (AWave and GT_WAVE_PULSE) <> 0 then
      ASIDWave := ASIDWave or SIDEVO_WAVE_PULSE;
    if (AWave and GT_WAVE_SAWTOOTH) <> 0 then
      ASIDWave := ASIDWave or SIDEVO_WAVE_SAWTOOTH;
    if (AWave and GT_WAVE_TRIANGLE) <> 0 then
      ASIDWave := ASIDWave or SIDEVO_WAVE_TRIANGLE;
  end;

  // Default to pulse if nothing set
  if ASIDWave = SIDEVO_WAVE_NONE then
    ASIDWave := SIDEVO_WAVE_PULSE;

  FSIDEvo.SetWaveform(AChannel, ASIDWave);
end;

procedure TSedaiGoatTracker.ApplyInstrument(AChannel: Integer; AInstrNum: Integer);
var
  AInstr: TGTInstrument;
  AAttack, ADecay, ASustain, ARelease: Single;
begin
  if (AInstrNum < 1) or (AInstrNum > FInstrumentCount) then Exit;

  AInstr := FInstruments[AInstrNum];
  FChannels[AChannel].CurrentInstrument := AInstrNum;

  // Set ADSR
  AAttack := ((AInstr.AttackDecay shr 4) and $0F) / 15.0;
  ADecay := (AInstr.AttackDecay and $0F) / 15.0;
  ASustain := ((AInstr.SustainRelease shr 4) and $0F) / 15.0;
  ARelease := (AInstr.SustainRelease and $0F) / 15.0;

  FSIDEvo.SetAttack(AChannel, AAttack);
  FSIDEvo.SetDecay(AChannel, ADecay);
  FSIDEvo.SetSustain(AChannel, ASustain);
  FSIDEvo.SetRelease(AChannel, ARelease);

  // Initialize table pointers (1-based in file, convert to 0-based)
  // IMPORTANT: Reset all table state when applying instrument
  if AInstr.WavePointer > 0 then
    FChannels[AChannel].WaveTablePos := AInstr.WavePointer - 1
  else
    FChannels[AChannel].WaveTablePos := -1;

  if AInstr.PulsePointer > 0 then
  begin
    FChannels[AChannel].PulseTablePos := AInstr.PulsePointer - 1;
    // Reset pulse modulation state to start fresh
    FChannels[AChannel].CurrentPulse := $800;  // Default 50% pulse width
    FChannels[AChannel].PulseModTime := 0;
  end
  else
    FChannels[AChannel].PulseTablePos := -1;

  // Filter is global - only set if not already active
  // (first instrument to trigger wins)
  if (AInstr.FilterPointer > 0) and (FFilterPtr < 0) then
  begin
    FFilterPtr := AInstr.FilterPointer - 1;
    FFilterModTime := 0;
  end;

  // Vibrato
  FChannels[AChannel].VibratoDelayCounter := AInstr.VibratoDelay;
  if AInstr.VibratoSpeedPtr > 0 then
  begin
    // Get vibrato params from speedtable
    FChannels[AChannel].VibratoSpeed := FSpeedTable.Entries[AInstr.VibratoSpeedPtr - 1].Left;
    FChannels[AChannel].VibratoDepth := FSpeedTable.Entries[AInstr.VibratoSpeedPtr - 1].Right;
  end
  else
  begin
    FChannels[AChannel].VibratoSpeed := 0;
    FChannels[AChannel].VibratoDepth := 0;
  end;

  // Gate off timer / hard restart
  FChannels[AChannel].HardRestartTimer := AInstr.GateOffTimer;

  // First waveform
  if AInstr.HRFirstWave <> 0 then
    SetVoiceWaveform(AChannel, AInstr.HRFirstWave);

  // Reset delays
  FChannels[AChannel].WaveTableDelay := 0;
  FChannels[AChannel].PulseTableDelay := 0;
end;

procedure TSedaiGoatTracker.TriggerNote(AChannel: Integer; ANote: Integer);
var
  ANoteIndex: Integer;
  AFreqReg: Word;
begin
  // Convert GT note ($60 = C-0) to our note index (0 = C-0)
  ANoteIndex := ANote - GT_NOTE_FIRST;

  // Apply transpose
  ANoteIndex := ANoteIndex + FChannels[AChannel].Transpose;

  // Clamp
  if ANoteIndex < 0 then ANoteIndex := 0;
  if ANoteIndex > 95 then ANoteIndex := 95;

  FChannels[AChannel].CurrentNote := ANoteIndex;

  // Get frequency register value
  AFreqReg := NoteToFreqReg(ANoteIndex);
  FChannels[AChannel].FreqReg := AFreqReg;

  // Set frequency
  FSIDEvo.SetFrequencyHz(AChannel, FreqRegToHz(AFreqReg));

  // Gate on
  if not FVoiceMuted[AChannel] then
  begin
    FSIDEvo.GateOn(AChannel);
    FChannels[AChannel].GateOn := True;
  end;

  // Reset vibrato phase
  FChannels[AChannel].VibratoPhase := 0;

  // Also set LastNote for portamento calculations
  FChannels[AChannel].LastNote := ANoteIndex;
  FChannels[AChannel].VibTime := 0;
end;

// Get speed value from speedtable, handling relative mode
// If speedtable entry has high bit set ($80+), calculate relative to note interval
function TSedaiGoatTracker.GetSpeedFromTable(AParam: Byte; ALastNote: Integer): Word;
var
  ASpeedLeft, ASpeedRight: Byte;
  ANoteInterval: Word;
begin
  Result := 0;
  if AParam = 0 then Exit;
  if (AParam - 1) >= FSpeedTable.Size then Exit;

  ASpeedLeft := FSpeedTable.Entries[AParam - 1].Left;
  ASpeedRight := FSpeedTable.Entries[AParam - 1].Right;

  // Combine to 16-bit value: left = high byte, right = low byte
  Result := (Word(ASpeedLeft) shl 8) or ASpeedRight;

  // If high bit set ($8000+), use relative mode:
  // Calculate interval between current note and next note, then shift right
  if Result >= $8000 then
  begin
    // Clamp note
    if ALastNote < 0 then ALastNote := 0;
    if ALastNote >= 95 then ALastNote := 94;  // Need room for +1

    // Get frequency difference between adjacent notes
    ANoteInterval := NoteFreqTable[ALastNote + 1] - NoteFreqTable[ALastNote];

    // Shift right by the low byte of speedtable entry
    Result := ANoteInterval shr ASpeedRight;
  end;
end;

// Process wavetable commands ($F0-$FE)
procedure TSedaiGoatTracker.ProcessWaveCommand(AChannel: Integer; ACmd, AParam: Byte);
const
  CmdNames: array[0..14] of string = (
    'DONOTHING', 'PORTAUP', 'PORTADOWN', 'TONEPORTA', 'VIBRATO',
    'SETAD', 'SETSR', 'SETWAVE', 'SETWAVEPTR', 'SETPULSEPTR',
    'SETFILTERPTR', 'SETFILTERCTRL', 'SETFILTERCUTOFF', 'SETMASTERVOL', 'FUNKTEMPO'
  );
var
  ASpeed: Word;
  ATargetFreq: Word;
  ACmpValue: Byte;
  ANote: Integer;
begin
  // Verbose output for wavetable commands (W key toggle)
  if FWaveVerbose and (ACmd <= 14) then
    WriteLn('[CH', AChannel, '] WaveCmd: $F', IntToHex(ACmd, 1), ' ', CmdNames[ACmd], ' param=$', IntToHex(AParam, 2));

  case ACmd of
    GT_CMD_DONOTHING:
      ; // Do nothing

    GT_CMD_PORTAUP:  // $F1 - Portamento up
      begin
        ASpeed := GetSpeedFromTable(AParam, FChannels[AChannel].LastNote);
        // Increase frequency by speed, checking for overflow
        if FChannels[AChannel].FreqReg <= $FFFF - ASpeed then
          FChannels[AChannel].FreqReg := FChannels[AChannel].FreqReg + ASpeed
        else
          FChannels[AChannel].FreqReg := $FFFF;  // Clamp at max
        // Apply new frequency
        FSIDEvo.SetFrequencyHz(AChannel, FreqRegToHz(FChannels[AChannel].FreqReg));
      end;

    GT_CMD_PORTADOWN:  // $F2 - Portamento down
      begin
        ASpeed := GetSpeedFromTable(AParam, FChannels[AChannel].LastNote);
        // Decrease frequency by speed
        if FChannels[AChannel].FreqReg > ASpeed then
          FChannels[AChannel].FreqReg := FChannels[AChannel].FreqReg - ASpeed
        else
          FChannels[AChannel].FreqReg := 0;
        // Apply new frequency
        FSIDEvo.SetFrequencyHz(AChannel, FreqRegToHz(FChannels[AChannel].FreqReg));
      end;

    GT_CMD_TONEPORTA:  // $F3 - Tone portamento (slide to target note)
      begin
        // Target frequency from CurrentNote
        ANote := FChannels[AChannel].CurrentNote;
        if ANote < 0 then ANote := 0;
        if ANote > 95 then ANote := 95;
        ATargetFreq := NoteFreqTable[ANote];

        if AParam = 0 then
        begin
          // Instant portamento
          FChannels[AChannel].FreqReg := ATargetFreq;
          FChannels[AChannel].LastNote := ANote;
          FChannels[AChannel].VibTime := 0;
        end
        else
        begin
          ASpeed := GetSpeedFromTable(AParam, FChannels[AChannel].LastNote);

          // Slide towards target
          if FChannels[AChannel].FreqReg < ATargetFreq then
          begin
            FChannels[AChannel].FreqReg := FChannels[AChannel].FreqReg + ASpeed;
            if FChannels[AChannel].FreqReg > ATargetFreq then
            begin
              FChannels[AChannel].FreqReg := ATargetFreq;
              FChannels[AChannel].LastNote := ANote;
              FChannels[AChannel].VibTime := 0;
            end;
          end
          else if FChannels[AChannel].FreqReg > ATargetFreq then
          begin
            if FChannels[AChannel].FreqReg > ASpeed then
              FChannels[AChannel].FreqReg := FChannels[AChannel].FreqReg - ASpeed
            else
              FChannels[AChannel].FreqReg := 0;
            if FChannels[AChannel].FreqReg < ATargetFreq then
            begin
              FChannels[AChannel].FreqReg := ATargetFreq;
              FChannels[AChannel].LastNote := ANote;
              FChannels[AChannel].VibTime := 0;
            end;
          end;
        end;
        FSIDEvo.SetFrequencyHz(AChannel, FreqRegToHz(FChannels[AChannel].FreqReg));
      end;

    GT_CMD_VIBRATO:  // $F4 - Vibrato using speedtable
      begin
        ASpeed := 0;
        ACmpValue := 0;
        if (AParam > 0) and ((AParam - 1) < FSpeedTable.Size) then
        begin
          ACmpValue := FSpeedTable.Entries[AParam - 1].Left;
          ASpeed := FSpeedTable.Entries[AParam - 1].Right;
        end;

        // If high bit set, use relative mode
        if ACmpValue >= $80 then
        begin
          ACmpValue := ACmpValue and $7F;
          // Calculate relative speed from note interval
          ANote := FChannels[AChannel].LastNote;
          if ANote < 0 then ANote := 0;
          if ANote >= 95 then ANote := 94;
          ASpeed := (NoteFreqTable[ANote + 1] - NoteFreqTable[ANote]) shr ASpeed;
        end;

        // Vibrato logic from gplay.c:
        // VibTime oscillates, toggling direction when reaching cmpvalue
        if (FChannels[AChannel].VibTime < $80) and
           (FChannels[AChannel].VibTime > ACmpValue) then
          FChannels[AChannel].VibTime := FChannels[AChannel].VibTime xor $FF;

        FChannels[AChannel].VibTime := FChannels[AChannel].VibTime + 2;

        // Apply vibrato: odd = subtract, even = add
        if (FChannels[AChannel].VibTime and 1) <> 0 then
        begin
          if FChannels[AChannel].FreqReg > ASpeed then
            FChannels[AChannel].FreqReg := FChannels[AChannel].FreqReg - ASpeed
          else
            FChannels[AChannel].FreqReg := 0;
        end
        else
          FChannels[AChannel].FreqReg := FChannels[AChannel].FreqReg + ASpeed;

        FSIDEvo.SetFrequencyHz(AChannel, FreqRegToHz(FChannels[AChannel].FreqReg));
      end;

    GT_CMD_SETAD:  // $F5 - Set Attack/Decay
      begin
        FSIDEvo.SetAttack(AChannel, ((AParam shr 4) and $0F) / 15.0);
        FSIDEvo.SetDecay(AChannel, (AParam and $0F) / 15.0);
      end;

    GT_CMD_SETSR:  // $F6 - Set Sustain/Release
      begin
        FSIDEvo.SetSustain(AChannel, ((AParam shr 4) and $0F) / 15.0);
        FSIDEvo.SetRelease(AChannel, (AParam and $0F) / 15.0);
      end;

    GT_CMD_SETWAVE:  // $F7 - Set waveform directly
      SetVoiceWaveform(AChannel, AParam);

    GT_CMD_SETWAVEPTR:  // $F8 - Set wave table pointer (illegal in wavetable)
      ; // Not allowed from wavetable

    GT_CMD_SETPULSEPTR:  // $F9 - Set pulse table pointer
      begin
        if AParam > 0 then
        begin
          FChannels[AChannel].PulseTablePos := AParam - 1;
          FChannels[AChannel].PulseModTime := 0;
        end
        else
          FChannels[AChannel].PulseTablePos := -1;
      end;

    GT_CMD_SETFILTERPTR:  // $FA - Set filter table pointer
      begin
        if AParam > 0 then
        begin
          FFilterPtr := AParam - 1;
          FFilterModTime := 0;
        end
        else
          FFilterPtr := -1;
      end;

    GT_CMD_SETFILTERCTRL:  // $FB - Set filter control
      begin
        FFilterCtrl := AParam;
        if AParam = 0 then
          FFilterPtr := -1;  // Turn off filter

        // Apply routing and resonance
        if (AParam and $01) <> 0 then
        begin
          FSIDEvo.SetVoiceFilter(0, True);
          FSIDEvo.SetVoiceFilterResonance(0, ((AParam shr 4) and $0F) / 15.0);
        end
        else
          FSIDEvo.SetVoiceFilter(0, False);

        if (AParam and $02) <> 0 then
        begin
          FSIDEvo.SetVoiceFilter(1, True);
          FSIDEvo.SetVoiceFilterResonance(1, ((AParam shr 4) and $0F) / 15.0);
        end
        else
          FSIDEvo.SetVoiceFilter(1, False);

        if (AParam and $04) <> 0 then
        begin
          FSIDEvo.SetVoiceFilter(2, True);
          FSIDEvo.SetVoiceFilterResonance(2, ((AParam shr 4) and $0F) / 15.0);
        end
        else
          FSIDEvo.SetVoiceFilter(2, False);
      end;

    GT_CMD_SETFILTERCUTOFF:  // $FC - Set filter cutoff
      begin
        FFilterCutoff := Word(AParam) shl 3;  // Scale to 11-bit
        // Apply to routed channels
        if (FFilterCtrl and $01) <> 0 then
          FSIDEvo.SetVoiceFilterCutoff(0, FFilterCutoff / 2047.0);
        if (FFilterCtrl and $02) <> 0 then
          FSIDEvo.SetVoiceFilterCutoff(1, FFilterCutoff / 2047.0);
        if (FFilterCtrl and $04) <> 0 then
          FSIDEvo.SetVoiceFilterCutoff(2, FFilterCutoff / 2047.0);
      end;

    GT_CMD_SETMASTERVOL:  // $FD - Set master volume
      begin
        if AParam < 16 then
          FSIDEvo.MasterVolume := AParam / 15.0;
      end;

    GT_CMD_FUNKTEMPO:  // $FE - Funk tempo (not used in wavetable)
      ; // Not implemented
  end;
end;

procedure TSedaiGoatTracker.ProcessWaveTable(AChannel: Integer);
label
  ProcessEntry;
var
  APos: Integer;
  ALeft, ARight: Byte;
  ANewNote: Integer;
  AFreqReg: Word;
begin
ProcessEntry:
  APos := FChannels[AChannel].WaveTablePos;
  if (APos < 0) or (APos >= FWaveTable.Size) then Exit;

  ALeft := FWaveTable.Entries[APos].Left;
  ARight := FWaveTable.Entries[APos].Right;

  // Based on GoatTracker gplay.c wavetable execution:
  //
  // Left column (wave byte):
  // $00 = no waveform change (but may still have note offset in right)
  // $01-$0F = delay (wait n frames before advancing)
  // $10-$DF = waveform value (applied to SID waveform register)
  // $E0-$EF = silent waveform (waveform without gate)
  // $F0-$FE = special commands (portamento, vibrato, etc.)
  // $FF = jump to position in right column
  //
  // Right column (note byte):
  // $00-$7F = relative note (added to current note for frequency)
  // $80 = no frequency change
  // $81-$FF = absolute note ($80 subtracted to get note index)

  // Check for delay (values $01-$0F mean wait that many frames)
  if (ALeft >= $01) and (ALeft <= $0F) then
  begin
    // Delay: wavetime counter
    if FChannels[AChannel].WaveTableDelay < ALeft then
    begin
      Inc(FChannels[AChannel].WaveTableDelay);
      Exit;  // Don't advance pointer yet
    end;
    // Delay complete, reset and advance
    FChannels[AChannel].WaveTableDelay := 0;
    Inc(FChannels[AChannel].WaveTablePos);
    Exit;
  end;

  // Reset delay counter for non-delay entries
  FChannels[AChannel].WaveTableDelay := 0;

  // Jump command ($FF)
  if ALeft = $FF then
  begin
    if ARight > 0 then
    begin
      FChannels[AChannel].WaveTablePos := ARight - 1;  // 1-based to 0-based
      // Process the jump target immediately in the same frame
      goto ProcessEntry;
    end
    else
      FChannels[AChannel].WaveTablePos := -1;  // End
    Exit;
  end;

  // Special commands ($F0-$FE) - execute command from wavetable
  if (ALeft >= $F0) and (ALeft <= $FE) then
  begin
    // Execute the command: low nybble is command number, ARight is parameter
    ProcessWaveCommand(AChannel, ALeft and $0F, ARight);
    // Advance pointer after command execution
    Inc(FChannels[AChannel].WaveTablePos);
    // Skip note processing for commands (from gplay.c: goto PULSEEXEC)
    Exit;
  end;

  // Silent/inaudible waveform ($E0-$EF)
  if (ALeft >= $E0) and (ALeft <= $EF) then
  begin
    // Set waveform but without gate
    SetVoiceWaveform(AChannel, ALeft and $0F);
    // Don't trigger gate
  end
  // Normal waveform ($10-$DF)
  else if (ALeft >= $10) and (ALeft <= $DF) then
  begin
    // Apply waveform directly
    SetVoiceWaveform(AChannel, ALeft);

    // Handle gate bit
    if (ALeft and GT_WAVE_GATE) <> 0 then
    begin
      if not FVoiceMuted[AChannel] and not FChannels[AChannel].GateOn then
      begin
        FSIDEvo.GateOn(AChannel);
        FChannels[AChannel].GateOn := True;
      end;
    end
    else
    begin
      // No gate bit - release
      if FChannels[AChannel].GateOn then
      begin
        FSIDEvo.GateOff(AChannel);
        FChannels[AChannel].GateOn := False;
      end;
    end;
  end;
  // $00 = no waveform change, but still process note

  // Right column: frequency/note modification
  // $80 = no change, otherwise modify frequency
  if ARight <> $80 then
  begin
    if ARight < $80 then
    begin
      // Relative note offset: add to current note
      ANewNote := FChannels[AChannel].CurrentNote + ARight;
    end
    else
    begin
      // Absolute note: $81-$FF means note (ARight - $80)
      ANewNote := ARight - $80;
    end;

    // Clamp note
    if ANewNote < 0 then ANewNote := 0;
    if ANewNote > 95 then ANewNote := 95;

    // Update frequency
    AFreqReg := NoteToFreqReg(ANewNote);
    FChannels[AChannel].FreqReg := AFreqReg;
    FSIDEvo.SetFrequencyHz(AChannel, FreqRegToHz(AFreqReg));
    FSIDEvo.UpdateActiveVoice(AChannel);
  end;

  // Always advance pointer after processing an entry
  // The goto processes the jump target in the same frame, then we increment for next frame
  Inc(FChannels[AChannel].WaveTablePos);
end;

procedure TSedaiGoatTracker.ProcessPulseTable(AChannel: Integer);
label
  ProcessPulseEntry;
var
  APos: Integer;
  ALeft, ARight: Byte;
  ASpeed: Integer;
  ANewPulse: Integer;
begin
ProcessPulseEntry:
  APos := FChannels[AChannel].PulseTablePos;
  if (APos < 0) or (APos >= FPulseTable.Size) then Exit;

  // Based on GoatTracker gplay.c pulsetable execution:
  //
  // If PulseModTime > 0, we're in modulation mode:
  //   - Apply speed from right column each frame
  //   - Decrement PulseModTime
  //   - When PulseModTime reaches 0, advance pointer
  //
  // Otherwise read new entry:
  //   - Left >= $80: Set absolute pulse width, advance pointer
  //   - Left < $80: Set PulseModTime = Left (start modulation)
  //   - Left = $FF: Jump to position in right column

  // Process ongoing modulation
  if FChannels[AChannel].PulseModTime > 0 then
  begin
    // Get speed from right column
    ARight := FPulseTable.Entries[APos].Right;

    // Apply speed to current pulse
    // Speed < $80: add (increase pulse width)
    // Speed >= $80: subtract (decrease pulse width)
    if ARight < $80 then
    begin
      ANewPulse := Integer(FChannels[AChannel].CurrentPulse) + ARight;
    end
    else
    begin
      // Signed: ARight >= $80 means negative
      ASpeed := Integer(ARight) - 256;  // Convert to signed
      ANewPulse := Integer(FChannels[AChannel].CurrentPulse) + ASpeed;
    end;

    // Wrap to 12-bit range (0-4095)
    FChannels[AChannel].CurrentPulse := Word(ANewPulse and $FFF);

    // Apply to SIDEvo
    FSIDEvo.SetPulseWidth(AChannel, FChannels[AChannel].CurrentPulse / 4096.0);

    // Decrement modulation timer
    Dec(FChannels[AChannel].PulseModTime);

    // If modulation finished, advance pointer
    if FChannels[AChannel].PulseModTime <= 0 then
      Inc(FChannels[AChannel].PulseTablePos);

    Exit;
  end;

  // Read new entry
  ALeft := FPulseTable.Entries[APos].Left;
  ARight := FPulseTable.Entries[APos].Right;

  // Jump command ($FF)
  if ALeft = $FF then
  begin
    if ARight > 0 then
    begin
      FChannels[AChannel].PulseTablePos := ARight - 1;  // 1-based to 0-based
      // Process the jump target immediately in the same frame
      goto ProcessPulseEntry;
    end
    else
      FChannels[AChannel].PulseTablePos := -1;  // End
    Exit;
  end;

  // Set absolute pulse width (Left >= $80)
  if (ALeft and $80) <> 0 then
  begin
    // High nybble of left (lower 4 bits) + right byte = 12-bit value
    FChannels[AChannel].CurrentPulse := (Word(ALeft and $0F) shl 8) or ARight;
    FSIDEvo.SetPulseWidth(AChannel, FChannels[AChannel].CurrentPulse / 4096.0);
    Inc(FChannels[AChannel].PulseTablePos);
    Exit;
  end;

  // Start modulation (Left < $80)
  // Left = duration (number of frames)
  // Right = speed (applied each frame)
  if ALeft > 0 then
  begin
    FChannels[AChannel].PulseModTime := ALeft;
    // Don't advance pointer - we'll read speed from same entry during modulation
  end
  else
  begin
    // ALeft = 0: no modulation, advance
    Inc(FChannels[AChannel].PulseTablePos);
  end;
end;

procedure TSedaiGoatTracker.ProcessFilterTable;
label
  ProcessFilterEntry;
var
  ALeft, ARight: Byte;
  ASpeed: Integer;
  ANewCutoff: Integer;
  ACutoffNorm: Single;
  AFilterTypeFlag: Byte;
begin
ProcessFilterEntry:
  // Filter is inactive if pointer is negative
  if (FFilterPtr < 0) or (FFilterPtr >= FFilterTable.Size) then Exit;

  // Based on GoatTracker gplay.c filter processing:
  //
  // If FFilterModTime > 0, we're in modulation mode:
  //   - Apply speed from right column each frame
  //   - Decrement modulation time
  //   - When time reaches 0, advance pointer
  //
  // Otherwise read new entry:
  //   - Left >= $80: Set filter type and resonance
  //   - Left < $80: Set modulation duration
  //   - Left = $FF: Jump to position in right column
  //   - Left = $00: Stop filter execution

  // Process ongoing modulation
  if FFilterModTime > 0 then
  begin
    // Get speed from right column
    ARight := FFilterTable.Entries[FFilterPtr].Right;

    // Apply speed to cutoff
    // Speed < $80: add (increase cutoff)
    // Speed >= $80: subtract (decrease cutoff)
    if ARight < $80 then
      ANewCutoff := Integer(FFilterCutoff) + ARight
    else
    begin
      ASpeed := Integer(ARight) - 256;  // Convert to signed
      ANewCutoff := Integer(FFilterCutoff) + ASpeed;
    end;

    // Clamp to 11-bit range (0-2047)
    if ANewCutoff < 0 then ANewCutoff := 0;
    if ANewCutoff > 2047 then ANewCutoff := 2047;
    FFilterCutoff := Word(ANewCutoff);

    // Apply to SIDEvo - normalize 0-2047 to 0.0-1.0
    ACutoffNorm := FFilterCutoff / 2047.0;

    // Apply filter to all channels based on routing
    // For now, apply globally (SID filter affects all routed voices together)
    if (FFilterCtrl and $01) <> 0 then
      FSIDEvo.SetVoiceFilterCutoff(0, ACutoffNorm);
    if (FFilterCtrl and $02) <> 0 then
      FSIDEvo.SetVoiceFilterCutoff(1, ACutoffNorm);
    if (FFilterCtrl and $04) <> 0 then
      FSIDEvo.SetVoiceFilterCutoff(2, ACutoffNorm);

    Dec(FFilterModTime);
    if FFilterModTime <= 0 then
      Inc(FFilterPtr);

    Exit;
  end;

  // Read new entry
  ALeft := FFilterTable.Entries[FFilterPtr].Left;
  ARight := FFilterTable.Entries[FFilterPtr].Right;

  // Stop command ($00)
  if ALeft = $00 then
  begin
    FFilterPtr := -1;  // Stop filter execution
    Exit;
  end;

  // Jump command ($FF)
  if ALeft = $FF then
  begin
    if ARight > 0 then
    begin
      FFilterPtr := ARight - 1;  // 1-based to 0-based
      // Process the jump target immediately in the same frame
      goto ProcessFilterEntry;
    end
    else
      FFilterPtr := -1;  // End
    Exit;
  end;

  // Set filter type and resonance (Left >= $80)
  if (ALeft and $80) <> 0 then
  begin
    // Left bits:
    //   Bit 7: Always set (marker)
    //   Bits 4-6: Filter type ($10=LP, $20=BP, $40=HP)
    //   Bits 0-3: Channel routing
    // Right: Resonance (high nybble) + additional routing or cutoff

    FFilterType := ALeft and $70;  // Extract filter type bits
    FFilterCtrl := ARight;          // Resonance and control

    // Apply filter type to SIDEvo
    // Map SID filter types to SIDEvo
    AFilterTypeFlag := 0;
    if (FFilterType and $10) <> 0 then  // Lowpass
      AFilterTypeFlag := AFilterTypeFlag or SIDEVO_FILTER_LOWPASS;
    if (FFilterType and $20) <> 0 then  // Bandpass
      AFilterTypeFlag := AFilterTypeFlag or SIDEVO_FILTER_BANDPASS;
    if (FFilterType and $40) <> 0 then  // Highpass
      AFilterTypeFlag := AFilterTypeFlag or SIDEVO_FILTER_HIGHPASS;

    // Apply resonance (high nybble of control)
    ACutoffNorm := ((FFilterCtrl shr 4) and $0F) / 15.0;

    // Apply to routed channels
    if (ALeft and $01) <> 0 then
    begin
      FSIDEvo.SetVoiceFilter(0, True);
      FSIDEvo.SetVoiceFilterType(0, AFilterTypeFlag);
      FSIDEvo.SetVoiceFilterResonance(0, ACutoffNorm);
    end;
    if (ALeft and $02) <> 0 then
    begin
      FSIDEvo.SetVoiceFilter(1, True);
      FSIDEvo.SetVoiceFilterType(1, AFilterTypeFlag);
      FSIDEvo.SetVoiceFilterResonance(1, ACutoffNorm);
    end;
    if (ALeft and $04) <> 0 then
    begin
      FSIDEvo.SetVoiceFilter(2, True);
      FSIDEvo.SetVoiceFilterType(2, AFilterTypeFlag);
      FSIDEvo.SetVoiceFilterResonance(2, ACutoffNorm);
    end;

    Inc(FFilterPtr);
    Exit;
  end;

  // Set absolute cutoff or start modulation (Left < $80)
  // If bit 6 is set ($40-$7F), it's a set cutoff command
  // Otherwise it's modulation duration
  if (ALeft and $40) <> 0 then
  begin
    // Set absolute cutoff: Left bits 0-5 (low) + Right (high)
    FFilterCutoff := (Word(ALeft and $07) shl 8) or ARight;

    // Apply cutoff
    ACutoffNorm := FFilterCutoff / 2047.0;
    if (FFilterCtrl and $01) <> 0 then
      FSIDEvo.SetVoiceFilterCutoff(0, ACutoffNorm);
    if (FFilterCtrl and $02) <> 0 then
      FSIDEvo.SetVoiceFilterCutoff(1, ACutoffNorm);
    if (FFilterCtrl and $04) <> 0 then
      FSIDEvo.SetVoiceFilterCutoff(2, ACutoffNorm);

    Inc(FFilterPtr);
    Exit;
  end;

  // Start modulation (Left < $40, non-zero)
  if ALeft > 0 then
    FFilterModTime := ALeft
  else
    Inc(FFilterPtr);
end;

procedure TSedaiGoatTracker.ProcessCommand(AChannel: Integer; ACmd, AData: Byte);
const
  CmdNames: array[0..15] of string = (
    'none', 'PortaUp', 'PortaDn', 'TonePorta', 'Vibrato',
    'SetAD', 'SetSR', 'Waveform', 'WaveTblPtr', 'PulseTblPtr',
    'FilterPtr', 'FilterCtrl', 'FilterCut', 'MasterVol', 'FunkTempo', 'Tempo'
  );
begin
  // Verbose output for pattern commands (V key toggle)
  if FVerbose and (ACmd > 0) and (ACmd <= 15) then
    WriteLn('[CH', AChannel, '] PatCmd: $', IntToHex(ACmd, 1), ' ', CmdNames[ACmd], ' data=$', IntToHex(AData, 2));

  case ACmd of
    $00: ; // No command
    $01: ; // Portamento up
    $02: ; // Portamento down
    $03: ; // Tone portamento
    $04: ; // Vibrato
    $05: begin
           // Set Attack/Decay
           FSIDEvo.SetAttack(AChannel, ((AData shr 4) and $0F) / 15.0);
           FSIDEvo.SetDecay(AChannel, (AData and $0F) / 15.0);
         end;
    $06: begin
           // Set Sustain/Release
           FSIDEvo.SetSustain(AChannel, ((AData shr 4) and $0F) / 15.0);
           FSIDEvo.SetRelease(AChannel, (AData and $0F) / 15.0);
         end;
    $07: SetVoiceWaveform(AChannel, AData);  // Waveform
    $08: FChannels[AChannel].WaveTablePos := AData - 1;  // Wavetable pointer
    $09: FChannels[AChannel].PulseTablePos := AData - 1; // Pulsetable pointer
    $0A: begin
           // Filtertable pointer (global - not per-channel)
           FFilterPtr := AData - 1;
           FFilterModTime := 0;  // Reset modulation
         end;
    $0B: begin
           // Filter control: high nybble = resonance, low nybble = routing
           FFilterCtrl := AData;
           // Apply resonance to all routed channels
           if (AData and $01) <> 0 then
             FSIDEvo.SetVoiceFilterResonance(0, ((AData shr 4) and $0F) / 15.0);
           if (AData and $02) <> 0 then
             FSIDEvo.SetVoiceFilterResonance(1, ((AData shr 4) and $0F) / 15.0);
           if (AData and $04) <> 0 then
             FSIDEvo.SetVoiceFilterResonance(2, ((AData shr 4) and $0F) / 15.0);
         end;
    $0C: begin
           // Filter cutoff (direct set)
           FFilterCutoff := Word(AData) shl 3;  // Scale to 11-bit
           // Apply to routed channels
           if (FFilterCtrl and $01) <> 0 then
             FSIDEvo.SetVoiceFilterCutoff(0, FFilterCutoff / 2047.0);
           if (FFilterCtrl and $02) <> 0 then
             FSIDEvo.SetVoiceFilterCutoff(1, FFilterCutoff / 2047.0);
           if (FFilterCtrl and $04) <> 0 then
             FSIDEvo.SetVoiceFilterCutoff(2, FFilterCutoff / 2047.0);
         end;
    $0D: FSIDEvo.MasterVolume := AData / 15.0;  // Master volume
    $0E: ; // Funk tempo
    $0F: FTempo := AData;  // Tempo
  end;
end;

procedure TSedaiGoatTracker.AdvanceOrderlist(AChannel: Integer);
var
  AOrderlist: ^TGTOrderlist;
  AValue: Byte;
  ALoopCount: Integer;
  ALooped: Boolean;
begin
  AOrderlist := @FOrderLists[FCurrentSubtune, AChannel];
  ALooped := False;

  // Advance position
  Inc(FChannels[AChannel].OrderPos);

  // Safety counter to prevent infinite loops
  ALoopCount := 0;

  // Process orderlist entries until we find a pattern
  while ALoopCount < 256 do  // Max iterations to prevent infinite loop
  begin
    Inc(ALoopCount);

    // Check bounds - if past length, either loop or stop
    if FChannels[AChannel].OrderPos >= AOrderlist^.Length then
    begin
      // If not looping, mark song as finished and stop
      if not FLooping then
      begin
        FSongFinished := True;
        FChannels[AChannel].PatternNum := -1;
        // Stop playback on channel 0 reaching end (triggers stop for all)
        if AChannel = 0 then
          FIsPlaying := False;
        Exit;
      end;

      // Looping enabled - jump to restart position
      FChannels[AChannel].OrderPos := AOrderlist^.RestartPos;
      ALooped := True;
      // Safety: if restart is also past length, stop
      if FChannels[AChannel].OrderPos >= AOrderlist^.Length then
      begin
        FChannels[AChannel].PatternNum := -1;  // No valid pattern
        Exit;
      end;
    end;

    AValue := AOrderlist^.Data[FChannels[AChannel].OrderPos];

    if AValue <= GT_ORD_PATTERN_MAX then
    begin
      // Pattern number - use it
      FChannels[AChannel].PatternNum := AValue;
      FChannels[AChannel].PatternRow := 0;

      // If we looped, reset channel for clean restart
      if ALooped then
      begin
        // IMPORTANT: Stop current note to avoid "bleeding" into next loop
        if FChannels[AChannel].GateOn then
        begin
          FSIDEvo.GateOff(AChannel);
          FChannels[AChannel].GateOn := False;
        end;

        // Reset tick counter to process first row immediately (no delay!)
        FChannels[AChannel].TickCounter := 1;

        // Reset channel states for clean loop
        FChannels[AChannel].WaveTablePos := -1;
        FChannels[AChannel].PulseTablePos := -1;
        FChannels[AChannel].WaveTableDelay := 0;
        FChannels[AChannel].PulseTableDelay := 0;
        FChannels[AChannel].CurrentPulse := $800;
        FChannels[AChannel].PulseModTime := 0;
        FChannels[AChannel].CurrentNote := 0;
        FChannels[AChannel].LastNote := 0;
        FChannels[AChannel].VibTime := 0;
        FChannels[AChannel].CurrentInstrument := 0;

        // Reset global state only on channel 0 loop
        if AChannel = 0 then
        begin
          // Reset filter
          FFilterPtr := -1;
          FFilterCutoff := 0;
          FFilterCtrl := 0;
          FFilterType := 0;
          FFilterModTime := 0;

          // Reset master volume to full - this is crucial!
          // Without this, CMD_SETMASTERVOL fade effects would accumulate
          FSIDEvo.MasterVolume := 1.0;
        end;
      end;

      Exit;
    end
    else if (AValue >= GT_ORD_REPEAT_MIN) and (AValue <= GT_ORD_REPEAT_MAX) then
    begin
      // Repeat command - for now just skip
      Inc(FChannels[AChannel].OrderPos);
    end
    else if (AValue >= GT_ORD_TRANSPOSE_MIN) and (AValue <= GT_ORD_TRANSPOSE_MAX) then
    begin
      // Transpose command
      if AValue <= $EF then
        FChannels[AChannel].Transpose := AValue - $E0
      else
        FChannels[AChannel].Transpose := Integer(AValue) - $100;  // Negative
      Inc(FChannels[AChannel].OrderPos);
    end
    else if AValue = GT_ORD_RST then
    begin
      // Restart marker - if not looping, stop here
      if not FLooping then
      begin
        FSongFinished := True;
        FChannels[AChannel].PatternNum := -1;
        if AChannel = 0 then
          FIsPlaying := False;
        Exit;
      end;
      // Looping enabled - jump to restart position
      FChannels[AChannel].OrderPos := AOrderlist^.RestartPos;
      ALooped := True;
      // Continue loop to process the pattern at restart position
    end
    else
      Inc(FChannels[AChannel].OrderPos);
  end;

  // If we got here, something went wrong
  WriteLn('Warning: AdvanceOrderlist loop limit reached for channel ', AChannel);
  FChannels[AChannel].PatternNum := -1;
end;

procedure TSedaiGoatTracker.ProcessRow(AChannel: Integer);
var
  APattern: ^TGTPattern;
  ARow: TGTPatternRow;
  ANoteIndex: Integer;
begin
  // Check if we need next pattern
  if FChannels[AChannel].PatternNum < 0 then
  begin
    AdvanceOrderlist(AChannel);
  end;

  // Bounds check
  if (FChannels[AChannel].PatternNum < 0) or
     (FChannels[AChannel].PatternNum >= FPatternCount) then Exit;

  APattern := @FPatterns[FChannels[AChannel].PatternNum];

  // Check if we're past end of pattern
  if FChannels[AChannel].PatternRow >= APattern^.Length then
  begin
    AdvanceOrderlist(AChannel);
    if (FChannels[AChannel].PatternNum < 0) or
       (FChannels[AChannel].PatternNum >= FPatternCount) then Exit;
    APattern := @FPatterns[FChannels[AChannel].PatternNum];
  end;

  // Get current row
  ARow := APattern^.Rows[FChannels[AChannel].PatternRow];

  // Process note
  if (ARow.Note >= GT_NOTE_FIRST) and (ARow.Note <= GT_NOTE_LAST) then
  begin
    // New note
    if ARow.Instrument > 0 then
      ApplyInstrument(AChannel, ARow.Instrument);
    TriggerNote(AChannel, ARow.Note);
  end
  else if ARow.Note = GT_NOTE_KEYOFF then
  begin
    // Key off
    if not FVoiceMuted[AChannel] and FChannels[AChannel].GateOn then
    begin
      FSIDEvo.GateOff(AChannel);
      FChannels[AChannel].GateOn := False;
    end;
  end
  else if ARow.Note = GT_NOTE_KEYON then
  begin
    // Key on (retrigger without new note)
    if not FVoiceMuted[AChannel] and not FChannels[AChannel].GateOn then
    begin
      FSIDEvo.GateOn(AChannel);
      FChannels[AChannel].GateOn := True;
    end;
  end;
  // GT_NOTE_REST ($BD) = do nothing, hold current note

  // Process command
  if ARow.Command > 0 then
    ProcessCommand(AChannel, ARow.Command, ARow.CommandData);

  // Advance to next row
  Inc(FChannels[AChannel].PatternRow);

  // Reset tick counter
  FChannels[AChannel].TickCounter := FTempo;
end;

procedure TSedaiGoatTracker.ProcessChannel(AChannel: Integer);
begin
  // Decrement tick counter
  Dec(FChannels[AChannel].TickCounter);

  // Process new row if tick counter reached zero
  if FChannels[AChannel].TickCounter <= 0 then
    ProcessRow(AChannel);

  // Process tables every frame (per-channel tables)
  ProcessWaveTable(AChannel);
  ProcessPulseTable(AChannel);
  // Note: Filter is processed globally in Update, not per-channel
end;

procedure TSedaiGoatTracker.Play(ASubtune: Integer);
var
  i: Integer;
  AValue: Byte;
begin
  if ASubtune >= FInfo.SubtuneCount then
    ASubtune := 0;

  FCurrentSubtune := ASubtune;
  FIsPlaying := True;
  FFrameCounter := 0;
  FSongFinished := False;  // Reset finished flag
  FDebugJumpCount := 0;    // DEBUG: reset jump counter

  // Initialize channel states
  for i := 0 to 2 do
  begin
    FChannels[i].OrderPos := -1;  // Will be advanced to 0
    FChannels[i].PatternNum := -1;
    FChannels[i].PatternRow := 0;
    FChannels[i].Transpose := 0;
    FChannels[i].TickCounter := 1;  // Process first row immediately
    FChannels[i].CurrentInstrument := 0;
    FChannels[i].CurrentNote := 0;
    FChannels[i].LastNote := 0;        // For portamento calculations
    FChannels[i].GateOn := False;
    FChannels[i].WaveTablePos := -1;
    FChannels[i].PulseTablePos := -1;
    // FilterTablePos is deprecated - using global FFilterPtr instead
    FChannels[i].WaveTableDelay := 0;
    FChannels[i].PulseTableDelay := 0;
    FChannels[i].CurrentPulse := $800;  // Default 50% pulse width
    FChannels[i].PulseModTime := 0;
    FChannels[i].FreqReg := 0;
    FChannels[i].HardRestartTimer := 0;
    FChannels[i].VibratoDelayCounter := 0;
    FChannels[i].VibratoPhase := 0;
    FChannels[i].VibratoSpeed := 0;
    FChannels[i].VibratoDepth := 0;
    FChannels[i].VibTime := 0;         // For wavetable vibrato command

    // Find first pattern in orderlist
    AValue := FOrderLists[ASubtune, i].Data[0];
    if AValue <= GT_ORD_PATTERN_MAX then
    begin
      FChannels[i].OrderPos := 0;
      FChannels[i].PatternNum := AValue;
    end;
  end;

  // Initialize global filter state
  FFilterPtr := -1;       // No filter table active initially
  FFilterCutoff := 0;     // Filter off
  FFilterCtrl := 0;       // No resonance, no channels routed
  FFilterType := 0;       // No filter type
  FFilterModTime := 0;

  // Set default master volume
  FSIDEvo.MasterVolume := 1.0;
end;

procedure TSedaiGoatTracker.Stop;
var
  i: Integer;
begin
  FIsPlaying := False;

  for i := 0 to 2 do
  begin
    FSIDEvo.GateOff(i);
    FSIDEvo.StopVoice(i);
  end;
end;

procedure TSedaiGoatTracker.Pause;
var
  i: Integer;
begin
  FIsPlaying := False;
  // Silence all voices
  for i := 0 to 2 do
    FSIDEvo.GateOff(i);
end;

procedure TSedaiGoatTracker.Resume;
begin
  FIsPlaying := True;
end;

procedure TSedaiGoatTracker.Update;
var
  i: Integer;
begin
  if not FIsPlaying then Exit;

  Inc(FFrameCounter);

  // Process all 3 channels
  for i := 0 to 2 do
    ProcessChannel(i);

  // Process global filter table (once per frame)
  ProcessFilterTable;
end;

procedure TSedaiGoatTracker.SetVoiceMute(AVoice: Integer; AMuted: Boolean);
begin
  if (AVoice >= 0) and (AVoice <= 2) then
  begin
    FVoiceMuted[AVoice] := AMuted;
    if AMuted then
      FSIDEvo.GateOff(AVoice);
  end;
end;

function TSedaiGoatTracker.IsVoiceMuted(AVoice: Integer): Boolean;
begin
  if (AVoice >= 0) and (AVoice <= 2) then
    Result := FVoiceMuted[AVoice]
  else
    Result := False;
end;

procedure TSedaiGoatTracker.PrintSongInfo;
begin
  WriteLn('=== GoatTracker Song Info ===');
  WriteLn('Title: ', FInfo.Title);
  WriteLn('Author: ', FInfo.Author);
  WriteLn('Copyright: ', FInfo.Copyright);
  WriteLn('Subtunes: ', FInfo.SubtuneCount);
  WriteLn('Instruments: ', FInstrumentCount);
  WriteLn('Patterns: ', FPatternCount);
  WriteLn('WaveTable: ', FWaveTable.Size, ' rows');
  WriteLn('PulseTable: ', FPulseTable.Size, ' rows');
  WriteLn('FilterTable: ', FFilterTable.Size, ' rows');
  WriteLn('SpeedTable: ', FSpeedTable.Size, ' rows');
  WriteLn('Tempo: ', FTempo, ' frames/row');
end;

procedure TSedaiGoatTracker.PrintInstrument(AIndex: Integer);
var
  AInstr: TGTInstrument;
begin
  if (AIndex < 1) or (AIndex > FInstrumentCount) then
  begin
    WriteLn('Invalid instrument index: ', AIndex);
    Exit;
  end;

  AInstr := FInstruments[AIndex];
  WriteLn('=== Instrument ', AIndex, ': ', AInstr.Name, ' ===');
  WriteLn('  AD: $', IntToHex(AInstr.AttackDecay, 2),
          ' SR: $', IntToHex(AInstr.SustainRelease, 2));
  WriteLn('  Wave: ', AInstr.WavePointer,
          ' Pulse: ', AInstr.PulsePointer,
          ' Filter: ', AInstr.FilterPointer);
  WriteLn('  Vibrato: ', AInstr.VibratoSpeedPtr,
          ' Delay: ', AInstr.VibratoDelay);
  WriteLn('  GateOff: ', AInstr.GateOffTimer,
          ' HRFirstWave: $', IntToHex(AInstr.HRFirstWave, 2));
end;

procedure TSedaiGoatTracker.PrintPattern(AIndex: Integer);
var
  APattern: TGTPattern;
  i: Integer;
  ANoteStr: string;
  AOctave, ANote: Integer;
const
  NoteNames: array[0..11] of string = ('C-', 'C#', 'D-', 'D#', 'E-', 'F-',
                                       'F#', 'G-', 'G#', 'A-', 'A#', 'B-');
begin
  if (AIndex < 0) or (AIndex >= FPatternCount) then
  begin
    WriteLn('Invalid pattern index: ', AIndex);
    Exit;
  end;

  APattern := FPatterns[AIndex];
  WriteLn('=== Pattern ', AIndex, ' (', APattern.Length, ' rows) ===');

  for i := 0 to APattern.Length - 1 do
  begin
    with APattern.Rows[i] do
    begin
      // Decode note
      if (Note >= GT_NOTE_FIRST) and (Note <= GT_NOTE_LAST) then
      begin
        ANote := Note - GT_NOTE_FIRST;
        AOctave := ANote div 12;
        ANoteStr := NoteNames[ANote mod 12] + IntToStr(AOctave);
      end
      else if Note = GT_NOTE_REST then
        ANoteStr := '...'
      else if Note = GT_NOTE_KEYOFF then
        ANoteStr := 'OFF'
      else if Note = GT_NOTE_KEYON then
        ANoteStr := 'ON '
      else
        ANoteStr := '???';

      WriteLn(Format('  %2d: %s %2d %X%02X',
              [i, ANoteStr, Instrument, Command, CommandData]));
    end;
  end;
end;

procedure TSedaiGoatTracker.PrintOrderlist(ASubtune, AChannel: Integer);
var
  AOrderlist: TGTOrderlist;
  i: Integer;
  AValue: Byte;
begin
  if (ASubtune < 0) or (ASubtune >= FInfo.SubtuneCount) then
  begin
    WriteLn('Invalid subtune: ', ASubtune);
    Exit;
  end;
  if (AChannel < 0) or (AChannel > 2) then
  begin
    WriteLn('Invalid channel: ', AChannel);
    Exit;
  end;

  AOrderlist := FOrderLists[ASubtune, AChannel];
  WriteLn('=== Orderlist Subtune ', ASubtune, ' Channel ', AChannel + 1, ' ===');
  WriteLn('  Length: ', AOrderlist.Length, ' Restart: ', AOrderlist.RestartPos);

  for i := 0 to AOrderlist.Length - 1 do
  begin
    AValue := AOrderlist.Data[i];
    Write('  ', i:2, ': ');
    if AValue <= GT_ORD_PATTERN_MAX then
      WriteLn('Pattern ', AValue)
    else if (AValue >= GT_ORD_REPEAT_MIN) and (AValue <= GT_ORD_REPEAT_MAX) then
      WriteLn('Repeat ', AValue - GT_ORD_REPEAT_MIN + 1)
    else if (AValue >= GT_ORD_TRANSPOSE_MIN) and (AValue <= GT_ORD_TRANSPOSE_MAX) then
    begin
      if AValue <= $EF then
        WriteLn('Transpose +', AValue - $E0)
      else
        WriteLn('Transpose ', Integer(AValue) - $100);
    end
    else if AValue = GT_ORD_RST then
      WriteLn('RST -> ', AOrderlist.Data[i + 1])
    else
      WriteLn('$', IntToHex(AValue, 2));
  end;
end;

procedure TSedaiGoatTracker.PrintWaveTable;
var
  i: Integer;
  ALeft, ARight: Byte;
  ACmdName: string;
begin
  WriteLn('=== WaveTable (', FWaveTable.Size, ' entries) ===');
  WriteLn('Pos  Left  Right  Description');
  WriteLn('---  ----  -----  -----------');

  for i := 0 to FWaveTable.Size - 1 do
  begin
    ALeft := FWaveTable.Entries[i].Left;
    ARight := FWaveTable.Entries[i].Right;

    Write(i+1:3, '  $', IntToHex(ALeft, 2), '   $', IntToHex(ARight, 2), '   ');

    // Decode left column
    if ALeft = $FF then
      WriteLn('JUMP to ', ARight)
    else if ALeft = $00 then
      WriteLn('No wave change, note=$', IntToHex(ARight, 2))
    else if (ALeft >= $01) and (ALeft <= $0F) then
      WriteLn('DELAY ', ALeft, ' frames')
    else if (ALeft >= $F0) and (ALeft <= $FE) then
    begin
      case ALeft and $0F of
        0: ACmdName := 'DONOTHING';
        1: ACmdName := 'PORTAUP';
        2: ACmdName := 'PORTADOWN';
        3: ACmdName := 'TONEPORTA';
        4: ACmdName := 'VIBRATO';
        5: ACmdName := 'SETAD';
        6: ACmdName := 'SETSR';
        7: ACmdName := 'SETWAVE';
        8: ACmdName := 'SETWAVEPTR';
        9: ACmdName := 'SETPULSEPTR';
        10: ACmdName := 'SETFILTERPTR';
        11: ACmdName := 'SETFILTERCTRL';
        12: ACmdName := 'SETFILTERCUTOFF';
        13: ACmdName := 'SETMASTERVOL';
        14: ACmdName := 'FUNKTEMPO';
      else
        ACmdName := '???';
      end;
      WriteLn('CMD: ', ACmdName, ' param=$', IntToHex(ARight, 2));
    end
    else if (ALeft >= $E0) and (ALeft <= $EF) then
      WriteLn('SILENT wave $', IntToHex(ALeft and $0F, 1))
    else if (ALeft >= $10) and (ALeft <= $DF) then
      WriteLn('WAVE $', IntToHex(ALeft, 2), ' (gate=', (ALeft and 1), ')')
    else
      WriteLn('???');
  end;
end;

procedure TSedaiGoatTracker.PrintSpeedTable;
var
  i: Integer;
  ALeft, ARight: Byte;
  AValue: Word;
begin
  WriteLn('=== SpeedTable (', FSpeedTable.Size, ' entries) ===');
  WriteLn('Pos  Left  Right  Value   Mode');
  WriteLn('---  ----  -----  ------  ----');

  for i := 0 to FSpeedTable.Size - 1 do
  begin
    ALeft := FSpeedTable.Entries[i].Left;
    ARight := FSpeedTable.Entries[i].Right;
    AValue := (Word(ALeft) shl 8) or ARight;

    Write(i+1:3, '  $', IntToHex(ALeft, 2), '   $', IntToHex(ARight, 2), '   $', IntToHex(AValue, 4), '  ');

    if AValue >= $8000 then
      WriteLn('RELATIVE (shift by ', ARight, ')')
    else
      WriteLn('ABSOLUTE (', AValue, ')');
  end;
end;

procedure TSedaiGoatTracker.PrintAllPatternsWithCommand(ACmd: Byte);
var
  i, j: Integer;
  APattern: TGTPattern;
  ARow: TGTPatternRow;
  AFoundAny: Boolean;
begin
  WriteLn('=== Searching for command $', IntToHex(ACmd, 2), ' in all patterns ===');
  AFoundAny := False;

  for i := 0 to FPatternCount - 1 do
  begin
    APattern := FPatterns[i];
    for j := 0 to APattern.Length - 1 do
    begin
      ARow := APattern.Rows[j];
      if ARow.Command = ACmd then
      begin
        AFoundAny := True;
        WriteLn('Pattern ', i:3, ', Row ', j:3, ': Note=$', IntToHex(ARow.Note, 2),
                ' Instr=', ARow.Instrument:2, ' Cmd=$', IntToHex(ARow.Command, 1),
                ' Data=$', IntToHex(ARow.CommandData, 2));
      end;
    end;
  end;

  if not AFoundAny then
    WriteLn('No command $', IntToHex(ACmd, 2), ' found in any pattern.');
end;

procedure TSedaiGoatTracker.PrintAllCommandsUsed;
var
  i, j: Integer;
  APattern: TGTPattern;
  ARow: TGTPatternRow;
  ACmdCounts: array[0..15] of Integer;
  ACmdNames: array[0..15] of string;
begin
  WriteLn('=== Commands Used in All Patterns ===');

  // Initialize
  for i := 0 to 15 do
    ACmdCounts[i] := 0;

  ACmdNames[0] := '0 (none)';
  ACmdNames[1] := '1 (Portamento Up)';
  ACmdNames[2] := '2 (Portamento Down)';
  ACmdNames[3] := '3 (Tone Portamento)';
  ACmdNames[4] := '4 (Vibrato)';
  ACmdNames[5] := '5 (AD)';
  ACmdNames[6] := '6 (SR)';
  ACmdNames[7] := '7 (Waveform)';
  ACmdNames[8] := '8 (Wavetable Ptr)';
  ACmdNames[9] := '9 (Pulsetable Ptr)';
  ACmdNames[$A] := 'A (Filtertable Ptr)';
  ACmdNames[$B] := 'B (Filter Ctrl)';
  ACmdNames[$C] := 'C (Filter Cutoff)';
  ACmdNames[$D] := 'D (Master Volume)';
  ACmdNames[$E] := 'E (Funk Tempo)';
  ACmdNames[$F] := 'F (Tempo/Global)';

  // Count commands
  for i := 0 to FPatternCount - 1 do
  begin
    APattern := FPatterns[i];
    for j := 0 to APattern.Length - 1 do
    begin
      ARow := APattern.Rows[j];
      if ARow.Command <= 15 then
        Inc(ACmdCounts[ARow.Command]);
    end;
  end;

  // Print results
  for i := 0 to 15 do
  begin
    if ACmdCounts[i] > 0 then
      WriteLn('  $', IntToHex(i, 1), ' ', ACmdNames[i], ': ', ACmdCounts[i], ' uses');
  end;
end;

end.
