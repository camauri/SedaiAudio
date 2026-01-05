{*
 * Sedai Audio Foundation - GoatTracker v2 Player
 *
 * This is a clean reimplementation following the EXACT architecture of
 * the original GoatTracker source code (gplay.c, gsid.cpp, gsound.c).
 *
 * Key design principles from original:
 * - playroutine() updates sidreg[] array (shadow registers)
 * - sid_fillbuffer() writes registers to SID with SIDWRITEDELAY timing
 * - Audio callback calls sid_fillbuffer() which generates samples
 * - Player timing is managed by snd_bpmcount in the mixer
 *
 * Reference: GoatTracker 2 source code
 * (c) 2024-2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiGoatTracker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiSIDEvo;

const
  // ============================================================================
  // CONSTANTS FROM gcommon.h
  // ============================================================================
  MAX_STR = 32;
  MAX_INSTR = 64;
  MAX_CHN = 3;
  MAX_PATT = 208;
  MAX_TABLES = 4;
  MAX_TABLELEN = 255;
  MAX_INSTRNAMELEN = 16;
  MAX_PATTROWS = 128;
  MAX_SONGLEN = 254;
  MAX_SONGS = 32;
  MAX_NOTES = 96;

  // Commands
  CMD_DONOTHING = 0;
  CMD_PORTAUP = 1;
  CMD_PORTADOWN = 2;
  CMD_TONEPORTA = 3;
  CMD_VIBRATO = 4;
  CMD_SETAD = 5;
  CMD_SETSR = 6;
  CMD_SETWAVE = 7;
  CMD_SETWAVEPTR = 8;
  CMD_SETPULSEPTR = 9;
  CMD_SETFILTERPTR = 10;
  CMD_SETFILTERCTRL = 11;
  CMD_SETFILTERCUTOFF = 12;
  CMD_SETMASTERVOL = 13;
  CMD_FUNKTEMPO = 14;
  CMD_SETTEMPO = 15;

  // Table indices
  WTBL = 0;
  PTBL = 1;
  FTBL = 2;
  STBL = 3;

  // Song markers
  REPEAT_MARK = $D0;
  TRANSDOWN = $E0;
  TRANSUP = $F0;
  LOOPSONG = $FF;

  // Pattern markers
  ENDPATT = $FF;
  FIRSTNOTE = $60;
  LASTNOTE = $BC;
  REST = $BD;
  KEYOFF = $BE;
  KEYON = $BF;

  // Wavetable values
  WAVELASTDELAY = $0F;
  WAVESILENT = $E0;
  WAVELASTSILENT = $EF;
  WAVECMD = $F0;
  WAVELASTCMD = $FE;

  // From gsid.h
  NUMSIDREGS = $19;
  SIDWRITEDELAY = 14;

  // Timing
  PALFRAMERATE = 50;
  NTSCFRAMERATE = 60;
  PALCLOCKRATE = 985248;
  NTSCCLOCKRATE = 1022727;

  // Play modes
  PLAY_STOPPED = $80;
  PLAY_STOP = $81;
  PLAY_BEGINNING = 1;
  PLAY_PATTERN = 2;
  PLAY_POS = 3;

type
  // ============================================================================
  // TYPES FROM gcommon.h
  // ============================================================================

  TGTInstrument = record
    AD: Byte;
    SR: Byte;
    Ptr: array[0..MAX_TABLES-1] of Byte;
    VibDelay: Byte;
    GateTimer: Byte;
    FirstWave: Byte;
    Name: array[0..MAX_INSTRNAMELEN-1] of Char;
  end;

  // Channel structure from gplay.c
  TGTChannel = record
    Trans: Integer;
    Instr: Integer;
    Note: Integer;
    NewNote: Integer;
    LastNote: Integer;
    Wave: Byte;
    Gate: Byte;
    Tick: Integer;
    Tempo: Integer;
    GateTimer: Integer;
    Freq: Word;
    Pulse: Word;
    Ptr: array[0..MAX_TABLES-1] of Integer;
    WaveTime: Integer;
    PulseTime: Integer;
    VibTime: Integer;
    VibDelay: Integer;
    Command: Byte;
    CmdData: Byte;
    NewCommand: Byte;
    NewCmdData: Byte;
    SongPtr: Integer;
    PattNum: Integer;
    PattPtr: Integer;
    Repeat_: Integer;
    Advance: Boolean;
    Mute: Boolean;
  end;

  { TSedaiGoatTracker }
  TSedaiGoatTracker = class
  private
    // SID chip
    FSID: TSedaiSIDEvo;
    FOwnsChip: Boolean;

    // Song data (from gsong.c)
    FInstr: array[0..MAX_INSTR-1] of TGTInstrument;
    FLTable: array[0..MAX_TABLES-1, 0..MAX_TABLELEN-1] of Byte;
    FRTable: array[0..MAX_TABLES-1, 0..MAX_TABLELEN-1] of Byte;
    FSongOrder: array[0..MAX_SONGS-1, 0..MAX_CHN-1, 0..MAX_SONGLEN+1] of Byte;
    FPattern: array[0..MAX_PATT-1, 0..MAX_PATTROWS*4+3] of Byte;
    FSongName: array[0..MAX_STR-1] of Char;
    FAuthorName: array[0..MAX_STR-1] of Char;
    FCopyrightName: array[0..MAX_STR-1] of Char;
    FPattLen: array[0..MAX_PATT-1] of Integer;
    FSongLen: array[0..MAX_SONGS-1, 0..MAX_CHN-1] of Integer;

    // Playback state (from gplay.c)
    FChn: array[0..MAX_CHN-1] of TGTChannel;
    FFilterCtrl: Byte;
    FFilterType: Byte;
    FFilterCutoff: Byte;
    FFilterTime: Byte;
    FFilterPtr: Byte;
    FFunkTable: array[0..1] of Byte;
    FMasterFader: Byte;
    FPsNum: Integer;
    FSongInit: Integer;
    FLastSongInit: Integer;
    FStartPattPos: Integer;

    // SID register shadow buffer (from gsid.c)
    FSidReg: array[0..NUMSIDREGS-1] of Byte;

    // Timing
    FFrameRate: Integer;
    FClockRate: Integer;
    FSampleRate: Integer;
    FMultiplier: Integer;

    // State
    FLoaded: Boolean;
    FLooping: Boolean;
    FSongFinished: Boolean;
    FFrameCounter: Int64;
    FVerbose: Boolean;
    FWaveVerbose: Boolean;

    // Audio generation state
    FSampleOffset: Int64;

    // BPM timing (like BME snd_bpmcount)
    FBpmCount: Integer;      // Samples until next playroutine call
    FBpmTempo: Integer;      // 125 for PAL, 150 for NTSC

    // Hard restart parameter (from goattrk2.c: adparam = 0x0f00)
    FAdParam: Word;          // AD/SR values for hard restart

    // Frequency tables from gplay.c
    FFreqTblLo: array[0..127] of Byte;
    FFreqTblHi: array[0..127] of Byte;

    // SID register write order (from gsid.cpp)
    FSidOrder: array[0..NUMSIDREGS-1] of Byte;

    procedure InitFrequencyTables;
    procedure InitSidOrder;
    procedure InitChannels;
    procedure PlayRoutine;
    procedure Sequencer(C: Integer; var Chn: TGTChannel);
    function GetNoteFreq(ANote: Integer): Word;
    procedure CountPatternLengths;
    function GetSongName: string;
    function GetAuthor: string;
    function GetCopyright: string;

  public
    constructor Create;
    destructor Destroy; override;

    // Loading
    function LoadFromFile(const AFilename: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;

    // Playback control
    procedure Play(ASubtune: Integer = 0);
    procedure Stop;
    procedure Pause;
    procedure Resume;

    // Audio generation - THE KEY FUNCTION
    // This is sid_fillbuffer from gsid.cpp
    function FillBuffer(ABuffer: PSmallInt; ASamples: Integer): Integer;

    // Voice control
    procedure SetVoiceMute(AVoice: Integer; AMuted: Boolean);
    function IsVoiceMuted(AVoice: Integer): Boolean;

    // Debug
    procedure PrintSongInfo;
    procedure PrintTables;
    procedure AnalyzeCommands;

    // Register dump support
    function GetSidRegister(AReg: Integer): Byte;
    procedure CallPlayRoutine;

    // Properties
    property SID: TSedaiSIDEvo read FSID;
    property Loaded: Boolean read FLoaded;
    property SongName: string read GetSongName;
    property Author: string read GetAuthor;
    property Copyright: string read GetCopyright;
    property SubtuneCount: Integer read FPsNum;
    property FrameRate: Integer read FFrameRate write FFrameRate;
    property SampleRate: Integer read FSampleRate write FSampleRate;
    property Looping: Boolean read FLooping write FLooping;
    property SongFinished: Boolean read FSongFinished;
    property Verbose: Boolean read FVerbose write FVerbose;
    property WaveVerbose: Boolean read FWaveVerbose write FWaveVerbose;
    property Playing: Boolean read FLoaded;
    property FrameCounter: Int64 read FFrameCounter;

    procedure SetSampleRate(ARate: Integer);
  end;

implementation

const
  // Frequency table from gplay.c
  FreqTblLoData: array[0..95] of Byte = (
    $17,$27,$39,$4b,$5f,$74,$8a,$a1,$ba,$d4,$f0,$0e,
    $2d,$4e,$71,$96,$be,$e8,$14,$43,$74,$a9,$e1,$1c,
    $5a,$9c,$e2,$2d,$7c,$cf,$28,$85,$e8,$52,$c1,$37,
    $b4,$39,$c5,$5a,$f7,$9e,$4f,$0a,$d1,$a3,$82,$6e,
    $68,$71,$8a,$b3,$ee,$3c,$9e,$15,$a2,$46,$04,$dc,
    $d0,$e2,$14,$67,$dd,$79,$3c,$29,$44,$8d,$08,$b8,
    $a1,$c5,$28,$cd,$ba,$f1,$78,$53,$87,$1a,$10,$71,
    $42,$89,$4f,$9b,$74,$e2,$f0,$a6,$0e,$33,$20,$ff
  );

  FreqTblHiData: array[0..95] of Byte = (
    $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$02,
    $02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$04,
    $04,$04,$04,$05,$05,$05,$06,$06,$06,$07,$07,$08,
    $08,$09,$09,$0a,$0a,$0b,$0c,$0d,$0d,$0e,$0f,$10,
    $11,$12,$13,$14,$15,$17,$18,$1a,$1b,$1d,$1f,$20,
    $22,$24,$27,$29,$2b,$2e,$31,$34,$37,$3a,$3e,$41,
    $45,$49,$4e,$52,$57,$5c,$62,$68,$6e,$75,$7c,$83,
    $8b,$93,$9c,$a5,$af,$b9,$c4,$d0,$dd,$ea,$f8,$ff
  );

  // SID register order from gsid.cpp
  SidOrderData: array[0..NUMSIDREGS-1] of Byte = (
    $15,$16,$18,$17,
    $05,$06,$02,$03,$00,$01,$04,
    $0c,$0d,$09,$0a,$07,$08,$0b,
    $13,$14,$10,$11,$0e,$0f,$12
  );

{ TSedaiGoatTracker }

constructor TSedaiGoatTracker.Create;
begin
  inherited Create;

  FSID := TSedaiSIDEvo.Create;
  FOwnsChip := True;
  FSID.Clock := PALCLOCKRATE;

  FFrameRate := PALFRAMERATE;
  FClockRate := PALCLOCKRATE;
  FSampleRate := 44100;
  FMultiplier := 1;
  FMasterFader := $0F;
  FSongInit := PLAY_STOPPED;
  FLoaded := False;
  FLooping := False;
  FSongFinished := False;
  FVerbose := False;
  FWaveVerbose := False;
  FBpmTempo := 125;  // PAL
  FBpmCount := 0;
  FAdParam := $0F00;  // Default hard restart: AD=$0F, SR=$00 (original GoatTracker)

  InitFrequencyTables;
  InitSidOrder;
  InitChannels;

  FSID.SetSampleRate(FSampleRate);
end;

destructor TSedaiGoatTracker.Destroy;
begin
  if FOwnsChip and Assigned(FSID) then
    FSID.Free;
  inherited Destroy;
end;

procedure TSedaiGoatTracker.InitFrequencyTables;
var
  I: Integer;
begin
  for I := 0 to 95 do
  begin
    FFreqTblLo[I] := FreqTblLoData[I];
    FFreqTblHi[I] := FreqTblHiData[I];
  end;
  for I := 96 to 127 do
  begin
    FFreqTblLo[I] := 0;
    FFreqTblHi[I] := 0;
  end;
end;

procedure TSedaiGoatTracker.InitSidOrder;
var
  I: Integer;
begin
  for I := 0 to NUMSIDREGS - 1 do
    FSidOrder[I] := SidOrderData[I];
end;

procedure TSedaiGoatTracker.InitChannels;
var
  C: Integer;
begin
  FillChar(FChn, SizeOf(FChn), 0);

  for C := 0 to MAX_CHN - 1 do
  begin
    FChn[C].Trans := 0;
    FChn[C].Instr := 1;
    if FMultiplier > 0 then
      FChn[C].Tempo := 6 * FMultiplier - 1
    else
      FChn[C].Tempo := 5;
  end;

  if FMultiplier > 0 then
  begin
    FFunkTable[0] := 9 * FMultiplier - 1;
    FFunkTable[1] := 6 * FMultiplier - 1;
  end
  else
  begin
    FFunkTable[0] := 8;
    FFunkTable[1] := 5;
  end;
end;

function TSedaiGoatTracker.GetNoteFreq(ANote: Integer): Word;
begin
  if (ANote >= 0) and (ANote < 96) then
    Result := FFreqTblLo[ANote] or (FFreqTblHi[ANote] shl 8)
  else
    Result := 0;
end;

procedure TSedaiGoatTracker.SetSampleRate(ARate: Integer);
begin
  FSampleRate := ARate;
  if Assigned(FSID) then
    FSID.SetSampleRate(ARate);
end;

function TSedaiGoatTracker.GetSongName: string;
begin
  Result := StrPas(FSongName);
end;

function TSedaiGoatTracker.GetAuthor: string;
begin
  Result := StrPas(FAuthorName);
end;

function TSedaiGoatTracker.GetCopyright: string;
begin
  Result := StrPas(FCopyrightName);
end;

procedure TSedaiGoatTracker.CountPatternLengths;
var
  C, D, E: Integer;
begin
  for C := 0 to MAX_PATT - 1 do
  begin
    D := 0;
    while D <= MAX_PATTROWS do
    begin
      if FPattern[C, D * 4] = ENDPATT then
        Break;
      Inc(D);
    end;
    FPattLen[C] := D;
  end;

  for E := 0 to MAX_SONGS - 1 do
  begin
    for C := 0 to MAX_CHN - 1 do
    begin
      D := 0;
      while D < MAX_SONGLEN do
      begin
        if FSongOrder[E, C, D] >= LOOPSONG then
          Break;
        Inc(D);
      end;
      FSongLen[E, C] := D;
    end;
  end;
end;

function TSedaiGoatTracker.LoadFromFile(const AFilename: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  if not FileExists(AFilename) then
    Exit;

  FS := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

function TSedaiGoatTracker.LoadFromStream(AStream: TStream): Boolean;
var
  Ident: array[0..3] of Char;
  Amount, C, D, Length, LoadSize: Integer;
begin
  Result := False;
  FLoaded := False;

  // Clear existing data
  FillChar(FInstr, SizeOf(FInstr), 0);
  FillChar(FLTable, SizeOf(FLTable), 0);
  FillChar(FRTable, SizeOf(FRTable), 0);
  FillChar(FSongOrder, SizeOf(FSongOrder), 0);
  FillChar(FPattern, SizeOf(FPattern), 0);
  FillChar(FSongName, SizeOf(FSongName), 0);
  FillChar(FAuthorName, SizeOf(FAuthorName), 0);
  FillChar(FCopyrightName, SizeOf(FCopyrightName), 0);
  FillChar(FPattLen, SizeOf(FPattLen), 0);
  FillChar(FSongLen, SizeOf(FSongLen), 0);

  // Read identifier
  if AStream.Read(Ident, 4) <> 4 then
    Exit;

  if not ((Ident = 'GTS3') or (Ident = 'GTS4') or (Ident = 'GTS5')) then
  begin
    WriteLn('Unknown format: ', Ident);
    Exit;
  end;

  // Read infotexts
  AStream.Read(FSongName, MAX_STR);
  AStream.Read(FAuthorName, MAX_STR);
  AStream.Read(FCopyrightName, MAX_STR);

  // Read song orderlists
  Amount := AStream.ReadByte;
  FPsNum := Amount;

  for D := 0 to Amount - 1 do
  begin
    for C := 0 to MAX_CHN - 1 do
    begin
      Length := AStream.ReadByte;
      LoadSize := Length + 1;
      AStream.Read(FSongOrder[D, C, 0], LoadSize);
    end;
  end;

  // Read instruments
  Amount := AStream.ReadByte;
  for C := 1 to Amount do
  begin
    FInstr[C].AD := AStream.ReadByte;
    FInstr[C].SR := AStream.ReadByte;
    FInstr[C].Ptr[WTBL] := AStream.ReadByte;
    FInstr[C].Ptr[PTBL] := AStream.ReadByte;
    FInstr[C].Ptr[FTBL] := AStream.ReadByte;
    FInstr[C].Ptr[STBL] := AStream.ReadByte;
    FInstr[C].VibDelay := AStream.ReadByte;
    FInstr[C].GateTimer := AStream.ReadByte;
    FInstr[C].FirstWave := AStream.ReadByte;
    AStream.Read(FInstr[C].Name, MAX_INSTRNAMELEN);
  end;

  // Read tables
  for C := 0 to MAX_TABLES - 1 do
  begin
    LoadSize := AStream.ReadByte;
    if LoadSize > 0 then
    begin
      AStream.Read(FLTable[C, 0], LoadSize);
      AStream.Read(FRTable[C, 0], LoadSize);
    end;
  end;

  // Read patterns
  Amount := AStream.ReadByte;
  for C := 0 to Amount - 1 do
  begin
    Length := AStream.ReadByte * 4;
    AStream.Read(FPattern[C, 0], Length);
  end;

  CountPatternLengths;

  FLoaded := True;
  FSongFinished := False;
  Result := True;
end;

procedure TSedaiGoatTracker.Play(ASubtune: Integer);
begin
  if not FLoaded then
    Exit;

  FSongInit := PLAY_STOPPED;
  FPsNum := ASubtune;
  FSongInit := PLAY_BEGINNING;
  FStartPattPos := 0;
  FSongFinished := False;
  FFrameCounter := 0;
  FSampleOffset := 0;

  // Reset SID registers
  FillChar(FSidReg, SizeOf(FSidReg), 0);
  FSID.Reset;

  InitChannels;
end;

procedure TSedaiGoatTracker.Stop;
begin
  FSongInit := PLAY_STOPPED;
  FillChar(FSidReg, SizeOf(FSidReg), 0);
  FSID.Reset;
end;

procedure TSedaiGoatTracker.Pause;
begin
  // Gate off all voices
  FSidReg[$04] := FSidReg[$04] and $FE;
  FSidReg[$0B] := FSidReg[$0B] and $FE;
  FSidReg[$12] := FSidReg[$12] and $FE;
end;

procedure TSedaiGoatTracker.Resume;
begin
  // Gates will be restored by PlayRoutine
end;

procedure TSedaiGoatTracker.Sequencer(C: Integer; var Chn: TGTChannel);
begin
  if (FSongInit <> PLAY_STOPPED) and (Chn.PattPtr = $7FFFFFFF) then
  begin
    Chn.PattPtr := FStartPattPos * 4;
    if not Chn.Advance then
      Exit;

    // Song loop
    if FSongOrder[FPsNum, C, Chn.SongPtr] = LOOPSONG then
    begin
      Chn.SongPtr := FSongOrder[FPsNum, C, Chn.SongPtr + 1];
      if Chn.SongPtr >= FSongLen[FPsNum, C] then
      begin
        if FLooping then
          Chn.SongPtr := 0
        else
        begin
          FSongFinished := True;
          FSongInit := PLAY_STOPPED;
          Chn.SongPtr := 0;
        end;
        Exit;
      end;
    end;

    // Transpose
    if (FSongOrder[FPsNum, C, Chn.SongPtr] >= TRANSDOWN) and
       (FSongOrder[FPsNum, C, Chn.SongPtr] < LOOPSONG) then
    begin
      Chn.Trans := FSongOrder[FPsNum, C, Chn.SongPtr] - TRANSUP;
      Inc(Chn.SongPtr);
    end;

    // Repeat
    if (FSongOrder[FPsNum, C, Chn.SongPtr] >= REPEAT_MARK) and
       (FSongOrder[FPsNum, C, Chn.SongPtr] < TRANSDOWN) then
    begin
      Chn.Repeat_ := FSongOrder[FPsNum, C, Chn.SongPtr] - REPEAT_MARK;
      Inc(Chn.SongPtr);
    end;

    // Pattern number
    Chn.PattNum := FSongOrder[FPsNum, C, Chn.SongPtr];
    if Chn.Repeat_ > 0 then
      Dec(Chn.Repeat_)
    else
      Inc(Chn.SongPtr);

    // Check for illegal pattern
    if Chn.PattNum >= MAX_PATT then
    begin
      FSongInit := PLAY_STOPPED;
      FSongFinished := True;
      Chn.PattNum := 0;
    end;

    if Chn.PattPtr >= FPattLen[Chn.PattNum] * 4 then
      Chn.PattPtr := 0;
  end;
end;

procedure TSedaiGoatTracker.PlayRoutine;
label
  FILTERSTOP, WAVEEXEC, TICKNEFFECTS, PULSEEXEC, PULSEEXECDONE, NEXTCHN;
var
  C: Integer;
  Iptr: ^TGTInstrument;
  Cptr: ^TGTChannel;
  Wave, Note, NoteValue, Speed: Byte;
  TargetFreq, SpeedW: Word;
  CmpValue: Byte;
  Param: Byte;
begin
  // Handle stop request
  if FSongInit = PLAY_STOP then
  begin
    FSongInit := PLAY_STOPPED;
    Exit;
  end;

  // Song initialization
  if (FSongInit > 0) and (FSongInit < PLAY_STOPPED) then
  begin
    FLastSongInit := FSongInit;
    FFilterCtrl := 0;
    FFilterPtr := 0;

    for C := 0 to MAX_CHN - 1 do
    begin
      Cptr := @FChn[C];
      Cptr^.SongPtr := 0;
      Cptr^.Command := 0;
      Cptr^.CmdData := 0;
      Cptr^.NewCommand := 0;
      Cptr^.NewCmdData := 0;
      Cptr^.Advance := True;
      Cptr^.Wave := 0;
      Cptr^.Ptr[WTBL] := 0;
      Cptr^.NewNote := 0;
      Cptr^.Repeat_ := 0;

      if FMultiplier > 0 then
        Cptr^.Tick := 6 * FMultiplier - 1
      else
        Cptr^.Tick := 5;

      Cptr^.GateTimer := FInstr[1].GateTimer and $3F;
      Cptr^.PattPtr := $7FFFFFFF;

      if Cptr^.Tempo < 2 then
        Cptr^.Tempo := 0;

      case FSongInit of
        PLAY_BEGINNING:
        begin
          if FMultiplier > 0 then
          begin
            FFunkTable[0] := 9 * FMultiplier - 1;
            FFunkTable[1] := 6 * FMultiplier - 1;
            Cptr^.Tempo := 6 * FMultiplier - 1;
          end
          else
          begin
            FFunkTable[0] := 8;
            FFunkTable[1] := 5;
            Cptr^.Tempo := 5;
          end;

          // Check for tempo in instrument 63
          if (FInstr[MAX_INSTR - 1].AD >= 2) and (FInstr[MAX_INSTR - 1].Ptr[WTBL] = 0) then
            Cptr^.Tempo := FInstr[MAX_INSTR - 1].AD - 1;

          Cptr^.Trans := 0;
          Cptr^.Instr := 1;
          Sequencer(C, Cptr^);
        end;

        PLAY_PATTERN:
        begin
          Cptr^.Advance := False;
          Cptr^.PattPtr := FStartPattPos * 4;
          if Cptr^.PattPtr >= FPattLen[Cptr^.PattNum] * 4 then
            Cptr^.PattPtr := 0;
        end;

        PLAY_POS:
        begin
          Sequencer(C, Cptr^);
        end;
      end;
    end;

    if FSongInit <> PLAY_STOP then
      FSongInit := 0
    else
      FSongInit := PLAY_STOPPED;

    // Zero length song check
    if (FSongLen[FPsNum, 0] = 0) or (FSongLen[FPsNum, 1] = 0) or (FSongLen[FPsNum, 2] = 0) then
      FSongInit := PLAY_STOPPED;

    FStartPattPos := 0;
    Exit;
  end;

  // Filter processing
  if FFilterPtr > 0 then
  begin
    // Filter jump
    if FLTable[FTBL, FFilterPtr - 1] = $FF then
    begin
      FFilterPtr := FRTable[FTBL, FFilterPtr - 1];
      if FFilterPtr = 0 then
        goto FILTERSTOP;
    end;

    if FFilterTime = 0 then
    begin
      // Filter set
      if FLTable[FTBL, FFilterPtr - 1] >= $80 then
      begin
        FFilterType := FLTable[FTBL, FFilterPtr - 1] and $70;
        FFilterCtrl := FRTable[FTBL, FFilterPtr - 1];
        Inc(FFilterPtr);
        // Can be combined with cutoff set
        if FLTable[FTBL, FFilterPtr - 1] = $00 then
        begin
          FFilterCutoff := FRTable[FTBL, FFilterPtr - 1];
          Inc(FFilterPtr);
        end;
      end
      else
      begin
        // New modulation step
        if FLTable[FTBL, FFilterPtr - 1] > 0 then
          FFilterTime := FLTable[FTBL, FFilterPtr - 1]
        else
        begin
          // Cutoff set
          FFilterCutoff := FRTable[FTBL, FFilterPtr - 1];
          Inc(FFilterPtr);
        end;
      end;
    end;

    // Filter modulation
    if FFilterTime > 0 then
    begin
      FFilterCutoff := FFilterCutoff + FRTable[FTBL, FFilterPtr - 1];
      Dec(FFilterTime);
      if FFilterTime = 0 then
        Inc(FFilterPtr);
    end;
  end;

FILTERSTOP:
  FSidReg[$15] := $00;
  FSidReg[$16] := FFilterCutoff;
  FSidReg[$17] := FFilterCtrl;
  FSidReg[$18] := FFilterType or FMasterFader;

  // Process each channel
  for C := 0 to MAX_CHN - 1 do
  begin
    Cptr := @FChn[C];
    Iptr := @FInstr[Cptr^.Instr];

    // Reset tempo in jam mode
    if (FSongInit = PLAY_STOPPED) and (Cptr^.Tempo < 2) then
    begin
      if FMultiplier > 0 then
        Cptr^.Tempo := 6 * FMultiplier - 1
      else
        Cptr^.Tempo := 5;
    end;

    // Decrease tick
    Dec(Cptr^.Tick);

    if Cptr^.Tick <> 0 then
    begin
      // Tick N
      // Reload counter
      if Cptr^.Tick < 0 then
      begin
        if Cptr^.Tempo >= 2 then
          Cptr^.Tick := Cptr^.Tempo
        else
        begin
          // Funktempo
          Cptr^.Tick := FFunkTable[Cptr^.Tempo];
          Cptr^.Tempo := Cptr^.Tempo xor 1;
        end;

        // Check for illegally high gatetimer
        if Cptr^.GateTimer > Cptr^.Tick then
        begin
          FSongInit := PLAY_STOPPED;
          FSongFinished := True;
        end;
      end;
      goto WAVEEXEC;
    end;

    // Tick 0 - Advance in sequencer
    Sequencer(C, Cptr^);

    // Get gatetimer compare value
    Cptr^.GateTimer := Iptr^.GateTimer and $3F;

    // New note init
    if Cptr^.NewNote <> 0 then
    begin
      Cptr^.Note := Cptr^.NewNote - FIRSTNOTE;
      Cptr^.Command := 0;
      Cptr^.VibDelay := Iptr^.VibDelay;
      Cptr^.CmdData := Iptr^.Ptr[STBL];

      if Cptr^.NewCommand <> CMD_TONEPORTA then
      begin
        // DO NOT reset Freq here - GoatTracker keeps the previous frequency
        // This allows the wavetable to create different effects on repeated notes

        if Iptr^.FirstWave <> 0 then
        begin
          if Iptr^.FirstWave >= $FE then
            Cptr^.Gate := Iptr^.FirstWave
          else
          begin
            Cptr^.Wave := Iptr^.FirstWave;
            Cptr^.Gate := $FF;
          end;
        end;

        Cptr^.Ptr[WTBL] := Iptr^.Ptr[WTBL];

        if Iptr^.Ptr[PTBL] <> 0 then
        begin
          Cptr^.Ptr[PTBL] := Iptr^.Ptr[PTBL];
          Cptr^.PulseTime := 0;
        end;

        if Iptr^.Ptr[FTBL] <> 0 then
        begin
          FFilterPtr := Iptr^.Ptr[FTBL];
          FFilterTime := 0;
        end;

        FSidReg[$05 + 7 * C] := Iptr^.AD;
        FSidReg[$06 + 7 * C] := Iptr^.SR;
      end;
    end;

    // Tick 0 effects
    case Cptr^.NewCommand of
      CMD_DONOTHING:
      begin
        Cptr^.Command := 0;
        Cptr^.CmdData := Iptr^.Ptr[STBL];
      end;

      CMD_PORTAUP, CMD_PORTADOWN:
      begin
        Cptr^.VibTime := 0;
        Cptr^.Command := Cptr^.NewCommand;
        Cptr^.CmdData := Cptr^.NewCmdData;
      end;

      CMD_TONEPORTA, CMD_VIBRATO:
      begin
        Cptr^.Command := Cptr^.NewCommand;
        Cptr^.CmdData := Cptr^.NewCmdData;
      end;

      CMD_SETAD:
        FSidReg[$05 + 7 * C] := Cptr^.NewCmdData;

      CMD_SETSR:
        FSidReg[$06 + 7 * C] := Cptr^.NewCmdData;

      CMD_SETWAVE:
        Cptr^.Wave := Cptr^.NewCmdData;

      CMD_SETWAVEPTR:
      begin
        Cptr^.Ptr[WTBL] := Cptr^.NewCmdData;
        Cptr^.WaveTime := 0;
      end;

      CMD_SETPULSEPTR:
      begin
        Cptr^.Ptr[PTBL] := Cptr^.NewCmdData;
        Cptr^.PulseTime := 0;
      end;

      CMD_SETFILTERPTR:
      begin
        FFilterPtr := Cptr^.NewCmdData;
        FFilterTime := 0;
      end;

      CMD_SETFILTERCTRL:
      begin
        FFilterCtrl := Cptr^.NewCmdData;
        if FFilterCtrl = 0 then
          FFilterPtr := 0;
      end;

      CMD_SETFILTERCUTOFF:
        FFilterCutoff := Cptr^.NewCmdData;

      CMD_SETMASTERVOL:
        if Cptr^.NewCmdData < $10 then
          FMasterFader := Cptr^.NewCmdData;

      CMD_FUNKTEMPO:
      begin
        if Cptr^.NewCmdData <> 0 then
        begin
          FFunkTable[0] := FLTable[STBL, Cptr^.NewCmdData - 1] - 1;
          FFunkTable[1] := FRTable[STBL, Cptr^.NewCmdData - 1] - 1;
        end;
        FChn[0].Tempo := 0;
        FChn[1].Tempo := 0;
        FChn[2].Tempo := 0;
      end;

      CMD_SETTEMPO:
      begin
        Speed := Cptr^.NewCmdData and $7F;
        if Speed >= 3 then
          Dec(Speed);
        if Cptr^.NewCmdData >= $80 then
          Cptr^.Tempo := Speed
        else
        begin
          FChn[0].Tempo := Speed;
          FChn[1].Tempo := Speed;
          FChn[2].Tempo := Speed;
        end;
      end;
    end;

    if Cptr^.NewNote <> 0 then
    begin
      Cptr^.NewNote := 0;
      if Cptr^.NewCommand <> CMD_TONEPORTA then
        goto NEXTCHN;
    end;

WAVEEXEC:
    // Wavetable processing
    if Cptr^.Ptr[WTBL] > 0 then
    begin
      Wave := FLTable[WTBL, Cptr^.Ptr[WTBL] - 1];
      Note := FRTable[WTBL, Cptr^.Ptr[WTBL] - 1];

      if Wave > WAVELASTDELAY then
      begin
        // Normal waveform values
        if Wave < WAVESILENT then
          Cptr^.Wave := Wave;
        // Values without waveform selected
        if (Wave >= WAVESILENT) and (Wave <= WAVELASTSILENT) then
          Cptr^.Wave := Wave and $0F;
        // Command execution from wavetable
        if (Wave >= WAVECMD) and (Wave <= WAVELASTCMD) then
        begin
          Param := FRTable[WTBL, Cptr^.Ptr[WTBL] - 1];
          case Wave and $0F of
            CMD_PORTAUP:
            begin
              SpeedW := 0;
              if Param > 0 then
                SpeedW := (FLTable[STBL, Param - 1] shl 8) or FRTable[STBL, Param - 1];
              if SpeedW >= $8000 then
              begin
                SpeedW := GetNoteFreq(Cptr^.LastNote + 1) - GetNoteFreq(Cptr^.LastNote);
                SpeedW := SpeedW shr FRTable[STBL, Param - 1];
              end;
              Cptr^.Freq := Cptr^.Freq + SpeedW;
            end;

            CMD_PORTADOWN:
            begin
              SpeedW := 0;
              if Param > 0 then
                SpeedW := (FLTable[STBL, Param - 1] shl 8) or FRTable[STBL, Param - 1];
              if SpeedW >= $8000 then
              begin
                SpeedW := GetNoteFreq(Cptr^.LastNote + 1) - GetNoteFreq(Cptr^.LastNote);
                SpeedW := SpeedW shr FRTable[STBL, Param - 1];
              end;
              Cptr^.Freq := Cptr^.Freq - SpeedW;
            end;

            CMD_TONEPORTA:
            begin
              TargetFreq := GetNoteFreq(Cptr^.Note);
              if Param = 0 then
              begin
                Cptr^.Freq := TargetFreq;
                Cptr^.LastNote := Cptr^.Note;
                Cptr^.VibTime := 0;
              end
              else
              begin
                SpeedW := (FLTable[STBL, Param - 1] shl 8) or FRTable[STBL, Param - 1];
                if SpeedW >= $8000 then
                begin
                  SpeedW := GetNoteFreq(Cptr^.LastNote + 1) - GetNoteFreq(Cptr^.LastNote);
                  SpeedW := SpeedW shr FRTable[STBL, Param - 1];
                end;
                if Cptr^.Freq < TargetFreq then
                begin
                  Cptr^.Freq := Cptr^.Freq + SpeedW;
                  if Cptr^.Freq > TargetFreq then
                  begin
                    Cptr^.Freq := TargetFreq;
                    Cptr^.LastNote := Cptr^.Note;
                    Cptr^.VibTime := 0;
                  end;
                end;
                if Cptr^.Freq > TargetFreq then
                begin
                  Cptr^.Freq := Cptr^.Freq - SpeedW;
                  if Cptr^.Freq < TargetFreq then
                  begin
                    Cptr^.Freq := TargetFreq;
                    Cptr^.LastNote := Cptr^.Note;
                    Cptr^.VibTime := 0;
                  end;
                end;
              end;
            end;

            CMD_VIBRATO:
            begin
              SpeedW := 0;
              CmpValue := 0;
              if Param > 0 then
              begin
                CmpValue := FLTable[STBL, Param - 1];
                SpeedW := FRTable[STBL, Param - 1];
              end;
              if CmpValue >= $80 then
              begin
                CmpValue := CmpValue and $7F;
                SpeedW := GetNoteFreq(Cptr^.LastNote + 1) - GetNoteFreq(Cptr^.LastNote);
                SpeedW := SpeedW shr FRTable[STBL, Param - 1];
              end;
              if (Cptr^.VibTime < $80) and (Cptr^.VibTime > CmpValue) then
                Cptr^.VibTime := Cptr^.VibTime xor $FF;
              Cptr^.VibTime := Cptr^.VibTime + 2;
              if (Cptr^.VibTime and 1) <> 0 then
                Cptr^.Freq := Cptr^.Freq - SpeedW
              else
                Cptr^.Freq := Cptr^.Freq + SpeedW;
            end;

            CMD_SETAD:
              FSidReg[$05 + 7 * C] := Param;

            CMD_SETSR:
              FSidReg[$06 + 7 * C] := Param;

            CMD_SETWAVE:
              Cptr^.Wave := Param;

            CMD_SETPULSEPTR:
            begin
              Cptr^.Ptr[PTBL] := Param;
              Cptr^.PulseTime := 0;
            end;

            CMD_SETFILTERPTR:
            begin
              FFilterPtr := Param;
              FFilterTime := 0;
            end;

            CMD_SETFILTERCTRL:
            begin
              FFilterCtrl := Param;
              if FFilterCtrl = 0 then
                FFilterPtr := 0;
            end;

            CMD_SETFILTERCUTOFF:
              FFilterCutoff := Param;

            CMD_SETMASTERVOL:
              if Param < $10 then
                FMasterFader := Param;
          end;
        end;
      end
      else
      begin
        // Wavetable delay
        if Cptr^.WaveTime <> Wave then
        begin
          Inc(Cptr^.WaveTime);
          goto TICKNEFFECTS;
        end;
      end;

      Cptr^.WaveTime := 0;
      Inc(Cptr^.Ptr[WTBL]);

      // Wavetable jump
      if FLTable[WTBL, Cptr^.Ptr[WTBL] - 1] = $FF then
        Cptr^.Ptr[WTBL] := FRTable[WTBL, Cptr^.Ptr[WTBL] - 1];

      if (Wave >= WAVECMD) and (Wave <= WAVELASTCMD) then
        goto PULSEEXEC;

      if Note <> $80 then
      begin
        if Note < $80 then
          NoteValue := Note + Cptr^.Note
        else
          NoteValue := Note;
        NoteValue := NoteValue and $7F;
        Cptr^.Freq := GetNoteFreq(NoteValue);
        Cptr^.VibTime := 0;
        Cptr^.LastNote := NoteValue;
        goto PULSEEXEC;
      end;
    end;

TICKNEFFECTS:
    // Tick N effects
    case Cptr^.Command of
      CMD_PORTAUP:
      begin
        SpeedW := 0;
        if Cptr^.CmdData > 0 then
          SpeedW := (FLTable[STBL, Cptr^.CmdData - 1] shl 8) or FRTable[STBL, Cptr^.CmdData - 1];
        if SpeedW >= $8000 then
        begin
          SpeedW := GetNoteFreq(Cptr^.LastNote + 1) - GetNoteFreq(Cptr^.LastNote);
          SpeedW := SpeedW shr FRTable[STBL, Cptr^.CmdData - 1];
        end;
        Cptr^.Freq := Cptr^.Freq + SpeedW;
      end;

      CMD_PORTADOWN:
      begin
        SpeedW := 0;
        if Cptr^.CmdData > 0 then
          SpeedW := (FLTable[STBL, Cptr^.CmdData - 1] shl 8) or FRTable[STBL, Cptr^.CmdData - 1];
        if SpeedW >= $8000 then
        begin
          SpeedW := GetNoteFreq(Cptr^.LastNote + 1) - GetNoteFreq(Cptr^.LastNote);
          SpeedW := SpeedW shr FRTable[STBL, Cptr^.CmdData - 1];
        end;
        Cptr^.Freq := Cptr^.Freq - SpeedW;
      end;

      CMD_DONOTHING:
      begin
        if (Cptr^.CmdData = 0) or (Cptr^.VibDelay = 0) then
          goto PULSEEXEC;
        if Cptr^.VibDelay > 1 then
        begin
          Dec(Cptr^.VibDelay);
          goto PULSEEXEC;
        end;
        // Fall through to vibrato
        SpeedW := 0;
        CmpValue := 0;
        if Cptr^.CmdData > 0 then
        begin
          CmpValue := FLTable[STBL, Cptr^.CmdData - 1];
          SpeedW := FRTable[STBL, Cptr^.CmdData - 1];
        end;
        if CmpValue >= $80 then
        begin
          CmpValue := CmpValue and $7F;
          SpeedW := GetNoteFreq(Cptr^.LastNote + 1) - GetNoteFreq(Cptr^.LastNote);
          SpeedW := SpeedW shr FRTable[STBL, Cptr^.CmdData - 1];
        end;
        if (Cptr^.VibTime < $80) and (Cptr^.VibTime > CmpValue) then
          Cptr^.VibTime := Cptr^.VibTime xor $FF;
        Cptr^.VibTime := Cptr^.VibTime + 2;
        if (Cptr^.VibTime and 1) <> 0 then
          Cptr^.Freq := Cptr^.Freq - SpeedW
        else
          Cptr^.Freq := Cptr^.Freq + SpeedW;
      end;

      CMD_VIBRATO:
      begin
        SpeedW := 0;
        CmpValue := 0;
        if Cptr^.CmdData > 0 then
        begin
          CmpValue := FLTable[STBL, Cptr^.CmdData - 1];
          SpeedW := FRTable[STBL, Cptr^.CmdData - 1];
        end;
        if CmpValue >= $80 then
        begin
          CmpValue := CmpValue and $7F;
          SpeedW := GetNoteFreq(Cptr^.LastNote + 1) - GetNoteFreq(Cptr^.LastNote);
          SpeedW := SpeedW shr FRTable[STBL, Cptr^.CmdData - 1];
        end;
        if (Cptr^.VibTime < $80) and (Cptr^.VibTime > CmpValue) then
          Cptr^.VibTime := Cptr^.VibTime xor $FF;
        Cptr^.VibTime := Cptr^.VibTime + 2;
        if (Cptr^.VibTime and 1) <> 0 then
          Cptr^.Freq := Cptr^.Freq - SpeedW
        else
          Cptr^.Freq := Cptr^.Freq + SpeedW;
      end;

      CMD_TONEPORTA:
      begin
        TargetFreq := GetNoteFreq(Cptr^.Note);
        if Cptr^.CmdData = 0 then
        begin
          Cptr^.Freq := TargetFreq;
          Cptr^.LastNote := Cptr^.Note;
          Cptr^.VibTime := 0;
        end
        else
        begin
          SpeedW := (FLTable[STBL, Cptr^.CmdData - 1] shl 8) or FRTable[STBL, Cptr^.CmdData - 1];
          if SpeedW >= $8000 then
          begin
            SpeedW := GetNoteFreq(Cptr^.LastNote + 1) - GetNoteFreq(Cptr^.LastNote);
            SpeedW := SpeedW shr FRTable[STBL, Cptr^.CmdData - 1];
          end;
          if Cptr^.Freq < TargetFreq then
          begin
            Cptr^.Freq := Cptr^.Freq + SpeedW;
            if Cptr^.Freq > TargetFreq then
            begin
              Cptr^.Freq := TargetFreq;
              Cptr^.LastNote := Cptr^.Note;
              Cptr^.VibTime := 0;
            end;
          end;
          if Cptr^.Freq > TargetFreq then
          begin
            Cptr^.Freq := Cptr^.Freq - SpeedW;
            if Cptr^.Freq < TargetFreq then
            begin
              Cptr^.Freq := TargetFreq;
              Cptr^.LastNote := Cptr^.Note;
              Cptr^.VibTime := 0;
            end;
          end;
        end;
      end;
    end;

PULSEEXEC:
    // Pulsetable processing
    if Cptr^.Ptr[PTBL] > 0 then
    begin
      // Pulsetable jump
      if FLTable[PTBL, Cptr^.Ptr[PTBL] - 1] = $FF then
      begin
        Cptr^.Ptr[PTBL] := FRTable[PTBL, Cptr^.Ptr[PTBL] - 1];
        if Cptr^.Ptr[PTBL] = 0 then
          goto PULSEEXECDONE;
      end;

      if Cptr^.PulseTime = 0 then
      begin
        // Set pulse
        if FLTable[PTBL, Cptr^.Ptr[PTBL] - 1] >= $80 then
        begin
          Cptr^.Pulse := (FLTable[PTBL, Cptr^.Ptr[PTBL] - 1] and $0F) shl 8;
          Cptr^.Pulse := Cptr^.Pulse or FRTable[PTBL, Cptr^.Ptr[PTBL] - 1];
          Inc(Cptr^.Ptr[PTBL]);
        end
        else
          Cptr^.PulseTime := FLTable[PTBL, Cptr^.Ptr[PTBL] - 1];
      end;

      // Pulse modulation
      if Cptr^.PulseTime > 0 then
      begin
        Speed := FRTable[PTBL, Cptr^.Ptr[PTBL] - 1];
        if Speed < $80 then
        begin
          Cptr^.Pulse := Cptr^.Pulse + Speed;
          Cptr^.Pulse := Cptr^.Pulse and $FFF;
        end
        else
        begin
          Cptr^.Pulse := Cptr^.Pulse + Speed - $100;
          Cptr^.Pulse := Cptr^.Pulse and $FFF;
        end;
        Dec(Cptr^.PulseTime);
        if Cptr^.PulseTime = 0 then
          Inc(Cptr^.Ptr[PTBL]);
      end;
    end;

PULSEEXECDONE:
    if (FSongInit <> PLAY_STOPPED) and (Cptr^.Tick = Cptr^.GateTimer) then
    begin
      // Get new notes
      NoteValue := FPattern[Cptr^.PattNum, Cptr^.PattPtr];
      if FPattern[Cptr^.PattNum, Cptr^.PattPtr + 1] <> 0 then
        Cptr^.Instr := FPattern[Cptr^.PattNum, Cptr^.PattPtr + 1];
      Cptr^.NewCommand := FPattern[Cptr^.PattNum, Cptr^.PattPtr + 2];
      Cptr^.NewCmdData := FPattern[Cptr^.PattNum, Cptr^.PattPtr + 3];
      Cptr^.PattPtr := Cptr^.PattPtr + 4;

      if FPattern[Cptr^.PattNum, Cptr^.PattPtr] = ENDPATT then
        Cptr^.PattPtr := $7FFFFFFF;

      if NoteValue = KEYOFF then
        Cptr^.Gate := $FE;
      if NoteValue = KEYON then
        Cptr^.Gate := $FF;
      if NoteValue <= LASTNOTE then
      begin
        Cptr^.NewNote := NoteValue + Cptr^.Trans;
        if Cptr^.NewCommand <> CMD_TONEPORTA then
        begin
          if (FInstr[Cptr^.Instr].GateTimer and $40) = 0 then
          begin
            Cptr^.Gate := $FE;
            if (FInstr[Cptr^.Instr].GateTimer and $80) = 0 then
            begin
              // Hard restart - use adparam like original GoatTracker
              FSidReg[$05 + 7 * C] := FAdParam shr 8;    // AD
              FSidReg[$06 + 7 * C] := FAdParam and $FF;  // SR
            end;
          end;
        end;
      end;
    end;

NEXTCHN:
    // Write SID registers for this channel
    if Cptr^.Mute then
    begin
      FSidReg[$04 + 7 * C] := $08;
      Cptr^.Wave := $08;
    end
    else
    begin
      FSidReg[$00 + 7 * C] := Cptr^.Freq and $FF;
      FSidReg[$01 + 7 * C] := Cptr^.Freq shr 8;
      FSidReg[$02 + 7 * C] := Cptr^.Pulse and $FE;
      FSidReg[$03 + 7 * C] := Cptr^.Pulse shr 8;
      FSidReg[$04 + 7 * C] := Cptr^.Wave and Cptr^.Gate;
    end;

  end;

  Inc(FFrameCounter);
end;

function TSedaiGoatTracker.FillBuffer(ABuffer: PSmallInt; ASamples: Integer): Integer;
var
  MusicSamples: Integer;
  TDelta, TDelta2: Integer;
  C: Integer;
  O: Byte;
  SamplesOut: Integer;
  Ptr: PSmallInt;
  SamplesRemaining: Integer;
  FloatSample: Single;
  NeedWriteRegs: Boolean;
begin
  Result := 0;
  if not FLoaded then
    Exit;
  if ASamples <= 0 then
    Exit;

  Ptr := ABuffer;
  SamplesRemaining := ASamples;
  NeedWriteRegs := False;

  // Main mixing loop - EXACTLY like BME bme_snd.c
  while SamplesRemaining > 0 do
  begin
    // Check if we need to call PlayRoutine (like BME snd_bpmcount)
    if (FBpmCount = 0) and (FSongInit <> PLAY_STOPPED) then
    begin
      // Call player - updates FSidReg[]
      PlayRoutine;
      // Reset tempo counter: snd_bpmcount = ((snd_mixrate * 5) >> 1) / snd_bpmtempo
      FBpmCount := ((FSampleRate * 5) shr 1) div FBpmTempo;
      NeedWriteRegs := True;  // Mark that we need to write registers
    end;

    // Calculate how many samples to mix this iteration
    MusicSamples := SamplesRemaining;
    if (FBpmCount > 0) and (MusicSamples > FBpmCount) then
      MusicSamples := FBpmCount;
    if FBpmCount > 0 then
      Dec(FBpmCount, MusicSamples);

    // Calculate cycles for this chunk
    TDelta := (FClockRate * MusicSamples) div FSampleRate;

    // Write SID registers ONLY after PlayRoutine was called (like gsid.cpp)
    if NeedWriteRegs then
    begin
      NeedWriteRegs := False;

      for C := 0 to NUMSIDREGS - 1 do
      begin
        O := FSidOrder[C];
        FSID.WriteRegister(O, FSidReg[O]);

        // Clock SID for SIDWRITEDELAY cycles
        TDelta2 := SIDWRITEDELAY;
        if TDelta2 > TDelta then
          TDelta2 := TDelta;

        while TDelta2 > 0 do
        begin
          FSID.ClockCycle;
          Dec(TDelta2);
          Dec(TDelta);

          Inc(FSampleOffset, FSampleRate);
          if FSampleOffset >= FClockRate then
          begin
            Dec(FSampleOffset, FClockRate);
            if MusicSamples > 0 then
            begin
              FloatSample := FSID.Output;
              SamplesOut := Round(FloatSample * 32767);
              if SamplesOut > 32767 then SamplesOut := 32767;
              if SamplesOut < -32768 then SamplesOut := -32768;
              Ptr^ := SamplesOut;
              Inc(Ptr);
              Dec(MusicSamples);
              Dec(SamplesRemaining);
              Inc(Result);
            end;
          end;
        end;

        if TDelta <= 0 then
          Break;
      end;
    end;

    // Clock remaining cycles and generate samples
    while (TDelta > 0) and (MusicSamples > 0) do
    begin
      FSID.ClockCycle;
      Dec(TDelta);

      Inc(FSampleOffset, FSampleRate);
      if FSampleOffset >= FClockRate then
      begin
        Dec(FSampleOffset, FClockRate);
        FloatSample := FSID.Output;
        SamplesOut := Round(FloatSample * 32767);
        if SamplesOut > 32767 then SamplesOut := 32767;
        if SamplesOut < -32768 then SamplesOut := -32768;
        Ptr^ := SamplesOut;
        Inc(Ptr);
        Dec(MusicSamples);
        Dec(SamplesRemaining);
        Inc(Result);
      end;
    end;

    // Fill any remaining samples for this chunk (if cycles exhausted before samples)
    while MusicSamples > 0 do
    begin
      FloatSample := FSID.Output;
      SamplesOut := Round(FloatSample * 32767);
      if SamplesOut > 32767 then SamplesOut := 32767;
      if SamplesOut < -32768 then SamplesOut := -32768;
      Ptr^ := SamplesOut;
      Inc(Ptr);
      Dec(MusicSamples);
      Dec(SamplesRemaining);
      Inc(Result);
    end;
  end;
end;

procedure TSedaiGoatTracker.SetVoiceMute(AVoice: Integer; AMuted: Boolean);
begin
  if (AVoice >= 0) and (AVoice < MAX_CHN) then
    FChn[AVoice].Mute := AMuted;
end;

function TSedaiGoatTracker.IsVoiceMuted(AVoice: Integer): Boolean;
begin
  if (AVoice >= 0) and (AVoice < MAX_CHN) then
    Result := FChn[AVoice].Mute
  else
    Result := False;
end;

procedure TSedaiGoatTracker.PrintSongInfo;
begin
  WriteLn('=== GoatTracker Song Info ===');
  WriteLn('Title:    ', FSongName);
  WriteLn('Author:   ', FAuthorName);
  WriteLn('Copyright:', FCopyrightName);
  WriteLn('Subtunes: ', FPsNum);
  WriteLn('Frame Rate:', FFrameRate, ' Hz');
end;

procedure TSedaiGoatTracker.PrintTables;
var
  I: Integer;
begin
  WriteLn('=== Instrument 7 (CH2 bass) ===');
  WriteLn(Format('AD=$%02X SR=$%02X FirstWave=$%02X GateTimer=$%02X',
    [FInstr[7].AD, FInstr[7].SR, FInstr[7].FirstWave, FInstr[7].GateTimer]));
  WriteLn(Format('WavePtr=%d PulsePtr=%d FilterPtr=%d SpeedPtr=%d',
    [FInstr[7].Ptr[WTBL], FInstr[7].Ptr[PTBL], FInstr[7].Ptr[FTBL], FInstr[7].Ptr[STBL]]));

  WriteLn;
  WriteLn('=== Instrument 10 ===');
  WriteLn(Format('AD=$%02X SR=$%02X FirstWave=$%02X GateTimer=$%02X',
    [FInstr[10].AD, FInstr[10].SR, FInstr[10].FirstWave, FInstr[10].GateTimer]));
  WriteLn(Format('WavePtr=%d PulsePtr=%d FilterPtr=%d SpeedPtr=%d',
    [FInstr[10].Ptr[WTBL], FInstr[10].Ptr[PTBL], FInstr[10].Ptr[FTBL], FInstr[10].Ptr[STBL]]));

  WriteLn;
  WriteLn('=== WaveTable (first 10 entries - for instr 7) ===');
  for I := 0 to 9 do
  begin
    WriteLn(Format('%02d: L=$%02X R=$%02X', [I + 1, FLTable[WTBL, I], FRTable[WTBL, I]]));
  end;

  WriteLn;
  WriteLn('=== WaveTable (from ptr 40 to 70) ===');
  for I := 39 to 70 do  // Show entries from ptr 40 to 70
  begin
    if I < MAX_TABLELEN then
      WriteLn(Format('%02d: L=$%02X R=$%02X', [I + 1, FLTable[WTBL, I], FRTable[WTBL, I]]));
  end;

  WriteLn;
  WriteLn('=== Pulse Table (first 20 entries) ===');
  for I := 0 to 19 do
  begin
    if (FLTable[PTBL, I] <> 0) or (FRTable[PTBL, I] <> 0) then
      WriteLn(Format('%02d: L=$%02X R=$%02X', [I + 1, FLTable[PTBL, I], FRTable[PTBL, I]]));
  end;

  WriteLn;
  WriteLn('=== Speed Table ===');
  for I := 0 to 15 do
  begin
    if (FLTable[STBL, I] <> 0) or (FRTable[STBL, I] <> 0) then
      WriteLn(Format('%02X: %02X %02X', [I + 1, FLTable[STBL, I], FRTable[STBL, I]]));
  end;
end;

function TSedaiGoatTracker.GetSidRegister(AReg: Integer): Byte;
begin
  if (AReg >= 0) and (AReg < NUMSIDREGS) then
    Result := FSidReg[AReg]
  else
    Result := 0;
end;

procedure TSedaiGoatTracker.CallPlayRoutine;
begin
  if FLoaded and (FSongInit <> PLAY_STOPPED) then
    PlayRoutine;
end;

procedure TSedaiGoatTracker.AnalyzeCommands;
var
  P, R, Cmd: Integer;
  Wave: Byte;
  CmdCounts: array[0..15] of Integer;
  WaveCmdCounts: array[0..15] of Integer;
begin
  // Initialize counters
  for Cmd := 0 to 15 do
  begin
    CmdCounts[Cmd] := 0;
    WaveCmdCounts[Cmd] := 0;
  end;

  // Scan patterns for commands
  WriteLn('=== Scanning Patterns for Commands ===');
  for P := 0 to MAX_PATT - 1 do
  begin
    R := 0;
    while R <= MAX_PATTROWS do
    begin
      if FPattern[P, R * 4] = ENDPATT then
        Break;
      Cmd := FPattern[P, R * 4 + 2];  // Command byte
      if (Cmd >= 0) and (Cmd <= 15) then
        Inc(CmdCounts[Cmd]);
      Inc(R);
    end;
  end;

  WriteLn('Pattern command counts:');
  for Cmd := 0 to 15 do
  begin
    if CmdCounts[Cmd] > 0 then
      WriteLn(Format('  CMD %2d: %d occurrences', [Cmd, CmdCounts[Cmd]]));
  end;
  if CmdCounts[CMD_SETMASTERVOL] = 0 then
    WriteLn('  *** CMD_SETMASTERVOL (13) NOT FOUND in patterns! ***');

  // Scan wavetable for commands ($F0-$FE)
  WriteLn;
  WriteLn('=== Scanning WaveTable for Commands ===');
  for R := 0 to MAX_TABLELEN - 1 do
  begin
    Wave := FLTable[WTBL, R];
    if (Wave >= WAVECMD) and (Wave <= WAVELASTCMD) then
    begin
      Cmd := Wave and $0F;
      Inc(WaveCmdCounts[Cmd]);
    end;
  end;

  WriteLn('WaveTable command counts:');
  for Cmd := 0 to 15 do
  begin
    if WaveCmdCounts[Cmd] > 0 then
      WriteLn(Format('  WAVECMD %2d ($F%X): %d occurrences', [Cmd, Cmd, WaveCmdCounts[Cmd]]));
  end;
  if WaveCmdCounts[CMD_SETMASTERVOL] = 0 then
    WriteLn('  *** WAVECMD_SETMASTERVOL ($FD) NOT FOUND in wavetable! ***');
end;

end.
