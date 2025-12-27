{*
 * Sedai Audio Foundation - Professional audio synthesis library
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * SedaiSIDDumpPlayer - SID Register Dump Player
 *
 * This module provides playback of SID register dump files,
 * which contain frame-by-frame captures of SID register states.
 * This allows playing C64 SID music without 6502 CPU emulation.
 *
 * Supported formats:
 * - Binary dump (25 bytes per frame, raw register values)
 * - Text dump (human-readable hex values)
 * - Compressed dump (only changed registers per frame)
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

unit SedaiSIDDumpPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiSIDEvo;

const
  // SID Register addresses (offset from $D400)
  SID_REG_COUNT = 25;  // 25 registers ($D400-$D418)

  // Voice 1 registers (offset 0-6)
  SID_V1_FREQ_LO   = 0;   // $D400
  SID_V1_FREQ_HI   = 1;   // $D401
  SID_V1_PW_LO     = 2;   // $D402
  SID_V1_PW_HI     = 3;   // $D403
  SID_V1_CTRL      = 4;   // $D404
  SID_V1_AD        = 5;   // $D405
  SID_V1_SR        = 6;   // $D406

  // Voice 2 registers (offset 7-13)
  SID_V2_FREQ_LO   = 7;   // $D407
  SID_V2_FREQ_HI   = 8;   // $D408
  SID_V2_PW_LO     = 9;   // $D409
  SID_V2_PW_HI     = 10;  // $D40A
  SID_V2_CTRL      = 11;  // $D40B
  SID_V2_AD        = 12;  // $D40C
  SID_V2_SR        = 13;  // $D40D

  // Voice 3 registers (offset 14-20)
  SID_V3_FREQ_LO   = 14;  // $D40E
  SID_V3_FREQ_HI   = 15;  // $D40F
  SID_V3_PW_LO     = 16;  // $D410
  SID_V3_PW_HI     = 17;  // $D411
  SID_V3_CTRL      = 18;  // $D412
  SID_V3_AD        = 19;  // $D413
  SID_V3_SR        = 20;  // $D414

  // Filter and volume registers (offset 21-24)
  SID_FILTER_LO    = 21;  // $D415 - Filter cutoff low (bits 0-2)
  SID_FILTER_HI    = 22;  // $D416 - Filter cutoff high (bits 3-10)
  SID_FILTER_CTRL  = 23;  // $D417 - Filter resonance and routing
  SID_VOL_FILTER   = 24;  // $D418 - Volume and filter mode

  // Control register bits
  SID_CTRL_GATE    = $01;
  SID_CTRL_SYNC    = $02;
  SID_CTRL_RING    = $04;
  SID_CTRL_TEST    = $08;
  SID_CTRL_TRI     = $10;
  SID_CTRL_SAW     = $20;
  SID_CTRL_PULSE   = $40;
  SID_CTRL_NOISE   = $80;

  // Frame rates
  SID_FRAMERATE_PAL  = 50;   // 50 Hz (PAL C64)
  SID_FRAMERATE_NTSC = 60;   // 60 Hz (NTSC C64)

  // SID clock frequency for frequency calculation
  SID_CLOCK_PAL  = 985248;   // PAL clock
  SID_CLOCK_NTSC = 1022727;  // NTSC clock

type
  // Single frame of SID register data
  TSIDFrame = array[0..SID_REG_COUNT-1] of Byte;

  // Dump file format
  TSIDDumpFormat = (
    sdfBinaryRaw,      // 25 bytes per frame, raw
    sdfBinaryCompact,  // Only changed registers
    sdfText            // Human-readable hex
  );

  // Player state
  TSIDPlayerState = (
    spsIdle,
    spsPlaying,
    spsPaused,
    spsStopped
  );

  // SID Dump file info
  TSIDDumpInfo = record
    Title: string;
    Author: string;
    Copyright: string;
    FrameCount: Integer;
    FrameRate: Integer;      // 50 or 60
    DurationSeconds: Single;
    Format: TSIDDumpFormat;
    SIDModel: Integer;       // 0=6581, 1=8580
  end;

  { TSedaiSIDDumpPlayer }
  TSedaiSIDDumpPlayer = class
  private
    FSIDEvo: TSedaiSIDEvo;
    FOwnsEvo: Boolean;

    // Dump data
    FFrames: array of TSIDFrame;
    FFrameCount: Integer;
    FCurrentFrame: Integer;
    FFrameRate: Integer;

    // Playback state
    FState: TSIDPlayerState;
    FLooping: Boolean;

    // Info
    FInfo: TSIDDumpInfo;

    // Previous frame for change detection
    FPrevFrame: TSIDFrame;

    // SID clock for frequency conversion
    FSIDClock: Integer;

    procedure ApplyFrame(const AFrame: TSIDFrame);
    procedure ApplyVoice(AVoice: Integer; const AFrame: TSIDFrame; ABaseReg: Integer);
    procedure ApplyFilter(const AFrame: TSIDFrame);
    procedure ApplyVolume(const AFrame: TSIDFrame);

    function SIDFreqToHz(AFreqReg: Word): Single;
    function SIDPulseWidthToFloat(APW: Word): Single;
    function SIDADToFloat(AAD: Byte; AIsAttack: Boolean): Single;
    function SIDSRToFloat(ASR: Byte; AIsSustain: Boolean): Single;
    function SIDControlToWaveform(ACtrl: Byte): Byte;
    function SIDFilterCutoffToFloat(ALo, AHi: Byte): Single;
    function SIDResonanceToFloat(ARes: Byte): Single;

  public
    constructor Create; overload;
    constructor Create(ASIDEvo: TSedaiSIDEvo); overload;
    destructor Destroy; override;

    // File loading
    function LoadFromFile(const AFilename: string): Boolean;
    function LoadFromBinaryFile(const AFilename: string): Boolean;
    function LoadFromTextFile(const AFilename: string): Boolean;
    function LoadFromSIDDumpTextFile(const AFilename: string): Boolean;  // Tabular SIDDump format
    function LoadFromStream(AStream: TStream; AFormat: TSIDDumpFormat): Boolean;

    // Playback control
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Rewind;
    procedure SetPosition(AFrame: Integer);

    // Must be called periodically (e.g., from timer or game loop)
    // Returns True if a frame was processed
    function Update: Boolean;

    // Single frame advance (for step debugging)
    procedure StepFrame;

    // Debug: Print parsed frame data
    procedure PrintFrameDebug(AFrameNum: Integer);
    // Debug: Export parsed frames to a log file for verification
    procedure ExportDebugLog(const AFilename: string; AMaxFrames: Integer = 100);
    // Debug: Count frames with gate ON for a voice
    function CountGates(AVoice: Integer; AMaxFrames: Integer): Integer;

    // Voice muting (for debugging/mixing)
    procedure SetVoiceMute(AVoice: Integer; AMuted: Boolean);
    function IsVoiceMuted(AVoice: Integer): Boolean;
    procedure ToggleVoiceMute(AVoice: Integer);

    // Properties
    property State: TSIDPlayerState read FState;
    property CurrentFrame: Integer read FCurrentFrame;
    property FrameCount: Integer read FFrameCount;
    property FrameRate: Integer read FFrameRate write FFrameRate;
    property Looping: Boolean read FLooping write FLooping;
    property Info: TSIDDumpInfo read FInfo;
    property SIDEvo: TSedaiSIDEvo read FSIDEvo;

    function GetProgress: Single;  // 0.0 to 1.0
    function GetPositionSeconds: Single;
    function GetDurationSeconds: Single;
    function IsPlaying: Boolean;
  end;

// Utility function to convert .SID to dump using external tool
function ConvertSIDToDump(const ASIDFile, ADumpFile: string): Boolean;

implementation

{ TSedaiSIDDumpPlayer }

constructor TSedaiSIDDumpPlayer.Create;
begin
  inherited Create;
  FSIDEvo := TSedaiSIDEvo.Create;
  FSIDEvo.Initialize(8);
  FOwnsEvo := True;

  // Enable SID authenticity mode for accurate playback
  FSIDEvo.SetSIDMode;

  FFrameCount := 0;
  FCurrentFrame := 0;
  FFrameRate := SID_FRAMERATE_PAL;
  FState := spsIdle;
  FLooping := False;
  FSIDClock := SID_CLOCK_PAL;

  FillChar(FPrevFrame, SizeOf(TSIDFrame), 0);
  FillChar(FInfo, SizeOf(TSIDDumpInfo), 0);
  FInfo.FrameRate := SID_FRAMERATE_PAL;
end;

constructor TSedaiSIDDumpPlayer.Create(ASIDEvo: TSedaiSIDEvo);
begin
  inherited Create;
  FSIDEvo := ASIDEvo;
  FOwnsEvo := False;

  // Suggest enabling SID mode for accurate playback
  // (user can override if they prefer evolved sound)
  WriteLn('SIDDump: Using external SIDEvo instance.');
  WriteLn('         For authentic SID playback, call SIDEvo.SetSIDMode');

  FFrameCount := 0;
  FCurrentFrame := 0;
  FFrameRate := SID_FRAMERATE_PAL;
  FState := spsIdle;
  FLooping := False;
  FSIDClock := SID_CLOCK_PAL;

  FillChar(FPrevFrame, SizeOf(TSIDFrame), 0);
  FillChar(FInfo, SizeOf(TSIDDumpInfo), 0);
  FInfo.FrameRate := SID_FRAMERATE_PAL;
end;

destructor TSedaiSIDDumpPlayer.Destroy;
begin
  Stop;
  SetLength(FFrames, 0);

  if FOwnsEvo and Assigned(FSIDEvo) then
  begin
    FSIDEvo.Shutdown;
    FreeAndNil(FSIDEvo);
  end;

  inherited Destroy;
end;

// Convert SID frequency register to Hz
function TSedaiSIDDumpPlayer.SIDFreqToHz(AFreqReg: Word): Single;
begin
  // SID formula: Fout = (Fn * Fclk) / 16777216
  // Where Fn is the 16-bit frequency register value
  Result := (AFreqReg * FSIDClock) / 16777216.0;
end;

// Convert SID pulse width (12-bit) to 0.0-1.0
function TSedaiSIDDumpPlayer.SIDPulseWidthToFloat(APW: Word): Single;
begin
  // PW is 12-bit (0-4095), duty cycle = PW/4096
  Result := (APW and $0FFF) / 4096.0;
end;

// Convert SID Attack/Decay nibble to float (0.0-1.0)
function TSedaiSIDDumpPlayer.SIDADToFloat(AAD: Byte; AIsAttack: Boolean): Single;
begin
  // Attack is upper nibble, Decay is lower nibble
  // Both are 0-15 values
  if AIsAttack then
    Result := ((AAD shr 4) and $0F) / 15.0
  else
    Result := (AAD and $0F) / 15.0;
end;

// Convert SID Sustain/Release nibble to float
function TSedaiSIDDumpPlayer.SIDSRToFloat(ASR: Byte; AIsSustain: Boolean): Single;
begin
  // Sustain is upper nibble (level), Release is lower nibble (time)
  if AIsSustain then
    Result := ((ASR shr 4) and $0F) / 15.0
  else
    Result := (ASR and $0F) / 15.0;
end;

// Convert SID control byte to SIDEvo waveform
function TSedaiSIDDumpPlayer.SIDControlToWaveform(ACtrl: Byte): Byte;
begin
  Result := SIDEVO_WAVE_NONE;

  // Map SID waveform bits to SIDEvo constants
  if (ACtrl and SID_CTRL_NOISE) <> 0 then
    Result := Result or SIDEVO_WAVE_NOISE
  else
  begin
    if (ACtrl and SID_CTRL_PULSE) <> 0 then
      Result := Result or SIDEVO_WAVE_PULSE;
    if (ACtrl and SID_CTRL_SAW) <> 0 then
      Result := Result or SIDEVO_WAVE_SAWTOOTH;
    if (ACtrl and SID_CTRL_TRI) <> 0 then
      Result := Result or SIDEVO_WAVE_TRIANGLE;
  end;

  // Default to triangle if nothing set
  if Result = SIDEVO_WAVE_NONE then
    Result := SIDEVO_WAVE_TRIANGLE;
end;

// Convert SID filter cutoff to 0.0-1.0
function TSedaiSIDDumpPlayer.SIDFilterCutoffToFloat(ALo, AHi: Byte): Single;
var
  ACutoff: Word;
begin
  // Filter cutoff is 11-bit: low 3 bits from $D415, high 8 bits from $D416
  ACutoff := (ALo and $07) or (Word(AHi) shl 3);
  Result := ACutoff / 2047.0;
end;

// Convert SID resonance to 0.0-1.0
function TSedaiSIDDumpPlayer.SIDResonanceToFloat(ARes: Byte): Single;
begin
  // Resonance is upper 4 bits of $D417
  Result := ((ARes shr 4) and $0F) / 15.0;
end;

// Apply a single voice from frame data
procedure TSedaiSIDDumpPlayer.ApplyVoice(AVoice: Integer; const AFrame: TSIDFrame; ABaseReg: Integer);
var
  AFreqReg: Word;
  APWReg: Word;
  ACtrl, APrevCtrl, AAD, ASR: Byte;
  AFreqHz: Single;
  AGateOn, APrevGateOn: Boolean;
  AWaveform: Byte;
begin
  // Read registers
  AFreqReg := AFrame[ABaseReg] or (Word(AFrame[ABaseReg + 1]) shl 8);
  APWReg := AFrame[ABaseReg + 2] or (Word(AFrame[ABaseReg + 3] and $0F) shl 8);
  ACtrl := AFrame[ABaseReg + 4];
  AAD := AFrame[ABaseReg + 5];
  ASR := AFrame[ABaseReg + 6];

  // Get previous control byte for gate edge detection
  APrevCtrl := FPrevFrame[ABaseReg + 4];

  // Convert frequency
  AFreqHz := SIDFreqToHz(AFreqReg);

  // Check gate - current and previous for edge detection
  AGateOn := (ACtrl and SID_CTRL_GATE) <> 0;
  APrevGateOn := (APrevCtrl and SID_CTRL_GATE) <> 0;

  // Convert waveform
  AWaveform := SIDControlToWaveform(ACtrl);

  // Apply to SIDEvo
  FSIDEvo.SetWaveform(AVoice, AWaveform);
  FSIDEvo.SetFrequencyHz(AVoice, AFreqHz);
  FSIDEvo.SetPulseWidth(AVoice, SIDPulseWidthToFloat(APWReg));

  // ADSR
  FSIDEvo.SetAttack(AVoice, SIDADToFloat(AAD, True));
  FSIDEvo.SetDecay(AVoice, SIDADToFloat(AAD, False));
  FSIDEvo.SetSustain(AVoice, SIDSRToFloat(ASR, True));
  FSIDEvo.SetRelease(AVoice, SIDSRToFloat(ASR, False));

  // Ring modulation and sync
  if (ACtrl and SID_CTRL_RING) <> 0 then
    FSIDEvo.SetRingModulation(AVoice, True)
  else
    FSIDEvo.SetRingModulation(AVoice, False);

  if (ACtrl and SID_CTRL_SYNC) <> 0 then
    FSIDEvo.SetHardSync(AVoice, True)
  else
    FSIDEvo.SetHardSync(AVoice, False);

  // Gate control - use edge detection for proper trills/arpeggios
  // Gate ON: trigger on rising edge (0->1) OR if voice not active
  // Gate OFF: trigger on falling edge (1->0)
  if AGateOn then
  begin
    if (not APrevGateOn) or (not FSIDEvo.IsVoiceActive(AVoice)) then
      // Rising edge: gate was OFF, now ON - start new note
      FSIDEvo.GateOn(AVoice)
    else
      // Gate still ON - just update frequency (for glissando, vibrato)
      FSIDEvo.UpdateActiveVoice(AVoice);
  end
  else
  begin
    if APrevGateOn then
      // Falling edge: gate was ON, now OFF - start release
      FSIDEvo.GateOff(AVoice);
  end;
end;

// Apply filter settings
procedure TSedaiSIDDumpPlayer.ApplyFilter(const AFrame: TSIDFrame);
var
  AFilterCtrl: Byte;
  ACutoff, AResonance: Single;
  AFilterType: Byte;
  i: Integer;
begin
  AFilterCtrl := AFrame[SID_VOL_FILTER];
  ACutoff := SIDFilterCutoffToFloat(AFrame[SID_FILTER_LO], AFrame[SID_FILTER_HI]);
  AResonance := SIDResonanceToFloat(AFrame[SID_FILTER_CTRL]);

  // Determine filter type from mode bits
  AFilterType := SIDEVO_FILTER_OFF;
  if (AFilterCtrl and $10) <> 0 then
    AFilterType := AFilterType or SIDEVO_FILTER_LOWPASS;
  if (AFilterCtrl and $20) <> 0 then
    AFilterType := AFilterType or SIDEVO_FILTER_BANDPASS;
  if (AFilterCtrl and $40) <> 0 then
    AFilterType := AFilterType or SIDEVO_FILTER_HIGHPASS;

  // Apply global filter
  FSIDEvo.SetGlobalFilterType(AFilterType);
  FSIDEvo.SetGlobalFilterCutoff(ACutoff);
  FSIDEvo.SetGlobalFilterResonance(AResonance);
  FSIDEvo.SetGlobalFilter(AFilterType <> SIDEVO_FILTER_OFF);

  // Route voices through filter based on bits 0-2 of $D417
  for i := 0 to 2 do
  begin
    if (AFrame[SID_FILTER_CTRL] and (1 shl i)) <> 0 then
    begin
      // Voice i is routed through filter
      FSIDEvo.SetVoiceFilterType(i, AFilterType);
      FSIDEvo.SetVoiceFilterCutoff(i, ACutoff);
      FSIDEvo.SetVoiceFilterResonance(i, AResonance);
      FSIDEvo.SetVoiceFilter(i, True);
    end
    else
      FSIDEvo.SetVoiceFilter(i, False);
  end;
end;

// Apply volume
procedure TSedaiSIDDumpPlayer.ApplyVolume(const AFrame: TSIDFrame);
var
  AVol: Single;
begin
  // Volume is lower 4 bits of $D418
  AVol := (AFrame[SID_VOL_FILTER] and $0F) / 15.0;
  FSIDEvo.SetMasterVolume(AVol);
end;

// Apply a complete frame
procedure TSedaiSIDDumpPlayer.ApplyFrame(const AFrame: TSIDFrame);
begin
  // Apply all three voices
  ApplyVoice(0, AFrame, SID_V1_FREQ_LO);
  ApplyVoice(1, AFrame, SID_V2_FREQ_LO);
  ApplyVoice(2, AFrame, SID_V3_FREQ_LO);

  // Apply filter and volume
  ApplyFilter(AFrame);
  ApplyVolume(AFrame);

  // Store for next comparison
  Move(AFrame, FPrevFrame, SizeOf(TSIDFrame));
end;

// Load from binary file (raw 25 bytes per frame)
function TSedaiSIDDumpPlayer.LoadFromBinaryFile(const AFilename: string): Boolean;
var
  AStream: TFileStream;
  AFileSize: Int64;
  i: Integer;
begin
  Result := False;

  if not FileExists(AFilename) then
  begin
    WriteLn('SIDDump: File not found: ', AFilename);
    Exit;
  end;

  try
    AStream := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite);
    try
      AFileSize := AStream.Size;

      // Calculate frame count
      FFrameCount := AFileSize div SID_REG_COUNT;
      if FFrameCount = 0 then
      begin
        WriteLn('SIDDump: File too small');
        Exit;
      end;

      // Allocate frames
      SetLength(FFrames, FFrameCount);

      // Read all frames
      for i := 0 to FFrameCount - 1 do
        AStream.Read(FFrames[i], SID_REG_COUNT);

      // Update info
      FInfo.FrameCount := FFrameCount;
      FInfo.Format := sdfBinaryRaw;
      FInfo.DurationSeconds := FFrameCount / FFrameRate;
      FInfo.Title := ExtractFileName(AFilename);

      WriteLn('SIDDump: Loaded ', FFrameCount, ' frames (', FInfo.DurationSeconds:0:2, ' seconds)');
      Result := True;

    finally
      AStream.Free;
    end;
  except
    on E: Exception do
      WriteLn('SIDDump: Error loading file: ', E.Message);
  end;
end;

// Load from text file (hex dump format)
function TSedaiSIDDumpPlayer.LoadFromTextFile(const AFilename: string): Boolean;
var
  ALines: TStringList;
  ALine: string;
  AParts: TStringArray;
  AFrame: TSIDFrame;
  i, j, ARegIndex: Integer;
  AValue: Integer;
begin
  Result := False;

  if not FileExists(AFilename) then
  begin
    WriteLn('SIDDump: File not found: ', AFilename);
    Exit;
  end;

  ALines := TStringList.Create;
  try
    ALines.LoadFromFile(AFilename);

    // Count valid frames and allocate
    FFrameCount := 0;
    for i := 0 to ALines.Count - 1 do
    begin
      ALine := Trim(ALines[i]);
      // Skip comments and empty lines
      if (ALine = '') or (ALine[1] = ';') or (ALine[1] = '#') then
        Continue;
      Inc(FFrameCount);
    end;

    if FFrameCount = 0 then
    begin
      WriteLn('SIDDump: No valid frames found');
      Exit;
    end;

    SetLength(FFrames, FFrameCount);

    // Parse frames
    j := 0;
    for i := 0 to ALines.Count - 1 do
    begin
      ALine := Trim(ALines[i]);
      if (ALine = '') or (ALine[1] = ';') or (ALine[1] = '#') then
        Continue;

      // Parse hex values separated by spaces or commas
      ALine := StringReplace(ALine, ',', ' ', [rfReplaceAll]);
      ALine := StringReplace(ALine, '  ', ' ', [rfReplaceAll]);
      AParts := ALine.Split([' ']);

      FillChar(AFrame, SizeOf(AFrame), 0);
      ARegIndex := 0;

      for AValue := 0 to Length(AParts) - 1 do
      begin
        if (ARegIndex >= SID_REG_COUNT) then Break;
        if AParts[AValue] = '' then Continue;

        // Handle ".." as unchanged (use previous value)
        if (AParts[AValue] = '..') or (AParts[AValue] = '--') then
        begin
          if j > 0 then
            AFrame[ARegIndex] := FFrames[j-1][ARegIndex]
          else
            AFrame[ARegIndex] := 0;
        end
        else
        begin
          // Parse hex value
          try
            AFrame[ARegIndex] := StrToInt('$' + AParts[AValue]);
          except
            AFrame[ARegIndex] := 0;
          end;
        end;
        Inc(ARegIndex);
      end;

      FFrames[j] := AFrame;
      Inc(j);
    end;

    FFrameCount := j;
    SetLength(FFrames, FFrameCount);

    // Update info
    FInfo.FrameCount := FFrameCount;
    FInfo.Format := sdfText;
    FInfo.DurationSeconds := FFrameCount / FFrameRate;
    FInfo.Title := ExtractFileName(AFilename);

    WriteLn('SIDDump: Loaded ', FFrameCount, ' frames from text (', FInfo.DurationSeconds:0:2, ' seconds)');
    Result := True;

  finally
    ALines.Free;
  end;
end;

// Parse a hex value, returning previous value if '....' or '..'
function ParseHexOrUnchanged(const AStr: string; APrevValue: Word): Word;
var
  ATrimmed: string;
begin
  ATrimmed := Trim(AStr);
  // Handle unchanged markers
  if (ATrimmed = '') or (ATrimmed = '....') or (ATrimmed = '..') or
     (ATrimmed = '...') or (ATrimmed = '.') or (Pos('..', ATrimmed) > 0) then
    Result := APrevValue
  else
  begin
    try
      Result := StrToInt('$' + ATrimmed);
    except
      Result := APrevValue;
    end;
  end;
end;

// Load from SIDDump tabular text format
// Format: | Frame | Freq Note/Abs WF ADSR Pul | Freq Note/Abs WF ADSR Pul | Freq Note/Abs WF ADSR Pul | FCut RC Typ V |
function TSedaiSIDDumpPlayer.LoadFromSIDDumpTextFile(const AFilename: string): Boolean;
var
  ALines: TStringList;
  ALine: string;
  AFrame, APrevFrame: TSIDFrame;
  i, j, AFrameIdx: Integer;
  APos, ANextPos: Integer;
  AVoiceSection: string;
  AFilterSection: string;
  AFreqStr, AWFStr, AADSRStr, APulStr: string;
  AFreq, APulse, AADSR: Word;
  AWF, AAD, ASR: Byte;
  AFilterCut, AFilterRC: Word;
  AFilterTyp, AFilterVol: Byte;
  AIsDataLine: Boolean;
  AFrameNum: Integer;

  // Extract a substring between two | characters
  function ExtractSection(const S: string; ASectionNum: Integer): string;
  var
    AStart, AEnd, ACount: Integer;
  begin
    Result := '';
    AStart := 1;
    ACount := 0;
    while AStart <= Length(S) do
    begin
      if S[AStart] = '|' then
      begin
        Inc(ACount);
        if ACount = ASectionNum then
        begin
          AEnd := AStart + 1;
          while (AEnd <= Length(S)) and (S[AEnd] <> '|') do
            Inc(AEnd);
          Result := Trim(Copy(S, AStart + 1, AEnd - AStart - 1));
          Exit;
        end;
      end;
      Inc(AStart);
    end;
  end;

  // Parse voice section: "05D4  F-2 9D  41 14C8 800" or "05D4 (+ 0014) .. .... 80A"
  // Format: Freq Note/Abs WF ADSR Pul
  // Where Note/Abs can be: "F-2 9D" (note + absolute), "(+ 0014)" (relative), "(F-2 9D)" (in parentheses)
  procedure ParseVoiceSection(const AVoice: string; AVoiceNum: Integer;
    var AFrame: TSIDFrame; const APrevFrame: TSIDFrame);
  var
    ATokens: array of string;
    ATokenCount: Integer;
    ABaseReg: Integer;
    AVal: string;
    AFreqVal, APulseVal, AADSRVal: Word;
    AWFVal: Byte;
    i, APos: Integer;
    AInParen: Boolean;

    procedure AddToken(const S: string);
    begin
      if Trim(S) <> '' then
      begin
        Inc(ATokenCount);
        SetLength(ATokens, ATokenCount);
        ATokens[ATokenCount - 1] := Trim(S);
      end;
    end;

  begin
    ABaseReg := AVoiceNum * 7;  // Voice 0: 0-6, Voice 1: 7-13, Voice 2: 14-20

    // Initialize with previous values
    AFrame[ABaseReg] := APrevFrame[ABaseReg];         // Freq Lo
    AFrame[ABaseReg + 1] := APrevFrame[ABaseReg + 1]; // Freq Hi
    AFrame[ABaseReg + 2] := APrevFrame[ABaseReg + 2]; // PW Lo
    AFrame[ABaseReg + 3] := APrevFrame[ABaseReg + 3]; // PW Hi
    AFrame[ABaseReg + 4] := APrevFrame[ABaseReg + 4]; // Control
    AFrame[ABaseReg + 5] := APrevFrame[ABaseReg + 5]; // AD
    AFrame[ABaseReg + 6] := APrevFrame[ABaseReg + 6]; // SR

    // Tokenize: split by spaces but treat (...) as a single token
    ATokenCount := 0;
    SetLength(ATokens, 0);
    AVal := '';
    AInParen := False;

    for i := 1 to Length(AVoice) do
    begin
      if AVoice[i] = '(' then
      begin
        // Start of parenthesis - save any pending token first
        AddToken(AVal);
        AVal := '(';
        AInParen := True;
      end
      else if AVoice[i] = ')' then
      begin
        AVal := AVal + ')';
        AddToken(AVal);
        AVal := '';
        AInParen := False;
      end
      else if (AVoice[i] = ' ') and not AInParen then
      begin
        AddToken(AVal);
        AVal := '';
      end
      else
        AVal := AVal + AVoice[i];
    end;
    AddToken(AVal);

    // Now we have tokens like: ["05D4", "(+ 0014)", "..", "....", "80A"]
    // or: ["05D4", "F-2", "9D", "41", "14C8", "800"]
    // Format is: Freq, Note/Abs (1 or 2 tokens), WF, ADSR, Pul

    if ATokenCount < 1 then Exit;

    // Token 0: Frequency (always first, 4 hex or ....)
    AFreqVal := ParseHexOrUnchanged(ATokens[0], Word(APrevFrame[ABaseReg]) or (Word(APrevFrame[ABaseReg + 1]) shl 8));
    AFrame[ABaseReg] := AFreqVal and $FF;
    AFrame[ABaseReg + 1] := (AFreqVal shr 8) and $FF;

    // Find where Note/Abs ends and WF begins
    // Note/Abs is in parentheses, OR is "..." marker, OR is "X-N YY" (note + abs)
    APos := 1;  // Start after frequency

    if APos < ATokenCount then
    begin
      AVal := ATokens[APos];

      if (Length(AVal) > 0) and (AVal[1] = '(') then
      begin
        // Parenthesized - skip entire token
        Inc(APos);
      end
      else if (AVal = '...') then
      begin
        // Unchanged marker - skip it
        Inc(APos);
      end
      else if (Length(AVal) = 3) and (AVal[1] in ['A'..'G']) and
              (AVal[2] in ['-', '#']) and (AVal[3] in ['0'..'9']) then
      begin
        // Note name like "F-2" - skip it
        Inc(APos);
        // Also skip the absolute value after it (like "9D")
        if (APos < ATokenCount) then
        begin
          AVal := ATokens[APos];
          // Absolute values are 2 hex digits, typically 80-FF
          if (Length(AVal) = 2) and (AVal[1] in ['0'..'9', 'A'..'F', 'a'..'f']) and
             (AVal[2] in ['0'..'9', 'A'..'F', 'a'..'f']) then
            Inc(APos);
        end;
      end;
    end;

    // Token at APos: WF (2 hex digits or ..)
    if APos < ATokenCount then
    begin
      AWFVal := ParseHexOrUnchanged(ATokens[APos], APrevFrame[ABaseReg + 4]) and $FF;
      AFrame[ABaseReg + 4] := AWFVal;
      Inc(APos);
    end;

    // Token at APos: ADSR (4 hex digits or ....)
    // Format: XXYY where XX=AD (Attack/Decay), YY=SR (Sustain/Release)
    if APos < ATokenCount then
    begin
      AADSRVal := ParseHexOrUnchanged(ATokens[APos], (Word(APrevFrame[ABaseReg + 5]) shl 8) or Word(APrevFrame[ABaseReg + 6]));
      AFrame[ABaseReg + 5] := (AADSRVal shr 8) and $FF;  // AD = high byte
      AFrame[ABaseReg + 6] := AADSRVal and $FF;          // SR = low byte
      Inc(APos);
    end;

    // Token at APos: Pulse width (3 hex digits or ...)
    if APos < ATokenCount then
    begin
      APulseVal := ParseHexOrUnchanged(ATokens[APos], Word(APrevFrame[ABaseReg + 2]) or (Word(APrevFrame[ABaseReg + 3] and $0F) shl 8));
      AFrame[ABaseReg + 2] := APulseVal and $FF;
      AFrame[ABaseReg + 3] := (APulseVal shr 8) and $0F;
    end;

  end;

  // Parse filter section: "FCut RC Typ V" -> "0400 E1 L.. F"
  procedure ParseFilterSection(const AFilter: string; var AFrame: TSIDFrame; const APrevFrame: TSIDFrame);
  var
    AParts: TStringArray;
    AIdx: Integer;
    AVal: string;
    ACutoff: Word;
    AResCtrl, AVolMode: Byte;
    APartIdx: Integer;
  begin
    // Initialize with previous values
    AFrame[SID_FILTER_LO] := APrevFrame[SID_FILTER_LO];
    AFrame[SID_FILTER_HI] := APrevFrame[SID_FILTER_HI];
    AFrame[SID_FILTER_CTRL] := APrevFrame[SID_FILTER_CTRL];
    AFrame[SID_VOL_FILTER] := APrevFrame[SID_VOL_FILTER];

    AParts := AFilter.Split([' ']);
    APartIdx := 0;
    AIdx := 0;

    while (APartIdx < Length(AParts)) and (AIdx < 4) do
    begin
      AVal := Trim(AParts[APartIdx]);
      Inc(APartIdx);
      if AVal = '' then Continue;

      case AIdx of
        0: begin  // Filter cutoff (4 hex digits, 11-bit)
             ACutoff := ParseHexOrUnchanged(AVal,
               Word(APrevFrame[SID_FILTER_LO] and $07) or (Word(APrevFrame[SID_FILTER_HI]) shl 3));
             AFrame[SID_FILTER_LO] := ACutoff and $07;
             AFrame[SID_FILTER_HI] := (ACutoff shr 3) and $FF;
           end;
        1: begin  // Resonance + routing (2 hex digits)
             AResCtrl := ParseHexOrUnchanged(AVal, APrevFrame[SID_FILTER_CTRL]) and $FF;
             AFrame[SID_FILTER_CTRL] := AResCtrl;
           end;
        2: begin  // Filter type (L/H/B/. combinations or Low/High/Band words)
             // Parse filter type string like "L..", "LH.", ".BP", "Low", "High", "Band", etc.
             if (AVal = '...') or (AVal = '..') or (AVal = '.') then
             begin
               // Unchanged - keep previous value
             end
             else
             begin
               AVolMode := AFrame[SID_VOL_FILTER] and $0F;  // Keep volume
               // Check for word forms or letter forms
               if (Pos('L', UpperCase(AVal)) > 0) then AVolMode := AVolMode or $10;  // Lowpass
               if (Pos('B', UpperCase(AVal)) > 0) and (Pos('LOW', UpperCase(AVal)) = 0) then
                 AVolMode := AVolMode or $20;  // Bandpass (but not "Low")
               if (Pos('H', UpperCase(AVal)) > 0) and (Pos('HIGH', UpperCase(AVal)) = 0) or
                  (UpperCase(AVal) = 'HIGH') then
                 AVolMode := AVolMode or $40;  // Highpass
               AFrame[SID_VOL_FILTER] := (AFrame[SID_VOL_FILTER] and $0F) or (AVolMode and $F0);
             end;
           end;
        3: begin  // Volume (1 hex digit)
             if (AVal <> '.') and (AVal <> '..') then
             begin
               try
                 AVolMode := StrToInt('$' + AVal) and $0F;
                 AFrame[SID_VOL_FILTER] := (AFrame[SID_VOL_FILTER] and $F0) or AVolMode;
               except
               end;
             end;
           end;
      end;
      Inc(AIdx);
    end;
  end;

begin
  Result := False;

  if not FileExists(AFilename) then
  begin
    WriteLn('SIDDump: File not found: ', AFilename);
    Exit;
  end;

  ALines := TStringList.Create;
  try
    ALines.LoadFromFile(AFilename);

    // First pass: count data lines (lines starting with | and containing frame number or timestamp)
    FFrameCount := 0;
    for i := 0 to ALines.Count - 1 do
    begin
      ALine := Trim(ALines[i]);
      // Data lines start with | followed by a number or timestamp
      if (Length(ALine) > 2) and (ALine[1] = '|') then
      begin
        // Check if second section contains a number (frame number) or timestamp (0:00.00)
        AVoiceSection := ExtractSection(ALine, 1);
        if (AVoiceSection <> '') and (AVoiceSection <> 'Frame') and
           (Pos('+', AVoiceSection) = 0) and (Pos('-', AVoiceSection) = 0) then
        begin
          // Check for numeric frame number
          try
            AFrameNum := StrToInt(Trim(AVoiceSection));
            Inc(FFrameCount);
          except
            // Check for timestamp format (0:00.00)
            if (Length(AVoiceSection) >= 7) and (AVoiceSection[2] = ':') then
              Inc(FFrameCount);
          end;
        end;
      end;
    end;

    if FFrameCount = 0 then
    begin
      WriteLn('SIDDump: No valid frames found in tabular format');
      Exit;
    end;

    SetLength(FFrames, FFrameCount);
    FillChar(APrevFrame, SizeOf(TSIDFrame), 0);
    APrevFrame[SID_VOL_FILTER] := $0F;  // Default volume to max

    // Second pass: parse data
    AFrameIdx := 0;
    for i := 0 to ALines.Count - 1 do
    begin
      ALine := Trim(ALines[i]);
      if (Length(ALine) < 2) or (ALine[1] <> '|') then Continue;

      // Extract frame number or timestamp
      AVoiceSection := ExtractSection(ALine, 1);
      if (AVoiceSection = '') or (AVoiceSection = 'Frame') then Continue;
      // Skip separator lines
      if (Pos('+', AVoiceSection) > 0) or (Pos('-', AVoiceSection) > 0) then Continue;

      // Accept either numeric frame or timestamp
      try
        AFrameNum := StrToInt(Trim(AVoiceSection));
      except
        // Check for timestamp format (0:00.00)
        if (Length(AVoiceSection) >= 7) and (AVoiceSection[2] = ':') then
          AFrameNum := AFrameIdx  // Use sequential index for timestamp format
        else
          Continue;
      end;

      // Initialize frame
      FillChar(AFrame, SizeOf(TSIDFrame), 0);

      // Parse Voice 1 (section 2)
      AVoiceSection := ExtractSection(ALine, 2);
      if AVoiceSection <> '' then
        ParseVoiceSection(AVoiceSection, 0, AFrame, APrevFrame);

      // Parse Voice 2 (section 3)
      AVoiceSection := ExtractSection(ALine, 3);
      if AVoiceSection <> '' then
        ParseVoiceSection(AVoiceSection, 1, AFrame, APrevFrame);

      // Parse Voice 3 (section 4)
      AVoiceSection := ExtractSection(ALine, 4);
      if AVoiceSection <> '' then
        ParseVoiceSection(AVoiceSection, 2, AFrame, APrevFrame);

      // Parse Filter (section 5)
      AFilterSection := ExtractSection(ALine, 5);
      if AFilterSection <> '' then
        ParseFilterSection(AFilterSection, AFrame, APrevFrame);

      FFrames[AFrameIdx] := AFrame;
      APrevFrame := AFrame;
      Inc(AFrameIdx);
    end;

    FFrameCount := AFrameIdx;
    SetLength(FFrames, FFrameCount);

    // Update info
    FInfo.FrameCount := FFrameCount;
    FInfo.Format := sdfText;
    FInfo.DurationSeconds := FFrameCount / FFrameRate;
    FInfo.Title := ExtractFileName(AFilename);

    WriteLn('SIDDump: Loaded ', FFrameCount, ' frames from SIDDump format (', FInfo.DurationSeconds:0:2, ' seconds)');
    Result := True;

  finally
    ALines.Free;
  end;
end;

function TSedaiSIDDumpPlayer.LoadFromFile(const AFilename: string): Boolean;
var
  AExt: string;
  ATestFile: TextFile;
  AFirstLine: string;
  AIsSIDDumpFormat: Boolean;
begin
  AExt := LowerCase(ExtractFileExt(AFilename));

  if (AExt = '.txt') or (AExt = '.csv') or (AExt = '.hex') or (AExt = '.dmp') then
  begin
    // Auto-detect SIDDump tabular format by checking for | Frame | header
    AIsSIDDumpFormat := False;
    if FileExists(AFilename) then
    begin
      AssignFile(ATestFile, AFilename);
      try
        Reset(ATestFile);
        while not Eof(ATestFile) do
        begin
          ReadLn(ATestFile, AFirstLine);
          AFirstLine := Trim(AFirstLine);
          if AFirstLine = '' then Continue;
          // Check for SIDDump format: lines with | separators and Frame header
          if (Pos('| Frame |', AFirstLine) > 0) or
             (Pos('|Frame|', AFirstLine) > 0) or
             ((Pos('|', AFirstLine) > 0) and (Pos('Freq', AFirstLine) > 0) and (Pos('ADSR', AFirstLine) > 0)) then
          begin
            AIsSIDDumpFormat := True;
          end;
          Break;  // Only check first non-empty line
        end;
      finally
        CloseFile(ATestFile);
      end;
    end;

    if AIsSIDDumpFormat then
      Result := LoadFromSIDDumpTextFile(AFilename)
    else
      Result := LoadFromTextFile(AFilename);
  end
  else
    Result := LoadFromBinaryFile(AFilename);
end;

function TSedaiSIDDumpPlayer.LoadFromStream(AStream: TStream; AFormat: TSIDDumpFormat): Boolean;
var
  i: Integer;
begin
  Result := False;

  case AFormat of
    sdfBinaryRaw:
      begin
        FFrameCount := AStream.Size div SID_REG_COUNT;
        if FFrameCount = 0 then Exit;

        SetLength(FFrames, FFrameCount);
        for i := 0 to FFrameCount - 1 do
          AStream.Read(FFrames[i], SID_REG_COUNT);

        Result := True;
      end;
    // Other formats can be added here
  end;

  if Result then
  begin
    FInfo.FrameCount := FFrameCount;
    FInfo.Format := AFormat;
    FInfo.DurationSeconds := FFrameCount / FFrameRate;
  end;
end;

procedure TSedaiSIDDumpPlayer.Play;
begin
  if FFrameCount = 0 then Exit;

  if FState = spsPaused then
    FState := spsPlaying
  else
  begin
    FCurrentFrame := 0;
    FState := spsPlaying;
    FillChar(FPrevFrame, SizeOf(TSIDFrame), 0);
  end;

  WriteLn('SIDDump: Playing...');
end;

procedure TSedaiSIDDumpPlayer.Pause;
begin
  if FState = spsPlaying then
  begin
    FState := spsPaused;
    WriteLn('SIDDump: Paused');
  end
  else if FState = spsPaused then
  begin
    FState := spsPlaying;
    WriteLn('SIDDump: Resumed');
  end;
end;

procedure TSedaiSIDDumpPlayer.Stop;
begin
  FState := spsStopped;
  FCurrentFrame := 0;
  FSIDEvo.StopAll;
  WriteLn('SIDDump: Stopped');
end;

procedure TSedaiSIDDumpPlayer.Rewind;
begin
  FCurrentFrame := 0;
  FillChar(FPrevFrame, SizeOf(TSIDFrame), 0);
end;

procedure TSedaiSIDDumpPlayer.SetPosition(AFrame: Integer);
begin
  if AFrame < 0 then AFrame := 0;
  if AFrame >= FFrameCount then AFrame := FFrameCount - 1;
  FCurrentFrame := AFrame;
end;

function TSedaiSIDDumpPlayer.Update: Boolean;
begin
  Result := False;

  if FState <> spsPlaying then Exit;
  if FFrameCount = 0 then Exit;

  // Apply current frame
  ApplyFrame(FFrames[FCurrentFrame]);
  Result := True;

  // Advance to next frame
  Inc(FCurrentFrame);

  // Check for end
  if FCurrentFrame >= FFrameCount then
  begin
    if FLooping then
      FCurrentFrame := 0
    else
    begin
      FState := spsStopped;
      FSIDEvo.StopAll;
      WriteLn('SIDDump: Playback finished');
    end;
  end;
end;

procedure TSedaiSIDDumpPlayer.StepFrame;
begin
  if FFrameCount = 0 then Exit;
  if FCurrentFrame >= FFrameCount then Exit;

  ApplyFrame(FFrames[FCurrentFrame]);
  Inc(FCurrentFrame);
end;

function TSedaiSIDDumpPlayer.GetProgress: Single;
begin
  if FFrameCount = 0 then
    Result := 0.0
  else
    Result := FCurrentFrame / FFrameCount;
end;

function TSedaiSIDDumpPlayer.GetPositionSeconds: Single;
begin
  Result := FCurrentFrame / FFrameRate;
end;

function TSedaiSIDDumpPlayer.GetDurationSeconds: Single;
begin
  Result := FFrameCount / FFrameRate;
end;

function TSedaiSIDDumpPlayer.IsPlaying: Boolean;
begin
  Result := FState = spsPlaying;
end;

procedure TSedaiSIDDumpPlayer.PrintFrameDebug(AFrameNum: Integer);
var
  AFrame: TSIDFrame;
  AFreq1, AFreq2, AFreq3: Word;
  APW1, APW2, APW3: Word;
  ACutoff: Word;
begin
  if (AFrameNum < 0) or (AFrameNum >= FFrameCount) then
  begin
    WriteLn('Frame ', AFrameNum, ' out of range (0-', FFrameCount - 1, ')');
    Exit;
  end;

  AFrame := FFrames[AFrameNum];

  // Extract values
  AFreq1 := Word(AFrame[0]) or (Word(AFrame[1]) shl 8);
  APW1 := Word(AFrame[2]) or (Word(AFrame[3] and $0F) shl 8);
  AFreq2 := Word(AFrame[7]) or (Word(AFrame[8]) shl 8);
  APW2 := Word(AFrame[9]) or (Word(AFrame[10] and $0F) shl 8);
  AFreq3 := Word(AFrame[14]) or (Word(AFrame[15]) shl 8);
  APW3 := Word(AFrame[16]) or (Word(AFrame[17] and $0F) shl 8);
  ACutoff := (AFrame[21] and $07) or (Word(AFrame[22]) shl 3);

  WriteLn('=== Frame ', AFrameNum, ' ===');
  WriteLn('Voice 1: Freq=$', IntToHex(AFreq1, 4), ' (', SIDFreqToHz(AFreq1):0:1, 'Hz)',
          ' PW=$', IntToHex(APW1, 3),
          ' WF=$', IntToHex(AFrame[4], 2),
          ' AD=$', IntToHex(AFrame[5], 2),
          ' SR=$', IntToHex(AFrame[6], 2));
  WriteLn('Voice 2: Freq=$', IntToHex(AFreq2, 4), ' (', SIDFreqToHz(AFreq2):0:1, 'Hz)',
          ' PW=$', IntToHex(APW2, 3),
          ' WF=$', IntToHex(AFrame[11], 2),
          ' AD=$', IntToHex(AFrame[12], 2),
          ' SR=$', IntToHex(AFrame[13], 2));
  WriteLn('Voice 3: Freq=$', IntToHex(AFreq3, 4), ' (', SIDFreqToHz(AFreq3):0:1, 'Hz)',
          ' PW=$', IntToHex(APW3, 3),
          ' WF=$', IntToHex(AFrame[18], 2),
          ' AD=$', IntToHex(AFrame[19], 2),
          ' SR=$', IntToHex(AFrame[20], 2));
  WriteLn('Filter: Cutoff=$', IntToHex(ACutoff, 4),
          ' ResCtrl=$', IntToHex(AFrame[23], 2),
          ' VolMode=$', IntToHex(AFrame[24], 2));
  WriteLn;
end;

procedure TSedaiSIDDumpPlayer.ExportDebugLog(const AFilename: string; AMaxFrames: Integer);
var
  AFile: TextFile;
  AFrame: TSIDFrame;
  i: Integer;
  AFreq1, AFreq2, AFreq3: Word;
  APW1, APW2, APW3: Word;
  ACutoff: Word;
  ACount: Integer;
begin
  if FFrameCount = 0 then
  begin
    WriteLn('No frames to export');
    Exit;
  end;

  ACount := AMaxFrames;
  if ACount > FFrameCount then ACount := FFrameCount;

  AssignFile(AFile, AFilename);
  try
    Rewrite(AFile);

    WriteLn(AFile, 'SIDDump Debug Log - ', FFrameCount, ' total frames, showing first ', ACount);
    WriteLn(AFile, 'Frame Rate: ', FFrameRate, ' Hz');
    WriteLn(AFile, '');
    WriteLn(AFile, 'Legend: Freq=16-bit frequency reg, PW=12-bit pulse width, WF=waveform/control byte');
    WriteLn(AFile, '        WF bit 0=Gate, bit 1=Sync, bit 2=Ring, bit 3=Test');
    WriteLn(AFile, '        WF bit 4=Triangle, bit 5=Saw, bit 6=Pulse, bit 7=Noise');
    WriteLn(AFile, '        AD=Attack/Decay, SR=Sustain/Release');
    WriteLn(AFile, '');
    WriteLn(AFile, 'Frame | V1 Freq  Hz     PW   WF AD SR | V2 Freq  Hz     PW   WF AD SR | V3 Freq  Hz     PW   WF AD SR | FCut ResC VolM');
    WriteLn(AFile, '------+-------------------------------+-------------------------------+-------------------------------+----------------');

    for i := 0 to ACount - 1 do
    begin
      AFrame := FFrames[i];

      // Extract values
      AFreq1 := Word(AFrame[0]) or (Word(AFrame[1]) shl 8);
      APW1 := Word(AFrame[2]) or (Word(AFrame[3] and $0F) shl 8);
      AFreq2 := Word(AFrame[7]) or (Word(AFrame[8]) shl 8);
      APW2 := Word(AFrame[9]) or (Word(AFrame[10] and $0F) shl 8);
      AFreq3 := Word(AFrame[14]) or (Word(AFrame[15]) shl 8);
      APW3 := Word(AFrame[16]) or (Word(AFrame[17] and $0F) shl 8);
      ACutoff := (AFrame[21] and $07) or (Word(AFrame[22]) shl 3);

      WriteLn(AFile, Format('%5d | %4s %7.1f %4s %2s %2s %2s | %4s %7.1f %4s %2s %2s %2s | %4s %7.1f %4s %2s %2s %2s | %4s %2s   %2s',
        [i,
         IntToHex(AFreq1, 4), SIDFreqToHz(AFreq1), IntToHex(APW1, 3),
         IntToHex(AFrame[4], 2), IntToHex(AFrame[5], 2), IntToHex(AFrame[6], 2),
         IntToHex(AFreq2, 4), SIDFreqToHz(AFreq2), IntToHex(APW2, 3),
         IntToHex(AFrame[11], 2), IntToHex(AFrame[12], 2), IntToHex(AFrame[13], 2),
         IntToHex(AFreq3, 4), SIDFreqToHz(AFreq3), IntToHex(APW3, 3),
         IntToHex(AFrame[18], 2), IntToHex(AFrame[19], 2), IntToHex(AFrame[20], 2),
         IntToHex(ACutoff, 4), IntToHex(AFrame[23], 2), IntToHex(AFrame[24], 2)]));
    end;

    WriteLn(AFile, '');
    WriteLn(AFile, 'Gate analysis (first ', ACount, ' frames):');
    WriteLn(AFile, '  V1 gates ON: ', CountGates(0, ACount), ' frames');
    WriteLn(AFile, '  V2 gates ON: ', CountGates(1, ACount), ' frames');
    WriteLn(AFile, '  V3 gates ON: ', CountGates(2, ACount), ' frames');

    CloseFile(AFile);
    WriteLn('Debug log exported to: ', AFilename);

  except
    on E: Exception do
      WriteLn('Error writing debug log: ', E.Message);
  end;
end;

function TSedaiSIDDumpPlayer.CountGates(AVoice: Integer; AMaxFrames: Integer): Integer;
var
  i, ABaseReg: Integer;
begin
  Result := 0;
  ABaseReg := AVoice * 7 + 4;  // Control register offset
  if AMaxFrames > FFrameCount then AMaxFrames := FFrameCount;

  for i := 0 to AMaxFrames - 1 do
  begin
    if (FFrames[i][ABaseReg] and SID_CTRL_GATE) <> 0 then
      Inc(Result);
  end;
end;

procedure TSedaiSIDDumpPlayer.SetVoiceMute(AVoice: Integer; AMuted: Boolean);
begin
  if Assigned(FSIDEvo) then
    FSIDEvo.SetVoiceMute(AVoice, AMuted);
end;

function TSedaiSIDDumpPlayer.IsVoiceMuted(AVoice: Integer): Boolean;
begin
  if Assigned(FSIDEvo) then
    Result := FSIDEvo.IsVoiceMuted(AVoice)
  else
    Result := False;
end;

procedure TSedaiSIDDumpPlayer.ToggleVoiceMute(AVoice: Integer);
begin
  SetVoiceMute(AVoice, not IsVoiceMuted(AVoice));
end;

// Utility function stub - actual conversion requires external tool
function ConvertSIDToDump(const ASIDFile, ADumpFile: string): Boolean;
begin
  WriteLn('NOTE: To convert .SID files to dump format, use the SIDDump tool:');
  WriteLn('  siddump -o ', ADumpFile, ' ', ASIDFile);
  WriteLn('Download from: https://csdb.dk/release/?id=192079');
  Result := False;
end;

end.
