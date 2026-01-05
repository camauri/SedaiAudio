{*
 * Sedai Audio Foundation - Audio File Reader
 *
 * TSedaiAudioFileReader provides unified interface for reading
 * WAV, OGG Vorbis, and FLAC audio files.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAudioFileReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioBuffer;

type
  // Supported audio file formats
  TAudioFileFormat = (
    affUnknown,
    affWAV,        // Microsoft WAV (PCM, IEEE Float)
    affOGG,        // OGG Vorbis
    affFLAC,       // Free Lossless Audio Codec
    affAIFF,       // Apple AIFF
    affMP3         // MPEG Layer 3 (decode only)
  );

  // WAV format codes
  TWAVFormatCode = (
    wfPCM = 1,
    wfIEEEFloat = 3,
    wfALaw = 6,
    wfMuLaw = 7,
    wfExtensible = $FFFE
  );

  // File info structure
  TAudioFileInfo = record
    Format: TAudioFileFormat;
    SampleRate: Integer;
    Channels: Integer;
    BitsPerSample: Integer;
    SampleCount: Int64;
    Duration: Double;           // Seconds
    Bitrate: Integer;          // For compressed formats
    IsVBR: Boolean;            // Variable bitrate
    Title: string;
    Artist: string;
    Album: string;
    Comment: string;
  end;

  // Read callback for streaming
  TAudioReadCallback = procedure(ABuffer: PSingle; AFrames: Integer;
                                  AProgress: Double) of object;

  { TSedaiAudioFileReader }
  // Unified audio file reader
  TSedaiAudioFileReader = class
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FInfo: TAudioFileInfo;
    FIsOpen: Boolean;
    FPosition: Int64;          // Current sample position
    FLastError: string;

    // WAV specific
    FWAVDataOffset: Int64;
    FWAVDataSize: Int64;
    FWAVFormatCode: Word;
    FWAVBlockAlign: Word;
    FWAVBytesPerSample: Integer;

    // Internal buffers
    FDecodeBuffer: array of Byte;
    FConvertBuffer: array of Single;

    function DetectFormat(AStream: TStream): TAudioFileFormat;
    function OpenWAV: Boolean;
    function ReadWAVSamples(ABuffer: PSingle; AFrameCount: Integer): Integer;

    // Sample conversion
    procedure ConvertPCM8ToFloat(ASrc: PByte; ADst: PSingle; ACount: Integer);
    procedure ConvertPCM16ToFloat(ASrc: PSmallInt; ADst: PSingle; ACount: Integer);
    procedure ConvertPCM24ToFloat(ASrc: PByte; ADst: PSingle; ACount: Integer);
    procedure ConvertPCM32ToFloat(ASrc: PLongInt; ADst: PSingle; ACount: Integer);
    procedure ConvertFloat32ToFloat(ASrc: PSingle; ADst: PSingle; ACount: Integer);
    procedure ConvertFloat64ToFloat(ASrc: PDouble; ADst: PSingle; ACount: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    // Open file
    function OpenFile(const AFileName: string): Boolean;
    function OpenStream(AStream: TStream; AOwnsStream: Boolean = False): Boolean;

    // Close file
    procedure Close;

    // Read samples (interleaved stereo output)
    function ReadSamples(ABuffer: PSingle; AFrameCount: Integer): Integer;

    // Read entire file into buffer
    function ReadAll(out ABuffer: TSedaiAudioBuffer): Boolean;

    // Seeking
    function Seek(ASamplePosition: Int64): Boolean;
    function SeekSeconds(ASeconds: Double): Boolean;

    // Get file info without fully opening
    class function GetFileInfo(const AFileName: string;
                               out AInfo: TAudioFileInfo): Boolean;

    // Format detection
    class function DetectFileFormat(const AFileName: string): TAudioFileFormat;

    // Properties
    property Info: TAudioFileInfo read FInfo;
    property IsOpen: Boolean read FIsOpen;
    property Position: Int64 read FPosition;
    property LastError: string read FLastError;
  end;

  { TSedaiWAVChunkReader }
  // Helper for reading WAV chunks
  TSedaiWAVChunkReader = class
  private
    FStream: TStream;
  public
    constructor Create(AStream: TStream);

    function ReadChunkID: string;
    function ReadDWord: LongWord;
    function ReadWord: Word;
    function SkipBytes(ACount: Integer): Boolean;
    function FindChunk(const AID: string; out ASize: LongWord): Boolean;
  end;

implementation

{ TSedaiWAVChunkReader }

constructor TSedaiWAVChunkReader.Create(AStream: TStream);
begin
  FStream := AStream;
end;

function TSedaiWAVChunkReader.ReadChunkID: string;
var
  ID: array[0..3] of AnsiChar;
begin
  FStream.ReadBuffer(ID, 4);
  Result := string(ID);
end;

function TSedaiWAVChunkReader.ReadDWord: LongWord;
begin
  FStream.ReadBuffer(Result, 4);
  // WAV is little-endian
  Result := LEtoN(Result);
end;

function TSedaiWAVChunkReader.ReadWord: Word;
begin
  FStream.ReadBuffer(Result, 2);
  Result := LEtoN(Result);
end;

function TSedaiWAVChunkReader.SkipBytes(ACount: Integer): Boolean;
begin
  Result := FStream.Seek(ACount, soCurrent) >= 0;
end;

function TSedaiWAVChunkReader.FindChunk(const AID: string; out ASize: LongWord): Boolean;
var
  ChunkID: string;
  ChunkSize: LongWord;
begin
  Result := False;

  while FStream.Position < FStream.Size - 8 do
  begin
    ChunkID := ReadChunkID;
    ChunkSize := ReadDWord;

    if ChunkID = AID then
    begin
      ASize := ChunkSize;
      Result := True;
      Exit;
    end;

    // Skip to next chunk (pad to even boundary)
    if ChunkSize mod 2 = 1 then
      Inc(ChunkSize);
    FStream.Seek(ChunkSize, soCurrent);
  end;
end;

{ TSedaiAudioFileReader }

constructor TSedaiAudioFileReader.Create;
begin
  FStream := nil;
  FOwnsStream := False;
  FIsOpen := False;
  FPosition := 0;
  FLastError := '';

  FillChar(FInfo, SizeOf(FInfo), 0);
end;

destructor TSedaiAudioFileReader.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TSedaiAudioFileReader.DetectFormat(AStream: TStream): TAudioFileFormat;
var
  Header: array[0..11] of Byte;
  OldPos: Int64;
begin
  Result := affUnknown;
  OldPos := AStream.Position;

  try
    if AStream.Read(Header, 12) < 12 then
      Exit;

    // Check WAV (RIFF....WAVE)
    if (Header[0] = Ord('R')) and (Header[1] = Ord('I')) and
       (Header[2] = Ord('F')) and (Header[3] = Ord('F')) and
       (Header[8] = Ord('W')) and (Header[9] = Ord('A')) and
       (Header[10] = Ord('V')) and (Header[11] = Ord('E')) then
    begin
      Result := affWAV;
      Exit;
    end;

    // Check OGG (OggS)
    if (Header[0] = Ord('O')) and (Header[1] = Ord('g')) and
       (Header[2] = Ord('g')) and (Header[3] = Ord('S')) then
    begin
      Result := affOGG;
      Exit;
    end;

    // Check FLAC (fLaC)
    if (Header[0] = Ord('f')) and (Header[1] = Ord('L')) and
       (Header[2] = Ord('a')) and (Header[3] = Ord('C')) then
    begin
      Result := affFLAC;
      Exit;
    end;

    // Check AIFF (FORM....AIFF)
    if (Header[0] = Ord('F')) and (Header[1] = Ord('O')) and
       (Header[2] = Ord('R')) and (Header[3] = Ord('M')) and
       (Header[8] = Ord('A')) and (Header[9] = Ord('I')) and
       (Header[10] = Ord('F')) and (Header[11] = Ord('F')) then
    begin
      Result := affAIFF;
      Exit;
    end;

    // Check MP3 (ID3 tag or frame sync)
    if ((Header[0] = Ord('I')) and (Header[1] = Ord('D')) and (Header[2] = Ord('3'))) or
       ((Header[0] = $FF) and ((Header[1] and $E0) = $E0)) then
    begin
      Result := affMP3;
      Exit;
    end;

  finally
    AStream.Position := OldPos;
  end;
end;

function TSedaiAudioFileReader.OpenWAV: Boolean;
var
  Reader: TSedaiWAVChunkReader;
  ChunkSize: LongWord;
  FmtSize: LongWord;
  ValidBits: Word;
begin
  Result := False;
  Reader := TSedaiWAVChunkReader.Create(FStream);

  try
    // Skip RIFF header (already validated)
    FStream.Position := 12;

    // Find fmt chunk
    if not Reader.FindChunk('fmt ', FmtSize) then
    begin
      FLastError := 'WAV: fmt chunk not found';
      Exit;
    end;

    // Read format
    FWAVFormatCode := Reader.ReadWord;
    FInfo.Channels := Reader.ReadWord;
    FInfo.SampleRate := Reader.ReadDWord;
    Reader.ReadDWord;  // Byte rate
    FWAVBlockAlign := Reader.ReadWord;
    FInfo.BitsPerSample := Reader.ReadWord;

    // Handle extended format
    if (FWAVFormatCode = Word(wfExtensible)) and (FmtSize >= 40) then
    begin
      Reader.ReadWord;  // cbSize
      ValidBits := Reader.ReadWord;
      Reader.ReadDWord; // Channel mask
      // Read actual format GUID (first 2 bytes are format code)
      FWAVFormatCode := Reader.ReadWord;
      if ValidBits > 0 then
        FInfo.BitsPerSample := ValidBits;
    end;

    // Validate format
    if not (FWAVFormatCode in [Word(wfPCM), Word(wfIEEEFloat)]) then
    begin
      FLastError := Format('WAV: Unsupported format code %d', [FWAVFormatCode]);
      Exit;
    end;

    FWAVBytesPerSample := FInfo.BitsPerSample div 8;

    // Find data chunk
    FStream.Position := 12;  // Reset to after RIFF header
    if not Reader.FindChunk('data', ChunkSize) then
    begin
      FLastError := 'WAV: data chunk not found';
      Exit;
    end;

    FWAVDataOffset := FStream.Position;
    FWAVDataSize := ChunkSize;

    // Calculate sample count
    FInfo.SampleCount := FWAVDataSize div (FInfo.Channels * FWAVBytesPerSample);
    FInfo.Duration := FInfo.SampleCount / FInfo.SampleRate;
    FInfo.Format := affWAV;
    FInfo.Bitrate := FInfo.SampleRate * FInfo.Channels * FInfo.BitsPerSample;
    FInfo.IsVBR := False;

    // Allocate decode buffer
    SetLength(FDecodeBuffer, 65536);
    SetLength(FConvertBuffer, 32768);

    Result := True;

  finally
    Reader.Free;
  end;
end;

function TSedaiAudioFileReader.ReadWAVSamples(ABuffer: PSingle;
  AFrameCount: Integer): Integer;
var
  BytesToRead: Integer;
  BytesRead: Integer;
  SamplesToConvert: Integer;
  FramesRemaining: Int64;
begin
  Result := 0;

  // Check bounds
  FramesRemaining := FInfo.SampleCount - FPosition;
  if AFrameCount > FramesRemaining then
    AFrameCount := FramesRemaining;

  if AFrameCount <= 0 then
    Exit;

  // Calculate bytes needed
  BytesToRead := AFrameCount * FInfo.Channels * FWAVBytesPerSample;

  // Ensure buffer size
  if Length(FDecodeBuffer) < BytesToRead then
    SetLength(FDecodeBuffer, BytesToRead);

  // Read raw data
  BytesRead := FStream.Read(FDecodeBuffer[0], BytesToRead);
  if BytesRead <= 0 then
    Exit;

  SamplesToConvert := BytesRead div FWAVBytesPerSample;
  Result := SamplesToConvert div FInfo.Channels;

  // Convert to float based on format
  if FWAVFormatCode = Word(wfIEEEFloat) then
  begin
    if FInfo.BitsPerSample = 32 then
      ConvertFloat32ToFloat(@FDecodeBuffer[0], ABuffer, SamplesToConvert)
    else if FInfo.BitsPerSample = 64 then
      ConvertFloat64ToFloat(@FDecodeBuffer[0], ABuffer, SamplesToConvert);
  end
  else // PCM
  begin
    case FInfo.BitsPerSample of
      8:  ConvertPCM8ToFloat(@FDecodeBuffer[0], ABuffer, SamplesToConvert);
      16: ConvertPCM16ToFloat(@FDecodeBuffer[0], ABuffer, SamplesToConvert);
      24: ConvertPCM24ToFloat(@FDecodeBuffer[0], ABuffer, SamplesToConvert);
      32: ConvertPCM32ToFloat(@FDecodeBuffer[0], ABuffer, SamplesToConvert);
    end;
  end;

  Inc(FPosition, Result);
end;

procedure TSedaiAudioFileReader.ConvertPCM8ToFloat(ASrc: PByte; ADst: PSingle;
  ACount: Integer);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
  begin
    // 8-bit WAV is unsigned, 128 = silence
    ADst[I] := (ASrc[I] - 128) / 128.0;
  end;
end;

procedure TSedaiAudioFileReader.ConvertPCM16ToFloat(ASrc: PSmallInt; ADst: PSingle;
  ACount: Integer);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    ADst[I] := ASrc[I] / 32768.0;
end;

procedure TSedaiAudioFileReader.ConvertPCM24ToFloat(ASrc: PByte; ADst: PSingle;
  ACount: Integer);
var
  I: Integer;
  Sample: LongInt;
begin
  for I := 0 to ACount - 1 do
  begin
    // 24-bit little-endian to 32-bit signed
    Sample := ASrc[I * 3] or (ASrc[I * 3 + 1] shl 8) or (ASrc[I * 3 + 2] shl 16);
    // Sign extend
    if (Sample and $800000) <> 0 then
      Sample := Sample or $FF000000;
    ADst[I] := Sample / 8388608.0;
  end;
end;

procedure TSedaiAudioFileReader.ConvertPCM32ToFloat(ASrc: PLongInt; ADst: PSingle;
  ACount: Integer);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    ADst[I] := ASrc[I] / 2147483648.0;
end;

procedure TSedaiAudioFileReader.ConvertFloat32ToFloat(ASrc, ADst: PSingle;
  ACount: Integer);
begin
  Move(ASrc^, ADst^, ACount * SizeOf(Single));
end;

procedure TSedaiAudioFileReader.ConvertFloat64ToFloat(ASrc: PDouble; ADst: PSingle;
  ACount: Integer);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    ADst[I] := ASrc[I];
end;

function TSedaiAudioFileReader.OpenFile(const AFileName: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  Close;

  try
    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    Result := OpenStream(FS, True);

    if not Result then
      FS.Free;

  except
    on E: Exception do
    begin
      FLastError := E.Message;
      Result := False;
    end;
  end;
end;

function TSedaiAudioFileReader.OpenStream(AStream: TStream;
  AOwnsStream: Boolean): Boolean;
begin
  Result := False;
  Close;

  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FPosition := 0;

  // Detect format
  FInfo.Format := DetectFormat(AStream);

  case FInfo.Format of
    affWAV:
      Result := OpenWAV;
    affOGG:
      begin
        // TODO: Implement OGG Vorbis decoding
        FLastError := 'OGG format not yet implemented';
        Result := False;
      end;
    affFLAC:
      begin
        // TODO: Implement FLAC decoding
        FLastError := 'FLAC format not yet implemented';
        Result := False;
      end;
    affAIFF:
      begin
        // TODO: Implement AIFF decoding
        FLastError := 'AIFF format not yet implemented';
        Result := False;
      end;
    affMP3:
      begin
        // TODO: Implement MP3 decoding
        FLastError := 'MP3 format not yet implemented';
        Result := False;
      end;
  else
    FLastError := 'Unknown audio format';
    Result := False;
  end;

  FIsOpen := Result;

  if not Result then
  begin
    if FOwnsStream then
      FStream.Free;
    FStream := nil;
  end;
end;

procedure TSedaiAudioFileReader.Close;
begin
  if Assigned(FStream) and FOwnsStream then
    FStream.Free;

  FStream := nil;
  FOwnsStream := False;
  FIsOpen := False;
  FPosition := 0;

  SetLength(FDecodeBuffer, 0);
  SetLength(FConvertBuffer, 0);

  FillChar(FInfo, SizeOf(FInfo), 0);
end;

function TSedaiAudioFileReader.ReadSamples(ABuffer: PSingle;
  AFrameCount: Integer): Integer;
begin
  Result := 0;

  if not FIsOpen then
  begin
    FLastError := 'File not open';
    Exit;
  end;

  case FInfo.Format of
    affWAV: Result := ReadWAVSamples(ABuffer, AFrameCount);
  end;
end;

function TSedaiAudioFileReader.ReadAll(out ABuffer: TSedaiAudioBuffer): Boolean;
var
  TempBuffer: array of Single;
  FramesRead: Integer;
begin
  Result := False;
  ABuffer := nil;

  if not FIsOpen then
  begin
    FLastError := 'File not open';
    Exit;
  end;

  // Seek to beginning
  Seek(0);

  // Allocate buffer
  ABuffer := TSedaiAudioBuffer.Create;
  ABuffer.SetFormat(FInfo.SampleRate, FInfo.Channels);
  ABuffer.SetSize(FInfo.SampleCount);

  // Allocate temp buffer
  SetLength(TempBuffer, FInfo.SampleCount * FInfo.Channels);

  // Read all samples
  FramesRead := ReadSamples(@TempBuffer[0], FInfo.SampleCount);

  if FramesRead <> FInfo.SampleCount then
  begin
    ABuffer.Free;
    ABuffer := nil;
    FLastError := 'Failed to read all samples';
    Exit;
  end;

  // Copy to buffer (deinterleave if needed)
  ABuffer.WriteInterleaved(@TempBuffer[0], 0, FramesRead);

  Result := True;
end;

function TSedaiAudioFileReader.Seek(ASamplePosition: Int64): Boolean;
var
  ByteOffset: Int64;
begin
  Result := False;

  if not FIsOpen then
  begin
    FLastError := 'File not open';
    Exit;
  end;

  if (ASamplePosition < 0) or (ASamplePosition >= FInfo.SampleCount) then
  begin
    FLastError := 'Seek position out of range';
    Exit;
  end;

  case FInfo.Format of
    affWAV:
      begin
        ByteOffset := FWAVDataOffset + ASamplePosition * FInfo.Channels * FWAVBytesPerSample;
        FStream.Position := ByteOffset;
        FPosition := ASamplePosition;
        Result := True;
      end;
  end;
end;

function TSedaiAudioFileReader.SeekSeconds(ASeconds: Double): Boolean;
var
  SamplePos: Int64;
begin
  SamplePos := Round(ASeconds * FInfo.SampleRate);
  Result := Seek(SamplePos);
end;

class function TSedaiAudioFileReader.GetFileInfo(const AFileName: string;
  out AInfo: TAudioFileInfo): Boolean;
var
  Reader: TSedaiAudioFileReader;
begin
  FillChar(AInfo, SizeOf(AInfo), 0);

  Reader := TSedaiAudioFileReader.Create;
  try
    Result := Reader.OpenFile(AFileName);
    if Result then
      AInfo := Reader.Info;
  finally
    Reader.Free;
  end;
end;

class function TSedaiAudioFileReader.DetectFileFormat(
  const AFileName: string): TAudioFileFormat;
var
  FS: TFileStream;
  Reader: TSedaiAudioFileReader;
begin
  Result := affUnknown;

  try
    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      Reader := TSedaiAudioFileReader.Create;
      try
        Result := Reader.DetectFormat(FS);
      finally
        Reader.Free;
      end;
    finally
      FS.Free;
    end;
  except
    Result := affUnknown;
  end;
end;

end.
