{*
 * Sedai Audio Foundation - Audio File Writer
 *
 * TSedaiAudioFileWriter provides unified interface for writing
 * WAV and AIFF audio files (OGG Vorbis and FLAC encoding are not yet
 * implemented). AIFF is written as big-endian PCM (16/24/32-bit) with an
 * 80-bit IEEE 754 extended sample rate, the exact inverse of the AIFF reader.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAudioFileWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioBuffer, SedaiFLACEncoder;

type
  // Export format settings
  TAudioExportFormat = (
    aefWAV16,         // WAV 16-bit PCM
    aefWAV24,         // WAV 24-bit PCM
    aefWAV32,         // WAV 32-bit PCM
    aefWAVFloat,      // WAV 32-bit IEEE Float
    aefOGG,           // OGG Vorbis
    aefFLAC,          // FLAC (16-bit, lossless)
    aefAIFF16,        // AIFF 16-bit PCM (big-endian)
    aefAIFF24,        // AIFF 24-bit PCM (big-endian)
    aefAIFF32,        // AIFF 32-bit PCM (big-endian)
    aefFLAC24         // FLAC (24-bit, lossless) — appended to keep prior ordinals
  );

  // Dithering type
  TDitherType = (
    dtNone,           // No dithering
    dtRPDF,           // Rectangular probability density function
    dtTPDF,           // Triangular probability density function
    dtNoiseShaped     // Noise-shaped dithering
  );

  // Export settings
  TAudioExportSettings = record
    Format: TAudioExportFormat;
    SampleRate: Integer;
    Channels: Integer;
    Quality: Single;       // 0.0-1.0 for lossy formats
    DitherType: TDitherType;
    Normalize: Boolean;
    NormalizePeak: Single; // Target peak in dB (e.g., -0.1)

    // Metadata
    Title: string;
    Artist: string;
    Album: string;
    Year: Integer;
    TrackNumber: Integer;
    Comment: string;
  end;

  // Progress callback
  TAudioWriteProgressCallback = procedure(AProgress: Double;
                                           var ACancel: Boolean) of object;

  { TSedaiAudioFileWriter }
  // Unified audio file writer
  TSedaiAudioFileWriter = class
  private
    FStream: TStream;
    FOwnsStream: Boolean;
    FSettings: TAudioExportSettings;
    FIsOpen: Boolean;
    FSamplesWritten: Int64;
    FLastError: string;

    // WAV specific
    FWAVDataOffset: Int64;
    FWAVDataSize: Int64;

    // AIFF specific (stream offsets of placeholder fields, patched on close)
    FAIFFCommFramesOffset: Int64;    // numSampleFrames in the COMM chunk
    FAIFFSsndSizeOffset: Int64;      // chunk-size field of the SSND chunk

    // Dithering state
    FDitherState: array[0..1] of Single;
    FNoiseShapeBuffer: array[0..1] of Single;

    // Internal buffers
    FEncodeBuffer: array of Byte;

    // FLAC encoding (nil unless the export format is aefFLAC)
    FFLACEnc: TSedaiFLACEncoder;

    procedure InitDithering;
    function GetDitherSample: Single;
    function GetNoiseShapedDither(AChannel: Integer): Single;

    // WAV writing
    procedure WriteWAVHeader;
    procedure FinalizeWAV;
    function WriteWAVSamples(ABuffer: PSingle; AFrameCount: Integer): Boolean;

    // AIFF writing (big-endian)
    procedure WriteBEWord(AValue: Word);
    procedure WriteBEDWord(AValue: LongWord);
    procedure WriteAIFFExtended(AValue: Double);
    procedure WriteAIFFHeader;
    procedure FinalizeAIFF;
    function WriteAIFFSamples(ABuffer: PSingle; AFrameCount: Integer): Boolean;

    // Sample conversion with dithering
    procedure ConvertFloatToPCM8(ASrc: PSingle; ADst: PByte; ACount: Integer);
    procedure ConvertFloatToPCM16(ASrc: PSingle; ADst: PSmallInt; ACount: Integer);
    procedure ConvertFloatToPCM24(ASrc: PSingle; ADst: PByte; ACount: Integer);
    procedure ConvertFloatToPCM32(ASrc: PSingle; ADst: PLongInt; ACount: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    // Default settings
    class function GetDefaultSettings(AFormat: TAudioExportFormat): TAudioExportSettings;

    // Create file
    function CreateFile(const AFileName: string;
                        const ASettings: TAudioExportSettings): Boolean;
    function CreateStream(AStream: TStream; AOwnsStream: Boolean;
                          const ASettings: TAudioExportSettings): Boolean;

    // Close and finalize file
    procedure Close;

    // Write samples (interleaved float input)
    function WriteSamples(ABuffer: PSingle; AFrameCount: Integer): Boolean;

    // Write entire buffer
    function WriteBuffer(ABuffer: TSedaiAudioBuffer;
                         ACallback: TAudioWriteProgressCallback = nil): Boolean;

    // Quick export functions
    class function ExportToWAV(ABuffer: TSedaiAudioBuffer;
                               const AFileName: string;
                               ABitDepth: Integer = 16): Boolean;

    class function ExportToWAVFloat(ABuffer: TSedaiAudioBuffer;
                                    const AFileName: string): Boolean;

    class function ExportToAIFF(ABuffer: TSedaiAudioBuffer;
                                const AFileName: string;
                                ABitDepth: Integer = 16): Boolean;

    // Normalize buffer
    class procedure NormalizeBuffer(ABuffer: TSedaiAudioBuffer;
                                    ATargetPeakDB: Single = -0.1);

    // Properties
    property Settings: TAudioExportSettings read FSettings;
    property IsOpen: Boolean read FIsOpen;
    property SamplesWritten: Int64 read FSamplesWritten;
    property LastError: string read FLastError;
  end;

implementation

{ TSedaiAudioFileWriter }

constructor TSedaiAudioFileWriter.Create;
begin
  FStream := nil;
  FOwnsStream := False;
  FIsOpen := False;
  FSamplesWritten := 0;
  FLastError := '';

  FillChar(FSettings, SizeOf(FSettings), 0);
  InitDithering;
end;

destructor TSedaiAudioFileWriter.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TSedaiAudioFileWriter.InitDithering;
begin
  FDitherState[0] := 0;
  FDitherState[1] := 0;
  FNoiseShapeBuffer[0] := 0;
  FNoiseShapeBuffer[1] := 0;
end;

function TSedaiAudioFileWriter.GetDitherSample: Single;
begin
  case FSettings.DitherType of
    dtRPDF:
      Result := (Random - 0.5) / 32768.0;
    dtTPDF:
      Result := ((Random - 0.5) + (Random - 0.5)) / 32768.0;
  else
    Result := 0;
  end;
end;

function TSedaiAudioFileWriter.GetNoiseShapedDither(AChannel: Integer): Single;
var
  Dither: Single;
begin
  // Simple first-order noise shaping
  Dither := ((Random - 0.5) + (Random - 0.5)) / 32768.0;
  Result := Dither - 0.5 * FNoiseShapeBuffer[AChannel];
  FNoiseShapeBuffer[AChannel] := Dither;
end;

class function TSedaiAudioFileWriter.GetDefaultSettings(
  AFormat: TAudioExportFormat): TAudioExportSettings;
begin
  FillChar(Result, SizeOf(Result), 0);

  Result.Format := AFormat;
  Result.SampleRate := 44100;
  Result.Channels := 2;
  Result.Quality := 0.8;
  Result.DitherType := dtTPDF;
  Result.Normalize := False;
  Result.NormalizePeak := -0.1;
end;

procedure TSedaiAudioFileWriter.WriteWAVHeader;
var
  FormatCode: Word;
  ByteRate: LongWord;
  BlockAlign: Word;
  BitsPerSample: Word;
  RIFFSize: LongWord;
  DataSize: LongWord;
begin
  // Determine format parameters
  case FSettings.Format of
    aefWAV16:
      begin
        FormatCode := 1;  // PCM
        BitsPerSample := 16;
      end;
    aefWAV24:
      begin
        FormatCode := 1;  // PCM
        BitsPerSample := 24;
      end;
    aefWAV32:
      begin
        FormatCode := 1;  // PCM
        BitsPerSample := 32;
      end;
    aefWAVFloat:
      begin
        FormatCode := 3;  // IEEE Float
        BitsPerSample := 32;
      end;
  else
    Exit;  // Unsupported format
  end;

  BlockAlign := FSettings.Channels * (BitsPerSample div 8);
  ByteRate := FSettings.SampleRate * BlockAlign;

  // We'll update these when closing
  RIFFSize := 0;
  DataSize := 0;

  // Write RIFF header
  FStream.WriteBuffer('RIFF', 4);
  FStream.WriteBuffer(RIFFSize, 4);
  FStream.WriteBuffer('WAVE', 4);

  // Write fmt chunk
  FStream.WriteBuffer('fmt ', 4);
  RIFFSize := 16;  // fmt chunk size
  FStream.WriteBuffer(RIFFSize, 4);
  FStream.WriteBuffer(FormatCode, 2);
  FStream.WriteBuffer(FSettings.Channels, 2);
  FStream.WriteBuffer(FSettings.SampleRate, 4);
  FStream.WriteBuffer(ByteRate, 4);
  FStream.WriteBuffer(BlockAlign, 2);
  FStream.WriteBuffer(BitsPerSample, 2);

  // Write data chunk header
  FStream.WriteBuffer('data', 4);
  FWAVDataOffset := FStream.Position;
  FStream.WriteBuffer(DataSize, 4);  // Placeholder
end;

procedure TSedaiAudioFileWriter.FinalizeWAV;
var
  FileSize: Int64;
  RIFFSize: LongWord;
  DataSize: LongWord;
begin
  FileSize := FStream.Size;

  // Update RIFF size (file size - 8)
  FStream.Position := 4;
  RIFFSize := FileSize - 8;
  FStream.WriteBuffer(RIFFSize, 4);

  // Update data chunk size
  FStream.Position := FWAVDataOffset;
  DataSize := FileSize - FWAVDataOffset - 4;
  FStream.WriteBuffer(DataSize, 4);

  // Seek to end
  FStream.Position := FileSize;
end;

function TSedaiAudioFileWriter.WriteWAVSamples(ABuffer: PSingle;
  AFrameCount: Integer): Boolean;
var
  BytesPerSample: Integer;
  BytesToWrite: Integer;
  SampleCount: Integer;
begin
  Result := False;
  SampleCount := AFrameCount * FSettings.Channels;

  case FSettings.Format of
    aefWAV16:
      begin
        BytesPerSample := 2;
        BytesToWrite := SampleCount * BytesPerSample;
        if Length(FEncodeBuffer) < BytesToWrite then
          SetLength(FEncodeBuffer, BytesToWrite);
        ConvertFloatToPCM16(ABuffer, @FEncodeBuffer[0], SampleCount);
      end;
    aefWAV24:
      begin
        BytesPerSample := 3;
        BytesToWrite := SampleCount * BytesPerSample;
        if Length(FEncodeBuffer) < BytesToWrite then
          SetLength(FEncodeBuffer, BytesToWrite);
        ConvertFloatToPCM24(ABuffer, @FEncodeBuffer[0], SampleCount);
      end;
    aefWAV32:
      begin
        BytesPerSample := 4;
        BytesToWrite := SampleCount * BytesPerSample;
        if Length(FEncodeBuffer) < BytesToWrite then
          SetLength(FEncodeBuffer, BytesToWrite);
        ConvertFloatToPCM32(ABuffer, @FEncodeBuffer[0], SampleCount);
      end;
    aefWAVFloat:
      begin
        BytesPerSample := 4;
        BytesToWrite := SampleCount * BytesPerSample;
        // Float: just write directly
        FStream.WriteBuffer(ABuffer^, BytesToWrite);
        Inc(FSamplesWritten, AFrameCount);
        Result := True;
        Exit;
      end;
  else
    Exit;
  end;

  FStream.WriteBuffer(FEncodeBuffer[0], BytesToWrite);
  Inc(FSamplesWritten, AFrameCount);
  Result := True;
end;

procedure TSedaiAudioFileWriter.WriteBEWord(AValue: Word);
var
  V: Word;
begin
  V := NtoBE(AValue);
  FStream.WriteBuffer(V, 2);
end;

procedure TSedaiAudioFileWriter.WriteBEDWord(AValue: LongWord);
var
  V: LongWord;
begin
  V := NtoBE(AValue);
  FStream.WriteBuffer(V, 4);
end;

procedure TSedaiAudioFileWriter.WriteAIFFExtended(AValue: Double);
var
  B: array[0..9] of Byte;
  Sign, Expon: Integer;
  HiMant, LoMant: LongWord;
  FMant, M: Double;
  E: Integer;
begin
  // 80-bit IEEE 754 extended precision, big-endian, with an explicit integer
  // bit (Apple's ConvertToIeeeExtended). Exact inverse of the reader's
  // ReadExtended: value = mantissa * 2^(exponent - 16383 - 63).
  FillChar(B, SizeOf(B), 0);
  if AValue < 0 then
  begin
    Sign := $80;
    AValue := -AValue;
  end
  else
    Sign := 0;

  if AValue = 0 then
  begin
    Expon := 0;
    HiMant := 0;
    LoMant := 0;
  end
  else
  begin
    Frexp(AValue, M, E);   // AValue = M * 2^E, 0.5 <= M < 1
    if E > 16384 then
    begin
      Expon := $7FFF;      // out of range -> Inf
      HiMant := 0;
      LoMant := 0;
    end
    else
    begin
      Expon := E + 16382;  // bias for the explicit integer bit
      // M is non-negative, so Trunc == Floor; Trunc returns Int64 (Floor would
      // overflow its 32-bit Integer result on the ~2^31..2^32 mantissa).
      FMant := Ldexp(M, 32);
      HiMant := LongWord(Trunc(FMant));
      FMant := Ldexp(FMant - HiMant, 32);
      LoMant := LongWord(Trunc(FMant));
    end;
  end;

  B[0] := ((Expon shr 8) and $7F) or Sign;
  B[1] := Expon and $FF;
  B[2] := (HiMant shr 24) and $FF;
  B[3] := (HiMant shr 16) and $FF;
  B[4] := (HiMant shr 8) and $FF;
  B[5] := HiMant and $FF;
  B[6] := (LoMant shr 24) and $FF;
  B[7] := (LoMant shr 16) and $FF;
  B[8] := (LoMant shr 8) and $FF;
  B[9] := LoMant and $FF;
  FStream.WriteBuffer(B, 10);
end;

procedure TSedaiAudioFileWriter.WriteAIFFHeader;
var
  SampleSize: Word;
begin
  case FSettings.Format of
    aefAIFF16: SampleSize := 16;
    aefAIFF24: SampleSize := 24;
    aefAIFF32: SampleSize := 32;
  else
    Exit;  // Unsupported format
  end;

  // FORM container (size patched in FinalizeAIFF)
  FStream.WriteBuffer('FORM', 4);
  WriteBEDWord(0);
  FStream.WriteBuffer('AIFF', 4);

  // COMM chunk (18-byte body)
  FStream.WriteBuffer('COMM', 4);
  WriteBEDWord(18);
  WriteBEWord(Word(FSettings.Channels));
  FAIFFCommFramesOffset := FStream.Position;
  WriteBEDWord(0);                          // numSampleFrames (patched on close)
  WriteBEWord(SampleSize);
  WriteAIFFExtended(FSettings.SampleRate);

  // SSND chunk (size patched on close)
  FStream.WriteBuffer('SSND', 4);
  FAIFFSsndSizeOffset := FStream.Position;
  WriteBEDWord(0);                          // chunk size (patched on close)
  WriteBEDWord(0);                          // offset
  WriteBEDWord(0);                          // blockSize
  // sample frames follow
end;

procedure TSedaiAudioFileWriter.FinalizeAIFF;
var
  DataEnd, FileSize: Int64;
  SsndChunkSize: LongWord;
  PadByte: Byte;
begin
  // SSND chunk size counts offset + blockSize + sample data (everything after
  // the size field), measured before any even-boundary pad byte.
  DataEnd := FStream.Size;
  SsndChunkSize := LongWord(DataEnd - (FAIFFSsndSizeOffset + 4));

  // AIFF chunks are padded to an even byte boundary.
  if Odd(DataEnd) then
  begin
    FStream.Position := DataEnd;
    PadByte := 0;
    FStream.WriteBuffer(PadByte, 1);
  end;
  FileSize := FStream.Size;

  // FORM size = file size - 8
  FStream.Position := 4;
  WriteBEDWord(LongWord(FileSize - 8));

  // COMM numSampleFrames
  FStream.Position := FAIFFCommFramesOffset;
  WriteBEDWord(LongWord(FSamplesWritten));

  // SSND chunk size
  FStream.Position := FAIFFSsndSizeOffset;
  WriteBEDWord(SsndChunkSize);

  FStream.Position := FileSize;
end;

function TSedaiAudioFileWriter.WriteAIFFSamples(ABuffer: PSingle;
  AFrameCount: Integer): Boolean;
var
  BytesPerSample, BytesToWrite, SampleCount, I, J, Base: Integer;
  Tmp: Byte;
begin
  Result := False;
  SampleCount := AFrameCount * FSettings.Channels;

  case FSettings.Format of
    aefAIFF16: BytesPerSample := 2;
    aefAIFF24: BytesPerSample := 3;
    aefAIFF32: BytesPerSample := 4;
  else
    Exit;
  end;

  BytesToWrite := SampleCount * BytesPerSample;
  if Length(FEncodeBuffer) < BytesToWrite then
    SetLength(FEncodeBuffer, BytesToWrite);

  // Reuse the native-endian (little-endian on x86) PCM converters, including
  // their dithering, then byte-swap each sample to big-endian for AIFF.
  case FSettings.Format of
    aefAIFF16: ConvertFloatToPCM16(ABuffer, PSmallInt(@FEncodeBuffer[0]), SampleCount);
    aefAIFF24: ConvertFloatToPCM24(ABuffer, @FEncodeBuffer[0], SampleCount);
    aefAIFF32: ConvertFloatToPCM32(ABuffer, PLongInt(@FEncodeBuffer[0]), SampleCount);
  end;

  for I := 0 to SampleCount - 1 do
  begin
    Base := I * BytesPerSample;
    for J := 0 to (BytesPerSample div 2) - 1 do
    begin
      Tmp := FEncodeBuffer[Base + J];
      FEncodeBuffer[Base + J] := FEncodeBuffer[Base + BytesPerSample - 1 - J];
      FEncodeBuffer[Base + BytesPerSample - 1 - J] := Tmp;
    end;
  end;

  FStream.WriteBuffer(FEncodeBuffer[0], BytesToWrite);
  Inc(FSamplesWritten, AFrameCount);
  Result := True;
end;

procedure TSedaiAudioFileWriter.ConvertFloatToPCM8(ASrc: PSingle; ADst: PByte;
  ACount: Integer);
var
  I: Integer;
  Sample: Single;
  Value: Integer;
begin
  for I := 0 to ACount - 1 do
  begin
    Sample := ASrc[I];

    // Add dither
    if FSettings.DitherType <> dtNone then
      Sample := Sample + GetDitherSample;

    // Clamp and convert
    Sample := EnsureRange(Sample, -1.0, 1.0);
    Value := Round(Sample * 127.0) + 128;
    ADst[I] := EnsureRange(Value, 0, 255);
  end;
end;

procedure TSedaiAudioFileWriter.ConvertFloatToPCM16(ASrc: PSingle; ADst: PSmallInt;
  ACount: Integer);
var
  I: Integer;
  Sample: Single;
  Value: LongInt;
  Ch: Integer;
begin
  for I := 0 to ACount - 1 do
  begin
    Sample := ASrc[I];
    Ch := I mod FSettings.Channels;

    // Add dither
    case FSettings.DitherType of
      dtRPDF, dtTPDF:
        Sample := Sample + GetDitherSample;
      dtNoiseShaped:
        Sample := Sample + GetNoiseShapedDither(Ch);
    end;

    // Clamp and convert
    Sample := EnsureRange(Sample, -1.0, 1.0);
    Value := Round(Sample * 32767.0);
    ADst[I] := EnsureRange(Value, -32768, 32767);
  end;
end;

procedure TSedaiAudioFileWriter.ConvertFloatToPCM24(ASrc: PSingle; ADst: PByte;
  ACount: Integer);
var
  I: Integer;
  Sample: Single;
  Value: LongInt;
begin
  for I := 0 to ACount - 1 do
  begin
    Sample := ASrc[I];

    // Add dither (scaled for 24-bit)
    if FSettings.DitherType <> dtNone then
      Sample := Sample + GetDitherSample / 256.0;

    // Clamp and convert
    Sample := EnsureRange(Sample, -1.0, 1.0);
    Value := Round(Sample * 8388607.0);
    Value := EnsureRange(Value, -8388608, 8388607);

    // Write 24-bit little-endian
    ADst[I * 3] := Value and $FF;
    ADst[I * 3 + 1] := (Value shr 8) and $FF;
    ADst[I * 3 + 2] := (Value shr 16) and $FF;
  end;
end;

procedure TSedaiAudioFileWriter.ConvertFloatToPCM32(ASrc: PSingle; ADst: PLongInt;
  ACount: Integer);
var
  I: Integer;
  Sample: Single;
  Value: Int64;
begin
  for I := 0 to ACount - 1 do
  begin
    Sample := ASrc[I];
    // No dithering needed for 32-bit

    // Clamp and convert
    Sample := EnsureRange(Sample, -1.0, 1.0);
    Value := Round(Sample * 2147483647.0);
    ADst[I] := EnsureRange(Value, -2147483648, 2147483647);
  end;
end;

function TSedaiAudioFileWriter.CreateFile(const AFileName: string;
  const ASettings: TAudioExportSettings): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  Close;

  try
    FS := TFileStream.Create(AFileName, fmCreate);
    Result := CreateStream(FS, True, ASettings);

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

function TSedaiAudioFileWriter.CreateStream(AStream: TStream;
  AOwnsStream: Boolean; const ASettings: TAudioExportSettings): Boolean;
begin
  Result := False;
  Close;

  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FSettings := ASettings;
  FSamplesWritten := 0;

  case ASettings.Format of
    aefWAV16, aefWAV24, aefWAV32, aefWAVFloat:
      begin
        WriteWAVHeader;
        FIsOpen := True;
        Result := True;
      end;
    aefAIFF16, aefAIFF24, aefAIFF32:
      begin
        WriteAIFFHeader;
        FIsOpen := True;
        Result := True;
      end;
    aefOGG:
      begin
        // TODO: Implement OGG Vorbis encoding
        FLastError := 'OGG format not yet implemented';
      end;
    aefFLAC, aefFLAC24:
      begin
        // Lossless FLAC; the encoder writes its own fLaC + STREAMINFO. The
        // export format selects the bit depth (16 or 24).
        FFLACEnc := TSedaiFLACEncoder.Create;
        if FFLACEnc.Init(FStream, ASettings.SampleRate, ASettings.Channels,
             IfThen(ASettings.Format = aefFLAC24, 24, 16)) then
        begin
          FIsOpen := True;
          Result := True;
        end
        else
        begin
          FLastError := FFLACEnc.LastError;
          FreeAndNil(FFLACEnc);
        end;
      end;
  else
    FLastError := 'Unknown export format';
  end;

  if not Result then
  begin
    if FOwnsStream then
      FStream.Free;
    FStream := nil;
  end;
end;

procedure TSedaiAudioFileWriter.Close;
begin
  if FIsOpen then
  begin
    case FSettings.Format of
      aefWAV16, aefWAV24, aefWAV32, aefWAVFloat:
        FinalizeWAV;
      aefAIFF16, aefAIFF24, aefAIFF32:
        FinalizeAIFF;
      aefFLAC, aefFLAC24:
        if Assigned(FFLACEnc) then FFLACEnc.Finalize;
    end;
  end;

  FreeAndNil(FFLACEnc);

  if Assigned(FStream) and FOwnsStream then
    FStream.Free;

  FStream := nil;
  FOwnsStream := False;
  FIsOpen := False;

  SetLength(FEncodeBuffer, 0);
end;

function TSedaiAudioFileWriter.WriteSamples(ABuffer: PSingle;
  AFrameCount: Integer): Boolean;
begin
  Result := False;

  if not FIsOpen then
  begin
    FLastError := 'File not open';
    Exit;
  end;

  case FSettings.Format of
    aefWAV16, aefWAV24, aefWAV32, aefWAVFloat:
      Result := WriteWAVSamples(ABuffer, AFrameCount);
    aefAIFF16, aefAIFF24, aefAIFF32:
      Result := WriteAIFFSamples(ABuffer, AFrameCount);
    aefFLAC, aefFLAC24:
      if Assigned(FFLACEnc) then
      begin
        Result := FFLACEnc.WriteFrames(ABuffer, AFrameCount);
        if Result then Inc(FSamplesWritten, AFrameCount);
      end;
  end;
end;

function TSedaiAudioFileWriter.WriteBuffer(ABuffer: TSedaiAudioBuffer;
  ACallback: TAudioWriteProgressCallback): Boolean;
var
  InterleavedBuffer: array of Single;
  BlockSize: Integer;
  FramesRemaining: Int64;
  FramesToWrite: Integer;
  Position: Int64;
  Progress: Double;
  Cancel: Boolean;
begin
  Result := False;

  if not FIsOpen then
  begin
    FLastError := 'File not open';
    Exit;
  end;

  if ABuffer = nil then
  begin
    FLastError := 'Buffer is nil';
    Exit;
  end;

  BlockSize := 8192;
  SetLength(InterleavedBuffer, BlockSize * ABuffer.Channels);

  Position := 0;
  FramesRemaining := ABuffer.SampleCount;
  Cancel := False;

  while (FramesRemaining > 0) and not Cancel do
  begin
    FramesToWrite := Min(BlockSize, FramesRemaining);

    // Read interleaved from buffer
    ABuffer.ReadInterleaved(@InterleavedBuffer[0], Position, FramesToWrite);

    // Write samples
    if not WriteSamples(@InterleavedBuffer[0], FramesToWrite) then
      Exit;

    Inc(Position, FramesToWrite);
    Dec(FramesRemaining, FramesToWrite);

    // Progress callback
    if Assigned(ACallback) then
    begin
      Progress := Position / ABuffer.SampleCount;
      ACallback(Progress, Cancel);
    end;
  end;

  Result := not Cancel;
end;

class function TSedaiAudioFileWriter.ExportToWAV(ABuffer: TSedaiAudioBuffer;
  const AFileName: string; ABitDepth: Integer): Boolean;
var
  Writer: TSedaiAudioFileWriter;
  ExportSettings: TAudioExportSettings;
begin
  Result := False;

  if ABuffer = nil then
    Exit;

  case ABitDepth of
    16: ExportSettings := GetDefaultSettings(aefWAV16);
    24: ExportSettings := GetDefaultSettings(aefWAV24);
    32: ExportSettings := GetDefaultSettings(aefWAV32);
  else
    ExportSettings := GetDefaultSettings(aefWAV16);
  end;

  ExportSettings.SampleRate := ABuffer.SampleRate;
  ExportSettings.Channels := ABuffer.Channels;

  Writer := TSedaiAudioFileWriter.Create;
  try
    if Writer.CreateFile(AFileName, ExportSettings) then
      Result := Writer.WriteBuffer(ABuffer);
  finally
    Writer.Free;
  end;
end;

class function TSedaiAudioFileWriter.ExportToWAVFloat(ABuffer: TSedaiAudioBuffer;
  const AFileName: string): Boolean;
var
  Writer: TSedaiAudioFileWriter;
  ExportSettings: TAudioExportSettings;
begin
  Result := False;

  if ABuffer = nil then
    Exit;

  ExportSettings := GetDefaultSettings(aefWAVFloat);
  ExportSettings.SampleRate := ABuffer.SampleRate;
  ExportSettings.Channels := ABuffer.Channels;

  Writer := TSedaiAudioFileWriter.Create;
  try
    if Writer.CreateFile(AFileName, ExportSettings) then
      Result := Writer.WriteBuffer(ABuffer);
  finally
    Writer.Free;
  end;
end;

class function TSedaiAudioFileWriter.ExportToAIFF(ABuffer: TSedaiAudioBuffer;
  const AFileName: string; ABitDepth: Integer): Boolean;
var
  Writer: TSedaiAudioFileWriter;
  ExportSettings: TAudioExportSettings;
begin
  Result := False;

  if ABuffer = nil then
    Exit;

  case ABitDepth of
    16: ExportSettings := GetDefaultSettings(aefAIFF16);
    24: ExportSettings := GetDefaultSettings(aefAIFF24);
    32: ExportSettings := GetDefaultSettings(aefAIFF32);
  else
    ExportSettings := GetDefaultSettings(aefAIFF16);
  end;

  ExportSettings.SampleRate := ABuffer.SampleRate;
  ExportSettings.Channels := ABuffer.Channels;

  Writer := TSedaiAudioFileWriter.Create;
  try
    if Writer.CreateFile(AFileName, ExportSettings) then
      Result := Writer.WriteBuffer(ABuffer);
  finally
    Writer.Free;
  end;
end;

class procedure TSedaiAudioFileWriter.NormalizeBuffer(ABuffer: TSedaiAudioBuffer;
  ATargetPeakDB: Single);
var
  Peak: Single;
  TargetPeak: Single;
  Gain: Single;
  I, Ch: Integer;
  Sample: Single;
  Data: PSingle;
begin
  if ABuffer = nil then
    Exit;

  // Find peak
  Peak := 0;
  for Ch := 0 to ABuffer.Channels - 1 do
  begin
    Data := ABuffer.GetChannelData(Ch);
    for I := 0 to ABuffer.SampleCount - 1 do
    begin
      Sample := Abs(Data[I]);
      if Sample > Peak then
        Peak := Sample;
    end;
  end;

  if Peak < 0.0001 then
    Exit;  // Too quiet, don't normalize

  // Calculate gain
  TargetPeak := Power(10, ATargetPeakDB / 20.0);
  Gain := TargetPeak / Peak;

  // Apply gain
  for Ch := 0 to ABuffer.Channels - 1 do
  begin
    Data := ABuffer.GetChannelData(Ch);
    for I := 0 to ABuffer.SampleCount - 1 do
      Data[I] := Data[I] * Gain;
  end;
end;

end.
