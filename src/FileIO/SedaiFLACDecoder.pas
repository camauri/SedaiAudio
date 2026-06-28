{*
 * Sedai Audio Foundation - FLAC Decoder (pure Free Pascal)
 *
 * TSedaiFLACDecoder is a from-scratch, dependency-free decoder for the Free
 * Lossless Audio Codec. It implements the full FLAC subset emitted by libFLAC
 * and ffmpeg: STREAMINFO parsing, fixed-blocksize and variable-blocksize
 * frames, all four subframe kinds (CONSTANT / VERBATIM / FIXED orders 0..4 /
 * LPC orders 1..32), partitioned Rice residual coding (4- and 5-bit params,
 * including escaped raw partitions) and the four channel assignments
 * (independent, left/side, right/side, mid/side).
 *
 * Because FLAC is lossless, the decoded integer samples are bit-exact against
 * the original PCM; we then scale to float by 1/2^(bits-1) to match the
 * WAV/AIFF reader paths. Output is interleaved at the native channel count.
 *
 * Reference: the FLAC format specification (xiph.org), designed by Josh Coalson
 * and maintained by the Xiph.Org Foundation. No third-party code is vendored;
 * this is a clean-room implementation from the spec. Our thanks to the FLAC
 * authors for an open, well-documented lossless format. See LICENSING.md.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0 OR Commercial (clean-room; not a
 * derivative of any GPL'd decoder)
 *}
unit SedaiFLACDecoder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiAudioDecoder;

type
  { TFLACBitReader }
  // MSB-first bit reader over a TStream with an internal byte buffer. FLAC's
  // bitstream is big-endian at the bit level, which falls out naturally from
  // reading bytes high-bit first.
  TFLACBitReader = class
  private
    FStream: TStream;
    FBuf: array[0..8191] of Byte;
    FBufLen: Integer;          // valid bytes in FBuf
    FBufPos: Integer;          // next byte to consume from FBuf
    FAcc: UInt64;              // bit accumulator (MSB-aligned within valid bits)
    FAccBits: Integer;         // number of valid bits currently in FAcc
    FEOF: Boolean;
    function FillByte(out AByte: Byte): Boolean;
  public
    constructor Create(AStream: TStream);
    procedure Reset;                           // re-sync after a raw stream seek
    function ReadBits(ACount: Integer): UInt32;       // ACount in 0..32
    function ReadBits64(ACount: Integer): UInt64;     // ACount in 0..57
    function ReadBitsSigned(ACount: Integer): Int32;  // sign-extended, 1..32
    function ReadUnary: UInt32;                       // count of leading zeros
    procedure AlignToByte;
    function AtEOF: Boolean;
    function BytePosition: Int64;           // absolute byte offset (byte-aligned only)
  end;

  // Decoded integer samples, one dynamic array per channel.
  TFLACChannelData = array of Int32;

  { TSedaiFLACDecoder }
  TSedaiFLACDecoder = class(TSedaiAudioDecoder)
  private
    FBits: TFLACBitReader;
    FMinBlockSize: Integer;
    FMaxBlockSize: Integer;
    FFirstFrameOffset: Int64;        // stream offset of the first audio frame
    FScale: Double;                  // 1 / 2^(bits-1), int sample -> float

    // Per-frame working storage
    FChannelData: array of TFLACChannelData;   // [channel][sample]
    FBlockSize: Integer;                        // samples in current frame
    FFrameChannels: Integer;
    FChannelAssignment: Integer;

    // Output FIFO of the current decoded frame (interleaved float)
    FOutBuf: array of Single;
    FOutFrames: Integer;             // frames available in FOutBuf
    FOutRead: Integer;               // next frame index to hand out

    function ReadMetadata: Boolean;
    function ReadUTF8Coded: UInt64;             // FLAC UTF-8-like coded number
    function DecodeNextFrame: Boolean;          // fills FOutBuf, returns False at EOF
    procedure DecodeSubframe(AChannel, ABps: Integer);
    procedure DecodeResidual(AChannel, APredictorOrder: Integer);
    procedure ApplyChannelDecorrelation;
    procedure RestartFromFirstFrame;
  public
    constructor Create; override;
    destructor Destroy; override;
    function OpenStream(AStream: TStream): Boolean; override;
    function ReadFrames(ABuffer: PSingle; AFrameCount: Integer): Integer; override;
    function Seek(AFrame: Int64): Boolean; override;
  end;

implementation

const
  FLAC_BLOCKSIZE_TABLE: array[0..15] of Integer = (
    0, 192, 576, 1152, 2304, 4608, 0, 0,
    256, 512, 1024, 2048, 4096, 8192, 16384, 32768);

  FLAC_SAMPLERATE_TABLE: array[0..11] of Integer = (
    0, 88200, 176400, 192000, 8000, 16000, 22050, 24000,
    32000, 44100, 48000, 96000);

  FLAC_BPS_TABLE: array[0..7] of Integer = (
    0, 8, 12, 0, 16, 20, 24, 32);

// Arithmetic (sign-preserving, floor) right shift. FPC's `shr` is logical, so
// negative LPC predictions would shift in zero bits — use this instead.
function SarI64(AValue: Int64; AShift: Integer): Int64;
var
  D: Int64;
begin
  if AShift <= 0 then Exit(AValue);
  D := Int64(1) shl AShift;
  Result := AValue div D;
  if (AValue < 0) and (Result * D <> AValue) then
    Dec(Result);                           // round toward -infinity (arithmetic)
end;

{ TFLACBitReader }

constructor TFLACBitReader.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
  Reset;
end;

procedure TFLACBitReader.Reset;
begin
  FBufLen := 0;
  FBufPos := 0;
  FAcc := 0;
  FAccBits := 0;
  FEOF := False;
end;

function TFLACBitReader.FillByte(out AByte: Byte): Boolean;
begin
  if FBufPos >= FBufLen then
  begin
    FBufLen := FStream.Read(FBuf[0], SizeOf(FBuf));
    FBufPos := 0;
    if FBufLen <= 0 then
    begin
      FEOF := True;
      AByte := 0;
      Exit(False);
    end;
  end;
  AByte := FBuf[FBufPos];
  Inc(FBufPos);
  Result := True;
end;

function TFLACBitReader.ReadBits(ACount: Integer): UInt32;
var
  B: Byte;
begin
  if ACount <= 0 then Exit(0);
  while FAccBits < ACount do
  begin
    if not FillByte(B) then
    begin
      // Pad with zeros at EOF so callers terminate gracefully.
      FAcc := FAcc shl 8;
      Inc(FAccBits, 8);
    end
    else
    begin
      FAcc := (FAcc shl 8) or B;
      Inc(FAccBits, 8);
    end;
  end;
  Dec(FAccBits, ACount);
  if ACount >= 32 then
    Result := UInt32(FAcc shr FAccBits)
  else
    Result := UInt32((FAcc shr FAccBits) and ((UInt64(1) shl ACount) - 1));
end;

function TFLACBitReader.ReadBits64(ACount: Integer): UInt64;
begin
  if ACount <= 32 then
    Result := ReadBits(ACount)
  else
    Result := (UInt64(ReadBits(ACount - 32)) shl 32) or ReadBits(32);
end;

function TFLACBitReader.ReadBitsSigned(ACount: Integer): Int32;
var
  V: UInt32;
begin
  if ACount <= 0 then Exit(0);
  V := ReadBits(ACount);
  // Sign-extend from ACount bits.
  if (ACount < 32) and ((V and (UInt32(1) shl (ACount - 1))) <> 0) then
    Result := Int32(V or (not ((UInt32(1) shl ACount) - 1)))
  else
    Result := Int32(V);
end;

function TFLACBitReader.ReadUnary: UInt32;
begin
  Result := 0;
  while ReadBits(1) = 0 do
  begin
    Inc(Result);
    if FEOF and (FAccBits = 0) then Break;   // safety against runaway at EOF
  end;
end;

procedure TFLACBitReader.AlignToByte;
begin
  FAccBits := FAccBits - (FAccBits and 7);
end;

function TFLACBitReader.AtEOF: Boolean;
begin
  Result := FEOF and (FAccBits = 0);
end;

function TFLACBitReader.BytePosition: Int64;
begin
  // Absolute offset of the next unconsumed bit, accounting for the bytes still
  // buffered ahead and whole bytes parked in the accumulator. Caller must be
  // byte-aligned (FAccBits a multiple of 8).
  Result := FStream.Position - (FBufLen - FBufPos) - (FAccBits div 8);
end;

{ TSedaiFLACDecoder }

constructor TSedaiFLACDecoder.Create;
begin
  inherited Create;
  FBits := nil;
  FOutFrames := 0;
  FOutRead := 0;
end;

destructor TSedaiFLACDecoder.Destroy;
begin
  FBits.Free;
  inherited Destroy;
end;

function TSedaiFLACDecoder.OpenStream(AStream: TStream): Boolean;
var
  Magic: array[0..3] of AnsiChar;
begin
  Result := False;
  FStream := AStream;
  FStream.Position := 0;

  // 'fLaC' stream marker.
  if FStream.Read(Magic, 4) <> 4 then
  begin
    FLastError := 'FLAC: stream too short';
    Exit;
  end;
  if (Magic[0] <> 'f') or (Magic[1] <> 'L') or
     (Magic[2] <> 'a') or (Magic[3] <> 'C') then
  begin
    FLastError := 'FLAC: missing fLaC marker';
    Exit;
  end;

  FBits := TFLACBitReader.Create(FStream);
  if not ReadMetadata then Exit;

  if (FSampleRate <= 0) or (FChannels <= 0) or (FBitsPerSample <= 0) then
  begin
    FLastError := 'FLAC: invalid STREAMINFO';
    Exit;
  end;

  // Logical end of the metadata = start of the first audio frame. NOT
  // FStream.Position, which has run ahead due to the bit reader's buffering.
  FBits.AlignToByte;
  FFirstFrameOffset := FBits.BytePosition;
  FScale := 1.0 / (Int64(1) shl (FBitsPerSample - 1));
  FSeekable := True;
  FPosition := 0;
  FOutFrames := 0;
  FOutRead := 0;
  SetLength(FChannelData, FChannels);

  Result := True;
end;

function TSedaiFLACDecoder.ReadMetadata: Boolean;
var
  IsLast: Boolean;
  BlockType: LongWord;
  BlockLen: LongWord;
  ChannelsM1, BpsM1: Integer;
  Consumed, ToSkip: Integer;
begin
  Result := False;
  IsLast := False;

  while not IsLast do
  begin
    // METADATA_BLOCK_HEADER: 1 bit last-flag, 7 bit type, 24 bit length.
    IsLast := (FBits.ReadBits(1) = 1);
    BlockType := FBits.ReadBits(7);
    BlockLen := FBits.ReadBits(24);

    if BlockType = 0 then
    begin
      // STREAMINFO (mandatory, first). 34 bytes.
      FMinBlockSize := FBits.ReadBits(16);
      FMaxBlockSize := FBits.ReadBits(16);
      FBits.ReadBits(24);                    // min frame size (unused)
      FBits.ReadBits(24);                    // max frame size (unused)
      FSampleRate := FBits.ReadBits(20);
      ChannelsM1 := FBits.ReadBits(3);
      BpsM1 := FBits.ReadBits(5);
      FChannels := ChannelsM1 + 1;
      FBitsPerSample := BpsM1 + 1;
      FTotalFrames := Int64(FBits.ReadBits64(36));
      // Skip the 128-bit MD5 (16 bytes) — we are byte-aligned here.
      FBits.AlignToByte;
      for Consumed := 1 to 16 do
        FBits.ReadBits(8);
    end
    else
    begin
      // Any other metadata block: skip its body. We are byte-aligned after
      // the 32-bit header.
      FBits.AlignToByte;
      ToSkip := BlockLen;
      while ToSkip > 0 do
      begin
        FBits.ReadBits(8);
        Dec(ToSkip);
      end;
    end;
  end;

  Result := True;
end;

function TSedaiFLACDecoder.ReadUTF8Coded: UInt64;
var
  X, B: UInt32;
  Follow, I: Integer;
  V: UInt64;
begin
  X := FBits.ReadBits(8);
  if (X and $80) = 0 then Exit(X);
  if (X and $E0) = $C0 then begin Follow := 1; V := X and $1F; end
  else if (X and $F0) = $E0 then begin Follow := 2; V := X and $0F; end
  else if (X and $F8) = $F0 then begin Follow := 3; V := X and $07; end
  else if (X and $FC) = $F8 then begin Follow := 4; V := X and $03; end
  else if (X and $FE) = $FC then begin Follow := 5; V := X and $01; end
  else if X = $FE then begin Follow := 6; V := 0; end
  else begin Follow := 0; V := X; end;    // invalid lead byte
  for I := 1 to Follow do
  begin
    B := FBits.ReadBits(8);
    V := (V shl 6) or (B and $3F);
  end;
  Result := V;
end;

function TSedaiFLACDecoder.DecodeNextFrame: Boolean;
var
  SyncCode: UInt32;
  BlockSizeCode, SampleRateCode, ChAssign, SampleSizeCode: Integer;
  FrameBps, SideBps, BS, Ch, I: Integer;
begin
  Result := False;
  if FBits.AtEOF then Exit;

  // Frame sync code: 14-bit 0b11111111111110 (0x3FFE).
  SyncCode := FBits.ReadBits(14);
  if SyncCode <> $3FFE then Exit;          // out of frames / corrupt -> stop

  FBits.ReadBits(1);                       // reserved (0)
  FBits.ReadBits(1);                       // blocking strategy (unused here)
  BlockSizeCode := FBits.ReadBits(4);
  SampleRateCode := FBits.ReadBits(4);
  ChAssign := FBits.ReadBits(4);
  SampleSizeCode := FBits.ReadBits(3);
  FBits.ReadBits(1);                       // reserved (0)

  // Coded frame/sample number — consumed but not needed.
  ReadUTF8Coded;

  // Resolve block size (samples per channel in this frame).
  case BlockSizeCode of
    0: Exit;                               // reserved
    6: BS := FBits.ReadBits(8) + 1;
    7: BS := FBits.ReadBits(16) + 1;
  else
    BS := FLAC_BLOCKSIZE_TABLE[BlockSizeCode];
  end;
  if BS <= 0 then Exit;

  // Consume any explicit sample-rate bits (value already known from header).
  case SampleRateCode of
    12: FBits.ReadBits(8);
    13: FBits.ReadBits(16);
    14: FBits.ReadBits(16);
  end;

  FBits.ReadBits(8);                       // frame-header CRC-8 (ignored)

  FBlockSize := BS;
  FChannelAssignment := ChAssign;
  if ChAssign < 8 then
    FFrameChannels := ChAssign + 1
  else
    FFrameChannels := 2;                   // L/S, R/S, M/S

  if SampleSizeCode = 0 then
    FrameBps := FBitsPerSample
  else
    FrameBps := FLAC_BPS_TABLE[SampleSizeCode];

  if Length(FChannelData) < FFrameChannels then
    SetLength(FChannelData, FFrameChannels);
  for Ch := 0 to FFrameChannels - 1 do
    if Length(FChannelData[Ch]) < BS then
      SetLength(FChannelData[Ch], BS);

  // Decode each subframe; the "side" channel carries one extra bit.
  for Ch := 0 to FFrameChannels - 1 do
  begin
    SideBps := FrameBps;
    case ChAssign of
      8:  if Ch = 1 then Inc(SideBps);     // left/side  -> side is channel 1
      9:  if Ch = 0 then Inc(SideBps);     // right/side -> side is channel 0
      10: if Ch = 1 then Inc(SideBps);     // mid/side   -> side is channel 1
    end;
    DecodeSubframe(Ch, SideBps);
  end;

  // Frame footer: byte align + 16-bit CRC (ignored).
  FBits.AlignToByte;
  FBits.ReadBits(16);

  ApplyChannelDecorrelation;

  // Emit interleaved float.
  if Length(FOutBuf) < BS * FFrameChannels then
    SetLength(FOutBuf, BS * FFrameChannels);
  for I := 0 to BS - 1 do
    for Ch := 0 to FFrameChannels - 1 do
      FOutBuf[I * FFrameChannels + Ch] := FChannelData[Ch][I] * FScale;

  FOutFrames := BS;
  FOutRead := 0;
  Result := True;
end;

procedure TSedaiFLACDecoder.DecodeSubframe(AChannel, ABps: Integer);
var
  SubframeType, Order, WastedBits, EffBps: Integer;
  I, J, BS: Integer;
  C: Int32;
  D: TFLACChannelData;
  Precision, Shift: Integer;
  Coefs: array[0..31] of Int32;
  Pred: Int64;
begin
  BS := FBlockSize;
  D := FChannelData[AChannel];

  // Subframe header: 1 padding bit (must be 0), 6-bit type, wasted-bit flag.
  FBits.ReadBits(1);
  SubframeType := FBits.ReadBits(6);
  WastedBits := 0;
  if FBits.ReadBits(1) = 1 then
    WastedBits := FBits.ReadUnary + 1;     // unary k -> k+1 wasted low bits

  EffBps := ABps - WastedBits;

  if SubframeType = 0 then
  begin
    // CONSTANT
    C := FBits.ReadBitsSigned(EffBps);
    for I := 0 to BS - 1 do D[I] := C;
  end
  else if SubframeType = 1 then
  begin
    // VERBATIM
    for I := 0 to BS - 1 do D[I] := FBits.ReadBitsSigned(EffBps);
  end
  else if (SubframeType >= 8) and (SubframeType <= 12) then
  begin
    // FIXED predictor, order 0..4
    Order := SubframeType - 8;
    for I := 0 to Order - 1 do D[I] := FBits.ReadBitsSigned(EffBps);
    DecodeResidual(AChannel, Order);
    case Order of
      0: ;                                 // residual is the signal
      1: for I := Order to BS - 1 do
           D[I] := Int32(Int64(D[I]) + Int64(D[I-1]));
      2: for I := Order to BS - 1 do
           D[I] := Int32(Int64(D[I]) + 2*Int64(D[I-1]) - Int64(D[I-2]));
      3: for I := Order to BS - 1 do
           D[I] := Int32(Int64(D[I]) + 3*Int64(D[I-1]) - 3*Int64(D[I-2]) + Int64(D[I-3]));
      4: for I := Order to BS - 1 do
           D[I] := Int32(Int64(D[I]) + 4*Int64(D[I-1]) - 6*Int64(D[I-2]) + 4*Int64(D[I-3]) - Int64(D[I-4]));
    end;
  end
  else if SubframeType >= 32 then
  begin
    // LPC, order 1..32
    Order := (SubframeType and $1F) + 1;
    for I := 0 to Order - 1 do D[I] := FBits.ReadBitsSigned(EffBps);
    Precision := FBits.ReadBits(4) + 1;    // coefficient precision in bits
    Shift := FBits.ReadBitsSigned(5);      // quantization shift (>= 0 in practice)
    if Shift < 0 then Shift := 0;
    for J := 0 to Order - 1 do
      Coefs[J] := FBits.ReadBitsSigned(Precision);
    DecodeResidual(AChannel, Order);
    for I := Order to BS - 1 do
    begin
      Pred := 0;
      for J := 0 to Order - 1 do
        Pred := Pred + Int64(Coefs[J]) * Int64(D[I-1-J]);
      D[I] := Int32(Int64(D[I]) + SarI64(Pred, Shift));
    end;
  end;

  // Restore wasted low bits.
  if WastedBits > 0 then
    for I := 0 to BS - 1 do
      D[I] := D[I] shl WastedBits;
end;

procedure TSedaiFLACDecoder.DecodeResidual(AChannel, APredictorOrder: Integer);
var
  Method, PartOrder, Partitions, P: Integer;
  ParamBits, EscFlag, RiceParam, EscBits: Integer;
  Cnt, N, SampleIdx: Integer;
  D: TFLACChannelData;
  Q, U: UInt32;
  Val: Int32;
begin
  D := FChannelData[AChannel];
  Method := FBits.ReadBits(2);             // 0 = 4-bit, 1 = 5-bit Rice params
  PartOrder := FBits.ReadBits(4);
  Partitions := 1 shl PartOrder;
  if Method = 0 then begin ParamBits := 4; EscFlag := $0F; end
  else begin ParamBits := 5; EscFlag := $1F; end;

  SampleIdx := APredictorOrder;
  for P := 0 to Partitions - 1 do
  begin
    if P = 0 then
      Cnt := (FBlockSize shr PartOrder) - APredictorOrder
    else
      Cnt := FBlockSize shr PartOrder;

    RiceParam := FBits.ReadBits(ParamBits);
    if RiceParam = EscFlag then
    begin
      // Escaped partition: raw residuals of EscBits bits each.
      EscBits := FBits.ReadBits(5);
      for N := 0 to Cnt - 1 do
      begin
        D[SampleIdx] := FBits.ReadBitsSigned(EscBits);
        Inc(SampleIdx);
      end;
    end
    else
    begin
      for N := 0 to Cnt - 1 do
      begin
        Q := FBits.ReadUnary;
        U := Q shl RiceParam;
        if RiceParam > 0 then
          U := U or FBits.ReadBits(RiceParam);
        // zig-zag de-interleave: even -> +, odd -> -
        if (U and 1) <> 0 then
          Val := Int32(-(Int64(U shr 1) + 1))
        else
          Val := Int32(U shr 1);
        D[SampleIdx] := Val;
        Inc(SampleIdx);
      end;
    end;
  end;
end;

procedure TSedaiFLACDecoder.ApplyChannelDecorrelation;
var
  I, BS: Integer;
  Mid, Side: Int64;
  L0, R0: TFLACChannelData;
begin
  BS := FBlockSize;
  if FChannelAssignment < 8 then Exit;     // independent channels

  L0 := FChannelData[0];
  R0 := FChannelData[1];
  case FChannelAssignment of
    8:  // left/side: ch0=left, ch1=side(=left-right) -> right = left - side
      for I := 0 to BS - 1 do
        R0[I] := Int32(Int64(L0[I]) - Int64(R0[I]));
    9:  // right/side: ch0=side(=left-right), ch1=right -> left = right + side
      for I := 0 to BS - 1 do
        L0[I] := Int32(Int64(R0[I]) + Int64(L0[I]));
    10: // mid/side: ch0=mid, ch1=side
      for I := 0 to BS - 1 do
      begin
        Side := R0[I];
        Mid := (Int64(L0[I]) shl 1) or (Side and 1);
        L0[I] := Int32((Mid + Side) div 2);   // even by construction -> exact
        R0[I] := Int32((Mid - Side) div 2);
      end;
  end;
end;

procedure TSedaiFLACDecoder.RestartFromFirstFrame;
begin
  FStream.Position := FFirstFrameOffset;
  FBits.Reset;
  FOutFrames := 0;
  FOutRead := 0;
  FPosition := 0;
end;

function TSedaiFLACDecoder.ReadFrames(ABuffer: PSingle; AFrameCount: Integer): Integer;
var
  Produced, Avail, ToCopy: Integer;
  Dst: PSingle;
begin
  Result := 0;
  if (AFrameCount <= 0) or (FBits = nil) then Exit;
  Dst := ABuffer;
  Produced := 0;
  while Produced < AFrameCount do
  begin
    if FOutRead >= FOutFrames then
    begin
      if not DecodeNextFrame then Break;   // EOF / no more frames
      if FOutFrames = 0 then Break;
    end;
    Avail := FOutFrames - FOutRead;
    ToCopy := AFrameCount - Produced;
    if ToCopy > Avail then ToCopy := Avail;
    Move(FOutBuf[FOutRead * FFrameChannels], Dst^,
         ToCopy * FFrameChannels * SizeOf(Single));
    Inc(Dst, ToCopy * FFrameChannels);
    Inc(FOutRead, ToCopy);
    Inc(Produced, ToCopy);
  end;
  Inc(FPosition, Produced);
  Result := Produced;
end;

function TSedaiFLACDecoder.Seek(AFrame: Int64): Boolean;
var
  Scratch: array of Single;
  ToSkip: Int64;
  Chunk, Got: Integer;
begin
  Result := False;
  if not FSeekable then Exit;
  if AFrame < 0 then AFrame := 0;
  if (FTotalFrames > 0) and (AFrame > FTotalFrames) then AFrame := FTotalFrames;

  // Linear scan: rewind if seeking backwards, then decode-and-discard forward.
  if AFrame < FPosition then
    RestartFromFirstFrame;

  ToSkip := AFrame - FPosition;
  SetLength(Scratch, 4096 * FChannels);
  while ToSkip > 0 do
  begin
    Chunk := 4096;
    if Chunk > ToSkip then Chunk := Integer(ToSkip);
    Got := ReadFrames(@Scratch[0], Chunk);
    if Got <= 0 then Break;
    Dec(ToSkip, Got);
  end;
  Result := (ToSkip = 0);
end;

end.