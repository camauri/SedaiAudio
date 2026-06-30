{*
 * Sedai Audio Foundation - FLAC Encoder (pure Free Pascal)
 *
 * TSedaiFLACEncoder writes a valid, standards-compliant FLAC stream from
 * interleaved float PCM. It is a clean-room implementation from the FLAC
 * format specification (Xiph.Org / Josh Coalson), the encode-side counterpart
 * to SedaiFLACDecoder.
 *
 * Coding choices (a real but compact encoder — not maximum compression):
 *   - STREAMINFO + audio frames, fixed block size (4096), independent channels.
 *   - Per subframe: CONSTANT when possible, else the best FIXED predictor
 *     (orders 0..4) chosen by exact Rice bit cost, else VERBATIM fallback.
 *   - Residuals: partitioned Rice, partition order 0, 4-bit parameter, with the
 *     optimal parameter per subframe. (No LPC, no inter-channel decorrelation.)
 *   - Correct CRC-8 (frame header) and CRC-16 (frame) for interoperability.
 *
 * Lossless: re-decoding the output reproduces the input integer PCM exactly.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0 OR Commercial (clean-room).
 *}
unit SedaiFLACEncoder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TFLACBitWriter }
  // MSB-first bit writer into a growable byte buffer (matches the decoder's
  // big-endian-at-the-bit-level reader).
  TFLACBitWriter = class
  private
    FBuf: array of Byte;
    FLen: Integer;            // bytes committed
    FAcc: UInt64;
    FAccBits: Integer;
    procedure PutByte(AByte: Byte);
  public
    procedure Clear;
    procedure WriteBits(AValue: UInt32; ACount: Integer);
    procedure WriteUnary(AQuotient: UInt32);
    procedure AlignToByte;    // pad with zero bits to a byte boundary
    property Len: Integer read FLen;
    function ByteAt(AIndex: Integer): Byte;
  end;

  { TSedaiFLACEncoder }
  TSedaiFLACEncoder = class
  private
    FStream: TStream;
    FSampleRate, FChannels, FBitsPerSample: Integer;
    FBlockSize: Integer;
    FStreamInfoOffset: Int64;
    FTotalFrames: Int64;
    FFrameNumber: Int64;
    FMinFrameSize, FMaxFrameSize: Integer;
    FActualMinBlock, FActualMaxBlock: Integer;
    // accumulation of interleaved int samples until a full block is ready
    FAccum: array of Int32;          // interleaved, FAccumFrames * FChannels
    FAccumFrames: Integer;
    FLastError: string;

    procedure WriteStreamInfo(AFinal: Boolean);
    procedure EncodeBlock(const AInterleaved: array of Int32; AFrames: Integer);
    procedure EncodeSubframe(bw: TFLACBitWriter; const ASamples: array of Int32; ACount: Integer);
    procedure WriteResidual(bw: TFLACBitWriter; const AResidual: array of Int64; AStart, ACount: Integer);
  public
    constructor Create;
    function Init(AStream: TStream; ASampleRate, AChannels, ABitsPerSample: Integer): Boolean;
    function WriteFrames(ABuffer: PSingle; AFrameCount: Integer): Boolean;
    function Finalize: Boolean;
    property LastError: string read FLastError;
  end;

implementation

{ TFLACBitWriter }

procedure TFLACBitWriter.PutByte(AByte: Byte);
begin
  if FLen >= Length(FBuf) then
  begin
    if Length(FBuf) = 0 then SetLength(FBuf, 1024)
    else SetLength(FBuf, Length(FBuf) * 2);
  end;
  FBuf[FLen] := AByte;
  Inc(FLen);
end;

procedure TFLACBitWriter.Clear;
begin
  FLen := 0;
  FAcc := 0;
  FAccBits := 0;
end;

procedure TFLACBitWriter.WriteBits(AValue: UInt32; ACount: Integer);
begin
  if ACount <= 0 then Exit;
  if ACount < 32 then AValue := AValue and ((UInt32(1) shl ACount) - 1);
  FAcc := (FAcc shl ACount) or AValue;
  Inc(FAccBits, ACount);
  while FAccBits >= 8 do
  begin
    Dec(FAccBits, 8);
    PutByte(Byte((FAcc shr FAccBits) and $FF));
  end;
end;

procedure TFLACBitWriter.WriteUnary(AQuotient: UInt32);
begin
  // AQuotient zero bits then a terminating 1 bit.
  while AQuotient >= 32 do
  begin
    WriteBits(0, 32);
    Dec(AQuotient, 32);
  end;
  // write AQuotient zeros + the 1 = a single 1 in (AQuotient+1) bits
  WriteBits(1, Integer(AQuotient) + 1);
end;

procedure TFLACBitWriter.AlignToByte;
begin
  if (FAccBits and 7) <> 0 then
    WriteBits(0, 8 - (FAccBits and 7));
end;

function TFLACBitWriter.ByteAt(AIndex: Integer): Byte;
begin
  Result := FBuf[AIndex];
end;

// --- CRC helpers over a byte buffer ---
function CRC8(bw: TFLACBitWriter; ALen: Integer): Byte;
var i, b: Integer; crc: Byte;
begin
  crc := 0;
  for i := 0 to ALen - 1 do
  begin
    crc := crc xor bw.ByteAt(i);
    for b := 0 to 7 do
      if (crc and $80) <> 0 then crc := Byte((crc shl 1) xor $07)
      else crc := Byte(crc shl 1);
  end;
  Result := crc;
end;

function CRC16(bw: TFLACBitWriter; ALen: Integer): Word;
var i, b: Integer; crc: Word;
begin
  crc := 0;
  for i := 0 to ALen - 1 do
  begin
    crc := crc xor (Word(bw.ByteAt(i)) shl 8);
    for b := 0 to 7 do
      if (crc and $8000) <> 0 then crc := Word((crc shl 1) xor $8005)
      else crc := Word(crc shl 1);
  end;
  Result := crc;
end;

{ TSedaiFLACEncoder }

constructor TSedaiFLACEncoder.Create;
begin
  inherited Create;
  FBlockSize := 4096;
end;

function TSedaiFLACEncoder.Init(AStream: TStream; ASampleRate, AChannels, ABitsPerSample: Integer): Boolean;
var marker: array[0..3] of AnsiChar;
begin
  Result := False;
  if (ASampleRate <= 0) or (AChannels < 1) or (AChannels > 8) or
     ((ABitsPerSample <> 16) and (ABitsPerSample <> 24) and (ABitsPerSample <> 8)) then
  begin FLastError := 'FLAC encoder: unsupported format'; Exit; end;

  FStream := AStream;
  FSampleRate := ASampleRate;
  FChannels := AChannels;
  FBitsPerSample := ABitsPerSample;
  FTotalFrames := 0;
  FFrameNumber := 0;
  FAccumFrames := 0;
  FMinFrameSize := MaxInt; FMaxFrameSize := 0;
  FActualMinBlock := MaxInt; FActualMaxBlock := 0;
  SetLength(FAccum, FBlockSize * FChannels);

  marker[0] := 'f'; marker[1] := 'L'; marker[2] := 'a'; marker[3] := 'C';
  FStream.WriteBuffer(marker, 4);
  FStreamInfoOffset := FStream.Position;
  WriteStreamInfo(False);     // placeholder, patched in Finalize
  Result := True;
end;

procedure TSedaiFLACEncoder.WriteStreamInfo(AFinal: Boolean);
var
  bw: TFLACBitWriter;
  i, minB, maxB: Integer;
  buf: array of Byte;
begin
  bw := TFLACBitWriter.Create;
  try
    // METADATA_BLOCK_HEADER: last=1, type=0 (STREAMINFO), length=34.
    bw.WriteBits(1, 1);
    bw.WriteBits(0, 7);
    bw.WriteBits(34, 24);
    if AFinal then begin minB := FActualMinBlock; maxB := FActualMaxBlock; end
    else begin minB := FBlockSize; maxB := FBlockSize; end;
    bw.WriteBits(UInt32(minB), 16);
    bw.WriteBits(UInt32(maxB), 16);
    if AFinal and (FMaxFrameSize > 0) then
    begin
      bw.WriteBits(UInt32(FMinFrameSize), 24);
      bw.WriteBits(UInt32(FMaxFrameSize), 24);
    end
    else
    begin
      bw.WriteBits(0, 24);    // min frame size unknown
      bw.WriteBits(0, 24);    // max frame size unknown
    end;
    bw.WriteBits(UInt32(FSampleRate), 20);
    bw.WriteBits(UInt32(FChannels - 1), 3);
    bw.WriteBits(UInt32(FBitsPerSample - 1), 5);
    bw.WriteBits(UInt32(FTotalFrames shr 32) and $F, 4);   // total samples hi 4 bits
    bw.WriteBits(UInt32(FTotalFrames and $FFFFFFFF), 32);  // total samples lo 32 bits
    // 128-bit MD5 = 0 (not computed; permitted by the spec).
    for i := 0 to 15 do bw.WriteBits(0, 8);
    bw.AlignToByte;
    SetLength(buf, bw.Len);
    for i := 0 to bw.Len - 1 do buf[i] := bw.ByteAt(i);
    FStream.WriteBuffer(buf[0], bw.Len);
  finally
    bw.Free;
  end;
end;

// Compute the optimal 4-bit Rice parameter (0..14) and its exact bit cost for
// the residuals [AStart, AStart+ACount). Returns -1 in AParam if escape needed.
procedure BestRiceParam(const AResidual: array of Int64; AStart, ACount: Integer;
  out AParam: Integer; out ABits: Int64);
var
  i, k: Integer;
  u: UInt64;
  total, best, cost: Int64;
  sumU: UInt64;
begin
  AParam := 0; ABits := High(Int64);
  if ACount <= 0 then begin ABits := 0; Exit; end;
  // Try k = 0..14, pick the one with the least total bits.
  best := High(Int64);
  for k := 0 to 14 do
  begin
    total := Int64(ACount) * (k + 1);    // each value: k low bits + the unary 1
    for i := AStart to AStart + ACount - 1 do
    begin
      if AResidual[i] >= 0 then u := UInt64(AResidual[i]) shl 1
      else u := (UInt64(-AResidual[i]) shl 1) - 1;
      total := total + Int64(u shr k);    // unary quotient zeros
      if total >= best then Break;        // early out
    end;
    if total < best then begin best := total; AParam := k; end;
  end;
  ABits := best;
end;

procedure TSedaiFLACEncoder.WriteResidual(bw: TFLACBitWriter; const AResidual: array of Int64; AStart, ACount: Integer);
var
  k, i: Integer;
  bits: Int64;
  u: UInt64;
begin
  BestRiceParam(AResidual, AStart, ACount, k, bits);
  // residual coding method = 0 (4-bit params), partition order = 0.
  bw.WriteBits(0, 2);
  bw.WriteBits(0, 4);
  bw.WriteBits(UInt32(k), 4);
  for i := AStart to AStart + ACount - 1 do
  begin
    if AResidual[i] >= 0 then u := UInt64(AResidual[i]) shl 1
    else u := (UInt64(-AResidual[i]) shl 1) - 1;
    bw.WriteUnary(UInt32(u shr k));
    if k > 0 then bw.WriteBits(UInt32(u and ((UInt64(1) shl k) - 1)), k);
  end;
end;

// Encode one channel's samples (ACount of them) as a FLAC subframe.
procedure TSedaiFLACEncoder.EncodeSubframe(bw: TFLACBitWriter; const ASamples: array of Int32; ACount: Integer);
var
  i, order, bestOrder: Integer;
  allEqual: Boolean;
  res: array of array of Int64;        // residuals per fixed order 0..4
  bits, bestBits, verbatimBits: Int64;
  rk: Integer;
  rbits: Int64;
  o: Integer;
begin
  // CONSTANT subframe if every sample is identical.
  allEqual := True;
  for i := 1 to ACount - 1 do
    if ASamples[i] <> ASamples[0] then begin allEqual := False; Break; end;
  if allEqual then
  begin
    bw.WriteBits(0, 1);          // padding
    bw.WriteBits(0, 6);          // subframe type = CONSTANT
    bw.WriteBits(0, 1);          // no wasted bits
    bw.WriteBits(UInt32(ASamples[0]), FBitsPerSample);
    Exit;
  end;

  // Build residuals for fixed orders 0..4 (orders that fit in the block).
  SetLength(res, 5);
  for o := 0 to 4 do
  begin
    if o >= ACount then Break;
    SetLength(res[o], ACount);
    for i := 0 to ACount - 1 do
    begin
      if i < o then res[o][i] := 0
      else
        case o of
          0: res[o][i] := ASamples[i];
          1: res[o][i] := Int64(ASamples[i]) - ASamples[i-1];
          2: res[o][i] := Int64(ASamples[i]) - 2*Int64(ASamples[i-1]) + ASamples[i-2];
          3: res[o][i] := Int64(ASamples[i]) - 3*Int64(ASamples[i-1]) + 3*Int64(ASamples[i-2]) - ASamples[i-3];
          4: res[o][i] := Int64(ASamples[i]) - 4*Int64(ASamples[i-1]) + 6*Int64(ASamples[i-2]) - 4*Int64(ASamples[i-3]) + ASamples[i-4];
        end;
    end;
  end;

  // Pick the fixed order with the least estimated bit cost (warmup + residual).
  bestOrder := 0; bestBits := High(Int64);
  for o := 0 to 4 do
  begin
    if (o >= ACount) or (Length(res[o]) = 0) then Break;
    // residuals must fit a 4-bit-param Rice coding (param <= 14); else skip.
    BestRiceParam(res[o], o, ACount - o, rk, rbits);
    bits := Int64(o) * FBitsPerSample + rbits;
    if bits < bestBits then begin bestBits := bits; bestOrder := o; end;
  end;

  verbatimBits := Int64(ACount) * FBitsPerSample;

  if bestBits >= verbatimBits then
  begin
    // VERBATIM fallback.
    bw.WriteBits(0, 1);
    bw.WriteBits(1, 6);          // subframe type = VERBATIM
    bw.WriteBits(0, 1);
    for i := 0 to ACount - 1 do
      bw.WriteBits(UInt32(ASamples[i]), FBitsPerSample);
    Exit;
  end;

  order := bestOrder;
  bw.WriteBits(0, 1);            // padding
  bw.WriteBits(UInt32(8 + order), 6);  // FIXED predictor, order
  bw.WriteBits(0, 1);            // no wasted bits
  for i := 0 to order - 1 do     // warmup samples, verbatim
    bw.WriteBits(UInt32(ASamples[i]), FBitsPerSample);
  WriteResidual(bw, res[order], order, ACount - order);
end;

procedure TSedaiFLACEncoder.EncodeBlock(const AInterleaved: array of Int32; AFrames: Integer);
var
  bw: TFLACBitWriter;
  ch, i, hdrLen, frameSize: Integer;
  chan: array of Int32;
  bsCode: Integer;
  fn: Int64;
  utf: array[0..6] of Byte;
  utfLen, shift: Integer;
  crc: Word;
  c8: Byte;
  buf: array of Byte;
begin
  bw := TFLACBitWriter.Create;
  try
    // --- frame header ---
    bw.WriteBits($3FFE, 14);     // sync
    bw.WriteBits(0, 1);          // reserved
    bw.WriteBits(0, 1);          // blocking strategy = fixed block size
    if AFrames = FBlockSize then bsCode := 12   // 4096 -> table code 12
    else bsCode := 7;                            // 16-bit explicit (blocksize-1)
    bw.WriteBits(UInt32(bsCode), 4);
    bw.WriteBits(0, 4);          // sample rate: get from STREAMINFO
    bw.WriteBits(UInt32(FChannels - 1), 4);      // independent channels
    bw.WriteBits(0, 3);          // sample size: get from STREAMINFO
    bw.WriteBits(0, 1);          // reserved

    // UTF-8-coded frame number (fixed-blocksize uses the frame number).
    fn := FFrameNumber;
    if fn < $80 then begin utf[0] := Byte(fn); utfLen := 1; end
    else
    begin
      if fn < $800 then utfLen := 2
      else if fn < $10000 then utfLen := 3
      else if fn < $200000 then utfLen := 4
      else if fn < $4000000 then utfLen := 5
      else if fn < $80000000 then utfLen := 6
      else utfLen := 7;
      for i := utfLen - 1 downto 1 do
      begin utf[i] := Byte($80 or (fn and $3F)); fn := fn shr 6; end;
      shift := 8 - utfLen;     // number of leading-1 bits = utfLen
      utf[0] := Byte(($FF shl (shift)) and $FF) or Byte(fn);
    end;
    for i := 0 to utfLen - 1 do bw.WriteBits(utf[i], 8);

    if bsCode = 7 then bw.WriteBits(UInt32(AFrames - 1), 16);  // explicit block size

    // CRC-8 over the header bytes so far (header is byte-aligned here).
    c8 := CRC8(bw, bw.Len);
    bw.WriteBits(c8, 8);

    // --- subframes (independent channels) ---
    SetLength(chan, AFrames);
    for ch := 0 to FChannels - 1 do
    begin
      for i := 0 to AFrames - 1 do chan[i] := AInterleaved[i * FChannels + ch];
      EncodeSubframe(bw, chan, AFrames);
    end;

    // --- frame footer: byte align + CRC-16 over the whole frame ---
    bw.AlignToByte;
    crc := CRC16(bw, bw.Len);
    bw.WriteBits(crc, 16);

    frameSize := bw.Len;
    if frameSize < FMinFrameSize then FMinFrameSize := frameSize;
    if frameSize > FMaxFrameSize then FMaxFrameSize := frameSize;
    if AFrames < FActualMinBlock then FActualMinBlock := AFrames;
    if AFrames > FActualMaxBlock then FActualMaxBlock := AFrames;

    SetLength(buf, frameSize);
    for i := 0 to frameSize - 1 do buf[i] := bw.ByteAt(i);
    FStream.WriteBuffer(buf[0], frameSize);
    Inc(FFrameNumber);
  finally
    bw.Free;
  end;
end;

function TSedaiFLACEncoder.WriteFrames(ABuffer: PSingle; AFrameCount: Integer): Boolean;
var
  i, ch, room, take, srcIdx: Integer;
  scale: Double;
  maxv, minv, v: Int64;
  p: PSingle;
begin
  Result := True;
  scale := Int64(1) shl (FBitsPerSample - 1);
  maxv := (Int64(1) shl (FBitsPerSample - 1)) - 1;
  minv := -(Int64(1) shl (FBitsPerSample - 1));
  p := ABuffer;
  srcIdx := 0;
  while srcIdx < AFrameCount do
  begin
    room := FBlockSize - FAccumFrames;
    take := AFrameCount - srcIdx;
    if take > room then take := room;
    for i := 0 to take - 1 do
      for ch := 0 to FChannels - 1 do
      begin
        v := Round(p[(srcIdx + i) * FChannels + ch] * scale);
        if v > maxv then v := maxv else if v < minv then v := minv;
        FAccum[(FAccumFrames + i) * FChannels + ch] := Int32(v);
      end;
    Inc(FAccumFrames, take);
    Inc(srcIdx, take);
    Inc(FTotalFrames, take);
    if FAccumFrames = FBlockSize then
    begin
      EncodeBlock(FAccum, FBlockSize);
      FAccumFrames := 0;
    end;
  end;
end;

function TSedaiFLACEncoder.Finalize: Boolean;
var savePos: Int64;
begin
  Result := False;
  if FStream = nil then Exit;
  if FAccumFrames > 0 then
  begin
    EncodeBlock(FAccum, FAccumFrames);
    FAccumFrames := 0;
  end;
  // Patch STREAMINFO with the final totals (stream must be seekable).
  savePos := FStream.Position;
  try
    FStream.Position := FStreamInfoOffset;
    WriteStreamInfo(True);
    FStream.Position := savePos;
  except
    // non-seekable stream: leave the placeholder (total samples = 0, valid)
  end;
  Result := True;
end;

end.
