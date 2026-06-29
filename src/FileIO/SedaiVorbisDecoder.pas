{*
 * Sedai Audio Foundation - OGG Vorbis Decoder (work in progress)
 *
 * TSedaiVorbisDecoder decodes OGG Vorbis to interleaved float, behind the
 * common TSedaiAudioDecoder interface (like the FLAC decoder). It is a
 * clean-room implementation from the Ogg and Vorbis I specifications.
 *
 * SESSION 1 SCOPE (this file, so far):
 *   - Ogg container layer: page parsing, CRC32 validation, packet reassembly
 *     across pages and continued packets.
 *   - Vorbis LSB-first bit reader (Vorbis packs bits little-endian within a
 *     byte, the opposite of FLAC's MSB-first reader).
 *   - The three Vorbis setup packets: identification header (sample rate /
 *     channels / block sizes), comment header (vendor string, skipped),
 *     and the setup header bytes are captured for the next stage.
 *   - ReadFrames / Seek are NOT yet implemented (audio decode = next session):
 *     the codebooks / floors / residues / mapping / modes parse and the
 *     per-packet floor+residue+IMDCT+overlap-add come next.
 *
 * Reference: Xiph.Org "Ogg" + "Vorbis I specification" (public documents).
 * Acknowledgement for the eventual VQ/codebook approach: Sean Barrett's
 * public-domain stb_vorbis (to be credited in LICENSING.md once ported).
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiVorbisDecoder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioDecoder;

type
  // A reassembled Vorbis packet (one logical packet, possibly spanning pages).
  TVorbisPacket = record
    Data: array of Byte;
    Len: Integer;
    GranulePos: Int64;   // granule of the page on which this packet completed
    EndOfStream: Boolean;
  end;

  // --- Vorbis setup-header structures (codebooks/floors/residues/mappings/modes) ---

  // A codebook: a canonical Huffman code over Entries codewords, optionally with
  // a VQ lookup that maps each entry to a Dimensions-long vector of floats.
  TVorbisCodebook = record
    Dimensions: Integer;
    Entries: Integer;
    Lengths: array of Byte;        // codeword length per entry (0 = unused/no code)
    Codewords: array of LongWord;  // assigned codeword per entry, LSB-first; $FFFFFFFF = unused
    LookupType: Integer;           // 0 = none, 1 = implicitly-tiled, 2 = explicit
    SequenceP: Boolean;
    HasLookup: Boolean;
    ValueVectors: array of Single; // dequantized VQ table, Entries*Dimensions (lookup 1/2)
  end;

  // Floor curve description. Type 1 (piecewise linear over X positions) is by far
  // the common case; type 0 (LSP) is parsed for completeness but rare.
  TVorbisFloor = record
    FloorType: Integer;
    // type 1
    Partitions: Integer;
    PartitionClassList: array of Integer;
    ClassDimensions: array of Integer;
    ClassSubclasses: array of Integer;
    ClassMasterbooks: array of Integer;
    SubclassBooks: array of array of Integer;  // [class][1 shl subclasses], book index or -1
    Multiplier: Integer;
    Rangebits: Integer;
    XList: array of Integer;
    Values: Integer;               // number of X positions (incl. the two endpoints)
    // type 0 (LSP)
    Order, Rate, BarkMapSize, AmplitudeBits, AmplitudeOffset, NumberOfBooks: Integer;
    BookList: array of Integer;
  end;

  // Residue description (vector residue, decoded via the codebooks).
  TVorbisResidue = record
    ResidueType: Integer;          // 0, 1 or 2
    ResBegin: Integer;
    ResEnd: Integer;
    PartitionSize: Integer;
    Classifications: Integer;
    Classbook: Integer;
    Cascade: array of Integer;
    Books: array of array of Integer;  // [classification][8], book index or -1
  end;

  // Channel mapping: coupling steps (M/S de-coupling) + per-channel submap routing
  // to a (floor, residue) pair.
  TVorbisMapping = record
    Submaps: Integer;
    CouplingSteps: Integer;
    Magnitude: array of Integer;
    Angle: array of Integer;
    Mux: array of Integer;             // per-channel submap index
    SubmapFloor: array of Integer;
    SubmapResidue: array of Integer;
  end;

  // A mode selects block size (long/short) and the mapping used for a packet.
  TVorbisMode = record
    BlockFlag: Boolean;            // True = long block (Blocksize1)
    WindowType: Integer;
    TransformType: Integer;
    Mapping: Integer;
  end;

  { TVorbisBitReader }
  // LSB-first bit reader over a single packet's bytes. The first bit read is
  // the least-significant bit of the first byte (Vorbis bit-packing convention).
  TVorbisBitReader = class
  private
    FData: PByte;
    FLen: Integer;
    FBytePos: Integer;
    FBitPos: Integer;      // 0..7, next bit within FData[FBytePos]
    FOverrun: Boolean;     // set once a read goes past the packet end
  public
    constructor Create(AData: PByte; ALen: Integer);
    function ReadBit: LongWord;
    function ReadBits(ACount: Integer): LongWord;  // 0..32 bits, LSB-first
    function AtEnd: Boolean;
    property BytePos: Integer read FBytePos;
    property BitPos: Integer read FBitPos;
    property Overrun: Boolean read FOverrun;
  end;

  { TSedaiVorbisDecoder }
  TSedaiVorbisDecoder = class(TSedaiAudioDecoder)
  private
    FSerial: LongWord;        // logical bitstream serial number (from the BOS page)
    FHaveSerial: Boolean;
    FBlocksize0: Integer;
    FBlocksize1: Integer;
    FBitrateNominal: Integer;
    FVendor: string;          // comment-header vendor string
    FSetupPacket: array of Byte;   // raw setup header (codebooks etc.)
    FStreamStart: Int64;      // stream offset where Ogg paging begins

    // Parsed setup-header configuration (codebooks/floors/residues/mappings/modes).
    FCodebooks: array of TVorbisCodebook;
    FFloors: array of TVorbisFloor;
    FResidues: array of TVorbisResidue;
    FMappings: array of TVorbisMapping;
    FModes: array of TVorbisMode;
    FSetupParsed: Boolean;
    FSetupTrailingBytes: Integer;  // bytes left unread in the setup packet after framing bit

    // Current Ogg page state (for packet reassembly).
    FPageBody: array of Byte;
    FSegTable: array of Byte;
    FSegCount: Integer;
    FSegIdx: Integer;         // next segment to consume
    FBodyPos: Integer;        // byte offset in FPageBody for the next segment
    FPageGranule: Int64;
    FPageEOS: Boolean;
    FExhausted: Boolean;      // no more pages

    function GetSetupPacketSize: Integer;
    function LoadNextPage: Boolean;
    function ReadPacket(out APacket: TVorbisPacket): Boolean;
    function ParseIdentification(const APacket: TVorbisPacket): Boolean;
    function ParseComment(const APacket: TVorbisPacket): Boolean;

    // Setup-header parsing (codebooks/floors/residues/mappings/modes).
    function ParseSetup: Boolean;
    function DecodeCodebook(ABR: TVorbisBitReader; var ACB: TVorbisCodebook): Boolean;
    function DecodeFloor(ABR: TVorbisBitReader; var AFloor: TVorbisFloor): Boolean;
    function DecodeResidue(ABR: TVorbisBitReader; var ARes: TVorbisResidue): Boolean;
    function DecodeMapping(ABR: TVorbisBitReader; var AMap: TVorbisMapping): Boolean;

    function GetCodebookCount: Integer;
    function GetFloorCount: Integer;
    function GetResidueCount: Integer;
    function GetMappingCount: Integer;
    function GetModeCount: Integer;
  public
    function OpenStream(AStream: TStream): Boolean; override;
    function ReadFrames(ABuffer: PSingle; AFrameCount: Integer): Integer; override;
    function Seek(AFrame: Int64): Boolean; override;

    // Container self-check: read every remaining packet (validating each page's
    // CRC and the cross-page reassembly), returning the packet count and the
    // last granule position seen (= total decoded samples at EOS).
    function CountRemainingPackets(out ALastGranule: Int64): Integer;

    property Blocksize0: Integer read FBlocksize0;
    property Blocksize1: Integer read FBlocksize1;
    property BitrateNominal: Integer read FBitrateNominal;
    property Vendor: string read FVendor;
    property SetupPacketSize: Integer read GetSetupPacketSize;
    property SetupParsed: Boolean read FSetupParsed;
    property SetupTrailingBytes: Integer read FSetupTrailingBytes;
    property CodebookCount: Integer read GetCodebookCount;
    property FloorCount: Integer read GetFloorCount;
    property ResidueCount: Integer read GetResidueCount;
    property MappingCount: Integer read GetMappingCount;
    property ModeCount: Integer read GetModeCount;
  end;

implementation

var
  GOggCRCTable: array[0..255] of LongWord;
  GOggCRCReady: Boolean = False;

procedure InitOggCRC;
var
  i, j: Integer;
  r: LongWord;
begin
  // Ogg CRC: polynomial 0x04C11DB7, MSB-first, no input/output reflection,
  // init 0, no final XOR.
  for i := 0 to 255 do
  begin
    r := LongWord(i) shl 24;
    for j := 0 to 7 do
      if (r and $80000000) <> 0 then
        r := (r shl 1) xor $04C11DB7
      else
        r := r shl 1;
    GOggCRCTable[i] := r;
  end;
  GOggCRCReady := True;
end;

function OggCRC32(const ABuf: array of Byte; ALen: Integer): LongWord;
var
  i: Integer;
  crc: LongWord;
begin
  if not GOggCRCReady then InitOggCRC;
  crc := 0;
  for i := 0 to ALen - 1 do
    crc := (crc shl 8) xor GOggCRCTable[((crc shr 24) xor ABuf[i]) and $FF];
  Result := crc;
end;

function ReadU32LE(const ABuf: array of Byte; AOffset: Integer): LongWord;
begin
  Result := LongWord(ABuf[AOffset]) or (LongWord(ABuf[AOffset+1]) shl 8) or
            (LongWord(ABuf[AOffset+2]) shl 16) or (LongWord(ABuf[AOffset+3]) shl 24);
end;

// Vorbis ilog: number of bits needed to represent x (position of the highest
// set bit). ilog(0)=0, ilog(1)=1, ilog(7)=3, ilog(8)=4.
function ILog(x: Integer): Integer;
begin
  Result := 0;
  while x > 0 do
  begin
    Inc(Result);
    x := x shr 1;
  end;
end;

// Unpack a Vorbis "float32" codebook value (a custom 32-bit float layout, NOT
// IEEE-754): 21-bit mantissa, sign bit, 10-bit biased exponent (bias 788).
function Float32Unpack(x: LongWord): Single;
var
  mantissa: LongWord;
  exponent: Integer;
  val: Double;
begin
  mantissa := x and $1FFFFF;
  exponent := Integer((x and $7FE00000) shr 21);
  val := mantissa;
  if (x and $80000000) <> 0 then val := -val;
  Result := ldexp(val, exponent - 788);
end;

// lookup1_values: the largest integer r such that r^dimensions <= entries.
function Lookup1Values(AEntries, ADim: Integer): Integer;
  function PowFits(b: Integer): Boolean;   // b^ADim <= AEntries (without overflow)
  var
    i: Integer;
    acc: Int64;
  begin
    acc := 1;
    for i := 1 to ADim do
    begin
      acc := acc * b;
      if acc > AEntries then Exit(False);
    end;
    Result := True;
  end;
var
  r: Integer;
begin
  if (ADim <= 0) or (AEntries <= 0) then Exit(0);
  r := Trunc(Exp(Ln(AEntries) / ADim)) + 1;
  if r < 1 then r := 1;
  while (r > 1) and (not PowFits(r)) do Dec(r);
  while PowFits(r + 1) do Inc(r);
  Result := r;
end;

// Reverse the 32 bits of n (codeword from MSB-justified to the LSB-first form
// the bit reader matches against).
function BitReverse32(n: LongWord): LongWord;
begin
  n := ((n and $AAAAAAAA) shr 1) or ((n and $55555555) shl 1);
  n := ((n and $CCCCCCCC) shr 2) or ((n and $33333333) shl 2);
  n := ((n and $F0F0F0F0) shr 4) or ((n and $0F0F0F0F) shl 4);
  n := ((n and $FF00FF00) shr 8) or ((n and $00FF00FF) shl 8);
  Result := (n shr 16) or (n shl 16);
end;

// Assign canonical Huffman codewords from per-entry lengths (the algorithm from
// the Vorbis spec / stb_vorbis). Returns False if the implied tree is
// over-specified (a length with no free leaf). Single-entry / sparse books are
// allowed (under-specification is not rejected here).
function ComputeCodewords(var ACB: TVorbisCodebook): Boolean;
var
  available: array[0..32] of LongWord;
  i, k, z, y, n: Integer;
  res: LongWord;
begin
  n := ACB.Entries;
  SetLength(ACB.Codewords, n);
  for i := 0 to n - 1 do ACB.Codewords[i] := $FFFFFFFF;  // unused
  for i := 0 to 32 do available[i] := 0;

  // First used entry gets codeword 0 of its length; seed the free-leaf list.
  k := 0;
  while (k < n) and (ACB.Lengths[k] = 0) do Inc(k);
  if k = n then Exit(True);   // entirely unused codebook (degenerate but valid)
  if ACB.Lengths[k] > 32 then Exit(False);
  ACB.Codewords[k] := 0;
  for i := 1 to ACB.Lengths[k] do
    available[i] := LongWord(1) shl (32 - i);

  for i := k + 1 to n - 1 do
  begin
    if ACB.Lengths[i] = 0 then Continue;
    z := ACB.Lengths[i];
    if z > 32 then Exit(False);
    while (z > 0) and (available[z] = 0) do Dec(z);
    if z = 0 then Exit(False);   // over-specified: no free leaf at/below this length
    res := available[z];
    available[z] := 0;
    ACB.Codewords[i] := BitReverse32(res);
    // The chosen leaf was longer than needed: re-publish the deeper free leaves.
    if z <> ACB.Lengths[i] then
      for y := ACB.Lengths[i] downto z + 1 do
        available[y] := res + (LongWord(1) shl (32 - y));
  end;
  Result := True;
end;

{ TVorbisBitReader }

constructor TVorbisBitReader.Create(AData: PByte; ALen: Integer);
begin
  inherited Create;
  FData := AData;
  FLen := ALen;
  FBytePos := 0;
  FBitPos := 0;
  FOverrun := False;
end;

function TVorbisBitReader.ReadBit: LongWord;
begin
  if FBytePos >= FLen then
  begin
    Result := 0;   // reading past the packet end yields zero bits
    FOverrun := True;
    Exit;
  end;
  Result := (FData[FBytePos] shr FBitPos) and 1;
  Inc(FBitPos);
  if FBitPos = 8 then
  begin
    FBitPos := 0;
    Inc(FBytePos);
  end;
end;

function TVorbisBitReader.ReadBits(ACount: Integer): LongWord;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to ACount - 1 do
    Result := Result or (ReadBit shl i);   // first bit -> LSB
end;

function TVorbisBitReader.AtEnd: Boolean;
begin
  Result := FBytePos >= FLen;
end;

{ TSedaiVorbisDecoder }

function TSedaiVorbisDecoder.GetSetupPacketSize: Integer;
begin
  Result := Length(FSetupPacket);
end;

// Read one Ogg page from the stream into FPageBody/FSegTable, validating CRC.
function TSedaiVorbisDecoder.LoadNextPage: Boolean;
var
  hdr: array[0..26] of Byte;
  page: array of Byte;
  segCount, i, bodyLen, headerLen: Integer;
  serial: LongWord;
  storedCRC: LongWord;
begin
  Result := False;

  // Fixed 27-byte page header.
  if FStream.Read(hdr[0], 27) <> 27 then Exit;
  if (hdr[0] <> Ord('O')) or (hdr[1] <> Ord('g')) or
     (hdr[2] <> Ord('g')) or (hdr[3] <> Ord('S')) then
  begin
    FLastError := 'Ogg: bad page capture pattern';
    Exit;
  end;
  if hdr[4] <> 0 then
  begin
    FLastError := 'Ogg: unsupported stream structure version';
    Exit;
  end;

  segCount := hdr[26];
  headerLen := 27 + segCount;

  // Assemble the whole page (header + segment table + body) for CRC checking.
  SetLength(page, headerLen);
  Move(hdr[0], page[0], 27);
  if segCount > 0 then
    if FStream.Read(page[27], segCount) <> segCount then Exit;

  bodyLen := 0;
  for i := 0 to segCount - 1 do
    bodyLen := bodyLen + page[27 + i];

  SetLength(page, headerLen + bodyLen);
  if bodyLen > 0 then
    if FStream.Read(page[headerLen], bodyLen) <> bodyLen then Exit;

  // Validate CRC (field at bytes 22..25 is zeroed during computation).
  storedCRC := ReadU32LE(page, 22);
  page[22] := 0; page[23] := 0; page[24] := 0; page[25] := 0;
  if OggCRC32(page, Length(page)) <> storedCRC then
  begin
    FLastError := 'Ogg: page CRC mismatch';
    Exit;
  end;

  // Logical bitstream serial; lock onto the first one seen (BOS).
  serial := ReadU32LE(page, 14);
  if not FHaveSerial then
  begin
    FSerial := serial;
    FHaveSerial := True;
  end
  else if serial <> FSerial then
  begin
    // A multiplexed/chained stream: skip foreign pages by recursing.
    Result := LoadNextPage;
    Exit;
  end;

  // Granule position (signed 64-bit LE) and EOS flag.
  FPageGranule := Int64(ReadU32LE(page, 6)) or (Int64(ReadU32LE(page, 10)) shl 32);
  FPageEOS := (hdr[5] and $04) <> 0;

  // Publish the body + segment table for the packet reassembler.
  SetLength(FSegTable, segCount);
  if segCount > 0 then
    Move(page[27], FSegTable[0], segCount);
  FSegCount := segCount;
  SetLength(FPageBody, bodyLen);
  if bodyLen > 0 then
    Move(page[headerLen], FPageBody[0], bodyLen);
  FSegIdx := 0;
  FBodyPos := 0;
  Result := True;
end;

// Reassemble one logical Vorbis packet, spanning pages/continuations as needed.
function TSedaiVorbisDecoder.ReadPacket(out APacket: TVorbisPacket): Boolean;
var
  segLen, oldLen: Integer;
begin
  Result := False;
  APacket.Len := 0;
  SetLength(APacket.Data, 0);
  APacket.GranulePos := -1;
  APacket.EndOfStream := False;

  while True do
  begin
    if FSegIdx >= FSegCount then
    begin
      if FExhausted then Exit;
      if not LoadNextPage then
      begin
        FExhausted := True;
        // If we accumulated a partial packet but ran out of pages, it's invalid.
        Exit;
      end;
      Continue;
    end;

    segLen := FSegTable[FSegIdx];
    if segLen > 0 then
    begin
      oldLen := APacket.Len;
      SetLength(APacket.Data, oldLen + segLen);
      Move(FPageBody[FBodyPos], APacket.Data[oldLen], segLen);
      Inc(APacket.Len, segLen);
      Inc(FBodyPos, segLen);
    end;
    Inc(FSegIdx);

    if segLen < 255 then
    begin
      // A lacing value < 255 terminates the packet.
      APacket.GranulePos := FPageGranule;
      APacket.EndOfStream := FPageEOS and (FSegIdx >= FSegCount);
      Result := True;
      Exit;
    end;
    // segLen = 255 -> the packet continues into the next segment/page.
  end;
end;

function TSedaiVorbisDecoder.ParseIdentification(const APacket: TVorbisPacket): Boolean;
var
  br: TVorbisBitReader;
  i, packetType: Integer;
  ver, chans, srate: LongWord;
  bsByte: LongWord;
  framing: LongWord;
  ok: Boolean;
begin
  Result := False;
  if APacket.Len < 30 then
  begin
    FLastError := 'Vorbis: identification header too short';
    Exit;
  end;

  br := TVorbisBitReader.Create(@APacket.Data[0], APacket.Len);
  try
    packetType := br.ReadBits(8);
    // The 6-byte "vorbis" signature follows.
    ok := (packetType = 1);
    for i := 0 to 5 do
      if br.ReadBits(8) <> Ord('vorbis'[i + 1]) then ok := False;
    if not ok then
    begin
      FLastError := 'Vorbis: not an identification header';
      Exit;
    end;

    ver   := br.ReadBits(32);
    chans := br.ReadBits(8);
    srate := br.ReadBits(32);
    br.ReadBits(32);            // bitrate_maximum
    FBitrateNominal := Integer(br.ReadBits(32));
    br.ReadBits(32);            // bitrate_minimum
    bsByte   := br.ReadBits(8);
    framing  := br.ReadBit;

    if ver <> 0 then
    begin
      FLastError := 'Vorbis: unsupported version';
      Exit;
    end;
    if (chans = 0) or (srate = 0) then
    begin
      FLastError := 'Vorbis: invalid channels/sample rate';
      Exit;
    end;
    if framing <> 1 then
    begin
      FLastError := 'Vorbis: identification framing bit not set';
      Exit;
    end;

    FChannels := Integer(chans);
    FSampleRate := Integer(srate);
    FBitsPerSample := 16;       // informational: the decoder yields float
    FBlocksize0 := 1 shl (bsByte and $0F);
    FBlocksize1 := 1 shl ((bsByte shr 4) and $0F);

    // Sanity per the Vorbis spec: 64..8192, power of two, bs0 <= bs1.
    if (FBlocksize0 < 64) or (FBlocksize1 > 8192) or (FBlocksize0 > FBlocksize1) then
    begin
      FLastError := 'Vorbis: invalid block sizes';
      Exit;
    end;

    Result := True;
  finally
    br.Free;
  end;
end;

function TSedaiVorbisDecoder.ParseComment(const APacket: TVorbisPacket): Boolean;
var
  i, packetType, vlen: Integer;
  ok: Boolean;
begin
  Result := False;
  if APacket.Len < 7 then
  begin
    FLastError := 'Vorbis: comment header too short';
    Exit;
  end;
  packetType := APacket.Data[0];
  ok := (packetType = 3);
  for i := 0 to 5 do
    if APacket.Data[1 + i] <> Byte(Ord('vorbis'[i + 1])) then ok := False;
  if not ok then
  begin
    FLastError := 'Vorbis: not a comment header';
    Exit;
  end;

  // [7..10] vendor_length, then the vendor string. We only keep the vendor.
  if APacket.Len < 11 then Exit;
  vlen := Integer(ReadU32LE(APacket.Data, 7));
  if (vlen > 0) and (11 + vlen <= APacket.Len) then
  begin
    SetLength(FVendor, vlen);
    Move(APacket.Data[11], FVendor[1], vlen);
  end;
  Result := True;   // user comment list is intentionally ignored
end;

function TSedaiVorbisDecoder.GetCodebookCount: Integer;
begin Result := Length(FCodebooks); end;

function TSedaiVorbisDecoder.GetFloorCount: Integer;
begin Result := Length(FFloors); end;

function TSedaiVorbisDecoder.GetResidueCount: Integer;
begin Result := Length(FResidues); end;

function TSedaiVorbisDecoder.GetMappingCount: Integer;
begin Result := Length(FMappings); end;

function TSedaiVorbisDecoder.GetModeCount: Integer;
begin Result := Length(FModes); end;

// Decode one codebook configuration: the codeword lengths (ordered or sparse),
// the canonical Huffman codewords, and (if present) the dequantized VQ vectors.
function TSedaiVorbisDecoder.DecodeCodebook(ABR: TVorbisBitReader;
  var ACB: TVorbisCodebook): Boolean;
var
  sync: LongWord;
  ordered, sparse: Boolean;
  i, j, curEntry, curLen, num: Integer;
  minVal, deltaVal: Single;
  valueBits, lookupValues, idxDiv, moff: Integer;
  mult: array of LongWord;
  last, v: Double;
begin
  Result := False;

  sync := ABR.ReadBits(24);
  if sync <> $564342 then Exit;   // "BCV" codebook sync pattern
  ACB.Dimensions := Integer(ABR.ReadBits(16));
  ACB.Entries := Integer(ABR.ReadBits(24));
  if (ACB.Entries <= 0) or (ACB.Dimensions < 0) then Exit;
  SetLength(ACB.Lengths, ACB.Entries);

  ordered := ABR.ReadBit = 1;
  if not ordered then
  begin
    sparse := ABR.ReadBit = 1;
    for i := 0 to ACB.Entries - 1 do
    begin
      if sparse then
      begin
        if ABR.ReadBit = 1 then
          ACB.Lengths[i] := Byte(ABR.ReadBits(5) + 1)
        else
          ACB.Lengths[i] := 0;   // unused entry
      end
      else
        ACB.Lengths[i] := Byte(ABR.ReadBits(5) + 1);
    end;
  end
  else
  begin
    // Lengths are non-decreasing and run-length coded.
    curEntry := 0;
    curLen := Integer(ABR.ReadBits(5)) + 1;
    while curEntry < ACB.Entries do
    begin
      num := Integer(ABR.ReadBits(ILog(ACB.Entries - curEntry)));
      if curEntry + num > ACB.Entries then Exit;
      for i := curEntry to curEntry + num - 1 do
        ACB.Lengths[i] := Byte(curLen);
      Inc(curEntry, num);
      Inc(curLen);
    end;
  end;

  if not ComputeCodewords(ACB) then Exit;

  // VQ lookup.
  ACB.LookupType := Integer(ABR.ReadBits(4));
  ACB.HasLookup := False;
  if ACB.LookupType = 0 then
    // no lookup
  else if (ACB.LookupType = 1) or (ACB.LookupType = 2) then
  begin
    minVal := Float32Unpack(ABR.ReadBits(32));
    deltaVal := Float32Unpack(ABR.ReadBits(32));
    valueBits := Integer(ABR.ReadBits(4)) + 1;
    ACB.SequenceP := ABR.ReadBit = 1;
    if ACB.LookupType = 1 then
      lookupValues := Lookup1Values(ACB.Entries, ACB.Dimensions)
    else
      lookupValues := ACB.Entries * ACB.Dimensions;
    SetLength(mult, lookupValues);
    for i := 0 to lookupValues - 1 do
      mult[i] := ABR.ReadBits(valueBits);

    // Dequantize each entry's vector.
    SetLength(ACB.ValueVectors, ACB.Entries * ACB.Dimensions);
    if ACB.LookupType = 1 then
    begin
      for i := 0 to ACB.Entries - 1 do
      begin
        last := 0;
        idxDiv := 1;
        for j := 0 to ACB.Dimensions - 1 do
        begin
          moff := (i div idxDiv) mod lookupValues;
          v := mult[moff] * deltaVal + minVal + last;
          ACB.ValueVectors[i * ACB.Dimensions + j] := v;
          if ACB.SequenceP then last := v;
          idxDiv := idxDiv * lookupValues;
        end;
      end;
    end
    else  // lookup type 2: one multiplicand per (entry, dimension)
    begin
      for i := 0 to ACB.Entries - 1 do
      begin
        last := 0;
        moff := i * ACB.Dimensions;
        for j := 0 to ACB.Dimensions - 1 do
        begin
          v := mult[moff] * deltaVal + minVal + last;
          ACB.ValueVectors[i * ACB.Dimensions + j] := v;
          if ACB.SequenceP then last := v;
          Inc(moff);
        end;
      end;
    end;
    ACB.HasLookup := True;
  end
  else
    Exit;   // reserved lookup type

  Result := True;
end;

// Decode one floor configuration (type 0 = LSP, type 1 = piecewise linear).
function TSedaiVorbisDecoder.DecodeFloor(ABR: TVorbisBitReader;
  var AFloor: TVorbisFloor): Boolean;
var
  i, j, maxClass, subCount, idx, total, curClass: Integer;
begin
  Result := False;
  AFloor.FloorType := Integer(ABR.ReadBits(16));

  if AFloor.FloorType = 0 then
  begin
    AFloor.Order := Integer(ABR.ReadBits(8));
    AFloor.Rate := Integer(ABR.ReadBits(16));
    AFloor.BarkMapSize := Integer(ABR.ReadBits(16));
    AFloor.AmplitudeBits := Integer(ABR.ReadBits(6));
    AFloor.AmplitudeOffset := Integer(ABR.ReadBits(8));
    AFloor.NumberOfBooks := Integer(ABR.ReadBits(4)) + 1;
    SetLength(AFloor.BookList, AFloor.NumberOfBooks);
    for i := 0 to AFloor.NumberOfBooks - 1 do
    begin
      AFloor.BookList[i] := Integer(ABR.ReadBits(8));
      if AFloor.BookList[i] >= Length(FCodebooks) then Exit;
    end;
    Result := True;
    Exit;
  end;

  if AFloor.FloorType <> 1 then Exit;   // unknown floor type

  AFloor.Partitions := Integer(ABR.ReadBits(5));
  SetLength(AFloor.PartitionClassList, AFloor.Partitions);
  maxClass := -1;
  for i := 0 to AFloor.Partitions - 1 do
  begin
    AFloor.PartitionClassList[i] := Integer(ABR.ReadBits(4));
    if AFloor.PartitionClassList[i] > maxClass then
      maxClass := AFloor.PartitionClassList[i];
  end;

  SetLength(AFloor.ClassDimensions, maxClass + 1);
  SetLength(AFloor.ClassSubclasses, maxClass + 1);
  SetLength(AFloor.ClassMasterbooks, maxClass + 1);
  SetLength(AFloor.SubclassBooks, maxClass + 1);
  for i := 0 to maxClass do
  begin
    AFloor.ClassDimensions[i] := Integer(ABR.ReadBits(3)) + 1;
    AFloor.ClassSubclasses[i] := Integer(ABR.ReadBits(2));
    if AFloor.ClassSubclasses[i] > 0 then
    begin
      AFloor.ClassMasterbooks[i] := Integer(ABR.ReadBits(8));
      if AFloor.ClassMasterbooks[i] >= Length(FCodebooks) then Exit;
    end
    else
      AFloor.ClassMasterbooks[i] := 0;
    subCount := 1 shl AFloor.ClassSubclasses[i];
    SetLength(AFloor.SubclassBooks[i], subCount);
    for j := 0 to subCount - 1 do
    begin
      AFloor.SubclassBooks[i][j] := Integer(ABR.ReadBits(8)) - 1;  // -1 = unused
      if AFloor.SubclassBooks[i][j] >= Length(FCodebooks) then Exit;
    end;
  end;

  AFloor.Multiplier := Integer(ABR.ReadBits(2)) + 1;
  AFloor.Rangebits := Integer(ABR.ReadBits(4));

  // X positions: two endpoints (0 and 2^rangebits) plus one per class dimension.
  total := 2;
  for i := 0 to AFloor.Partitions - 1 do
    total := total + AFloor.ClassDimensions[AFloor.PartitionClassList[i]];
  SetLength(AFloor.XList, total);
  AFloor.XList[0] := 0;
  AFloor.XList[1] := 1 shl AFloor.Rangebits;
  idx := 2;
  for i := 0 to AFloor.Partitions - 1 do
  begin
    curClass := AFloor.PartitionClassList[i];
    for j := 0 to AFloor.ClassDimensions[curClass] - 1 do
    begin
      AFloor.XList[idx] := Integer(ABR.ReadBits(AFloor.Rangebits));
      Inc(idx);
    end;
  end;
  AFloor.Values := total;
  Result := True;
end;

// Decode one residue configuration (types 0/1/2 share the same setup layout).
function TSedaiVorbisDecoder.DecodeResidue(ABR: TVorbisBitReader;
  var ARes: TVorbisResidue): Boolean;
var
  i, j, highBits, lowBits: Integer;
begin
  Result := False;
  ARes.ResidueType := Integer(ABR.ReadBits(16));
  if ARes.ResidueType > 2 then Exit;

  ARes.ResBegin := Integer(ABR.ReadBits(24));
  ARes.ResEnd := Integer(ABR.ReadBits(24));
  ARes.PartitionSize := Integer(ABR.ReadBits(24)) + 1;
  ARes.Classifications := Integer(ABR.ReadBits(6)) + 1;
  ARes.Classbook := Integer(ABR.ReadBits(8));
  if ARes.Classbook >= Length(FCodebooks) then Exit;

  SetLength(ARes.Cascade, ARes.Classifications);
  for i := 0 to ARes.Classifications - 1 do
  begin
    highBits := 0;
    lowBits := Integer(ABR.ReadBits(3));
    if ABR.ReadBit = 1 then
      highBits := Integer(ABR.ReadBits(5));
    ARes.Cascade[i] := highBits * 8 + lowBits;
  end;

  SetLength(ARes.Books, ARes.Classifications);
  for i := 0 to ARes.Classifications - 1 do
  begin
    SetLength(ARes.Books[i], 8);
    for j := 0 to 7 do
      if (ARes.Cascade[i] and (1 shl j)) <> 0 then
      begin
        ARes.Books[i][j] := Integer(ABR.ReadBits(8));
        if ARes.Books[i][j] >= Length(FCodebooks) then Exit;
      end
      else
        ARes.Books[i][j] := -1;
  end;
  Result := True;
end;

// Decode one channel mapping (type 0): coupling steps + per-channel routing to a
// (floor, residue) submap.
function TSedaiVorbisDecoder.DecodeMapping(ABR: TVorbisBitReader;
  var AMap: TVorbisMapping): Boolean;
var
  i, mappingType, couplingBits: Integer;
begin
  Result := False;
  mappingType := Integer(ABR.ReadBits(16));
  if mappingType <> 0 then Exit;

  if ABR.ReadBit = 1 then
    AMap.Submaps := Integer(ABR.ReadBits(4)) + 1
  else
    AMap.Submaps := 1;

  if ABR.ReadBit = 1 then
  begin
    AMap.CouplingSteps := Integer(ABR.ReadBits(8)) + 1;
    SetLength(AMap.Magnitude, AMap.CouplingSteps);
    SetLength(AMap.Angle, AMap.CouplingSteps);
    couplingBits := ILog(FChannels - 1);
    for i := 0 to AMap.CouplingSteps - 1 do
    begin
      AMap.Magnitude[i] := Integer(ABR.ReadBits(couplingBits));
      AMap.Angle[i] := Integer(ABR.ReadBits(couplingBits));
      if (AMap.Magnitude[i] = AMap.Angle[i]) or
         (AMap.Magnitude[i] >= FChannels) or (AMap.Angle[i] >= FChannels) then Exit;
    end;
  end
  else
    AMap.CouplingSteps := 0;

  if ABR.ReadBits(2) <> 0 then Exit;   // reserved field must be zero

  SetLength(AMap.Mux, FChannels);
  if AMap.Submaps > 1 then
  begin
    for i := 0 to FChannels - 1 do
    begin
      AMap.Mux[i] := Integer(ABR.ReadBits(4));
      if AMap.Mux[i] >= AMap.Submaps then Exit;
    end;
  end
  else
    for i := 0 to FChannels - 1 do AMap.Mux[i] := 0;

  SetLength(AMap.SubmapFloor, AMap.Submaps);
  SetLength(AMap.SubmapResidue, AMap.Submaps);
  for i := 0 to AMap.Submaps - 1 do
  begin
    ABR.ReadBits(8);   // discard the (unused) time-configuration placeholder
    AMap.SubmapFloor[i] := Integer(ABR.ReadBits(8));
    AMap.SubmapResidue[i] := Integer(ABR.ReadBits(8));
    if AMap.SubmapFloor[i] >= Length(FFloors) then Exit;
    if AMap.SubmapResidue[i] >= Length(FResidues) then Exit;
  end;
  Result := True;
end;

// Parse the whole setup header from FSetupPacket: codebooks, time-transform
// placeholders, floors, residues, mappings, modes, and the trailing framing bit.
// Consuming exactly the packet (framing bit set, no overrun) is the correctness
// signal for the bit layout.
function TSedaiVorbisDecoder.ParseSetup: Boolean;
var
  br: TVorbisBitReader;
  i, count, timeCount: Integer;
begin
  Result := False;
  FSetupParsed := False;
  if Length(FSetupPacket) < 7 then
  begin FLastError := 'Vorbis: setup header too short'; Exit; end;

  br := TVorbisBitReader.Create(@FSetupPacket[0], Length(FSetupPacket));
  try
    if br.ReadBits(8) <> 5 then
    begin FLastError := 'Vorbis: not a setup header'; Exit; end;
    for i := 0 to 5 do
      if br.ReadBits(8) <> Byte(Ord('vorbis'[i + 1])) then
      begin FLastError := 'Vorbis: bad setup signature'; Exit; end;

    // Codebooks.
    count := Integer(br.ReadBits(8)) + 1;
    SetLength(FCodebooks, count);
    for i := 0 to count - 1 do
      if not DecodeCodebook(br, FCodebooks[i]) then
      begin FLastError := Format('Vorbis: bad codebook %d', [i]); Exit; end;

    // Time-domain transforms: placeholders, every value must be zero.
    timeCount := Integer(br.ReadBits(6)) + 1;
    for i := 0 to timeCount - 1 do
      if br.ReadBits(16) <> 0 then
      begin FLastError := 'Vorbis: nonzero time transform'; Exit; end;

    // Floors.
    count := Integer(br.ReadBits(6)) + 1;
    SetLength(FFloors, count);
    for i := 0 to count - 1 do
      if not DecodeFloor(br, FFloors[i]) then
      begin FLastError := Format('Vorbis: bad floor %d', [i]); Exit; end;

    // Residues.
    count := Integer(br.ReadBits(6)) + 1;
    SetLength(FResidues, count);
    for i := 0 to count - 1 do
      if not DecodeResidue(br, FResidues[i]) then
      begin FLastError := Format('Vorbis: bad residue %d', [i]); Exit; end;

    // Mappings.
    count := Integer(br.ReadBits(6)) + 1;
    SetLength(FMappings, count);
    for i := 0 to count - 1 do
      if not DecodeMapping(br, FMappings[i]) then
      begin FLastError := Format('Vorbis: bad mapping %d', [i]); Exit; end;

    // Modes.
    count := Integer(br.ReadBits(6)) + 1;
    SetLength(FModes, count);
    for i := 0 to count - 1 do
    begin
      FModes[i].BlockFlag := br.ReadBit = 1;
      FModes[i].WindowType := Integer(br.ReadBits(16));
      FModes[i].TransformType := Integer(br.ReadBits(16));
      FModes[i].Mapping := Integer(br.ReadBits(8));
      if (FModes[i].WindowType <> 0) or (FModes[i].TransformType <> 0) then
      begin FLastError := 'Vorbis: bad mode window/transform'; Exit; end;
      if FModes[i].Mapping >= Length(FMappings) then
      begin FLastError := 'Vorbis: mode mapping out of range'; Exit; end;
    end;

    // Trailing framing bit must be set, and we must not have read past the end.
    if br.ReadBit <> 1 then
    begin FLastError := 'Vorbis: setup framing bit not set'; Exit; end;
    if br.Overrun then
    begin FLastError := 'Vorbis: setup header overran packet'; Exit; end;

    // Bytes left after the (partial) framing byte = setup padding. A correct
    // parse leaves only the current byte's remainder; large leftovers signal a
    // bit-layout error.
    FSetupTrailingBytes := Length(FSetupPacket) - br.BytePos -
      Ord(br.BitPos > 0);
    FSetupParsed := True;
    Result := True;
  finally
    br.Free;
  end;
end;

function TSedaiVorbisDecoder.OpenStream(AStream: TStream): Boolean;
var
  pkt: TVorbisPacket;
begin
  Result := False;
  FStream := AStream;
  FHaveSerial := False;
  FExhausted := False;
  FSegCount := 0; FSegIdx := 0; FBodyPos := 0;
  FStreamStart := AStream.Position;
  FPosition := 0;
  FSeekable := False;   // seeking arrives with audio decode
  FTotalFrames := 0;    // unknown until we scan the last page (later)

  // Header packet 1: identification.
  if not ReadPacket(pkt) then begin FLastError := 'Vorbis: no identification packet'; Exit; end;
  if not ParseIdentification(pkt) then Exit;

  // Header packet 2: comment.
  if not ReadPacket(pkt) then begin FLastError := 'Vorbis: no comment packet'; Exit; end;
  if not ParseComment(pkt) then Exit;

  // Header packet 3: setup (codebooks/floors/residues/mappings/modes). Captured
  // now; parsing + audio decode is the next stage.
  if not ReadPacket(pkt) then begin FLastError := 'Vorbis: no setup packet'; Exit; end;
  if (pkt.Len < 7) or (pkt.Data[0] <> 5) then
  begin
    FLastError := 'Vorbis: not a setup header';
    Exit;
  end;
  SetLength(FSetupPacket, pkt.Len);
  if pkt.Len > 0 then
    Move(pkt.Data[0], FSetupPacket[0], pkt.Len);

  // Parse the setup header (codebooks/floors/residues/mappings/modes). Audio
  // decode (floor/residue/IMDCT/overlap-add) is the next stage.
  if not ParseSetup then Exit;

  Result := True;
end;

function TSedaiVorbisDecoder.ReadFrames(ABuffer: PSingle; AFrameCount: Integer): Integer;
begin
  // Audio decode (setup-header parse + floor/residue/IMDCT/overlap-add) is the
  // next session's work. Headers parse and validate today.
  Result := 0;
  FLastError := 'Vorbis: audio decode not yet implemented';
end;

function TSedaiVorbisDecoder.Seek(AFrame: Int64): Boolean;
begin
  Result := False;
  FLastError := 'Vorbis: seek not yet implemented';
end;

function TSedaiVorbisDecoder.CountRemainingPackets(out ALastGranule: Int64): Integer;
var
  pkt: TVorbisPacket;
  n: Integer;
begin
  n := 0;
  ALastGranule := -1;
  while ReadPacket(pkt) do
  begin
    Inc(n);
    if pkt.GranulePos >= 0 then
      ALastGranule := pkt.GranulePos;
  end;
  Result := n;
end;

initialization
  InitOggCRC;

end.
