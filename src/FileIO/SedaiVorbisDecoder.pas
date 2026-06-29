{*
 * Sedai Audio Foundation - OGG Vorbis Decoder (work in progress)
 *
 * TSedaiVorbisDecoder decodes OGG Vorbis to interleaved float, behind the
 * common TSedaiAudioDecoder interface (like the FLAC decoder). It is a
 * clean-room implementation from the Ogg and Vorbis I specifications (no
 * upstream decoder source vendored).
 *
 * Pipeline:
 *   - Ogg container: page parsing, CRC32 validation, packet reassembly across
 *     pages and continued packets, granulepos tracking.
 *   - Vorbis LSB-first bit reader (Vorbis packs bits little-endian within a
 *     byte, the opposite of FLAC's MSB-first reader).
 *   - Setup-header parse: codebooks (canonical Huffman codewords + VQ lookup
 *     type 1/2), floor type 0/1, residue 0/1/2, channel mapping, modes.
 *   - Per-packet audio decode: mode/window selection -> floor1 curve decode ->
 *     residue (VQ) decode -> inverse channel coupling -> floor multiply ->
 *     IMDCT -> windowed overlap-add -> interleaved float PCM. Output is
 *     trimmed to the final-page granulepos. Seek is not yet implemented.
 *
 * The MDCT pair uses modulus M = N/2 (bin k -> (k+0.5)/N cycles/sample); the
 * decoder applies no 1/N scaling (libvorbis folds it into the forward MDCT).
 *
 * References: Xiph.Org "Ogg bitstream" and "Vorbis I specification" (public
 * documents); the floor1_inverse_dB table is the constant given in the spec.
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
    // Decode acceleration: sorted (length,codeword) -> entry, for the bit reader.
    DecodeKeys: array of QWord;    // sorted keys = (QWord(len) shl 32) or codeword_low_bits
    DecodeVals: array of Integer;  // parallel entry index
    MaxLen: Integer;               // longest codeword length
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

    // --- Audio decode state (per-packet pipeline + overlap-add) ---
    FPrevTail: array of array of Single;  // [ch] windowed right tail of the previous block
    FPrevTailLen: Integer;                // length of each FPrevTail[ch]
    FFirstAudio: Boolean;                 // first audio packet primes the overlap (no output)
    FCosTab0, FCosTab1: array of Single;  // IMDCT cosine tables for bs0 / bs1
    FAudioStarted: Boolean;               // decode tables/buffers initialized
    // Output FIFO (interleaved float), filled a packet at a time, drained by ReadFrames.
    FOutBuf: array of Single;
    FOutHead: Integer;                    // index of next sample to emit (in samples, not frames)
    FOutLen: Integer;                     // valid samples in FOutBuf
    FDecodeEOS: Boolean;                  // saw the end-of-stream packet
    FFinalGranule: Int64;                 // total sample frames (last page granulepos), -1 = unknown

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

    // Audio decode pipeline.
    procedure InitAudioDecode;
    function DecodeScalar(ABR: TVorbisBitReader; const ACB: TVorbisCodebook): Integer;
    procedure DecodeFloor1Packet(ABR: TVorbisBitReader; const AFloor: TVorbisFloor;
      var AY: array of Integer; out AUnused: Boolean);
    procedure RenderFloor1(const AFloor: TVorbisFloor; const AY: array of Integer;
      ACurve: PSingle; AHalf: Integer);
    procedure DecodeResiduePacket(ABR: TVorbisBitReader; const ARes: TVorbisResidue;
      ACh: Integer; const ADoNotDecode: array of Boolean;
      AVectors: array of PSingle; AHalf: Integer);
    procedure IMDCT(const ACosTab: array of Single; AInput, AOutput: PSingle; AN: Integer);
    function DecodeAudioPacket(const APacket: TVorbisPacket): Boolean;
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

// Build the sorted (length,codeword)->entry decode table for a codebook so the
// bit reader can resolve a symbol in O(maxlen * log entries).
procedure BuildCodebookDecode(var ACB: TVorbisCodebook);
var
  i, used: Integer;
  // simple in-place quicksort over the parallel (keys, vals) arrays
  procedure QSort(lo, hi: Integer);
  var
    a, b: Integer;
    pivot, tk: QWord;
    tv: Integer;
  begin
    a := lo; b := hi;
    pivot := ACB.DecodeKeys[(lo + hi) div 2];
    while a <= b do
    begin
      while ACB.DecodeKeys[a] < pivot do Inc(a);
      while ACB.DecodeKeys[b] > pivot do Dec(b);
      if a <= b then
      begin
        tk := ACB.DecodeKeys[a]; ACB.DecodeKeys[a] := ACB.DecodeKeys[b]; ACB.DecodeKeys[b] := tk;
        tv := ACB.DecodeVals[a]; ACB.DecodeVals[a] := ACB.DecodeVals[b]; ACB.DecodeVals[b] := tv;
        Inc(a); Dec(b);
      end;
    end;
    if lo < b then QSort(lo, b);
    if a < hi then QSort(a, hi);
  end;
begin
  used := 0;
  ACB.MaxLen := 0;
  for i := 0 to ACB.Entries - 1 do
    if ACB.Lengths[i] > 0 then
    begin
      Inc(used);
      if ACB.Lengths[i] > ACB.MaxLen then ACB.MaxLen := ACB.Lengths[i];
    end;
  SetLength(ACB.DecodeKeys, used);
  SetLength(ACB.DecodeVals, used);
  used := 0;
  for i := 0 to ACB.Entries - 1 do
    if ACB.Lengths[i] > 0 then
    begin
      ACB.DecodeKeys[used] := (QWord(ACB.Lengths[i]) shl 32) or
        (QWord(ACB.Codewords[i]) and ((QWord(1) shl ACB.Lengths[i]) - 1));
      ACB.DecodeVals[used] := i;
      Inc(used);
    end;
  if used > 1 then QSort(0, used - 1);
end;

// floor1 amplitude -> linear value lookup (Vorbis spec inverse-dB table, 256 entries).
const
  FLOOR1_INVERSE_DB: array[0..255] of Single = (
    1.0649863e-07, 1.1341951e-07, 1.2079015e-07, 1.2863978e-07,
    1.3699951e-07, 1.4590251e-07, 1.5538408e-07, 1.6548181e-07,
    1.7623575e-07, 1.8768855e-07, 1.9988561e-07, 2.1287530e-07,
    2.2670913e-07, 2.4144197e-07, 2.5713223e-07, 2.7384213e-07,
    2.9163793e-07, 3.1059021e-07, 3.3077411e-07, 3.5226968e-07,
    3.7516214e-07, 3.9954229e-07, 4.2550680e-07, 4.5315863e-07,
    4.8260743e-07, 5.1396998e-07, 5.4737065e-07, 5.8294187e-07,
    6.2082472e-07, 6.6116941e-07, 7.0413592e-07, 7.4989464e-07,
    7.9862701e-07, 8.5052630e-07, 9.0579828e-07, 9.6466216e-07,
    1.0273513e-06, 1.0941144e-06, 1.1652161e-06, 1.2409384e-06,
    1.3215816e-06, 1.4074654e-06, 1.4989305e-06, 1.5963394e-06,
    1.7000785e-06, 1.8105592e-06, 1.9282195e-06, 2.0535261e-06,
    2.1869758e-06, 2.3290978e-06, 2.4804557e-06, 2.6416497e-06,
    2.8133190e-06, 2.9961443e-06, 3.1908506e-06, 3.3982101e-06,
    3.6190449e-06, 3.8542308e-06, 4.1047004e-06, 4.3714470e-06,
    4.6555282e-06, 4.9580707e-06, 5.2802740e-06, 5.6234160e-06,
    5.9888572e-06, 6.3780469e-06, 6.7925283e-06, 7.2339451e-06,
    7.7040476e-06, 8.2047000e-06, 8.7378876e-06, 9.3057248e-06,
    9.9104632e-06, 1.0554501e-05, 1.1240392e-05, 1.1970856e-05,
    1.2748789e-05, 1.3577278e-05, 1.4459606e-05, 1.5399272e-05,
    1.6400004e-05, 1.7465768e-05, 1.8600792e-05, 1.9809576e-05,
    2.1096914e-05, 2.2467911e-05, 2.3928002e-05, 2.5482978e-05,
    2.7139006e-05, 2.8902651e-05, 3.0780908e-05, 3.2781225e-05,
    3.4911534e-05, 3.7180282e-05, 3.9596466e-05, 4.2169667e-05,
    4.4910090e-05, 4.7828601e-05, 5.0936773e-05, 5.4246931e-05,
    5.7772202e-05, 6.1526565e-05, 6.5524908e-05, 6.9783085e-05,
    7.4317983e-05, 7.9147585e-05, 8.4291040e-05, 8.9768747e-05,
    9.5602426e-05, 0.00010181521, 0.00010843174, 0.00011547824,
    0.00012298267, 0.00013097477, 0.00013948625, 0.00014855085,
    0.00015820453, 0.00016848555, 0.00017943469, 0.00019109536,
    0.00020351382, 0.00021673929, 0.00023082423, 0.00024582449,
    0.00026179955, 0.00027881276, 0.00029693158, 0.00031622787,
    0.00033677814, 0.00035866388, 0.00038197188, 0.00040679456,
    0.00043323036, 0.00046138411, 0.00049136745, 0.00052329927,
    0.00055730621, 0.00059352311, 0.00063209358, 0.00067317058,
    0.00071691700, 0.00076350630, 0.00081312324, 0.00086596457,
    0.00092223983, 0.00098217216, 0.0010459992,  0.0011139742,
    0.0011863665,  0.0012634633,  0.0013455702,  0.0014330129,
    0.0015261382,  0.0016253153,  0.0017309374,  0.0018434235,
    0.0019632195,  0.0020908006,  0.0022266726,  0.0023713743,
    0.0025254795,  0.0026895994,  0.0028643847,  0.0030505286,
    0.0032487691,  0.0034598925,  0.0036847358,  0.0039241906,
    0.0041792066,  0.0044507950,  0.0047400328,  0.0050480668,
    0.0053761186,  0.0057254891,  0.0060975636,  0.0064938176,
    0.0069158225,  0.0073652516,  0.0078438871,  0.0083536271,
    0.0088964928,  0.0094746183,  0.010090352,   0.010746080,
    0.011444421,   0.012188144,   0.012980198,   0.013823725,
    0.014722068,   0.015678791,   0.016697687,   0.017782797,
    0.018938423,   0.020169149,   0.021479854,   0.022875735,
    0.024362330,   0.025945531,   0.027631618,   0.029427276,
    0.031339626,   0.033376252,   0.035545228,   0.037855157,
    0.040315199,   0.042935108,   0.045725273,   0.048696758,
    0.051861348,   0.055231591,   0.058820850,   0.062643361,
    0.066714279,   0.071049749,   0.075666962,   0.080584227,
    0.085821044,   0.091398179,   0.097337747,   0.10366330,
    0.11039993,    0.11757434,    0.12521498,    0.13335215,
    0.14201813,    0.15124727,    0.16107617,    0.17154380,
    0.18269168,    0.19456402,    0.20720788,    0.22067342,
    0.23501402,    0.25028656,    0.26655159,    0.28387361,
    0.30232132,    0.32196786,    0.34289114,    0.36517414,
    0.38890521,    0.41417847,    0.44109412,    0.46975890,
    0.50028648,    0.53279791,    0.56742212,    0.60429640,
    0.64356699,    0.68538959,    0.72993007,    0.77736504,
    0.82788260,    0.88168307,    0.93847990,    1.00000000
  );

// floor1 low/high neighbor search over X[0..AIdx-1] (spec 7.2.1).
function LowNeighbor(const AX: array of Integer; AIdx: Integer): Integer;
var i, best: Integer;
begin
  Result := 0; best := -1;
  for i := 0 to AIdx - 1 do
    if (AX[i] < AX[AIdx]) and (AX[i] > best) then
    begin best := AX[i]; Result := i; end;
end;

function HighNeighbor(const AX: array of Integer; AIdx: Integer): Integer;
var i, best: Integer;
begin
  Result := 0; best := MaxInt;
  for i := 0 to AIdx - 1 do
    if (AX[i] > AX[AIdx]) and (AX[i] < best) then
    begin best := AX[i]; Result := i; end;
end;

// floor1 integer line-point interpolation (spec 7.2.1).
function RenderPoint(x0, y0, x1, y1, x: Integer): Integer;
var dy, adx, ady, err, off: Integer;
begin
  dy := y1 - y0; adx := x1 - x0; ady := Abs(dy);
  err := ady * (x - x0);
  off := err div adx;
  if dy < 0 then Result := y0 - off else Result := y0 + off;
end;

// floor1 line render: write the inverse-dB curve value for each x in [x0,x1)
// into ACurve (spec 7.2.1 render_line). y indexes FLOOR1_INVERSE_DB (clamped).
procedure RenderLine(x0, y0, x1, y1: Integer; ACurve: PSingle; AN: Integer);
var dy, adx, ady, base, sy, x, y, err, yi: Integer;
begin
  dy := y1 - y0; adx := x1 - x0;
  ady := Abs(dy); base := dy div adx;
  if dy < 0 then sy := base - 1 else sy := base + 1;
  ady := ady - Abs(base) * adx;
  y := y0; err := 0;
  if x1 > AN then x1 := AN;
  if x0 < AN then
  begin
    yi := y0; if yi < 0 then yi := 0 else if yi > 255 then yi := 255;
    ACurve[x0] := FLOOR1_INVERSE_DB[yi];
  end;
  x := x0 + 1;
  while x < x1 do
  begin
    err := err + ady;
    if err >= adx then begin err := err - adx; y := y + sy; end
    else y := y + base;
    yi := y; if yi < 0 then yi := 0 else if yi > 255 then yi := 255;
    ACurve[x] := FLOOR1_INVERSE_DB[yi];
    Inc(x);
  end;
end;

// Build the Vorbis synthesis window over [0,n) with the given slope boundaries
// (spec 1.3.2). Rising slope on [ls,le), flat 1 on [le,rs), falling on [rs,re).
procedure BuildVorbisWindow(AWin: PSingle; ANs, ALS, ALE, ARS, ARE: Integer);
var i, lsz, rsz: Integer; s: Double;
begin
  lsz := ALE - ALS; rsz := ARE - ARS;
  for i := 0 to ALS - 1 do AWin[i] := 0;
  for i := ALS to ALE - 1 do
  begin
    s := Sin((i - ALS + 0.5) / lsz * (PI / 2));
    AWin[i] := Sin((PI / 2) * s * s);
  end;
  for i := ALE to ARS - 1 do AWin[i] := 1;
  for i := ARS to ARE - 1 do
  begin
    s := Sin((i - ARS + 0.5) / rsz * (PI / 2) + (PI / 2));
    AWin[i] := Sin((PI / 2) * s * s);
  end;
  for i := ARE to ANs - 1 do AWin[i] := 0;
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

  BuildCodebookDecode(ACB);
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

// Build the IMDCT cosine tables and the overlap-add state. Called lazily on the
// first ReadFrames after the headers are parsed.
procedure TSedaiVorbisDecoder.InitAudioDecode;
var
  j, ch: Integer;
begin
  if FAudioStarted then Exit;
  // Table length = 8*M = 4*blocksize, entry j = cos(pi*j/(4M)) = cos(pi*j/(2*blocksize)).
  SetLength(FCosTab0, 4 * FBlocksize0);
  for j := 0 to 4 * FBlocksize0 - 1 do
    FCosTab0[j] := Cos(PI * j / (2 * FBlocksize0));
  SetLength(FCosTab1, 4 * FBlocksize1);
  for j := 0 to 4 * FBlocksize1 - 1 do
    FCosTab1[j] := Cos(PI * j / (2 * FBlocksize1));
  SetLength(FPrevTail, FChannels);
  for ch := 0 to FChannels - 1 do SetLength(FPrevTail[ch], 0);
  FPrevTailLen := 0;
  FFirstAudio := True;
  FOutHead := 0;
  FOutLen := 0;
  FDecodeEOS := False;
  FFinalGranule := -1;
  FAudioStarted := True;
end;

// Resolve one Huffman symbol from the bitstream against a codebook's sorted
// (length,codeword) decode table. Returns the entry index, or -1 on error.
function TSedaiVorbisDecoder.DecodeScalar(ABR: TVorbisBitReader;
  const ACB: TVorbisCodebook): Integer;
var
  code: LongWord;
  len, lo, hi, mid: Integer;
  key: QWord;
begin
  Result := -1;
  code := 0;
  for len := 1 to ACB.MaxLen do
  begin
    code := code or (ABR.ReadBit shl (len - 1));
    if ABR.Overrun then Exit(-1);
    key := (QWord(len) shl 32) or code;
    lo := 0; hi := High(ACB.DecodeKeys);
    while lo <= hi do
    begin
      mid := (lo + hi) div 2;
      if ACB.DecodeKeys[mid] = key then Exit(ACB.DecodeVals[mid])
      else if ACB.DecodeKeys[mid] < key then lo := mid + 1
      else hi := mid - 1;
    end;
  end;
end;

// Decode a floor type-1 packet into the per-post Y values; AUnused is True if
// this floor (hence channel) carries no audio in this packet.
procedure TSedaiVorbisDecoder.DecodeFloor1Packet(ABR: TVorbisBitReader;
  const AFloor: TVorbisFloor; var AY: array of Integer; out AUnused: Boolean);
const
  RANGE_TBL: array[1..4] of Integer = (256, 128, 86, 64);
var
  range, ilogRange, i, j, cls, cdim, cbits, csub, cval, book, offset: Integer;
begin
  AUnused := False;
  if ABR.ReadBit = 0 then begin AUnused := True; Exit; end;
  range := RANGE_TBL[AFloor.Multiplier];
  ilogRange := ILog(range - 1);
  AY[0] := Integer(ABR.ReadBits(ilogRange));
  AY[1] := Integer(ABR.ReadBits(ilogRange));
  offset := 2;
  for i := 0 to AFloor.Partitions - 1 do
  begin
    cls := AFloor.PartitionClassList[i];
    cdim := AFloor.ClassDimensions[cls];
    cbits := AFloor.ClassSubclasses[cls];
    csub := (1 shl cbits) - 1;
    cval := 0;
    if cbits > 0 then
      cval := DecodeScalar(ABR, FCodebooks[AFloor.ClassMasterbooks[cls]]);
    for j := 0 to cdim - 1 do
    begin
      book := AFloor.SubclassBooks[cls][cval and csub];
      cval := cval shr cbits;
      if book >= 0 then
        AY[offset + j] := DecodeScalar(ABR, FCodebooks[book])
      else
        AY[offset + j] := 0;
    end;
    offset := offset + cdim;
  end;
end;

// Synthesize the floor type-1 curve (linear in the inverse-dB domain) into
// ACurve[0..AHalf-1] from the decoded Y values (spec 7.2.4).
procedure TSedaiVorbisDecoder.RenderFloor1(const AFloor: TVorbisFloor;
  const AY: array of Integer; ACurve: PSingle; AHalf: Integer);
const
  RANGE_TBL: array[1..4] of Integer = (256, 128, 86, 64);
var
  values, i, j, range, mult, lowOff, highOff, pred, val, highroom, lowroom, room: Integer;
  hx, hy, lx, ly, tmp, yi: Integer;
  finalY: array of Integer;
  step2: array of Boolean;
  order: array of Integer;
begin
  values := AFloor.Values;
  range := RANGE_TBL[AFloor.Multiplier];
  mult := AFloor.Multiplier;
  SetLength(finalY, values);
  SetLength(step2, values);

  // Step 1: amplitude value computation with low/high-neighbor prediction.
  step2[0] := True; step2[1] := True;
  finalY[0] := AY[0]; finalY[1] := AY[1];
  for i := 2 to values - 1 do
  begin
    lowOff := LowNeighbor(AFloor.XList, i);
    highOff := HighNeighbor(AFloor.XList, i);
    pred := RenderPoint(AFloor.XList[lowOff], finalY[lowOff],
                        AFloor.XList[highOff], finalY[highOff], AFloor.XList[i]);
    val := AY[i];
    highroom := range - pred;
    lowroom := pred;
    if highroom < lowroom then room := highroom * 2 else room := lowroom * 2;
    if val <> 0 then
    begin
      step2[lowOff] := True; step2[highOff] := True; step2[i] := True;
      if val >= room then
      begin
        if highroom > lowroom then finalY[i] := val - lowroom + pred
        else finalY[i] := pred - val + highroom - 1;
      end
      else
      begin
        if (val and 1) = 1 then finalY[i] := pred - ((val + 1) div 2)
        else finalY[i] := pred + (val div 2);
      end;
    end
    else
    begin
      step2[i] := False;
      finalY[i] := pred;
    end;
  end;

  // Clamp amplitudes into the valid range.
  for i := 0 to values - 1 do
  begin
    if finalY[i] < 0 then finalY[i] := 0
    else if finalY[i] > range - 1 then finalY[i] := range - 1;
  end;

  // Sort posts by X (insertion sort; values is small).
  SetLength(order, values);
  for i := 0 to values - 1 do order[i] := i;
  for i := 1 to values - 1 do
  begin
    tmp := order[i]; j := i - 1;
    while (j >= 0) and (AFloor.XList[order[j]] > AFloor.XList[tmp]) do
    begin order[j + 1] := order[j]; Dec(j); end;
    order[j + 1] := tmp;
  end;

  // Step 2: render the curve as connected line segments through the step2 posts.
  hx := 0; hy := 0;
  lx := 0; ly := finalY[order[0]] * mult;
  for i := 1 to values - 1 do
  begin
    j := order[i];
    if step2[j] then
    begin
      hy := finalY[j] * mult;
      hx := AFloor.XList[j];
      RenderLine(lx, ly, hx, hy, ACurve, AHalf);
      lx := hx; ly := hy;
    end;
  end;
  // Fill any remaining samples past the last post with the last value.
  if hx < AHalf then
  begin
    yi := ly; if yi < 0 then yi := 0 else if yi > 255 then yi := 255;
    for i := hx to AHalf - 1 do ACurve[i] := FLOOR1_INVERSE_DB[yi];
  end;
end;

// Decode a residue packet (types 0/1/2) for ACh channels, adding the decoded
// residue into AVectors[ch][0..AHalf-1] (do-not-decode channels are skipped).
procedure TSedaiVorbisDecoder.DecodeResiduePacket(ABR: TVorbisBitReader;
  const ARes: TVorbisResidue; ACh: Integer; const ADoNotDecode: array of Boolean;
  AVectors: array of PSingle; AHalf: Integer);

  // Decode one partition's VQ vectors into AVec at offset AOff.
  procedure DecodePartition(const ABook: TVorbisCodebook; ALayout0: Boolean;
    APartSize: Integer; AVec: PSingle; AOff: Integer);
  var
    dim, step, i, j, entry: Integer;
  begin
    dim := ABook.Dimensions;
    if dim <= 0 then Exit;
    if ALayout0 then
    begin
      step := APartSize div dim;
      for i := 0 to step - 1 do
      begin
        entry := DecodeScalar(ABR, ABook);
        if entry < 0 then Exit;
        for j := 0 to dim - 1 do
          AVec[AOff + i + j * step] := AVec[AOff + i + j * step] +
            ABook.ValueVectors[entry * dim + j];
      end;
    end
    else
    begin
      i := 0;
      while i < APartSize do
      begin
        entry := DecodeScalar(ABR, ABook);
        if entry < 0 then Exit;
        for j := 0 to dim - 1 do
        begin
          AVec[AOff + i] := AVec[AOff + i] + ABook.ValueVectors[entry * dim + j];
          Inc(i);
          if i >= APartSize then Break;
        end;
      end;
    end;
  end;

  // Core residue 0/1 decode over nch vectors of length vlen.
  procedure DoResidue01(rtype, nch, vlen: Integer; vecs: array of PSingle;
    dnd: array of Boolean);
  var
    classwords, nToRead, partitionsToRead, partSize, rbegin, rend: Integer;
    pass, pc, i, ch, temp, vqbook: Integer;
    classifications: array of array of Integer;
    layout0: Boolean;
  begin
    partSize := ARes.PartitionSize;
    rbegin := ARes.ResBegin; rend := ARes.ResEnd;
    if rend > vlen then rend := vlen;
    if rbegin > rend then rbegin := rend;
    nToRead := rend - rbegin;
    if nToRead <= 0 then Exit;
    classwords := FCodebooks[ARes.Classbook].Dimensions;
    if classwords <= 0 then Exit;
    partitionsToRead := nToRead div partSize;
    layout0 := (rtype = 0);
    SetLength(classifications, nch);
    for ch := 0 to nch - 1 do
      SetLength(classifications[ch], partitionsToRead + classwords);
    for pass := 0 to 7 do
    begin
      pc := 0;
      while pc < partitionsToRead do
      begin
        if pass = 0 then
          for ch := 0 to nch - 1 do
            if not dnd[ch] then
            begin
              temp := DecodeScalar(ABR, FCodebooks[ARes.Classbook]);
              if temp < 0 then Exit;
              for i := classwords - 1 downto 0 do
              begin
                classifications[ch][pc + i] := temp mod ARes.Classifications;
                temp := temp div ARes.Classifications;
              end;
            end;
        i := 0;
        while (i < classwords) and (pc < partitionsToRead) do
        begin
          for ch := 0 to nch - 1 do
            if not dnd[ch] then
            begin
              vqbook := ARes.Books[classifications[ch][pc]][pass];
              if vqbook >= 0 then
                DecodePartition(FCodebooks[vqbook], layout0, partSize,
                  vecs[ch], rbegin + pc * partSize);
            end;
          Inc(pc); Inc(i);
        end;
      end;
    end;
  end;

var
  i, ch: Integer;
  allDND: Boolean;
  combined: array of Single;
  cvec: array of PSingle;
  cdnd: array of Boolean;
begin
  if ARes.ResidueType = 2 then
  begin
    allDND := True;
    for ch := 0 to ACh - 1 do if not ADoNotDecode[ch] then allDND := False;
    if allDND then Exit;
    SetLength(combined, ACh * AHalf);
    for i := 0 to ACh * AHalf - 1 do combined[i] := 0;
    SetLength(cvec, 1); cvec[0] := @combined[0];
    SetLength(cdnd, 1); cdnd[0] := False;
    DoResidue01(1, 1, ACh * AHalf, cvec, cdnd);   // type 2 uses the type-1 layout
    // De-interleave the combined vector back into the per-channel vectors.
    for i := 0 to AHalf - 1 do
      for ch := 0 to ACh - 1 do
        AVectors[ch][i] := combined[i * ACh + ch];
  end
  else
    DoResidue01(ARes.ResidueType, ACh, AHalf, AVectors, ADoNotDecode);
end;

// Inverse MDCT: M = AN/2 spectral coefficients -> AN time samples, the standard
// MDCT pair x[n] = (2/N) sum_k X[k] cos[(pi/M)(n+1/2+M/2)(k+1/2)] with M = N/2
// (the modulus is the bin count, so bin k maps to (k+0.5)/N cycles/sample).
// Direct cosine form via a precomputed cos table; ACosTab[j] = cos(pi*j/(4M)).
// No 1/N scaling here: libvorbis folds the normalization into the forward MDCT,
// so the decoder's cosine sum (windowed + overlap-added) already yields PCM.
procedure TSedaiVorbisDecoder.IMDCT(const ACosTab: array of Single;
  AInput, AOutput: PSingle; AN: Integer);
var
  M, P, n, k, idx, stepN, baseI: Integer;
  acc: Double;
begin
  M := AN div 2;
  P := 8 * M;
  for n := 0 to AN - 1 do
  begin
    baseI := 2 * n + 1 + M;
    idx := baseI mod P;
    stepN := (2 * baseI) mod P;
    acc := 0;
    for k := 0 to M - 1 do
    begin
      acc := acc + AInput[k] * ACosTab[idx];
      idx := idx + stepN;
      if idx >= P then idx := idx - P;
    end;
    AOutput[n] := acc;
  end;
end;

// Decode a single audio packet: mode/window, floor decode, residue decode,
// inverse coupling, floor multiply, IMDCT, window + overlap-add, emit PCM.
function TSedaiVorbisDecoder.DecodeAudioPacket(const APacket: TVorbisPacket): Boolean;
var
  br: TVorbisBitReader;
  modeNum, n, half, i, k, ch, submap, cnt, outFrames, base: Integer;
  blockflag, prevFlag, nextFlag: Boolean;
  leftStart, leftEnd, rightStart, rightEnd: Integer;
  mapping: TVorbisMapping;
  zeroCh, floorUnused: array of Boolean;
  floorY: array of array of Integer;
  residue, timeBuf: array of array of Single;
  curve, win: array of Single;
  submapCh: array of Integer;
  submapPtrs: array of PSingle;
  submapDND: array of Boolean;
  m, a: Single;
  mag, ang: Integer;
begin
  Result := False;
  if APacket.Len < 1 then Exit;

  br := TVorbisBitReader.Create(@APacket.Data[0], APacket.Len);
  try
    if br.ReadBit <> 0 then Exit;   // not an audio packet
    modeNum := Integer(br.ReadBits(ILog(Length(FModes) - 1)));
    if modeNum >= Length(FModes) then Exit;
    blockflag := FModes[modeNum].BlockFlag;
    if blockflag then n := FBlocksize1 else n := FBlocksize0;
    if blockflag then
    begin
      prevFlag := br.ReadBit = 1;
      nextFlag := br.ReadBit = 1;
    end
    else begin prevFlag := False; nextFlag := False; end;
    half := n div 2;

    if blockflag and (not prevFlag) then
    begin leftStart := (n - FBlocksize0) div 4; leftEnd := (n + FBlocksize0) div 4; end
    else begin leftStart := 0; leftEnd := half; end;
    if blockflag and (not nextFlag) then
    begin rightStart := (n * 3 - FBlocksize0) div 4; rightEnd := (n * 3 + FBlocksize0) div 4; end
    else begin rightStart := half; rightEnd := n; end;

    mapping := FMappings[FModes[modeNum].Mapping];

    SetLength(zeroCh, FChannels);
    SetLength(floorUnused, FChannels);
    SetLength(floorY, FChannels);
    SetLength(residue, FChannels);
    SetLength(timeBuf, FChannels);
    SetLength(curve, half);
    for ch := 0 to FChannels - 1 do
    begin
      SetLength(residue[ch], half);
      SetLength(timeBuf[ch], n);
      for i := 0 to half - 1 do residue[ch][i] := 0;
    end;

    // 1) Floor decode per channel.
    for ch := 0 to FChannels - 1 do
    begin
      submap := mapping.Mux[ch];
      SetLength(floorY[ch], FFloors[mapping.SubmapFloor[submap]].Values);
      DecodeFloor1Packet(br, FFloors[mapping.SubmapFloor[submap]], floorY[ch], floorUnused[ch]);
      zeroCh[ch] := floorUnused[ch];
    end;

    // 2) Propagate non-zero across coupling pairs.
    for i := 0 to mapping.CouplingSteps - 1 do
      if (not zeroCh[mapping.Magnitude[i]]) or (not zeroCh[mapping.Angle[i]]) then
      begin
        zeroCh[mapping.Magnitude[i]] := False;
        zeroCh[mapping.Angle[i]] := False;
      end;

    // 3) Residue decode per submap.
    for submap := 0 to mapping.Submaps - 1 do
    begin
      cnt := 0;
      SetLength(submapCh, FChannels);
      for ch := 0 to FChannels - 1 do
        if mapping.Mux[ch] = submap then begin submapCh[cnt] := ch; Inc(cnt); end;
      if cnt = 0 then Continue;
      SetLength(submapPtrs, cnt);
      SetLength(submapDND, cnt);
      for i := 0 to cnt - 1 do
      begin
        submapPtrs[i] := @residue[submapCh[i]][0];
        submapDND[i] := zeroCh[submapCh[i]];
      end;
      DecodeResiduePacket(br, FResidues[mapping.SubmapResidue[submap]],
        cnt, submapDND, submapPtrs, half);
    end;

    // 4) Inverse coupling (reverse order, square-polar).
    for i := mapping.CouplingSteps - 1 downto 0 do
    begin
      mag := mapping.Magnitude[i];
      ang := mapping.Angle[i];
      for k := 0 to half - 1 do
      begin
        m := residue[mag][k];
        a := residue[ang][k];
        if m > 0 then
        begin
          if a > 0 then begin residue[mag][k] := m; residue[ang][k] := m - a; end
          else begin residue[ang][k] := m; residue[mag][k] := m + a; end;
        end
        else
        begin
          if a > 0 then begin residue[mag][k] := m; residue[ang][k] := m + a; end
          else begin residue[ang][k] := m; residue[mag][k] := m - a; end;
        end;
      end;
    end;

    // 5) Floor multiply (channels whose floor was unused stay silent).
    for ch := 0 to FChannels - 1 do
    begin
      if floorUnused[ch] then
      begin
        for i := 0 to half - 1 do residue[ch][i] := 0;
      end
      else
      begin
        submap := mapping.Mux[ch];
        RenderFloor1(FFloors[mapping.SubmapFloor[submap]], floorY[ch], @curve[0], half);
        for i := 0 to half - 1 do residue[ch][i] := residue[ch][i] * curve[i];
      end;
    end;

    // 6) IMDCT per channel.
    for ch := 0 to FChannels - 1 do
      if blockflag then IMDCT(FCosTab1, @residue[ch][0], @timeBuf[ch][0], n)
      else IMDCT(FCosTab0, @residue[ch][0], @timeBuf[ch][0], n);

    // 7) Window + overlap-add.
    SetLength(win, n);
    BuildVorbisWindow(@win[0], n, leftStart, leftEnd, rightStart, rightEnd);
    for ch := 0 to FChannels - 1 do
      for i := 0 to n - 1 do timeBuf[ch][i] := timeBuf[ch][i] * win[i];

    if not FFirstAudio then
      for ch := 0 to FChannels - 1 do
        for i := 0 to FPrevTailLen - 1 do
          if leftStart + i < n then
            timeBuf[ch][leftStart + i] := timeBuf[ch][leftStart + i] + FPrevTail[ch][i];

    // Emit the finalized region [leftStart, rightStart) (nothing on the first packet).
    if FFirstAudio then outFrames := 0
    else outFrames := rightStart - leftStart;
    if outFrames > 0 then
    begin
      base := FOutLen;
      SetLength(FOutBuf, FOutLen + outFrames * FChannels);
      for i := 0 to outFrames - 1 do
        for ch := 0 to FChannels - 1 do
          FOutBuf[base + i * FChannels + ch] := timeBuf[ch][leftStart + i];
      FOutLen := FOutLen + outFrames * FChannels;
    end;

    // Save the new right tail for the next packet's overlap.
    FPrevTailLen := n - rightStart;
    for ch := 0 to FChannels - 1 do
    begin
      SetLength(FPrevTail[ch], FPrevTailLen);
      for i := 0 to FPrevTailLen - 1 do
        FPrevTail[ch][i] := timeBuf[ch][rightStart + i];
    end;
    FFirstAudio := False;
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
var
  produced, avail, take: Integer;
  pkt: TVorbisPacket;
begin
  if not FAudioStarted then InitAudioDecode;
  produced := 0;
  while produced < AFrameCount do
  begin
    // Stop once we have emitted the stream's full sample count (granulepos trim).
    if (FFinalGranule >= 0) and (FPosition + produced >= FFinalGranule) then Break;

    // Drain whatever is already decoded in the interleaved FIFO.
    if FOutHead < FOutLen then
    begin
      avail := (FOutLen - FOutHead) div FChannels;
      take := AFrameCount - produced;
      if take > avail then take := avail;
      // Clamp to the final granule so trailing overlap padding is not emitted.
      if (FFinalGranule >= 0) and (FPosition + produced + take > FFinalGranule) then
        take := FFinalGranule - (FPosition + produced);
      if take > 0 then
      begin
        Move(FOutBuf[FOutHead], ABuffer[produced * FChannels],
          take * FChannels * SizeOf(Single));
        FOutHead := FOutHead + take * FChannels;
        produced := produced + take;
      end;
      if FOutHead >= FOutLen then begin FOutHead := 0; FOutLen := 0; end;
      Continue;
    end;

    // FIFO empty: decode the next audio packet.
    if FDecodeEOS then Break;
    if not ReadPacket(pkt) then begin FDecodeEOS := True; Break; end;
    if pkt.EndOfStream then
    begin
      FDecodeEOS := True;                 // decode this last packet, then stop
      if pkt.GranulePos >= 0 then FFinalGranule := pkt.GranulePos;
    end;
    FOutHead := 0; FOutLen := 0;
    if not DecodeAudioPacket(pkt) then
    begin
      if FDecodeEOS then Break;
      Continue;   // skip an undecodable packet
    end;
  end;
  FPosition := FPosition + produced;
  Result := produced;
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
