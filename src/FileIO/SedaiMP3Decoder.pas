{*
 * Sedai Audio Foundation - MP3 Decoder
 *
 * TSedaiMP3Decoder decodes MPEG-1/2/2.5 Layer III to interleaved float, behind
 * the common TSedaiAudioDecoder interface (like the FLAC / Vorbis decoders).
 *
 * This is a faithful pure-Pascal port of the public-domain (CC0) single-file
 * decoder "minimp3" by Lieff (https://github.com/lieff/minimp3) — Layer III
 * only, float output, scalar (no SIMD). The constant tables in
 * SedaiMP3Tables.inc are extracted verbatim from minimp3. MP3 decoding is
 * patent-unencumbered worldwide since 2017.
 *
 * Acknowledgement: Lieff and the minimp3 contributors (CC0 / public domain).
 *
 * The whole stream is decoded into an interleaved float buffer at open, so
 * ReadFrames/Seek are exact and TotalFrames is known.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiMP3Decoder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioDecoder;

type
  { TSedaiMP3Decoder }
  TSedaiMP3Decoder = class(TSedaiAudioDecoder)
  private
    FPCM: array of Single;       // whole-file decoded PCM, interleaved
    FFrameCount: Int64;          // total sample frames
    function DecodeAll: Boolean;
  public
    function OpenStream(AStream: TStream): Boolean; override;
    function ReadFrames(ABuffer: PSingle; AFrameCount: Integer): Integer; override;
    function Seek(AFrame: Int64): Boolean; override;
  end;

implementation

{$R-}{$Q-}   // C-style pointer/overflow semantics for the minimp3 port

const
  MAX_FREE_FORMAT_FRAME_SIZE = 2304;
  MAX_FRAME_SYNC_MATCHES     = 10;
  MAX_L3_FRAME_PAYLOAD_BYTES = MAX_FREE_FORMAT_FRAME_SIZE;
  MAX_BITRESERVOIR_BYTES     = 511;
  SHORT_BLOCK_TYPE           = 2;
  STOP_BLOCK_TYPE            = 3;
  MODE_MONO                  = 3;
  MODE_JOINT_STEREO          = 1;
  HDR_SIZE                   = 4;
  BITS_DEQUANTIZER_OUT       = -1;
  MAX_SCF                    = 255 + BITS_DEQUANTIZER_OUT*4 - 210;   // 41
  MAX_SCFI                   = (MAX_SCF + 3) and (not 3);            // 44
  MINIMP3_MAX_SAMPLES_PER_FRAME = 1152*2;

{$I SedaiMP3Tables.inc}

type
  PL3GrInfo = ^TL3GrInfo;
  TL3GrInfo = record
    Sfbtab: PByte;
    Part23Length, BigValues, ScalefacCompress: Word;
    GlobalGain, BlockType, MixedBlockFlag, NLongSfb, NShortSfb: Byte;
    TableSelect: array[0..2] of Byte;
    RegionCount: array[0..2] of Byte;
    SubblockGain: array[0..2] of Byte;
    Preflag, ScalefacScale, Count1Table, Scfsi: Byte;
  end;

  TMP3BitReader = record
    Buf: PByte;
    Pos, Limit: Integer;
  end;

  TMP3Core = record
    MdctOverlap: array[0..1, 0..9*32-1] of Single;
    QmfState: array[0..15*2*32-1] of Single;
    Reserv, FreeFormatBytes: Integer;
    Header: array[0..3] of Byte;
    ReservBuf: array[0..510] of Byte;
  end;

  TMP3FrameInfo = record
    FrameBytes, FrameOffset, Channels, Hz, Layer, BitrateKbps: Integer;
  end;

  TMP3Scratch = record
    Bs: TMP3BitReader;
    Maindata: array[0..MAX_BITRESERVOIR_BYTES + MAX_L3_FRAME_PAYLOAD_BYTES - 1] of Byte;
    GrInfo: array[0..3] of TL3GrInfo;
    Grbuf: array[0..2*576-1] of Single;     // grbuf[0] and grbuf[1] contiguous
    Scf: array[0..39] of Single;
    Syn: array[0..(18+15)*2*32-1] of Single;
    IstPos: array[0..1, 0..38] of Byte;
  end;

// --- arithmetic shift right (C ">>" on signed values) ---
function ASR(v: LongInt; n: Integer): LongInt; inline;
begin
  if v >= 0 then Result := LongInt(LongWord(v) shr n)
  else Result := LongInt(not (LongWord(not v) shr n));
end;

// --- bit reader ---
procedure BsInit(out bs: TMP3BitReader; data: PByte; bytes: Integer);
begin
  bs.Buf := data;
  bs.Pos := 0;
  bs.Limit := bytes*8;
end;

function GetBits(var bs: TMP3BitReader; n: Integer): LongWord;
var
  next, cache: LongWord;
  s, shl_: Integer;
  p: PByte;
begin
  cache := 0;
  s := bs.Pos and 7;
  shl_ := n + s;
  p := bs.Buf + (bs.Pos shr 3);
  bs.Pos := bs.Pos + n;
  if bs.Pos > bs.Limit then Exit(0);
  next := p^ and (255 shr s); Inc(p);
  while True do
  begin
    Dec(shl_, 8);
    if shl_ <= 0 then Break;
    cache := cache or (next shl shl_);
    next := p^; Inc(p);
  end;
  Result := cache or (next shr (-shl_));
end;

// --- header helpers (h points to a 4-byte frame header) ---
function HDR_IS_MONO(h: PByte): Boolean; inline; begin Result := (h[3] and $C0) = $C0; end;
function HDR_IS_MS_STEREO(h: PByte): Boolean; inline; begin Result := (h[3] and $E0) = $60; end;
function HDR_IS_FREE_FORMAT(h: PByte): Boolean; inline; begin Result := (h[2] and $F0) = 0; end;
function HDR_IS_CRC(h: PByte): Boolean; inline; begin Result := (h[1] and 1) = 0; end;
function HDR_TEST_PADDING(h: PByte): Boolean; inline; begin Result := (h[2] and 2) <> 0; end;
function HDR_TEST_MPEG1(h: PByte): Boolean; inline; begin Result := (h[1] and 8) <> 0; end;
function HDR_TEST_NOT_MPEG25(h: PByte): Boolean; inline; begin Result := (h[1] and $10) <> 0; end;
function HDR_TEST_I_STEREO(h: PByte): Boolean; inline; begin Result := (h[3] and $10) <> 0; end;
function HDR_TEST_MS_STEREO(h: PByte): Boolean; inline; begin Result := (h[3] and $20) <> 0; end;
function HDR_GET_STEREO_MODE(h: PByte): Integer; inline; begin Result := (h[3] shr 6) and 3; end;
function HDR_GET_STEREO_MODE_EXT(h: PByte): Integer; inline; begin Result := (h[3] shr 4) and 3; end;
function HDR_GET_LAYER(h: PByte): Integer; inline; begin Result := (h[1] shr 1) and 3; end;
function HDR_GET_BITRATE(h: PByte): Integer; inline; begin Result := h[2] shr 4; end;
function HDR_GET_SAMPLE_RATE(h: PByte): Integer; inline; begin Result := (h[2] shr 2) and 3; end;
function HDR_GET_MY_SAMPLE_RATE(h: PByte): Integer; inline;
begin Result := HDR_GET_SAMPLE_RATE(h) + (((h[1] shr 3) and 1) + ((h[1] shr 4) and 1))*3; end;
function HDR_IS_FRAME_576(h: PByte): Boolean; inline; begin Result := (h[1] and 14) = 2; end;
function HDR_IS_LAYER_1(h: PByte): Boolean; inline; begin Result := (h[1] and 6) = 6; end;

function hdr_valid(h: PByte): Boolean;
begin
  Result := (h[0] = $ff) and
    (((h[1] and $F0) = $f0) or ((h[1] and $FE) = $e2)) and
    (HDR_GET_LAYER(h) <> 0) and
    (HDR_GET_BITRATE(h) <> 15) and
    (HDR_GET_SAMPLE_RATE(h) <> 3);
end;

function hdr_compare(h1, h2: PByte): Boolean;
begin
  Result := hdr_valid(h2) and
    (((h1[1] xor h2[1]) and $FE) = 0) and
    (((h1[2] xor h2[2]) and $0C) = 0) and
    (HDR_IS_FREE_FORMAT(h1) = HDR_IS_FREE_FORMAT(h2));
end;

function hdr_bitrate_kbps(h: PByte): Integer;
const
  halfrate: array[0..1, 0..2, 0..14] of Byte = (
    ( ( 0,4,8,12,16,20,24,28,32,40,48,56,64,72,80 ), ( 0,4,8,12,16,20,24,28,32,40,48,56,64,72,80 ), ( 0,16,24,28,32,40,48,56,64,72,80,88,96,112,128 ) ),
    ( ( 0,16,20,24,28,32,40,48,56,64,80,96,112,128,160 ), ( 0,16,24,28,32,40,48,56,64,80,96,112,128,160,192 ), ( 0,16,32,48,64,80,96,112,128,144,160,176,192,208,224 ) )
  );
begin
  Result := 2*halfrate[Ord(HDR_TEST_MPEG1(h)), HDR_GET_LAYER(h) - 1, HDR_GET_BITRATE(h)];
end;

function hdr_sample_rate_hz(h: PByte): Integer;
const g_hz: array[0..2] of Integer = (44100, 48000, 32000);
begin
  Result := g_hz[HDR_GET_SAMPLE_RATE(h)] shr Ord(not HDR_TEST_MPEG1(h)) shr Ord(not HDR_TEST_NOT_MPEG25(h));
end;

function hdr_frame_samples(h: PByte): Integer;
begin
  if HDR_IS_LAYER_1(h) then Result := 384
  else Result := 1152 shr Ord(HDR_IS_FRAME_576(h));
end;

function hdr_frame_bytes(h: PByte; free_format_size: Integer): Integer;
var fb: Integer;
begin
  fb := hdr_frame_samples(h)*hdr_bitrate_kbps(h)*125 div hdr_sample_rate_hz(h);
  if HDR_IS_LAYER_1(h) then fb := fb and (not 3);
  if fb <> 0 then Result := fb else Result := free_format_size;
end;

function hdr_padding(h: PByte): Integer;
begin
  if HDR_TEST_PADDING(h) then
  begin if HDR_IS_LAYER_1(h) then Result := 4 else Result := 1; end
  else Result := 0;
end;

// --- Layer III side info ---
function L3_read_side_info(var bs: TMP3BitReader; gr: PL3GrInfo; hdr: PByte): Integer;
var
  tables, scfsi: LongWord;
  main_data_begin, part_23_sum, sr_idx, gr_count: Integer;
  gp: PL3GrInfo;
begin
  scfsi := 0; part_23_sum := 0;
  sr_idx := HDR_GET_MY_SAMPLE_RATE(hdr); Dec(sr_idx, Ord(sr_idx <> 0));
  if HDR_IS_MONO(hdr) then gr_count := 1 else gr_count := 2;

  if HDR_TEST_MPEG1(hdr) then
  begin
    gr_count := gr_count*2;
    main_data_begin := Integer(GetBits(bs, 9));
    scfsi := GetBits(bs, 7 + gr_count);
  end
  else
    main_data_begin := Integer(GetBits(bs, 8 + gr_count) shr gr_count);

  gp := gr;
  repeat
    if HDR_IS_MONO(hdr) then scfsi := scfsi shl 4;
    gp^.Part23Length := Word(GetBits(bs, 12));
    Inc(part_23_sum, gp^.Part23Length);
    gp^.BigValues := Word(GetBits(bs, 9));
    if gp^.BigValues > 288 then Exit(-1);
    gp^.GlobalGain := Byte(GetBits(bs, 8));
    if HDR_TEST_MPEG1(hdr) then gp^.ScalefacCompress := Word(GetBits(bs, 4))
    else gp^.ScalefacCompress := Word(GetBits(bs, 9));
    gp^.Sfbtab := @MP3_SCF_LONG[sr_idx*23];
    gp^.NLongSfb := 22;
    gp^.NShortSfb := 0;
    if GetBits(bs, 1) <> 0 then
    begin
      gp^.BlockType := Byte(GetBits(bs, 2));
      if gp^.BlockType = 0 then Exit(-1);
      gp^.MixedBlockFlag := Byte(GetBits(bs, 1));
      gp^.RegionCount[0] := 7;
      gp^.RegionCount[1] := 255;
      if gp^.BlockType = SHORT_BLOCK_TYPE then
      begin
        scfsi := scfsi and $0F0F;
        if gp^.MixedBlockFlag = 0 then
        begin
          gp^.RegionCount[0] := 8;
          gp^.Sfbtab := @MP3_SCF_SHORT[sr_idx*40];
          gp^.NLongSfb := 0;
          gp^.NShortSfb := 39;
        end
        else
        begin
          gp^.Sfbtab := @MP3_SCF_MIXED[sr_idx*40];
          if HDR_TEST_MPEG1(hdr) then gp^.NLongSfb := 8 else gp^.NLongSfb := 6;
          gp^.NShortSfb := 30;
        end;
      end;
      tables := GetBits(bs, 10);
      tables := tables shl 5;
      gp^.SubblockGain[0] := Byte(GetBits(bs, 3));
      gp^.SubblockGain[1] := Byte(GetBits(bs, 3));
      gp^.SubblockGain[2] := Byte(GetBits(bs, 3));
    end
    else
    begin
      gp^.BlockType := 0;
      gp^.MixedBlockFlag := 0;
      tables := GetBits(bs, 15);
      gp^.RegionCount[0] := Byte(GetBits(bs, 4));
      gp^.RegionCount[1] := Byte(GetBits(bs, 3));
      gp^.RegionCount[2] := 255;
    end;
    gp^.TableSelect[0] := Byte(tables shr 10);
    gp^.TableSelect[1] := Byte((tables shr 5) and 31);
    gp^.TableSelect[2] := Byte(tables and 31);
    if HDR_TEST_MPEG1(hdr) then gp^.Preflag := Byte(GetBits(bs, 1))
    else gp^.Preflag := Byte(Ord(gp^.ScalefacCompress >= 500));
    gp^.ScalefacScale := Byte(GetBits(bs, 1));
    gp^.Count1Table := Byte(GetBits(bs, 1));
    gp^.Scfsi := Byte((scfsi shr 12) and 15);
    scfsi := scfsi shl 4;
    Inc(gp);
    Dec(gr_count);
  until gr_count = 0;

  if part_23_sum + bs.Pos > bs.Limit + main_data_begin*8 then Exit(-1);
  Result := main_data_begin;
end;

// --- scalefactors ---
procedure L3_read_scalefactors(scf, ist_pos, scf_size, scf_count: PByte; var bs: TMP3BitReader; scfsi: Integer);
var
  i, k, cnt, bits, max_scf, s: Integer;
begin
  i := 0;
  while (i < 4) and (scf_count[i] <> 0) do
  begin
    cnt := scf_count[i];
    if (scfsi and 8) <> 0 then
    begin
      Move(ist_pos^, scf^, cnt);
    end
    else
    begin
      bits := scf_size[i];
      if bits = 0 then
      begin
        FillChar(scf^, cnt, 0);
        FillChar(ist_pos^, cnt, 0);
      end
      else
      begin
        if scfsi < 0 then max_scf := (1 shl bits) - 1 else max_scf := -1;
        for k := 0 to cnt - 1 do
        begin
          s := Integer(GetBits(bs, bits));
          if s = max_scf then ist_pos[k] := Byte(-1) else ist_pos[k] := Byte(s);
          scf[k] := Byte(s);
        end;
      end;
    end;
    Inc(ist_pos, cnt);
    Inc(scf, cnt);
    Inc(i);
    scfsi := scfsi*2;
  end;
  scf[0] := 0; scf[1] := 0; scf[2] := 0;
end;

function L3_ldexp_q2(y: Single; exp_q2: Integer): Single;
var e: Integer;
begin
  repeat
    e := Min(30*4, exp_q2);
    y := y * MP3_EXPFRAC[e and 3] * ((LongWord(1) shl 30) shr (e shr 2));
    Dec(exp_q2, e);
  until exp_q2 <= 0;
  Result := y;
end;

procedure L3_decode_scalefactors(hdr: PByte; ist_pos: PByte; var bs: TMP3BitReader;
  gr: PL3GrInfo; scf: PSingle; ch: Integer);
var
  scf_partition: PByte;
  scf_size: array[0..3] of Byte;
  iscf: array[0..39] of Byte;
  i, scf_shift, gain_exp, scfsi, sh, part, k, modprod, sfc, ist: Integer;
  gain: Single;
begin
  scf_partition := @MP3_SCF_PARTITIONS[(Ord(gr^.NShortSfb <> 0) + Ord(gr^.NLongSfb = 0))*28];
  scf_shift := gr^.ScalefacScale + 1;
  scfsi := gr^.Scfsi;

  if HDR_TEST_MPEG1(hdr) then
  begin
    part := MP3_SCFC_DECODE[gr^.ScalefacCompress];
    scf_size[1] := Byte(part shr 2); scf_size[0] := scf_size[1];
    scf_size[3] := Byte(part and 3); scf_size[2] := scf_size[3];
  end
  else
  begin
    ist := Ord(HDR_TEST_I_STEREO(hdr) and (ch <> 0));
    sfc := gr^.ScalefacCompress shr ist;
    k := ist*3*4;
    while sfc >= 0 do
    begin
      modprod := 1;
      for i := 3 downto 0 do
      begin
        scf_size[i] := Byte(sfc div modprod mod MP3_MOD[k + i]);
        modprod := modprod * MP3_MOD[k + i];
      end;
      Dec(sfc, modprod);
      Inc(k, 4);
    end;
    Inc(scf_partition, k);
    scfsi := -16;
  end;
  L3_read_scalefactors(@iscf[0], ist_pos, @scf_size[0], scf_partition, bs, scfsi);

  if gr^.NShortSfb <> 0 then
  begin
    sh := 3 - scf_shift;
    i := 0;
    while i < gr^.NShortSfb do
    begin
      iscf[gr^.NLongSfb + i + 0] := Byte(iscf[gr^.NLongSfb + i + 0] + (gr^.SubblockGain[0] shl sh));
      iscf[gr^.NLongSfb + i + 1] := Byte(iscf[gr^.NLongSfb + i + 1] + (gr^.SubblockGain[1] shl sh));
      iscf[gr^.NLongSfb + i + 2] := Byte(iscf[gr^.NLongSfb + i + 2] + (gr^.SubblockGain[2] shl sh));
      Inc(i, 3);
    end;
  end
  else if gr^.Preflag <> 0 then
  begin
    for i := 0 to 9 do
      iscf[11 + i] := Byte(iscf[11 + i] + MP3_PREAMP[i]);
  end;

  gain_exp := gr^.GlobalGain + BITS_DEQUANTIZER_OUT*4 - 210 - (Ord(HDR_IS_MS_STEREO(hdr))*2);
  gain := L3_ldexp_q2(1 shl (MAX_SCFI div 4), MAX_SCFI - gain_exp);
  for i := 0 to Integer(gr^.NLongSfb + gr^.NShortSfb) - 1 do
    scf[i] := L3_ldexp_q2(gain, iscf[i] shl scf_shift);
end;

function L3_pow_43(x: Integer): Single;
var frac: Single; sign, mult: Integer;
begin
  mult := 256;
  if x < 129 then Exit(MP3_POW43[16 + x]);
  if x < 1024 then begin mult := 16; x := x shl 3; end;
  sign := 2*x and 64;
  frac := Single((x and 63) - sign) / ((x and (not 63)) + sign);
  Result := MP3_POW43[16 + ((x + sign) shr 6)] * (1.0 + frac*((4.0/3) + frac*(2.0/9))) * mult;
end;

// --- Layer III Huffman spectral decode ---
procedure L3_huffman(dst: PSingle; var bs: TMP3BitReader; gr_info: PL3GrInfo; scf: PSingle; layer3gr_limit: Integer);
var
  one: Single;
  ireg, big_val_cnt, tab_num, sfb_cnt, linbits, cbOfs: Integer;
  np, pairs_to_decode, j, w, leaf, lsb, sh: Integer;
  sfb: PByte;
  bs_next: Integer;
  bs_cache: LongWord;
  bs_sh: Integer;
  countTab: PByte;

  function PEEK(n: Integer): LongWord; inline; begin Result := bs_cache shr (32 - n); end;
  procedure FLUSH(n: Integer); inline; begin bs_cache := bs_cache shl n; Inc(bs_sh, n); end;
  procedure CHECK; inline;
  begin
    while bs_sh >= 0 do
    begin
      bs_cache := bs_cache or (LongWord(bs.Buf[bs_next]) shl bs_sh);
      Inc(bs_next);
      Dec(bs_sh, 8);
    end;
  end;
  function BSPOS: Integer; inline; begin Result := bs_next*8 - 24 + bs_sh; end;

begin
  one := 0.0;
  ireg := 0; big_val_cnt := gr_info^.BigValues;
  sfb := gr_info^.Sfbtab;
  bs_next := bs.Pos div 8;
  bs_cache := (((LongWord(bs.Buf[bs_next])*256 + bs.Buf[bs_next+1])*256 + bs.Buf[bs_next+2])*256 + bs.Buf[bs_next+3]) shl (bs.Pos and 7);
  bs_sh := (bs.Pos and 7) - 8;
  Inc(bs_next, 4);

  while big_val_cnt > 0 do
  begin
    tab_num := gr_info^.TableSelect[ireg];
    sfb_cnt := gr_info^.RegionCount[ireg]; Inc(ireg);
    cbOfs := MP3_TABINDEX[tab_num];
    linbits := MP3_LINBITS[tab_num];
    if linbits <> 0 then
    begin
      repeat
        np := sfb^ div 2; Inc(sfb);
        pairs_to_decode := Min(big_val_cnt, np);
        one := scf^; Inc(scf);
        repeat
          w := 5;
          leaf := MP3_TABS[cbOfs + PEEK(w)];
          while leaf < 0 do
          begin
            FLUSH(w);
            w := leaf and 7;
            leaf := MP3_TABS[cbOfs + Integer(PEEK(w)) - ASR(leaf, 3)];
          end;
          FLUSH(ASR(leaf, 8));
          for j := 0 to 1 do
          begin
            lsb := leaf and $0F;
            if lsb = 15 then
            begin
              Inc(lsb, Integer(PEEK(linbits)));
              FLUSH(linbits);
              CHECK;
              if (bs_cache and $80000000) <> 0 then
                dst^ := one*L3_pow_43(lsb)*(-1)
              else
                dst^ := one*L3_pow_43(lsb)*(1);
            end
            else
              dst^ := MP3_POW43[16 + lsb - 16*(bs_cache shr 31)]*one;
            if lsb <> 0 then FLUSH(1);
            Inc(dst);
            leaf := leaf shr 4;
          end;
          CHECK;
          Dec(pairs_to_decode);
        until pairs_to_decode = 0;
        Dec(big_val_cnt, np);
        Dec(sfb_cnt);
      until not ((big_val_cnt > 0) and (sfb_cnt >= 0));
    end
    else
    begin
      repeat
        np := sfb^ div 2; Inc(sfb);
        pairs_to_decode := Min(big_val_cnt, np);
        one := scf^; Inc(scf);
        repeat
          w := 5;
          leaf := MP3_TABS[cbOfs + PEEK(w)];
          while leaf < 0 do
          begin
            FLUSH(w);
            w := leaf and 7;
            leaf := MP3_TABS[cbOfs + Integer(PEEK(w)) - ASR(leaf, 3)];
          end;
          FLUSH(ASR(leaf, 8));
          for j := 0 to 1 do
          begin
            lsb := leaf and $0F;
            dst^ := MP3_POW43[16 + lsb - 16*(bs_cache shr 31)]*one;
            if lsb <> 0 then FLUSH(1);
            Inc(dst);
            leaf := leaf shr 4;
          end;
          CHECK;
          Dec(pairs_to_decode);
        until pairs_to_decode = 0;
        Dec(big_val_cnt, np);
        Dec(sfb_cnt);
      until not ((big_val_cnt > 0) and (sfb_cnt >= 0));
    end;
  end;

  np := 1 - big_val_cnt;
  while True do
  begin
    if gr_info^.Count1Table <> 0 then countTab := @MP3_TAB33[0] else countTab := @MP3_TAB32[0];
    leaf := countTab[PEEK(4)];
    if (leaf and 8) = 0 then
    begin
      sh := 32 - (leaf and 3);
      if sh >= 32 then leaf := countTab[(leaf shr 3)]
      else leaf := countTab[(leaf shr 3) + ((bs_cache shl 4) shr sh)];
    end;
    FLUSH(leaf and 7);
    if BSPOS > layer3gr_limit then Break;

    Dec(np);
    if np = 0 then
    begin np := sfb^ div 2; Inc(sfb); if np = 0 then Break; one := scf^; Inc(scf); end;
    if (leaf and (128 shr 0)) <> 0 then
    begin if (bs_cache and $80000000) <> 0 then dst[0] := -one else dst[0] := one; FLUSH(1); end;
    if (leaf and (128 shr 1)) <> 0 then
    begin if (bs_cache and $80000000) <> 0 then dst[1] := -one else dst[1] := one; FLUSH(1); end;
    Dec(np);
    if np = 0 then
    begin np := sfb^ div 2; Inc(sfb); if np = 0 then Break; one := scf^; Inc(scf); end;
    if (leaf and (128 shr 2)) <> 0 then
    begin if (bs_cache and $80000000) <> 0 then dst[2] := -one else dst[2] := one; FLUSH(1); end;
    if (leaf and (128 shr 3)) <> 0 then
    begin if (bs_cache and $80000000) <> 0 then dst[3] := -one else dst[3] := one; FLUSH(1); end;
    CHECK;
    Inc(dst, 4);
  end;

  bs.Pos := layer3gr_limit;
end;

// --- stereo ---
procedure L3_midside_stereo(left: PSingle; n: Integer);
var i: Integer; right: PSingle; a, b: Single;
begin
  right := left + 576;
  for i := 0 to n - 1 do
  begin
    a := left[i]; b := right[i];
    left[i] := a + b;
    right[i] := a - b;
  end;
end;

procedure L3_intensity_stereo_band(left: PSingle; n: Integer; kl, kr: Single);
var i: Integer;
begin
  for i := 0 to n - 1 do
  begin
    left[i + 576] := left[i]*kr;
    left[i] := left[i]*kl;
  end;
end;

procedure L3_stereo_top_band(right: PSingle; sfb: PByte; nbands: Integer; var max_band: array of Integer);
var i, k: Integer;
begin
  max_band[0] := -1; max_band[1] := -1; max_band[2] := -1;
  for i := 0 to nbands - 1 do
  begin
    k := 0;
    while k < sfb[i] do
    begin
      if (right[k] <> 0) or (right[k + 1] <> 0) then
      begin max_band[i mod 3] := i; Break; end;
      Inc(k, 2);
    end;
    Inc(right, sfb[i]);
  end;
end;

procedure L3_stereo_process(left: PSingle; ist_pos: PByte; sfb: PByte; hdr: PByte; var max_band: array of Integer; mpeg2_sh: Integer);
const
  g_pan: array[0..13] of Single = (0,1,0.21132487,0.78867513,0.36602540,0.63397460,0.5,0.5,0.63397460,0.36602540,0.78867513,0.21132487,1,0);
var
  i, max_pos, ipos: Integer;
  kl, kr, s: Single;
begin
  if HDR_TEST_MPEG1(hdr) then max_pos := 7 else max_pos := 64;
  i := 0;
  while sfb[i] <> 0 do
  begin
    ipos := ist_pos[i];
    if (i > max_band[i mod 3]) and (ipos < max_pos) then
    begin
      if HDR_TEST_MS_STEREO(hdr) then s := 1.41421356 else s := 1;
      if HDR_TEST_MPEG1(hdr) then
      begin
        kl := g_pan[2*ipos];
        kr := g_pan[2*ipos + 1];
      end
      else
      begin
        kl := 1;
        kr := L3_ldexp_q2(1, ((ipos + 1) shr 1) shl mpeg2_sh);
        if (ipos and 1) <> 0 then begin kl := kr; kr := 1; end;
      end;
      L3_intensity_stereo_band(left, sfb[i], kl*s, kr*s);
    end
    else if HDR_TEST_MS_STEREO(hdr) then
      L3_midside_stereo(left, sfb[i]);
    Inc(left, sfb[i]);
    Inc(i);
  end;
end;

procedure L3_intensity_stereo(left: PSingle; ist_pos: PByte; gr: PL3GrInfo; hdr: PByte);
var
  max_band: array[0..2] of Integer;
  n_sfb, i, max_blocks, default_pos, itop, prev: Integer;
  gr1: PL3GrInfo;
begin
  n_sfb := gr^.NLongSfb + gr^.NShortSfb;
  if gr^.NShortSfb <> 0 then max_blocks := 3 else max_blocks := 1;

  L3_stereo_top_band(left + 576, gr^.Sfbtab, n_sfb, max_band);
  if gr^.NLongSfb <> 0 then
  begin
    i := Max(Max(max_band[0], max_band[1]), max_band[2]);
    max_band[0] := i; max_band[1] := i; max_band[2] := i;
  end;
  for i := 0 to max_blocks - 1 do
  begin
    if HDR_TEST_MPEG1(hdr) then default_pos := 3 else default_pos := 0;
    itop := n_sfb - max_blocks + i;
    prev := itop - max_blocks;
    if max_band[i] >= prev then ist_pos[itop] := default_pos
    else ist_pos[itop] := ist_pos[prev];
  end;
  gr1 := gr; Inc(gr1);
  L3_stereo_process(left, ist_pos, gr^.Sfbtab, hdr, max_band, gr1^.ScalefacCompress and 1);
end;

// --- reorder / antialias ---
procedure L3_reorder(grbuf: PSingle; scratch: PSingle; sfb: PByte);
var i, len: Integer; src, dst: PSingle;
begin
  src := grbuf; dst := scratch;
  while True do
  begin
    len := sfb^;
    if len = 0 then Break;
    for i := 0 to len - 1 do
    begin
      dst^ := src[0*len]; Inc(dst);
      dst^ := src[1*len]; Inc(dst);
      dst^ := src[2*len]; Inc(dst);
      Inc(src);
    end;
    Inc(sfb, 3);
    Inc(src, 2*len);
  end;
  Move(scratch^, grbuf^, (dst - scratch)*SizeOf(Single));
end;

procedure L3_antialias(grbuf: PSingle; nbands: Integer);
var i: Integer; u, d: Single;
begin
  while nbands > 0 do
  begin
    for i := 0 to 7 do
    begin
      u := grbuf[18 + i];
      d := grbuf[17 - i];
      grbuf[18 + i] := u*MP3_AA[i] - d*MP3_AA[8 + i];
      grbuf[17 - i] := u*MP3_AA[8 + i] + d*MP3_AA[i];
    end;
    Dec(nbands);
    Inc(grbuf, 18);
  end;
end;

// --- IMDCT ---
procedure L3_dct3_9(y: PSingle);
var s0,s1,s2,s3,s4,s5,s6,s7,s8,t0,t2,t4: Single;
begin
  s0 := y[0]; s2 := y[2]; s4 := y[4]; s6 := y[6]; s8 := y[8];
  t0 := s0 + s6*0.5;
  s0 := s0 - s6;
  t4 := (s4 + s2)*0.93969262;
  t2 := (s8 + s2)*0.76604444;
  s6 := (s4 - s8)*0.17364818;
  s4 := s4 + (s8 - s2);

  s2 := s0 - s4*0.5;
  y[4] := s4 + s0;
  s8 := t0 - t2 + s6;
  s0 := t0 - t4 + t2;
  s4 := t0 + t4 - s6;

  s1 := y[1]; s3 := y[3]; s5 := y[5]; s7 := y[7];

  s3 := s3*0.86602540;
  t0 := (s5 + s1)*0.98480775;
  t4 := (s5 - s7)*0.34202014;
  t2 := (s1 + s7)*0.64278761;
  s1 := (s1 - s5 - s7)*0.86602540;

  s5 := t0 - s3 - t2;
  s7 := t4 - s3 - t0;
  s3 := t4 + s3 - t2;

  y[0] := s4 - s7;
  y[1] := s2 + s1;
  y[2] := s0 - s3;
  y[3] := s8 + s5;
  y[5] := s8 - s5;
  y[6] := s0 + s3;
  y[7] := s2 - s1;
  y[8] := s4 + s7;
end;

procedure L3_imdct36(grbuf, overlap: PSingle; window: PSingle; nbands: Integer);
var
  i, j: Integer;
  co, si: array[0..8] of Single;
  ovl, sum: Single;
begin
  for j := 0 to nbands - 1 do
  begin
    co[0] := -grbuf[0];
    si[0] := grbuf[17];
    for i := 0 to 3 do
    begin
      si[8 - 2*i] :=   grbuf[4*i + 1] - grbuf[4*i + 2];
      co[1 + 2*i] :=   grbuf[4*i + 1] + grbuf[4*i + 2];
      si[7 - 2*i] :=   grbuf[4*i + 4] - grbuf[4*i + 3];
      co[2 + 2*i] := -(grbuf[4*i + 3] + grbuf[4*i + 4]);
    end;
    L3_dct3_9(@co[0]);
    L3_dct3_9(@si[0]);

    si[1] := -si[1]; si[3] := -si[3]; si[5] := -si[5]; si[7] := -si[7];

    for i := 0 to 8 do
    begin
      ovl := overlap[i];
      sum := co[i]*MP3_TWID9[9 + i] + si[i]*MP3_TWID9[0 + i];
      overlap[i] := co[i]*MP3_TWID9[0 + i] - si[i]*MP3_TWID9[9 + i];
      grbuf[i]      := ovl*window[0 + i] - sum*window[9 + i];
      grbuf[17 - i] := ovl*window[9 + i] + sum*window[0 + i];
    end;
    Inc(grbuf, 18);
    Inc(overlap, 9);
  end;
end;

procedure L3_idct3(x0, x1, x2: Single; dst: PSingle);
var m1, a1: Single;
begin
  m1 := x1*0.86602540;
  a1 := x0 - x2*0.5;
  dst[1] := x0 + x2;
  dst[0] := a1 + m1;
  dst[2] := a1 - m1;
end;

procedure L3_imdct12(x, dst, overlap: PSingle);
const g_twid3: array[0..5] of Single = (0.79335334,0.92387953,0.99144486, 0.60876143,0.38268343,0.13052619);
var co, si: array[0..2] of Single; i: Integer; ovl, sum: Single;
begin
  L3_idct3(-x[0], x[6] + x[3], x[12] + x[9], @co[0]);
  L3_idct3(x[15], x[12] - x[9], x[6] - x[3], @si[0]);
  si[1] := -si[1];
  for i := 0 to 2 do
  begin
    ovl := overlap[i];
    sum := co[i]*g_twid3[3 + i] + si[i]*g_twid3[0 + i];
    overlap[i] := co[i]*g_twid3[0 + i] - si[i]*g_twid3[3 + i];
    dst[i]     := ovl*g_twid3[2 - i] - sum*g_twid3[5 - i];
    dst[5 - i] := ovl*g_twid3[5 - i] + sum*g_twid3[2 - i];
  end;
end;

procedure L3_imdct_short(grbuf, overlap: PSingle; nbands: Integer);
var tmp: array[0..17] of Single;
begin
  while nbands > 0 do
  begin
    Move(grbuf^, tmp[0], SizeOf(tmp));
    Move(overlap^, grbuf^, 6*SizeOf(Single));
    L3_imdct12(@tmp[0], grbuf + 6, overlap + 6);
    L3_imdct12(@tmp[1], grbuf + 12, overlap + 6);
    L3_imdct12(@tmp[2], overlap, overlap + 6);
    Dec(nbands);
    Inc(overlap, 9);
    Inc(grbuf, 18);
  end;
end;

procedure L3_change_sign(grbuf: PSingle);
var b, i: Integer; p: PSingle;
begin
  p := grbuf + 18;
  b := 0;
  while b < 32 do
  begin
    i := 1;
    while i < 18 do begin p[i] := -p[i]; Inc(i, 2); end;
    Inc(b, 2);
    Inc(p, 36);
  end;
end;

procedure L3_imdct_gr(grbuf, overlap: PSingle; block_type, n_long_bands: LongWord);
var winOfs: Integer;
begin
  if n_long_bands <> 0 then
  begin
    L3_imdct36(grbuf, overlap, @MP3_MDCTWIN[0], n_long_bands);
    Inc(grbuf, 18*n_long_bands);
    Inc(overlap, 9*n_long_bands);
  end;
  if block_type = SHORT_BLOCK_TYPE then
    L3_imdct_short(grbuf, overlap, 32 - n_long_bands)
  else
  begin
    if block_type = STOP_BLOCK_TYPE then winOfs := 18 else winOfs := 0;
    L3_imdct36(grbuf, overlap, @MP3_MDCTWIN[winOfs], 32 - n_long_bands);
  end;
end;

// --- reservoir / decode ---
procedure L3_save_reservoir(var h: TMP3Core; var s: TMP3Scratch);
var pos, remains: Integer;
begin
  pos := (s.Bs.Pos + 7) div 8;
  remains := s.Bs.Limit div 8 - pos;
  if remains > MAX_BITRESERVOIR_BYTES then
  begin
    Inc(pos, remains - MAX_BITRESERVOIR_BYTES);
    remains := MAX_BITRESERVOIR_BYTES;
  end;
  if remains > 0 then
    Move(s.Maindata[pos], h.ReservBuf[0], remains);
  h.Reserv := remains;
end;

function L3_restore_reservoir(var h: TMP3Core; var bs: TMP3BitReader; var s: TMP3Scratch; main_data_begin: Integer): Boolean;
var frame_bytes, bytes_have: Integer;
begin
  frame_bytes := (bs.Limit - bs.Pos) div 8;
  bytes_have := Min(h.Reserv, main_data_begin);
  if Min(h.Reserv, main_data_begin) > 0 then
    Move(h.ReservBuf[Max(0, h.Reserv - main_data_begin)], s.Maindata[0], Min(h.Reserv, main_data_begin));
  Move((bs.Buf + bs.Pos div 8)^, s.Maindata[bytes_have], frame_bytes);
  BsInit(s.Bs, @s.Maindata[0], bytes_have + frame_bytes);
  Result := h.Reserv >= main_data_begin;
end;

procedure L3_decode(var h: TMP3Core; var s: TMP3Scratch; gr_info: PL3GrInfo; nch: Integer);
var
  ch, layer3gr_limit, aa_bands, n_long_bands: Integer;
  gp: PL3GrInfo;
begin
  for ch := 0 to nch - 1 do
  begin
    layer3gr_limit := s.Bs.Pos + gr_info[ch].Part23Length;
    L3_decode_scalefactors(@h.Header[0], @s.IstPos[ch][0], s.Bs, @gr_info[ch], @s.Scf[0], ch);
    L3_huffman(@s.Grbuf[576*ch], s.Bs, @gr_info[ch], @s.Scf[0], layer3gr_limit);
  end;

  if HDR_TEST_I_STEREO(@h.Header[0]) then
    L3_intensity_stereo(@s.Grbuf[0], @s.IstPos[1][0], gr_info, @h.Header[0])
  else if HDR_IS_MS_STEREO(@h.Header[0]) then
    L3_midside_stereo(@s.Grbuf[0], 576);

  gp := gr_info;
  for ch := 0 to nch - 1 do
  begin
    aa_bands := 31;
    n_long_bands := (Ord(gp^.MixedBlockFlag <> 0)*2) shl Ord(HDR_GET_MY_SAMPLE_RATE(@h.Header[0]) = 2);
    if gp^.NShortSfb <> 0 then
    begin
      aa_bands := n_long_bands - 1;
      L3_reorder(@s.Grbuf[576*ch + n_long_bands*18], @s.Syn[0], gp^.Sfbtab + gp^.NLongSfb);
    end;
    L3_antialias(@s.Grbuf[576*ch], aa_bands);
    L3_imdct_gr(@s.Grbuf[576*ch], @h.MdctOverlap[ch][0], gp^.BlockType, n_long_bands);
    L3_change_sign(@s.Grbuf[576*ch]);
    Inc(gp);
  end;
end;

// --- synthesis filterbank ---
procedure mp3d_DCT_II(grbuf: PSingle; n: Integer);
const
  g_sec: array[0..23] of Single = (
    10.19000816,0.50060302,0.50241929,3.40760851,0.50547093,0.52249861,2.05778098,0.51544732,0.56694406,1.48416460,0.53104258,0.64682180,1.16943991,0.55310392,0.78815460,0.97256821,0.58293498,1.06067765,0.83934963,0.62250412,1.72244716,0.74453628,0.67480832,5.10114861);
var
  i, k: Integer;
  t: array[0..3, 0..7] of Single;
  y: PSingle;
  x0,x1,x2,x3,x4,x5,x6,x7,xt,t0,t1,t2,t3: Single;
begin
  for k := 0 to n - 1 do
  begin
    y := grbuf + k;
    for i := 0 to 7 do
    begin
      x0 := y[i*18];
      x1 := y[(15 - i)*18];
      x2 := y[(16 + i)*18];
      x3 := y[(31 - i)*18];
      t0 := x0 + x3;
      t1 := x1 + x2;
      t2 := (x1 - x2)*g_sec[3*i + 0];
      t3 := (x0 - x3)*g_sec[3*i + 1];
      t[0][i] := t0 + t1;
      t[1][i] := (t0 - t1)*g_sec[3*i + 2];
      t[2][i] := t3 + t2;
      t[3][i] := (t3 - t2)*g_sec[3*i + 2];
    end;
    for i := 0 to 3 do
    begin
      x0 := t[i][0]; x1 := t[i][1]; x2 := t[i][2]; x3 := t[i][3];
      x4 := t[i][4]; x5 := t[i][5]; x6 := t[i][6]; x7 := t[i][7];
      xt := x0 - x7; x0 := x0 + x7;
      x7 := x1 - x6; x1 := x1 + x6;
      x6 := x2 - x5; x2 := x2 + x5;
      x5 := x3 - x4; x3 := x3 + x4;
      x4 := x0 - x3; x0 := x0 + x3;
      x3 := x1 - x2; x1 := x1 + x2;
      t[i][0] := x0 + x1;
      t[i][4] := (x0 - x1)*0.70710677;
      x5 := x5 + x6;
      x6 := (x6 + x7)*0.70710677;
      x7 := x7 + xt;
      x3 := (x3 + x4)*0.70710677;
      x5 := x5 - x7*0.198912367;
      x7 := x7 + x5*0.382683432;
      x5 := x5 - x7*0.198912367;
      x0 := xt - x6; xt := xt + x6;
      t[i][1] := (xt + x7)*0.50979561;
      t[i][2] := (x4 + x3)*0.54119611;
      t[i][3] := (x0 - x5)*0.60134488;
      t[i][5] := (x0 + x5)*0.89997619;
      t[i][6] := (x4 - x3)*1.30656302;
      t[i][7] := (xt - x7)*2.56291556;
    end;
    for i := 0 to 6 do
    begin
      y[0*18] := t[0][i];
      y[1*18] := t[2][i] + t[3][i] + t[3][i + 1];
      y[2*18] := t[1][i] + t[1][i + 1];
      y[3*18] := t[2][i + 1] + t[3][i] + t[3][i + 1];
      Inc(y, 4*18);
    end;
    y[0*18] := t[0][7];
    y[1*18] := t[2][7] + t[3][7];
    y[2*18] := t[1][7];
    y[3*18] := t[3][7];
  end;
end;

function mp3d_scale_pcm(sample: Single): Single; inline;
begin
  Result := sample*(1.0/32768.0);
end;

procedure mp3d_synth_pair(pcm: PSingle; nch: Integer; z: PSingle);
var a: Single;
begin
  a  := (z[14*64] - z[    0]) * 29;
  a := a + (z[ 1*64] + z[13*64]) * 213;
  a := a + (z[12*64] - z[ 2*64]) * 459;
  a := a + (z[ 3*64] + z[11*64]) * 2037;
  a := a + (z[10*64] - z[ 4*64]) * 5153;
  a := a + (z[ 5*64] + z[ 9*64]) * 6574;
  a := a + (z[ 8*64] - z[ 6*64]) * 37489;
  a := a + z[ 7*64]              * 75038;
  pcm[0] := mp3d_scale_pcm(a);

  Inc(z, 2);
  a  := z[14*64] * 104;
  a := a + z[12*64] * 1567;
  a := a + z[10*64] * 9727;
  a := a + z[ 8*64] * 64019;
  a := a + z[ 6*64] * (-9975);
  a := a + z[ 4*64] * (-45);
  a := a + z[ 2*64] * 146;
  a := a + z[ 0*64] * (-5);
  pcm[16*nch] := mp3d_scale_pcm(a);
end;

procedure mp3d_synth(xl: PSingle; dstl: PSingle; nch: Integer; lins: PSingle);
var
  i, j, wOfs: Integer;
  xr, dstr, zlin, vz, vy: PSingle;
  w0, w1: Single;
  a, b: array[0..3] of Single;
begin
  xr := xl + 576*(nch - 1);
  dstr := dstl + (nch - 1);
  zlin := lins + 15*64;
  wOfs := 0;

  zlin[4*15]     := xl[18*16];
  zlin[4*15 + 1] := xr[18*16];
  zlin[4*15 + 2] := xl[0];
  zlin[4*15 + 3] := xr[0];

  zlin[4*31]     := xl[1 + 18*16];
  zlin[4*31 + 1] := xr[1 + 18*16];
  zlin[4*31 + 2] := xl[1];
  zlin[4*31 + 3] := xr[1];

  mp3d_synth_pair(dstr, nch, lins + 4*15 + 1);
  mp3d_synth_pair(dstr + 32*nch, nch, lins + 4*15 + 64 + 1);
  mp3d_synth_pair(dstl, nch, lins + 4*15);
  mp3d_synth_pair(dstl + 32*nch, nch, lins + 4*15 + 64);

  for i := 14 downto 0 do
  begin
    zlin[4*i]     := xl[18*(31 - i)];
    zlin[4*i + 1] := xr[18*(31 - i)];
    zlin[4*i + 2] := xl[1 + 18*(31 - i)];
    zlin[4*i + 3] := xr[1 + 18*(31 - i)];
    zlin[4*(i + 16)]     := xl[1 + 18*(1 + i)];
    zlin[4*(i + 16) + 1] := xr[1 + 18*(1 + i)];
    zlin[4*(i - 16) + 2] := xl[18*(1 + i)];
    zlin[4*(i - 16) + 3] := xr[18*(1 + i)];

    // S0(0) S2(1) S1(2) S2(3) S1(4) S2(5) S1(6) S2(7)
    for j := 0 to 7 do
    begin
      w0 := MP3_WIN[wOfs]; Inc(wOfs);
      w1 := MP3_WIN[wOfs]; Inc(wOfs);
      vz := @zlin[4*i - j*64];
      vy := @zlin[4*i - (15 - j)*64];
      // minimp3 sequence: S0(0) S2(1) S1(2) S2(3) S1(4) S2(5) S1(6) S2(7)
      if j = 0 then
      begin
        // S0: b = vz*w1 + vy*w0; a = vz*w0 - vy*w1
        b[0] := vz[0]*w1 + vy[0]*w0; a[0] := vz[0]*w0 - vy[0]*w1;
        b[1] := vz[1]*w1 + vy[1]*w0; a[1] := vz[1]*w0 - vy[1]*w1;
        b[2] := vz[2]*w1 + vy[2]*w0; a[2] := vz[2]*w0 - vy[2]*w1;
        b[3] := vz[3]*w1 + vy[3]*w0; a[3] := vz[3]*w0 - vy[3]*w1;
      end
      else if (j and 1) = 0 then   // S1 at j = 2,4,6
      begin
        // S1: b += vz*w1 + vy*w0; a += vz*w0 - vy*w1
        b[0] := b[0] + (vz[0]*w1 + vy[0]*w0); a[0] := a[0] + (vz[0]*w0 - vy[0]*w1);
        b[1] := b[1] + (vz[1]*w1 + vy[1]*w0); a[1] := a[1] + (vz[1]*w0 - vy[1]*w1);
        b[2] := b[2] + (vz[2]*w1 + vy[2]*w0); a[2] := a[2] + (vz[2]*w0 - vy[2]*w1);
        b[3] := b[3] + (vz[3]*w1 + vy[3]*w0); a[3] := a[3] + (vz[3]*w0 - vy[3]*w1);
      end
      else                          // S2 at j = 1,3,5,7
      begin
        // S2: b += vz*w1 + vy*w0; a += vy*w1 - vz*w0
        b[0] := b[0] + (vz[0]*w1 + vy[0]*w0); a[0] := a[0] + (vy[0]*w1 - vz[0]*w0);
        b[1] := b[1] + (vz[1]*w1 + vy[1]*w0); a[1] := a[1] + (vy[1]*w1 - vz[1]*w0);
        b[2] := b[2] + (vz[2]*w1 + vy[2]*w0); a[2] := a[2] + (vy[2]*w1 - vz[2]*w0);
        b[3] := b[3] + (vz[3]*w1 + vy[3]*w0); a[3] := a[3] + (vy[3]*w1 - vz[3]*w0);
      end;
    end;

    dstr[(15 - i)*nch] := mp3d_scale_pcm(a[1]);
    dstr[(17 + i)*nch] := mp3d_scale_pcm(b[1]);
    dstl[(15 - i)*nch] := mp3d_scale_pcm(a[0]);
    dstl[(17 + i)*nch] := mp3d_scale_pcm(b[0]);
    dstr[(47 - i)*nch] := mp3d_scale_pcm(a[3]);
    dstr[(49 + i)*nch] := mp3d_scale_pcm(b[3]);
    dstl[(47 - i)*nch] := mp3d_scale_pcm(a[2]);
    dstl[(49 + i)*nch] := mp3d_scale_pcm(b[2]);
  end;
end;

procedure mp3d_synth_granule(qmf_state: PSingle; grbuf: PSingle; nbands, nch: Integer; pcm: PSingle; lins: PSingle);
var i: Integer;
begin
  for i := 0 to nch - 1 do
    mp3d_DCT_II(grbuf + 576*i, nbands);

  Move(qmf_state^, lins^, SizeOf(Single)*15*64);

  i := 0;
  while i < nbands do
  begin
    mp3d_synth(grbuf + i, pcm + 32*nch*i, nch, lins + i*64);
    Inc(i, 2);
  end;

  if nch = 1 then
  begin
    i := 0;
    while i < 15*64 do begin qmf_state[i] := lins[nbands*64 + i]; Inc(i, 2); end;
  end
  else
    Move(lins[nbands*64], qmf_state^, SizeOf(Single)*15*64);
end;

// --- frame finding ---
function mp3d_match_frame(hdr: PByte; mp3_bytes, frame_bytes: Integer): Boolean;
var i, nmatch: Integer;
begin
  i := 0;
  for nmatch := 0 to MAX_FRAME_SYNC_MATCHES - 1 do
  begin
    Inc(i, hdr_frame_bytes(hdr + i, frame_bytes) + hdr_padding(hdr + i));
    if i + HDR_SIZE > mp3_bytes then Exit(nmatch > 0);
    if not hdr_compare(hdr, hdr + i) then Exit(False);
  end;
  Result := True;
end;

function mp3d_find_frame(mp3: PByte; mp3_bytes: Integer; var free_format_bytes: Integer; var ptr_frame_bytes: Integer): Integer;
var
  i, k, frame_bytes, frame_and_padding, fb, nextfb: Integer;
begin
  i := 0;
  while i < mp3_bytes - HDR_SIZE do
  begin
    if hdr_valid(mp3) then
    begin
      frame_bytes := hdr_frame_bytes(mp3, free_format_bytes);
      frame_and_padding := frame_bytes + hdr_padding(mp3);

      k := HDR_SIZE;
      while (frame_bytes = 0) and (k < MAX_FREE_FORMAT_FRAME_SIZE) and (i + 2*k < mp3_bytes - HDR_SIZE) do
      begin
        if hdr_compare(mp3, mp3 + k) then
        begin
          fb := k - hdr_padding(mp3);
          nextfb := fb + hdr_padding(mp3 + k);
          if (i + k + nextfb + HDR_SIZE > mp3_bytes) or (not hdr_compare(mp3, mp3 + k + nextfb)) then
          begin Inc(k); Continue; end;
          frame_and_padding := k;
          frame_bytes := fb;
          free_format_bytes := fb;
        end;
        Inc(k);
      end;

      if ((frame_bytes <> 0) and (i + frame_and_padding <= mp3_bytes) and
          mp3d_match_frame(mp3, mp3_bytes - i, frame_bytes)) or
         ((i = 0) and (frame_and_padding = mp3_bytes)) then
      begin
        ptr_frame_bytes := frame_and_padding;
        Exit(i);
      end;
      free_format_bytes := 0;
    end;
    Inc(i); Inc(mp3);
  end;
  ptr_frame_bytes := 0;
  Result := mp3_bytes;
end;

procedure mp3dec_init_core(var dec: TMP3Core);
begin
  FillChar(dec, SizeOf(dec), 0);
end;

// Decode one frame. Returns samples per channel produced (0 if none). Fills info.
function mp3dec_decode_frame(var dec: TMP3Core; var scratch: TMP3Scratch;
  mp3: PByte; mp3_bytes: Integer; pcm: PSingle; var info: TMP3FrameInfo): Integer;
var
  i, igr, frame_size, ngr: Integer;
  success: Boolean;
  hdr: PByte;
  bs_frame: TMP3BitReader;
  main_data_begin: Integer;
  pcmp: PSingle;
begin
  i := 0; frame_size := 0; success := True;

  if (mp3_bytes > 4) and (dec.Header[0] = $ff) and hdr_compare(@dec.Header[0], mp3) then
  begin
    frame_size := hdr_frame_bytes(mp3, dec.FreeFormatBytes) + hdr_padding(mp3);
    if (frame_size <> mp3_bytes) and ((frame_size + HDR_SIZE > mp3_bytes) or (not hdr_compare(mp3, mp3 + frame_size))) then
      frame_size := 0;
  end;
  if frame_size = 0 then
  begin
    mp3dec_init_core(dec);
    i := mp3d_find_frame(mp3, mp3_bytes, dec.FreeFormatBytes, frame_size);
    if (frame_size = 0) or (i + frame_size > mp3_bytes) then
    begin
      info.FrameBytes := i;
      Exit(0);
    end;
  end;

  hdr := mp3 + i;
  Move(hdr^, dec.Header[0], HDR_SIZE);
  info.FrameBytes := i + frame_size;
  info.FrameOffset := i;
  if HDR_IS_MONO(hdr) then info.Channels := 1 else info.Channels := 2;
  info.Hz := hdr_sample_rate_hz(hdr);
  info.Layer := 4 - HDR_GET_LAYER(hdr);
  info.BitrateKbps := hdr_bitrate_kbps(hdr);

  if pcm = nil then Exit(hdr_frame_samples(hdr));

  BsInit(bs_frame, hdr + HDR_SIZE, frame_size - HDR_SIZE);
  if HDR_IS_CRC(hdr) then GetBits(bs_frame, 16);

  if info.Layer = 3 then
  begin
    main_data_begin := L3_read_side_info(bs_frame, @scratch.GrInfo[0], hdr);
    if (main_data_begin < 0) or (bs_frame.Pos > bs_frame.Limit) then
    begin
      mp3dec_init_core(dec);
      Exit(0);
    end;
    success := L3_restore_reservoir(dec, bs_frame, scratch, main_data_begin);
    if success then
    begin
      if HDR_TEST_MPEG1(hdr) then ngr := 2 else ngr := 1;
      pcmp := pcm;
      for igr := 0 to ngr - 1 do
      begin
        FillChar(scratch.Grbuf[0], 576*2*SizeOf(Single), 0);
        L3_decode(dec, scratch, @scratch.GrInfo[igr*info.Channels], info.Channels);
        mp3d_synth_granule(@dec.QmfState[0], @scratch.Grbuf[0], 18, info.Channels, pcmp, @scratch.Syn[0]);
        Inc(pcmp, 576*info.Channels);
      end;
    end;
    L3_save_reservoir(dec, scratch);
  end
  else
  begin
    // Layer I/II not supported
    Exit(0);
  end;

  if success then Result := hdr_frame_samples(@dec.Header[0]) else Result := 0;
end;

// Parse a Xing/Info VBR tag (+ LAME extension) in the first frame for gapless
// playback. Returns 1 if a tag with a frame count was found (frames/delay/
// padding filled), -1 if a tag without the frame count, 0 if no tag. delay and
// padding are per-channel sample-frame counts (delay already includes the
// 528+1 decoder delay; padding already has it subtracted).
function CheckVbrtag(frame: PByte; frame_size: Integer; out frames: LongWord;
  out delay, padding: Integer): Integer;
const
  FRAMES_FLAG = 1; BYTES_FLAG = 2; TOC_FLAG = 4; VBR_SCALE_FLAG = 8;
var
  bs: TMP3BitReader;
  gr: array[0..3] of TL3GrInfo;
  tagOff, flags: Integer;
  tag: PByte;
begin
  frames := 0; delay := 0; padding := 0;
  Result := 0;
  if frame_size < HDR_SIZE + 8 then Exit;
  BsInit(bs, frame + HDR_SIZE, frame_size - HDR_SIZE);
  if HDR_IS_CRC(frame) then GetBits(bs, 16);
  if L3_read_side_info(bs, @gr[0], frame) < 0 then Exit;

  tagOff := HDR_SIZE + bs.Pos div 8;
  if (tagOff > frame_size) or (frame_size - tagOff < 8) then Exit;
  tag := frame + tagOff;
  if not ( ((tag[0]=Ord('X')) and (tag[1]=Ord('i')) and (tag[2]=Ord('n')) and (tag[3]=Ord('g'))) or
           ((tag[0]=Ord('I')) and (tag[1]=Ord('n')) and (tag[2]=Ord('f')) and (tag[3]=Ord('o'))) ) then Exit;
  flags := tag[7];
  if (flags and FRAMES_FLAG) = 0 then Exit(-1);
  Inc(tagOff, 8);
  if frame_size - tagOff < 4 then Exit;
  tag := frame + tagOff;
  frames := (LongWord(tag[0]) shl 24) or (LongWord(tag[1]) shl 16) or (LongWord(tag[2]) shl 8) or tag[3];
  Inc(tagOff, 4);
  if (flags and BYTES_FLAG) <> 0 then
  begin if frame_size - tagOff < 4 then Exit; Inc(tagOff, 4); end;
  if (flags and TOC_FLAG) <> 0 then
  begin if frame_size - tagOff < 100 then Exit; Inc(tagOff, 100); end;
  if (flags and VBR_SCALE_FLAG) <> 0 then
  begin if frame_size - tagOff < 4 then Exit; Inc(tagOff, 4); end;
  if frame_size - tagOff < 1 then Exit;
  tag := frame + tagOff;
  if tag[0] <> 0 then    // LAME/Lavc/etc. extension
  begin
    if frame_size - tagOff <= 35 then Exit;
    Inc(tagOff, 21);
    tag := frame + tagOff;
    delay   := ((tag[0] shl 4) or (tag[1] shr 4)) + (528 + 1);
    padding := (((tag[1] and $F) shl 8) or tag[2]) - (528 + 1);
  end;
  Result := 1;
end;

{ TSedaiMP3Decoder }

function TSedaiMP3Decoder.DecodeAll: Boolean;
var
  data: array of Byte;
  sz, pos, samples, total, cap, k, startFrame, skip: Integer;
  core: TMP3Core;
  scratch: TMP3Scratch;
  info: TMP3FrameInfo;
  framePCM: array of Single;
  ffOfs, ffSize, freefmt, vbr, toSkip: Integer;
  vbrFrames: LongWord;
  vbrDelay, vbrPadding: Integer;
  detectedFrames: Int64;
begin
  Result := False;
  sz := FStream.Size - FStream.Position;
  if sz <= 0 then Exit;
  SetLength(data, sz);
  if FStream.Read(data[0], sz) <> sz then Exit;

  mp3dec_init_core(core);
  SetLength(framePCM, MINIMP3_MAX_SAMPLES_PER_FRAME);
  FillChar(info, SizeOf(info), 0);

  // Locate the first frame and inspect it for a Xing/Info+LAME tag (gapless).
  toSkip := 0; detectedFrames := -1; pos := 0;
  freefmt := 0; ffSize := 0;
  ffOfs := mp3d_find_frame(@data[0], sz, freefmt, ffSize);
  if (ffSize > 0) and (ffOfs + ffSize <= sz) then
  begin
    vbr := CheckVbrtag(@data[ffOfs], ffSize, vbrFrames, vbrDelay, vbrPadding);
    if vbr > 0 then
    begin
      toSkip := vbrDelay;                       // leading samples to drop (per channel)
      detectedFrames := Int64(hdr_frame_samples(@data[ffOfs])) * vbrFrames - vbrDelay;
      if vbrPadding > 0 then detectedFrames := detectedFrames - vbrPadding;
      if detectedFrames < 0 then detectedFrames := 0;
      pos := ffOfs + ffSize;                    // skip the Xing/Info tag frame itself
      mp3dec_init_core(core);                   // fresh reservoir for the first real frame
    end;
  end;

  cap := 1 shl 20;
  SetLength(FPCM, cap);
  total := 0;
  FChannels := 0; FSampleRate := 0;

  while pos < sz do
  begin
    samples := mp3dec_decode_frame(core, scratch, @data[pos], sz - pos, @framePCM[0], info);
    if info.FrameBytes = 0 then Break;       // no further frame found
    if samples > 0 then
    begin
      if FChannels = 0 then
      begin
        FChannels := info.Channels;
        FSampleRate := info.Hz;
      end;
      // Drop the encoder/decoder-delay frames at the very start (gapless).
      startFrame := 0;
      if toSkip > 0 then
      begin
        skip := toSkip; if skip > samples then skip := samples;
        startFrame := skip;
        Dec(toSkip, skip);
      end;
      k := (samples - startFrame) * info.Channels;
      if k > 0 then
      begin
        if (total + k) > cap then
        begin
          while (total + k) > cap do cap := cap * 2;
          SetLength(FPCM, cap);
        end;
        Move(framePCM[startFrame * info.Channels], FPCM[total], k * SizeOf(Single));
        Inc(total, k);
      end;
    end;
    Inc(pos, info.FrameBytes);
  end;

  if (FChannels = 0) or (total = 0) then
  begin
    FLastError := 'MP3: no decodable frames';
    Exit;
  end;
  FFrameCount := total div FChannels;
  // Trim trailing encoder padding to the tag's detected length.
  if (detectedFrames >= 0) and (FFrameCount > detectedFrames) then
    FFrameCount := detectedFrames;
  SetLength(FPCM, FFrameCount * FChannels);
  Result := True;
end;

function TSedaiMP3Decoder.OpenStream(AStream: TStream): Boolean;
begin
  Result := False;
  FStream := AStream;
  FPosition := 0;
  FBitsPerSample := 16;
  if not DecodeAll then Exit;
  FTotalFrames := FFrameCount;
  FSeekable := True;
  Result := True;
end;

function TSedaiMP3Decoder.ReadFrames(ABuffer: PSingle; AFrameCount: Integer): Integer;
var avail, take: Integer;
begin
  avail := FFrameCount - FPosition;
  if avail <= 0 then Exit(0);
  take := AFrameCount;
  if take > avail then take := avail;
  Move(FPCM[FPosition * FChannels], ABuffer^, take * FChannels * SizeOf(Single));
  FPosition := FPosition + take;
  Result := take;
end;

function TSedaiMP3Decoder.Seek(AFrame: Int64): Boolean;
begin
  if AFrame < 0 then AFrame := 0;
  if AFrame > FFrameCount then Exit(False);
  FPosition := AFrame;
  Result := True;
end;

end.
