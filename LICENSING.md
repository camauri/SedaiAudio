# Licensing

Sedai Audio Foundation is distributed under the **GNU General Public License v3.0**.

## The project — GPL-3.0

All Sedai Audio Foundation code is released under the **GNU General Public License
version 3.0** — full text in [`LICENSE`](LICENSE). There is no commercial or
proprietary option: if you distribute a work based on this code, it must be GPL too.

## Third-party derived components

The following files are **ports of GPL'd third-party engines**, so they are derivative
works and carry their upstream's terms — the **GNU GPL version 2 or (at your option) any
later version** (full text in [`LICENSE.GPL-2.0`](LICENSE.GPL-2.0)):

| File | Derived from | Upstream author |
|------|--------------|-----------------|
| `src/SID/SedaiSIDEvo.pas` | reSID / reSID-fp | Dag Lem / Antti S. Lankila |
| `src/SID/SedaiSIDEvo_WaveTables.inc` (reSID combined-waveform data, verbatim) | reSID | Dag Lem |
| `src/Players/SedaiGoatTracker.pas` | GoatTracker 2 player routine | Lasse Öörni |

The "**or later**" in their upstream licence is what lets them sit inside a GPL-3.0
project: as distributed here they are covered by GPL-3.0 like everything else, and the
v2 text is kept so that option remains available to anyone who prefers it. Their upstream
terms cannot be narrowed — a fact worth remembering if the licensing question is ever
revisited, because these files are the reason the project cannot be anything but GPL.

## Acknowledgements & thanks

The SID emulation and tracker playback in this project exist thanks to the work of others,
released under the GPL:

- **Dag Lem** &lt;resid@nimrod.no&gt; — author of **reSID**, the cycle-accurate
  MOS 6581/8580 SID emulator (Copyright © 2004 Dag Lem). All credit for the original SID
  emulation algorithms and the combined-waveform data goes to him. `SedaiSIDEvo` is a
  faithful, bit-exact port of his engine — our heartfelt thanks.
- **Antti S. Lankila** — author of the **reSID-fp** non-linear 6581 filter ("distortion")
  model, from which the optional `sfmDistortion` filter is ported. Thank you.
- **Lasse Öörni** (Cadaver / Covert Bitops) — author of **GoatTracker 2**, the C64 music
  editor and player. `SedaiGoatTracker` is a reimplementation of his player routine
  (byte-exact register output) and uses his note frequency tables. Thank you.
- **Julius O. Smith III** — band-limited resampling theory and `filterkit` (the
  Kaiser-windowed sinc FIR and the Bessel I0 function), on which the resampler is based.

We likewise stand on the work of those who designed the **open audio formats** we decode.
These decoders are implemented **clean-room from the public format specifications** (no
upstream code vendored), so they are original work under the GPL-3.0 above rather than
derivative works — but the formats themselves are very much someone else's labour, and that
deserves recognition:

- **Josh Coalson** and the **Xiph.Org Foundation** — author and maintainers of **FLAC**
  (Free Lossless Audio Codec) and its format specification, from which `SedaiFLACDecoder` and
  `SedaiFLACEncoder` are implemented clean-room. Thank you for an open, meticulously documented
  lossless format — the clarity of the spec is the reason a from-scratch decoder *and encoder*
  were even feasible.

- The **Xiph.Org Foundation** and the **Vorbis** authors — for the **Ogg** container and
  **Vorbis I** specifications, from which `SedaiVorbisDecoder` is implemented clean-room
  (container paging/CRC, codebooks, floor, residue, channel coupling, IMDCT). The
  `floor1_inverse_dB` lookup table is the constant table given in the Vorbis I specification.
  No upstream decoder source (e.g. `stb_vorbis`, `libvorbis`) was vendored.

- **Lieff** and the **minimp3** contributors — `src/FileIO/SedaiMP3Decoder.pas` is a faithful
  pure-Pascal **port of minimp3** (https://github.com/lieff/minimp3), the public-domain (CC0)
  single-file MP3 decoder, Layer III only. The constant tables in `src/FileIO/SedaiMP3Tables.inc`
  are extracted verbatim from minimp3. minimp3 is released under CC0 (public domain), so the
  port carries no copyleft obligation; this credit is given on principle. (MP3 decoding is
  patent-unencumbered worldwide since 2017.)

See the headers of `src/SID/SedaiSIDEvo.pas`, `src/Players/SedaiGoatTracker.pas`,
`src/FileIO/SedaiFLACDecoder.pas`, `src/FileIO/SedaiFLACEncoder.pas`,
`src/FileIO/SedaiVorbisDecoder.pas` and `src/FileIO/SedaiMP3Decoder.pas` for the detailed
per-component attribution.
