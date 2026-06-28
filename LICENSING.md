# Licensing

Sedai Audio Foundation is distributed under **two** licenses, depending on the file.

## 1. Most of the project — dual licensed: GPL-3.0 **OR** Commercial

All Sedai Audio Foundation code authored for this project is dual-licensed:

- **Open source:** GNU General Public License v3.0 — full text in [`LICENSE`](LICENSE).
- **Commercial / proprietary:** available on request — contact
  **Maurizio Cammalleri** &lt;maurizio.cammalleri@gmail.com&gt;.

You may use this code under *either* option.

## 2. SID engine and GoatTracker player — GPL-2.0-or-later only (no commercial)

The following files are **ports of GPL'd third-party engines** and are therefore
**derivative works** to which the dual license above **does not apply**. They are
distributed under the **same license as their upstream — the GNU GPL version 2 or (at
your option) any later version** (see [`LICENSE.GPL-2.0`](LICENSE.GPL-2.0)):

| File | Derived from | Upstream author |
|------|--------------|-----------------|
| `src/SID/SedaiSIDEvo.pas` | reSID / reSID-fp | Dag Lem / Antti S. Lankila |
| `src/SID/SedaiSIDEvo_WaveTables.inc` (reSID combined-waveform data, verbatim) | reSID | Dag Lem |
| `src/Players/SedaiGoatTracker.pas` | GoatTracker 2 player routine | Lasse Öörni |

A commercial/proprietary license for these files cannot be granted without permission
from the respective upstream copyright holders. The `sng_player` front-end
(`test/sng_player.lpr`) links these GPL units, so the resulting executable is GPL too.

> "GPL-2.0-or-later" means the GPL v2 terms apply, and you may also choose to comply under
> any later GPL version (e.g. GPL-3.0). The full GPL v2 text is provided so the v2 option
> is available; the GPL v3 text is in `LICENSE`.

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
upstream code vendored), so they are part of the dual-licensed code in section 1 — not
derivative works — but the formats themselves are very much someone else's labour, and that
deserves recognition:

- **Josh Coalson** and the **Xiph.Org Foundation** — author and maintainers of **FLAC**
  (Free Lossless Audio Codec) and its format specification, from which `SedaiFLACDecoder` is
  implemented. Thank you for an open, meticulously documented lossless format — the clarity of
  the spec is the reason a from-scratch decoder was even feasible.

As OGG Vorbis and MP3 decoding are added, this recognition will extend to the Xiph.Org Vorbis
authors and to the format / reference-decoder authors actually drawn upon (e.g. if a
public-domain reference decoder such as stb_vorbis or minimp3 is ported rather than written
from the spec, its author is credited here and in the unit header).

See the headers of `src/SID/SedaiSIDEvo.pas`, `src/Players/SedaiGoatTracker.pas` and
`src/FileIO/SedaiFLACDecoder.pas` for the detailed per-component attribution.
