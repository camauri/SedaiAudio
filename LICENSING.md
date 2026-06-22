# Licensing

Sedai Audio Foundation is distributed under **two** licenses, depending on the file.

## 1. Most of the project — dual licensed: GPL-3.0 **OR** Commercial

All Sedai Audio Foundation code authored for this project is dual-licensed:

- **Open source:** GNU General Public License v3.0 — full text in [`LICENSE`](LICENSE).
- **Commercial / proprietary:** available on request — contact
  **Maurizio Cammalleri** &lt;maurizio.cammalleri@gmail.com&gt;.

You may use this code under *either* option.

## 2. The SID engine — GPL-2.0-or-later only (no commercial)

The following files are a **Pascal port of reSID / reSID-fp** (partially built on the SAF
framework) and are therefore a **derivative work of reSID**:

- `src/SID/SedaiSIDEvo.pas`
- `src/SID/SedaiSIDEvo_WaveTables.inc` (reSID combined-waveform data, verbatim)

Because reSID is licensed under the **GNU GPL version 2 or (at your option) any later
version**, these files carry the **same license** — see [`LICENSE.GPL-2.0`](LICENSE.GPL-2.0).
The dual "GPL-3.0 OR Commercial" option above **does not apply** to them: a
commercial/proprietary license for the SID engine cannot be granted without permission
from the reSID copyright holders.

> "GPL-2.0-or-later" means the GPL v2 terms apply, and you may also choose to comply under
> any later GPL version (e.g. GPL-3.0). The full GPL v2 text is provided so the v2 option
> is available; the GPL v3 text is in `LICENSE`.

## Acknowledgements & thanks

The SID emulation in this project exists thanks to the work of others, released under the GPL:

- **Dag Lem** &lt;resid@nimrod.no&gt; — author of **reSID**, the cycle-accurate
  MOS 6581/8580 SID emulator (Copyright © 2004 Dag Lem). All credit for the original SID
  emulation algorithms and the combined-waveform data goes to him. `SedaiSIDEvo` is a
  faithful, bit-exact port of his engine — our heartfelt thanks.
- **Antti S. Lankila** — author of the **reSID-fp** non-linear 6581 filter ("distortion")
  model, from which the optional `sfmDistortion` filter is ported. Thank you.
- **Julius O. Smith III** — band-limited resampling theory and `filterkit` (the
  Kaiser-windowed sinc FIR and the Bessel I0 function), on which the resampler is based.

See the header of `src/SID/SedaiSIDEvo.pas` for the detailed per-component attribution.
