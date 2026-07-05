# SAF Instrument Library

Ready-to-play instruments for the Sedai Audio Foundation, shipped as `.safinst`
files. Each `.safinst` is a text library holding one or more presets; load it
with `TSedaiInstrumentRegistry.LoadFromStream` (or `LoadFromFile`) and apply a
preset to a `TSAFPart`.

## Libraries

| File | Contents | Technique | Licence |
|------|----------|-----------|---------|
| `builtin.safinst` | 32 synth presets: classic (9), FM (5), wavetable (2), additive (6), Karplus (5), SID (5) | all techniques | GPL-3.0 (SAF original) |
| `winds.safinst` | Clarinet, Soprano/Alto/Tenor Sax | `psReed` (waveguide physical model) | GPL-3.0 (SAF original) |
| `vcsl.safinst` | Alto Recorder, Saxello, Tenor Sax (×2 registers) | `psAdditive` (resynthesis from analysis) | preset data CC0 (see below) |

*(more libraries — free-partial high-fidelity instruments, more sampled-derived
families — land here as they are curated.)*

## Licensing & attribution

- **`builtin.safinst`** — SAF's out-of-the-box synth palette across every
  synthesis technique the engine offers (subtractive, FM, wavetable, additive,
  Karplus-Strong, SID-flavoured). All SAF-original, no external data. Licence
  GPL-3.0, same as SAF.

- **`winds.safinst`** — physically-modelled reed instruments generated entirely
  by SAF's `TSedaiReedGenerator` (a re-derivation of the McIntyre–Schumacher–
  Woodhouse reed model / Smith & STK clarinet + Saxofony algorithms by Perry Cook
  and Gary Scavone; re-implemented in Free Pascal, no STK code or dependency).
  No sampled audio. Licence: GPL-3.0, same as SAF.

- **`vcsl.safinst`** — additive presets resynthesised from single-note samples of
  the **Versilian Community Sample Library (VCSL)** by Versilian Studios, released
  under **CC0 (public domain)**. SAF's analysis pipeline extracts a per-harmonic
  additive model (levels + amplitude tracks) from each sample; the resulting preset
  *data* is a derived analysis of CC0 audio and is likewise CC0. No sample audio is
  shipped — only the additive parameters. Credit: Versilian Studios, VCSL. Each
  preset's `desc=` field names its source sample. SAF's rule: a code licence
  (GPL/AGPL) binds the code, not the preset data; each data source is credited on
  principle.

## Playing a preset (sketch)

```pascal
reg := TSedaiInstrumentRegistry.CreateEmpty;
fs  := TFileStream.Create('library/winds.safinst', fmOpenRead);
try reg.LoadFromStream(fs); finally fs.Free; end;

part := TSAFPart.Create;
part.SetSampleRate(48000);
reg.ApplyToPartByName('Tenor Sax', part);   // configures the voice pool
part.NoteOn(60, 1.0);                         // then RenderBlock(...)
```
