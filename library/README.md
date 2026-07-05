# SAF Instrument Library

Ready-to-play instruments for the Sedai Audio Foundation, shipped as `.safinst`
files. Each `.safinst` is a text library holding one or more presets; load it
with `TSedaiInstrumentRegistry.LoadFromStream` (or `LoadFromFile`) and apply a
preset to a `TSAFPart`.

## Libraries

| File | Contents | Technique | Licence |
|------|----------|-----------|---------|
| `winds.safinst` | Clarinet, Soprano/Alto/Tenor Sax | `psReed` (waveguide physical model) | GPL-3.0 (SAF original) |

*(more libraries — additive/partial from CC0 samples, classic/FM synths, SID —
land here as they are curated.)*

## Licensing & attribution

- **`winds.safinst`** — physically-modelled reed instruments generated entirely
  by SAF's `TSedaiReedGenerator` (a re-derivation of the McIntyre–Schumacher–
  Woodhouse reed model / Smith & STK clarinet + Saxofony algorithms by Perry Cook
  and Gary Scavone; re-implemented in Free Pascal, no STK code or dependency).
  No sampled audio. Licence: GPL-3.0, same as SAF.

- Preset libraries **derived from external sample sets** (e.g. VCSL, CC0) will
  carry their own attribution here and keep the source's data licence; SAF's rule
  is that a code licence (GPL/AGPL) binds the code, not the preset data, and each
  data source is credited on principle — see the project's attribution policy.

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
