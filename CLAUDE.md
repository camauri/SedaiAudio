# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Critical Rules

- **NEVER compile or run**: Do not run `build.ps1`, `build.sh`, `fpc`, `lazbuild`, or launch any executable (`sng_player.exe`, `demo_synth.exe`, ...). The user builds and runs manually to hear/see output himself. Provide the command instead.
- **NEVER take initiative on code changes**: When asked a question, answer it. Propose changes and wait for explicit confirmation ("sì"/"procedi") before editing source. Creating docs/memory the user requested is fine; touching `.pas`/`.lpr` without a go-ahead is not.
- **NEVER perform destructive git operations** on the working tree (`reset --hard`, `checkout --`, `restore`, `clean`). There are local, unpushed modifications.
- **NEVER delete or move user files** without explicit authorization.
- **Communicate in Italian**; keep code comments and documentation in English.
- **Keep the repo root clean**: build scripts + `README.md`/`LICENSE`/`*.md` docs only. Scratch/test files go in `./tmp/`.

## Build System

The user handles compilation. For reference only:

- **Language**: Free Pascal (FPC), `{$mode objfpc}{$H+}`
- **Build scripts**: `build.ps1` (Windows) / `build.sh` (Linux/macOS); `setup.ps1` / `setup.sh` fetch deps (SDL2 dll downloaded separately)
- **Targets**: `test_saf_main`, `demo_synth`, `sng_player`, `sng_dump`, `audiotest`, `sedaisid_test`
  - Build one: `./build.ps1 -Target sng_player`  (add `-Clean` to rebuild)
- **Output**: `bin/<cpu>-<os>/` (executables), `lib/<cpu>-<os>/` (compiled units)
- **Platform flags**: `-CPU x86_64|i386|aarch64`, `-OS win64|win32|linux|darwin`

## Running Programs

```
bin/x86_64-win64/sng_player.exe [--sdl2] <file.sng> [subtune]   # GoatTracker .sng player (SAF backend by default)
bin/x86_64-win64/sng_dump.exe <file.sng>                        # Dump .sng structure
bin/x86_64-win64/sedaisid_test.exe                              # SID emulation reference/regression test
bin/x86_64-win64/demo_synth.exe                                 # Synthesis demo
```

`sng_player` controls during playback: SPACE pause, R restart, L loop, V/W verbose, S tables, 1/2/3 mute voices, +/- subtune, Q/ESC quit.

## Architecture

Sedai Audio Foundation (SAF) is a modular Free Pascal audio framework.

- `src/SedaiAudioFoundation.pas` — high-level facade. `TSAFSynthType = (safClassic, safFM, safWavetable)`. NOTE: the Additive generator and SID Evo are standalone units, not wired into this facade.
- `src/SID/SedaiSIDEvo.pas` — MOS 6581/8580 SID emulation (ReSID-style two-integrator-loop model), plus EVO extensions (up to 64 voices, stereo, LFO, extended waveforms). Core of the project (~4100 lines).
- `src/Players/SedaiGoatTracker.pas` — clean reimplementation of the GoatTracker v2 player routine (gplay.c/gsid.cpp/gsound.c); plays `.sng`. Frontend = `sng_player`.
- `src/Players/SedaiMIDIPlayer.pas` — `TSedaiMIDIPlayer` (object API, not the global API shown in some README sections).
- `src/Platform/SedaiAudioBackend.pas` — audio backend (SAF default, SDL2 fallback via `--sdl2`).
- Other folders: `Core/`, `Generators/`, `Effects/`, `Processors/`, `Modulators/`, `Mixer/`, `Voice/`, `Transport/`, `FileIO/`, `Wavetable/`, `Engine/`.

`src.bak/`, `src.old/`, `test.bak/`, `test.old/`, `temp/` are historical/working dirs (gitignored) — ignore unless investigating what was removed.

## Documentation Status

README.md / ARCHITECTURE_PROPOSAL.md / TODO.md have drifted from the code (audit 2026-06-21): several documented APIs/demos no longer exist, and some implemented units are undocumented. Treat docs as approximate; verify against sources before relying on them.
