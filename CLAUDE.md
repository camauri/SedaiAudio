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

**Dual-platform: the project is developed on both Windows and Linux — never break either one.**
The primary machine is now Linux (`/home/camauri/Progetti/FreePascal/SedaiAudioFoundation`);
the Windows checkout was `C:\Progetti\Artiforge\SedaiAudioFoundation`.

The user handles compilation. For reference only:

- **Language**: Free Pascal (FPC), `{$mode objfpc}{$H+}`
- **Build scripts**: `build.ps1` (Windows) / `build.sh` (Linux/macOS) — functionally equivalent ports; `setup.ps1` / `setup.sh` fetch deps
  - PowerShell `-Switch` ⇄ bash `--switch` (`-Target x` ⇄ `--target x`, `-SkipDemos` ⇄ `--skip-demos`, ...); the mapping is in each script's header
- **Targets**: `sng_player`, `sng_dump` (tool) · `saf_play`, `demo_synth` (demo) · `test_saf_main`, `audiotest`, `sedaisid_test`, `saf_regression` (test)
  - Build one: `./build.ps1 -Target sng_player` / `./build.sh --target sng_player`  (add `-Clean` / `--clean` to rebuild)
- **Output**: `bin/<cpu>-<os>/` (executables), `lib/<cpu>-<os>/` (compiled units) — Linux artefacts live in `bin/x86_64-linux/`, Windows ones in `bin/x86_64-win64/`
- **Platform flags**: `-CPU`/`--cpu` `x86_64|i386|aarch64`, `-OS`/`--os` `win64|win32|linux|darwin` (the .sh defaults to the host)
- **cthreads**: `{$IFDEF UNIX}cthreads,{$ENDIF}` must be the FIRST unit of any program that opens audio (SDL2 calls back from its own thread) — `sng_player`, `demo_synth`, `audiotest`, `TestSAFMain`
- **SDL2 on Linux needs the `-dev` package** (`libsdl2-dev` / `SDL2-devel` / `sdl2`): the bindings `dlopen("libSDL2.so")`, which only the dev symlink provides. Missing ⇒ silent no-audio, not a crash

## Running Programs

Linux (`bin/x86_64-linux/`), Windows (`bin/x86_64-win64/`, add `.exe`):

```
bin/x86_64-linux/sng_player [--sdl2] <file.sng> [subtune]   # GoatTracker .sng player (SAF backend by default)
bin/x86_64-linux/sng_dump <file.sng>                        # Dump .sng structure
bin/x86_64-linux/sedaisid_test                              # SID emulation reference/regression test
bin/x86_64-linux/saf_regression                             # Headless render-path regression suite
bin/x86_64-linux/demo_synth                                 # Synthesis demo
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

README.md was updated 2026-06-22 — the **SID Evo** and **GoatTracker Player** sections (emulation accuracy, sampling methods, filter models, player fidelity, `sng_player` controls) are current and accurate. The rest of README, plus ARCHITECTURE_PROPOSAL.md / TODO.md (both gitignored), may still have drifted (audit 2026-06-21): some documented APIs/demos no longer exist and some implemented units are undocumented. Verify against sources before relying on the non-SID parts.
