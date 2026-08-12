#!/usr/bin/env bash
#
# SedaiAudio Foundation - Setup Script for Linux/macOS
# Copyright (c) 2025 Maurizio Cammalleri
# Released under GNU GPL v3 or Commercial License
#
# Functional port of setup.ps1:
#   - creates the directory structure (bin/<platform>, lib/<platform>, deps)
#   - checks (and optionally installs) the Free Pascal Compiler
#   - downloads the SDL2 Pascal bindings into deps/sdl2 (same release + SHA256
#     as setup.ps1)
#   - checks the SDL2 shared library (on Windows setup.ps1 downloads SDL2.dll;
#     on Linux/macOS the library comes from the system package manager)
#   - builds the project through ./build.sh
#
# Option mapping setup.ps1 -> setup.sh
#   -SkipFpc        (default here)   -ForceFpc       --force-fpc
#   -ForceSDL2      --force-sdl2     -SkipSDL2       --skip-sdl2
#   -ForceRuntime   --force-runtime  -SkipRuntime    --skip-runtime
#   -Clean          --clean
# Extra: --install-fpc (install FPC via the system package manager),
#        --no-build (stop before compiling).
#

set -u

# ============================================================================
# Configuration
# ============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR"
SRC_DIR="$PROJECT_ROOT/src"
DEPS_DIR="$PROJECT_ROOT/deps"
SDL2_DIR="$DEPS_DIR/sdl2"
SDL2_MARKER="$SDL2_DIR/sdl2.pas"

# Host platform -> same <cpu>-<os> naming build.sh uses
case "$(uname -m)" in
    x86_64|amd64)  HOST_CPU="x86_64" ;;
    aarch64|arm64) HOST_CPU="aarch64" ;;
    i?86)          HOST_CPU="i386" ;;
    *)             HOST_CPU="x86_64" ;;
esac
case "$(uname -s)" in
    Linux)  HOST_OS="linux" ;;
    Darwin) HOST_OS="darwin" ;;
    *)      HOST_OS="linux" ;;
esac
PLATFORM="$HOST_CPU-$HOST_OS"

BIN_DIR="$PROJECT_ROOT/bin/$PLATFORM"
LIB_DIR="$PROJECT_ROOT/lib/$PLATFORM"

# SDL2 Pascal bindings download configuration (identical to setup.ps1)
SDL2_VERSION="2.3"
SDL2_DOWNLOAD_URL="https://github.com/camauri/SedaiBasic2-Deps/releases/download/SDL2-for-Pascal-v2.3/SDL2-for-Pascal-v2.3.zip"
SDL2_EXPECTED_HASH="829dd68bebfe7756bf037160e7cc268c115976d640480d73ebb8badaa46a9e47"

# Default flags
INSTALL_FPC=false
FORCE_FPC=false
FORCE_SDL2=false
FORCE_RUNTIME=false
SKIP_SDL2=false
SKIP_RUNTIME=false
CLEAN_BUILD=false
DO_BUILD=true

# Colors
if [[ -t 1 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
    CYAN='\033[0;36m'; WHITE='\033[1;37m'; GRAY='\033[0;90m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; CYAN=''; WHITE=''; GRAY=''; NC=''
fi

# ============================================================================
# Download helpers
# ============================================================================
UTILS_PATH="$PROJECT_ROOT/scripts/lib/download-utils.sh"
if [[ -f "$UTILS_PATH" ]]; then
    # shellcheck source=scripts/lib/download-utils.sh
    source "$UTILS_PATH"
else
    echo -e "${RED}ERROR: download-utils.sh not found at: $UTILS_PATH${NC}"
    exit 1
fi

# ============================================================================
# Display helpers
# ============================================================================
show_banner() {
    local border="======================================================================"
    echo ""
    echo -e "${CYAN}${border}${NC}"
    echo ""
    echo -e "${WHITE}   ____           _       _    _             _ _       ${NC}"
    echo -e "${WHITE}  / ___|  ___  __| | __ _(_)  / \\  _   _  __| (_) ___  ${NC}"
    echo -e "${WHITE}  \\___ \\ / _ \\/ _\` |/ _\` | | / _ \\| | | |/ _\` | |/ _ \\ ${NC}"
    echo -e "${WHITE}   ___) |  __/ (_| | (_| | |/ ___ \\ |_| | (_| | | (_) |${NC}"
    echo -e "${WHITE}  |____/ \\___|\\__,_|\\__,_|_/_/   \\_\\__,_|\\__,_|_|\\___/ ${NC}"
    echo ""
    echo -e "${YELLOW}                   SETUP SCRIPT${NC}"
    echo ""
    echo -e "${CYAN}${border}${NC}"
    echo ""
    echo -e "${GRAY}  Audio Foundation - Professional Audio Synthesis Library${NC}"
    echo -e "${GRAY}  Copyright (c) 2025 Maurizio Cammalleri - GPL-3.0 or Commercial${NC}"
    echo ""
    echo -e "${CYAN}${border}${NC}"
    echo ""
}

show_help() {
    echo "SedaiAudio Foundation - Setup Script for Linux/macOS"
    echo ""
    echo "Usage: ./setup.sh [options]"
    echo ""
    echo "Options:"
    echo "  --install-fpc     Install FPC via the system package manager (needs sudo)"
    echo "  --force-fpc       Reinstall FPC even if already present (implies --install-fpc)"
    echo "  --force-sdl2      Force reinstallation of the SDL2 Pascal bindings"
    echo "  --force-runtime   Re-check the SDL2 shared library even if found"
    echo "  --skip-sdl2       Skip the SDL2 Pascal bindings installation"
    echo "  --skip-runtime    Skip the SDL2 shared library check"
    echo "  --clean           Clean bin/ and lib/ before setup"
    echo "  --no-build        Do not build the project at the end"
    echo "  --help            Show this help message"
    echo ""
    echo "Examples:"
    echo "  ./setup.sh                    # Full setup (recommended)"
    echo "  ./setup.sh --install-fpc      # Setup + install FPC"
    echo "  ./setup.sh --clean            # Clean and reinstall everything"
    echo "  ./setup.sh --no-build         # Dependencies only"
    echo ""
    echo "Notes:"
    echo "  - The SDL2 Pascal bindings are required to COMPILE"
    echo "  - The SDL2 shared library is required to PLAY audio; it is loaded at"
    echo "    runtime (SedaiAudioSDL2Dyn), so a missing library only means silence"
    echo "  - FPC installation is optional if you already have it"
    echo "  - After setup, run ./build.sh to compile"
    echo ""
}

show_step() {   # show_step <n> <total> <title>
    echo ""
    echo -e "${CYAN}  [$1/$2] $3${NC}"
    echo -e "${GRAY}  ------------------------------------------------------------${NC}"
}

status_ok()   { echo -e "      ${GREEN}[OK]${NC} $1"; }
status_err()  { echo -e "      ${RED}[ERROR]${NC} $1"; }
status_warn() { echo -e "      ${YELLOW}[!]${NC} $1"; }
status_skip() { echo -e "      ${YELLOW}[SKIP]${NC} $1"; }
status_info() { echo -e "      ${GRAY}$1${NC}"; }

show_summary() {   # show_summary <success:true|false>
    local border="======================================================================"
    echo ""
    if [[ "$1" == true ]]; then
        echo -e "${GREEN}${border}${NC}"
        echo ""
        echo -e "${GREEN}  SETUP COMPLETED SUCCESSFULLY!${NC}"
        echo ""
        echo -e "${WHITE}  Directory structure:${NC}"
        echo -e "${GRAY}    - bin/$PLATFORM/   (executables)${NC}"
        echo -e "${GRAY}    - lib/$PLATFORM/   (compiled units)${NC}"
        echo -e "${GRAY}    - deps/sdl2/       (SDL2 Pascal bindings)${NC}"
        echo ""
        echo -e "${WHITE}  Next steps:${NC}"
        echo -e "${GRAY}    1. Run ./build.sh to compile${NC}"
        echo -e "${GRAY}    2. Executables will be in bin/$PLATFORM/${NC}"
        echo ""
        echo -e "${GREEN}${border}${NC}"
    else
        echo -e "${RED}${border}${NC}"
        echo ""
        echo -e "${RED}  SETUP FAILED!${NC}"
        echo ""
        echo -e "${WHITE}  Please check the error messages above.${NC}"
        echo ""
        echo -e "${RED}${border}${NC}"
    fi
    echo ""
}

# ============================================================================
# Parse Arguments
# ============================================================================
while [[ $# -gt 0 ]]; do
    case "$1" in
        --install-fpc)   INSTALL_FPC=true; shift ;;
        --force-fpc)     INSTALL_FPC=true; FORCE_FPC=true; shift ;;
        --skip-fpc)      INSTALL_FPC=false; shift ;;   # accepted for symmetry with setup.ps1
        --force-sdl2)    FORCE_SDL2=true; shift ;;
        --force-runtime) FORCE_RUNTIME=true; shift ;;
        --skip-sdl2)     SKIP_SDL2=true; shift ;;
        --skip-runtime)  SKIP_RUNTIME=true; shift ;;
        --clean)         CLEAN_BUILD=true; shift ;;
        --no-build)      DO_BUILD=false; shift ;;
        --help|-h)       show_help; exit 0 ;;
        *)
            echo -e "${RED}ERROR: Unknown option: $1${NC}"
            show_help
            exit 1
            ;;
    esac
done

# ============================================================================
# Steps
# ============================================================================
clean_directories() {
    status_info "Cleaning directories..."
    if [[ -d "$BIN_DIR" ]]; then
        rm -rf "${BIN_DIR:?}"/*
        status_ok "Cleaned: bin/$PLATFORM"
    fi
    if [[ -d "$LIB_DIR" ]]; then
        rm -rf "${LIB_DIR:?}"/*
        status_ok "Cleaned: lib/$PLATFORM"
    fi
    if [[ "$FORCE_SDL2" == true && -d "$SDL2_DIR" ]]; then
        rm -rf "$SDL2_DIR"
        status_ok "Cleaned: deps/sdl2"
    fi
}

init_directory_structure() {
    status_info "Checking directory structure..."

    local d name
    for d in "$BIN_DIR:bin/$PLATFORM" "$LIB_DIR:lib/$PLATFORM" "$DEPS_DIR:deps"; do
        name="${d#*:}"; d="${d%%:*}"
        if [[ ! -d "$d" ]]; then
            mkdir -p "$d"
            status_ok "Created: $name"
        else
            status_info "Exists: $name"
        fi
    done

    if [[ ! -d "$SRC_DIR" ]]; then
        status_err "Source directory not found: $SRC_DIR"
        return 1
    fi
    status_ok "Source: src/"
    return 0
}

# Same discovery order as build.sh, so an fpcupdeluxe install outside PATH
# (e.g. ~/tools/fp/fpc-stable/fpc/bin/x86_64-linux/fpc) is picked up too.
FPC_FOUND=""
find_fpc() {
    if [[ -n "${FPC:-}" && -x "${FPC:-}" ]]; then echo "$FPC"; return 0; fi

    local candidates=(
        "$PROJECT_ROOT/fpc/bin/$PLATFORM/fpc"
        "$PROJECT_ROOT/fpc/bin/fpc"
        "$PROJECT_ROOT/fpc/3.2.2/bin/$PLATFORM/fpc"
    )
    local d
    for d in "$HOME"/tools/fp/*/fpc "$HOME"/fpcupdeluxe/fpc "$HOME"/fpc "$HOME"/tools/fpc; do
        candidates+=("$d/bin/$PLATFORM/fpc" "$d/bin/fpc")
    done
    candidates+=("/usr/local/bin/fpc" "/opt/fpc/bin/fpc")

    local c
    for c in "${candidates[@]}"; do
        [[ -x "$c" ]] && { echo "$c"; return 0; }
    done
    command -v fpc 2>/dev/null && return 0
    return 1
}

install_fpc_step() {
    FPC_FOUND="$(find_fpc)" || FPC_FOUND=""

    if [[ -n "$FPC_FOUND" && "$FORCE_FPC" == false ]]; then
        status_ok "FPC $("$FPC_FOUND" -iV 2>/dev/null || echo '?') found at $FPC_FOUND"
        return 0
    fi

    if [[ "$INSTALL_FPC" == false ]]; then
        status_err "FPC not found and --install-fpc was not given."
        status_info "Searched: \$FPC, ./fpc/, ~/tools/fp/*/fpc, ~/fpcupdeluxe/fpc, ~/fpc, PATH."
        status_info "Install it with your package manager, e.g.:"
        status_info "  Debian/Ubuntu: sudo apt install fpc"
        status_info "  Fedora:        sudo dnf install fpc"
        status_info "  Arch:          sudo pacman -S fpc"
        status_info "  macOS:         brew install fpc"
        status_info "Or re-run: ./setup.sh --install-fpc"
        return 1
    fi

    local installer="$PROJECT_ROOT/scripts/linux/install-fpc.sh"
    if [[ ! -f "$installer" ]]; then
        status_err "FPC install script not found: $installer"
        return 1
    fi

    local args=()
    [[ "$FORCE_FPC" == true ]] && args+=("--force")

    bash "$installer" ${args[@]+"${args[@]}"}
    local rc=$?
    case $rc in
        0|5)
            FPC_FOUND="$(find_fpc)" || FPC_FOUND=""
            status_ok "FPC ready${FPC_FOUND:+ at $FPC_FOUND}"
            return 0
            ;;
        *) status_err "FPC installation failed (exit code: $rc)"; return 1 ;;
    esac
}

test_sdl2_bindings() { [[ -f "$SDL2_MARKER" ]]; }

install_sdl2_step() {
    if test_sdl2_bindings; then
        if [[ "$FORCE_SDL2" == true ]]; then
            status_warn "Removing existing SDL2 bindings..."
            rm -rf "$SDL2_DIR"
        else
            status_skip "SDL2 Pascal bindings already installed"
            return 0
        fi
    fi

    status_info "Installing SDL2 for Pascal v$SDL2_VERSION..."

    local temp_dir zip_file
    temp_dir="${TMPDIR:-/tmp}/sedaiaudio-sdl2-install"
    zip_file="$temp_dir/SDL2-for-Pascal-v$SDL2_VERSION.zip"
    mkdir -p "$temp_dir"

    status_info "Downloading SDL2 Pascal bindings..."
    status_info "URL: $SDL2_DOWNLOAD_URL"
    if ! du_download "$SDL2_DOWNLOAD_URL" "$zip_file"; then
        status_err "$DU_MESSAGE"
        return 1
    fi
    status_ok "Downloaded: $(( DU_BYTES / 1048576 )) MB"

    status_info "Verifying file integrity (SHA256)..."
    if ! du_verify_hash "$zip_file" "$SDL2_EXPECTED_HASH"; then
        status_err "Hash mismatch! File may be corrupted."
        status_info "$DU_MESSAGE"
        rm -f "$zip_file"
        return 1
    fi
    status_ok "Hash verified"

    status_info "Extracting to deps/sdl2..."
    if ! du_extract "$zip_file" "$DEPS_DIR"; then
        status_err "$DU_MESSAGE"
        return 1
    fi
    status_ok "Extraction complete"

    if ! test_sdl2_bindings; then
        # The archive may carry an extra top-level folder
        local found
        found="$(find "$DEPS_DIR" -name 'sdl2.pas' -print -quit 2>/dev/null)"
        if [[ -n "$found" ]]; then
            status_warn "sdl2.pas found at: $(dirname "$found")"
            status_warn "Expected deps/sdl2/ - move it there or re-run with --force-sdl2"
        else
            status_err "SDL2 verification failed - sdl2.pas not found"
            return 1
        fi
    fi

    rm -rf "$temp_dir"
    status_ok "SDL2 for Pascal installed successfully"
    return 0
}

# The Windows setup downloads SDL2.dll into bin/. On Linux/macOS the shared
# library belongs to the system package manager, so we only verify it is there
# and reachable under the exact name the bindings dlopen() at runtime.
check_sdl2_runtime_step() {
    local lib_name pkg_hint found=""
    if [[ "$HOST_OS" == "darwin" ]]; then
        lib_name="libSDL2.dylib"
        pkg_hint="brew install sdl2"
        local p
        for p in /usr/local/lib /opt/homebrew/lib /usr/lib; do
            [[ -e "$p/$lib_name" ]] && { found="$p/$lib_name"; break; }
        done
    else
        # sdl2.pas (FPC branch) declares SDL_LibName = 'libSDL2.so', i.e. the
        # DEVELOPMENT symlink - the runtime package alone ships only
        # libSDL2-2.0.so.0, which dlopen('libSDL2.so') will NOT find.
        lib_name="libSDL2.so"
        pkg_hint="sudo apt install libsdl2-dev   (Fedora: SDL2-devel, Arch: sdl2)"
        # ldconfig lives in /sbin, which is not on a plain user's PATH
        local ldc=""
        if command -v ldconfig >/dev/null 2>&1; then ldc="ldconfig"
        elif [[ -x /sbin/ldconfig ]];            then ldc="/sbin/ldconfig"
        elif [[ -x /usr/sbin/ldconfig ]];        then ldc="/usr/sbin/ldconfig"
        fi
        if [[ -n "$ldc" ]]; then
            found="$("$ldc" -p 2>/dev/null | awk -v n="$lib_name" '$1==n {print $NF; exit}')"
        fi
        if [[ -z "$found" ]]; then
            local p
            for p in /usr/lib/"$(uname -m)"-linux-gnu /usr/lib64 /usr/lib /usr/local/lib; do
                [[ -e "$p/$lib_name" ]] && { found="$p/$lib_name"; break; }
            done
        fi
    fi

    if [[ -n "$found" ]]; then
        status_ok "SDL2 shared library: $found"
        return 0
    fi

    status_warn "SDL2 shared library '$lib_name' not found."
    status_info "Audio output will be silently disabled (the library is loaded"
    status_info "at runtime by SedaiAudioSDL2Dyn; compilation is unaffected)."
    status_info "Install it with: $pkg_hint"
    return 0   # non-fatal by design
}

build_project_step() {
    local build_script="$PROJECT_ROOT/build.sh"
    if [[ ! -f "$build_script" ]]; then
        status_err "Build script not found: $build_script"
        return 1
    fi

    status_info "Building SedaiAudioFoundation using build.sh..."
    status_info "Output: $BIN_DIR/"
    echo ""

    local extra=()
    [[ -n "$FPC_FOUND" ]] && extra+=(--fpc-path "$FPC_FOUND")

    ( cd "$PROJECT_ROOT" && bash "$build_script" --no-banner --skip-demos ${extra[@]+"${extra[@]}"} )
    local rc=$?
    echo ""

    if [[ $rc -eq 0 ]]; then
        status_ok "Build successful!"
        return 0
    fi
    status_err "Compilation failed (exit code: $rc)"
    return 1
}

# ============================================================================
# Main
# ============================================================================
show_banner

total_steps=1                                   # directory structure
[[ "$CLEAN_BUILD"  == true  ]] && total_steps=$((total_steps + 1))
total_steps=$((total_steps + 1))                # FPC check
[[ "$SKIP_SDL2"    == false ]] && total_steps=$((total_steps + 1))
[[ "$SKIP_RUNTIME" == false ]] && total_steps=$((total_steps + 1))
[[ "$DO_BUILD"     == true  ]] && total_steps=$((total_steps + 1))

echo -e "${GRAY}  Configuration:${NC}"
echo -e "${GRAY}    - Platform:              $PLATFORM${NC}"
echo -e "${GRAY}    - Install FPC:           $(if [[ "$INSTALL_FPC" == true ]]; then echo Yes; else echo 'No (check only)'; fi)${NC}"
echo -e "${GRAY}    - Install SDL2 bindings: $(if [[ "$SKIP_SDL2" == false ]]; then echo Yes; else echo 'No (skipped)'; fi)${NC}"
echo -e "${GRAY}    - Check SDL2 runtime:    $(if [[ "$SKIP_RUNTIME" == false ]]; then echo Yes; else echo 'No (skipped)'; fi)${NC}"
echo -e "${GRAY}    - Build:                 $(if [[ "$DO_BUILD" == true ]]; then echo Yes; else echo No; fi)${NC}"
echo -e "${GRAY}    - Clean:                 $(if [[ "$CLEAN_BUILD" == true ]]; then echo Yes; else echo No; fi)${NC}"
echo ""

current_step=0

if [[ "$CLEAN_BUILD" == true ]]; then
    current_step=$((current_step + 1))
    show_step $current_step $total_steps "Cleaning Directories"
    clean_directories
fi

current_step=$((current_step + 1))
show_step $current_step $total_steps "Initializing Directory Structure"
init_directory_structure || { show_summary false; exit 1; }

current_step=$((current_step + 1))
show_step $current_step $total_steps "Checking Free Pascal Compiler"
install_fpc_step || { show_summary false; exit 1; }

if [[ "$SKIP_SDL2" == false ]]; then
    current_step=$((current_step + 1))
    show_step $current_step $total_steps "Installing SDL2 Pascal Bindings"
    install_sdl2_step || { show_summary false; exit 1; }
fi

if [[ "$SKIP_RUNTIME" == false ]]; then
    current_step=$((current_step + 1))
    show_step $current_step $total_steps "Checking SDL2 Shared Library"
    check_sdl2_runtime_step
fi

if [[ "$DO_BUILD" == true ]]; then
    current_step=$((current_step + 1))
    show_step $current_step $total_steps "Building SedaiAudioFoundation"
    build_project_step || { show_summary false; exit 1; }
fi

show_summary true
exit 0
