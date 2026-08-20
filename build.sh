#!/usr/bin/env bash
#
# SedaiAudio Foundation - Build Script for Linux/macOS
# Copyright (c) 2025 Maurizio Cammalleri
# Released under the GNU GPL v3
#
# Functional port of build.ps1: same targets, same target "kinds"
# (tool/demo/test), same compiler flags, same layout
# (bin/<cpu>-<os>/, lib/<cpu>-<os>/) and the same exit code (# of failures).
#
# Option mapping build.ps1 -> build.sh
#   -LibOnly        --lib-only          -TestOnly    --test-only
#   -Tests          --tests             -Demos       --demos
#   -SkipDemos      --skip-demos        -Clean       --clean
#   -CleanOnly      --clean-only        -Debug       --debug
#   -NoBanner       --no-banner         -FpcPath     --fpc-path <path>
#   -Target         --target <name>     -Define      --define <SYM>
#   -Source         --source <file>     -Dest        --dest <file>
#   -CPU            --cpu <cpu>         -OS          --os <os>
#   -AvxAll/-AvxCp/-AvxOp/-AvxCf        --avx-all/--avx-cp/--avx-op/--avx-cf
#

set -u

# ============================================================================
# Configuration
# ============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR"
SRC_DIR="$PROJECT_ROOT/src"
TEST_DIR="$PROJECT_ROOT/test"
LIB_DIR="$PROJECT_ROOT/lib"
BIN_DIR="$PROJECT_ROOT/bin"
DEPS_DIR="$PROJECT_ROOT/deps"
SDL2_DIR="$DEPS_DIR/sdl2"

# Defaults
FPC_PATH=""
SELECT_FPC=false
TARGET=""
SOURCE=""
DEST=""
LIB_ONLY=false
TEST_ONLY=false
WITH_TESTS=false
FORCE_DEMOS=false
SKIP_DEMOS=false
CLEAN_BUILD=false
CLEAN_ONLY=false
DEBUG_BUILD=false
NO_BANNER=false
AVX_CP=false
AVX_OP=false
AVX_CF=false
DEFINES=()
ARG_COUNT=$#

# Colors (disabled when stdout is not a terminal)
if [[ -t 1 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
    CYAN='\033[0;36m'; WHITE='\033[1;37m'; GRAY='\033[0;90m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; CYAN=''; WHITE=''; GRAY=''; NC=''
fi

# ============================================================================
# Host platform detection (defaults for --cpu / --os)
# ============================================================================
detect_cpu() {
    case "$(uname -m)" in
        x86_64|amd64)   echo "x86_64" ;;
        aarch64|arm64)  echo "aarch64" ;;
        i?86)           echo "i386" ;;
        *)              echo "x86_64" ;;
    esac
}

detect_os() {
    case "$(uname -s)" in
        Linux)   echo "linux" ;;
        Darwin)  echo "darwin" ;;
        MINGW*|MSYS*|CYGWIN*) echo "win64" ;;
        *)       echo "linux" ;;
    esac
}

CPU="$(detect_cpu)"
OS="$(detect_os)"

# ============================================================================
# ASCII Art Banner
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
    echo -e "${YELLOW}                   BUILD SCRIPT${NC}"
    echo ""
    echo -e "${CYAN}${border}${NC}"
    echo ""
    echo -e "${GRAY}  Audio Foundation - Professional Audio Synthesis Library${NC}"
    echo -e "${GRAY}  Copyright (c) 2025 Maurizio Cammalleri - GPL-3.0${NC}"
    echo ""
    echo -e "${CYAN}${border}${NC}"
    echo ""
}

# ============================================================================
# Help
# ============================================================================
show_help() {
    show_banner
    echo -e "${CYAN}SedaiAudio Foundation Build Script${NC}"
    echo ""
    echo -e "${YELLOW}USAGE:${NC}"
    echo "    ./build.sh [options]"
    echo ""
    echo -e "${YELLOW}OPTIONS:${NC}"
    echo "    --help            Show this help message"
    echo "    (no options)      Build tools + ask whether to build the demos"
    echo "    --lib-only        Build only the library units"
    echo "    --test-only       Build only the QA test suite"
    echo "    --tests           Also build the QA test suite (alongside tools)"
    echo "    --demos           Build the demos (no prompt)"
    echo "    --skip-demos      Do not build the demos (no prompt)"
    echo "    --target <name>   Build one specific target (tools/demos/tests)"
    echo "    --clean           Clean build artifacts before building"
    echo "    --clean-only      Only clean, do not build"
    echo "    --debug           Build with debug info instead of release"
    echo "    --no-banner       Suppress the ASCII art banner"
    echo "    --fpc-path <path> Path to a specific FPC compiler (one-off, not stored)"
    echo "    --select-fpc      List every FPC found and choose one (stored in setup.config.json)"
    echo "    --define <SYM>    Extra conditional define (repeatable)"
    echo "    --cpu <cpu>       Target CPU: x86_64, i386, aarch64 (default: $CPU)"
    echo "    --os <os>         Target OS: linux, darwin, win64, win32 (default: $OS)"
    echo ""
    echo -e "${YELLOW}AVX2 OPTIONS (use with caution - may cause issues in audio callbacks):${NC}"
    echo "    --avx-all         Enable all AVX2 optimizations"
    echo "    --avx-cp          Enable -CpCOREAVX2 (target CPU)"
    echo "    --avx-op          Enable -OpCOREAVX2 (optimize for CPU)"
    echo "    --avx-cf          Enable -CfAVX2 (AVX2 FPU instructions)"
    echo ""
    echo -e "${YELLOW}CUSTOM BUILD:${NC}"
    echo "    --source <file>   Custom source file path"
    echo "    --dest <file>     Custom destination executable path"
    echo ""
    echo -e "${YELLOW}DIRECTORY STRUCTURE:${NC}"
    echo "    src/              Library source files"
    echo "    test/             Test and demo programs"
    echo "    deps/sdl2/        SDL2 Pascal bindings"
    echo "    bin/<platform>/   Output executables"
    echo "    lib/<platform>/   Compiled units"
    echo ""
    echo -e "${YELLOW}AVAILABLE TARGETS:${NC}"
    echo "    sng_player        GoatTracker .sng player            [tool]"
    echo "    sng_dump          SNG register dump tool             [tool]"
    echo "    saf_play          Instrument library quick-start     [demo]"
    echo "    demo_synth        Synthesis demo                     [demo]"
    echo "    test_saf_main     Main SAF API test                  [test]"
    echo "    audiotest         Audio backend test                 [test]"
    echo "    sedaisid_test     SedaiSIDEvo verification test      [test]"
    echo "    saf_regression    Headless render-path regression    [test]"
    echo "    patch_bas         .patch <-> SedaiBasic MODERN bridge [tool]"
    echo ""
    echo -e "${YELLOW}EXAMPLES:${NC}"
    echo "    ./build.sh                              # Build tools (+ demo prompt)"
    echo "    ./build.sh --clean --tests              # Clean, build tools + tests"
    echo "    ./build.sh --target sng_player          # Build one target"
    echo "    ./build.sh --debug                      # Build with debug symbols"
    echo "    ./build.sh --source tmp/x.lpr --dest bin/x86_64-linux/x"
    echo ""
    echo -e "${YELLOW}NOTES:${NC}"
    echo "    - Run ./setup.sh first to install dependencies"
    echo "    - Executables are output to bin/<platform>/"
    echo "    - Compiled units are output to lib/<platform>/"
    echo "    - AVX2 is disabled by default (causes crashes in SDL2 callbacks)"
    echo ""
}

# ============================================================================
# Parse Arguments
# ============================================================================
while [[ $# -gt 0 ]]; do
    case "$1" in
        --help|-h)       show_help; exit 0 ;;
        --lib-only)      LIB_ONLY=true; shift ;;
        --test-only)     TEST_ONLY=true; shift ;;
        --tests)         WITH_TESTS=true; shift ;;
        --demos)         FORCE_DEMOS=true; shift ;;
        --skip-demos)    SKIP_DEMOS=true; shift ;;
        --clean)         CLEAN_BUILD=true; shift ;;
        --clean-only)    CLEAN_ONLY=true; shift ;;
        --debug)         DEBUG_BUILD=true; shift ;;
        --no-banner)     NO_BANNER=true; shift ;;
        --avx-all)       AVX_CP=true; AVX_OP=true; AVX_CF=true; shift ;;
        --avx-cp)        AVX_CP=true; shift ;;
        --avx-op)        AVX_OP=true; shift ;;
        --avx-cf)        AVX_CF=true; shift ;;
        --fpc-path)      FPC_PATH="${2:-}"; shift 2 ;;
        --select-fpc)    SELECT_FPC=true; shift ;;
        --target)        TARGET="${2:-}"; shift 2 ;;
        --define)        DEFINES+=("${2:-}"); shift 2 ;;
        --source)        SOURCE="${2:-}"; shift 2 ;;
        --dest)          DEST="${2:-}"; shift 2 ;;
        --cpu)           CPU="${2:-}"; shift 2 ;;
        --os)            OS="${2:-}"; shift 2 ;;
        *)
            echo -e "${RED}ERROR: Unknown option: $1${NC}"
            echo "Run './build.sh --help' for usage."
            exit 1
            ;;
    esac
done

case "$CPU" in
    x86_64|i386|aarch64) ;;
    *) echo -e "${RED}ERROR: Invalid --cpu '$CPU' (x86_64, i386, aarch64)${NC}"; exit 1 ;;
esac
case "$OS" in
    linux|darwin|win64|win32) ;;
    *) echo -e "${RED}ERROR: Invalid --os '$OS' (linux, darwin, win64, win32)${NC}"; exit 1 ;;
esac

PLATFORM_DIR="$CPU-$OS"
# Executable suffix: .exe only for Windows targets
EXE_EXT=""
[[ "$OS" == win* ]] && EXE_EXT=".exe"

# ============================================================================
# Find FPC Compiler
# ============================================================================
# ----------------------------------------------------------------------------
# Reading and writing setup.config.json, so the compiler is chosen once and not
# at every build. Same file and same keys as build.ps1 and as SedaiBasic2, so a
# shared checkout keeps working across the two scripts and the two platforms.
# ----------------------------------------------------------------------------
config_value() {
    local key="$1" file="$SCRIPT_DIR/setup.config.json"
    [[ -f "$file" ]] || return 1
    sed -n "s/.*\"$key\"[[:space:]]*:[[:space:]]*\"\([^\"]*\)\".*/\1/p" "$file" | head -1
}

config_set() {
    local key="$1" val="$2" file="$SCRIPT_DIR/setup.config.json" esc tmp
    esc="$(printf '%s' "$val" | sed 's/[\\"]/\\&/g')"
    if [[ ! -f "$file" ]]; then
        printf '{\n  "%s": "%s"\n}\n' "$key" "$esc" > "$file"
        return 0
    fi
    tmp="$file.tmp.$$"
    if grep -q "\"$key\"[[:space:]]*:" "$file"; then
        sed "s|\"$key\"[[:space:]]*:[[:space:]]*\"[^\"]*\"|\"$key\": \"$esc\"|" "$file" > "$tmp"
    else
        # Insert as the first member, so a file with or without a trailing
        # comma both stay valid.
        sed "0,/{/s|{|{\n  \"$key\": \"$esc\",|" "$file" > "$tmp"
    fi
    mv "$tmp" "$file"
}

# Every compiler on this machine, most-likely-meant first, real paths, no
# duplicates. The deep scan is deliberately last: it finds installs in odd
# places but says nothing about which one is intended.
fpc_candidates() {
    local platform="$1" c d
    {
        printf '%s\n' "$PROJECT_ROOT/fpc/bin/$platform/fpc"
        printf '%s\n' "$PROJECT_ROOT/fpc/bin/fpc"
        printf '%s\n' "$PROJECT_ROOT/fpc/3.2.2/bin/$platform/fpc"
        for d in "$HOME"/tools/fp/*/fpc "$HOME"/fpcupdeluxe/fpc "$HOME"/fpc "$HOME"/tools/fpc; do
            printf '%s\n' "$d/bin/$platform/fpc"
            printf '%s\n' "$d/bin/fpc"
        done
        printf '%s\n' "/usr/local/bin/fpc"
        printf '%s\n' "/opt/fpc/bin/fpc"
        command -v fpc 2>/dev/null || true
        find "$HOME" -maxdepth 6 -type f -name fpc -perm -u+x 2>/dev/null || true
    } | while read -r c; do
        [[ -n "$c" && -x "$c" ]] || continue
        readlink -f "$c" 2>/dev/null || printf '%s\n' "$c"
    done | awk '!seen[$0]++'
}

# Does this compiler actually COMPILE? Not "does the binary run" — fpc -iV
# answers that happily on an install whose RTL it cannot find, which is exactly
# how a half-finished tree ends up shadowing a working one and breaking the
# build with "Can't find unit system". The only honest test is a build, done
# the way build.sh builds: no explicit config file, because that is what the
# real invocation does.
fpc_works() {
    local fpc="$1" d rc
    d="$(mktemp -d)" || return 1
    printf 'begin end.\n' > "$d/probe.pas"
    ( cd "$d" && "$fpc" -o"$d/probe" "$d/probe.pas" ) >/dev/null 2>&1
    rc=$?
    rm -rf "$d"
    return $rc
}

# .../fpc/bin/<platform>/fpc  ->  .../fpc   (the root form build.ps1 stores as
# FpcPath). A system install has no such root and prints nothing.
fpc_root_of() {
    local bin="$1" platform="$2"
    case "$bin" in
        */bin/"$platform"/fpc) printf '%s\n' "${bin%/bin/$platform/fpc}" ;;
        *) : ;;
    esac
}

# List what is installed, prove which ones work, and ask — once.
choose_fpc() {
    local platform="$1" c ver ok n=0 sel root i
    local -a paths=() vers=() good=()

    while read -r c; do
        [[ -n "$c" ]] || continue
        ver="$("$c" -iV 2>/dev/null)"
        [[ -n "$ver" ]] || continue
        if fpc_works "$c"; then ok=yes; else ok=no; fi
        paths+=("$c"); vers+=("$ver"); good+=("$ok")
    done < <(fpc_candidates "$platform")

    n=${#paths[@]}
    if [[ $n -eq 0 ]]; then
        return 1
    fi

    # One working compiler and nothing else to weigh: take it and say so,
    # rather than asking a question with a single answer.
    local -a usable=()
    for ((i=0; i<n; i++)); do [[ "${good[$i]}" == yes ]] && usable+=("$i"); done
    if [[ ${#usable[@]} -eq 0 ]]; then
        echo -e "${RED}ERROR: a Free Pascal Compiler was found, but none can compile.${NC}" >&2
        for ((i=0; i<n; i++)); do
            printf "  FPC %-8s %s\n" "${vers[$i]}" "${paths[$i]}" >&2
        done
        echo -e "${YELLOW}An install without a usable fpc.cfg is the usual cause.${NC}" >&2
        return 1
    fi
    if [[ ${#usable[@]} -eq 1 && $n -eq 1 ]]; then
        printf '%s\n' "${paths[${usable[0]}]}"
        return 0
    fi

    echo "" >&2
    echo -e "${CYAN}Free Pascal compilers found on this machine:${NC}" >&2
    for ((i=0; i<n; i++)); do
        if [[ "${good[$i]}" == yes ]]; then
            printf "  %d) FPC %-8s %s\n" "$((i+1))" "${vers[$i]}" "${paths[$i]}" >&2
        else
            printf "  %d) FPC %-8s %s   ${YELLOW}[cannot compile - skipped]${NC}\n" \
                   "$((i+1))" "${vers[$i]}" "${paths[$i]}" >&2
        fi
    done
    echo "" >&2

    # No terminal means no question: a script or a CI run must fail loudly
    # rather than hang on a prompt, or pick for the user and be wrong quietly.
    if [[ ! -t 0 ]]; then
        echo -e "${YELLOW}Not a terminal, so nothing was chosen and nothing was stored.${NC}" >&2
        echo -e "${YELLOW}Run ./build.sh --select-fpc once interactively, or pass --fpc-path.${NC}" >&2
        return 1
    fi

    local default=$((usable[0]+1))
    while :; do
        read -r -p "Which one should this project use? [$default] " sel >&2 || return 1
        [[ -z "$sel" ]] && sel=$default
        [[ "$sel" =~ ^[0-9]+$ ]] || { echo "  a number, please" >&2; continue; }
        (( sel >= 1 && sel <= n )) || { echo "  out of range" >&2; continue; }
        [[ "${good[$((sel-1))]}" == yes ]] || { echo "  that one cannot compile; pick another" >&2; continue; }
        break
    done

    c="${paths[$((sel-1))]}"
    config_set FpcBin "$c"
    root="$(fpc_root_of "$c" "$platform")"
    [[ -n "$root" ]] && config_set FpcPath "$root"
    echo -e "${GREEN}Stored in setup.config.json: FPC ${vers[$((sel-1))]} - $c${NC}" >&2
    echo -e "${GRAY}Change it later with ./build.sh --select-fpc${NC}" >&2
    printf '%s\n' "$c"
}

find_fpc() {
    local platform="$PLATFORM_DIR" candidate

    # 1. Explicit override — deliberately NOT stored: it is a one-off, and
    #    writing it would turn "just this once" into the project's setting.
    if [[ -n "$FPC_PATH" ]]; then
        if [[ -x "$FPC_PATH" ]]; then
            echo "$FPC_PATH"; return 0
        fi
        echo -e "${YELLOW}WARNING: Specified FPC path not found: $FPC_PATH${NC}" >&2
    fi
    if [[ -n "${FPC:-}" && -x "${FPC:-}" ]]; then
        echo "$FPC"; return 0
    fi

    # 2. The stored choice.
    if [[ "$SELECT_FPC" != "true" ]]; then
        candidate="$(config_value FpcBin 2>/dev/null || true)"
        if [[ -n "$candidate" && -x "$candidate" ]]; then
            echo "$candidate"; return 0
        fi
        # The root form, which is what build.ps1 writes.
        candidate="$(config_value FpcPath 2>/dev/null || true)"
        if [[ -n "$candidate" && -x "$candidate/bin/$platform/fpc" ]]; then
            echo "$candidate/bin/$platform/fpc"; return 0
        fi
    fi

    # 3. Nothing stored, or --select-fpc: look at everything and ask, once.
    choose_fpc "$platform"
}

# ============================================================================
# Verify Directory Structure
# ============================================================================
check_directory_structure() {
    local errors=()

    [[ -d "$SRC_DIR" ]]  || errors+=("src")
    [[ -d "$SDL2_DIR" ]] || errors+=("deps/sdl2")
    [[ -f "$SDL2_DIR/sdl2.pas" ]] || errors+=("deps/sdl2/sdl2.pas (SDL2 Pascal bindings)")

    # Auto-create the non-critical ones
    local d
    for d in "$TEST_DIR" "$BIN_DIR/$PLATFORM_DIR" "$LIB_DIR/$PLATFORM_DIR"; do
        if [[ ! -d "$d" ]]; then
            mkdir -p "$d"
            echo -e "  ${YELLOW}Created: ${d#$PROJECT_ROOT/}${NC}"
        fi
    done

    if [[ ${#errors[@]} -gt 0 ]]; then
        echo ""
        echo -e "${RED}ERROR: Required directories/files not found:${NC}"
        local e
        for e in "${errors[@]}"; do
            echo -e "${RED}  - $e${NC}"
        done
        echo ""
        echo -e "${YELLOW}Please run ./setup.sh first to initialize the project.${NC}"
        return 1
    fi
    return 0
}

# ============================================================================
# Build a Single Target
#   build_target <source_file> <source_dir> <output_path>
# ============================================================================
build_target() {
    local source_file="$1" source_dir="$2" output_path="$3"
    local src_path="$source_dir/$source_file"

    if [[ ! -f "$src_path" ]]; then
        echo -e "  ${RED}ERROR: Source file not found: $src_path${NC}"
        return 1
    fi

    local lib_path="$LIB_DIR/$PLATFORM_DIR"
    local output_dir
    output_dir="$(dirname "$output_path")"
    mkdir -p "$lib_path" "$output_dir"

    local opts=()

    # Output name
    opts+=("-o$output_path")

    # Target platform
    opts+=("-P$CPU" "-T$OS")

    # Mode
    opts+=('-MObjFPC' '-Sh')

    if [[ "$DEBUG_BUILD" == false ]]; then
        # Release optimizations
        opts+=('-O2')

        # AVX2 optimizations (optional - disabled by default)
        # WARNING: AVX2 causes crashes in SDL2 audio callbacks on Win64 when
        # complex floating-point expressions are used in cdecl callbacks.
        [[ "$AVX_CP" == true ]] && opts+=('-CpCOREAVX2')
        [[ "$AVX_OP" == true ]] && opts+=('-OpCOREAVX2')
        [[ "$AVX_CF" == true ]] && opts+=('-CfAVX2')

        # Additional optimizations (safe)
        opts+=('-OoREGVAR' '-OoCSE' '-OoDFA' '-OoFASTMATH' '-OoCONSTPROP')

        # Strip and smart linking
        opts+=('-Xs' '-XX')
    else
        # Debug options
        opts+=('-g' '-gl' '-gw' '-Ci' '-Cr' '-Co' '-dDEBUG')
    fi

    # Unit search paths - src/ and every subdirectory used by the project
    opts+=("-Fu$SRC_DIR")
    local sub
    for sub in Core Generators Modulators Processors Effects Voice Mixer \
               Transport Project Platform SID Players FileIO Engine Wavetable Patch; do
        [[ -d "$SRC_DIR/$sub" ]] && opts+=("-Fu$SRC_DIR/$sub")
    done
    opts+=("-Fu$TEST_DIR")
    opts+=("-Fu$SDL2_DIR")       # SDL2 Pascal bindings
    opts+=("-FU$lib_path")

    # Custom defines
    local def
    for def in ${DEFINES[@]+"${DEFINES[@]}"}; do
        opts+=("-d$def")
    done

    if [[ ${#DEFINES[@]} -gt 0 ]]; then
        local joined
        joined="$(printf '%s, ' "${DEFINES[@]}")"
        echo -e "  ${CYAN}Defines: ${joined%, }${NC}"
    fi

    local output_name
    output_name="$(basename "$output_path")"
    echo -ne "  ${WHITE}Building $output_name...${NC}"

    # Show AVX2 status if any enabled
    local avx_flags=()
    [[ "$AVX_CP" == true ]] && avx_flags+=('CpCOREAVX2')
    [[ "$AVX_OP" == true ]] && avx_flags+=('OpCOREAVX2')
    [[ "$AVX_CF" == true ]] && avx_flags+=('CfAVX2')
    if [[ ${#avx_flags[@]} -gt 0 ]]; then
        local avx_joined
        avx_joined="$(printf '%s, ' "${avx_flags[@]}")"
        echo -ne " ${YELLOW}[AVX2: ${avx_joined%, }]${NC}"
    fi

    local build_log
    build_log="$(cd "$PROJECT_ROOT" && "$FPC" "${opts[@]}" "$src_path" 2>&1)"
    local rc=$?

    if [[ $rc -eq 0 ]]; then
        echo -e " ${GREEN}OK${NC}"
        return 0
    fi

    echo -e " ${RED}FAILED${NC}"
    echo "$build_log" | sed "s/^/    /"
    return 1
}

# ============================================================================
# Clean Build Artifacts
# ============================================================================
clean_build() {
    echo -e "${YELLOW}Cleaning build artifacts...${NC}"

    # Clean ALL platform directories in lib/ (not just the current platform)
    if [[ -d "$LIB_DIR" ]]; then
        local sub
        for sub in "$LIB_DIR"/*/; do
            [[ -d "$sub" ]] || continue
            if find "$sub" -maxdepth 1 -type f \
                 \( -name '*.ppu' -o -name '*.o' -o -name '*.a' -o -name '*.rsj' \) \
                 -print -quit | grep -q .; then
                find "$sub" -maxdepth 1 -type f \
                     \( -name '*.ppu' -o -name '*.o' -o -name '*.a' -o -name '*.rsj' \) -delete
                echo -e "  ${GRAY}Cleaned: ${sub%/}${NC}"
            fi
        done
    fi

    # Clean ALL platform directories in bin/ (intermediate files only, keep executables)
    if [[ -d "$BIN_DIR" ]]; then
        local sub
        for sub in "$BIN_DIR"/*/; do
            [[ -d "$sub" ]] || continue
            if find "$sub" -maxdepth 1 -type f \
                 \( -name '*.ppu' -o -name '*.o' -o -name '*.rsj' \) \
                 -print -quit | grep -q .; then
                find "$sub" -maxdepth 1 -type f \
                     \( -name '*.ppu' -o -name '*.o' -o -name '*.rsj' \) -delete
                echo -e "  ${GRAY}Cleaned: ${sub%/} (units only)${NC}"
            fi
        done
    fi

    # Clean src/ and test/ intermediate files
    local d
    for d in "$SRC_DIR" "$TEST_DIR"; do
        [[ -d "$d" ]] || continue
        find "$d" -maxdepth 1 -type f \
             \( -name '*.ppu' -o -name '*.o' -o -name '*.a' -o -name '*.rsj' \
                -o -name '*.compiled' -o -name 'link.res' -o -name 'ppas.sh' \
                -o -name 'ppas.bat' \) -delete 2>/dev/null
        echo -e "  ${GRAY}Cleaned: $d (intermediate files)${NC}"
    done

    echo -e "${GREEN}Clean complete.${NC}"
}

# ============================================================================
# Build Targets
#   name|source|source_dir|output|kind
# Kind: 'tool' = user-facing app (built by default), 'demo' = example (built by
# default only after the interactive prompt / --demos), 'test' = QA suite (built
# only with --tests / --test-only).
# ============================================================================
ALL_TARGETS=(
    "sng_player|sng_player.lpr|$TEST_DIR|sng_player|tool"
    "sng_dump|sng_dump.lpr|$TEST_DIR|sng_dump|tool"
    "saf_play|saf_play.lpr|$TEST_DIR|saf_play|demo"
    "demo_synth|demo_synth.lpr|$TEST_DIR|demo_synth|demo"
    "test_saf_main|TestSAFMain.lpr|$TEST_DIR|TestSAFMain|test"
    "audiotest|audiotest.lpr|$TEST_DIR|audiotest|test"
    "sedaisid_test|sedaisid_test.lpr|$TEST_DIR|sedaisid_test|test"
    "saf_regression|saf_regression.lpr|$TEST_DIR|saf_regression|test"
    "patch_bas|patch_bas.lpr|$TEST_DIR|patch_bas|tool"
)

target_field() {   # target_field <name> <index 2..5>
    local name="$1" idx="$2" entry
    for entry in "${ALL_TARGETS[@]}"; do
        if [[ "${entry%%|*}" == "$name" ]]; then
            echo "$entry" | cut -d'|' -f"$idx"
            return 0
        fi
    done
    return 1
}

list_targets() {
    local entry
    for entry in "${ALL_TARGETS[@]}"; do
        echo "  - ${entry%%|*}"
    done | sort
}

# ============================================================================
# Main
# ============================================================================
[[ "$NO_BANNER" == false ]] && show_banner

FPC="$(find_fpc)" || FPC=""
if [[ -z "$FPC" ]]; then
    echo -e "${RED}ERROR: no usable Free Pascal Compiler.${NC}"
    echo ""
    echo -e "${YELLOW}Anything listed above as [cannot compile] was found but could not build a${NC}"
    echo -e "${YELLOW}two-word program — usually an install whose fpc.cfg is missing, so it does${NC}"
    echo -e "${YELLOW}not know where its own RTL is. Those are skipped rather than used.${NC}"
    echo ""
    echo -e "${YELLOW}Searched:${NC}"
    echo -e "${GRAY}  1. --fpc-path, then the FPC environment variable (one-off, never stored)${NC}"
    echo -e "${GRAY}  2. FpcBin / FpcPath in setup.config.json (the stored choice)${NC}"
    echo -e "${GRAY}  3. ./fpc/, ~/tools/fp/*/fpc, ~/fpcupdeluxe/fpc, ~/fpc, /usr/local, /opt${NC}"
    echo -e "${GRAY}  4. System PATH, then a scan of \$HOME${NC}"
    echo ""
    echo -e "${YELLOW}Install it with your package manager, e.g.:${NC}"
    echo "  Debian/Ubuntu: sudo apt install fpc"
    echo "  Fedora:        sudo dnf install fpc"
    echo "  Arch:          sudo pacman -S fpc"
    echo "  macOS:         brew install fpc"
    echo ""
    echo -e "${YELLOW}Or run ./build.sh --select-fpc to choose one, ./setup.sh --install-fpc,${NC}"
    echo -e "${YELLOW}or pass --fpc-path <path> for a single build.${NC}"
    exit 1
fi

FPC_VERSION="$("$FPC" -iV 2>/dev/null || echo 'unknown')"
echo -e "${GRAY}Compiler: FPC $FPC_VERSION${NC}"
echo -e "${GRAY}Path: $FPC${NC}"
echo -e "${GRAY}Platform: $PLATFORM_DIR${NC}"
echo -e "${GRAY}Mode: $(if [[ "$DEBUG_BUILD" == true ]]; then echo Debug; else echo Release; fi)${NC}"

if [[ "$AVX_CP" == true || "$AVX_OP" == true || "$AVX_CF" == true ]]; then
    echo -e "${YELLOW}AVX2: ENABLED (use with caution)${NC}"
else
    echo -e "${GRAY}AVX2: disabled (safe mode)${NC}"
fi
echo ""

echo -e "${GRAY}Checking directory structure...${NC}"
check_directory_structure || exit 1
echo -e "${GREEN}Directory structure OK${NC}"
echo ""

if [[ "$CLEAN_BUILD" == true || "$CLEAN_ONLY" == true ]]; then
    clean_build
    echo ""
    if [[ "$CLEAN_ONLY" == true ]]; then
        echo -e "${GREEN}Clean completed.${NC}"
        exit 0
    fi
fi

success=0
failed=0

if [[ -n "$SOURCE" ]]; then
    # ------------------------------------------------------------------ custom
    echo -e "${CYAN}Custom Build${NC}"
    echo -e "${CYAN}============${NC}"

    source_path="$SOURCE"
    [[ "$source_path" != /* ]] && source_path="$PROJECT_ROOT/$SOURCE"

    if [[ ! -f "$source_path" ]]; then
        echo -e "${RED}ERROR: Source file not found: $source_path${NC}"
        exit 1
    fi

    dest_path="$DEST"
    if [[ -z "$dest_path" ]]; then
        base_name="$(basename "$SOURCE")"
        base_name="${base_name%.*}"
        dest_path="$BIN_DIR/$PLATFORM_DIR/$base_name$EXE_EXT"
    elif [[ "$dest_path" != /* ]]; then
        dest_path="$PROJECT_ROOT/$dest_path"
    fi

    if build_target "$(basename "$source_path")" "$(dirname "$source_path")" "$dest_path"; then
        success=$((success + 1))
    else
        failed=$((failed + 1))
    fi
    echo ""

elif [[ -n "$TARGET" ]]; then
    # ------------------------------------------------------------ single target
    normalized="${TARGET%.exe}"; normalized="${normalized%.pas}"; normalized="${normalized%.lpr}"

    if ! target_field "$normalized" 2 >/dev/null; then
        echo -e "${RED}ERROR: Unknown target '$TARGET'${NC}"
        echo ""
        echo -e "${YELLOW}Available targets:${NC}"
        list_targets
        echo ""
        echo -e "${GRAY}Or use --source and --dest for custom builds.${NC}"
        exit 1
    fi

    echo -e "${CYAN}Building Target: $normalized${NC}"
    echo -e "${CYAN}================================${NC}"

    t_source="$(target_field "$normalized" 2)"
    t_dir="$(target_field "$normalized" 3)"
    t_out="$(target_field "$normalized" 4)"

    if build_target "$t_source" "$t_dir" "$BIN_DIR/$PLATFORM_DIR/$t_out$EXE_EXT"; then
        success=$((success + 1))
    else
        failed=$((failed + 1))
    fi
    echo ""

elif [[ "$LIB_ONLY" == true ]]; then
    # ---------------------------------------------------------------- lib only
    # Compile the library units by building audiotest (which pulls them all in)
    echo -e "${CYAN}Building Library...${NC}"
    echo -e "${CYAN}===================${NC}"

    t_source="$(target_field audiotest 2)"
    t_dir="$(target_field audiotest 3)"
    t_out="$(target_field audiotest 4)"

    if build_target "$t_source" "$t_dir" "$BIN_DIR/$PLATFORM_DIR/$t_out$EXE_EXT"; then
        success=$((success + 1))
    else
        failed=$((failed + 1))
    fi
    echo ""

else
    # ------------------------------------------------------------- by target kind
    #   tools  -> always (unless --test-only)
    #   tests  -> only with --tests or --test-only
    #   demos  -> --demos forces on, --skip-demos forces off; with NO arguments
    #             at all we ask interactively; with any other argument, off.
    build_tools=true
    [[ "$TEST_ONLY" == true ]] && build_tools=false
    build_tests=false
    { [[ "$WITH_TESTS" == true ]] || [[ "$TEST_ONLY" == true ]]; } && build_tests=true

    if   [[ "$FORCE_DEMOS" == true ]]; then build_demos=true
    elif [[ "$SKIP_DEMOS"  == true ]]; then build_demos=false
    elif [[ "$TEST_ONLY"   == true ]]; then build_demos=false
    elif [[ $ARG_COUNT -eq 0 ]]; then
        # Interactive (bare invocation) only. In a non-interactive shell / CI
        # there is no tty -> default to skipping the demos.
        if [[ -t 0 ]]; then
            read -r -p "Build the demo programs (saf_play, demo_synth)? [y/N] " ans
        else
            ans="n"
        fi
        if [[ "$ans" =~ ^([yY]|[yY][eE][sS])$ ]]; then build_demos=true; else build_demos=false; fi
    else
        build_demos=false
    fi

    if [[ "$TEST_ONLY" == true ]]; then
        echo -e "${CYAN}Building Tests...${NC}"
    else
        echo -e "${CYAN}Building...${NC}"
    fi
    echo -e "${CYAN}===================${NC}"

    while IFS= read -r entry; do
        name="$(echo "$entry" | cut -d'|' -f1)"
        t_source="$(echo "$entry" | cut -d'|' -f2)"
        t_dir="$(echo "$entry" | cut -d'|' -f3)"
        t_out="$(echo "$entry" | cut -d'|' -f4)"
        kind="$(echo "$entry" | cut -d'|' -f5)"

        case "$kind" in
            tool) do_build="$build_tools" ;;
            test) do_build="$build_tests" ;;
            demo) do_build="$build_demos" ;;
            *)    do_build=false ;;
        esac
        [[ "$do_build" == true ]] || continue

        if build_target "$t_source" "$t_dir" "$BIN_DIR/$PLATFORM_DIR/$t_out$EXE_EXT"; then
            success=$((success + 1))
        else
            failed=$((failed + 1))
        fi
    done < <(printf '%s\n' "${ALL_TARGETS[@]}" | sort)
    echo ""
fi

# ============================================================================
# Summary
# ============================================================================
echo -e "${CYAN}============================================${NC}"
echo -e "${CYAN}Build Summary${NC}"
echo -e "${CYAN}============================================${NC}"
echo -e "  ${GREEN}Successful: $success${NC}"
[[ $failed -gt 0 ]] && echo -e "  ${RED}Failed: $failed${NC}"
echo ""

bin_path="$BIN_DIR/$PLATFORM_DIR"
if [[ -d "$bin_path" ]]; then
    exes="$(find "$bin_path" -maxdepth 1 -type f -perm -u+x 2>/dev/null | sort)"
    if [[ -n "$exes" ]]; then
        echo -e "${GRAY}Built executables:${NC}"
        while IFS= read -r exe; do
            echo -e "${GRAY}  $exe${NC}"
        done <<< "$exes"
        echo ""
    fi
fi

if [[ $failed -eq 0 ]]; then
    echo -e "${GREEN}Build completed successfully!${NC}"
else
    echo -e "${YELLOW}Build completed with errors.${NC}"
fi

exit $failed
