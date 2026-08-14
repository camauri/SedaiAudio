#!/usr/bin/env bash
#
# Installs the Free Pascal Compiler on Linux/macOS.
#
# Linux counterpart of scripts/windows/install-fpc.ps1. The Windows script
# downloads a pre-packaged FPC into the project-local fpc/ folder because
# Windows has no package manager; here the distribution package is both the
# normal and the supported way, so this script drives the system package
# manager instead (apt / dnf / yum / pacman / zypper / apk / brew).
#
# Exit codes (same convention as install-fpc.ps1):
#     0 = Success
#     1 = Installation error (package manager failed / no network)
#     2 = No supported package manager found
#     5 = Already installed (skipped)
#
# Options:
#     --force      Reinstall even if FPC is already present
#     --dry-run    Only print the command that would be run
#     --quiet      Minimal output (for use from other scripts)
#     --help       Show this help
#

set -u

EXIT_SUCCESS=0
EXIT_INSTALL_ERROR=1
EXIT_NO_PKG_MANAGER=2
EXIT_ALREADY_INSTALLED=5

FORCE=false
DRY_RUN=false
QUIET=false

if [[ -t 1 ]]; then
    RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'
    CYAN='\033[0;36m'; GRAY='\033[0;90m'; NC='\033[0m'
else
    RED=''; GREEN=''; YELLOW=''; CYAN=''; GRAY=''; NC=''
fi

say()      { [[ "$QUIET" == true ]] || echo -e "$1"; }
say_ok()   { [[ "$QUIET" == true ]] || echo -e "${GREEN}$1${NC}"; }
say_warn() { [[ "$QUIET" == true ]] || echo -e "${YELLOW}$1${NC}"; }
say_err()  { echo -e "${RED}ERROR: $1${NC}" >&2; }

show_help() {
    sed -n '3,25p' "$0" | sed 's/^# \{0,1\}//'
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --force)   FORCE=true; shift ;;
        --dry-run) DRY_RUN=true; shift ;;
        --quiet)   QUIET=true; shift ;;
        --help|-h) show_help; exit 0 ;;
        *) say_err "Unknown option: $1"; exit $EXIT_INSTALL_ERROR ;;
    esac
done

# ----------------------------------------------------------------------------
# Already installed?
# ----------------------------------------------------------------------------
say "${CYAN}============================================${NC}"
say "${CYAN}  Free Pascal Compiler Installer${NC}"
say "${CYAN}  Target: $(uname -m)-$(uname -s | tr '[:upper:]' '[:lower:]')${NC}"
say "${CYAN}============================================${NC}"

if command -v fpc >/dev/null 2>&1; then
    FPC_VERSION="$(fpc -iV 2>/dev/null || echo unknown)"
    if [[ "$FORCE" == false ]]; then
        say_ok "FPC $FPC_VERSION already installed at: $(command -v fpc)"
        exit $EXIT_ALREADY_INSTALLED
    fi
    say_warn "FPC $FPC_VERSION found. --force specified, reinstalling."
fi

# ----------------------------------------------------------------------------
# Pick a package manager
# ----------------------------------------------------------------------------
SUDO=""
if [[ "$(id -u)" -ne 0 ]] && command -v sudo >/dev/null 2>&1; then
    SUDO="sudo"
fi

INSTALL_CMD=""
if   command -v apt-get >/dev/null 2>&1; then INSTALL_CMD="$SUDO apt-get install -y fpc"
elif command -v dnf     >/dev/null 2>&1; then INSTALL_CMD="$SUDO dnf install -y fpc"
elif command -v yum     >/dev/null 2>&1; then INSTALL_CMD="$SUDO yum install -y fpc"
elif command -v pacman  >/dev/null 2>&1; then INSTALL_CMD="$SUDO pacman -S --needed --noconfirm fpc"
elif command -v zypper  >/dev/null 2>&1; then INSTALL_CMD="$SUDO zypper install -y fpc"
elif command -v apk     >/dev/null 2>&1; then INSTALL_CMD="$SUDO apk add fpc"
elif command -v brew    >/dev/null 2>&1; then INSTALL_CMD="brew install fpc"
else
    say_err "No supported package manager found (apt/dnf/yum/pacman/zypper/apk/brew)."
    echo ""
    echo "Install FPC manually, then re-run ./setup.sh --skip-fpc:"
    echo "  - Official releases: https://www.freepascal.org/download.html"
    echo "  - fpcupdeluxe (side-by-side installs): https://github.com/LongDirtyAnimAlf/fpcupdeluxe"
    echo "  - Or point the build at an existing compiler: ./build.sh --fpc-path <path/to/fpc>"
    exit $EXIT_NO_PKG_MANAGER
fi

say ""
say "${GRAY}Command: $INSTALL_CMD${NC}"

if [[ "$DRY_RUN" == true ]]; then
    say_warn "--dry-run: nothing was installed."
    exit $EXIT_SUCCESS
fi

# apt needs an index refresh often enough to be worth doing up front
if [[ "$INSTALL_CMD" == *apt-get* ]]; then
    say "${CYAN}Updating package index...${NC}"
    $SUDO apt-get update -qq || say_warn "apt-get update failed - continuing anyway"
fi

say "${CYAN}Installing FPC...${NC}"
if ! $INSTALL_CMD; then
    say_err "Package manager failed to install FPC."
    exit $EXIT_INSTALL_ERROR
fi

# ----------------------------------------------------------------------------
# Verify
# ----------------------------------------------------------------------------
if ! command -v fpc >/dev/null 2>&1; then
    say_err "FPC installed but 'fpc' is not on PATH."
    exit $EXIT_INSTALL_ERROR
fi

FPC_VERSION="$(fpc -iV 2>/dev/null || echo unknown)"
say ""
say "${GREEN}============================================${NC}"
say_ok "  FPC $FPC_VERSION installed successfully!"
say "${GREEN}  Compiler: $(command -v fpc)${NC}"
say "${GREEN}============================================${NC}"

exit $EXIT_SUCCESS
