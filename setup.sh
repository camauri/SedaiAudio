#!/bin/bash
#
# SedaiAudio Foundation - Setup Script for Linux/macOS
# Copyright (c) 2025 Maurizio Cammalleri
# Released under GNU GPL v3 or Commercial License
#
# Usage:
#   ./setup.sh [options]
#
# Options:
#   --install-fpc     Download and install FPC (optional)
#   --force-sdl2      Force reinstallation of SDL2 bindings
#   --force-runtime   Force reinstallation of SDL2 runtime
#   --skip-sdl2       Skip SDL2 bindings installation
#   --skip-runtime    Skip SDL2 runtime installation
#   --clean           Clean directories before setup
#   --help            Show this help message
#
# Examples:
#   ./setup.sh                    # Full setup (recommended)
#   ./setup.sh --install-fpc      # Setup + install FPC
#   ./setup.sh --clean            # Clean and reinstall everything
#

set -e

# ============================================================================
# Configuration
# ============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$SCRIPT_DIR/src"
BIN_DIR="$SCRIPT_DIR/bin"
LIB_DIR="$SCRIPT_DIR/lib"
DEPS_DIR="$SCRIPT_DIR/deps"
SDL2_DIR="$DEPS_DIR/sdl2"

# Default values
INSTALL_FPC=false
FORCE_SDL2=false
FORCE_RUNTIME=false
SKIP_SDL2=false
SKIP_RUNTIME=false
CLEAN_BUILD=false

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
GRAY='\033[0;90m'
NC='\033[0m'

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

# ============================================================================
# Help
# ============================================================================
show_help() {
    echo "SedaiAudio Foundation - Setup Script for Linux/macOS"
    echo ""
    echo "Usage: ./setup.sh [options]"
    echo ""
    echo "Options:"
    echo "  --install-fpc     Download and install FPC (optional)"
    echo "  --force-sdl2      Force reinstallation of SDL2 bindings"
    echo "  --force-runtime   Force reinstallation of SDL2 runtime"
    echo "  --skip-sdl2       Skip SDL2 bindings installation"
    echo "  --skip-runtime    Skip SDL2 runtime installation"
    echo "  --clean           Clean directories before setup"
    echo "  --help            Show this help message"
    echo ""
    echo "Examples:"
    echo "  ./setup.sh                    # Full setup (recommended)"
    echo "  ./setup.sh --install-fpc      # Setup + install FPC"
    echo "  ./setup.sh --clean            # Clean and reinstall everything"
    echo ""
    echo "Notes:"
    echo "  - SDL2 Pascal bindings are required for compilation"
    echo "  - SDL2 dev libraries are required (install via package manager)"
    echo "  - FPC installation is optional if you already have it"
    echo "  - After setup, run ./build.sh to compile the library"
    echo ""
}

# ============================================================================
# Parse Arguments
# ============================================================================
while [[ $# -gt 0 ]]; do
    case $1 in
        --install-fpc)
            INSTALL_FPC=true
            shift
            ;;
        --force-sdl2)
            FORCE_SDL2=true
            shift
            ;;
        --force-runtime)
            FORCE_RUNTIME=true
            shift
            ;;
        --skip-sdl2)
            SKIP_SDL2=true
            shift
            ;;
        --skip-runtime)
            SKIP_RUNTIME=true
            shift
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --help|-h)
            show_help
            exit 0
            ;;
        *)
            echo -e "${RED}ERROR: Unknown option: $1${NC}"
            show_help
            exit 1
            ;;
    esac
done

# ============================================================================
# Stub Notice
# ============================================================================
echo -e "${YELLOW}"
echo "╔════════════════════════════════════════════════════════════════════╗"
echo "║                                                                    ║"
echo "║  STUB: This setup script is a placeholder for Linux/macOS.        ║"
echo "║                                                                    ║"
echo "║  Full implementation will be completed when testing on Linux.     ║"
echo "║                                                                    ║"
echo "║  For now, manually install the dependencies:                      ║"
echo "║                                                                    ║"
echo "║  1. Install FPC:                                                  ║"
echo "║     Debian/Ubuntu: sudo apt install fpc                           ║"
echo "║     Fedora: sudo dnf install fpc                                  ║"
echo "║     Arch: sudo pacman -S fpc                                      ║"
echo "║     macOS: brew install fpc                                       ║"
echo "║                                                                    ║"
echo "║  2. Install SDL2 development libraries:                           ║"
echo "║     Debian/Ubuntu: sudo apt install libsdl2-dev                   ║"
echo "║     Fedora: sudo dnf install SDL2-devel                           ║"
echo "║     Arch: sudo pacman -S sdl2                                     ║"
echo "║     macOS: brew install sdl2                                      ║"
echo "║                                                                    ║"
echo "║  3. Download SDL2 Pascal bindings manually from:                  ║"
echo "║     https://github.com/camauri/SedaiBasic2-Deps/releases          ║"
echo "║     Extract to: deps/sdl2/                                        ║"
echo "║                                                                    ║"
echo "╚════════════════════════════════════════════════════════════════════╝"
echo -e "${NC}"

show_banner

# Show what would be done
echo -e "${CYAN}Configuration:${NC}"
echo -e "  Script directory: ${GRAY}$SCRIPT_DIR${NC}"
echo -e "  Source directory: ${GRAY}$SRC_DIR${NC}"
echo -e "  Install FPC:      ${GRAY}$INSTALL_FPC${NC}"
echo -e "  Skip SDL2:        ${GRAY}$SKIP_SDL2${NC}"
echo -e "  Skip Runtime:     ${GRAY}$SKIP_RUNTIME${NC}"
echo -e "  Clean:            ${GRAY}$CLEAN_BUILD${NC}"
echo ""

# Check for FPC
echo -e "${CYAN}Checking dependencies:${NC}"
if command -v fpc &> /dev/null; then
    FPC_VERSION=$(fpc -iV 2>/dev/null || echo "unknown")
    echo -e "  FPC: ${GREEN}Found (version $FPC_VERSION)${NC}"
else
    echo -e "  FPC: ${YELLOW}Not found${NC}"
fi

# Check for SDL2
if ldconfig -p 2>/dev/null | grep -q "libSDL2" || pkg-config --exists sdl2 2>/dev/null; then
    echo -e "  SDL2: ${GREEN}Found${NC}"
else
    echo -e "  SDL2: ${YELLOW}Not found (install libsdl2-dev)${NC}"
fi

# Check for SDL2 Pascal bindings
if [[ -f "$SDL2_DIR/sdl2.pas" ]]; then
    echo -e "  SDL2 Pascal bindings: ${GREEN}Found${NC}"
else
    echo -e "  SDL2 Pascal bindings: ${YELLOW}Not found (download from GitHub)${NC}"
fi

echo ""
echo -e "${CYAN}TODO: Implement full setup logic for Linux/macOS${NC}"
echo ""

exit 0
