#!/bin/bash
#
# SedaiAudio Foundation - Build Script for Linux/macOS
# Copyright (c) 2025 Maurizio Cammalleri
# Released under GNU GPL v3 or Commercial License
#
# Usage:
#   ./build.sh [options]
#
# Options:
#   --lib-only        Build only library (no demos)
#   --demo-only       Build only demo programs
#   --clean           Clean build artifacts before building
#   --fpc-path <path> Specify custom FPC compiler path
#   --debug           Build with debug info
#   --help            Show this help message
#
# Examples:
#   ./build.sh                              # Build all
#   ./build.sh --lib-only                   # Build only library
#   ./build.sh --demo-only                  # Build only demos
#   ./build.sh --clean                      # Clean and rebuild
#   ./build.sh --fpc-path /opt/fpc/bin/fpc  # Use specific FPC
#

set -e

# ============================================================================
# Configuration
# ============================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SRC_DIR="$SCRIPT_DIR/src"
LIB_DIR="$SCRIPT_DIR/lib"
BIN_DIR="$SCRIPT_DIR/bin"

# Default values
FPC_PATH=""
LIB_ONLY=false
DEMO_ONLY=false
CLEAN_BUILD=false
DEBUG_BUILD=false

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
    echo -e "${YELLOW}                   BUILD SCRIPT${NC}"
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
    echo "SedaiAudio Foundation - Build Script for Linux/macOS"
    echo ""
    echo "Usage: ./build.sh [options]"
    echo ""
    echo "Options:"
    echo "  --lib-only        Build only library (no demos)"
    echo "  --demo-only       Build only demo programs"
    echo "  --clean           Clean build artifacts before building"
    echo "  --fpc-path <path> Specify custom FPC compiler path"
    echo "  --debug           Build with debug info instead of release"
    echo "  --help            Show this help message"
    echo ""
    echo "Examples:"
    echo "  ./build.sh                              # Build all"
    echo "  ./build.sh --lib-only                   # Build only library"
    echo "  ./build.sh --demo-only                  # Build only demos"
    echo "  ./build.sh --clean                      # Clean and rebuild"
    echo "  ./build.sh --fpc-path /opt/fpc/bin/fpc  # Use specific FPC"
    echo ""
}

# ============================================================================
# Parse Arguments
# ============================================================================
while [[ $# -gt 0 ]]; do
    case $1 in
        --lib-only)
            LIB_ONLY=true
            shift
            ;;
        --demo-only)
            DEMO_ONLY=true
            shift
            ;;
        --clean)
            CLEAN_BUILD=true
            shift
            ;;
        --fpc-path)
            FPC_PATH="$2"
            shift 2
            ;;
        --debug)
            DEBUG_BUILD=true
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
# Find FPC Compiler
# ============================================================================
find_fpc() {
    # 1. Use custom path if specified
    if [[ -n "$FPC_PATH" ]]; then
        if [[ -x "$FPC_PATH" ]]; then
            echo "$FPC_PATH"
            return 0
        else
            echo -e "${YELLOW}WARNING: Specified FPC path not found: $FPC_PATH${NC}" >&2
        fi
    fi

    # 2. Project-local FPC (./fpc/)
    local local_fpc="$SCRIPT_DIR/fpc/bin/fpc"
    if [[ -x "$local_fpc" ]]; then
        echo "$local_fpc"
        return 0
    fi

    # 3. System PATH
    if command -v fpc &> /dev/null; then
        command -v fpc
        return 0
    fi

    return 1
}

# ============================================================================
# Stub Notice
# ============================================================================
echo -e "${YELLOW}"
echo "╔════════════════════════════════════════════════════════════════════╗"
echo "║                                                                    ║"
echo "║  STUB: This build script is a placeholder for Linux/macOS.        ║"
echo "║                                                                    ║"
echo "║  Full implementation will be completed when testing on Linux.     ║"
echo "║                                                                    ║"
echo "║  For now, use the Windows build.ps1 script or compile manually:   ║"
echo "║                                                                    ║"
echo "║    fpc -MObjFPC -Sh src/audiotest.lpr                             ║"
echo "║    fpc -MObjFPC -Sh src/demo_presets.pas                          ║"
echo "║                                                                    ║"
echo "╚════════════════════════════════════════════════════════════════════╝"
echo -e "${NC}"

show_banner

# Show what would be done
echo -e "${CYAN}Configuration:${NC}"
echo -e "  Script directory: ${GRAY}$SCRIPT_DIR${NC}"
echo -e "  Source directory: ${GRAY}$SRC_DIR${NC}"
echo -e "  Lib-only: ${GRAY}$LIB_ONLY${NC}"
echo -e "  Demo-only: ${GRAY}$DEMO_ONLY${NC}"
echo -e "  Clean: ${GRAY}$CLEAN_BUILD${NC}"
echo -e "  Debug: ${GRAY}$DEBUG_BUILD${NC}"
echo ""

# Try to find FPC
FPC=$(find_fpc) || true
if [[ -n "$FPC" ]]; then
    FPC_VERSION=$("$FPC" -iV 2>/dev/null || echo "unknown")
    echo -e "${GREEN}Found FPC: $FPC (version $FPC_VERSION)${NC}"
else
    echo -e "${YELLOW}FPC not found. Install with:${NC}"
    echo "  Debian/Ubuntu: sudo apt install fpc"
    echo "  Fedora: sudo dnf install fpc"
    echo "  Arch: sudo pacman -S fpc"
    echo "  macOS: brew install fpc"
fi

echo ""
echo -e "${CYAN}TODO: Implement full build logic for Linux/macOS${NC}"
echo ""

exit 0
