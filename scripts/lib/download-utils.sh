# Download utilities for SedaiAudio Foundation scripts (Linux/macOS)
# Bash counterpart of scripts/lib/download-utils.ps1.
#
# Meant to be sourced, not executed:
#   source "$SCRIPT_DIR/scripts/lib/download-utils.sh"
#
# Every function returns 0 on success and a non-zero status on failure, and
# leaves a human-readable message in DU_MESSAGE. Extra results are exported in
# DU_* variables (DU_BYTES, DU_ACTUAL_HASH, DU_AVAILABLE_BYTES).

DU_MESSAGE=""
DU_BYTES=0
DU_ACTUAL_HASH=""
DU_AVAILABLE_BYTES=0

# Colors are optional: honour the caller's palette when it defines one.
: "${RED:=}" ; : "${GREEN:=}" ; : "${YELLOW:=}" ; : "${CYAN:=}" ; : "${GRAY:=}" ; : "${NC:=}"

# ----------------------------------------------------------------------------
# du_file_size <path> -> prints size in bytes
# ----------------------------------------------------------------------------
du_file_size() {
    local f="$1"
    if stat -c%s "$f" >/dev/null 2>&1; then
        stat -c%s "$f"            # GNU coreutils
    else
        stat -f%z "$f"            # BSD / macOS
    fi
}

# ----------------------------------------------------------------------------
# du_download <url> <out_file> [--quiet]
#   Downloads with a progress indicator (curl, falling back to wget).
# ----------------------------------------------------------------------------
du_download() {
    local url="$1" out_file="$2" quiet="${3:-}"
    local out_dir
    DU_MESSAGE=""
    DU_BYTES=0

    out_dir="$(dirname "$out_file")"
    [[ -d "$out_dir" ]] || mkdir -p "$out_dir"

    local rc=0
    if command -v curl >/dev/null 2>&1; then
        if [[ "$quiet" == "--quiet" ]]; then
            curl -fsSL --retry 3 -A "SedaiAudio-Installer/1.0" -o "$out_file" "$url" || rc=$?
        else
            echo -e "${CYAN}Starting download...${NC}"
            curl -fL --retry 3 --progress-bar -A "SedaiAudio-Installer/1.0" -o "$out_file" "$url" || rc=$?
        fi
    elif command -v wget >/dev/null 2>&1; then
        if [[ "$quiet" == "--quiet" ]]; then
            wget -q -U "SedaiAudio-Installer/1.0" -O "$out_file" "$url" || rc=$?
        else
            echo -e "${CYAN}Starting download...${NC}"
            wget --show-progress -q -U "SedaiAudio-Installer/1.0" -O "$out_file" "$url" || rc=$?
        fi
    else
        DU_MESSAGE="Neither curl nor wget is available"
        return 1
    fi

    if [[ $rc -ne 0 ]]; then
        DU_MESSAGE="Download failed (exit code $rc): $url"
        rm -f "$out_file"
        return 1
    fi

    if [[ ! -f "$out_file" ]]; then
        DU_MESSAGE="Download completed but file not found"
        return 1
    fi

    DU_BYTES="$(du_file_size "$out_file")"
    if [[ "$DU_BYTES" -eq 0 ]]; then
        DU_MESSAGE="Downloaded file is empty"
        rm -f "$out_file"
        return 1
    fi

    DU_MESSAGE="Download completed successfully"
    return 0
}

# ----------------------------------------------------------------------------
# du_verify_hash <file> <expected sha256>
# ----------------------------------------------------------------------------
du_verify_hash() {
    local file_path="$1" expected="$2"
    DU_MESSAGE=""
    DU_ACTUAL_HASH=""

    if [[ ! -f "$file_path" ]]; then
        DU_MESSAGE="File not found: $file_path"
        return 3
    fi

    local actual=""
    if command -v sha256sum >/dev/null 2>&1; then
        actual="$(sha256sum "$file_path" | awk '{print $1}')"
    elif command -v shasum >/dev/null 2>&1; then
        actual="$(shasum -a 256 "$file_path" | awk '{print $1}')"
    else
        DU_MESSAGE="No sha256 tool available (sha256sum / shasum) - cannot verify"
        return 3
    fi

    DU_ACTUAL_HASH="$actual"
    local expected_lc
    expected_lc="$(echo "$expected" | tr '[:upper:]' '[:lower:]')"
    actual="$(echo "$actual" | tr '[:upper:]' '[:lower:]')"

    if [[ "$actual" == "$expected_lc" ]]; then
        DU_MESSAGE="Hash verification passed"
        return 0
    fi

    DU_MESSAGE="Hash mismatch. Expected: $expected_lc, Got: $actual"
    return 3
}

# ----------------------------------------------------------------------------
# du_extract <archive> <destination>
#   Supports .zip (unzip / bsdtar / python3) and .tar[.gz|.xz|.bz2].
# ----------------------------------------------------------------------------
du_extract() {
    local archive="$1" dest="$2"
    DU_MESSAGE=""

    if [[ ! -f "$archive" ]]; then
        DU_MESSAGE="Archive not found: $archive"
        return 2
    fi
    mkdir -p "$dest"

    local rc=0
    case "$archive" in
        *.zip)
            if command -v unzip >/dev/null 2>&1; then
                unzip -oq "$archive" -d "$dest" || rc=$?
            elif command -v bsdtar >/dev/null 2>&1; then
                bsdtar -xf "$archive" -C "$dest" || rc=$?
            elif command -v python3 >/dev/null 2>&1; then
                python3 -c 'import sys,zipfile; zipfile.ZipFile(sys.argv[1]).extractall(sys.argv[2])' \
                        "$archive" "$dest" || rc=$?
            else
                DU_MESSAGE="No unzip tool available (unzip / bsdtar / python3)"
                return 2
            fi
            ;;
        *.tar.gz|*.tgz)   tar -xzf "$archive" -C "$dest" || rc=$? ;;
        *.tar.xz)         tar -xJf "$archive" -C "$dest" || rc=$? ;;
        *.tar.bz2)        tar -xjf "$archive" -C "$dest" || rc=$? ;;
        *.tar)            tar -xf  "$archive" -C "$dest" || rc=$? ;;
        *)
            DU_MESSAGE="Unsupported archive type: $archive"
            return 2
            ;;
    esac

    if [[ $rc -ne 0 ]]; then
        DU_MESSAGE="Extraction failed (exit code $rc)"
        return 2
    fi

    DU_MESSAGE="Extraction completed successfully"
    return 0
}

# ----------------------------------------------------------------------------
# du_check_disk_space <path> <required MB>
# ----------------------------------------------------------------------------
du_check_disk_space() {
    local path="$1" required_mb="$2"
    DU_MESSAGE=""
    DU_AVAILABLE_BYTES=0

    # Walk up until an existing directory is found (the target may not exist yet)
    while [[ -n "$path" && ! -d "$path" ]]; do
        path="$(dirname "$path")"
    done

    local avail_kb
    avail_kb="$(df -Pk "$path" 2>/dev/null | awk 'NR==2 {print $4}')"
    if [[ -z "$avail_kb" ]]; then
        DU_MESSAGE="Could not verify disk space (assuming sufficient)"
        return 0
    fi

    DU_AVAILABLE_BYTES=$(( avail_kb * 1024 ))
    local avail_mb=$(( avail_kb / 1024 ))

    if [[ $avail_mb -lt $required_mb ]]; then
        DU_MESSAGE="Insufficient disk space. Required: $required_mb MB, Available: $avail_mb MB"
        return 4
    fi

    DU_MESSAGE="Disk space check passed ($avail_mb MB free)"
    return 0
}

# ----------------------------------------------------------------------------
# du_check_internet [test url]
# ----------------------------------------------------------------------------
du_check_internet() {
    local test_url="${1:-https://github.com}"
    DU_MESSAGE=""

    if command -v curl >/dev/null 2>&1; then
        if curl -fsI --max-time 10 "$test_url" >/dev/null 2>&1; then
            DU_MESSAGE="Internet connection available"
            return 0
        fi
    elif command -v wget >/dev/null 2>&1; then
        if wget -q --spider --timeout=10 "$test_url" >/dev/null 2>&1; then
            DU_MESSAGE="Internet connection available"
            return 0
        fi
    else
        DU_MESSAGE="Neither curl nor wget is available"
        return 1
    fi

    DU_MESSAGE="No internet connection (could not reach $test_url)"
    return 1
}
