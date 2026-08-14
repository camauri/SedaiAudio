#!/usr/bin/env bash
#
# Regenerates src/SID/SedaiSIDWaveTables.inc from the reSID combined-waveform
# sample tables (wave6581_*.cpp / wave8580_*.cpp).
#
# Bash counterpart of gen_wave_tables.ps1 - same input files, same extraction
# rule (hex bytes outside /* */ comments, first 4096 per table) and byte-for-byte
# the same output layout (16 values per line).
#
# Usage:
#   ./gen_wave_tables.sh                 # reads ../../job/GoatTracker/src/resid
#   RESID_SRC=/path/to/resid ./gen_wave_tables.sh
#
# NOTE: this is a one-shot code generator, not part of the build. The generated
# .inc is committed; re-run it only when the reSID tables change.
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
BASE_PATH="${RESID_SRC:-$PROJECT_ROOT/job/GoatTracker/src/resid}"
OUT_PATH="$SCRIPT_DIR/SedaiSIDWaveTables.inc"

if [[ ! -d "$BASE_PATH" ]]; then
    echo "ERROR: reSID source directory not found: $BASE_PATH" >&2
    echo "Set RESID_SRC to the folder holding wave6581__ST.cpp etc." >&2
    exit 1
fi

if ! command -v perl >/dev/null 2>&1; then
    echo "ERROR: perl is required by this script." >&2
    exit 1
fi

# file|pascal name|description
TABLES=(
    "wave6581__ST.cpp|Wave6581_ST|6581 Sawtooth+Triangle"
    "wave6581_P_T.cpp|Wave6581_PT|6581 Pulse+Triangle"
    "wave6581_PS_.cpp|Wave6581_PS|6581 Pulse+Sawtooth"
    "wave6581_PST.cpp|Wave6581_PST|6581 Pulse+Sawtooth+Triangle"
    "wave8580__ST.cpp|Wave8580_ST|8580 Sawtooth+Triangle"
    "wave8580_P_T.cpp|Wave8580_PT|8580 Pulse+Triangle"
    "wave8580_PS_.cpp|Wave8580_PS|8580 Pulse+Sawtooth"
    "wave8580_PST.cpp|Wave8580_PST|8580 Pulse+Sawtooth+Triangle"
)

{
    echo '{ Combined waveform lookup tables from ReSID }'
    echo '{ Each table contains 4096 bytes sampled from real SID chips }'
    echo ''
} > "$OUT_PATH"

for entry in "${TABLES[@]}"; do
    IFS='|' read -r file name desc <<< "$entry"
    src="$BASE_PATH/$file"

    if [[ ! -f "$src" ]]; then
        echo "ERROR: table source not found: $src" >&2
        exit 1
    fi

    # Strip /* */ comments, then take every standalone 0xNN byte (the same
    # negative look-around the PowerShell version uses, so 0x### addresses
    # inside the file are not picked up).
    count="$(perl -0777 -ne '
        s{/\*[^*]*\*/}{}g;
        my @v = /(?<![0-9a-fA-Fx])0x([0-9a-fA-F]{2})(?![0-9a-fA-F])/g;
        @v = @v[0..4095] if @v > 4096;
        print scalar(@v);
    ' "$src")"

    echo "Processing $name: $count values"

    {
        echo "{ $desc }"
        echo "$name: array[0..4095] of Byte = ("
        perl -0777 -ne '
            s{/\*[^*]*\*/}{}g;
            my @v = /(?<![0-9a-fA-Fx])0x([0-9a-fA-F]{2})(?![0-9a-fA-F])/g;
            @v = @v[0..4095] if @v > 4096;
            @v = map { "\$" . uc($_) } @v;
            for (my $i = 0; $i < 4096; $i += 16) {
                my $end = $i + 15; $end = 4095 if $end > 4095;
                my $line = join(", ", @v[$i..$end]);
                print "  " . $line . ($end < 4095 ? ",\n" : "\n");
            }
        ' "$src"
        echo ');'
        echo ''
    } >> "$OUT_PATH"
done

echo "Created: $OUT_PATH"
if stat -c%s "$OUT_PATH" >/dev/null 2>&1; then
    stat -c%s "$OUT_PATH"
else
    stat -f%z "$OUT_PATH"
fi
