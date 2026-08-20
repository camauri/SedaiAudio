<#
.SYNOPSIS
    Build SedaiAudio Foundation projects

.DESCRIPTION
    Cross-platform build script for SedaiAudio Foundation.
    Compiles library units and test/demo programs.
    Supports building individual targets.

    Directory structure:
    - src/   : Library source files (SedaiAudioFoundation units)
    - test/  : Test and demo programs
    - deps/  : Dependencies (SDL2 bindings)
    - bin/   : Output executables
    - lib/   : Compiled units

.PARAMETER LibOnly
    Build only library units (no tests)

.PARAMETER TestOnly
    Build only test programs

.PARAMETER Target
    Build a specific target by name (e.g., audiotest)
    Can be combined with -Clean

.PARAMETER Clean
    Clean build artifacts before building.
    If used alone, only cleans without building.
    If combined with other options, cleans first then builds.

.PARAMETER CleanOnly
    Only clean build artifacts, do not build anything

.PARAMETER FpcPath
    Path to a specific FPC compiler

.PARAMETER Debug
    Build with debug info instead of release optimizations

.PARAMETER NoBanner
    Suppress the ASCII art banner (useful when called from other scripts)

.PARAMETER CPU
    Target CPU: x86_64, i386, aarch64 (default: x86_64)

.PARAMETER OS
    Target OS: win64, win32, linux, darwin (default: win64)

.PARAMETER AvxAll
    Enable all AVX2 optimizations (CAUTION: may cause issues in SDL2 callbacks)

.PARAMETER AvxCp
    Enable -CpCOREAVX2 (target CPU)

.PARAMETER AvxOp
    Enable -OpCOREAVX2 (optimize for CPU)

.PARAMETER AvxCf
    Enable -CfAVX2 (use AVX2 FPU instructions)

.PARAMETER Source
    Custom source file path for ad-hoc compilation

.PARAMETER Dest
    Custom destination path for ad-hoc compilation output

.EXAMPLE
    .\build.ps1                      # Build all (library + tests)
    .\build.ps1 -LibOnly             # Build only library
    .\build.ps1 -TestOnly            # Build only tests
    .\build.ps1 -Clean               # Clean and rebuild all
    .\build.ps1 -CleanOnly           # Only clean, no build
    .\build.ps1 -Target audiotest    # Build only audiotest
    .\build.ps1 -Target audiotest -Clean  # Clean and build audiotest
    .\build.ps1 -AvxAll              # Build with AVX2 optimizations
    .\build.ps1 -Source "mytest.pas" -Dest "bin\mytest.exe"  # Custom build

.NOTES
    Copyright (c) 2025 Maurizio Cammalleri
    Released under the GNU GPL v3
#>

param(
    [switch]$Help,
    [switch]$LibOnly,
    [switch]$TestOnly,   # build ONLY the QA test suite
    [switch]$Tests,      # add the QA test suite to the product build
    [switch]$Demos,      # force-build the demos (no prompt)
    [switch]$SkipDemos,  # force-skip the demos (no prompt)
    [switch]$Clean,
    [switch]$CleanOnly,
    [switch]$Debug,
    [switch]$NoBanner,
    [string]$FpcPath = '',
    [switch]$SelectFpc,
    [string]$Target = '',
    [string[]]$Define = @(),

    # AVX2 options (disabled by default - causes issues in SDL2 audio callbacks)
    [switch]$AvxAll,
    [switch]$AvxCp,
    [switch]$AvxOp,
    [switch]$AvxCf,

    # Custom source/dest for ad-hoc compilation
    [string]$Source = '',
    [string]$Dest = '',

    [ValidateSet('x86_64', 'i386', 'aarch64', '')]
    [string]$CPU = 'x86_64',

    [ValidateSet('win64', 'win32', 'linux', 'darwin', '')]
    [string]$OS = 'win64'
)

# Handle -Help immediately (before anything else)
if ($Help) {
    $width = 70
    $border = "=" * $width
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
    Write-Host "   ____           _       _    _             _ _       " -ForegroundColor White
    Write-Host "  / ___|  ___  __| | __ _(_)  / \  _   _  __| (_) ___  " -ForegroundColor White
    Write-Host "  \___ \ / _ \/ _`` |/ _`` | | / _ \| | | |/ _`` | |/ _ \ " -ForegroundColor White
    Write-Host "   ___) |  __/ (_| | (_| | |/ ___ \ |_| | (_| | | (_) |" -ForegroundColor White
    Write-Host "  |____/ \___|\__,_|\__,_|_/_/   \_\__,_|\__,_|_|\___/ " -ForegroundColor White
    Write-Host ""
    Write-Host "                   BUILD SCRIPT" -ForegroundColor Yellow
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
    Write-Host "SedaiAudio Foundation Build Script" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "USAGE:" -ForegroundColor Yellow
    Write-Host "    .\build.ps1 [options]"
    Write-Host ""
    Write-Host "OPTIONS:" -ForegroundColor Yellow
    Write-Host "    -Help           Show this help message"
    Write-Host "    (no options)    Build tools + ask whether to build the demos"
    Write-Host "    -LibOnly        Build only the library units"
    Write-Host "    -TestOnly       Build only the QA test suite"
    Write-Host "    -Tests          Also build the QA test suite (alongside tools)"
    Write-Host "    -Demos          Build the demos (no prompt)"
    Write-Host "    -SkipDemos      Do not build the demos (no prompt)"
    Write-Host "    -Target <name>  Build one specific target (tools/demos/tests)"
    Write-Host "    -Clean          Clean build artifacts before building"
    Write-Host "    -CleanOnly      Only clean, do not build"
    Write-Host "    -Debug          Build with debug info instead of release"
    Write-Host "    -FpcPath <path> Path to a specific FPC compiler (one-off, not stored)"
    Write-Host "    -SelectFpc      List every FPC found and choose one (stored in setup.config.json)"
    Write-Host "    -CPU <cpu>      Target CPU: x86_64, i386, aarch64 (default: x86_64)"
    Write-Host "    -OS <os>        Target OS: win64, win32, linux, darwin (default: win64)"
    Write-Host ""
    Write-Host "AVX2 OPTIONS (use with caution - may cause issues in audio callbacks):" -ForegroundColor Yellow
    Write-Host "    -AvxAll         Enable all AVX2 optimizations"
    Write-Host "    -AvxCp          Enable -CpCOREAVX2 (target CPU)"
    Write-Host "    -AvxOp          Enable -OpCOREAVX2 (optimize for CPU)"
    Write-Host "    -AvxCf          Enable -CfAVX2 (AVX2 FPU instructions)"
    Write-Host ""
    Write-Host "CUSTOM BUILD:" -ForegroundColor Yellow
    Write-Host "    -Source <file>  Custom source file path"
    Write-Host "    -Dest <file>    Custom destination executable path"
    Write-Host ""
    Write-Host "DIRECTORY STRUCTURE:" -ForegroundColor Yellow
    Write-Host "    src/            Library source files"
    Write-Host "    test/           Test and demo programs"
    Write-Host "    deps/sdl2/      SDL2 Pascal bindings"
    Write-Host "    bin/<platform>/ Output executables"
    Write-Host "    lib/<platform>/ Compiled units"
    Write-Host ""
    Write-Host "AVAILABLE TARGETS:" -ForegroundColor Yellow
    Write-Host "    test_compilation     Unit compilation test"
    Write-Host "    test_saf_main        Main SAF API test (Classic, FM, Wavetable synthesis)"
    Write-Host "    demo_synth           Synth demo"
    Write-Host "    sng_player           SNG file player"
    Write-Host "    sng_dump             SNG register dump tool (VICE format)"
    Write-Host "    audiotest            Audio backend test"
    Write-Host "    sedaisid_test        SedaiSIDEvo verification test"
    Write-Host "    saf_regression       Headless integrated render-path regression suite"
    Write-Host ""
    Write-Host "EXAMPLES:" -ForegroundColor Yellow
    Write-Host "    .\build.ps1                           # Build all"
    Write-Host "    .\build.ps1 -Clean                    # Clean and rebuild all"
    Write-Host "    .\build.ps1 -Target test_compilation  # Build compilation test"
    Write-Host "    .\build.ps1 -Debug                    # Build with debug symbols"
    Write-Host "    .\build.ps1 -AvxAll                   # Build with AVX2 (risky)"
    Write-Host "    .\build.ps1 -Source test.pas -Dest bin\test.exe  # Custom build"
    Write-Host ""
    Write-Host "NOTES:" -ForegroundColor Yellow
    Write-Host "    - Run .\setup.ps1 first to install dependencies"
    Write-Host "    - Executables are output to bin\<platform>\"
    Write-Host "    - Compiled units are output to lib\<platform>\"
    Write-Host "    - AVX2 is disabled by default (causes crashes in SDL2 callbacks)"
    Write-Host ""
    exit 0
}

$ErrorActionPreference = 'Stop'
$Script:ProjectRoot = $PSScriptRoot
$Script:SrcDir = Join-Path $ProjectRoot 'src'
$Script:TestDir = Join-Path $ProjectRoot 'test'
$Script:LibDir = Join-Path $ProjectRoot 'lib'
$Script:BinDir = Join-Path $ProjectRoot 'bin'
$Script:DepsDir = Join-Path $ProjectRoot 'deps'
$Script:SDL2Dir = Join-Path $DepsDir 'sdl2'

# ============================================================================
# Verify Directory Structure
# ============================================================================
function Test-DirectoryStructure {
    param([string]$PlatformDir)

    $requiredDirs = @(
        @{ Path = $SrcDir; Name = 'src'; Critical = $true },
        @{ Path = $TestDir; Name = 'test'; Critical = $false },
        @{ Path = (Join-Path $BinDir $PlatformDir); Name = "bin\$PlatformDir"; Critical = $false },
        @{ Path = (Join-Path $LibDir $PlatformDir); Name = "lib\$PlatformDir"; Critical = $false },
        @{ Path = $SDL2Dir; Name = 'deps\sdl2'; Critical = $true }
    )

    $errors = @()
    $warnings = @()

    foreach ($dir in $requiredDirs) {
        if (-not (Test-Path $dir.Path)) {
            if ($dir.Critical) {
                $errors += $dir.Name
            }
            else {
                $warnings += $dir.Name
                # Auto-create non-critical directories
                New-Item -ItemType Directory -Path $dir.Path -Force | Out-Null
            }
        }
    }

    # Check for SDL2 marker file
    $sdl2Marker = Join-Path $SDL2Dir 'sdl2.pas'
    if (-not (Test-Path $sdl2Marker)) {
        $errors += 'deps\sdl2\sdl2.pas (SDL2 Pascal bindings)'
    }

    return @{
        Errors = $errors
        Warnings = $warnings
        Success = ($errors.Count -eq 0)
    }
}

# ============================================================================
# ASCII Art Banner
# ============================================================================
function Show-Banner {
    $width = 70
    $border = "=" * $width

    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
    Write-Host "   ____           _       _    _             _ _       " -ForegroundColor White
    Write-Host "  / ___|  ___  __| | __ _(_)  / \  _   _  __| (_) ___  " -ForegroundColor White
    Write-Host "  \___ \ / _ \/ _`` |/ _`` | | / _ \| | | |/ _`` | |/ _ \ " -ForegroundColor White
    Write-Host "   ___) |  __/ (_| | (_| | |/ ___ \ |_| | (_| | | (_) |" -ForegroundColor White
    Write-Host "  |____/ \___|\__,_|\__,_|_/_/   \_\__,_|\__,_|_|\___/ " -ForegroundColor White
    Write-Host ""
    Write-Host "                   BUILD SCRIPT" -ForegroundColor Yellow
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  Audio Foundation - Professional Audio Synthesis Library" -ForegroundColor Gray
    Write-Host "  Copyright (c) 2025 Maurizio Cammalleri - GPL-3.0" -ForegroundColor Gray
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
}

# ============================================================================
# Find FPC Compiler
# ============================================================================
# ----------------------------------------------------------------------------
# Reading and writing setup.config.json, so the compiler is chosen once and not
# at every build. Same file and same keys as build.sh, so a shared checkout
# keeps working across the two scripts and the two platforms.
# ----------------------------------------------------------------------------
function Get-ConfigValue {
    param([string]$Key)
    $file = Join-Path $ProjectRoot 'setup.config.json'
    if (-not (Test-Path $file)) { return $null }
    try {
        $cfg = Get-Content $file -Raw | ConvertFrom-Json
        if ($cfg.PSObject.Properties.Name -contains $Key) { return $cfg.$Key }
    } catch { }
    return $null
}

function Set-ConfigValue {
    param([string]$Key, [string]$Value)
    $file = Join-Path $ProjectRoot 'setup.config.json'
    $cfg = $null
    if (Test-Path $file) {
        try { $cfg = Get-Content $file -Raw | ConvertFrom-Json } catch { }
    }
    if (-not $cfg) { $cfg = [PSCustomObject]@{} }
    if ($cfg.PSObject.Properties.Name -contains $Key) { $cfg.$Key = $Value }
    else { $cfg | Add-Member -NotePropertyName $Key -NotePropertyValue $Value }
    $cfg | ConvertTo-Json -Depth 5 | Set-Content $file -Encoding UTF8
}

# Every compiler on this machine, most-likely-meant first, no duplicates. The
# deep scan is deliberately last: it finds installs in odd places but says
# nothing about which one is intended.
function Get-FpcCandidates {
    $list = New-Object System.Collections.Generic.List[string]

    $list.Add((Join-Path $ProjectRoot 'fpc\bin\fpc.exe'))
    foreach ($p in @('fpc\3.2.2\bin\x86_64-win64\fpc.exe',
                     'fpc\3.2.2\bin\i386-win32\fpc.exe')) {
        $list.Add((Join-Path $ProjectRoot $p))
    }
    foreach ($root in @('C:\lazarus', 'C:\FPC', "$env:USERPROFILE\tools\fp",
                        "$env:USERPROFILE\fpcupdeluxe", 'C:\Program Files\Lazarus')) {
        if (Test-Path $root) {
            Get-ChildItem -Path $root -Filter fpc.exe -Recurse -Depth 5 -ErrorAction SilentlyContinue |
                ForEach-Object { $list.Add($_.FullName) }
        }
    }
    Get-ChildItem -Path 'C:\' -Filter 'lazarus-*' -Directory -ErrorAction SilentlyContinue |
        ForEach-Object {
            Get-ChildItem -Path $_.FullName -Filter fpc.exe -Recurse -Depth 5 -ErrorAction SilentlyContinue |
                ForEach-Object { $list.Add($_.FullName) }
        }
    $onPath = Get-Command fpc -ErrorAction SilentlyContinue
    if ($onPath) { $list.Add($onPath.Source) }

    $seen = @{}
    $out = New-Object System.Collections.Generic.List[string]
    foreach ($c in $list) {
        if (-not $c) { continue }
        if (-not (Test-Path $c)) { continue }
        $full = (Resolve-Path $c -ErrorAction SilentlyContinue).Path
        if (-not $full) { $full = $c }
        if ($seen.ContainsKey($full)) { continue }
        $seen[$full] = $true
        $out.Add($full)
    }
    return $out
}

# Does this compiler actually COMPILE? Not "does the binary run" - fpc -iV
# answers that happily on an install whose RTL it cannot find, which is how a
# half-finished tree ends up shadowing a working one and breaking the build
# with "Can't find unit system". The only honest test is a build, done the way
# build.ps1 builds: no explicit config file, because that is what the real
# invocation does.
function Test-FpcWorks {
    param([string]$Fpc)
    $d = Join-Path ([System.IO.Path]::GetTempPath()) ([System.IO.Path]::GetRandomFileName())
    New-Item -ItemType Directory -Path $d -Force | Out-Null
    try {
        $src = Join-Path $d 'probe.pas'
        Set-Content -Path $src -Value 'begin end.' -Encoding ASCII
        $p = Start-Process -FilePath $Fpc -ArgumentList @("-o$d\probe.exe", $src) `
                           -WorkingDirectory $d -NoNewWindow -Wait -PassThru `
                           -RedirectStandardOutput ([System.IO.Path]::GetTempFileName()) `
                           -RedirectStandardError  ([System.IO.Path]::GetTempFileName())
        return ($p.ExitCode -eq 0)
    } catch {
        return $false
    } finally {
        Remove-Item $d -Recurse -Force -ErrorAction SilentlyContinue
    }
}

# ...\fpc\bin\<platform>\fpc.exe  ->  ...\fpc   (the root form build.sh reads
# as FpcPath). Anything else has no such root and returns nothing.
function Get-FpcRoot {
    param([string]$Bin)
    $binDir = Split-Path $Bin -Parent            # ...\bin\<platform>
    $up1    = Split-Path $binDir -Parent         # ...\bin
    if ((Split-Path $up1 -Leaf) -eq 'bin') { return (Split-Path $up1 -Parent) }
    if ((Split-Path $binDir -Leaf) -eq 'bin') { return (Split-Path $binDir -Parent) }
    return $null
}

# List what is installed, prove which ones work, and ask - once.
function Select-FPC {
    $paths = Get-FpcCandidates
    $rows  = @()
    foreach ($c in $paths) {
        $ver = (& $c -iV 2>$null)
        if (-not $ver) { continue }
        $rows += [PSCustomObject]@{ Path = $c; Version = "$ver".Trim(); Works = (Test-FpcWorks $c) }
    }
    if ($rows.Count -eq 0) { return $null }

    $usable = @($rows | Where-Object { $_.Works })
    if ($usable.Count -eq 0) {
        Write-Host "ERROR: a Free Pascal Compiler was found, but none can compile." -ForegroundColor Red
        foreach ($r in $rows) { Write-Host ("  FPC {0,-8} {1}" -f $r.Version, $r.Path) }
        Write-Host "An install without a usable fpc.cfg is the usual cause." -ForegroundColor Yellow
        return $null
    }
    # One working compiler and nothing else to weigh: take it, rather than
    # asking a question with a single answer.
    if ($rows.Count -eq 1) { return $rows[0].Path }

    Write-Host ""
    Write-Host "Free Pascal compilers found on this machine:" -ForegroundColor Cyan
    for ($i = 0; $i -lt $rows.Count; $i++) {
        if ($rows[$i].Works) {
            Write-Host ("  {0}) FPC {1,-8} {2}" -f ($i+1), $rows[$i].Version, $rows[$i].Path)
        } else {
            Write-Host ("  {0}) FPC {1,-8} {2}   [cannot compile - skipped]" -f `
                        ($i+1), $rows[$i].Version, $rows[$i].Path) -ForegroundColor Yellow
        }
    }
    Write-Host ""

    # No console means no question: a script or a CI run must fail loudly
    # rather than hang on a prompt, or pick for the user and be wrong quietly.
    if ([Console]::IsInputRedirected) {
        Write-Host "Not interactive, so nothing was chosen and nothing was stored." -ForegroundColor Yellow
        Write-Host "Run .\build.ps1 -SelectFpc once interactively, or pass -FpcPath." -ForegroundColor Yellow
        return $null
    }

    $default = [array]::IndexOf($rows, $usable[0]) + 1
    while ($true) {
        $sel = Read-Host "Which one should this project use? [$default]"
        if (-not $sel) { $sel = $default }
        $n = 0
        if (-not [int]::TryParse($sel, [ref]$n)) { Write-Host "  a number, please"; continue }
        if ($n -lt 1 -or $n -gt $rows.Count) { Write-Host "  out of range"; continue }
        if (-not $rows[$n-1].Works) { Write-Host "  that one cannot compile; pick another"; continue }
        break
    }

    $chosen = $rows[$n-1]
    Set-ConfigValue -Key 'FpcBin' -Value $chosen.Path
    $root = Get-FpcRoot $chosen.Path
    if ($root) { Set-ConfigValue -Key 'FpcPath' -Value $root }
    Write-Host ("Stored in setup.config.json: FPC {0} - {1}" -f $chosen.Version, $chosen.Path) -ForegroundColor Green
    Write-Host "Change it later with .\build.ps1 -SelectFpc" -ForegroundColor DarkGray
    return $chosen.Path
}

function Find-FPC {
    param([string]$CustomPath)

    # 1. Explicit override - deliberately NOT stored: it is a one-off, and
    #    writing it would turn "just this once" into the project's setting.
    if ($CustomPath -and (Test-Path $CustomPath)) { return $CustomPath }
    elseif ($CustomPath) {
        Write-Host "WARNING: Specified FPC path not found: $CustomPath" -ForegroundColor Yellow
    }

    # 2. The stored choice.
    if (-not $SelectFpc) {
        $stored = Get-ConfigValue -Key 'FpcBin'
        if ($stored -and (Test-Path $stored)) { return $stored }
        # The root form, which is what build.sh writes.
        $root = Get-ConfigValue -Key 'FpcPath'
        if ($root) {
            foreach ($p in @("bin\x86_64-win64\fpc.exe", "bin\i386-win32\fpc.exe", "bin\fpc.exe")) {
                $cand = Join-Path $root $p
                if (Test-Path $cand) { return $cand }
            }
        }
    }

    # 3. Nothing stored, or -SelectFpc: look at everything and ask, once.
    return Select-FPC
}

# ============================================================================
# Get Platform Directory
# ============================================================================
function Get-PlatformDir {
    param([string]$cpu, [string]$os)
    return "$cpu-$os"
}

# ============================================================================
# Build a Single Target
# ============================================================================
function Build-Target {
    param(
        [string]$SourceFile,
        [string]$SourceDir,
        [string]$OutputPath,
        [string]$FPC,
        [string]$PlatformDir,
        [string]$TargetCPU,
        [string]$TargetOS,
        [bool]$IsDebug,
        [bool]$UseAvxCp,
        [bool]$UseAvxOp,
        [bool]$UseAvxCf
    )

    $srcPath = Join-Path $SourceDir $SourceFile
    if (-not (Test-Path $srcPath)) {
        Write-Host "  ERROR: Source file not found: $srcPath" -ForegroundColor Red
        return $false
    }

    # Create output directories
    $libPath = Join-Path $LibDir $PlatformDir
    $outputDir = Split-Path -Parent $OutputPath

    if (-not (Test-Path $libPath)) { New-Item -ItemType Directory -Path $libPath -Force | Out-Null }
    if ($outputDir -and -not (Test-Path $outputDir)) { New-Item -ItemType Directory -Path $outputDir -Force | Out-Null }

    # Build compiler options
    $opts = @()

    # Output name
    $opts += "-o`"$OutputPath`""

    # Target platform
    $opts += "-P$TargetCPU"
    $opts += "-T$TargetOS"

    # Mode
    $opts += '-MObjFPC'
    $opts += '-Sh'  # Use ansistrings

    if (-not $IsDebug) {
        # Release optimizations
        $opts += '-O2'

        # AVX2 optimizations (optional - disabled by default)
        # WARNING: AVX2 causes crashes in SDL2 audio callbacks on Win64
        # when complex floating-point expressions are used in cdecl callbacks.
        # See: sdl2_exact_envelope_test.pas for reproduction case.
        if ($UseAvxCp) {
            $opts += '-CpCOREAVX2'
        }
        if ($UseAvxOp) {
            $opts += '-OpCOREAVX2'
        }
        if ($UseAvxCf) {
            $opts += '-CfAVX2'
        }

        # Additional optimizations (safe)
        $opts += '-OoREGVAR'
        $opts += '-OoCSE'
        $opts += '-OoDFA'
        $opts += '-OoFASTMATH'
        $opts += '-OoCONSTPROP'

        # Strip and smart linking
        $opts += '-Xs'
        $opts += '-XX'
    }
    else {
        # Debug options
        $opts += '-g'
        $opts += '-gl'
        $opts += '-gw'
        $opts += '-Ci'
        $opts += '-Cr'
        $opts += '-Co'
        $opts += '-dDEBUG'
    }

    # Paths - include src directory and all subdirectories
    $opts += "-Fu`"$SrcDir`""
    $opts += "-Fu`"$SrcDir\Core`""
    $opts += "-Fu`"$SrcDir\Generators`""
    $opts += "-Fu`"$SrcDir\Modulators`""
    $opts += "-Fu`"$SrcDir\Processors`""
    $opts += "-Fu`"$SrcDir\Effects`""
    $opts += "-Fu`"$SrcDir\Voice`""
    $opts += "-Fu`"$SrcDir\Mixer`""
    $opts += "-Fu`"$SrcDir\Transport`""
    $opts += "-Fu`"$SrcDir\Project`""
    $opts += "-Fu`"$SrcDir\Platform`""
    $opts += "-Fu`"$SrcDir\SID`""
    $opts += "-Fu`"$SrcDir\Players`""
    $opts += "-Fu`"$SrcDir\FileIO`""
    $opts += "-Fu`"$SrcDir\Engine`""
    $opts += "-Fu`"$SrcDir\Wavetable`""
    $opts += "-Fu`"$SrcDir\Patch`""
    $opts += "-Fu`"$TestDir`""
    $opts += "-Fu`"$SDL2Dir`""  # SDL2 Pascal bindings
    $opts += "-FU`"$libPath`""

    # Custom defines
    foreach ($def in $Define) {
        $opts += "-d$def"
    }

    # Build command line
    $cmdArgs = $opts + @("`"$srcPath`"")

    # DEBUG: Show defines being passed
    if ($Define.Count -gt 0) {
        Write-Host "  Defines: $($Define -join ', ')" -ForegroundColor Cyan
    }

    $outputName = Split-Path -Leaf $OutputPath
    Write-Host "  Building $outputName..." -ForegroundColor White -NoNewline

    # Show AVX2 status if any enabled
    $avxFlags = @()
    if ($UseAvxCp) { $avxFlags += 'CpCOREAVX2' }
    if ($UseAvxOp) { $avxFlags += 'OpCOREAVX2' }
    if ($UseAvxCf) { $avxFlags += 'CfAVX2' }
    if ($avxFlags.Count -gt 0) {
        Write-Host " [AVX2: $($avxFlags -join ', ')]" -ForegroundColor Yellow -NoNewline
    }

    # Execute compiler
    $pinfo = New-Object System.Diagnostics.ProcessStartInfo
    $pinfo.FileName = $FPC
    $pinfo.Arguments = $cmdArgs -join ' '
    $pinfo.RedirectStandardOutput = $true
    $pinfo.RedirectStandardError = $true
    $pinfo.UseShellExecute = $false
    $pinfo.CreateNoWindow = $true
    $pinfo.WorkingDirectory = $ProjectRoot

    $process = New-Object System.Diagnostics.Process
    $process.StartInfo = $pinfo
    $process.Start() | Out-Null
    $stdout = $process.StandardOutput.ReadToEnd()
    $stderr = $process.StandardError.ReadToEnd()
    $process.WaitForExit()

    if ($process.ExitCode -eq 0) {
        Write-Host " OK" -ForegroundColor Green
        return $true
    }
    else {
        Write-Host " FAILED" -ForegroundColor Red
        if ($stderr) { Write-Host $stderr -ForegroundColor DarkRed }
        if ($stdout) { Write-Host $stdout -ForegroundColor DarkRed }
        return $false
    }
}

# ============================================================================
# Clean Build Artifacts
# ============================================================================
function Clean-Build {
    param([string]$PlatformDir)

    Write-Host "Cleaning build artifacts..." -ForegroundColor Yellow

    # Clean ALL platform directories in lib/ (not just current platform)
    if (Test-Path $LibDir) {
        $libSubDirs = Get-ChildItem -Path $LibDir -Directory -ErrorAction SilentlyContinue
        foreach ($subDir in $libSubDirs) {
            $subPath = $subDir.FullName
            $cleaned = $false
            if (Get-ChildItem -Path "$subPath\*.ppu" -ErrorAction SilentlyContinue) {
                Remove-Item -Path "$subPath\*.ppu" -Force -ErrorAction SilentlyContinue
                $cleaned = $true
            }
            if (Get-ChildItem -Path "$subPath\*.o" -ErrorAction SilentlyContinue) {
                Remove-Item -Path "$subPath\*.o" -Force -ErrorAction SilentlyContinue
                $cleaned = $true
            }
            if (Get-ChildItem -Path "$subPath\*.a" -ErrorAction SilentlyContinue) {
                Remove-Item -Path "$subPath\*.a" -Force -ErrorAction SilentlyContinue
                $cleaned = $true
            }
            if (Get-ChildItem -Path "$subPath\*.rsj" -ErrorAction SilentlyContinue) {
                Remove-Item -Path "$subPath\*.rsj" -Force -ErrorAction SilentlyContinue
                $cleaned = $true
            }
            if ($cleaned) {
                Write-Host "  Cleaned: $subPath" -ForegroundColor Gray
            }
        }
    }

    # Clean ALL platform directories in bin/ (only intermediate files, not executables)
    if (Test-Path $BinDir) {
        $binSubDirs = Get-ChildItem -Path $BinDir -Directory -ErrorAction SilentlyContinue
        foreach ($subDir in $binSubDirs) {
            $subPath = $subDir.FullName
            $cleaned = $false
            if (Get-ChildItem -Path "$subPath\*.ppu" -ErrorAction SilentlyContinue) {
                Remove-Item -Path "$subPath\*.ppu" -Force -ErrorAction SilentlyContinue
                $cleaned = $true
            }
            if (Get-ChildItem -Path "$subPath\*.o" -ErrorAction SilentlyContinue) {
                Remove-Item -Path "$subPath\*.o" -Force -ErrorAction SilentlyContinue
                $cleaned = $true
            }
            if (Get-ChildItem -Path "$subPath\*.rsj" -ErrorAction SilentlyContinue) {
                Remove-Item -Path "$subPath\*.rsj" -Force -ErrorAction SilentlyContinue
                $cleaned = $true
            }
            if ($cleaned) {
                Write-Host "  Cleaned: $subPath (units only)" -ForegroundColor Gray
            }
        }
    }

    # Clean src directory intermediate files
    Remove-Item -Path "$SrcDir\*.ppu" -Force -ErrorAction SilentlyContinue
    Remove-Item -Path "$SrcDir\*.o" -Force -ErrorAction SilentlyContinue
    Remove-Item -Path "$SrcDir\*.a" -Force -ErrorAction SilentlyContinue
    Remove-Item -Path "$SrcDir\*.rsj" -Force -ErrorAction SilentlyContinue
    Remove-Item -Path "$SrcDir\*.compiled" -Force -ErrorAction SilentlyContinue
    Remove-Item -Path "$SrcDir\link.res" -Force -ErrorAction SilentlyContinue
    Remove-Item -Path "$SrcDir\ppas.bat" -Force -ErrorAction SilentlyContinue
    Write-Host "  Cleaned: $SrcDir (intermediate files)" -ForegroundColor Gray

    # Clean test directory intermediate files
    if (Test-Path $TestDir) {
        Remove-Item -Path "$TestDir\*.ppu" -Force -ErrorAction SilentlyContinue
        Remove-Item -Path "$TestDir\*.o" -Force -ErrorAction SilentlyContinue
        Remove-Item -Path "$TestDir\*.a" -Force -ErrorAction SilentlyContinue
        Remove-Item -Path "$TestDir\*.rsj" -Force -ErrorAction SilentlyContinue
        Remove-Item -Path "$TestDir\*.compiled" -Force -ErrorAction SilentlyContinue
        Remove-Item -Path "$TestDir\link.res" -Force -ErrorAction SilentlyContinue
        Remove-Item -Path "$TestDir\ppas.bat" -Force -ErrorAction SilentlyContinue
        Write-Host "  Cleaned: $TestDir (intermediate files)" -ForegroundColor Gray
    }

    Write-Host "Clean complete." -ForegroundColor Green
}

# ============================================================================
# Main Script
# ============================================================================

if (-not $NoBanner) {
    Show-Banner
}

# Find FPC
$fpc = Find-FPC -CustomPath $FpcPath
if (-not $fpc) {
    Write-Host "ERROR: no usable Free Pascal Compiler." -ForegroundColor Red
    Write-Host ""
    Write-Host "Anything listed above as [cannot compile] was found but could not build a" -ForegroundColor Yellow
    Write-Host "two-word program - usually an install whose fpc.cfg is missing, so it does" -ForegroundColor Yellow
    Write-Host "not know where its own RTL is. Those are skipped rather than used." -ForegroundColor Yellow
    Write-Host ""
    Write-Host "Searched:" -ForegroundColor Yellow
    Write-Host "  1. -FpcPath (one-off, never stored)" -ForegroundColor Gray
    Write-Host "  2. FpcBin / FpcPath in setup.config.json (the stored choice)" -ForegroundColor Gray
    Write-Host "  3. .\fpc\, C:\lazarus*, C:\FPC, %USERPROFILE%\tools\fp, fpcupdeluxe" -ForegroundColor Gray
    Write-Host "  4. System PATH" -ForegroundColor Gray
    Write-Host ""
    Write-Host "Run .\build.ps1 -SelectFpc to choose one, or pass -FpcPath for a single build." -ForegroundColor Yellow
    exit 1
}

# Get FPC version
$fpcVersion = & $fpc -iV 2>$null
Write-Host "Compiler: FPC $fpcVersion" -ForegroundColor Gray
Write-Host "Path: $fpc" -ForegroundColor DarkGray

# Get platform
$platformDir = Get-PlatformDir -cpu $CPU -os $OS
Write-Host "Platform: $platformDir" -ForegroundColor Gray
Write-Host "Mode: $(if ($Debug) { 'Debug' } else { 'Release' })" -ForegroundColor Gray

# Show AVX2 status
$useAvxCp = $AvxAll -or $AvxCp
$useAvxOp = $AvxAll -or $AvxOp
$useAvxCf = $AvxAll -or $AvxCf
if ($useAvxCp -or $useAvxOp -or $useAvxCf) {
    Write-Host "AVX2: ENABLED (use with caution)" -ForegroundColor Yellow
} else {
    Write-Host "AVX2: disabled (safe mode)" -ForegroundColor Gray
}
Write-Host ""

# Verify directory structure
Write-Host "Checking directory structure..." -ForegroundColor Gray
$dirCheck = Test-DirectoryStructure -PlatformDir $platformDir

if ($dirCheck.Warnings.Count -gt 0) {
    foreach ($warn in $dirCheck.Warnings) {
        Write-Host "  Created: $warn" -ForegroundColor DarkYellow
    }
}

if (-not $dirCheck.Success) {
    Write-Host ""
    Write-Host "ERROR: Required directories/files not found:" -ForegroundColor Red
    foreach ($err in $dirCheck.Errors) {
        Write-Host "  - $err" -ForegroundColor Red
    }
    Write-Host ""
    Write-Host "Please run .\setup.ps1 first to initialize the project." -ForegroundColor Yellow
    exit 1
}
Write-Host "Directory structure OK" -ForegroundColor Green
Write-Host ""

# Clean if requested
if ($Clean -or $CleanOnly) {
    Clean-Build -PlatformDir $platformDir
    Write-Host ""

    # If CleanOnly, exit here
    if ($CleanOnly) {
        Write-Host "Clean completed." -ForegroundColor Green
        exit 0
    }
}

# Build counters
$success = 0
$failed = 0

# Define all build targets
# Build targets using the new modular architecture
# Kind: 'tool' = user-facing app (built by default), 'demo' = example (built by
# default only after the interactive prompt / -Demos), 'test' = QA suite (built
# only with -Tests / -TestOnly).
$allTargets = @{
    'sng_player' = @{
        Source = 'sng_player.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'sng_player.exe'
        Kind = 'tool'
    }
    'sng_dump' = @{
        Source = 'sng_dump.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'sng_dump.exe'
        Kind = 'tool'
    }
    'saf_play' = @{
        Source = 'saf_play.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'saf_play.exe'
        Kind = 'demo'
    }
    'demo_synth' = @{
        Source = 'demo_synth.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'demo_synth.exe'
        Kind = 'demo'
    }
    'test_saf_main' = @{
        Source = 'TestSAFMain.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'TestSAFMain.exe'
        Kind = 'test'
    }
    'audiotest' = @{
        Source = 'audiotest.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'audiotest.exe'
        Kind = 'test'
    }
    'sedaisid_test' = @{
        Source = 'sedaisid_test.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'sedaisid_test.exe'
        Kind = 'test'
    }
    'saf_regression' = @{
        Source = 'saf_regression.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'saf_regression.exe'
        Kind = 'test'
    }
    'patch_bas' = @{
        Source = 'patch_bas.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'patch_bas.exe'
        Kind = 'tool'
    }
}

# Adjust extension based on OS
if ($OS -notmatch 'win') {
    foreach ($key in $allTargets.Keys) {
        $allTargets[$key].Output = $allTargets[$key].Output -replace '\.exe$', ''
    }
}

# Handle custom Source/Dest build
if ($Source) {
    Write-Host "Custom Build" -ForegroundColor Cyan
    Write-Host "============" -ForegroundColor Cyan

    # Determine source path
    $sourcePath = $Source
    if (-not [System.IO.Path]::IsPathRooted($Source)) {
        $sourcePath = Join-Path $ProjectRoot $Source
    }

    if (-not (Test-Path $sourcePath)) {
        Write-Host "ERROR: Source file not found: $sourcePath" -ForegroundColor Red
        exit 1
    }

    # Determine destination path
    $destPath = $Dest
    if (-not $Dest) {
        # Default: bin/<platform>/<sourcename>.exe
        $baseName = [System.IO.Path]::GetFileNameWithoutExtension($Source)
        $ext = if ($OS -match 'win') { '.exe' } else { '' }
        $destPath = Join-Path (Join-Path $BinDir $platformDir) "$baseName$ext"
    }
    elseif (-not [System.IO.Path]::IsPathRooted($Dest)) {
        $destPath = Join-Path $ProjectRoot $Dest
    }

    $sourceDir = Split-Path -Parent $sourcePath
    $sourceFile = Split-Path -Leaf $sourcePath

    $result = Build-Target -SourceFile $sourceFile -SourceDir $sourceDir `
        -OutputPath $destPath -FPC $fpc -PlatformDir $platformDir `
        -TargetCPU $CPU -TargetOS $OS -IsDebug $Debug `
        -UseAvxCp $useAvxCp -UseAvxOp $useAvxOp -UseAvxCf $useAvxCf

    if ($result) { $success++ } else { $failed++ }
    Write-Host ""
}
# Handle specific target build
elseif ($Target) {
    # Normalize target name (remove .exe, .pas, .lpr if present)
    $normalizedTarget = $Target -replace '\.(exe|pas|lpr)$', ''

    if (-not $allTargets.ContainsKey($normalizedTarget)) {
        Write-Host "ERROR: Unknown target '$Target'" -ForegroundColor Red
        Write-Host ""
        Write-Host "Available targets:" -ForegroundColor Yellow
        foreach ($key in $allTargets.Keys | Sort-Object) {
            Write-Host "  - $key" -ForegroundColor Gray
        }
        Write-Host ""
        Write-Host "Or use -Source and -Dest for custom builds." -ForegroundColor Gray
        exit 1
    }

    $targetInfo = $allTargets[$normalizedTarget]
    Write-Host "Building Target: $normalizedTarget" -ForegroundColor Cyan
    Write-Host "================================" -ForegroundColor Cyan

    $binPath = Join-Path $BinDir $platformDir
    $outputPath = Join-Path $binPath $targetInfo.Output

    $result = Build-Target -SourceFile $targetInfo.Source -SourceDir $targetInfo.SourceDir `
        -OutputPath $outputPath -FPC $fpc -PlatformDir $platformDir `
        -TargetCPU $CPU -TargetOS $OS -IsDebug $Debug `
        -UseAvxCp $useAvxCp -UseAvxOp $useAvxOp -UseAvxCf $useAvxCf

    if ($result) { $success++ } else { $failed++ }
    Write-Host ""
}
elseif ($LibOnly) {
    # LibOnly - just compile library units by building audiotest (which pulls in all units)
    Write-Host "Building Library..." -ForegroundColor Cyan
    Write-Host "===================" -ForegroundColor Cyan

    $targetInfo = $allTargets['audiotest']
    $binPath = Join-Path $BinDir $platformDir
    $outputPath = Join-Path $binPath $targetInfo.Output

    $result = Build-Target -SourceFile $targetInfo.Source -SourceDir $targetInfo.SourceDir `
        -OutputPath $outputPath -FPC $fpc -PlatformDir $platformDir `
        -TargetCPU $CPU -TargetOS $OS -IsDebug $Debug `
        -UseAvxCp $useAvxCp -UseAvxOp $useAvxOp -UseAvxCf $useAvxCf

    if ($result) { $success++ } else { $failed++ }
    Write-Host ""
}
else {
    # Decide which KINDs of target to build:
    #   tools  -> always (unless -TestOnly)
    #   tests  -> only with -Tests or -TestOnly
    #   demos  -> -Demos forces on, -SkipDemos forces off; with NO parameters at
    #            all we ask interactively; with any other parameter, off.
    $buildTools = -not $TestOnly
    $buildTests = ($Tests -or $TestOnly)
    if     ($Demos)     { $buildDemos = $true }
    elseif ($SkipDemos) { $buildDemos = $false }
    elseif ($TestOnly)  { $buildDemos = $false }
    elseif ($PSBoundParameters.Count -eq 0) {
        # Interactive (bare invocation) only. In a non-interactive shell / CI,
        # Read-Host is unavailable -> default to skipping the demos.
        try { $ans = Read-Host "Build the demo programs (saf_play, demo_synth)? [y/N]" }
        catch { $ans = 'n' }
        $buildDemos = ($ans -match '^(y|yes)$')
    }
    else { $buildDemos = $false }

    if ($TestOnly) { Write-Host "Building Tests..." -ForegroundColor Cyan }
    else           { Write-Host "Building..." -ForegroundColor Cyan }
    Write-Host "===================" -ForegroundColor Cyan

    foreach ($key in $allTargets.Keys | Sort-Object) {
        $targetInfo = $allTargets[$key]
        switch ($targetInfo.Kind) {
            'tool'  { $doBuild = $buildTools }
            'test'  { $doBuild = $buildTests }
            'demo'  { $doBuild = $buildDemos }
            default { $doBuild = $false }
        }
        if (-not $doBuild) { continue }

        $binPath = Join-Path $BinDir $platformDir
        $outputPath = Join-Path $binPath $targetInfo.Output

        $result = Build-Target -SourceFile $targetInfo.Source -SourceDir $targetInfo.SourceDir `
            -OutputPath $outputPath -FPC $fpc -PlatformDir $platformDir `
            -TargetCPU $CPU -TargetOS $OS -IsDebug $Debug `
            -UseAvxCp $useAvxCp -UseAvxOp $useAvxOp -UseAvxCf $useAvxCf

        if ($result) { $success++ } else { $failed++ }
    }
    Write-Host ""
}

# Summary
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "Build Summary" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  Successful: $success" -ForegroundColor Green
if ($failed -gt 0) {
    Write-Host "  Failed: $failed" -ForegroundColor Red
}
Write-Host ""

# List built executables
$binPath = Join-Path $BinDir $platformDir
if (Test-Path $binPath) {
    $exes = Get-ChildItem -Path $binPath -Filter "*.exe" -ErrorAction SilentlyContinue
    if ($exes) {
        Write-Host "Built executables:" -ForegroundColor Gray
        foreach ($exe in $exes) {
            Write-Host "  $($exe.FullName)" -ForegroundColor DarkGray
        }
        Write-Host ""
    }
}

if ($failed -eq 0) {
    Write-Host "Build completed successfully!" -ForegroundColor Green
}
else {
    Write-Host "Build completed with errors." -ForegroundColor Yellow
}

exit $failed
