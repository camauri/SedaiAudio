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
    Released under GNU GPL v3 or Commercial License
#>

param(
    [switch]$Help,
    [switch]$LibOnly,
    [switch]$TestOnly,
    [switch]$Clean,
    [switch]$CleanOnly,
    [switch]$Debug,
    [switch]$NoBanner,
    [string]$FpcPath = '',
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
    Write-Host "    -LibOnly        Build only library (no tests)"
    Write-Host "    -TestOnly       Build only test programs"
    Write-Host "    -Target <name>  Build a specific target by name"
    Write-Host "    -Clean          Clean build artifacts before building"
    Write-Host "    -CleanOnly      Only clean, do not build"
    Write-Host "    -Debug          Build with debug info instead of release"
    Write-Host "    -FpcPath <path> Path to a specific FPC compiler"
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
    Write-Host "  Copyright (c) 2025 Maurizio Cammalleri - GPL-3.0 or Commercial" -ForegroundColor Gray
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
}

# ============================================================================
# Find FPC Compiler
# ============================================================================
function Find-FPC {
    param([string]$CustomPath)

    # 1. Use custom path if specified
    if ($CustomPath -and (Test-Path $CustomPath)) {
        return $CustomPath
    }
    elseif ($CustomPath) {
        Write-Host "WARNING: Specified FPC path not found: $CustomPath" -ForegroundColor Yellow
    }

    # 2. Project-local FPC (./fpc/)
    $localFpc = Join-Path $ProjectRoot 'fpc\bin\fpc.exe'
    if (Test-Path $localFpc) {
        return $localFpc
    }

    # Also check versioned paths
    $versionedPaths = @(
        (Join-Path $ProjectRoot 'fpc\3.2.2\bin\x86_64-win64\fpc.exe'),
        (Join-Path $ProjectRoot 'fpc\3.2.2\bin\i386-win32\fpc.exe')
    )
    foreach ($vPath in $versionedPaths) {
        if (Test-Path $vPath) {
            return $vPath
        }
    }

    # 3. Check common Lazarus installations
    $lazarusPaths = @(
        'C:\lazarus-3.8\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus-3.6\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus-3.4\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus-3.2\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\FPC\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\FPC\3.2.2\bin\i386-win32\fpc.exe'
    )
    foreach ($lazPath in $lazarusPaths) {
        if (Test-Path $lazPath) {
            return $lazPath
        }
    }

    # 4. Fallback to system PATH
    $fpc = Get-Command fpc -ErrorAction SilentlyContinue
    if ($fpc) {
        return $fpc.Source
    }

    return $null
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
    Write-Host "ERROR: Free Pascal Compiler (fpc) not found!" -ForegroundColor Red
    Write-Host ""
    Write-Host "FPC was searched in:" -ForegroundColor Yellow
    Write-Host "  1. Custom path (--fpc-path parameter)" -ForegroundColor Gray
    Write-Host "  2. Project local: .\fpc\" -ForegroundColor Gray
    Write-Host "  3. Lazarus installations: C:\lazarus-*\fpc\" -ForegroundColor Gray
    Write-Host "  4. System PATH" -ForegroundColor Gray
    Write-Host ""
    Write-Host "Please install FPC or specify path with -FpcPath parameter." -ForegroundColor Yellow
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
$allTargets = @{
    'test_saf_main' = @{
        Source = 'TestSAFMain.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'TestSAFMain.exe'
        IsTest = $true
    }
    'demo_synth' = @{
        Source = 'demo_synth.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'demo_synth.exe'
        IsTest = $true
    }
    'sng_player' = @{
        Source = 'sng_player.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'sng_player.exe'
        IsTest = $true
    }
    'sng_dump' = @{
        Source = 'sng_dump.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'sng_dump.exe'
        IsTest = $true
    }
    'audiotest' = @{
        Source = 'audiotest.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'audiotest.exe'
        IsTest = $true
    }
    'sedaisid_test' = @{
        Source = 'sedaisid_test.lpr'
        SourceDir = (Join-Path $ProjectRoot 'test')
        Output = 'sedaisid_test.exe'
        IsTest = $true
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
else {
    # Build all tests (unless lib-only)
    if (-not $LibOnly) {
        Write-Host "Building Tests..." -ForegroundColor Cyan
        Write-Host "=================" -ForegroundColor Cyan

        foreach ($key in $allTargets.Keys | Sort-Object) {
            $targetInfo = $allTargets[$key]
            if ($targetInfo.IsTest) {
                $binPath = Join-Path $BinDir $platformDir
                $outputPath = Join-Path $binPath $targetInfo.Output

                $result = Build-Target -SourceFile $targetInfo.Source -SourceDir $targetInfo.SourceDir `
                    -OutputPath $outputPath -FPC $fpc -PlatformDir $platformDir `
                    -TargetCPU $CPU -TargetOS $OS -IsDebug $Debug `
                    -UseAvxCp $useAvxCp -UseAvxOp $useAvxOp -UseAvxCf $useAvxCf

                if ($result) { $success++ } else { $failed++ }
            }
        }
        Write-Host ""
    }
    else {
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
