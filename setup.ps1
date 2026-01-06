<#
.SYNOPSIS
    SedaiAudio Foundation Setup Script for Windows

.DESCRIPTION
    This script sets up the development environment for SedaiAudio Foundation:
    - Creates required directory structure (bin, lib, deps)
    - Downloads and installs Free Pascal Compiler (if not present)
    - Downloads SDL2 Pascal bindings (required for compilation)
    - Downloads SDL2 runtime DLLs (required for execution)

    Copyright (c) 2025 Maurizio Cammalleri
    Released under GNU GPL v3 or Commercial License

.PARAMETER Help
    Show help message

.PARAMETER SkipFpc
    Skip FPC installation (use if you already have FPC installed)

.PARAMETER ForceFpc
    Force reinstallation of FPC even if already present

.PARAMETER ForceSDL2
    Force reinstallation of SDL2 bindings even if already present

.PARAMETER ForceRuntime
    Force reinstallation of SDL2 runtime DLLs even if already present

.PARAMETER SkipSDL2
    Skip SDL2 bindings installation (not recommended)

.PARAMETER SkipRuntime
    Skip SDL2 runtime installation (not recommended)

.PARAMETER Clean
    Clean existing directories before setup

.EXAMPLE
    .\setup.ps1                    # Full setup with FPC (recommended)
    .\setup.ps1 -SkipFpc           # Setup without FPC (if already installed)
    .\setup.ps1 -ForceFpc          # Force reinstall FPC
    .\setup.ps1 -ForceSDL2         # Force reinstall SDL2 bindings
    .\setup.ps1 -ForceRuntime      # Force reinstall SDL2 runtime
    .\setup.ps1 -Clean             # Clean and reinstall everything
#>

param(
    [switch]$Help,
    [switch]$SkipFpc,
    [switch]$ForceFpc,
    [switch]$ForceSDL2,
    [switch]$ForceRuntime,
    [switch]$SkipSDL2,
    [switch]$SkipRuntime,
    [switch]$Clean
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
    Write-Host "                   SETUP SCRIPT" -ForegroundColor Yellow
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
    Write-Host "SedaiAudio Foundation Setup Script" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "USAGE:" -ForegroundColor Yellow
    Write-Host "    .\setup.ps1 [options]"
    Write-Host ""
    Write-Host "OPTIONS:" -ForegroundColor Yellow
    Write-Host "    -Help           Show this help message"
    Write-Host "    -SkipFpc        Skip FPC installation (if already installed)"
    Write-Host "    -ForceFpc       Force reinstallation of FPC"
    Write-Host "    -ForceSDL2      Force reinstallation of SDL2 bindings"
    Write-Host "    -ForceRuntime   Force reinstallation of SDL2 runtime DLLs"
    Write-Host "    -SkipSDL2       Skip SDL2 bindings installation (not recommended)"
    Write-Host "    -SkipRuntime    Skip SDL2 runtime installation (not recommended)"
    Write-Host "    -Clean          Clean directories before setup"
    Write-Host ""
    Write-Host "EXAMPLES:" -ForegroundColor Yellow
    Write-Host "    .\setup.ps1                    # Full setup with FPC (recommended)"
    Write-Host "    .\setup.ps1 -SkipFpc           # Setup without FPC installation"
    Write-Host "    .\setup.ps1 -ForceFpc          # Force reinstall FPC"
    Write-Host "    .\setup.ps1 -ForceSDL2         # Force reinstall SDL2 bindings"
    Write-Host "    .\setup.ps1 -ForceRuntime      # Force reinstall SDL2 DLLs"
    Write-Host "    .\setup.ps1 -Clean             # Clean setup with all components"
    Write-Host ""
    Write-Host "NOTES:" -ForegroundColor Yellow
    Write-Host "    - FPC is installed automatically (use -SkipFpc to skip)"
    Write-Host "    - SDL2 Pascal bindings are required for compilation"
    Write-Host "    - SDL2 runtime DLLs are required for execution"
    Write-Host "    - After setup, run .\build.ps1 to compile the library"
    Write-Host ""
    exit 0
}

$ErrorActionPreference = 'Stop'

# ============================================================================
#  IMPORT UTILITIES
# ============================================================================

$Script:ProjectRoot = $PSScriptRoot
$Script:UtilsPath = Join-Path $ProjectRoot "scripts\lib\download-utils.ps1"
if (Test-Path $UtilsPath) {
    . $UtilsPath
} else {
    Write-Host "ERROR: download-utils.ps1 not found at: $UtilsPath" -ForegroundColor Red
    exit 1
}

# ============================================================================
#  CONFIGURATION
# ============================================================================
$Script:FpcVersion = "3.2.2"
$Script:FpcArch = "x86_64-win64"
$Script:FpcDir = Join-Path $ProjectRoot "fpc"
$Script:FpcExe = Join-Path $FpcDir "$FpcVersion\bin\$FpcArch\fpc.exe"

$Script:SrcDir = Join-Path $ProjectRoot "src"
$Script:BinDir = Join-Path $ProjectRoot "bin\$FpcArch"
$Script:LibDir = Join-Path $ProjectRoot "lib\$FpcArch"
$Script:DepsDir = Join-Path $ProjectRoot "deps"
$Script:SDL2Dir = Join-Path $DepsDir "sdl2"
$Script:SDL2Marker = Join-Path $SDL2Dir "sdl2.pas"

# SDL2 Pascal bindings download configuration
$Script:SDL2Version = "2.3"
$Script:SDL2DownloadUrl = "https://github.com/camauri/SedaiBasic2-Deps/releases/download/SDL2-for-Pascal-v2.3/SDL2-for-Pascal-v2.3.zip"
$Script:SDL2ExpectedHash = "829dd68bebfe7756bf037160e7cc268c115976d640480d73ebb8badaa46a9e47"

# SDL2 Runtime (DLLs) download configuration
$Script:RuntimeVersion = "x86_64-win64"
$Script:RuntimeDownloadUrl = "https://github.com/camauri/SedaiBasic2-Deps/releases/download/Runtime-x86_64-win64/sedai_runtime-x86_64-win64.zip"
$Script:RuntimeExpectedHash = "0da2172731dd90ca4eac0c21fc31aa6e89debb4b6e8504214c1a3ce23f025967"
$Script:RuntimeMarker = Join-Path $BinDir "SDL2.dll"

# FPC download configuration
$Script:FpcDownloadUrl = "https://sourceforge.net/projects/freepascal/files/Win32/3.2.2/fpc-3.2.2.i386-win32.exe/download"

# ============================================================================
#  DISPLAY FUNCTIONS
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
    Write-Host "                   SETUP SCRIPT" -ForegroundColor Yellow
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  Audio Foundation - Professional Audio Synthesis Library" -ForegroundColor Gray
    Write-Host "  Copyright (c) 2025 Maurizio Cammalleri - GPL-3.0 or Commercial" -ForegroundColor Gray
    Write-Host ""
    Write-Host $border -ForegroundColor Cyan
    Write-Host ""
}

function Show-Help {
    Write-Host ""
    Write-Host "SedaiAudio Foundation Setup Script" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "USAGE:" -ForegroundColor Yellow
    Write-Host "    .\setup.ps1 [options]"
    Write-Host ""
    Write-Host "OPTIONS:" -ForegroundColor Yellow
    Write-Host "    -Help           Show this help message"
    Write-Host "    -SkipFpc        Skip FPC installation (if already installed)"
    Write-Host "    -ForceFpc       Force reinstallation of FPC"
    Write-Host "    -ForceSDL2      Force reinstallation of SDL2 bindings"
    Write-Host "    -ForceRuntime   Force reinstallation of SDL2 runtime DLLs"
    Write-Host "    -SkipSDL2       Skip SDL2 bindings installation (not recommended)"
    Write-Host "    -SkipRuntime    Skip SDL2 runtime installation (not recommended)"
    Write-Host "    -Clean          Clean directories before setup"
    Write-Host ""
    Write-Host "EXAMPLES:" -ForegroundColor Yellow
    Write-Host "    .\setup.ps1                    # Full setup with FPC (recommended)"
    Write-Host "    .\setup.ps1 -SkipFpc           # Setup without FPC installation"
    Write-Host "    .\setup.ps1 -ForceFpc          # Force reinstall FPC"
    Write-Host "    .\setup.ps1 -ForceSDL2         # Force reinstall SDL2 bindings"
    Write-Host "    .\setup.ps1 -ForceRuntime      # Force reinstall SDL2 DLLs"
    Write-Host "    .\setup.ps1 -Clean             # Clean setup with all components"
    Write-Host ""
    Write-Host "NOTES:" -ForegroundColor Yellow
    Write-Host "    - FPC is installed automatically (use -SkipFpc to skip)"
    Write-Host "    - SDL2 Pascal bindings are required for compilation"
    Write-Host "    - SDL2 runtime DLLs are required for execution"
    Write-Host "    - After setup, run .\build.ps1 to compile the library"
    Write-Host ""
}

function Show-Step {
    param(
        [int]$Number,
        [int]$Total,
        [string]$Title
    )
    Write-Host ""
    Write-Host ("  [{0}/{1}] {2}" -f $Number, $Total, $Title) -ForegroundColor Cyan
    Write-Host ("  " + "-" * 60) -ForegroundColor DarkGray
}

function Show-Status {
    param(
        [string]$Message,
        [string]$Type = "Info"
    )
    $prefix = "      "
    switch ($Type) {
        "Success" {
            Write-Host "$prefix[OK] " -ForegroundColor Green -NoNewline
            Write-Host $Message
        }
        "Error" {
            Write-Host "$prefix[ERROR] " -ForegroundColor Red -NoNewline
            Write-Host $Message
        }
        "Warning" {
            Write-Host "$prefix[!] " -ForegroundColor Yellow -NoNewline
            Write-Host $Message
        }
        "Skip" {
            Write-Host "$prefix[SKIP] " -ForegroundColor DarkYellow -NoNewline
            Write-Host $Message
        }
        default {
            Write-Host "$prefix" -NoNewline
            Write-Host $Message -ForegroundColor Gray
        }
    }
}

function Show-Summary {
    param(
        [bool]$Success
    )
    $border = "=" * 70

    Write-Host ""
    Write-Host $border -ForegroundColor $(if ($Success) { "Green" } else { "Red" })
    Write-Host ""

    if ($Success) {
        Write-Host "  SETUP COMPLETED SUCCESSFULLY!" -ForegroundColor Green
        Write-Host ""
        Write-Host "  Directory structure created:" -ForegroundColor White
        Write-Host "    - bin\$FpcArch\     (executables + SDL2.dll)" -ForegroundColor Gray
        Write-Host "    - lib\$FpcArch\     (compiled units)" -ForegroundColor Gray
        Write-Host "    - deps\sdl2\        (SDL2 Pascal bindings)" -ForegroundColor Gray
        Write-Host ""
        Write-Host "  Next steps:" -ForegroundColor White
        Write-Host "    1. Run .\build.ps1 to compile the library" -ForegroundColor Gray
        Write-Host "    2. Executables will be in bin\$FpcArch\" -ForegroundColor Gray
        Write-Host "    3. SDL2.dll is already in the bin folder" -ForegroundColor Gray
        Write-Host ""
    }
    else {
        Write-Host "  SETUP FAILED!" -ForegroundColor Red
        Write-Host ""
        Write-Host "  Please check the error messages above." -ForegroundColor White
    }

    Write-Host $border -ForegroundColor $(if ($Success) { "Green" } else { "Red" })
    Write-Host ""
}

# ============================================================================
#  UTILITY FUNCTIONS
# ============================================================================

function Test-DirectoryStructure {
    $dirs = @($BinDir, $LibDir, $SrcDir)
    $allExist = $true

    foreach ($dir in $dirs) {
        if (-not (Test-Path $dir)) {
            $allExist = $false
        }
    }

    return $allExist
}

function Initialize-DirectoryStructure {
    Show-Status "Checking directory structure..."

    $dirs = @(
        @{ Path = $BinDir; Name = "bin\$FpcArch" },
        @{ Path = $LibDir; Name = "lib\$FpcArch" },
        @{ Path = $DepsDir; Name = "deps" }
    )

    foreach ($dir in $dirs) {
        if (-not (Test-Path $dir.Path)) {
            New-Item -ItemType Directory -Path $dir.Path -Force | Out-Null
            Show-Status "Created: $($dir.Name)" -Type "Success"
        }
        else {
            Show-Status "Exists: $($dir.Name)" -Type "Info"
        }
    }

    # Verify src directory exists
    if (-not (Test-Path $SrcDir)) {
        Show-Status "Source directory not found: $SrcDir" -Type "Error"
        return $false
    }
    Show-Status "Source: src\" -Type "Success"

    return $true
}

function Clean-Directories {
    Show-Status "Cleaning directories..."

    if (Test-Path $BinDir) {
        Remove-Item -Path "$BinDir\*" -Recurse -Force -ErrorAction SilentlyContinue
        Show-Status "Cleaned: bin\$FpcArch"
    }

    if (Test-Path $LibDir) {
        Remove-Item -Path "$LibDir\*" -Recurse -Force -ErrorAction SilentlyContinue
        Show-Status "Cleaned: lib\$FpcArch"
    }

    if ($ForceSDL2 -and (Test-Path $SDL2Dir)) {
        Remove-Item -Path $SDL2Dir -Recurse -Force -ErrorAction SilentlyContinue
        Show-Status "Cleaned: deps\sdl2"
    }

    Show-Status "Clean complete" -Type "Success"
}

# ============================================================================
#  SDL2 INSTALLATION
# ============================================================================

function Test-SDL2Installation {
    return (Test-Path $SDL2Marker)
}


function Install-SDL2 {
    if (Test-SDL2Installation) {
        if ($ForceSDL2) {
            Show-Status "Removing existing SDL2 installation..." -Type "Warning"
            Remove-Item -Path $SDL2Dir -Recurse -Force -ErrorAction SilentlyContinue
        }
        else {
            Show-Status "SDL2 Pascal bindings already installed" -Type "Skip"
            return $true
        }
    }

    Show-Status "Installing SDL2 for Pascal v$SDL2Version..."

    # Create temp directory
    $tempDir = Join-Path $env:TEMP "sedaiaudio-sdl2-install"
    $zipFile = Join-Path $tempDir "SDL2-for-Pascal-v$SDL2Version.zip"

    if (-not (Test-Path $tempDir)) {
        New-Item -ItemType Directory -Path $tempDir -Force | Out-Null
    }

    # Download
    Show-Status "Downloading SDL2 Pascal bindings..."
    Show-Status "URL: $SDL2DownloadUrl"
    $downloadResult = Get-FileWithProgress -Url $SDL2DownloadUrl -OutFile $zipFile

    if ($downloadResult.Status -ne 0) {
        Show-Status "Download failed: $($downloadResult.Message)" -Type "Error"
        return $false
    }
    $sizeMB = [math]::Round($downloadResult.BytesDownloaded / 1MB, 2)
    Show-Status "Downloaded: $sizeMB MB" -Type "Success"

    # Verify hash
    Show-Status "Verifying file integrity..."
    $hashResult = Test-FileHash -FilePath $zipFile -ExpectedHash $SDL2ExpectedHash
    if ($hashResult.Status -ne 0) {
        Show-Status "Hash mismatch! File may be corrupted." -Type "Error"
        Show-Status $hashResult.Message
        Remove-Item $zipFile -Force -ErrorAction SilentlyContinue
        return $false
    }
    Show-Status "Hash verified" -Type "Success"

    # Extract
    Show-Status "Extracting to deps\sdl2..."
    $extractResult = Expand-ArchiveWithProgress -Path $zipFile -DestinationPath $DepsDir
    if ($extractResult.Status -ne 0) {
        Show-Status "Extraction failed: $($extractResult.Message)" -Type "Error"
        return $false
    }
    Show-Status "Extraction complete" -Type "Success"

    # Verify
    if (-not (Test-SDL2Installation)) {
        # Check if extracted to subdirectory
        $possiblePath = Get-ChildItem -Path $DepsDir -Recurse -Filter "sdl2.pas" | Select-Object -First 1
        if ($possiblePath) {
            Show-Status "SDL2 found at: $($possiblePath.DirectoryName)"
        }
        else {
            Show-Status "SDL2 verification failed - sdl2.pas not found" -Type "Error"
            return $false
        }
    }

    # Cleanup
    Remove-Item $zipFile -Force -ErrorAction SilentlyContinue
    Remove-Item $tempDir -Recurse -Force -ErrorAction SilentlyContinue

    Show-Status "SDL2 for Pascal installed successfully" -Type "Success"
    return $true
}

# ============================================================================
#  SDL2 RUNTIME INSTALLATION (DLLs)
# ============================================================================

function Test-RuntimeInstallation {
    return (Test-Path $RuntimeMarker)
}

function Install-Runtime {
    if (Test-RuntimeInstallation) {
        if ($ForceRuntime) {
            Show-Status "Removing existing runtime files..." -Type "Warning"
            # Only remove SDL2 related DLLs, not user files
            Remove-Item -Path (Join-Path $BinDir "SDL2.dll") -Force -ErrorAction SilentlyContinue
            Remove-Item -Path (Join-Path $BinDir "SDL2_ttf.dll") -Force -ErrorAction SilentlyContinue
            Remove-Item -Path (Join-Path $BinDir "zlib1.dll") -Force -ErrorAction SilentlyContinue
        }
        else {
            Show-Status "SDL2 runtime already installed" -Type "Skip"
            return $true
        }
    }

    Show-Status "Installing SDL2 runtime (DLLs)..."

    # Create temp directory
    $tempDir = Join-Path $env:TEMP "sedaiaudio-runtime-install"
    $zipFile = Join-Path $tempDir "sedai_runtime-$RuntimeVersion.zip"

    if (-not (Test-Path $tempDir)) {
        New-Item -ItemType Directory -Path $tempDir -Force | Out-Null
    }

    # Ensure bin directory exists
    if (-not (Test-Path $BinDir)) {
        New-Item -ItemType Directory -Path $BinDir -Force | Out-Null
    }

    # Download
    Show-Status "Downloading SDL2 runtime..."
    Show-Status "URL: $RuntimeDownloadUrl"
    $downloadResult = Get-FileWithProgress -Url $RuntimeDownloadUrl -OutFile $zipFile

    if ($downloadResult.Status -ne 0) {
        Show-Status "Download failed: $($downloadResult.Message)" -Type "Error"
        return $false
    }
    $sizeMB = [math]::Round($downloadResult.BytesDownloaded / 1MB, 2)
    Show-Status "Downloaded: $sizeMB MB" -Type "Success"

    # Verify hash
    Show-Status "Verifying file integrity..."
    $hashResult = Test-FileHash -FilePath $zipFile -ExpectedHash $RuntimeExpectedHash
    if ($hashResult.Status -ne 0) {
        Show-Status "Hash mismatch! File may be corrupted." -Type "Error"
        Show-Status $hashResult.Message
        Remove-Item $zipFile -Force -ErrorAction SilentlyContinue
        return $false
    }
    Show-Status "Hash verified" -Type "Success"

    # Extract to bin directory (zip contains x86_64-win64 folder structure)
    Show-Status "Extracting to bin\$FpcArch..."
    $binParent = Join-Path $ProjectRoot "bin"
    $extractResult = Expand-ArchiveWithProgress -Path $zipFile -DestinationPath $binParent
    if ($extractResult.Status -ne 0) {
        Show-Status "Extraction failed: $($extractResult.Message)" -Type "Error"
        return $false
    }
    Show-Status "Extraction complete" -Type "Success"

    # Verify required files
    $requiredFiles = @("SDL2.dll")
    $missingFiles = @()
    foreach ($file in $requiredFiles) {
        $filePath = Join-Path $BinDir $file
        if (-not (Test-Path $filePath)) {
            $missingFiles += $file
        }
    }

    if ($missingFiles.Count -gt 0) {
        Show-Status "Missing files after extraction:" -Type "Error"
        foreach ($file in $missingFiles) {
            Show-Status "  - $file" -Type "Error"
        }
        return $false
    }

    # Cleanup
    Remove-Item $zipFile -Force -ErrorAction SilentlyContinue
    Remove-Item $tempDir -Recurse -Force -ErrorAction SilentlyContinue

    Show-Status "SDL2 runtime installed successfully" -Type "Success"
    return $true
}

# ============================================================================
#  FPC INSTALLATION (OPTIONAL)
# ============================================================================

function Test-FpcInstallation {
    if (-not (Test-Path $FpcExe)) {
        return $false
    }

    try {
        $version = & $FpcExe -iV 2>&1
        return $true
    }
    catch {
        return $false
    }
}

function Find-SystemFpc {
    # Check common locations
    $paths = @(
        'C:\lazarus-3.8\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus-3.6\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus-3.4\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe',
        'C:\FPC\3.2.2\bin\x86_64-win64\fpc.exe'
    )

    foreach ($path in $paths) {
        if (Test-Path $path) {
            return $path
        }
    }

    # Check PATH
    $fpc = Get-Command fpc -ErrorAction SilentlyContinue
    if ($fpc) {
        return $fpc.Source
    }

    return $null
}

function Install-Fpc {
    # Always install local FPC (ignore system FPC)
    # Use the install script
    $installScript = Join-Path $ProjectRoot "scripts\windows\install-fpc.ps1"

    if (!(Test-Path $installScript)) {
        Show-Status "FPC install script not found: $installScript" -Type "Error"
        return $false
    }

    Show-Status "Running FPC installer..."

    $params = @{}
    if ($ForceFpc) { $params["Force"] = $true }

    & $installScript @params
    $exitCode = $LASTEXITCODE

    switch ($exitCode) {
        0 {
            Show-Status "FPC installed successfully" -Type "Success"
            return $true
        }
        5 {
            Show-Status "FPC already installed" -Type "Success"
            return $true
        }
        default {
            Show-Status "FPC installation failed (exit code: $exitCode)" -Type "Error"
            return $false
        }
    }
}

# ============================================================================
#  MAIN SETUP LOGIC
# ============================================================================

function Build-Project {
    $buildScript = Join-Path $ProjectRoot "build.ps1"

    if (!(Test-Path $buildScript)) {
        Show-Status "Build script not found: $buildScript" -Type "Error"
        return $false
    }

    Show-Status "Building SedaiAudioFoundation using build.ps1..."
    Show-Status "Output: $BinDir\"

    Push-Location $ProjectRoot
    try {
        Write-Host ""

        # Use build.ps1 to compile (suppress banner as setup shows its own)
        & $buildScript -NoBanner
        $exitCode = $LASTEXITCODE

        Write-Host ""

        if ($exitCode -eq 0) {
            Show-Status "Build successful!" -Type "Success"
            return $true
        }

        Show-Status "Compilation failed (exit code: $exitCode)" -Type "Error"
        return $false

    } finally {
        Pop-Location
    }
}

function Invoke-Setup {
    Show-Banner

    # Determine steps
    $doClean = $Clean
    $doFpc = -not $SkipFpc      # FPC installed by default (use -SkipFpc to skip)
    $doSDL2 = -not $SkipSDL2
    $doRuntime = -not $SkipRuntime
    $doBuild = $true            # Always build after setup

    $totalSteps = 1  # Directory structure
    if ($doClean) { $totalSteps++ }
    if ($doFpc) { $totalSteps++ }
    if ($doSDL2) { $totalSteps++ }
    if ($doRuntime) { $totalSteps++ }
    if ($doBuild) { $totalSteps++ }

    $currentStep = 0

    # Show configuration
    Write-Host "  Configuration:" -ForegroundColor Gray
    Write-Host "    - Install FPC:           $(if ($doFpc) { 'Yes' } else { 'No (skipped)' })" -ForegroundColor Gray
    Write-Host "    - Install SDL2 bindings: $(if ($doSDL2) { 'Yes' } else { 'No (skipped)' })" -ForegroundColor Gray
    Write-Host "    - Install SDL2 runtime:  $(if ($doRuntime) { 'Yes' } else { 'No (skipped)' })" -ForegroundColor Gray
    Write-Host "    - Build:                 $(if ($doBuild) { 'Yes' } else { 'No' })" -ForegroundColor Gray
    Write-Host "    - Clean:                 $(if ($doClean) { 'Yes' } else { 'No' })" -ForegroundColor Gray
    Write-Host ""

    # Step: Clean (if requested)
    if ($doClean) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Cleaning Directories"
        Clean-Directories
    }

    # Step: Directory Structure
    $currentStep++
    Show-Step -Number $currentStep -Total $totalSteps -Title "Initializing Directory Structure"

    if (-not (Initialize-DirectoryStructure)) {
        Show-Summary -Success $false
        return 1
    }

    # Step: FPC Installation
    if ($doFpc) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Installing Free Pascal Compiler"

        if (-not (Install-Fpc)) {
            Show-Summary -Success $false
            return 1
        }
    }

    # Step: SDL2 Bindings Installation
    if ($doSDL2) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Installing SDL2 Pascal Bindings"

        if (-not (Install-SDL2)) {
            Show-Summary -Success $false
            return 1
        }
    }

    # Step: SDL2 Runtime Installation (DLLs)
    if ($doRuntime) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Installing SDL2 Runtime (DLLs)"

        if (-not (Install-Runtime)) {
            Show-Summary -Success $false
            return 1
        }
    }

    # Step: Build Project
    if ($doBuild) {
        $currentStep++
        Show-Step -Number $currentStep -Total $totalSteps -Title "Building SedaiAudioFoundation"

        if (-not (Build-Project)) {
            Show-Summary -Success $false
            return 1
        }
    }

    Show-Summary -Success $true
    return 0
}

# ============================================================================
#  ENTRY POINT
# ============================================================================

$exitCode = Invoke-Setup
exit $exitCode
