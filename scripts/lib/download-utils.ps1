# Download utilities for SedaiAudio Foundation scripts
# Common functions for downloading files with progress

function Get-FileWithProgress {
    param(
        [Parameter(Mandatory=$true)]
        [string]$Url,

        [Parameter(Mandatory=$true)]
        [string]$OutFile,

        [switch]$Quiet
    )

    $result = @{
        Status = 0
        Message = ""
        BytesDownloaded = 0
    }

    try {
        $outDir = Split-Path -Parent $OutFile
        if ($outDir -and !(Test-Path $outDir)) {
            New-Item -ItemType Directory -Path $outDir -Force | Out-Null
        }

        if (!$Quiet) {
            Write-Host "Starting download..." -ForegroundColor Cyan
        }

        $oldProgressPreference = $ProgressPreference
        if ($Quiet) {
            $ProgressPreference = 'SilentlyContinue'
        } else {
            $ProgressPreference = 'Continue'
        }

        $webRequest = [System.Net.HttpWebRequest]::Create($Url)
        $webRequest.Method = "GET"
        $webRequest.AllowAutoRedirect = $true
        $webRequest.UserAgent = "SedaiAudio-Installer/1.0"

        $response = $webRequest.GetResponse()
        $totalBytes = $response.ContentLength
        $responseStream = $response.GetResponseStream()

        $fileStream = [System.IO.File]::Create($OutFile)
        $buffer = New-Object byte[] 65536
        $bytesRead = 0
        $totalBytesRead = 0
        $lastPercentReported = -1
        $startTime = Get-Date

        if (!$Quiet -and $totalBytes -gt 0) {
            $totalMB = [math]::Round($totalBytes / 1MB, 2)
            Write-Host "File size: $totalMB MB" -ForegroundColor Gray
        }

        while (($bytesRead = $responseStream.Read($buffer, 0, $buffer.Length)) -gt 0) {
            $fileStream.Write($buffer, 0, $bytesRead)
            $totalBytesRead += $bytesRead

            if (!$Quiet -and $totalBytes -gt 0) {
                $percent = [math]::Floor(($totalBytesRead / $totalBytes) * 100)
                if ($percent -ne $lastPercentReported) {
                    $downloadedMB = [math]::Round($totalBytesRead / 1MB, 2)
                    $totalMB = [math]::Round($totalBytes / 1MB, 2)
                    $elapsed = (Get-Date) - $startTime
                    if ($elapsed.TotalSeconds -gt 0) {
                        $speedMBps = [math]::Round(($totalBytesRead / 1MB) / $elapsed.TotalSeconds, 2)
                        Write-Host "`r  Progress: $percent% ($downloadedMB / $totalMB MB) - $speedMBps MB/s   " -NoNewline -ForegroundColor Yellow
                    } else {
                        Write-Host "`r  Progress: $percent% ($downloadedMB / $totalMB MB)   " -NoNewline -ForegroundColor Yellow
                    }
                    $lastPercentReported = $percent
                }
            }
        }

        $fileStream.Close()
        $responseStream.Close()
        $response.Close()

        $ProgressPreference = $oldProgressPreference

        if (!$Quiet) {
            Write-Host ""
        }

        if (!(Test-Path $OutFile)) {
            throw "Download completed but file not found"
        }

        $fileInfo = Get-Item $OutFile
        if ($fileInfo.Length -eq 0) {
            throw "Downloaded file is empty"
        }

        $result.BytesDownloaded = $fileInfo.Length
        $result.Message = "Download completed successfully"

    } catch {
        $result.Status = 1
        $result.Message = "Download failed: $($_.Exception.Message)"

        if (Test-Path $OutFile) {
            Remove-Item $OutFile -Force -ErrorAction SilentlyContinue
        }
    }

    return $result
}

function Test-FileHash {
    param(
        [Parameter(Mandatory=$true)]
        [string]$FilePath,

        [Parameter(Mandatory=$true)]
        [string]$ExpectedHash
    )

    $result = @{
        Status = 0
        Message = ""
        ActualHash = ""
    }

    try {
        if (!(Test-Path $FilePath)) {
            $result.Status = 3
            $result.Message = "File not found: $FilePath"
            return $result
        }

        $actualHash = (Get-FileHash -Path $FilePath -Algorithm SHA256).Hash.ToLower()
        $result.ActualHash = $actualHash

        if ($actualHash -eq $ExpectedHash.ToLower()) {
            $result.Message = "Hash verification passed"
        } else {
            $result.Status = 3
            $result.Message = "Hash mismatch. Expected: $ExpectedHash, Got: $actualHash"
        }

    } catch {
        $result.Status = 3
        $result.Message = "Hash verification failed: $($_.Exception.Message)"
    }

    return $result
}

function Expand-ArchiveWithProgress {
    param(
        [Parameter(Mandatory=$true)]
        [string]$Path,

        [Parameter(Mandatory=$true)]
        [string]$DestinationPath,

        [switch]$Quiet
    )

    $result = @{
        Status = 0
        Message = ""
    }

    try {
        if (!(Test-Path $Path)) {
            throw "Archive not found: $Path"
        }

        if (!(Test-Path $DestinationPath)) {
            New-Item -ItemType Directory -Path $DestinationPath -Force | Out-Null
        }

        if (!$Quiet) {
            Write-Host "Extracting archive..." -ForegroundColor Cyan
        }

        Add-Type -AssemblyName System.IO.Compression.FileSystem

        $zip = [System.IO.Compression.ZipFile]::OpenRead($Path)
        $totalEntries = $zip.Entries.Count
        $currentEntry = 0
        $lastPercent = -1

        foreach ($entry in $zip.Entries) {
            $currentEntry++
            $percent = [math]::Floor(($currentEntry / $totalEntries) * 100)

            if (!$Quiet -and $percent -ne $lastPercent -and ($percent % 10 -eq 0 -or $percent -eq 100)) {
                Write-Host "`r  Extracting: $percent% ($currentEntry / $totalEntries files)   " -NoNewline -ForegroundColor Yellow
                $lastPercent = $percent
            }

            $entryPath = Join-Path $DestinationPath $entry.FullName
            $entryDir = Split-Path -Parent $entryPath

            if ($entry.FullName.EndsWith('/')) {
                if (!(Test-Path $entryPath)) {
                    New-Item -ItemType Directory -Path $entryPath -Force | Out-Null
                }
            } else {
                if ($entryDir -and !(Test-Path $entryDir)) {
                    New-Item -ItemType Directory -Path $entryDir -Force | Out-Null
                }
                [System.IO.Compression.ZipFileExtensions]::ExtractToFile($entry, $entryPath, $true)
            }
        }

        $zip.Dispose()

        if (!$Quiet) {
            Write-Host ""
        }

        $result.Message = "Extraction completed successfully ($totalEntries files)"

    } catch {
        $result.Status = 2
        $result.Message = "Extraction failed: $($_.Exception.Message)"
    }

    return $result
}

function Test-DiskSpace {
    param(
        [Parameter(Mandatory=$true)]
        [string]$Path,

        [Parameter(Mandatory=$true)]
        [long]$RequiredBytes
    )

    $result = @{
        Status = 0
        Message = ""
        AvailableBytes = 0
    }

    try {
        $drive = (Get-Item $Path -ErrorAction SilentlyContinue).PSDrive
        if (!$drive) {
            $driveLetter = (Split-Path -Qualifier $Path)
            $drive = Get-PSDrive -Name $driveLetter.TrimEnd(':')
        }

        $availableBytes = $drive.Free
        $result.AvailableBytes = $availableBytes

        if ($availableBytes -lt $RequiredBytes) {
            $requiredMB = [math]::Round($RequiredBytes / 1MB, 2)
            $availableMB = [math]::Round($availableBytes / 1MB, 2)
            $result.Status = 4
            $result.Message = "Insufficient disk space. Required: $requiredMB MB, Available: $availableMB MB"
        } else {
            $result.Message = "Disk space check passed"
        }

    } catch {
        $result.Message = "Could not verify disk space (assuming sufficient)"
    }

    return $result
}

function Test-InternetConnection {
    param(
        [string]$TestUrl = "https://github.com"
    )

    $result = @{
        Status = 0
        Message = ""
    }

    try {
        $request = [System.Net.WebRequest]::Create($TestUrl)
        $request.Timeout = 10000
        $request.Method = "HEAD"
        $response = $request.GetResponse()
        $response.Close()
        $result.Message = "Internet connection available"
    } catch {
        $result.Status = 1
        $result.Message = "No internet connection: $($_.Exception.Message)"
    }

    return $result
}
