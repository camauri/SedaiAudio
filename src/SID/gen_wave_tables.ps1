$basePath = 'C:\Progetti\Artiforge\SedaiAudioFoundation\temp\src\resid'
$outPath = 'C:\Progetti\Artiforge\SedaiAudioFoundation\src\SID\SedaiSIDWaveTables.inc'

$output = @()
$output += '{ Combined waveform lookup tables from ReSID }'
$output += '{ Each table contains 4096 bytes sampled from real SID chips }'
$output += ''

$tables = @(
    @{ File = 'wave6581__ST.cpp'; Name = 'Wave6581_ST'; Desc = '6581 Sawtooth+Triangle' },
    @{ File = 'wave6581_P_T.cpp'; Name = 'Wave6581_PT'; Desc = '6581 Pulse+Triangle' },
    @{ File = 'wave6581_PS_.cpp'; Name = 'Wave6581_PS'; Desc = '6581 Pulse+Sawtooth' },
    @{ File = 'wave6581_PST.cpp'; Name = 'Wave6581_PST'; Desc = '6581 Pulse+Sawtooth+Triangle' },
    @{ File = 'wave8580__ST.cpp'; Name = 'Wave8580_ST'; Desc = '8580 Sawtooth+Triangle' },
    @{ File = 'wave8580_P_T.cpp'; Name = 'Wave8580_PT'; Desc = '8580 Pulse+Triangle' },
    @{ File = 'wave8580_PS_.cpp'; Name = 'Wave8580_PS'; Desc = '8580 Pulse+Sawtooth' },
    @{ File = 'wave8580_PST.cpp'; Name = 'Wave8580_PST'; Desc = '8580 Pulse+Sawtooth+Triangle' }
)

foreach ($table in $tables) {
    $filePath = Join-Path $basePath $table.File
    $content = Get-Content -Path $filePath -Raw

    # Remove all comments (both /* */ style)
    $content = $content -replace '/\*[^*]*\*/', ''

    # Extract hex values (only 0x## format, NOT 0x### addresses)
    $matches = [regex]::Matches($content, '(?<![0-9a-fA-Fx])0x([0-9a-fA-F]{2})(?![0-9a-fA-F])')
    $hexValues = @()
    foreach ($m in $matches) {
        $hexValues += '$' + $m.Groups[1].Value.ToUpper()
    }

    # Take first 4096 values
    if ($hexValues.Count -gt 4096) {
        $hexValues = $hexValues[0..4095]
    }

    Write-Host "Processing $($table.Name): $($hexValues.Count) values"

    $output += '{ ' + $table.Desc + ' }'
    $output += $table.Name + ': array[0..4095] of Byte = ('

    # Output 16 values per line
    for ($i = 0; $i -lt 4096; $i += 16) {
        $lineEnd = [Math]::Min($i + 15, 4095)
        $lineVals = $hexValues[$i..$lineEnd] -join ', '
        if ($lineEnd -lt 4095) {
            $output += '  ' + $lineVals + ','
        } else {
            $output += '  ' + $lineVals
        }
    }

    $output += ');'
    $output += ''
}

$output | Out-File -FilePath $outPath -Encoding ASCII
Write-Host "Created: $outPath"
(Get-Item $outPath).Length
