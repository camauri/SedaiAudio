{*
 * Sedai Audio Foundation - Professional audio synthesis library
 * Copyright (C) 2025 Maurizio Cammalleri
 *
 * This program is dual-licensed:
 *
 * 1) For open source use: GNU General Public License version 3 (GPL-3.0-only)
 *    You may redistribute and/or modify it under the terms of the GNU GPL v3
 *    as published by the Free Software Foundation.
 *    See <https://www.gnu.org/licenses/gpl-3.0.html>
 *
 * 2) For commercial/proprietary use: A separate commercial license is required.
 *    Contact: maurizio.cammalleri@gmail.com for licensing inquiries.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * SPDX-License-Identifier: GPL-3.0-only OR Commercial
 *}

unit SedaiPlatformDetector;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, SedaiAudioTypes
  {$IFDEF UNIX}
  , Unix, BaseUnix
  {$ENDIF}
  {$IFDEF WINDOWS}
  , Windows, Registry
  {$ENDIF};

type
  TSedaiPlatformDetector = class
  public
    // Platform detection
    class function DetectPlatform: TPlatform;
    class function GetPlatformName(APlatform: TPlatform): string;
    class function GetDetailedSystemInfo: string;

    // Suggested configurations (user can override)
    class function GetSuggestedChannelConfig(APlatform: TPlatform): TAudioChannelConfig;
    class function GetSuggestedStereoConfig(APlatform: TPlatform): TStereoConfig;
    class function GetSuggestedSampleRate(APlatform: TPlatform): Cardinal;
    class function GetSuggestedBufferSize(APlatform: TPlatform): Integer;
    class function GetSuggestedConfig(APlatform: TPlatform): TAudioConfig;

    // Hardware detection
    class function DetectCPUCores: Integer;
    class function DetectRAMSize: UInt64; // in MB
    class function DetectCPUSpeed: Integer; // in MHz (approximate)
    class function IsLowPowerDevice: Boolean;
    class function HasDedicatedAudioHardware: Boolean;

    // Platform capabilities
    class function SupportsStereo(APlatform: TPlatform): Boolean;
    class function SupportsHighSampleRates(APlatform: TPlatform): Boolean;
    class function GetMaxRecommendedChannels(APlatform: TPlatform): Integer;
    class function GetMinRecommendedBufferSize(APlatform: TPlatform): Integer;

    // Helper functions
    class function ChannelConfigToCount(AConfig: TAudioChannelConfig): Integer;
    class function CountToChannelConfig(ACount: Integer): TAudioChannelConfig;
    class function ValidatePlatformConfig(const AConfig: TAudioConfig; APlatform: TPlatform): Boolean;

    // System optimization
    class function GetOptimizedConfig(APlatform: TPlatform; APerformanceLevel: Integer): TAudioConfig;
    class procedure PrintPlatformCapabilities(APlatform: TPlatform);
  end;

implementation

// Detect the current platform
class function TSedaiPlatformDetector.DetectPlatform: TPlatform;
var
  {$IFDEF LINUX}
  ACPUInfo: string;
  AFile: TextFile;
  ALine: string;
  {$ENDIF}
  {$IFDEF WINDOWS}
  ASystemInfo: TSystemInfo;
  {$ENDIF}
begin
  Result := ptUnknown;

  {$IFDEF LINUX}
  // Try to read /proc/cpuinfo on Linux
  try
    if FileExists('/proc/cpuinfo') then
    begin
      AssignFile(AFile, '/proc/cpuinfo');
      Reset(AFile);
      while not EOF(AFile) do
      begin
        ReadLn(AFile, ALine);
        if Pos('BCM2711', ALine) > 0 then
        begin
          Result := ptRaspberryPi4;
          Break;
        end
        else if Pos('BCM2712', ALine) > 0 then
        begin
          Result := ptRaspberryPi5;
          Break;
        end;
      end;
      CloseFile(AFile);
    end;

    // If still unknown, check for high-end Linux systems
    if Result = ptUnknown then
    begin
      if DetectCPUCores >= 16 then
        Result := ptWorkstation
      else if DetectCPUCores >= 4 then
        Result := ptDesktop
      else
        Result := ptDesktop; // Default for unknown Linux
    end;
  except
    Result := ptDesktop; // Safe fallback
  end;
  {$ENDIF}

  {$IFDEF WINDOWS}
  try
    GetSystemInfo(ASystemInfo);

    // Detect Windows platform based on CPU cores and features
    if ASystemInfo.dwNumberOfProcessors >= 16 then
      Result := ptWorkstation
    else if ASystemInfo.dwNumberOfProcessors >= 4 then
      Result := ptDesktop
    else
      Result := ptDesktop; // Default for Windows
  except
    Result := ptDesktop;
  end;
  {$ENDIF}

  {$IFDEF DARWIN}
  // macOS detection
  if DetectCPUCores >= 8 then
    Result := ptWorkstation
  else
    Result := ptDesktop;
  {$ENDIF}

  // Final fallback
  if Result = ptUnknown then
    Result := ptDesktop;
end;

// Get human-readable platform name
class function TSedaiPlatformDetector.GetPlatformName(APlatform: TPlatform): string;
begin
  case APlatform of
    ptRaspberryPi4: Result := 'Raspberry Pi 4';
    ptRaspberryPi5: Result := 'Raspberry Pi 5';
    ptDesktop: Result := 'Desktop Computer';
    ptWorkstation: Result := 'High-End Workstation';
  else
    Result := 'Unknown Platform';
  end;
end;

// Get detailed system information
class function TSedaiPlatformDetector.GetDetailedSystemInfo: string;
var
  APlatform: TPlatform;
begin
  APlatform := DetectPlatform;
  Result := Format('Platform: %s, CPU Cores: %d, RAM: %d MB, Low Power: %s',
    [GetPlatformName(APlatform), DetectCPUCores, DetectRAMSize, BoolToStr(IsLowPowerDevice, True)]);
end;

// Platform-specific suggested configurations
class function TSedaiPlatformDetector.GetSuggestedChannelConfig(APlatform: TPlatform): TAudioChannelConfig;
begin
  case APlatform of
    ptRaspberryPi4: Result := acc16;   // Conservative for Pi 4
    ptRaspberryPi5: Result := acc32;   // More power on Pi 5
    ptDesktop: Result := acc64;        // Desktop can handle more
    ptWorkstation: Result := acc128;   // Workstation beast mode
  else
    Result := acc32; // Safe default
  end;
end;

class function TSedaiPlatformDetector.GetSuggestedStereoConfig(APlatform: TPlatform): TStereoConfig;
begin
  case APlatform of
    ptRaspberryPi4: Result := scAutoDetect; // Let system decide
    ptRaspberryPi5: Result := scStereo;     // Pi 5 can handle stereo well
    ptDesktop: Result := scStereo;          // Desktop defaults to stereo
    ptWorkstation: Result := scStereo;      // Workstation always stereo
  else
    Result := scAutoDetect;
  end;
end;

class function TSedaiPlatformDetector.GetSuggestedSampleRate(APlatform: TPlatform): Cardinal;
begin
  case APlatform of
    ptRaspberryPi4: Result := 44100;      // Standard quality
    ptRaspberryPi5: Result := 44100;      // Standard quality
    ptDesktop: Result := 48000;           // Higher quality
    ptWorkstation: Result := 96000;       // Professional quality
  else
    Result := 44100; // Safe default
  end;
end;

class function TSedaiPlatformDetector.GetSuggestedBufferSize(APlatform: TPlatform): Integer;
begin
  case APlatform of
    ptRaspberryPi4: Result := 2048;       // Larger buffer for stability
    ptRaspberryPi5: Result := 1024;       // Better performance
    ptDesktop: Result := 1024;            // Balanced
    ptWorkstation: Result := 512;         // Low latency
  else
    Result := 1024; // Safe default
  end;
end;

// Get complete suggested configuration
class function TSedaiPlatformDetector.GetSuggestedConfig(APlatform: TPlatform): TAudioConfig;
begin
  Result.ChannelConfig := GetSuggestedChannelConfig(APlatform);
  Result.CustomChannelCount := 0;
  Result.StereoConfig := GetSuggestedStereoConfig(APlatform);
  Result.SampleRate := GetSuggestedSampleRate(APlatform);
  Result.BufferSize := GetSuggestedBufferSize(APlatform);
end;

// Hardware detection functions
class function TSedaiPlatformDetector.DetectCPUCores: Integer;
{$IFDEF WINDOWS}
var
  ASystemInfo: TSystemInfo;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  GetSystemInfo(ASystemInfo);
  Result := ASystemInfo.dwNumberOfProcessors;
  {$ELSE}
  {$IFDEF UNIX}
  Result := fpSysconf(_SC_NPROCESSORS_ONLN);
  if Result <= 0 then Result := 1;
  {$ELSE}
  Result := 1; // Fallback
  {$ENDIF}
  {$ENDIF}
end;

class function TSedaiPlatformDetector.DetectRAMSize: UInt64;
{$IFDEF LINUX}
var
  AFile: TextFile;
  ALine: string;
  APos: Integer;
  ARamKB: UInt64;
{$ENDIF}
{$IFDEF WINDOWS}
var
  AMemStatus: TMemoryStatus;
{$ENDIF}
begin
  Result := 1024; // Default 1GB fallback

  {$IFDEF LINUX}
  try
    if FileExists('/proc/meminfo') then
    begin
      AssignFile(AFile, '/proc/meminfo');
      Reset(AFile);
      ReadLn(AFile, ALine);
      CloseFile(AFile);

      // Parse "MemTotal: XXXXXX kB"
      APos := Pos(':', ALine);
      if APos > 0 then
      begin
        Delete(ALine, 1, APos);
        ALine := Trim(ALine);
        APos := Pos(' ', ALine);
        if APos > 0 then
        begin
          ALine := Copy(ALine, 1, APos - 1);
          if TryStrToUInt64(ALine, ARamKB) then
            Result := ARamKB div 1024; // Convert KB to MB
        end;
      end;
    end;
  except
    Result := 1024;
  end;
  {$ENDIF}

  {$IFDEF WINDOWS}
  try
    AMemStatus.dwLength := SizeOf(AMemStatus);
    GlobalMemoryStatus(AMemStatus);
    Result := AMemStatus.dwTotalPhys div (1024 * 1024); // Convert bytes to MB
  except
    Result := 1024;
  end;
  {$ENDIF}
end;

class function TSedaiPlatformDetector.DetectCPUSpeed: Integer;
begin
  // Simplified CPU speed detection
  // In real implementation, you'd read from system files or registry
  case DetectPlatform of
    ptRaspberryPi4: Result := 1500;      // ~1.5 GHz
    ptRaspberryPi5: Result := 2400;      // ~2.4 GHz
    ptDesktop: Result := 3000;           // ~3.0 GHz average
    ptWorkstation: Result := 4000;       // ~4.0 GHz average
  else
    Result := 2000; // 2 GHz default
  end;
end;

class function TSedaiPlatformDetector.IsLowPowerDevice: Boolean;
var
  APlatform: TPlatform;
begin
  APlatform := DetectPlatform;
  Result := APlatform in [ptRaspberryPi4, ptRaspberryPi5];
end;

class function TSedaiPlatformDetector.HasDedicatedAudioHardware: Boolean;
begin
  // Simplified detection - in real implementation, check for audio devices
  Result := not IsLowPowerDevice;
end;

// Platform capabilities
class function TSedaiPlatformDetector.SupportsStereo(APlatform: TPlatform): Boolean;
begin
  Result := True; // All modern platforms support stereo
end;

class function TSedaiPlatformDetector.SupportsHighSampleRates(APlatform: TPlatform): Boolean;
begin
  Result := APlatform in [ptDesktop, ptWorkstation];
end;

class function TSedaiPlatformDetector.GetMaxRecommendedChannels(APlatform: TPlatform): Integer;
begin
  case APlatform of
    ptRaspberryPi4: Result := 32;
    ptRaspberryPi5: Result := 64;
    ptDesktop: Result := 128;
    ptWorkstation: Result := 256;
  else
    Result := 32;
  end;
end;

class function TSedaiPlatformDetector.GetMinRecommendedBufferSize(APlatform: TPlatform): Integer;
begin
  case APlatform of
    ptRaspberryPi4: Result := 1024;
    ptRaspberryPi5: Result := 512;
    ptDesktop: Result := 512;
    ptWorkstation: Result := 256;
  else
    Result := 1024;
  end;
end;

// Helper functions
class function TSedaiPlatformDetector.ChannelConfigToCount(AConfig: TAudioChannelConfig): Integer;
begin
  Result := GetChannelConfigCount(AConfig);
end;

class function TSedaiPlatformDetector.CountToChannelConfig(ACount: Integer): TAudioChannelConfig;
begin
  Result := SedaiAudioTypes.CountToChannelConfig(ACount);
end;

// Validate if configuration is suitable for platform
class function TSedaiPlatformDetector.ValidatePlatformConfig(const AConfig: TAudioConfig; APlatform: TPlatform): Boolean;
var
  AChannelCount: Integer;
  AMaxChannels: Integer;
  AMinBufferSize: Integer;
begin
  Result := True;

  // Get actual channel count
  if AConfig.ChannelConfig = accCustom then
    AChannelCount := AConfig.CustomChannelCount
  else
    AChannelCount := GetChannelConfigCount(AConfig.ChannelConfig);

  // Check channel count limits
  AMaxChannels := GetMaxRecommendedChannels(APlatform);
  if AChannelCount > AMaxChannels then
  begin
    Result := False;
    Exit;
  end;

  // Check buffer size
  AMinBufferSize := GetMinRecommendedBufferSize(APlatform);
  if AConfig.BufferSize < AMinBufferSize then
  begin
    Result := False;
    Exit;
  end;

  // Check sample rate for low-power devices
  if IsLowPowerDevice and (AConfig.SampleRate > 48000) then
  begin
    Result := False;
    Exit;
  end;
end;

// Get optimized configuration based on performance level
class function TSedaiPlatformDetector.GetOptimizedConfig(APlatform: TPlatform; APerformanceLevel: Integer): TAudioConfig;
begin
  Result := GetSuggestedConfig(APlatform);

  case APerformanceLevel of
    0: // Minimal performance
      begin
        Result.ChannelConfig := acc8;
        Result.StereoConfig := scMono;
        Result.SampleRate := 22050;
        Result.BufferSize := 2048;
      end;
    1: // Low performance
      begin
        Result.ChannelConfig := acc16;
        Result.StereoConfig := scMono;
        Result.SampleRate := 44100;
        Result.BufferSize := 1024;
      end;
    2: // Standard performance (suggested config)
      begin
        // Use suggested config as-is
      end;
    3: // High performance
      begin
        case APlatform of
          ptDesktop:
            begin
              Result.ChannelConfig := acc128;
              Result.StereoConfig := scStereo;
              Result.SampleRate := 48000;
              Result.BufferSize := 512;
            end;
          ptWorkstation:
            begin
              Result.ChannelConfig := acc256;
              Result.StereoConfig := scStereo;
              Result.SampleRate := 96000;
              Result.BufferSize := 256;
            end;
        end;
      end;
    4: // Maximum performance
      begin
        if APlatform = ptWorkstation then
        begin
          Result.ChannelConfig := accCustom;
          Result.CustomChannelCount := 512;
          Result.StereoConfig := scStereo;
          Result.SampleRate := 192000;
          Result.BufferSize := 128;
        end;
      end;
  end;
end;

// Print detailed platform capabilities
class procedure TSedaiPlatformDetector.PrintPlatformCapabilities(APlatform: TPlatform);
var
  ASuggestedConfig: TAudioConfig;
begin
  WriteLn('=== Platform Capabilities ===');
  WriteLn('Platform: ', GetPlatformName(APlatform));
  WriteLn('CPU Cores: ', DetectCPUCores);
  WriteLn('RAM: ', DetectRAMSize, ' MB');
  WriteLn('CPU Speed: ~', DetectCPUSpeed, ' MHz');
  WriteLn('Low Power Device: ', BoolToStr(IsLowPowerDevice, True));
  WriteLn('Supports Stereo: ', BoolToStr(SupportsStereo(APlatform), True));
  WriteLn('Supports High Sample Rates: ', BoolToStr(SupportsHighSampleRates(APlatform), True));
  WriteLn('Max Recommended Channels: ', GetMaxRecommendedChannels(APlatform));
  WriteLn('Min Buffer Size: ', GetMinRecommendedBufferSize(APlatform));
  WriteLn('');

  ASuggestedConfig := GetSuggestedConfig(APlatform);
  WriteLn('Suggested Configuration:');
  WriteLn('  ', AudioConfigToString(ASuggestedConfig));
end;

end.
