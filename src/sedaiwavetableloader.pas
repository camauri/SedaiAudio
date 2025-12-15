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

unit SedaiWavetableLoader;

{$mode objfpc}{$H+}
{$I DebugFlag.inc}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes;

type
  { TSedaiWavetableLoader }

  TSedaiWavetableLoader = class
  private
    class var FLoadedWavetables: TStringList; // Cache loaded wavetables

  public
    // Initialization
    class procedure Initialize;
    class procedure Finalize;

    // Format detection - uses types from SedaiAudioTypes
    class function DetectFormat(const AFilename: string): TWavetableFormat;
    class function GetWavetableInfo(const AFilename: string): TWavetableInfo;

    // Universal loader - auto-detect format
    class function LoadWavetable(const AFilename: string): TWavetable;
    class function LoadWavetableWithInfo(const AFilename: string; out AInfo: TWavetableInfo): TWavetable;

    // Format-specific loaders
    class function LoadSerumWavetable(const AFilename: string): TWavetable;
    class function LoadVitalWavetable(const AFilename: string): TWavetable;
    class function LoadSurgeWavetable(const AFilename: string): TWavetable;
    class function LoadGenericWave(const AFilename: string): TWavetable;

    // Directory operations
    class function ScanWavetableDirectory(const APath: string): TStringArray;
    class function LoadWavetableDirectory(const APath: string): TStringArray; // Returns names of loaded wavetables
    class function FindWavetableByName(const AName: string): TWavetable;

    // Cache management
    class procedure ClearCache;
    class function GetCacheInfo: string;
    class function IsCached(const AName: string): Boolean;

    // Utility functions - FIXED IMPLEMENTATIONS
    class function FormatToString(AFormat: TWavetableFormat): string;
    class function GetSupportedExtensions: TStringArray;
    class function IsWavetableFile(const AFilename: string): Boolean;

    // Validation
    class function ValidateWavetableFile(const AFilename: string): Boolean;
    class function ValidateWavetableData(const AWavetable: TWavetable): Boolean;
  end;

implementation

// Global cache for loaded wavetables
var
  GWavetableCache: array of record
    Name: string;
    Wavetable: TWavetable;
    LastAccess: TDateTime;
  end;

// Initialize the wavetable loader
class procedure TSedaiWavetableLoader.Initialize;
begin
  if not Assigned(FLoadedWavetables) then
    FLoadedWavetables := TStringList.Create;
  FLoadedWavetables.Clear;

  SetLength(GWavetableCache, 0);
  WriteLn('SEDAI Wavetable Loader initialized');
  WriteLn('Supported formats: Serum, Vital, Surge, Generic WAV');
end;

class procedure TSedaiWavetableLoader.Finalize;
var
  i: Integer;
begin
  // Clear cache and free memory
  for i := 0 to Length(GWavetableCache) - 1 do
  begin
    if GWavetableCache[i].Wavetable.IsLoaded then
      SetLength(GWavetableCache[i].Wavetable.Samples, 0, 0);
  end;
  SetLength(GWavetableCache, 0);

  FreeAndNil(FLoadedWavetables);
end;

// Detect wavetable format from file extension and content
class function TSedaiWavetableLoader.DetectFormat(const AFilename: string): TWavetableFormat;
var
  AExt: string;
  AFile: TFileStream;
  AHeader: array[0..11] of Byte;
  AHeaderStr: string;
begin
  Result := wtfUnknown;

  if not FileExists(AFilename) then Exit;

  AExt := LowerCase(ExtractFileExt(AFilename));

  // Check by extension first
  if AExt = '.wt' then
  begin
    Result := wtfSurge;
    Exit;
  end
  else if AExt = '.wav' then
  begin
    // Need to check content to distinguish Serum/Vital from generic WAV
    try
      AFile := TFileStream.Create(AFilename, fmOpenRead);
      try
        if AFile.Size > 12 then
        begin
          AFile.ReadBuffer(AHeader, 12);
          AHeaderStr := '';
          SetLength(AHeaderStr, 4);
          Move(AHeader[0], AHeaderStr[1], 4);

          if AHeaderStr = 'RIFF' then
          begin
            // Check if it's a wavetable-sized WAV (multiple of 2048 samples)
            if ((AFile.Size - 44) mod (2048 * 4)) = 0 then // 32-bit samples
              Result := wtfSerum
            else if ((AFile.Size - 44) mod (2048 * 2)) = 0 then // 16-bit samples
              Result := wtfVital
            else
              Result := wtfGeneric;
          end;
        end;
      finally
        AFile.Free;
      end;
    except
      Result := wtfGeneric;
    end;
  end
  else if AExt = '.flac' then
  begin
    Result := wtfGeneric; // Treat FLAC as generic for now
  end;
end;

// Get detailed wavetable information
class function TSedaiWavetableLoader.GetWavetableInfo(const AFilename: string): TWavetableInfo;
var
  AFile: TFileStream;
begin
  FillChar(Result, SizeOf(TWavetableInfo), 0);
  Result.Name := ChangeFileExt(ExtractFileName(AFilename), '');
  Result.Format := DetectFormat(AFilename);
  Result.IsValid := FileExists(AFilename);

  if Result.IsValid then
  begin
    try
      AFile := TFileStream.Create(AFilename, fmOpenRead);
      try
        Result.FileSize := AFile.Size;
      finally
        AFile.Free;
      end;
    except
      Result.FileSize := 0;
    end;

    case Result.Format of
      wtfSerum, wtfVital:
        begin
          // Calculate frame count for WAV files
          Result.FrameCount := (Result.FileSize - 44) div (2048 * 4); // Assume 32-bit
          if Result.FrameCount <= 0 then
            Result.FrameCount := (Result.FileSize - 44) div (2048 * 2); // Try 16-bit
          Result.SampleRate := 44100; // Default
        end;
      wtfSurge:
        begin
          Result.FrameCount := 64; // Surge standard
          Result.SampleRate := 44100;
        end;
      else
        begin
          Result.FrameCount := 1;
          Result.SampleRate := 44100;
        end;
    end;
  end;
end;

// Universal wavetable loader with auto-detection
class function TSedaiWavetableLoader.LoadWavetable(const AFilename: string): TWavetable;
var
  AInfo: TWavetableInfo;
begin
  Result := LoadWavetableWithInfo(AFilename, AInfo);
end;

class function TSedaiWavetableLoader.LoadWavetableWithInfo(const AFilename: string; out AInfo: TWavetableInfo): TWavetable;
begin
  FillChar(Result, SizeOf(TWavetable), 0);
  AInfo := GetWavetableInfo(AFilename);

  if not AInfo.IsValid then
  begin
    WriteLn('ERROR: Invalid wavetable file: ', AFilename);
    Exit;
  end;

  WriteLn('Loading wavetable: ', AInfo.Name, ' (', FormatToString(AInfo.Format), ')');

  case AInfo.Format of
    wtfSerum:  Result := LoadSerumWavetable(AFilename);
    wtfVital:  Result := LoadVitalWavetable(AFilename);
    wtfSurge:  Result := LoadSurgeWavetable(AFilename);
    wtfGeneric: Result := LoadGenericWave(AFilename);
  else
    WriteLn('WARNING: Unknown format, attempting generic load');
    Result := LoadGenericWave(AFilename);
  end;

  if Result.IsLoaded then
  begin
    WriteLn('  Loaded: ', Result.WaveCount, ' waves, ', Result.SampleLength, ' samples each');

    // Add to cache if not already there
    if not IsCached(Result.Name) then
    begin
      SetLength(GWavetableCache, Length(GWavetableCache) + 1);
      with GWavetableCache[High(GWavetableCache)] do
      begin
        Name := Result.Name;
        Wavetable := Result;
        LastAccess := Now;
      end;
    end;
  end
  else
    WriteLn('ERROR: Failed to load wavetable: ', AFilename);
end;

// Load Serum format wavetables (.wav with 2048-sample frames)
class function TSedaiWavetableLoader.LoadSerumWavetable(const AFilename: string): TWavetable;
var
  AFile: TFileStream;
  AWavHeader: array[0..43] of Byte;
  ASampleCount, AFrameCount: Integer;
  ASamples: array of SmallInt;
  i, j, ASampleIndex: Integer;
begin
  FillChar(Result, SizeOf(TWavetable), 0);
  Result.Name := ChangeFileExt(ExtractFileName(AFilename), '');

  try
    AFile := TFileStream.Create(AFilename, fmOpenRead);
    try
      // Read WAV header (simplified - assumes standard 44-byte header)
      if AFile.Size < 44 then
      begin
        WriteLn('ERROR: WAV file too small: ', AFilename);
        Exit;
      end;

      AFile.ReadBuffer(AWavHeader, 44);
      ASampleCount := (AFile.Size - 44) div 2; // Assume 16-bit samples
      AFrameCount := ASampleCount div 2048;

      if AFrameCount <= 0 then
      begin
        WriteLn('ERROR: Invalid Serum wavetable size: ', AFilename);
        Exit;
      end;

      Result.WaveCount := AFrameCount;
      Result.SampleLength := 2048;
      SetLength(Result.Samples, Result.WaveCount, Result.SampleLength);

      // Read sample data
      SetLength(ASamples, ASampleCount);
      AFile.ReadBuffer(ASamples[0], ASampleCount * 2);

      // Convert to wavetable format
      for i := 0 to Result.WaveCount - 1 do
      begin
        for j := 0 to Result.SampleLength - 1 do
        begin
          ASampleIndex := (i * 2048) + j;
          if ASampleIndex < ASampleCount then
            Result.Samples[i][j] := ASamples[ASampleIndex] / 32768.0
          else
            Result.Samples[i][j] := 0.0;
        end;
      end;

      Result.IsLoaded := True;
      WriteLn('  Serum wavetable loaded: ', Result.WaveCount, ' frames');

    finally
      AFile.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR loading Serum wavetable: ', E.Message);
      Result.IsLoaded := False;
    end;
  end;
end;

// Load Vital format wavetables (Serum-compatible)
class function TSedaiWavetableLoader.LoadVitalWavetable(const AFilename: string): TWavetable;
begin
  // Vital uses same format as Serum
  Result := LoadSerumWavetable(AFilename);
  if Result.IsLoaded then
    WriteLn('  Vital wavetable loaded (Serum-compatible): ', Result.Name);
end;

// Load Surge .wt format wavetables
class function TSedaiWavetableLoader.LoadSurgeWavetable(const AFilename: string): TWavetable;
var
  AFile: TFileStream;
  AHeader: array[0..7] of Byte;
  AWaveCount, ASampleLength: Integer;
  ASamples: array of Single;
  i, j, ASampleIndex: Integer;
begin
  // Surge .wt format: 8-byte header + float32 samples
  FillChar(Result, SizeOf(TWavetable), 0);
  Result.Name := ChangeFileExt(ExtractFileName(AFilename), '');

  try
    AFile := TFileStream.Create(AFilename, fmOpenRead);
    try
      if AFile.Size < 8 then
      begin
        WriteLn('ERROR: Surge .wt file too small: ', AFilename);
        Exit;
      end;

      // Read header (simplified - actual Surge format may vary)
      AFile.ReadBuffer(AHeader, 8);

      // Assume standard format: 2048 samples per frame
      ASampleLength := 2048;
      AWaveCount := (AFile.Size - 8) div (ASampleLength * 4); // 32-bit float

      if AWaveCount <= 0 then
      begin
        WriteLn('ERROR: Invalid Surge wavetable size: ', AFilename);
        Exit;
      end;

      Result.WaveCount := AWaveCount;
      Result.SampleLength := ASampleLength;
      SetLength(Result.Samples, Result.WaveCount, Result.SampleLength);

      // Read sample data as float32
      SetLength(ASamples, AWaveCount * ASampleLength);
      AFile.ReadBuffer(ASamples[0], AWaveCount * ASampleLength * 4);

      // Convert to wavetable format
      for i := 0 to Result.WaveCount - 1 do
      begin
        for j := 0 to Result.SampleLength - 1 do
        begin
          ASampleIndex := (i * ASampleLength) + j;
          if ASampleIndex < Length(ASamples) then
            Result.Samples[i][j] := ASamples[ASampleIndex]
          else
            Result.Samples[i][j] := 0.0;
        end;
      end;

      Result.IsLoaded := True;
      WriteLn('  Surge wavetable loaded: ', Result.WaveCount, ' frames');

    finally
      AFile.Free;
    end;
  except
    on E: Exception do
    begin
      WriteLn('ERROR loading Surge wavetable: ', E.Message);
      Result.IsLoaded := False;
    end;
  end;
end;

// Load generic WAV file as single-wave wavetable
class function TSedaiWavetableLoader.LoadGenericWave(const AFilename: string): TWavetable;
var
  i: Integer;
  APhase: Single;
begin
  // Simplified generic WAV loader
  FillChar(Result, SizeOf(TWavetable), 0);
  Result.Name := ChangeFileExt(ExtractFileName(AFilename), '');
  Result.WaveCount := 1;
  Result.SampleLength := 2048;

  SetLength(Result.Samples, 1, 2048);

  // Generate a simple sine wave for demo
  for i := 0 to 2047 do
  begin
    APhase := (i / 2048.0) * 2.0 * Pi;
    Result.Samples[0][i] := Sin(APhase);
  end;

  Result.IsLoaded := True;
  WriteLn('  Generic WAV loaded (simulated): ', Result.Name);
end;

// Scan directory for wavetable files - FIXED VERSION
class function TSedaiWavetableLoader.ScanWavetableDirectory(const APath: string): TStringArray;
var
  ASearchRec: TSearchRec;
  AFiles: TStringList;
  AFullPath: string;
  i: Integer;
begin
  AFiles := TStringList.Create;
  try
    //WriteLn('DEBUG: Scanning directory: ', APath);

    if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.*', faAnyFile, ASearchRec) = 0 then
    begin
      repeat
        if (ASearchRec.Attr and faDirectory) = 0 then
        begin
          AFullPath := IncludeTrailingPathDelimiter(APath) + ASearchRec.Name;
          //WriteLn('DEBUG: Found file: ', ASearchRec.Name, ' -> checking if wavetable...');

          if IsWavetableFile(AFullPath) then
          begin
            //WriteLn('DEBUG: YES - Adding to list: ', AFullPath);
            AFiles.Add(AFullPath);
          end
          else
            //WriteLn('DEBUG: NO - Not a wavetable file');
        end;
      until FindNext(ASearchRec) <> 0;
      FindClose(ASearchRec);
    end
    else
      //WriteLn('DEBUG: FindFirst failed for path: ', APath);

    WriteLn('Found ', AFiles.Count, ' wavetable files:');
    for i := 0 to AFiles.Count - 1 do
      WriteLn('  ', ExtractFileName(AFiles[i]));

    // Convert to array
    SetLength(Result, AFiles.Count);
    for i := 0 to AFiles.Count - 1 do
      Result[i] := AFiles[i];

  finally
    AFiles.Free;
  end;
end;

// Load all wavetables from directory
class function TSedaiWavetableLoader.LoadWavetableDirectory(const APath: string): TStringArray;
var
  AFiles: TStringArray;
  ALoadedNames: TStringList;
  i: Integer;
  AWavetable: TWavetable;
begin
  WriteLn('Scanning wavetable directory: ', APath);
  AFiles := ScanWavetableDirectory(APath);

  ALoadedNames := TStringList.Create;
  try
    for i := 0 to Length(AFiles) - 1 do
    begin
      AWavetable := LoadWavetable(AFiles[i]);
      if AWavetable.IsLoaded then
        ALoadedNames.Add(AWavetable.Name);
    end;

    WriteLn('Loaded ', ALoadedNames.Count, ' wavetables from directory');

    // Convert to array
    SetLength(Result, ALoadedNames.Count);
    for i := 0 to ALoadedNames.Count - 1 do
      Result[i] := ALoadedNames[i];

  finally
    ALoadedNames.Free;
  end;
end;

// Find wavetable by name in cache
class function TSedaiWavetableLoader.FindWavetableByName(const AName: string): TWavetable;
var
  i: Integer;
begin
  FillChar(Result, SizeOf(TWavetable), 0);

  for i := 0 to Length(GWavetableCache) - 1 do
  begin
    if SameText(GWavetableCache[i].Name, AName) then
    begin
      Result := GWavetableCache[i].Wavetable;
      GWavetableCache[i].LastAccess := Now; // Update access time
      Exit;
    end;
  end;

  WriteLn('WARNING: Wavetable not found in cache: ', AName);
end;

// Cache management
class procedure TSedaiWavetableLoader.ClearCache;
var
  i: Integer;
begin
  for i := 0 to Length(GWavetableCache) - 1 do
  begin
    if GWavetableCache[i].Wavetable.IsLoaded then
      SetLength(GWavetableCache[i].Wavetable.Samples, 0, 0);
  end;
  SetLength(GWavetableCache, 0);
  WriteLn('Wavetable cache cleared');
end;

class function TSedaiWavetableLoader.GetCacheInfo: string;
var
  i, ATotalWaves: Integer;
begin
  ATotalWaves := 0;
  for i := 0 to Length(GWavetableCache) - 1 do
    ATotalWaves := ATotalWaves + GWavetableCache[i].Wavetable.WaveCount;

  Result := Format('Cache: %d wavetables, %d total waves',
    [Length(GWavetableCache), ATotalWaves]);
end;

class function TSedaiWavetableLoader.IsCached(const AName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(GWavetableCache) - 1 do
  begin
    if SameText(GWavetableCache[i].Name, AName) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// FIXED: Utility functions - proper implementations
class function TSedaiWavetableLoader.FormatToString(AFormat: TWavetableFormat): string;
begin
  case AFormat of
    wtfSerum: Result := 'Serum';
    wtfVital: Result := 'Vital';
    wtfSurge: Result := 'Surge';
    wtfGeneric: Result := 'Generic WAV';
  else
    Result := 'Unknown';
  end;
end;

class function TSedaiWavetableLoader.GetSupportedExtensions: TStringArray;
begin
  SetLength(Result, 3);
  Result[0] := '.wav';
  Result[1] := '.wt';
  Result[2] := '.flac';
end;

// FIXED: IsWavetableFile - proper implementation without calling missing function
class function TSedaiWavetableLoader.IsWavetableFile(const AFilename: string): Boolean;
var
  AExt: string;
  AExts: TStringArray;
  i: Integer;
begin
  Result := False;
  AExt := LowerCase(ExtractFileExt(AFilename));
  AExts := GetSupportedExtensions;

  //WriteLn('DEBUG: Checking extension: "', AExt, '"');

  for i := 0 to Length(AExts) - 1 do
  begin
    //WriteLn('DEBUG: Comparing with: "', AExts[i], '"');
    if AExt = AExts[i] then
    begin
      //WriteLn('DEBUG: MATCH FOUND!');
      Result := True;
      Exit;
    end;
  end;

  //WriteLn('DEBUG: No match found for extension: ', AExt);
end;

class function TSedaiWavetableLoader.ValidateWavetableFile(const AFilename: string): Boolean;
var
  AFormat: TWavetableFormat;
begin
  Result := FileExists(AFilename) and IsWavetableFile(AFilename);
  if Result then
  begin
    AFormat := DetectFormat(AFilename);
    Result := AFormat <> wtfUnknown;
  end;
end;

class function TSedaiWavetableLoader.ValidateWavetableData(const AWavetable: TWavetable): Boolean;
begin
  Result := AWavetable.IsLoaded and
            (AWavetable.WaveCount > 0) and
            (AWavetable.SampleLength > 0) and
            (Length(AWavetable.Samples) = AWavetable.WaveCount);
end;

// Initialization/Finalization
initialization
  TSedaiWavetableLoader.Initialize;

finalization
  TSedaiWavetableLoader.Finalize;

end.
