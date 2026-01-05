{*
 * Sedai Audio Foundation - Wavetable Loader
 *
 * Loads wavetables from various formats: Serum, Vital, Surge, and generic WAV.
 * Supports single-cycle waveforms (AKWF) and multi-frame wavetables.
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiWavetableLoader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes;

type
  { TSedaiWavetableLoader }

  TSedaiWavetableLoader = class
  private
    class var FLoadedWavetables: TStringList;

  public
    // Initialization
    class procedure Initialize;
    class procedure Finalize;

    // Format detection
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
    class function LoadWavetableDirectory(const APath: string): TStringArray;
    class function FindWavetableByName(const AName: string): TWavetable;

    // Cache management
    class procedure ClearCache;
    class function GetCacheInfo: string;
    class function IsCached(const AName: string): Boolean;
    class function GetCacheCount: Integer;
    class function GetCachedName(AIndex: Integer): string;

    // Utility functions
    class function FormatToString(AFormat: TWavetableFormat): string;
    class function GetSupportedExtensions: TStringArray;
    class function IsWavetableFile(const AFilename: string): Boolean;

    // Validation
    class function ValidateWavetableFile(const AFilename: string): Boolean;
    class function ValidateWavetableData(const AWavetable: TWavetable): Boolean;
  end;

implementation

var
  GWavetableCache: array of record
    Name: string;
    Wavetable: TWavetable;
    LastAccess: TDateTime;
  end;

class procedure TSedaiWavetableLoader.Initialize;
begin
  if not Assigned(FLoadedWavetables) then
    FLoadedWavetables := TStringList.Create;
  FLoadedWavetables.Clear;
  SetLength(GWavetableCache, 0);
end;

class procedure TSedaiWavetableLoader.Finalize;
var
  I: Integer;
begin
  for I := 0 to Length(GWavetableCache) - 1 do
  begin
    if GWavetableCache[I].Wavetable.IsLoaded then
      SetLength(GWavetableCache[I].Wavetable.Samples, 0, 0);
  end;
  SetLength(GWavetableCache, 0);
  FreeAndNil(FLoadedWavetables);
end;

class function TSedaiWavetableLoader.DetectFormat(const AFilename: string): TWavetableFormat;
var
  AExt: string;
  AFile: TFileStream;
  AHeader: array[0..11] of Byte;
  AHeaderStr: string;
  ASampleCount: Integer;
begin
  Result := wtfUnknown;

  if not FileExists(AFilename) then Exit;

  AExt := LowerCase(ExtractFileExt(AFilename));

  if AExt = '.wt' then
  begin
    Result := wtfSurge;
    Exit;
  end
  else if AExt = '.wav' then
  begin
    try
      AFile := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
      try
        if AFile.Size > 12 then
        begin
          AFile.ReadBuffer(AHeader, 12);
          AHeaderStr := '';
          SetLength(AHeaderStr, 4);
          Move(AHeader[0], AHeaderStr[1], 4);

          if AHeaderStr = 'RIFF' then
          begin
            ASampleCount := (AFile.Size - 44) div 2;

            if ASampleCount < 2048 then
              Result := wtfGeneric
            else if ((AFile.Size - 44) mod (2048 * 4)) = 0 then
              Result := wtfSerum
            else if ((AFile.Size - 44) mod (2048 * 2)) = 0 then
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
    Result := wtfGeneric;
end;

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
      AFile := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
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
          Result.FrameCount := (Result.FileSize - 44) div (2048 * 4);
          if Result.FrameCount <= 0 then
            Result.FrameCount := (Result.FileSize - 44) div (2048 * 2);
          Result.SampleRate := 44100;
        end;
      wtfSurge:
        begin
          Result.FrameCount := 64;
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
    Exit;

  case AInfo.Format of
    wtfSerum:   Result := LoadSerumWavetable(AFilename);
    wtfVital:   Result := LoadVitalWavetable(AFilename);
    wtfSurge:   Result := LoadSurgeWavetable(AFilename);
    wtfGeneric: Result := LoadGenericWave(AFilename);
  else
    Result := LoadGenericWave(AFilename);
  end;

  if Result.IsLoaded then
  begin
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
  end;
end;

class function TSedaiWavetableLoader.LoadSerumWavetable(const AFilename: string): TWavetable;
var
  AFile: TFileStream;
  AWavHeader: array[0..43] of Byte;
  ASampleCount, AFrameCount: Integer;
  ASamples: array of SmallInt;
  I, J, ASampleIndex: Integer;
begin
  FillChar(Result, SizeOf(TWavetable), 0);
  Result.Name := ChangeFileExt(ExtractFileName(AFilename), '');

  try
    AFile := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
    try
      if AFile.Size < 44 then
        Exit;

      AFile.ReadBuffer(AWavHeader, 44);
      ASampleCount := (AFile.Size - 44) div 2;
      AFrameCount := ASampleCount div 2048;

      if AFrameCount <= 0 then
        Exit;

      Result.WaveCount := AFrameCount;
      Result.SampleLength := 2048;
      SetLength(Result.Samples, Result.WaveCount, Result.SampleLength);

      SetLength(ASamples, ASampleCount);
      AFile.ReadBuffer(ASamples[0], ASampleCount * 2);

      for I := 0 to Result.WaveCount - 1 do
      begin
        for J := 0 to Result.SampleLength - 1 do
        begin
          ASampleIndex := (I * 2048) + J;
          if ASampleIndex < ASampleCount then
            Result.Samples[I][J] := ASamples[ASampleIndex] / 32768.0
          else
            Result.Samples[I][J] := 0.0;
        end;
      end;

      Result.IsLoaded := True;

    finally
      AFile.Free;
    end;
  except
    Result.IsLoaded := False;
  end;
end;

class function TSedaiWavetableLoader.LoadVitalWavetable(const AFilename: string): TWavetable;
begin
  Result := LoadSerumWavetable(AFilename);
end;

class function TSedaiWavetableLoader.LoadSurgeWavetable(const AFilename: string): TWavetable;
var
  AFile: TFileStream;
  AHeader: array[0..7] of Byte;
  AWaveCount, ASampleLength: Integer;
  ASamples: array of Single;
  I, J, ASampleIndex: Integer;
begin
  FillChar(Result, SizeOf(TWavetable), 0);
  Result.Name := ChangeFileExt(ExtractFileName(AFilename), '');

  try
    AFile := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
    try
      if AFile.Size < 8 then
        Exit;

      AFile.ReadBuffer(AHeader, 8);

      ASampleLength := 2048;
      AWaveCount := (AFile.Size - 8) div (ASampleLength * 4);

      if AWaveCount <= 0 then
        Exit;

      Result.WaveCount := AWaveCount;
      Result.SampleLength := ASampleLength;
      SetLength(Result.Samples, Result.WaveCount, Result.SampleLength);

      SetLength(ASamples, AWaveCount * ASampleLength);
      AFile.ReadBuffer(ASamples[0], AWaveCount * ASampleLength * 4);

      for I := 0 to Result.WaveCount - 1 do
      begin
        for J := 0 to Result.SampleLength - 1 do
        begin
          ASampleIndex := (I * ASampleLength) + J;
          if ASampleIndex < Length(ASamples) then
            Result.Samples[I][J] := ASamples[ASampleIndex]
          else
            Result.Samples[I][J] := 0.0;
        end;
      end;

      Result.IsLoaded := True;

    finally
      AFile.Free;
    end;
  except
    Result.IsLoaded := False;
  end;
end;

class function TSedaiWavetableLoader.LoadGenericWave(const AFilename: string): TWavetable;
var
  AFile: TFileStream;
  AWavHeader: array[0..43] of Byte;
  ASampleCount: Integer;
  ABitsPerSample, ANumChannels: Word;
  ASamples16: array of SmallInt;
  ASamples8: array of Byte;
  I: Integer;
begin
  FillChar(Result, SizeOf(TWavetable), 0);
  Result.Name := ChangeFileExt(ExtractFileName(AFilename), '');

  try
    AFile := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
    try
      if AFile.Size < 44 then
        Exit;

      AFile.ReadBuffer(AWavHeader, 44);

      ANumChannels := AWavHeader[22] or (AWavHeader[23] shl 8);
      ABitsPerSample := AWavHeader[34] or (AWavHeader[35] shl 8);

      if ABitsPerSample = 16 then
        ASampleCount := (AFile.Size - 44) div 2
      else if ABitsPerSample = 8 then
        ASampleCount := AFile.Size - 44
      else
        Exit;

      if ANumChannels = 2 then
        ASampleCount := ASampleCount div 2;

      if ASampleCount <= 0 then
        Exit;

      Result.WaveCount := 1;
      Result.SampleLength := ASampleCount;
      SetLength(Result.Samples, 1, ASampleCount);

      if ABitsPerSample = 16 then
      begin
        if ANumChannels = 1 then
        begin
          SetLength(ASamples16, ASampleCount);
          AFile.ReadBuffer(ASamples16[0], ASampleCount * 2);
          for I := 0 to ASampleCount - 1 do
            Result.Samples[0][I] := ASamples16[I] / 32768.0;
        end
        else
        begin
          SetLength(ASamples16, ASampleCount * 2);
          AFile.ReadBuffer(ASamples16[0], ASampleCount * 4);
          for I := 0 to ASampleCount - 1 do
            Result.Samples[0][I] := ASamples16[I * 2] / 32768.0;
        end;
      end
      else
      begin
        if ANumChannels = 1 then
        begin
          SetLength(ASamples8, ASampleCount);
          AFile.ReadBuffer(ASamples8[0], ASampleCount);
          for I := 0 to ASampleCount - 1 do
            Result.Samples[0][I] := (ASamples8[I] - 128) / 128.0;
        end
        else
        begin
          SetLength(ASamples8, ASampleCount * 2);
          AFile.ReadBuffer(ASamples8[0], ASampleCount * 2);
          for I := 0 to ASampleCount - 1 do
            Result.Samples[0][I] := (ASamples8[I * 2] - 128) / 128.0;
        end;
      end;

      Result.IsLoaded := True;

    finally
      AFile.Free;
    end;
  except
    Result.IsLoaded := False;
  end;
end;

class function TSedaiWavetableLoader.ScanWavetableDirectory(const APath: string): TStringArray;
var
  ASearchRec: TSearchRec;
  AFiles: TStringList;
  AFullPath: string;
  I: Integer;
begin
  AFiles := TStringList.Create;
  try
    if FindFirst(IncludeTrailingPathDelimiter(APath) + '*.*', faAnyFile, ASearchRec) = 0 then
    begin
      repeat
        if (ASearchRec.Attr and faDirectory) = 0 then
        begin
          AFullPath := IncludeTrailingPathDelimiter(APath) + ASearchRec.Name;
          if IsWavetableFile(AFullPath) then
            AFiles.Add(AFullPath);
        end;
      until FindNext(ASearchRec) <> 0;
      FindClose(ASearchRec);
    end;

    SetLength(Result, AFiles.Count);
    for I := 0 to AFiles.Count - 1 do
      Result[I] := AFiles[I];

  finally
    AFiles.Free;
  end;
end;

class function TSedaiWavetableLoader.LoadWavetableDirectory(const APath: string): TStringArray;
var
  AFiles: TStringArray;
  ALoadedNames: TStringList;
  I: Integer;
  AWavetable: TWavetable;
begin
  AFiles := ScanWavetableDirectory(APath);

  ALoadedNames := TStringList.Create;
  try
    for I := 0 to Length(AFiles) - 1 do
    begin
      AWavetable := LoadWavetable(AFiles[I]);
      if AWavetable.IsLoaded then
        ALoadedNames.Add(AWavetable.Name);
    end;

    SetLength(Result, ALoadedNames.Count);
    for I := 0 to ALoadedNames.Count - 1 do
      Result[I] := ALoadedNames[I];

  finally
    ALoadedNames.Free;
  end;
end;

class function TSedaiWavetableLoader.FindWavetableByName(const AName: string): TWavetable;
var
  I: Integer;
begin
  FillChar(Result, SizeOf(TWavetable), 0);

  for I := 0 to Length(GWavetableCache) - 1 do
  begin
    if SameText(GWavetableCache[I].Name, AName) then
    begin
      Result := GWavetableCache[I].Wavetable;
      GWavetableCache[I].LastAccess := Now;
      Exit;
    end;
  end;
end;

class procedure TSedaiWavetableLoader.ClearCache;
var
  I: Integer;
begin
  for I := 0 to Length(GWavetableCache) - 1 do
  begin
    if GWavetableCache[I].Wavetable.IsLoaded then
      SetLength(GWavetableCache[I].Wavetable.Samples, 0, 0);
  end;
  SetLength(GWavetableCache, 0);
end;

class function TSedaiWavetableLoader.GetCacheInfo: string;
var
  I, ATotalWaves: Integer;
begin
  ATotalWaves := 0;
  for I := 0 to Length(GWavetableCache) - 1 do
    ATotalWaves := ATotalWaves + GWavetableCache[I].Wavetable.WaveCount;

  Result := Format('Cache: %d wavetables, %d total waves',
    [Length(GWavetableCache), ATotalWaves]);
end;

class function TSedaiWavetableLoader.IsCached(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(GWavetableCache) - 1 do
  begin
    if SameText(GWavetableCache[I].Name, AName) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

class function TSedaiWavetableLoader.GetCacheCount: Integer;
begin
  Result := Length(GWavetableCache);
end;

class function TSedaiWavetableLoader.GetCachedName(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < Length(GWavetableCache)) then
    Result := GWavetableCache[AIndex].Name
  else
    Result := '';
end;

class function TSedaiWavetableLoader.FormatToString(AFormat: TWavetableFormat): string;
begin
  case AFormat of
    wtfSerum:   Result := 'Serum';
    wtfVital:   Result := 'Vital';
    wtfSurge:   Result := 'Surge';
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

class function TSedaiWavetableLoader.IsWavetableFile(const AFilename: string): Boolean;
var
  AExt: string;
  AExts: TStringArray;
  I: Integer;
begin
  Result := False;
  AExt := LowerCase(ExtractFileExt(AFilename));
  AExts := GetSupportedExtensions;

  for I := 0 to Length(AExts) - 1 do
  begin
    if AExt = AExts[I] then
    begin
      Result := True;
      Exit;
    end;
  end;
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

initialization
  TSedaiWavetableLoader.Initialize;

finalization
  TSedaiWavetableLoader.Finalize;

end.
