{*
 * Sedai Audio Foundation - SNG Register Dump Tool
 *
 * Dumps SID register state per frame for comparison.
 * Format: FRAME: RR RR RR ... (25 hex values)
 *}

program sng_dump;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, SedaiGoatTracker;

const
  SAMPLE_RATE = 44100;
  MAX_FRAMES = 3000;    // ~60 seconds at 50Hz

var
  GPlayer: TSedaiGoatTracker;
  DumpFile: TextFile;
  FrameCount: Integer;
  LastReg18: Byte = $FF;

procedure DumpFrame;
var
  R: Integer;
  S: string;
  CurrentReg18: Byte;
begin
  S := Format('%04d:', [FrameCount]);
  for R := 0 to 24 do
    S := S + Format(' %02X', [GPlayer.GetSidRegister(R)]);
  WriteLn(DumpFile, S);

  // Track changes in register $18 (filter type + master volume)
  CurrentReg18 := GPlayer.GetSidRegister($18);
  if CurrentReg18 <> LastReg18 then
  begin
    WriteLn('Frame ', FrameCount, ': Reg $18 changed from $', IntToHex(LastReg18, 2),
            ' to $', IntToHex(CurrentReg18, 2),
            ' (FilterType=', (CurrentReg18 and $F0) shr 4,
            ', Volume=', CurrentReg18 and $0F, ')');
    LastReg18 := CurrentReg18;
  end;
end;

var
  AFilename: string;
  ADumpFilename: string;
  ASubtune: Integer;
begin
  WriteLn;
  WriteLn('SNG Register Dump Tool - Sedai Audio Foundation');
  WriteLn('------------------------------------------------');
  WriteLn;

  if ParamCount < 1 then
  begin
    WriteLn('Usage: sng_dump <filename.sng> [subtune] [output.dump]');
    WriteLn;
    WriteLn('Dumps SID register state per frame.');
    WriteLn('Format: FRAME: R00 R01 R02 ... R24 (hex)');
    Halt(1);
  end;

  AFilename := ParamStr(1);
  if not FileExists(AFilename) then
  begin
    WriteLn('Error: File not found: ', AFilename);
    Halt(1);
  end;

  ASubtune := 0;
  if ParamCount >= 2 then
    ASubtune := StrToIntDef(ParamStr(2), 0);

  ADumpFilename := ChangeFileExt(AFilename, '.dump');
  if ParamCount >= 3 then
    ADumpFilename := ParamStr(3);

  // Create player
  GPlayer := TSedaiGoatTracker.Create;
  GPlayer.SetSampleRate(SAMPLE_RATE);

  WriteLn('Loading: ', AFilename);

  if not GPlayer.LoadFromFile(AFilename) then
  begin
    WriteLn('Error: Failed to load file');
    GPlayer.Free;
    Halt(1);
  end;

  WriteLn('Title:    ', GPlayer.SongName);
  WriteLn('Author:   ', GPlayer.Author);
  WriteLn('Subtunes: ', GPlayer.SubtuneCount);
  WriteLn;

  // Analyze commands in the song file
  GPlayer.AnalyzeCommands;
  WriteLn;

  WriteLn('Dumping to: ', ADumpFilename);
  WriteLn('Max frames: ', MAX_FRAMES);

  // Open dump file
  AssignFile(DumpFile, ADumpFilename);
  Rewrite(DumpFile);

  // Header
  WriteLn(DumpFile, '# SNG Register Dump - ', ExtractFileName(AFilename));
  WriteLn(DumpFile, '# Frame: 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10 11 12 13 14 15 16 17 18');
  WriteLn(DumpFile, '#        FL FH PL PH CR AD SR FL FH PL PH CR AD SR FL FH PL PH CR AD SR FC FC RF MV');

  // Initialize
  FrameCount := 0;

  // Start playback
  GPlayer.Play(ASubtune);

  // Run frames and dump
  while (FrameCount < MAX_FRAMES) and (not GPlayer.SongFinished) do
  begin
    // Call PlayRoutine (advances one frame)
    GPlayer.CallPlayRoutine;

    // Dump register state
    DumpFrame;

    Inc(FrameCount);

    if (FrameCount mod 500) = 0 then
      Write(#13, 'Frame: ', FrameCount);
  end;

  WriteLn;
  WriteLn('Dumped ', FrameCount, ' frames');

  CloseFile(DumpFile);
  GPlayer.Free;

  WriteLn('Done.');
end.
