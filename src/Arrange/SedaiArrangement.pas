// ============================================================================
// SedaiArrangement — where the instruments STAND, and what comes out.
//
// A .patch says what an instrument IS. It deliberately does not say where it is,
// how loud it is against the others, or what format the result has: an
// instrument that had already decided it was "on the left" could never be turned
// around. Those are the arrangement's business, and this is the arrangement.
//
// Same shape as a patch, for the same reasons: it is TEXT, so it diffs and
// versions like source, and it is read rather than built through an API.
//
//   rate    48000
//   listen  at=0,1.2,0 facing=0,0,-1
//   part    bass = library/patches/moog_bass.patch channel=1 voices=4
//   place   bass at=-2,0,3
//   gain    bass = 0.8
//
// WHAT IS BORROWED AND WHAT IS NEW. The positional maths already existed —
// SedaiSpatialAudio has the listener, the distance models, the cone and the
// panning, and SedaiPatchVoices has the polyphonic pool. Nothing of that is
// rewritten here. What was missing was the thing that ties them: a way to say
// "this instrument stands there", and a renderer that sums several of them.
//
// AN INSTRUMENT MAY RADIATE FROM MORE THAN ONE POINT. A patch can declare
// several outputs with `pos=` along its own axis and an `extent=` in metres —
// a handpan is one object whose tone fields are spread across a shell a foot
// wide. Those points are placed around the instrument's position, on the axis
// it faces across, so moving the instrument moves them together and turning it
// turns them. An instrument with one output is a point source, which is every
// electronic sound.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiArrangement;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Math, StrUtils,
  SedaiSpatialAudio, SedaiPatchVoices, SedaiPatchGraph;

type
  TSedaiArrPart = class
  private
    FName: string;
    FPatchFile: string;
    FChannel: Integer;        // MIDI channel that drives it, 1..16; 0 = any
    FVoices: Integer;
    FGain: Single;
    FPool: TSedaiPatchVoicePool;
    // One spatial processor per RADIATING POINT, not per instrument: a patch
    // with two outputs is one object heard from two places.
    FSpat: array of TSedaiSpatialProcessor;
    FPosX, FPosY, FPosZ: Single;
    FDirX, FDirY, FDirZ: Single;
    FExtent: Single;
    FPlaced: Boolean;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;
    property Name: string read FName;
    property Channel: Integer read FChannel write FChannel;
    property Gain: Single read FGain write FGain;
    property Pool: TSedaiPatchVoicePool read FPool;
  end;

  { TSedaiArrangement }

  TSedaiArrangement = class
  private
    FParts: array of TSedaiArrPart;
    FListener: TSedaiSpatialListener;
    FSampleRate: Integer;
    FBlockSize: Integer;
    FLastError: string;
    FWarnings: string;
    FMono, FL, FR, FSumL, FSumR: array of Single;
    function FindPart(const AName: string): TSedaiArrPart;
    function Fail(const AMsg: string; ALine: Integer): Boolean;
    procedure PlacePoints(APart: TSedaiArrPart);
  public
    constructor Create;
    destructor Destroy; override;

    // Read the arrangement and build every part. ABaseDir is what patch paths
    // are relative to — the directory the .arr file itself came from, so an
    // arrangement can be moved with its patches.
    function LoadFromFile(const AFilename: string): Boolean;
    procedure Prepare(ASampleRate, ABlockSize: Integer);
    procedure Reset;

    // Render ACount frames of the whole arrangement into interleaved stereo.
    procedure Render(ACount: Integer; AOut: PSingle);

    function PartCount: Integer;
    function Part(AIndex: Integer): TSedaiArrPart;
    // The part a MIDI channel drives, or nil. Channels are 1..16 as written in
    // the file; a part with no channel answers to none.
    function PartForChannel(AChannel: Integer): TSedaiArrPart;

    function Describe: string;
    property SampleRate: Integer read FSampleRate;
    property Listener: TSedaiSpatialListener read FListener;
    property LastError: string read FLastError;
    property Warnings: string read FWarnings;
  end;

implementation

{ TSedaiArrPart }

constructor TSedaiArrPart.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FChannel := 0;
  FVoices := 8;
  FGain := 1.0;
  FPool := TSedaiPatchVoicePool.Create;
  FPosX := 0; FPosY := 0; FPosZ := 0;
  // Facing the listener's default direction: an instrument nobody aimed is
  // pointed at the audience, which is what a player does.
  FDirX := 0; FDirY := 0; FDirZ := -1;
  FExtent := 0;
  FPlaced := False;
end;

destructor TSedaiArrPart.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FSpat) do FSpat[I].Free;
  SetLength(FSpat, 0);
  FPool.Free;
  inherited Destroy;
end;

{ TSedaiArrangement }

constructor TSedaiArrangement.Create;
begin
  inherited Create;
  FSampleRate := 48000;
  FBlockSize := 512;
  FListener := TSedaiSpatialListener.Create(FSampleRate);
  FListener.SetPosition(0, 0, 0);
  FListener.SetOrientation(0, 0, -1, 0, 1, 0);
  FLastError := '';
  FWarnings := '';
end;

destructor TSedaiArrangement.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FParts) do FParts[I].Free;
  SetLength(FParts, 0);
  FListener.Free;
  inherited Destroy;
end;

function TSedaiArrangement.Fail(const AMsg: string; ALine: Integer): Boolean;
begin
  FLastError := Format('line %d: %s', [ALine, AMsg]);
  Result := False;
end;

function TSedaiArrangement.FindPart(const AName: string): TSedaiArrPart;
var
  I: Integer;
begin
  for I := 0 to High(FParts) do
    if SameText(FParts[I].FName, AName) then Exit(FParts[I]);
  Result := nil;
end;

function TSedaiArrangement.PartCount: Integer;
begin
  Result := Length(FParts);
end;

function TSedaiArrangement.Part(AIndex: Integer): TSedaiArrPart;
begin
  if (AIndex >= 0) and (AIndex < Length(FParts)) then Result := FParts[AIndex]
  else Result := nil;
end;

function TSedaiArrangement.PartForChannel(AChannel: Integer): TSedaiArrPart;
var
  I: Integer;
begin
  for I := 0 to High(FParts) do
    if FParts[I].FChannel = AChannel then Exit(FParts[I]);
  Result := nil;
end;

// Split "1.5,0,-3" into three numbers. The separator is a comma because a
// space already separates the words of a statement.
function ParseVec(const AText: string; out X, Y, Z: Single): Boolean;
var
  P: TStringArray;
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings; FS.DecimalSeparator := '.';
  P := AText.Split([',']);
  Result := (Length(P) = 3) and
            TryStrToFloat(Trim(P[0]), X, FS) and
            TryStrToFloat(Trim(P[1]), Y, FS) and
            TryStrToFloat(Trim(P[2]), Z, FS);
end;

function ParseNum(const AText: string; out V: Single): Boolean;
var
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings; FS.DecimalSeparator := '.';
  Result := TryStrToFloat(Trim(AText), V, FS);
end;

// Split "at=1,0,2" into key and value.
function KeyVal(const AWord: string; out AKey, AVal: string): Boolean;
var
  E: Integer;
begin
  E := Pos('=', AWord);
  Result := E > 1;
  if Result then
  begin
    AKey := LowerCase(Copy(AWord, 1, E - 1));
    AVal := Copy(AWord, E + 1, Length(AWord));
  end;
end;

// `at= 0,1,2` with a space is two words, and the value is lost. It is the trap
// people actually hit — lining columns up is the natural thing to do — so it
// gets its own message instead of a puzzle about missing coordinates.
function SaysNothing(const AVal: string): Boolean;
begin
  Result := Trim(AVal) = '';
end;

// Give each of a part's outputs a spatial processor, and put it where that
// radiating point actually is. With one output that is the instrument's own
// position; with more, they spread along the axis ACROSS the way it faces, so
// turning the instrument turns its shell with it.
procedure TSedaiArrangement.PlacePoints(APart: TSedaiArrPart);
var
  N, I: Integer;
  Pos_, Half, RX, RZ, Len: Single;
begin
  for I := 0 to High(APart.FSpat) do APart.FSpat[I].Free;
  N := APart.FPool.OutputCount;
  if N < 1 then N := 1;
  SetLength(APart.FSpat, N);

  // The right-hand axis: the facing direction turned a quarter turn in the
  // horizontal plane. Height is left out on purpose — an instrument wide in Y
  // is a rarer thing than one wide in X, and pretending otherwise would need a
  // full orientation frame the patch does not carry.
  RX := -APart.FDirZ; RZ := APart.FDirX;
  Len := Sqrt(RX * RX + RZ * RZ);
  if Len > 1e-6 then begin RX := RX / Len; RZ := RZ / Len; end
                else begin RX := 1; RZ := 0; end;
  // The patch may declare its own extent; the arrangement may override it.
  if (APart.FExtent = 0.0) then APart.FExtent := APart.FPool.Extent;
  Half := APart.FExtent * 0.5;

  for I := 0 to N - 1 do
  begin
    APart.FSpat[I] := TSedaiSpatialProcessor.Create(FListener);
    Pos_ := 0.0;
    if (Half > 0.0) and (I < APart.FPool.OutputCount) then
      Pos_ := APart.FPool.OutputPos(I);     // -1..+1 on the instrument's axis
    APart.FSpat[I].SetPosition(APart.FPosX + RX * Pos_ * Half,
                               APart.FPosY,
                               APart.FPosZ + RZ * Pos_ * Half);
    APart.FSpat[I].SetDirection(APart.FDirX, APart.FDirY, APart.FDirZ);
  end;
end;

function TSedaiArrangement.LoadFromFile(const AFilename: string): Boolean;
var
  Src: TStringList;
  BaseDir, Line, Verb, Key, Val, PatchPath: string;
  W: TStringArray;
  L, I, K, V: Integer;
  P: TSedaiArrPart;
  X, Y, Z, F: Single;
  DirSet: Boolean;
begin
  Result := False;
  FLastError := '';
  FWarnings := '';
  if not FileExists(AFilename) then
  begin
    FLastError := AFilename + ': not found';
    Exit;
  end;
  BaseDir := ExtractFilePath(AFilename);

  Src := TStringList.Create;
  try
    Src.LoadFromFile(AFilename);
    for L := 0 to Src.Count - 1 do
    begin
      Line := Trim(Src[L]);
      K := Pos('#', Line);
      if K > 0 then Line := Trim(Copy(Line, 1, K - 1));
      if Line = '' then Continue;
      W := Line.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
      if Length(W) = 0 then Continue;
      Verb := LowerCase(W[0]);

      // ---- rate <hz> ----
      if Verb = 'rate' then
      begin
        if (Length(W) < 2) or (StrToIntDef(W[1], 0) < 8000) then
          Exit(Fail('rate needs a sample rate, e.g. `rate 48000`', L + 1));
        FSampleRate := StrToIntDef(W[1], 48000);
        FListener.Free;
        FListener := TSedaiSpatialListener.Create(FSampleRate);
        FListener.SetPosition(0, 0, 0);
        FListener.SetOrientation(0, 0, -1, 0, 1, 0);
      end

      // ---- listen at=x,y,z [facing=x,y,z] ----
      else if Verb = 'listen' then
      begin
        for I := 1 to High(W) do
        begin
          if not KeyVal(W[I], Key, Val) then
            Exit(Fail('listen takes at= and facing=', L + 1));
          if Key = 'at' then
          begin
            if SaysNothing(Val) then
              Exit(Fail('at= has nothing after it: a space after "=" splits the '
                      + 'word in two. Write at=0,1.2,0', L + 1));
            if not ParseVec(Val, X, Y, Z) then Exit(Fail('at= needs x,y,z', L + 1));
            FListener.SetPosition(X, Y, Z);
          end
          else if Key = 'facing' then
          begin
            if not ParseVec(Val, X, Y, Z) then Exit(Fail('facing= needs x,y,z', L + 1));
            FListener.SetOrientation(X, Y, Z, 0, 1, 0);
          end
          else
            Exit(Fail('listen: unknown key "' + Key + '"', L + 1));
        end;
      end

      // ---- part <name> = <file.patch> [channel=n] [voices=n] ----
      else if Verb = 'part' then
      begin
        if (Length(W) < 4) or (W[2] <> '=') then
          Exit(Fail('part needs: part <name> = <file.patch> [channel=n] [voices=n]', L + 1));
        if FindPart(W[1]) <> nil then
          Exit(Fail('part "' + W[1] + '" declared twice', L + 1));
        P := TSedaiArrPart.Create(W[1]);
        P.FPatchFile := W[3];
        for I := 4 to High(W) do
        begin
          if not KeyVal(W[I], Key, Val) then
            Exit(Fail('part: expected key=value, got "' + W[I] + '"', L + 1));
          V := StrToIntDef(Val, -1);
          if Key = 'channel' then
          begin
            if (V < 1) or (V > 16) then Exit(Fail('channel= must be 1..16', L + 1));
            P.FChannel := V;
          end
          else if Key = 'voices' then
          begin
            if V < 1 then Exit(Fail('voices= must be at least 1', L + 1));
            P.FVoices := V;
          end
          else
            Exit(Fail('part: unknown key "' + Key + '"', L + 1));
        end;
        PatchPath := P.FPatchFile;
        if (BaseDir <> '') and (not FileExists(PatchPath)) then
          PatchPath := BaseDir + P.FPatchFile;
        if not P.FPool.LoadFromFile(PatchPath, P.FVoices) then
        begin
          FLastError := Format('line %d: %s', [L + 1, P.FPool.LastError]);
          P.Free;
          Exit;
        end;
        // Non-fatal things the patch said. Printing them is the only notice
        // anyone gets that an included file has moved on.
        if P.FPool.Warnings <> '' then FWarnings := FWarnings + P.FPool.Warnings;
        if P.FPool.LastError <> '' then
          FWarnings := FWarnings + Format('%s: %s'#10, [P.FName, P.FPool.LastError]);
        SetLength(FParts, Length(FParts) + 1);
        FParts[High(FParts)] := P;
      end

      // ---- place <name> at=x,y,z [facing=x,y,z] [extent=m] ----
      else if Verb = 'place' then
      begin
        if Length(W) < 3 then
          Exit(Fail('place needs: place <name> at=x,y,z [facing=x,y,z] [extent=m]', L + 1));
        P := FindPart(W[1]);
        if P = nil then Exit(Fail('place: no part called "' + W[1] + '"', L + 1));
        DirSet := False;
        for I := 2 to High(W) do
        begin
          if not KeyVal(W[I], Key, Val) then
            Exit(Fail('place: expected key=value, got "' + W[I] + '"', L + 1));
          if Key = 'at' then
          begin
            if SaysNothing(Val) then
              Exit(Fail('at= has nothing after it: a space after "=" splits the '
                      + 'word in two. Write at=-2.5,0.4,4', L + 1));
            if not ParseVec(Val, X, Y, Z) then Exit(Fail('at= needs x,y,z', L + 1));
            P.FPosX := X; P.FPosY := Y; P.FPosZ := Z;
            P.FPlaced := True;
          end
          else if Key = 'facing' then
          begin
            if not ParseVec(Val, X, Y, Z) then Exit(Fail('facing= needs x,y,z', L + 1));
            P.FDirX := X; P.FDirY := Y; P.FDirZ := Z;
            DirSet := True;
          end
          else if Key = 'extent' then
          begin
            if not ParseNum(Val, F) then Exit(Fail('extent= needs a number', L + 1));
            P.FExtent := F;
          end
          else
            Exit(Fail('place: unknown key "' + Key + '"', L + 1));
        end;
        if not DirSet then
        begin
          // Nobody aimed it, so it looks at the listener. That is what a player
          // does, and it beats leaving an instrument staring at a wall.
          P.FDirX := FListener.Position.X - P.FPosX;
          P.FDirY := FListener.Position.Y - P.FPosY;
          P.FDirZ := FListener.Position.Z - P.FPosZ;
          if Abs(P.FDirX) + Abs(P.FDirY) + Abs(P.FDirZ) < 1e-6 then
          begin P.FDirX := 0; P.FDirY := 0; P.FDirZ := -1; end;
        end;
      end

      // ---- gain <name> = <v> ----
      else if Verb = 'gain' then
      begin
        if (Length(W) < 4) or (W[2] <> '=') then
          Exit(Fail('gain needs: gain <name> = <value>', L + 1));
        P := FindPart(W[1]);
        if P = nil then Exit(Fail('gain: no part called "' + W[1] + '"', L + 1));
        if not ParseNum(W[3], F) then Exit(Fail('gain: not a number', L + 1));
        P.FGain := F;
      end

      else
        Exit(Fail(Format('unknown directive "%s" (expected rate, listen, part, place or gain)',
                         [W[0]]), L + 1));
    end;

    if Length(FParts) = 0 then
    begin
      FLastError := 'the arrangement has no parts';
      Exit;
    end;
    Result := True;
  finally
    Src.Free;
  end;
end;

procedure TSedaiArrangement.Prepare(ASampleRate, ABlockSize: Integer);
var
  I: Integer;
begin
  FSampleRate := ASampleRate;
  FBlockSize := ABlockSize;
  SetLength(FMono, ABlockSize);
  SetLength(FL, ABlockSize);
  SetLength(FR, ABlockSize);
  SetLength(FSumL, ABlockSize);
  SetLength(FSumR, ABlockSize);
  for I := 0 to High(FParts) do
  begin
    FParts[I].FPool.Prepare(ASampleRate, ABlockSize);
    FParts[I].FPool.Reset;
    PlacePoints(FParts[I]);
  end;
end;

procedure TSedaiArrangement.Reset;
var
  I: Integer;
begin
  for I := 0 to High(FParts) do FParts[I].FPool.Reset;
end;

procedure TSedaiArrangement.Render(ACount: Integer; AOut: PSingle);
var
  I, C, K, N: Integer;
  P: TSedaiArrPart;
  V: Single;
begin
  if ACount > FBlockSize then ACount := FBlockSize;
  for K := 0 to ACount - 1 do begin FSumL[K] := 0.0; FSumR[K] := 0.0; end;

  for I := 0 to High(FParts) do
  begin
    P := FParts[I];
    P.FPool.Render(ACount);
    N := P.FPool.OutputCount;
    if N > Length(P.FSpat) then N := Length(P.FSpat);
    for C := 0 to N - 1 do
    begin
      for K := 0 to ACount - 1 do FMono[K] := P.FPool.MixSample(C, K) * P.FGain;
      P.FSpat[C].ProcessBuffer(FMono, FL, FR, ACount);
      for K := 0 to ACount - 1 do
      begin
        FSumL[K] := FSumL[K] + FL[K];
        FSumR[K] := FSumR[K] + FR[K];
      end;
    end;
  end;

  for K := 0 to ACount - 1 do
  begin
    V := FSumL[K];
    if V > 1.0 then V := 1.0 else if V < -1.0 then V := -1.0;
    AOut[K * 2] := V;
    V := FSumR[K];
    if V > 1.0 then V := 1.0 else if V < -1.0 then V := -1.0;
    AOut[K * 2 + 1] := V;
  end;
end;

function TSedaiArrangement.Describe: string;
var
  I: Integer;
  P: TSedaiArrPart;
begin
  Result := Format('  %d parti, ascoltatore a %.2f,%.2f,%.2f, %d Hz'#10,
                   [Length(FParts), FListener.Position.X, FListener.Position.Y,
                    FListener.Position.Z, FSampleRate]);
  for I := 0 to High(FParts) do
  begin
    P := FParts[I];
    Result := Result + Format('    %-10s %-34s ch%-3s %d voci  a %6.2f,%5.2f,%6.2f  gain %.2f'#10,
      [P.FName, ExtractFileName(P.FPatchFile),
       IfThen(P.FChannel > 0, IntToStr(P.FChannel), '-'),
       P.FPool.VoiceCount, P.FPosX, P.FPosY, P.FPosZ, P.FGain]);
  end;
end;

end.
