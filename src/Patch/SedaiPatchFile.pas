// ============================================================================
// SedaiPatchFile — read a patch from text.
//
// The patch is a TABLE of modules, values and connections. That is deliberate:
// a table is a file you can diff, version and generate, and it is the ARP 2500 /
// EMS VCS3 matrix written as rows instead of pins. A matrix view and, later, a
// cord view are both just renderings of this file (design notes, 1.3 and 4.6).
//
//   # comments run to end of line
//   mode    = block | sample          # sample forces EVERY stage per-sample
//   module  osc1 = osc shape=saw freq=110
//   set     filt.cutoff = 2.0
//   connect osc1.out -> filt.in
//   connect note.pitch -> osc1.pitch normalled   # a default that yields
//   connect lfo1.out -> osc1.pitch amount=0.02
//   output  pan.l           # ogni riga output aggiunge un CANALE, in ordine
//   output  pan.r
//
// Numbers accept unit suffixes: 440Hz, 2ms, 120ms, 50%, plain floats.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchFile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SedaiPatchGraph, SedaiPatchModules, SedaiPatchElectronic, SedaiPatchInstruments, SedaiPatchPart, SedaiPatchSpace, SedaiPatchBody, SedaiPatchLegacy;

type
  TSedaiPatchLoadResult = record
    Success: Boolean;
    ErrorLine: Integer;
    ErrorText: string;
    ForceSampleRate: Boolean;
    // 0 = not stated, and the caller keeps its own default. An instrument that
    // is monophonic by nature should say so in the patch rather than depend on
    // how it was launched.
    Voices: Integer;
    // Not fatal, and not silent either: an included file that has changed since
    // this patch was written may have changed how it sounds. Nobody can decide
    // that for you, so it is said and not enforced.
    Warnings: string;
  end;

// Parse APatch (already-loaded text) into AGraph. The graph is NOT compiled
// here — the caller does that, so it can decide what to do with the mode flag.
function LoadPatchFromStrings(AGraph: TSedaiPatchGraph;
                              AText: TStrings): TSedaiPatchLoadResult;
// The checksum an `include` line may carry, so a patch can notice that the file
// it was written against has moved on.
function PatchChecksum(const AText: string): string;
function LoadPatchFromFile(AGraph: TSedaiPatchGraph;
                           const AFilename: string): TSedaiPatchLoadResult;

// Exposed because the renderer wants the same suffix rules on the command line.
function ParseValue(const AText: string; out AValue: Single): Boolean;

implementation

// Split on whitespace, but keep a double-quoted run together. Needed because an
// instrument is named the way a musician names it — "Drawbar Organ" — and the
// name has to survive reaching the module intact.
function SplitArgs(const S: string): TStringArray;
var
  I, N: Integer;
  Cur: string;
  InQuote: Boolean;

  procedure Flush;
  begin
    if Cur <> '' then
    begin
      SetLength(Result, N + 1);
      Result[N] := Cur;
      Inc(N);
      Cur := '';
    end;
  end;

begin
  SetLength(Result, 0);
  N := 0;
  Cur := '';
  InQuote := False;
  for I := 1 to Length(S) do
  begin
    if S[I] = '"' then
    begin
      InQuote := not InQuote;
      Cur := Cur + S[I];        // kept: the module strips them itself
    end
    else if (not InQuote) and ((S[I] = ' ') or (S[I] = #9)) then
      Flush
    else
      Cur := Cur + S[I];
  end;
  Flush;
end;

function ParseValue(const AText: string; out AValue: Single): Boolean;
var
  S: string;
  Scale: Single;
  FS: TFormatSettings;
begin
  AValue := 0.0;
  S := Trim(AText);
  if S = '' then Exit(False);
  Scale := 1.0;

  // Suffixes, longest first so "ms" is not eaten by "s".
  if (Length(S) > 2) and SameText(Copy(S, Length(S) - 1, 2), 'hz') then
    S := Copy(S, 1, Length(S) - 2)
  else if (Length(S) > 2) and SameText(Copy(S, Length(S) - 1, 2), 'ms') then
  begin
    S := Copy(S, 1, Length(S) - 2); Scale := 0.001;
  end
  else if (Length(S) > 1) and SameText(Copy(S, Length(S), 1), 's') then
    S := Copy(S, 1, Length(S) - 1)
  else if (Length(S) > 1) and (S[Length(S)] = '%') then
  begin
    S := Copy(S, 1, Length(S) - 1); Scale := 0.01;
  end
  else if (Length(S) > 2) and SameText(Copy(S, Length(S) - 1, 2), 'db') then
    S := Copy(S, 1, Length(S) - 2);

  // Patch files are written with a dot, whatever the machine's locale says.
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Result := TryStrToFloat(Trim(S), AValue, FS);
  if Result then AValue := AValue * Scale;
end;

// Split off a `key=value` token, tolerating spaces around the '='.
function SplitKeyValue(const AToken: string; out AKey, AValue: string): Boolean;
var
  P: Integer;
begin
  P := Pos('=', AToken);
  Result := P > 1;
  if Result then
  begin
    AKey := Trim(Copy(AToken, 1, P - 1));
    AValue := Trim(Copy(AToken, P + 1, Length(AToken)));
  end;
end;

function PatchChecksum(const AText: string): string;
var
  H: QWord;
  I: Integer;
  C: Char;
begin
  // FNV-1a over everything that is not whitespace or a comment: reformatting a
  // file must not look like changing it, or the warning cries wolf and gets
  // ignored, which is worse than not having it.
  H := 14695981039346656037;
  I := 1;
  while I <= Length(AText) do
  begin
    C := AText[I];
    if C = '#' then
    begin
      while (I <= Length(AText)) and (AText[I] <> #10) do Inc(I);
      Continue;
    end;
    if not (C in [' ', #9, #13, #10]) then
      H := (H xor QWord(Ord(C))) * 1099511628211;
    Inc(I);
  end;
  Result := IntToHex(H, 16);
end;

function LoadInto(AGraph: TSedaiPatchGraph; AText: TStrings;
  const APrefix, ABaseDir, AFromFile: string;
  ADepth: Integer): TSedaiPatchLoadResult; forward;

function LoadPatchFromStrings(AGraph: TSedaiPatchGraph;
  AText: TStrings): TSedaiPatchLoadResult;
begin
  Result := LoadInto(AGraph, AText, '', '', '', 0);
end;

function LoadInto(AGraph: TSedaiPatchGraph; AText: TStrings;
  const APrefix, ABaseDir, AFromFile: string;
  ADepth: Integer): TSedaiPatchLoadResult;
var
  L, I, P: Integer;
  Line, Verb, Rest, Key, Val, Src, Dst: string;
  Parts: TStringArray;
  M: TSedaiPatchModule;
  V, Amount, OutPos: Single;
  Normalled: Boolean;
  IncName, IncPrefix, IncPath, Sum: string;
  Inc2: TStringList;
  Sub: TSedaiPatchLoadResult;

  procedure Fail(const AMsg: string);
  begin
    Result.Success := False;
    Result.ErrorLine := L + 1;
    Result.ErrorText := AMsg;
  end;

  // Inside an include everything is named under its prefix, so two included
  // files may both call a module osc1 without colliding, and a name tells you
  // which file it came from.
  function Q(const AName: string): string;
  begin
    if APrefix = '' then Result := AName else Result := APrefix + '.' + AName;
  end;

begin
  Result.Success := True;
  Result.ErrorLine := 0;
  Result.ErrorText := '';
  Result.ForceSampleRate := False;
  Result.Voices := 0;
  Result.Warnings := '';

  for L := 0 to AText.Count - 1 do
  begin
    Line := AText[L];
    P := Pos('#', Line);
    if P > 0 then Line := Copy(Line, 1, P - 1);
    Line := Trim(Line);
    if Line = '' then Continue;

    Parts := Line.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
    if Length(Parts) = 0 then Continue;
    Verb := Parts[0];

    // ---- mode = block | sample -------------------------------------------
    if SameText(Verb, 'mode') then
    begin
      if not SplitKeyValue(Line, Key, Val) then
      begin Fail('mode needs a value: mode = block | sample'); Exit; end;
      if SameText(Val, 'sample') then Result.ForceSampleRate := True
      else if SameText(Val, 'block') then Result.ForceSampleRate := False
      else begin Fail(Format('unknown mode "%s" (expected block or sample)', [Val])); Exit; end;
      Continue;
    end;

    // ---- module <name> = <type> [k=v ...] --------------------------------
    if SameText(Verb, 'module') then
    begin
      Rest := Trim(Copy(Line, Length(Verb) + 1, Length(Line)));
      if not SplitKeyValue(Rest, Key, Val) then
      begin Fail('module needs: module <name> = <type> [key=value ...]'); Exit; end;
      Parts := SplitArgs(Val);
      if Length(Parts) = 0 then
      begin Fail('module needs a type'); Exit; end;

      M := CreateModuleByType(Parts[0]);
      if M = nil then M := CreateElectronicModuleByType(Parts[0]);
      if M = nil then M := CreateInstrumentModuleByType(Parts[0]);
      if M = nil then M := CreatePartModuleByType(Parts[0]);
      if M = nil then M := CreateSpaceModuleByType(Parts[0]);
      if M = nil then M := CreateBodyModuleByType(Parts[0]);
      // Native modules first, then the wrappers around SAF's existing units.
      if M = nil then M := CreateLegacyModuleByType(Parts[0]);
      if M = nil then
      begin
        Fail(Format('unknown module type "%s"'#10'  core: %s'#10'  electronic: %s'#10'  instruments: %s'#10'  library: %s'#10'  space: %s'#10'  body: %s'#10'  bridged: %s',
                    [Parts[0], KnownModuleTypes, KnownElectronicTypes,
                     KnownInstrumentTypes, KnownPartTypes, KnownSpaceTypes,
                     KnownBodyTypes, KnownLegacyTypes]));
        Exit;
      end;
      if not AGraph.AddModule(M, Q(Key)) then
      begin M.Free; Fail(AGraph.LastError); Exit; end;
      M.SourceFile := AFromFile;
      M.SourcePrefix := APrefix;

      for I := 1 to High(Parts) do
      begin
        if not SplitKeyValue(Parts[I], Key, Val) then
        begin Fail(Format('"%s" is not key=value', [Parts[I]])); Exit; end;
        if not M.Configure(Key, Val) then
        begin
          Fail(Format('module type "%s" does not understand "%s"', [M.TypeName, Key]));
          Exit;
        end;
      end;
      Continue;
    end;

    // ---- set <module>.<port> = <value> -----------------------------------
    if SameText(Verb, 'set') then
    begin
      Rest := Trim(Copy(Line, Length(Verb) + 1, Length(Line)));
      if not SplitKeyValue(Rest, Key, Val) then
      begin Fail('set needs: set <module>.<port> = <value>'); Exit; end;
      if not ParseValue(Val, V) then
      begin Fail(Format('"%s" is not a number', [Val])); Exit; end;
      if not AGraph.SetValue(Q(Key), V) then
      begin Fail(AGraph.LastError); Exit; end;
      Continue;
    end;

    // ---- connect <out> -> <in> [amount=x] --------------------------------
    if SameText(Verb, 'connect') then
    begin
      P := Pos('->', Line);
      if P = 0 then
      begin Fail('connect needs: connect <module>.<out> -> <module>.<in> [amount=x]'); Exit; end;
      Src := Trim(Copy(Line, Length(Verb) + 1, P - Length(Verb) - 1));
      Rest := Trim(Copy(Line, P + 2, Length(Line)));
      Parts := Rest.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
      if Length(Parts) = 0 then
      begin Fail('connect is missing its destination'); Exit; end;
      Dst := Parts[0];

      Amount := 1.0;
      Normalled := False;
      for I := 1 to High(Parts) do
      begin
        if SameText(Parts[I], 'normalled') then
        begin
          Normalled := True;
          Continue;
        end;
        if not SplitKeyValue(Parts[I], Key, Val) then
        begin Fail(Format('"%s" is not key=value', [Parts[I]])); Exit; end;
        if SameText(Key, 'amount') then
        begin
          if not ParseValue(Val, Amount) then
          begin Fail(Format('amount "%s" is not a number', [Val])); Exit; end;
        end
        else
        begin Fail(Format('connect does not understand "%s"', [Key])); Exit; end;
      end;

      if not AGraph.Connect(Q(Src), Q(Dst), Amount, Normalled) then
      begin Fail(AGraph.LastError); Exit; end;
      Continue;
    end;

    // ---- voices <n> ------------------------------------------------------
    if SameText(Verb, 'voices') then
    begin
      if APrefix <> '' then
      begin Fail('an included file cannot declare "voices": polyphony belongs to the instrument, not to a part of it'); Exit; end;
      if Length(Parts) < 2 then
      begin Fail('voices needs: voices <n>'); Exit; end;
      Result.Voices := StrToIntDef(Parts[1], 0);
      if Result.Voices < 1 then
      begin Fail(Format('"%s" is not a voice count', [Parts[1]])); Exit; end;
      Continue;
    end;

    // ---- output <module>.<port> [pos=x] [extent=m] ------------------------
    if SameText(Verb, 'output') then
    begin
      // An output is the finished instrument speaking. A part of one has no
      // business declaring where the whole thing radiates from.
      if APrefix <> '' then
      begin Fail('an included file cannot declare "output": the outputs belong to the instrument that includes it'); Exit; end;
      if Length(Parts) < 2 then
      begin Fail('output needs: output <module>.<port> [pos=x] [extent=m]'); Exit; end;
      OutPos := 0.0;
      for I := 2 to High(Parts) do
      begin
        if not SplitKeyValue(Parts[I], Key, Val) then
        begin Fail(Format('"%s" is not key=value', [Parts[I]])); Exit; end;
        if SameText(Key, 'pos') then
        begin
          if not ParseValue(Val, OutPos) then
          begin Fail(Format('pos "%s" is not a number', [Val])); Exit; end;
        end
        else if SameText(Key, 'extent') then
        begin
          if not ParseValue(Val, V) then
          begin Fail(Format('extent "%s" is not a number', [Val])); Exit; end;
          AGraph.Extent := V;
        end
        else
        begin Fail(Format('output takes pos= and extent=, not "%s"', [Key])); Exit; end;
      end;
      if not AGraph.AddOutputChannelAt(Parts[1], OutPos) then
      begin Fail(AGraph.LastError); Exit; end;
      Continue;
    end;

    // ---- include "file" as <prefix> [hash=xxxx] ---------------------------
    if SameText(Verb, 'include') then
    begin
      if ADepth >= 8 then
      begin Fail('include nested more than 8 deep — probably a cycle'); Exit; end;
      if (Length(Parts) < 4) or (not SameText(Parts[2], 'as')) then
      begin Fail('include needs: include "file.patch" as <prefix> [hash=xxxx]'); Exit; end;
      IncName := Parts[1];
      if (Length(IncName) >= 2) and (IncName[1] = '"') then
        IncName := Copy(IncName, 2, Length(IncName) - 2);
      IncPrefix := Parts[3];
      if APrefix <> '' then IncPrefix := APrefix + '.' + IncPrefix;

      if ABaseDir <> '' then IncPath := IncludeTrailingPathDelimiter(ABaseDir) + IncName
                        else IncPath := IncName;
      if not FileExists(IncPath) then
      begin Fail(Format('include: file not found: %s', [IncPath])); Exit; end;

      Inc2 := TStringList.Create;
      try
        Inc2.LoadFromFile(IncPath);
        Sum := PatchChecksum(Inc2.Text);
        // An expected checksum is optional. When it is there and no longer
        // matches, the file this patch was written against has moved on — that
        // may be a fix or may be a different instrument, and only a person can
        // tell. So it is said, not enforced.
        for I := 4 to High(Parts) do
          if SplitKeyValue(Parts[I], Key, Val) and SameText(Key, 'hash') then
            if not SameText(Trim(Val), Sum) then
              Result.Warnings := Result.Warnings +
                Format('include "%s" has changed since this patch was written '
                     + '(expected %s, found %s) — it may no longer sound the same'#10,
                       [IncName, Trim(Val), Sum]);
        Sub := LoadInto(AGraph, Inc2, IncPrefix, ExtractFilePath(IncPath),
                        IncPath, ADepth + 1);
        if not Sub.Success then
        begin
          Result.Success := False;
          Result.ErrorLine := L + 1;
          Result.ErrorText := Format('in %s line %d: %s',
                                     [IncName, Sub.ErrorLine, Sub.ErrorText]);
          Exit;
        end;
        Result.Warnings := Result.Warnings + Sub.Warnings;
        if Sub.ForceSampleRate then Result.ForceSampleRate := True;
      finally
        Inc2.Free;
      end;
      Continue;
    end;

    Fail(Format('unknown directive "%s" (expected include, voices, mode, module, set, connect or output)', [Verb]));
    Exit;
  end;
end;

function LoadPatchFromFile(AGraph: TSedaiPatchGraph;
  const AFilename: string): TSedaiPatchLoadResult;
var
  SL: TStringList;
begin
  if not FileExists(AFilename) then
  begin
    Result.Success := False;
    Result.ErrorLine := 0;
    Result.ErrorText := Format('patch file not found: %s', [AFilename]);
    Result.ForceSampleRate := False;
    Exit;
  end;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(AFilename);
    Result := LoadInto(AGraph, SL, '', ExtractFilePath(AFilename), AFilename, 0);
  finally
    SL.Free;
  end;
end;

end.
