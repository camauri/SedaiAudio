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
//   connect lfo1.out -> osc1.pitch amount=0.02
//   output  amp.out
//
// Numbers accept unit suffixes: 440Hz, 2ms, 120ms, 50%, plain floats.
//
// Part of Sedai Audio Foundation. GPL-3.0.
// ============================================================================
unit SedaiPatchFile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, SedaiPatchGraph, SedaiPatchModules;

type
  TSedaiPatchLoadResult = record
    Success: Boolean;
    ErrorLine: Integer;
    ErrorText: string;
    ForceSampleRate: Boolean;
  end;

// Parse APatch (already-loaded text) into AGraph. The graph is NOT compiled
// here — the caller does that, so it can decide what to do with the mode flag.
function LoadPatchFromStrings(AGraph: TSedaiPatchGraph;
                              AText: TStrings): TSedaiPatchLoadResult;
function LoadPatchFromFile(AGraph: TSedaiPatchGraph;
                           const AFilename: string): TSedaiPatchLoadResult;

// Exposed because the renderer wants the same suffix rules on the command line.
function ParseValue(const AText: string; out AValue: Single): Boolean;

implementation

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

function LoadPatchFromStrings(AGraph: TSedaiPatchGraph;
  AText: TStrings): TSedaiPatchLoadResult;
var
  L, I, P: Integer;
  Line, Verb, Rest, Key, Val, Src, Dst: string;
  Parts: TStringArray;
  M: TSedaiPatchModule;
  V, Amount: Single;

  procedure Fail(const AMsg: string);
  begin
    Result.Success := False;
    Result.ErrorLine := L + 1;
    Result.ErrorText := AMsg;
  end;

begin
  Result.Success := True;
  Result.ErrorLine := 0;
  Result.ErrorText := '';
  Result.ForceSampleRate := False;

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
      Parts := Val.Split([' ', #9], TStringSplitOptions.ExcludeEmpty);
      if Length(Parts) = 0 then
      begin Fail('module needs a type'); Exit; end;

      M := CreateModuleByType(Parts[0]);
      if M = nil then
      begin
        Fail(Format('unknown module type "%s" (known: %s)', [Parts[0], KnownModuleTypes]));
        Exit;
      end;
      if not AGraph.AddModule(M, Key) then
      begin M.Free; Fail(AGraph.LastError); Exit; end;

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
      if not AGraph.SetValue(Key, V) then
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
      for I := 1 to High(Parts) do
      begin
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

      if not AGraph.Connect(Src, Dst, Amount) then
      begin Fail(AGraph.LastError); Exit; end;
      Continue;
    end;

    // ---- output <module>.<port> ------------------------------------------
    if SameText(Verb, 'output') then
    begin
      if Length(Parts) < 2 then
      begin Fail('output needs: output <module>.<port>'); Exit; end;
      if not AGraph.SetOutput(Parts[1]) then
      begin Fail(AGraph.LastError); Exit; end;
      Continue;
    end;

    Fail(Format('unknown directive "%s" (expected module, set, connect, output or mode)', [Verb]));
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
    Result := LoadPatchFromStrings(AGraph, SL);
  finally
    SL.Free;
  end;
end;

end.
