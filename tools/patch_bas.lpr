program patch_bas;
// ============================================================================
// The other direction: from a .patch back to SedaiBasic MODERN.
//
//   patch_bas --lib                 the MODERN library, GENERATED from the registry
//   patch_bas <file.patch>          the MODERN program that reproduces that patch
//
// Why both live in one tool: the two halves have to agree on how a module type
// becomes a type name and how a port becomes a field. Written twice they would
// agree until the day they did not, and the failure would be a program that
// compiles and names something that is not there.
//
// The library is generated the way patch_doc generates the reference — by
// building every module and asking it — so it cannot describe an engine that
// does not exist. Add a module to SAF and it appears here on the next run.
//
// WHAT THIS IS FOR. A round trip:  .patch -> .bas -> sb -> .patch'  and then
// requiring .patch' to RENDER IDENTICALLY to .patch. That exercises the whole
// bridge over the real library instead of over one hand-made example, and any
// module type the MODERN side cannot express shows up at once.
//
// ⚠️ sb EXITS 0 EVEN WHEN IT FAILS. `sb x.bas > out.patch` on a broken program
// writes the error message into out.patch and reports success. Anything that
// automates this must look at the OUTPUT, never at the exit status.
// ============================================================================
{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Math,
  SedaiPatchGraph, SedaiPatchModules, SedaiPatchElectronic,
  SedaiPatchInstruments, SedaiPatchPart, SedaiPatchSpace, SedaiPatchBody,
  SedaiPatchLegacy;

// ---------------------------------------------------------------------------
// Naming. The one place both halves agree.
// ---------------------------------------------------------------------------

// A .patch type name becomes a MODERN type name. Three of them collide with
// classic BASIC commands and are renamed; the rest are simply capitalised.
// Measured against sb 2.0, not guessed: every other type name and every one of
// the 34 port names is accepted as written.
function TypeNameOf(const AType: string): string;
begin
  if SameText(AType, 'input') then Exit('AudioIn');    // INPUT is a BASIC command
  if SameText(AType, 'width') then Exit('Widener');    // WIDTH is a BASIC command
  if SameText(AType, 'space') then Exit('Room');       // SPACE$ is a BASIC function
  Result := UpperCase(Copy(AType, 1, 1)) + Copy(AType, 2, Length(AType));
end;

// A port name becomes a field name. Capitalised, and that is enough.
function FieldNameOf(const APort: string): string;
begin
  Result := UpperCase(Copy(APort, 1, 1)) + Copy(APort, 2, Length(APort));
end;

// A module name in a .patch becomes a variable name. Dots appear in included
// modules (core.f1) and are not legal in an identifier.
function VarNameOf(const AName: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AName) do
    if AName[I] = '.' then Result := Result + '_' else Result := Result + AName[I];
  Result := 'm_' + Result;
end;

function MakeModule(const AType: string): TSedaiPatchModule;
begin
  Result := CreateModuleByType(AType);
  if Result = nil then Result := CreateElectronicModuleByType(AType);
  if Result = nil then Result := CreateInstrumentModuleByType(AType);
  if Result = nil then Result := CreatePartModuleByType(AType);
  if Result = nil then Result := CreateSpaceModuleByType(AType);
  if Result = nil then Result := CreateBodyModuleByType(AType);
  if Result = nil then Result := CreateLegacyModuleByType(AType);
end;

function AllTypes: TStringList;

  procedure AddList(const AList: string);
  var
    P: TStringArray;
    I, K: Integer;
    S: string;
  begin
    P := AList.Split([',']);
    for I := 0 to High(P) do
    begin
      S := Trim(P[I]);
      // A registry entry may carry its usage after the name —
      //   inst (instrument=<name> | source=<technique> ...)
      // — and that text contains commas of its own. Keep the name only.
      K := Pos(' ', S);  if K > 0 then S := Copy(S, 1, K - 1);
      K := Pos('(', S);  if K > 0 then S := Copy(S, 1, K - 1);
      S := Trim(S);
      if (S <> '') and (Result.IndexOf(S) < 0) then Result.Add(S);
    end;
  end;

begin
  Result := TStringList.Create;
  AddList(KnownModuleTypes);
  AddList(KnownElectronicTypes);
  AddList(KnownInstrumentTypes);
  AddList(KnownPartTypes);
  AddList(KnownSpaceTypes);
  AddList(KnownBodyTypes);
  AddList(KnownLegacyTypes);
end;

// ---------------------------------------------------------------------------
// --lib : the MODERN library, from the registry
// ---------------------------------------------------------------------------
procedure EmitLibrary;
var
  Types: TStringList;
  M: TSedaiPatchModule;
  P: TSedaiPatchPort;
  I, J: Integer;
  T, Keys: string;
begin
  WriteLn(''''' ============================================================================');
  WriteLn(''''' saf.bas — describe a SAF instrument in SedaiBasic MODERN, and print a .patch.');
  WriteLn(''''' ');
  WriteLn(''''' GENERATED by patch_bas --lib. Do not edit: it is produced by building every');
  WriteLn(''''' module in the registry and asking it what ports it has, so it cannot');
  WriteLn(''''' describe an engine that does not exist. Regenerate after adding a module.');
  WriteLn(''''' ');
  WriteLn(''''' THE CONTRACT IS TEXT. This library knows nothing about SAF: it records what');
  WriteLn(''''' was declared and prints it. SAF knows nothing about SedaiBasic: it reads a');
  WriteLn(''''' .patch, which is what it already read. Neither project links the other.');
  WriteLn(''''' ');
  WriteLn(''''' Modules are held as POINTERS. That is not a workaround for the VM''s trouble');
  WriteLn(''''' with arrays of composite objects inside a Type: an array declared As Osc');
  WriteLn(''''' could not hold a Saw either, so anything polymorphic wants pointers anyway.');
  WriteLn(''''' ============================================================================');
  WriteLn;
  WriteLn('Const SAF_MAX_MOD  As Integer = 256');
  WriteLn('Const SAF_MAX_SET  As Integer = 512');
  WriteLn('Const SAF_MAX_LINK As Integer = 1024');
  WriteLn('Const SAF_MAX_OUT  As Integer = 16');
  WriteLn;
  WriteLn('Dim Shared As String  safModName(0 To 255)');
  WriteLn('Dim Shared As String  safModType(0 To 255)');
  WriteLn('Dim Shared As String  safModKeys(0 To 255)');
  WriteLn('Dim Shared As Integer safModN');
  WriteLn;
  WriteLn('Dim Shared As String  safSetPath(0 To 511)');
  WriteLn('Dim Shared As Double  safSetVal(0 To 511)');
  WriteLn('Dim Shared As Integer safSetN');
  WriteLn;
  WriteLn('Dim Shared As String  safLinkSrc(0 To 1023)');
  WriteLn('Dim Shared As String  safLinkDst(0 To 1023)');
  WriteLn('Dim Shared As Double  safLinkAmt(0 To 1023)');
  WriteLn('Dim Shared As Integer safLinkNorm(0 To 1023)');
  WriteLn('Dim Shared As Integer safLinkN');
  WriteLn;
  WriteLn('Dim Shared As String  safOutPath(0 To 15)');
  WriteLn('Dim Shared As Double  safOutPos(0 To 15)');
  WriteLn('Dim Shared As Integer safOutHasPos(0 To 15)');
  WriteLn('Dim Shared As Integer safOutN');
  WriteLn('Dim Shared As Double  safExtent');
  WriteLn('Dim Shared As Integer safVoices');
  WriteLn('Dim Shared As String  safTitle');
  WriteLn;
  WriteLn('Function SafDeclare(nm As String, ty As String) As Integer');
  WriteLn('  safModName(safModN) = nm');
  WriteLn('  safModType(safModN) = ty');
  WriteLn('  safModKeys(safModN) = ""');
  WriteLn('  safModN = safModN + 1');
  WriteLn('  Return safModN - 1');
  WriteLn('End Function');
  WriteLn;
  WriteLn('Sub SafSet(path As String, v As Double)');
  WriteLn('  safSetPath(safSetN) = path');
  WriteLn('  safSetVal(safSetN) = v');
  WriteLn('  safSetN = safSetN + 1');
  WriteLn('End Sub');
  WriteLn;
  WriteLn('Sub SafLink(src As String, dst As String, amount As Double, normalled As Integer)');
  WriteLn('  safLinkSrc(safLinkN) = src');
  WriteLn('  safLinkDst(safLinkN) = dst');
  WriteLn('  safLinkAmt(safLinkN) = amount');
  WriteLn('  safLinkNorm(safLinkN) = normalled');
  WriteLn('  safLinkN = safLinkN + 1');
  WriteLn('End Sub');
  WriteLn;
  WriteLn('Sub SafOutput(path As String)');
  WriteLn('  safOutPath(safOutN) = path');
  WriteLn('  safOutHasPos(safOutN) = 0');
  WriteLn('  safOutN = safOutN + 1');
  WriteLn('End Sub');
  WriteLn;
  // `pos` is reserved (POS is the classic cursor-column function) and a
  // parameter may not be one. A FIELD may: it is always reached after a dot,
  // so there is nothing to be ambiguous with — which is why the generated port
  // fields In, Out and Pos are all fine as they are.
  WriteLn('Sub SafOutputAt(path As String, atPos As Double)');
  WriteLn('  safOutPath(safOutN) = path');
  WriteLn('  safOutPos(safOutN) = atPos');
  WriteLn('  safOutHasPos(safOutN) = 1');
  WriteLn('  safOutN = safOutN + 1');
  WriteLn('End Sub');
  WriteLn;
  WriteLn(''''' The one mechanism: a port is a knob plus any number of sources, summed.');
  WriteLn('Type Port');
  WriteLn('  Public:');
  WriteLn('    Path As String');
  WriteLn('    Declare Sub Add(ByRef src As Port, amount As Double = 1.0)');
  WriteLn('    Declare Sub AddNormalled(ByRef src As Port, amount As Double = 1.0)');
  WriteLn('    Declare Property Value() As Double');
  WriteLn('    Declare Property Value(v As Double)');
  WriteLn('  Private:');
  WriteLn('    Knob As Double');
  WriteLn('End Type');
  WriteLn;
  WriteLn('Sub Port.Add(ByRef src As Port, amount As Double = 1.0)');
  WriteLn('  SafLink(src.Path, This.Path, amount, 0)');
  WriteLn('End Sub');
  WriteLn;
  WriteLn('Sub Port.AddNormalled(ByRef src As Port, amount As Double = 1.0)');
  WriteLn('  SafLink(src.Path, This.Path, amount, 1)');
  WriteLn('End Sub');
  WriteLn;
  WriteLn('Property Port.Value() As Double');
  WriteLn('  Return This.Knob');
  WriteLn('End Property');
  WriteLn;
  WriteLn('Property Port.Value(v As Double)');
  WriteLn('  This.Knob = v');
  WriteLn('  SafSet(This.Path, v)');
  WriteLn('End Property');
  WriteLn;
  WriteLn(''''' Every module knows its own name, so nothing anywhere spells "osc1.pitch".');
  WriteLn('Type Module Extends Object');
  WriteLn('  Public:');
  WriteLn('    Nm  As String');
  WriteLn('    Idx As Integer');
  WriteLn('    Out As Port');
  WriteLn('    Declare Sub Key(k As String, v As String)');
  WriteLn('    Declare Sub KeyN(k As String, v As Double)');
  WriteLn('End Type');
  WriteLn;
  WriteLn('Sub Module.Key(k As String, v As String)');
  WriteLn('  safModKeys(This.Idx) = safModKeys(This.Idx) + " " + k + "=" + v');
  WriteLn('End Sub');
  WriteLn;
  WriteLn('Sub Module.KeyN(k As String, v As Double)');
  WriteLn('  safModKeys(This.Idx) = safModKeys(This.Idx) + " " + k + "=" + Str(v)');
  WriteLn('End Sub');

  Types := AllTypes;
  try
    for I := 0 to Types.Count - 1 do
    begin
      T := Types[I];
      M := MakeModule(T);
      if M = nil then Continue;
      try
        M.Prepare(44100, 64);
        WriteLn;
        Keys := M.ConfigKeys;
        if Keys <> '' then
          WriteLn(''''' ', T, ' — declaration keys: ', Keys)
        else
          WriteLn(''''' ', T);
        WriteLn('Type ', TypeNameOf(T), ' Extends Module');
        WriteLn('  Public:');
        for J := 0 to M.PortCount - 1 do
        begin
          P := M.Port(J);
          // `out` lives on Module; a second output keeps its own name.
          if SameText(P.Name, 'out') and (P.Kind = pkOutput) then Continue;
          WriteLn('    ', FieldNameOf(P.Name), ' As Port');
        end;
        WriteLn('    Declare Sub Init(nm As String)');
        WriteLn('End Type');
        WriteLn;
        WriteLn('Sub ', TypeNameOf(T), '.Init(nm As String)');
        WriteLn('  This.Idx = SafDeclare(nm, "', T, '")');
        WriteLn('  This.Nm = nm');
        WriteLn('  This.Out.Path = nm + ".out"');
        for J := 0 to M.PortCount - 1 do
        begin
          P := M.Port(J);
          if SameText(P.Name, 'out') and (P.Kind = pkOutput) then Continue;
          WriteLn('  This.', FieldNameOf(P.Name), '.Path = nm + ".', P.Name, '"');
        end;
        WriteLn('End Sub');
      finally
        M.Free;
      end;
    end;
  finally
    Types.Free;
  end;

  WriteLn;
  WriteLn(''''' Printing. One place, at the end, so declarations may come in any order.');
  WriteLn('Sub SafEmit()');
  WriteLn('  Dim As Integer i');
  WriteLn('  If safTitle <> "" Then Print "# " + safTitle');
  WriteLn('  Print "# Generated from SedaiBasic MODERN. Do not edit: edit the .bas."');
  WriteLn('  If safVoices > 0 Then');
  WriteLn('    Print ""');
  WriteLn('    Print "voices " + Str(safVoices)');
  WriteLn('  End If');
  WriteLn('  Print ""');
  WriteLn('  For i = 0 To safModN - 1');
  WriteLn('    Print "module " + safModName(i) + " = " + safModType(i) + safModKeys(i)');
  WriteLn('  Next');
  WriteLn('  If safSetN > 0 Then Print ""');
  WriteLn('  For i = 0 To safSetN - 1');
  WriteLn('    Print "set " + safSetPath(i) + " = " + Str(safSetVal(i))');
  WriteLn('  Next');
  WriteLn('  Print ""');
  WriteLn('  For i = 0 To safLinkN - 1');
  WriteLn('    Dim As String s');
  WriteLn('    s = "connect " + safLinkSrc(i) + " -> " + safLinkDst(i)');
  WriteLn('    If safLinkAmt(i) <> 1.0 Then s = s + " amount=" + Str(safLinkAmt(i))');
  WriteLn('    If safLinkNorm(i) = 1 Then s = s + " normalled"');
  WriteLn('    Print s');
  WriteLn('  Next');
  WriteLn('  Print ""');
  WriteLn('  For i = 0 To safOutN - 1');
  WriteLn('    Dim As String o');
  WriteLn('    o = "output " + safOutPath(i)');
  WriteLn('    If safOutHasPos(i) = 1 Then o = o + " pos=" + Str(safOutPos(i))');
  WriteLn('    If (i = 0) And (safExtent > 0.0) Then o = o + " extent=" + Str(safExtent)');
  WriteLn('    Print o');
  WriteLn('  Next');
  WriteLn('End Sub');
end;

// ---------------------------------------------------------------------------
// <file.patch> : the MODERN program that reproduces it
//
// The .patch is read here as TEXT rather than through the graph, and that is a
// decision. A compiled graph has already thrown away what this needs: a
// connection's amount is not readable back, and a declaration key like
// shape=saw has become a private field with no way to ask for it. Reading the
// text is the only way to recover a patch WHOLE — and the round trip is what
// proves the reading was right.
// ---------------------------------------------------------------------------
type
  TDecl = record
    Name, Kind: string;
  end;

var
  Decls: array of TDecl;

function KindOfModule(const AName: string): string;
var
  I: Integer;
begin
  for I := 0 to High(Decls) do
    if SameText(Decls[I].Name, AName) then Exit(Decls[I].Kind);
  Result := '';
end;

// "osc1.pitch" -> variable "m_osc1", field "Pitch"
function PathToBas(const APath: string): string;
var
  D: Integer;
  M, P: string;
begin
  D := LastDelimiter('.', APath);
  if D <= 0 then Exit('?' + APath);
  M := Copy(APath, 1, D - 1);
  P := Copy(APath, D + 1, Length(APath));
  Result := VarNameOf(M) + '->' + FieldNameOf(P);
end;

// Split a line into words, honouring double quotes so that
// `inst instrument="Drawbar Organ"` survives.
function SplitWords(const ALine: string): TStringArray;
var
  I, N: Integer;
  Cur: string;
  InQ: Boolean;
begin
  SetLength(Result, 0);
  N := 0; Cur := ''; InQ := False;
  for I := 1 to Length(ALine) do
  begin
    if ALine[I] = '"' then begin InQ := not InQ; Cur := Cur + '"'; Continue; end;
    if (not InQ) and (ALine[I] in [' ', #9]) then
    begin
      if Cur <> '' then
      begin
        SetLength(Result, N + 1); Result[N] := Cur; Inc(N); Cur := '';
      end;
    end
    else
      Cur := Cur + ALine[I];
  end;
  if Cur <> '' then begin SetLength(Result, N + 1); Result[N] := Cur; end;
end;

function NumLit(const AText: string): string;
var
  V: Double;
  FS: TFormatSettings;
begin
  FS := DefaultFormatSettings; FS.DecimalSeparator := '.';
  if TryStrToFloat(AText, V, FS) then
  begin
    Result := FloatToStrF(V, ffGeneral, 15, 0, FS);
    if Pos('.', Result) = 0 then Result := Result + '.0';
  end
  else
    Result := AText;
end;

procedure EmitProgram(const AFile: string);
var
  Src: TStringList;
  I, J, K, Eq: Integer;
  Line, Cmd, S, Nm, Ty, Src1, Dst, Amt: string;
  W: TStringArray;
  Body: TStringList;
  Norm: Boolean;
  Pos_: string;
begin
  Src := TStringList.Create;
  Body := TStringList.Create;
  SetLength(Decls, 0);
  try
    Src.LoadFromFile(AFile);

    // An `include` brings in modules this reader never sees declared, so every
    // connection to them would dangle. Refusing is the only honest answer: a
    // program that looks right and is not is worse than no program.
    for I := 0 to Src.Count - 1 do
    begin
      Line := Trim(Src[I]);
      K := Pos('#', Line); if K > 0 then Line := Trim(Copy(Line, 1, K - 1));
      if LowerCase(Copy(Line, 1, 8)) = 'include ' then
      begin
        WriteLn(StdErr, ExtractFileName(AFile),
                ': usa `include`, che questo traduttore non sa ancora seguire.');
        WriteLn(StdErr, '  I moduli inclusi non verrebbero dichiarati e le connessioni');
        WriteLn(StdErr, '  a loro resterebbero appese. Niente in uscita, di proposito.');
        Halt(3);
      end;
    end;

    // pass 1: the declarations, so that pass 2 can name variables
    for I := 0 to Src.Count - 1 do
    begin
      Line := Trim(Src[I]);
      K := Pos('#', Line); if K > 0 then Line := Trim(Copy(Line, 1, K - 1));
      if Line = '' then Continue;
      W := SplitWords(Line);
      if Length(W) = 0 then Continue;
      // Both `module f = filter` and `module f= filter` are legal in a .patch,
      // and the library uses both. Requiring the space was a difference between
      // this reader and the real one — which is exactly what the round trip is
      // for: it turned into a variable called `m_filtR=`.
      if SameText(W[0], 'module') and (Length(W) >= 3) then
      begin
        SetLength(Decls, Length(Decls) + 1);
        if (Length(W[1]) > 0) and (W[1][Length(W[1])] = '=') then
        begin
          Decls[High(Decls)].Name := Copy(W[1], 1, Length(W[1]) - 1);
          Decls[High(Decls)].Kind := W[2];
        end
        else if (Length(W) >= 4) and (W[2] = '=') then
        begin
          Decls[High(Decls)].Name := W[1];
          Decls[High(Decls)].Kind := W[3];
        end
        else
          SetLength(Decls, Length(Decls) - 1);
      end;
    end;

    WriteLn(''''' ', ExtractFileName(AFile), ' — lifted from the .patch by patch_bas.');
    WriteLn(''''' Round trip:  sb <this> > out.patch  must render identically.');
    WriteLn('#include "library/instruments/saf.bas"');
    WriteLn;
    for I := 0 to High(Decls) do
      WriteLn('Dim As ', TypeNameOf(Decls[I].Kind), ' Ptr ', VarNameOf(Decls[I].Name));
    WriteLn;
    for I := 0 to High(Decls) do
      WriteLn(VarNameOf(Decls[I].Name), ' = New ', TypeNameOf(Decls[I].Kind),
              ' : ', VarNameOf(Decls[I].Name), '->Init("', Decls[I].Name, '")');
    WriteLn;

    // pass 2: everything else, IN ORDER — the order of connections into one
    // port is part of the sound, not a formatting detail.
    for I := 0 to Src.Count - 1 do
    begin
      Line := Trim(Src[I]);
      K := Pos('#', Line); if K > 0 then Line := Trim(Copy(Line, 1, K - 1));
      if Line = '' then Continue;
      W := SplitWords(Line);
      if Length(W) = 0 then Continue;
      Cmd := LowerCase(W[0]);

      if Cmd = 'module' then
      begin
        // the declaration keys, if any. Where they start depends on whether the
        // equals sign was glued to the name.
        if (Length(W) > 1) and (Length(W[1]) > 0) and (W[1][Length(W[1])] = '=') then
        begin
          Nm := Copy(W[1], 1, Length(W[1]) - 1);
          K := 3;
        end
        else
        begin
          Nm := W[1];
          K := 4;
        end;
        for J := K to High(W) do
        begin
          Eq := Pos('=', W[J]);
          if Eq <= 0 then Continue;
          Src1 := Copy(W[J], 1, Eq - 1);
          S    := Copy(W[J], Eq + 1, Length(W[J]));
          // The value is passed through as WRITTEN, always as a string. Turning
          // 110 into a number and back would go through two formatters and is
          // exactly how a round trip stops being one.
          if (Length(S) >= 2) and (S[1] = '"') then
            WriteLn(VarNameOf(Nm), '->Key("', Src1, '", ', S, ')')
          else
            WriteLn(VarNameOf(Nm), '->Key("', Src1, '", "', S, '")');
        end;
      end
      else if Cmd = 'voices' then
        WriteLn('safVoices = ', W[1])
      else if Cmd = 'set' then
      begin
        // set <path> = <value>
        if (Length(W) >= 4) and (W[2] = '=') then
          WriteLn(PathToBas(W[1]), '.Value = ', NumLit(W[3]));
      end
      else if Cmd = 'connect' then
      begin
        if (Length(W) < 4) or (W[2] <> '->') then Continue;
        Src1 := W[1]; Dst := W[3]; Amt := ''; Norm := False;
        for J := 4 to High(W) do
        begin
          if SameText(W[J], 'normalled') then Norm := True
          else if LowerCase(Copy(W[J], 1, 7)) = 'amount=' then
            Amt := NumLit(Copy(W[J], 8, Length(W[J])));
        end;
        S := PathToBas(Dst);
        if Norm then S := S + '.AddNormalled(' else S := S + '.Add(';
        S := S + PathToBas(Src1);
        if Amt <> '' then S := S + ', ' + Amt;
        WriteLn(S + ')');
      end
      else if Cmd = 'output' then
      begin
        Pos_ := '';
        for J := 2 to High(W) do
          if LowerCase(Copy(W[J], 1, 4)) = 'pos=' then
            Pos_ := NumLit(Copy(W[J], 5, Length(W[J])))
          else if LowerCase(Copy(W[J], 1, 7)) = 'extent=' then
            WriteLn('safExtent = ', NumLit(Copy(W[J], 8, Length(W[J]))));
        if Pos_ <> '' then
          WriteLn('SafOutputAt("', W[1], '", ', Pos_, ')')
        else
          WriteLn('SafOutput("', W[1], '")');
      end;
    end;

    WriteLn;
    WriteLn('SafEmit()');
  finally
    Body.Free;
    Src.Free;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('usage: patch_bas --lib            la libreria MODERN, dal registro');
    WriteLn('       patch_bas <file.patch>     il programma MODERN che la riproduce');
    Halt(2);
  end;
  if ParamStr(1) = '--lib' then EmitLibrary
  else if not FileExists(ParamStr(1)) then
  begin
    WriteLn(StdErr, ParamStr(1), ': non trovato');
    Halt(1);
  end
  else
    EmitProgram(ParamStr(1));
end.
