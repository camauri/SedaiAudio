program patch_fixture;
{$mode objfpc}{$H+}
// Sound fixtures: does any patch still sound the way it did?
//
// The fragile base class problem has no general solution — change a parent and
// every child changes, and nothing tells you which of those changes you meant.
// The same is true of an `include` that is still linked. So the goal is not to
// prevent it but to SEE it: every patch renders a short, fixed, deterministic
// take, and the take is compared with the one stored last time.
//
// The signature is a checksum AND three measurements, deliberately. A checksum
// alone says "something changed" and leaves you no wiser; peak, RMS and
// spectral centroid say whether it got louder, brighter, or merely moved by one
// bit in the last decimal.
//
//   patch_fixture            check every patch against references.txt
//   patch_fixture --update   rewrite references.txt (do this deliberately)
//   patch_fixture <file>     check one patch
uses
  SysUtils, Classes, Math, SedaiPatchGraph, SedaiPatchVoices;

const
  FIX_RATE  = 44100;
  FIX_BLOCK = 128;
  FIX_SECS  = 2.0;
  // Always the same notes, always the same moments: a fixture that varies
  // proves nothing.
  NOTE_ON: array[0..3] of Integer = (48, 55, 60, 64);
  ON_AT:   array[0..3] of Double  = (0.00, 0.25, 0.50, 0.75);
  OFF_AT:  array[0..3] of Double  = (0.90, 1.05, 1.20, 1.35);

type
  TSig = record
    Ok: Boolean;
    Sum: QWord;        // FNV-1a over the 16-bit samples
    Peak, RMS, Centroid: Double;
    Note: string;
  end;

function Measure(const A: array of Single; N: Integer): TSig;
var
  I, K, Bins, Win: Integer;
  S: SmallInt;
  Acc, Re, Im, W, Mag, MagSum, FreqSum, F: Double;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Sum := 14695981039346656037;
  Acc := 0.0;
  Result.Peak := 0.0;
  for I := 0 to N - 1 do
  begin
    // Quantise before hashing: the reference must not depend on bits that no
    // listener and no WAV file will ever carry.
    S := Round(Max(-1.0, Min(1.0, A[I])) * 32767.0);
    Result.Sum := (Result.Sum xor QWord(Word(S))) * 1099511628211;
    Acc := Acc + A[I] * A[I];
    if Abs(A[I]) > Result.Peak then Result.Peak := Abs(A[I]);
  end;
  Result.RMS := Sqrt(Acc / Max(N, 1));

  // A coarse DFT over a window at FULL rate. The first version decimated the
  // input by 8 to go faster and measured its own aliasing instead of the
  // timbre: every patch came out at 11 kHz, a bass and a bell alike, which is
  // how the mistake announced itself.
  Bins := 128;
  Win := Min(N, 32768);
  MagSum := 0.0; FreqSum := 0.0;
  for K := 1 to Bins do
  begin
    F := K * 8000.0 / Bins;           // 0..8 kHz: where a timbre actually lives
    Re := 0.0; Im := 0.0;
    for I := 0 to Win - 1 do
    begin
      W := 2.0 * Pi * F * I / FIX_RATE;
      Re := Re + A[I] * Cos(W);
      Im := Im - A[I] * Sin(W);
    end;
    Mag := Sqrt(Re * Re + Im * Im);
    MagSum := MagSum + Mag;
    FreqSum := FreqSum + F * Mag;
  end;
  if MagSum > 1.0e-12 then Result.Centroid := FreqSum / MagSum;
  Result.Ok := True;
end;

function RenderFixture(const APath: string; out ASig: TSig): Boolean;
var
  Pool: TSedaiPatchVoicePool;
  Buf: array of Single;
  Total, Done, I, K: Integer;
  Fired, Released: array[0..3] of Boolean;
  T: Double;
begin
  Result := False;
  FillChar(ASig, SizeOf(ASig), 0);
  Pool := TSedaiPatchVoicePool.Create;
  try
    if not Pool.LoadFromFile(APath, 6) then
    begin
      ASig.Note := 'non carica: ' + Pool.LastError;
      Exit;
    end;
    // An effect patch has no source of its own; rendering it would only
    // measure silence, and a fixture that is silent by construction proves
    // nothing about the effect.
    if Pool.HasAudioInput then
    begin
      ASig.Note := 'patch di effetto (ha un modulo input): saltata';
      Exit;
    end;
    Pool.Prepare(FIX_RATE, FIX_BLOCK);
    Pool.Reset;

    Total := Round(FIX_RATE * FIX_SECS);
    SetLength(Buf, Total);
    FillChar(Fired, SizeOf(Fired), 0);
    FillChar(Released, SizeOf(Released), 0);
    Done := 0;
    while Done < Total do
    begin
      T := Done / FIX_RATE;
      for K := 0 to 3 do
      begin
        if (not Fired[K]) and (T >= ON_AT[K]) then
        begin Pool.NoteOn(NOTE_ON[K]); Fired[K] := True; end;
        if (not Released[K]) and (T >= OFF_AT[K]) then
        begin Pool.NoteOff(NOTE_ON[K]); Released[K] := True; end;
      end;
      Pool.Render(FIX_BLOCK);
      for I := 0 to FIX_BLOCK - 1 do
        if Done + I < Total then Buf[Done + I] := Pool.MixSample(0, I);
      Inc(Done, FIX_BLOCK);
    end;
    ASig := Measure(Buf, Total);
    Result := ASig.Ok;
  finally
    Pool.Free;
  end;
end;

var
  Refs: TStringList;
  Names: TStringList;
  Update: Boolean = False;
  Only: string = '';
  Changed, Checked, Newly, Skipped: Integer;

function RefKey(const AName: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(AName), '');
end;

// FPC's Format has no '+' flag — "%+.1f" prints the number and then the
// literal ".1f", which is how this was found. The sign goes on by hand.
function Signed(V: Double; ADec: Integer = 1): string;
begin
  Result := FormatFloat('0.' + StringOfChar('0', ADec), V);
  if V >= 0 then Result := '+' + Result;
end;

function Fmt(const S: TSig): string;
begin
  Result := Format('%.16x %.6f %.6f %.1f', [S.Sum, S.Peak, S.RMS, S.Centroid]);
end;

procedure CheckOne(const APath: string);
var
  S: TSig;
  Key, Line, Old: string;
  P: TStringArray;
  OP, OR_, OC: Double;
  FS: TFormatSettings;
begin
  Key := RefKey(APath);
  if not RenderFixture(APath, S) then
  begin
    if S.Note <> '' then
    begin
      WriteLn(Format('  %-20s  %s', [Key, S.Note]));
      Inc(Skipped);
    end;
    Exit;
  end;
  Inc(Checked);
  Line := Fmt(S);
  Old := Refs.Values[Key];
  if Old = '' then
  begin
    if Update then Refs.Values[Key] := Line;
    WriteLn(Format('  %-20s  NUOVA   picco %.3f  RMS %.3f  centroide %.0f Hz',
                   [Key, S.Peak, S.RMS, S.Centroid]));
    Inc(Newly);
    Exit;
  end;
  // NIENTE scorciatoia per --update: anche riscrivendo i riferimenti si passa
  // per il confronto, perche' un aggiornamento che non dice cosa ha cambiato e'
  // un aggiornamento di cui nessuno puo' rispondere.
  if Old = Line then Exit;             // identica: silenzio, come deve essere

  FS := DefaultFormatSettings; FS.DecimalSeparator := '.';
  P := Old.Split([' ']);
  OP := 0; OR_ := 0; OC := 0;
  if Length(P) >= 4 then
  begin
    TryStrToFloat(P[1], OP, FS); TryStrToFloat(P[2], OR_, FS); TryStrToFloat(P[3], OC, FS);
  end;
  WriteLn(Format('  %-20s  CAMBIATA', [Key]));
  WriteLn(Format('       picco     %8.4f -> %8.4f   (%s dB)',
    [OP, S.Peak, Signed(20 * Log10(Max(S.Peak, 1e-9) / Max(OP, 1e-9)))]));
  WriteLn(Format('       RMS       %8.4f -> %8.4f   (%s dB)',
    [OR_, S.RMS, Signed(20 * Log10(Max(S.RMS, 1e-9) / Max(OR_, 1e-9)))]));
  WriteLn(Format('       centroide %8.0f -> %8.0f Hz (%s %%)',
    [OC, S.Centroid, Signed(100.0 * (S.Centroid - OC) / Max(OC, 1e-9), 0)]));
  // SOLO con --update. Scriverlo qui incondizionatamente, com'era prima,
  // significava che la guardia segnalava la differenza una volta e poi la
  // registrava come nuovo riferimento: la seconda esecuzione diceva "tutto a
  // posto" e la regressione era sparita senza che nessuno decidesse nulla.
  if Update then Refs.Values[Key] := Line;
  Inc(Changed);
end;

var
  RefFile: string;
  I: Integer;
  Rec: TSearchRec;
begin
  for I := 1 to ParamCount do
    if ParamStr(I) = '--update' then Update := True else Only := ParamStr(I);

  RefFile := 'library/patches/fixtures.txt';
  Refs := TStringList.Create;
  Names := TStringList.Create;
  try
    Refs.NameValueSeparator := '=';
    if FileExists(RefFile) then Refs.LoadFromFile(RefFile);

    if Only <> '' then Names.Add(Only)
    else
    begin
      if FindFirst('library/patches/*.patch', faAnyFile, Rec) = 0 then
      begin
        repeat Names.Add('library/patches/' + Rec.Name);
        until FindNext(Rec) <> 0;
        FindClose(Rec);
      end;
      Names.Sort;
    end;

    WriteLn;
    WriteLn('  fixture sonore — ', FIX_SECS:0:1, ' s, ', FIX_RATE, ' Hz, blocchi da ', FIX_BLOCK);
    WriteLn;
    Changed := 0; Checked := 0; Newly := 0; Skipped := 0;
    for I := 0 to Names.Count - 1 do CheckOne(Names[I]);

    // Il file di riferimento si tocca soltanto quando lo si chiede. E' l'unica
    // cosa che rende la guardia una guardia.
    if Update then
    begin
      Refs.Sort;
      Refs.SaveToFile(RefFile);
    end;
    WriteLn;
    WriteLn(Format('  %d verificate, %d cambiate, %d nuove, %d saltate',
                   [Checked, Changed, Newly, Skipped]));
    if Update then
      WriteLn('  riferimenti riscritti.')
    else
    begin
      if Changed > 0 then
        WriteLn('  >>> se il cambiamento era voluto: patch_fixture --update');
      if Newly > 0 then
        WriteLn('  >>> per registrare le nuove:      patch_fixture --update');
    end;
    if (Changed > 0) and (not Update) then Halt(1);
  finally
    Names.Free;
    Refs.Free;
  end;
end.
