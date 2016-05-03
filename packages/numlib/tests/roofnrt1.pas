program Roofnrte;

uses
  typ,
  roo;

type
  maxarray = array[1..128] of ArbFloat;
var
  n: ArbInt;
  a: ArbFloat;
  ah2: ArbFloat;

  procedure PraktikumEx(var x, fx: ArbFloat; var deff: boolean);
  var
    xloc: maxarray absolute x;
    floc: maxarray absolute fx;
    i:    ArbInt;
  begin
    floc[1] := 2 * (xloc[1] - xloc[2]) - ah2 * exp(xloc[1]);
    for i := 2 to n - 1 do
      floc[i] := -xloc[i - 1] + 2 * xloc[i] - xloc[i + 1] - ah2 * exp(xloc[i]);
    floc[n] := -xloc[n - 1] + 2 * xloc[n] - ah2 * exp(xloc[n]);
  end;

const
  m = 9;

  procedure NagExample(var x, fx: ArbFloat; var deff: boolean);
  var
    xloc: array[1..m] of ArbFloat absolute x;
    floc: array[1..m] of ArbFloat absolute fx;
    k:    ArbInt;
  begin
    floc[1] := 1 + (3 - 2 * xloc[1]) * xloc[1] - 2 * xloc[2];
    for k := 2 to m - 1 do
      floc[k] := 1 + (3 - 2 * xloc[k]) * xloc[k] - xloc[k - 1] - 2 * xloc[k + 1];
    floc[m] := 1 + (3 - 2 * xloc[m]) * xloc[m] - xloc[m - 1];
  end;

  procedure MatlabEx(var x, fx: ArbFloat; var deff: boolean);
  var
    xloc: array[1..3] of ArbFloat absolute x;
    floc: array[1..3] of ArbFloat absolute fx;
  begin
    floc[1] := sin(xloc[1]) + sqr(xloc[2]) + ln(xloc[3]) - 7;
    floc[2] := 3 * xloc[1] + exp(xloc[2] * ln(2)) - xloc[3] * sqr(xloc[3]) + 1;
    floc[3] := xloc[1] + xloc[2] + xloc[3] - 5;
  end;

  procedure TPNumlibEx(var x, fx: ArbFloat; var deff: boolean);
  begin
    fx := cos(x);
  end;

  procedure JdeJongEx(var x, fx: ArbFloat; var deff: boolean);
  begin
    if (x >= 0) and (x <= 1) then
      fx   := x - 2
    else
      deff := False;
  end;

  procedure Uitvoer(var x1: ArbFloat; n, step: ArbInt);
  var
    i:    ArbInt;
    xloc: maxarray absolute x1;
  begin
    i := 1;
    while (i <= n) do
    begin
      writeln(i: 5, ' ', xloc[i]: 20);
      Inc(i, step);
    end;
    writeln;
  end;

var
  x: ^maxarray;
  t, residu: ArbFloat;
  i, term: ArbInt;
begin

  { praktikum sommetje }

  n := 8;
  a := 0.50;
  repeat
    ah2 := a / sqr(n);
    GetMem(x, n * SizeOf(ArbFloat));

    for i := 1 to n do
      x^[i] := 0;

    writeln('Voorbeeld programma ''praktikum'',  resultaten voor n= ', n: 2);
    writeln;

    roofnr(@PraktikumEx, n, x^[1], residu, 1e-4, term);
    if term = 1 then
      writeln('    Norm van de residuen', residu: 20, #13#10,
        '    Berekende oplossing')
    else
      writeln('  Proces afgebroken term = ', term, #13#10,
        '  Laatst berekende waarden');
    writeln;
    Uitvoer(x^[1], n, n div 8);
    FreeMem(x, n * SizeOf(ArbFloat));
    n := n * 2
  until n = 128;

  { Nag procedure bibliotheek voorbeeld }

  GetMem(x, m * SizeOf(ArbFloat));

  for i := 1 to m do
    x^[i] := -1;

  writeln('Voorbeeld programma ''NAG-bibliotheek'' met m= ', m: 2);
  writeln;

  roofnr(@NagExample, m, x^[1], residu, 1e-6, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Uitvoer(x^[1], m, 1);
  FreeMem(x, m * SizeOf(ArbFloat));

  { Matlab voorbeeld uit handleiding }

  n := 3;

  GetMem(x, n * SizeOf(ArbFloat));

  for i := 1 to n do
    x^[i] := 1;

  writeln('Voorbeeld programma ''MATLAB handleiding'',  resultaten voor n= ', n: 2);
  writeln;

  roofnr(@MatlabEx, n, x^[1], residu, 1e-6, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Uitvoer(x^[1], n, 1);
  FreeMem(x, n * SizeOf(ArbFloat));

  { 1-dimensionaal voorbeeld uit TPNumlib }

  writeln('Voorbeeld programma ''TPNumlib'' voor ‚‚n dimensie');
  writeln;

  t := 1;
  roofnr(@TPNumlibEx, 1, t, residu, 1e-6, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Writeln('   ', t: 20);

  { Matlab voorbeeld uit handleiding }
  { dit moet fout gaan               }

  n := 3;

  GetMem(x, n * SizeOf(ArbFloat));

  for i := 1 to n do
    x^[i] := 1;

  writeln;
  writeln('Voorbeeld programma ''MATLAB handleiding'',  resultaten voor n= ', n: 2);
  writeln('Gaat niet goed want de relatieve fout is gelijk aan 0 gekozen');
  writeln;

  roofnr(@MatlabEx, n, x^[1], residu, 0, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Uitvoer(x^[1], n, 1);

  writeln;
  writeln('Voorbeeld programma ''MATLAB handleiding'',  resultaten voor n= ', n: 2);
  writeln;

  for i := 1 to n do
    x^[i] := 1;

  roofnr(@MatlabEx, n, x^[1], residu, 1e-8, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Uitvoer(x^[1], n, 1);
  FreeMem(x, n * SizeOf(ArbFloat));

  { 1-dimensionaal voorbeeld voor deff }

  writeln('Voorbeeld programma in ‚‚n dimensie, voor domein [0..1]');
  writeln;

  t := 0.5;
  roofnr(@JdeJongEx, 1, t, residu, 1e-6, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Writeln('   ', t: 20);

end.
program Roofnrte;

uses
  typ,
  roo;

type
  maxarray = array[1..128] of ArbFloat;
var
  n: ArbInt;
  a: ArbFloat;
  ah2: ArbFloat;

  procedure PraktikumEx(var x, fx: ArbFloat; var deff: boolean);
  var
    xloc: maxarray absolute x;
    floc: maxarray absolute fx;
    i:    ArbInt;
  begin
    floc[1] := 2 * (xloc[1] - xloc[2]) - ah2 * exp(xloc[1]);
    for i := 2 to n - 1 do
      floc[i] := -xloc[i - 1] + 2 * xloc[i] - xloc[i + 1] - ah2 * exp(xloc[i]);
    floc[n] := -xloc[n - 1] + 2 * xloc[n] - ah2 * exp(xloc[n]);
  end;

const
  m = 9;

  procedure NagExample(var x, fx: ArbFloat; var deff: boolean);
  var
    xloc: array[1..m] of ArbFloat absolute x;
    floc: array[1..m] of ArbFloat absolute fx;
    k:    ArbInt;
  begin
    floc[1] := 1 + (3 - 2 * xloc[1]) * xloc[1] - 2 * xloc[2];
    for k := 2 to m - 1 do
      floc[k] := 1 + (3 - 2 * xloc[k]) * xloc[k] - xloc[k - 1] - 2 * xloc[k + 1];
    floc[m] := 1 + (3 - 2 * xloc[m]) * xloc[m] - xloc[m - 1];
  end;

  procedure MatlabEx(var x, fx: ArbFloat; var deff: boolean);
  var
    xloc: array[1..3] of ArbFloat absolute x;
    floc: array[1..3] of ArbFloat absolute fx;
  begin
    floc[1] := sin(xloc[1]) + sqr(xloc[2]) + ln(xloc[3]) - 7;
    floc[2] := 3 * xloc[1] + exp(xloc[2] * ln(2)) - xloc[3] * sqr(xloc[3]) + 1;
    floc[3] := xloc[1] + xloc[2] + xloc[3] - 5;
  end;

  procedure TPNumlibEx(var x, fx: ArbFloat; var deff: boolean);
  begin
    fx := cos(x);
  end;

  procedure JdeJongEx(var x, fx: ArbFloat; var deff: boolean);
  begin
    if (x >= 0) and (x <= 1) then
      fx   := x - 2
    else
      deff := False;
  end;

  procedure Uitvoer(var x1: ArbFloat; n, step: ArbInt);
  var
    i:    ArbInt;
    xloc: maxarray absolute x1;
  begin
    i := 1;
    while (i <= n) do
    begin
      writeln(i: 5, ' ', xloc[i]: 20);
      Inc(i, step);
    end;
    writeln;
  end;

var
  x: ^maxarray;
  t, residu: ArbFloat;
  i, term: ArbInt;
begin

  { praktikum sommetje }

  n := 8;
  a := 0.50;
  repeat
    ah2 := a / sqr(n);
    GetMem(x, n * SizeOf(ArbFloat));

    for i := 1 to n do
      x^[i] := 0;

    writeln('Voorbeeld programma ''praktikum'',  resultaten voor n= ', n: 2);
    writeln;

    roofnr(@PraktikumEx, n, x^[1], residu, 1e-4, term);
    if term = 1 then
      writeln('    Norm van de residuen', residu: 20, #13#10,
        '    Berekende oplossing')
    else
      writeln('  Proces afgebroken term = ', term, #13#10,
        '  Laatst berekende waarden');
    writeln;
    Uitvoer(x^[1], n, n div 8);
    FreeMem(x, n * SizeOf(ArbFloat));
    n := n * 2
  until n = 128;

  { Nag procedure bibliotheek voorbeeld }

  GetMem(x, m * SizeOf(ArbFloat));

  for i := 1 to m do
    x^[i] := -1;

  writeln('Voorbeeld programma ''NAG-bibliotheek'' met m= ', m: 2);
  writeln;

  roofnr(@NagExample, m, x^[1], residu, 1e-6, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Uitvoer(x^[1], m, 1);
  FreeMem(x, m * SizeOf(ArbFloat));

  { Matlab voorbeeld uit handleiding }

  n := 3;

  GetMem(x, n * SizeOf(ArbFloat));

  for i := 1 to n do
    x^[i] := 1;

  writeln('Voorbeeld programma ''MATLAB handleiding'',  resultaten voor n= ', n: 2);
  writeln;

  roofnr(@MatlabEx, n, x^[1], residu, 1e-6, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Uitvoer(x^[1], n, 1);
  FreeMem(x, n * SizeOf(ArbFloat));

  { 1-dimensionaal voorbeeld uit TPNumlib }

  writeln('Voorbeeld programma ''TPNumlib'' voor ‚‚n dimensie');
  writeln;

  t := 1;
  roofnr(@TPNumlibEx, 1, t, residu, 1e-6, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Writeln('   ', t: 20);

  { Matlab voorbeeld uit handleiding }
  { dit moet fout gaan               }

  n := 3;

  GetMem(x, n * SizeOf(ArbFloat));

  for i := 1 to n do
    x^[i] := 1;

  writeln;
  writeln('Voorbeeld programma ''MATLAB handleiding'',  resultaten voor n= ', n: 2);
  writeln('Gaat niet goed want de relatieve fout is gelijk aan 0 gekozen');
  writeln;

  roofnr(@MatlabEx, n, x^[1], residu, 0, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Uitvoer(x^[1], n, 1);

  writeln;
  writeln('Voorbeeld programma ''MATLAB handleiding'',  resultaten voor n= ', n: 2);
  writeln;

  for i := 1 to n do
    x^[i] := 1;

  roofnr(@MatlabEx, n, x^[1], residu, 1e-8, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Uitvoer(x^[1], n, 1);
  FreeMem(x, n * SizeOf(ArbFloat));

  { 1-dimensionaal voorbeeld voor deff }

  writeln('Voorbeeld programma in ‚‚n dimensie, voor domein [0..1]');
  writeln;

  t := 0.5;
  roofnr(@JdeJongEx, 1, t, residu, 1e-6, term);
  if term = 1 then
    writeln('    Norm van de residuen', residu: 20, #13#10,
      '    Berekende oplossing')
  else
    writeln('  Proces afgebroken term = ', term, #13#10,
      '  Laatst berekende waarden');
  writeln;
  Writeln('   ', t: 20);

end.
