program eiggs3te;

uses
  typ,
  iom,
  omv,
  eig;

const
  m1 = -10;
  m2 = 10;
  m3 = -4;
  m4 = 15;
  n1 = -5;
  n2 = 10;
  n3 = -3;
  n4 = 10;
var
  i, j, ex, nex, i1, j1, i2, j2, n, term: ArbInt;
  a:    array[m1..m2, n1..n2] of ArbFloat;
  x, e: array[m3..m4, n3..n4] of ArbFloat;
  lam:  array[m1..m2] of ArbFloat;
begin
  Write(' program results eiggs3te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln('number of examples', nex: 2);
  writeln;
  for ex := 1 to nex do
  begin
    writeln('example number', ex: 2);
    writeln;
    Read(i1, j1, i2, j2, n);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    eiggs3(a[i1, j1], n, n2 - n1 + 1, lam[i1], x[i2, j2], n4 - n3 + 1, term);
    for i := 1 to n do
      for j := 1 to i - 1 do
        a[i1 + j - 1, j1 + i - 1] := a[i1 + i - 1, j1 + j - 1];
    writeln;
    writeln('A=');
    iomwrm(output, a[i1, j1], n, n, n2 - n1 + 1, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[i1], n, numdig);
      writeln;
      writeln('X=');
      iomwrm(output, x[i2, j2], n, n, n4 - n3 + 1, numdig);
      writeln;
      writeln('AX-lambda.X = ');
      omvmmm(a[i1, j1], n, n, n2 - n1 + 1, x[i2, j2], n, n4 - n3 + 1,
        e[i2, j2], n4 - n3 + 1);
      for j := 1 to n do
        for i := 1 to n do
          e[i + i2 - 1, j + j2 - 1] := e[i + i2 - 1, j + j2 - 1] - lam[i1 + j - 1] * x[i + i2 - 1, j + j2 - 1];
      iomwrm(output, e[i2, j2], n, n, n4 - n3 + 1, numdig);
    end;
    writeln('-------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eiggs3te;

uses
  typ,
  iom,
  omv,
  eig;

const
  m1 = -10;
  m2 = 10;
  m3 = -4;
  m4 = 15;
  n1 = -5;
  n2 = 10;
  n3 = -3;
  n4 = 10;
var
  i, j, ex, nex, i1, j1, i2, j2, n, term: ArbInt;
  a:    array[m1..m2, n1..n2] of ArbFloat;
  x, e: array[m3..m4, n3..n4] of ArbFloat;
  lam:  array[m1..m2] of ArbFloat;
begin
  Write(' program results eiggs3te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln('number of examples', nex: 2);
  writeln;
  for ex := 1 to nex do
  begin
    writeln('example number', ex: 2);
    writeln;
    Read(i1, j1, i2, j2, n);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    eiggs3(a[i1, j1], n, n2 - n1 + 1, lam[i1], x[i2, j2], n4 - n3 + 1, term);
    for i := 1 to n do
      for j := 1 to i - 1 do
        a[i1 + j - 1, j1 + i - 1] := a[i1 + i - 1, j1 + j - 1];
    writeln;
    writeln('A=');
    iomwrm(output, a[i1, j1], n, n, n2 - n1 + 1, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[i1], n, numdig);
      writeln;
      writeln('X=');
      iomwrm(output, x[i2, j2], n, n, n4 - n3 + 1, numdig);
      writeln;
      writeln('AX-lambda.X = ');
      omvmmm(a[i1, j1], n, n, n2 - n1 + 1, x[i2, j2], n, n4 - n3 + 1,
        e[i2, j2], n4 - n3 + 1);
      for j := 1 to n do
        for i := 1 to n do
          e[i + i2 - 1, j + j2 - 1] := e[i + i2 - 1, j + j2 - 1] - lam[i1 + j - 1] * x[i + i2 - 1, j + j2 - 1];
      iomwrm(output, e[i2, j2], n, n, n4 - n3 + 1, numdig);
    end;
    writeln('-------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
