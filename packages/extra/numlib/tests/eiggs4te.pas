program eiggs4te;

uses
  typ,
  iom,
  omv,
  eig;

const
  p1  = -9;
  p2  = 5;
  n1  = -10;
  n2  = 8;
  p3  = -10;
  p4  = 7;
  n3  = -11;
  n4  = 9;
  rwa = n2 - n1 + 1;
  rwx = n4 - n3 + 1;
var
  i, j, ex, nex, m2, k1, k2, i2, j2, i1, j1, n, term: ArbInt;
  a:    array[p1..p2, n1..n2] of ArbFloat;
  x, e: array[p3..p4, n3..n4] of ArbFloat;
  lam:  array[p1..p2] of ArbFloat;
begin
  Write(' program results eiggs4te');
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
    Read(i1, j1, i2, j2, n, k1, k2);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    eiggs4(a[i1, j1], n, rwa, k1, k2, lam[i1 + k1 - 1], x[i2, j2 + k1 - 1],
      rwx, m2, term);
    writeln;
    writeln('A=');
    for i := 1 to n do
      for j := 1 to i - 1 do
        a[i1 + j - 1, j1 + i - 1] := a[i1 + i - 1, j1 + j - 1];
    iomwrm(output, a[i1, j1], n, n, n2 - n1 + 1, numdig);
    writeln;
    writeln('k1=', k1: 2, '  k2=', k2: 2);
    writeln;
    writeln('term=', term: 2);
    if term < 3 then
    begin
      writeln('lambda=');
      iomwrv(output, lam[i1 + k1 - 1], k2 - k1 + 1, numdig);
      writeln;
      writeln(' m2 =', m2: 2);
      writeln;
      writeln('X=');
      iomwrm(output, x[i2, j2 + k1 - 1], n, m2 - k1 + 1, rwx, numdig);
      writeln;
      writeln('AX-lambda.X = ');
      omvmmm(a[i1, j1], n, n, n2 - n1 + 1, x[i2, j2 + k1 - 1], m2 - k1 + 1, n4 - n3 + 1,
        e[i2, j2 + k1 - 1], n4 - n3 + 1);
      for j := k1 to m2 do
        for i := 1 to n do
          e[i + i2 - 1, j + j2 - 1] := e[i + i2 - 1, j + j2 - 1] - lam[i1 + j - 1] * x[i + i2 - 1, j + j2 - 1];
      iomwrm(output, e[i2, j2 + k1 - 1], n, m2 - k1 + 1, n4 - n3 + 1, numdig);
    end;
    writeln('---------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eiggs4te;

uses
  typ,
  iom,
  omv,
  eig;

const
  p1  = -9;
  p2  = 5;
  n1  = -10;
  n2  = 8;
  p3  = -10;
  p4  = 7;
  n3  = -11;
  n4  = 9;
  rwa = n2 - n1 + 1;
  rwx = n4 - n3 + 1;
var
  i, j, ex, nex, m2, k1, k2, i2, j2, i1, j1, n, term: ArbInt;
  a:    array[p1..p2, n1..n2] of ArbFloat;
  x, e: array[p3..p4, n3..n4] of ArbFloat;
  lam:  array[p1..p2] of ArbFloat;
begin
  Write(' program results eiggs4te');
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
    Read(i1, j1, i2, j2, n, k1, k2);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    eiggs4(a[i1, j1], n, rwa, k1, k2, lam[i1 + k1 - 1], x[i2, j2 + k1 - 1],
      rwx, m2, term);
    writeln;
    writeln('A=');
    for i := 1 to n do
      for j := 1 to i - 1 do
        a[i1 + j - 1, j1 + i - 1] := a[i1 + i - 1, j1 + j - 1];
    iomwrm(output, a[i1, j1], n, n, n2 - n1 + 1, numdig);
    writeln;
    writeln('k1=', k1: 2, '  k2=', k2: 2);
    writeln;
    writeln('term=', term: 2);
    if term < 3 then
    begin
      writeln('lambda=');
      iomwrv(output, lam[i1 + k1 - 1], k2 - k1 + 1, numdig);
      writeln;
      writeln(' m2 =', m2: 2);
      writeln;
      writeln('X=');
      iomwrm(output, x[i2, j2 + k1 - 1], n, m2 - k1 + 1, rwx, numdig);
      writeln;
      writeln('AX-lambda.X = ');
      omvmmm(a[i1, j1], n, n, n2 - n1 + 1, x[i2, j2 + k1 - 1], m2 - k1 + 1, n4 - n3 + 1,
        e[i2, j2 + k1 - 1], n4 - n3 + 1);
      for j := k1 to m2 do
        for i := 1 to n do
          e[i + i2 - 1, j + j2 - 1] := e[i + i2 - 1, j + j2 - 1] - lam[i1 + j - 1] * x[i + i2 - 1, j + j2 - 1];
      iomwrm(output, e[i2, j2 + k1 - 1], n, m2 - k1 + 1, n4 - n3 + 1, numdig);
    end;
    writeln('---------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
