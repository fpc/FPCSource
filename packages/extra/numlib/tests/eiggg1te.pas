program eiggg1te;

uses
  typ,
  eig,
  iom;

const
  m1 = -9;
  m2 = 5;
  n1 = -10;
  n2 = 8;
  n3 = -7;
  n4 = 6;
var
  i, j, l, nex, i1, j1, i2, j2, n, term: ArbInt;
  a:   array[m1..m2, n1..n2] of ArbFloat;
  b:   array[m1..m2, n3..n4] of ArbFloat;
  lam: array[m1..m2] of ArbFloat;
begin
  Write(' program results eiggg1te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln('number of examples', nex: 2);
  writeln;
  for l := 1 to nex do
  begin
    writeln('example number', l: 2);
    writeln;
    Read(i1, j1, i2, j2, n);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    for i := 1 to n do
      for j := 1 to i do
        Read(b[i2 + i - 1, j2 + j - 1]);
    eiggg1(a[i1, j1], n, n2 - n1 + 1, b[i2, j2], n4 - n3 + 1, lam[i1], term);
    writeln;
    writeln('A=');
    writeln;
    for i := 1 to n do
      iomwrv(output, a[i1 + i - 1, j1], i, numdig);
    writeln;
    writeln('B=');
    writeln;
    for i := 1 to n do
      iomwrv(output, b[i2 + i - 1, j2], i, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 1 then
    begin
      writeln('lambda=');
      iomwrv(output, lam[i1], n, numdig);
      writeln;
    end;
    writeln('-------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eiggg1te;

uses
  typ,
  eig,
  iom;

const
  m1 = -9;
  m2 = 5;
  n1 = -10;
  n2 = 8;
  n3 = -7;
  n4 = 6;
var
  i, j, l, nex, i1, j1, i2, j2, n, term: ArbInt;
  a:   array[m1..m2, n1..n2] of ArbFloat;
  b:   array[m1..m2, n3..n4] of ArbFloat;
  lam: array[m1..m2] of ArbFloat;
begin
  Write(' program results eiggg1te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln('number of examples', nex: 2);
  writeln;
  for l := 1 to nex do
  begin
    writeln('example number', l: 2);
    writeln;
    Read(i1, j1, i2, j2, n);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    for i := 1 to n do
      for j := 1 to i do
        Read(b[i2 + i - 1, j2 + j - 1]);
    eiggg1(a[i1, j1], n, n2 - n1 + 1, b[i2, j2], n4 - n3 + 1, lam[i1], term);
    writeln;
    writeln('A=');
    writeln;
    for i := 1 to n do
      iomwrv(output, a[i1 + i - 1, j1], i, numdig);
    writeln;
    writeln('B=');
    writeln;
    for i := 1 to n do
      iomwrv(output, b[i2 + i - 1, j2], i, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 1 then
    begin
      writeln('lambda=');
      iomwrv(output, lam[i1], n, numdig);
      writeln;
    end;
    writeln('-------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
