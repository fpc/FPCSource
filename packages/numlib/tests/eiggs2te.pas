program eiggs2te;

uses
  typ,
  iom,
  eig;

const
  m1 = -9;
  m2 = 7;
  n1 = -10;
  n2 = 8;

var
  i, j, ex, nex, k1, k2, i1, j1, n, term: ArbInt;
  a:   array[m1..m2, n1..n2] of ArbFloat;
  lam: array[m1..m2] of ArbFloat;

begin
  Write(' program results eiggs2te');
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
    Read(i1, j1, n, k1, k2);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    eiggs2(a[i1, j1], n, n2 - n1 + 1, k1, k2, lam[i1 + k1 - 1], term);
    writeln;
    writeln('A=');
    writeln;
    for i := 1 to n do
      iomwrv(output, a[i1 + i - 1, j1], i, numdig);
    writeln;
    writeln('k1 =', k1: 2, '    k2 =', k2: 2);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[i1 + k1 - 1], k2 - k1 + 1, numdig);
    end;
    writeln('----------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eiggs2te;

uses
  typ,
  iom,
  eig;

const
  m1 = -9;
  m2 = 7;
  n1 = -10;
  n2 = 8;

var
  i, j, ex, nex, k1, k2, i1, j1, n, term: ArbInt;
  a:   array[m1..m2, n1..n2] of ArbFloat;
  lam: array[m1..m2] of ArbFloat;

begin
  Write(' program results eiggs2te');
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
    Read(i1, j1, n, k1, k2);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    eiggs2(a[i1, j1], n, n2 - n1 + 1, k1, k2, lam[i1 + k1 - 1], term);
    writeln;
    writeln('A=');
    writeln;
    for i := 1 to n do
      iomwrv(output, a[i1 + i - 1, j1], i, numdig);
    writeln;
    writeln('k1 =', k1: 2, '    k2 =', k2: 2);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[i1 + k1 - 1], k2 - k1 + 1, numdig);
    end;
    writeln('----------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
