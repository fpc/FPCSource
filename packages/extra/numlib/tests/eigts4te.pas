program eigts4te;

uses
  iom,
  typ,
  omv,
  eig;

const
  n1  = -10;
  n2  = 8;
  n3  = -11;
  n4  = 9;
  rwx = n4 - n3 + 1;
  rwa = rwx;
var
  i, j, ex, nex, m2, k1, k2, n, term: ArbInt;
  d, cd:   array[n1..n2] of ArbFloat;
  a, e, x: array[n1..n2, n3..n4] of ArbFloat;
  lam:     array[n1..n2] of ArbFloat;
begin
  Write(' program results eigts4te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln('number of examples', nex: 2);
  for ex := 1 to nex do
  begin
    writeln;
    writeln('example number', ex: 2);
    Read(n, k1, k2);
    iomrev(input, d[1], n);
    iomrev(input, cd[2], n - 1);
    eigts4(d[1], cd[2], n, k1, k2, lam[k1], x[1, k1], rwx, m2, term);
    writeln;
    writeln('diag = ');
    iomwrv(output, d[1], n, numdig);
    writeln;
    writeln('codiag = ');
    iomwrv(output, cd[2], n - 1, numdig);
    writeln;
    writeln('k1=', k1: 2, '  k2=', k2: 2);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[k1], k2 - k1 + 1, numdig);
      writeln;
      writeln(' m2 =', m2: 2);
      writeln;
      writeln(' X=');
      iomwrm(output, x[1, k1], n, m2 - k1 + 1, rwx, numdig);
      for i := 1 to n do
        for j := 1 to n do
          a[i, j] := 0;
      for i := 1 to n do
        a[i, i] := d[i];
      for i := 1 to n - 1 do
        a[1 + i, i] := cd[i + 1];
      for i := 1 to n - 1 do
        a[i, i + 1] := cd[i + 1];
      writeln;
      writeln('AX-lambda.X = ');
      omvmmm(a[1, 1], n, n, rwa, x[1, k1], m2 - k1 + 1, rwx, e[1, k1], rwx);
      for j := 1 to m2 - k1 + 1 do
        for i := 1 to n do
          e[i, j + k1 - 1] := e[i, j + k1 - 1] - lam[k1 + j - 1] * x[i, j + k1 - 1];
      iomwrm(output, e[1, k1], n, m2 - k1 + 1, rwx, numdig);
    end;
    writeln('-----------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eigts4te;

uses
  iom,
  typ,
  omv,
  eig;

const
  n1  = -10;
  n2  = 8;
  n3  = -11;
  n4  = 9;
  rwx = n4 - n3 + 1;
  rwa = rwx;
var
  i, j, ex, nex, m2, k1, k2, n, term: ArbInt;
  d, cd:   array[n1..n2] of ArbFloat;
  a, e, x: array[n1..n2, n3..n4] of ArbFloat;
  lam:     array[n1..n2] of ArbFloat;
begin
  Write(' program results eigts4te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln('number of examples', nex: 2);
  for ex := 1 to nex do
  begin
    writeln;
    writeln('example number', ex: 2);
    Read(n, k1, k2);
    iomrev(input, d[1], n);
    iomrev(input, cd[2], n - 1);
    eigts4(d[1], cd[2], n, k1, k2, lam[k1], x[1, k1], rwx, m2, term);
    writeln;
    writeln('diag = ');
    iomwrv(output, d[1], n, numdig);
    writeln;
    writeln('codiag = ');
    iomwrv(output, cd[2], n - 1, numdig);
    writeln;
    writeln('k1=', k1: 2, '  k2=', k2: 2);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[k1], k2 - k1 + 1, numdig);
      writeln;
      writeln(' m2 =', m2: 2);
      writeln;
      writeln(' X=');
      iomwrm(output, x[1, k1], n, m2 - k1 + 1, rwx, numdig);
      for i := 1 to n do
        for j := 1 to n do
          a[i, j] := 0;
      for i := 1 to n do
        a[i, i] := d[i];
      for i := 1 to n - 1 do
        a[1 + i, i] := cd[i + 1];
      for i := 1 to n - 1 do
        a[i, i + 1] := cd[i + 1];
      writeln;
      writeln('AX-lambda.X = ');
      omvmmm(a[1, 1], n, n, rwa, x[1, k1], m2 - k1 + 1, rwx, e[1, k1], rwx);
      for j := 1 to m2 - k1 + 1 do
        for i := 1 to n do
          e[i, j + k1 - 1] := e[i, j + k1 - 1] - lam[k1 + j - 1] * x[i, j + k1 - 1];
      iomwrm(output, e[1, k1], n, m2 - k1 + 1, rwx, numdig);
    end;
    writeln('-----------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
