program eigts3te;

uses
  typ,
  iom,
  omv,
  eig;

const
  m1  = -10;
  m2  = 10;
  rwx = m2 - m1 + 1;
  rwa = rwx;
var
  ex, nex, i1, j1, i2, j2, n, i, j, term: ArbInt;
  d, cd, lam: array[m1..m2] of ArbFloat;
  a, x, e:    array[m1..m2, m1..m2] of ArbFloat;
begin
  Write(' program results eigts3te');
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
    Read(i1, j1, i2, j2, n);
    iomrev(input, d[i1], n);
    iomrev(input, cd[j1 + 1], n - 1);
    eigts3(d[i1], cd[j1 + 1], n, lam[i1], x[i2, j2], rwx, term);
    writeln;
    writeln('diag =');
    iomwrv(output, d[i1], n, numdig);
    writeln;
    writeln('codiag =');
    iomwrv(output, cd[j1 + 1], n - 1, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[i1], n, numdig);
      writeln;
      writeln('X=');
      iomwrm(output, x[i2, j2], n, n, rwx, numdig);
      for i := i1 to i1 + n - 1 do
        for j := j1 to j1 + n - 1 do
          a[i, j] := 0;
      for i := 1 to n do
        a[i1 + i - 1, j1 + i - 1] := d[i1 + i - 1];
      for i := 1 to n - 1 do
        a[i1 + i, j1 + i - 1] := cd[j1 + i];
      for i := 1 to n - 1 do
        a[i1 + i - 1, j1 + i] := cd[j1 + i];
      writeln;
      writeln('AX-lambda.X = ');
      omvmmm(a[i1, j1], n, n, rwa, x[i2, j2], n, rwx, e[i2, j2], rwx);
      for j := 1 to n do
        for i := 1 to n do
          e[i + i2 - 1, j + j2 - 1] := e[i + i2 - 1, j + j2 - 1] - lam[i1 + j - 1] * x[i + i2 - 1, j + j2 - 1];
      iomwrm(output, e[i2, j2], n, n, rwx, numdig);
    end;
    writeln('-------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eigts3te;

uses
  typ,
  iom,
  omv,
  eig;

const
  m1  = -10;
  m2  = 10;
  rwx = m2 - m1 + 1;
  rwa = rwx;
var
  ex, nex, i1, j1, i2, j2, n, i, j, term: ArbInt;
  d, cd, lam: array[m1..m2] of ArbFloat;
  a, x, e:    array[m1..m2, m1..m2] of ArbFloat;
begin
  Write(' program results eigts3te');
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
    Read(i1, j1, i2, j2, n);
    iomrev(input, d[i1], n);
    iomrev(input, cd[j1 + 1], n - 1);
    eigts3(d[i1], cd[j1 + 1], n, lam[i1], x[i2, j2], rwx, term);
    writeln;
    writeln('diag =');
    iomwrv(output, d[i1], n, numdig);
    writeln;
    writeln('codiag =');
    iomwrv(output, cd[j1 + 1], n - 1, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[i1], n, numdig);
      writeln;
      writeln('X=');
      iomwrm(output, x[i2, j2], n, n, rwx, numdig);
      for i := i1 to i1 + n - 1 do
        for j := j1 to j1 + n - 1 do
          a[i, j] := 0;
      for i := 1 to n do
        a[i1 + i - 1, j1 + i - 1] := d[i1 + i - 1];
      for i := 1 to n - 1 do
        a[i1 + i, j1 + i - 1] := cd[j1 + i];
      for i := 1 to n - 1 do
        a[i1 + i - 1, j1 + i] := cd[j1 + i];
      writeln;
      writeln('AX-lambda.X = ');
      omvmmm(a[i1, j1], n, n, rwa, x[i2, j2], n, rwx, e[i2, j2], rwx);
      for j := 1 to n do
        for i := 1 to n do
          e[i + i2 - 1, j + j2 - 1] := e[i + i2 - 1, j + j2 - 1] - lam[i1 + j - 1] * x[i + i2 - 1, j + j2 - 1];
      iomwrm(output, e[i2, j2], n, n, rwx, numdig);
    end;
    writeln('-------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
