program eigts2te;

uses
  eig,
  iom,
  typ;

const
  m1 = -9;
  m2 = 37;
var
  t: ArbFloat;
  i, ex, nex, k1, k2, i1, j1, n, term: ArbInt;
  d, cd, lam: array[m1..m2] of ArbFloat;
begin
  Write(' program results eigts2te');
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
    if ex < nex then
    begin
      Read(i1, j1, n, k1, k2);
      iomrev(input, d[i1], n);
      iomrev(input, cd[j1 + 1], n - 1);
    end
    else
    begin
      i1 := 1;
      j1 := 1;
      n  := 30;
      k1 := 5;
      k2 := 8;
      for i := 1 to n do
      begin
        t    := i;
        d[i] := sqr(t * t);
      end;
      for i := 2 to n do
        cd[i] := i - 1;
    end;
    eigts2(d[i1], cd[j1 + 1], n, k1, k2, lam[j1 + k1 - 1], term);
    writeln('diag =');
    iomwrv(output, d[i1], n, numdig);
    writeln('codiag =');
    iomwrv(output, cd[j1 + 1], n - 1, numdig);
    writeln;
    writeln('k1=', k1: 2, '  k2=', k2: 2);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln('lambda=');
      iomwrv(output, lam[j1 + k1 - 1], k2 - k1 + 1, numdig);
    end;
    writeln('------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eigts2te;

uses
  eig,
  iom,
  typ;

const
  m1 = -9;
  m2 = 37;
var
  t: ArbFloat;
  i, ex, nex, k1, k2, i1, j1, n, term: ArbInt;
  d, cd, lam: array[m1..m2] of ArbFloat;
begin
  Write(' program results eigts2te');
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
    if ex < nex then
    begin
      Read(i1, j1, n, k1, k2);
      iomrev(input, d[i1], n);
      iomrev(input, cd[j1 + 1], n - 1);
    end
    else
    begin
      i1 := 1;
      j1 := 1;
      n  := 30;
      k1 := 5;
      k2 := 8;
      for i := 1 to n do
      begin
        t    := i;
        d[i] := sqr(t * t);
      end;
      for i := 2 to n do
        cd[i] := i - 1;
    end;
    eigts2(d[i1], cd[j1 + 1], n, k1, k2, lam[j1 + k1 - 1], term);
    writeln('diag =');
    iomwrv(output, d[i1], n, numdig);
    writeln('codiag =');
    iomwrv(output, cd[j1 + 1], n - 1, numdig);
    writeln;
    writeln('k1=', k1: 2, '  k2=', k2: 2);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln('lambda=');
      iomwrv(output, lam[j1 + k1 - 1], k2 - k1 + 1, numdig);
    end;
    writeln('------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
