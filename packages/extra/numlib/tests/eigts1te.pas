program eigts1te;

uses
  typ,
  iom,
  eig;

const
  m1 = -9;
  m2 = 37;
var
  i, ex, nex, i1, j1, n, term: ArbInt;
  t: ArbFloat;
  d, cd, lam: array[m1..m2] of ArbFloat;
begin
  Write(' program results eigts1te');
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
      Read(i1, j1, n);
      iomrev(input, d[i1], n);
      iomrev(input, cd[j1 + 1], n - 1);
    end
    else
    begin
      i1 := 1;
      j1 := 1;
      n  := 30;
      for i := 1 to n do
      begin
        t    := i;
        d[i] := sqr(t * t);
      end;
      for i := 2 to n do
        cd[i] := i - 1;
    end;
    eigts1(d[i1], cd[j1 + 1], n, lam[j1], term);
    writeln('diag =');
    iomwrv(output, d[i1], n, numdig);
    writeln('codiag =');
    iomwrv(output, cd[j1 + 1], n - 1, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[j1], n, numdig);
    end;
    writeln('------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eigts1te;

uses
  typ,
  iom,
  eig;

const
  m1 = -9;
  m2 = 37;
var
  i, ex, nex, i1, j1, n, term: ArbInt;
  t: ArbFloat;
  d, cd, lam: array[m1..m2] of ArbFloat;
begin
  Write(' program results eigts1te');
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
      Read(i1, j1, n);
      iomrev(input, d[i1], n);
      iomrev(input, cd[j1 + 1], n - 1);
    end
    else
    begin
      i1 := 1;
      j1 := 1;
      n  := 30;
      for i := 1 to n do
      begin
        t    := i;
        d[i] := sqr(t * t);
      end;
      for i := 2 to n do
        cd[i] := i - 1;
    end;
    eigts1(d[i1], cd[j1 + 1], n, lam[j1], term);
    writeln('diag =');
    iomwrv(output, d[i1], n, numdig);
    writeln('codiag =');
    iomwrv(output, cd[j1 + 1], n - 1, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[j1], n, numdig);
    end;
    writeln('------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
