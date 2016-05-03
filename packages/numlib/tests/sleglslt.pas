program sleglslt;

uses
  typ,
  iom,
  sle;

const
  mbov = 10;
  nbov = 8;

type
  ar1n = array[1..nbov] of ArbFloat;

var
  i, j, ii, m, n, k, nex, term: ArbInt;
  s:    ArbFloat;
  p:    array[1..mbov] of ^ar1n;
  b, e: array[1..mbov] of ArbFloat;
  x:    array[1..nbov] of ArbFloat;
begin
  Write(' program results sleglslt ');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln(' number of examples:', nex: 2);
  for ii := 1 to nex do
  begin
    Read(k, m, n);
    for i := k to m + k - 1 do
    begin
      getmem(p[i], n * sizeof(ArbFloat));
      iomrev(input, p[i]^[1], n);
    end;
    iomrev(input, b[k], m);
    sleglsl(p[k], m, n, b[k], x[k], term);
    writeln;
    writeln(' A =');
    for i := k to m + k - 1 do
      iomwrv(output, p[i]^[1], n, numdig);
    writeln;
    writeln(' b =');
    iomwrv(output, b[k], m, numdig);
    writeln;
    writeln('term=', term: 2);
    case term of
      1:
      begin
        writeln;
        writeln(' x =');
        iomwrv(output, x[k], n, numdig);
        writeln;
        writeln('Ax - b =');
        for i := k to m + k - 1 do
        begin
          s := 0;
          for j := 1 to n do
            s := s + p[i]^[j] * x[j + k - 1];
          e[i] := s - b[i];
        end;
        iomwrv(output, e[k], m, numdig);
      end;
      2: writeln(' A is (nearly) singular');
      3: writeln('wrong input (m<n or n<1)');
    end;
    for i := m + k - 1 downto k do
      freemem(p[i], n * sizeof(ArbFloat));
    writeln(' --------------------------------------------------');
  end;
end.
program sleglslt;

uses
  typ,
  iom,
  sle;

const
  mbov = 10;
  nbov = 8;

type
  ar1n = array[1..nbov] of ArbFloat;

var
  i, j, ii, m, n, k, nex, term: ArbInt;
  s:    ArbFloat;
  p:    array[1..mbov] of ^ar1n;
  b, e: array[1..mbov] of ArbFloat;
  x:    array[1..nbov] of ArbFloat;
begin
  Write(' program results sleglslt ');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln(' number of examples:', nex: 2);
  for ii := 1 to nex do
  begin
    Read(k, m, n);
    for i := k to m + k - 1 do
    begin
      getmem(p[i], n * sizeof(ArbFloat));
      iomrev(input, p[i]^[1], n);
    end;
    iomrev(input, b[k], m);
    sleglsl(p[k], m, n, b[k], x[k], term);
    writeln;
    writeln(' A =');
    for i := k to m + k - 1 do
      iomwrv(output, p[i]^[1], n, numdig);
    writeln;
    writeln(' b =');
    iomwrv(output, b[k], m, numdig);
    writeln;
    writeln('term=', term: 2);
    case term of
      1:
      begin
        writeln;
        writeln(' x =');
        iomwrv(output, x[k], n, numdig);
        writeln;
        writeln('Ax - b =');
        for i := k to m + k - 1 do
        begin
          s := 0;
          for j := 1 to n do
            s := s + p[i]^[j] * x[j + k - 1];
          e[i] := s - b[i];
        end;
        iomwrv(output, e[k], m, numdig);
      end;
      2: writeln(' A is (nearly) singular');
      3: writeln('wrong input (m<n or n<1)');
    end;
    for i := m + k - 1 downto k do
      freemem(p[i], n * sizeof(ArbFloat));
    writeln(' --------------------------------------------------');
  end;
end.
