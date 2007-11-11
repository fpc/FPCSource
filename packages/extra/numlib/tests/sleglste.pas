program sleglste;

uses
  typ,
  iom,
  omv,
  sle;

const
  k1  = -20;
  k2  = 20;
  l1  = -10;
  l2  = 10;
  r1  = -10;
  r2  = 18;
  v1  = -8;
  v2  = 18;
  rwa = l2 - l1 + 1;
var
  ex, nv, i, m, n, term, k, l, r, v: ArbInt;
  a:    array[k1..k2, l1..l2] of ArbFloat;
  b, e: array[r1..r2] of ArbFloat;
  x:    array[v1..v2] of ArbFloat;
begin
  Write('program results sleglste ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;
  Read(nv);
  writeln;
  writeln(' number of examples: ', nv: 2);
  for ex := 1 to nv do
  begin
    writeln;
    writeln('  example number :', ex: 2);
    Read(k, l, r, v, m, n);
    iomrem(input, a[k, l], m, n, rwa);
    iomrev(input, b[r], m);
    slegls(a[k, l], m, n, rwa, b[r], x[v], term);
    writeln;
    writeln(' A =');
    iomwrm(output, a[k, l], m, n, rwa, numdig);
    writeln;
    writeln(' b =');
    iomwrv(output, b[r], m, numdig);
    writeln;
    writeln(' term=', term: 2);
    case term of
      1:
      begin
        writeln;
        writeln(' x =');
        iomwrv(output, x[v], n, numdig);
        writeln;
        writeln(' Ax - b =');
        omvmmv(a[k, l], m, n, rwa, x[v], e[r]);
        for i := 1 to m do
          e[r - 1 + i] := e[r - 1 + i] - b[r - 1 + i];
        iomwrv(output, e[r], m, numdig);
      end;
      2: writeln(' A is (nearly) singular');
      3: writeln('wrong input (n<1 or m<n)')
    end;
    writeln(' -------------------------------------------');
  end;
end.
program sleglste;

uses
  typ,
  iom,
  omv,
  sle;

const
  k1  = -20;
  k2  = 20;
  l1  = -10;
  l2  = 10;
  r1  = -10;
  r2  = 18;
  v1  = -8;
  v2  = 18;
  rwa = l2 - l1 + 1;
var
  ex, nv, i, m, n, term, k, l, r, v: ArbInt;
  a:    array[k1..k2, l1..l2] of ArbFloat;
  b, e: array[r1..r2] of ArbFloat;
  x:    array[v1..v2] of ArbFloat;
begin
  Write('program results sleglste ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;
  Read(nv);
  writeln;
  writeln(' number of examples: ', nv: 2);
  for ex := 1 to nv do
  begin
    writeln;
    writeln('  example number :', ex: 2);
    Read(k, l, r, v, m, n);
    iomrem(input, a[k, l], m, n, rwa);
    iomrev(input, b[r], m);
    slegls(a[k, l], m, n, rwa, b[r], x[v], term);
    writeln;
    writeln(' A =');
    iomwrm(output, a[k, l], m, n, rwa, numdig);
    writeln;
    writeln(' b =');
    iomwrv(output, b[r], m, numdig);
    writeln;
    writeln(' term=', term: 2);
    case term of
      1:
      begin
        writeln;
        writeln(' x =');
        iomwrv(output, x[v], n, numdig);
        writeln;
        writeln(' Ax - b =');
        omvmmv(a[k, l], m, n, rwa, x[v], e[r]);
        for i := 1 to m do
          e[r - 1 + i] := e[r - 1 + i] - b[r - 1 + i];
        iomwrv(output, e[r], m, numdig);
      end;
      2: writeln(' A is (nearly) singular');
      3: writeln('wrong input (n<1 or m<n)')
    end;
    writeln(' -------------------------------------------');
  end;
end.
