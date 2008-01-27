program slegenlt;

uses
  typ,
  iom,
  sle;

const
  n1 = -10;
  n2 = 10;

var
  i, n, k, v, nv, term: ArbInt;
  ca:   ArbFloat;
  b, x: array[n1..n2] of ArbFloat;
  p:    array[n1..n2] of ^ArbFloat;
begin
  Write('program results slegenlt ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;
  Read(nv);
  writeln;
  writeln('   number of examples: ', nv: 2);
  for v := 1 to nv do
  begin
    writeln;
    writeln('  example number :', v: 2);
    Read(k, n);
    for i := k to n + k - 1 do
    begin
      Getmem(p[i], n * sizeOf(ArbFloat));
      iomrev(input, p[i]^, n);
    end;
    iomrev(input, b[k], n);
    slegenl(n, p[k], b[k], x[k], ca, term);
    writeln;
    writeln(' A =');
    for i := k to n + k - 1 do
      iomwrv(output, p[i]^, n, numdig);
    for i := n + k - 1 downto k do
      Freemem(p[i], n * sizeOf(ArbFloat));
    writeln;
    writeln('b=');
    iomwrv(output, b[k], n, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    case term of
      1:
      begin
        writeln('x=');
        iomwrv(output, x[k], n, numdig);
        writeln;
        writeln(' ca = ', ca: 12);
      end;
      2: writeln('solution not possible');
      3: writeln(' wrong value of n');
    end;
    writeln('-----------------------------------------------');
  end; {example}
end.
program slegenlt;

uses
  typ,
  iom,
  sle;

const
  n1 = -10;
  n2 = 10;

var
  i, n, k, v, nv, term: ArbInt;
  ca:   ArbFloat;
  b, x: array[n1..n2] of ArbFloat;
  p:    array[n1..n2] of ^ArbFloat;
begin
  Write('program results slegenlt ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;
  Read(nv);
  writeln;
  writeln('   number of examples: ', nv: 2);
  for v := 1 to nv do
  begin
    writeln;
    writeln('  example number :', v: 2);
    Read(k, n);
    for i := k to n + k - 1 do
    begin
      Getmem(p[i], n * sizeOf(ArbFloat));
      iomrev(input, p[i]^, n);
    end;
    iomrev(input, b[k], n);
    slegenl(n, p[k], b[k], x[k], ca, term);
    writeln;
    writeln(' A =');
    for i := k to n + k - 1 do
      iomwrv(output, p[i]^, n, numdig);
    for i := n + k - 1 downto k do
      Freemem(p[i], n * sizeOf(ArbFloat));
    writeln;
    writeln('b=');
    iomwrv(output, b[k], n, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    case term of
      1:
      begin
        writeln('x=');
        iomwrv(output, x[k], n, numdig);
        writeln;
        writeln(' ca = ', ca: 12);
      end;
      2: writeln('solution not possible');
      3: writeln(' wrong value of n');
    end;
    writeln('-----------------------------------------------');
  end; {example}
end.
