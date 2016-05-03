program slegpdlt;

uses
  typ,
  iom,
  sle;

const
  m1   = -8;
  m2   = 12;
  nmax = 10;

type
  row = array[1..nmax] of ArbFloat;

var
  i, j, n, k, l, v, nv, term: ArbInt;
  ca:   ArbFloat;
  b, x: array[m1..m2] of ArbFloat;
  a:    array[m1..m2] of ^row;
begin
  Assign(input, ParamStr(1));
  Reset(input);
  Assign(output, ParamStr(2));
  Rewrite(output);

  Write('program results slegpdlt ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;
  writeln;
  Read(randseed);
  writeln('  randseed = ', randseed: 15);
  Read(nv);
  writeln;
  writeln('   number of examples: ', nv: 2);
  for v := 1 to nv do
  begin
    writeln;
    writeln('  example number :', v: 2);
    Read(k, l, n);
    for i := 1 to n do
    begin
      Getmem(a[i + k - 1], n * sizeOf(ArbFloat));
      iomrev(input, a[i + k - 1]^[1], i);
    end;
    iomrev(input, b[l], n);
    slegpdl(n, a[k], b[l], x[l], ca, term);
    writeln;
    writeln(' A =');
    for i := 1 to n do
      for j := i + 1 to n do
        a[i + k - 1]^[j] := a[j + k - 1]^[i];
    for i := 1 to n do
      iomwrv(output, a[i + k - 1]^[1], n, numdig);
    for i := n downto 1 do
      Freemem(a[i + k - 1], n * sizeOf(ArbFloat));
    writeln;
    writeln('b=');
    iomwrv(output, b[l], n, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    case term of
      1:
      begin
        writeln('x=');
        iomwrv(output, x[l], n, numdig);
        writeln;
        writeln(' ca = ', ca: 12);
      end;
      2: writeln('solution not possible');
      3: writeln(' wrong value of n');
    end;
    writeln('-----------------------------------------------');
  end; {example}
end.
program slegpdlt;

uses
  typ,
  iom,
  sle;

const
  m1   = -8;
  m2   = 12;
  nmax = 10;

type
  row = array[1..nmax] of ArbFloat;

var
  i, j, n, k, l, v, nv, term: ArbInt;
  ca:   ArbFloat;
  b, x: array[m1..m2] of ArbFloat;
  a:    array[m1..m2] of ^row;
begin
  Assign(input, ParamStr(1));
  Reset(input);
  Assign(output, ParamStr(2));
  Rewrite(output);

  Write('program results slegpdlt ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;
  writeln;
  Read(randseed);
  writeln('  randseed = ', randseed: 15);
  Read(nv);
  writeln;
  writeln('   number of examples: ', nv: 2);
  for v := 1 to nv do
  begin
    writeln;
    writeln('  example number :', v: 2);
    Read(k, l, n);
    for i := 1 to n do
    begin
      Getmem(a[i + k - 1], n * sizeOf(ArbFloat));
      iomrev(input, a[i + k - 1]^[1], i);
    end;
    iomrev(input, b[l], n);
    slegpdl(n, a[k], b[l], x[l], ca, term);
    writeln;
    writeln(' A =');
    for i := 1 to n do
      for j := i + 1 to n do
        a[i + k - 1]^[j] := a[j + k - 1]^[i];
    for i := 1 to n do
      iomwrv(output, a[i + k - 1]^[1], n, numdig);
    for i := n downto 1 do
      Freemem(a[i + k - 1], n * sizeOf(ArbFloat));
    writeln;
    writeln('b=');
    iomwrv(output, b[l], n, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    case term of
      1:
      begin
        writeln('x=');
        iomwrv(output, x[l], n, numdig);
        writeln;
        writeln(' ca = ', ca: 12);
      end;
      2: writeln('solution not possible');
      3: writeln(' wrong value of n');
    end;
    writeln('-----------------------------------------------');
  end; {example}
end.
