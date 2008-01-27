program slegente;

uses
  typ,
  iom,
  sle;

const
  m1 = -10;
  m2 = 10;
  n1 = -10;
  n2 = 10;

type
  array1dr = array[m1..m2] of ArbFloat;
  array2dr = array[m1..m2, n1..n2] of ArbFloat;

var
  v, nv, k, j, n, term: ArbInt;
  ca:   ArbFloat;
  a:    array2dr;
  b, x: array1dr;
begin
  Write('program results slegente ');
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
    Read(k, j, n);
    iomrem(input, a[k, j], n, n, n2 - n1 + 1);
    iomrev(input, b[k], n);
    slegen(n, n2 - n1 + 1, a[k, j], b[k], x[k], ca, term);
    writeln;
    writeln(' A =');
    iomwrm(output, a[k, j], n, n, n2 - n1 + 1, numdig);
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
program slegente;

uses
  typ,
  iom,
  sle;

const
  m1 = -10;
  m2 = 10;
  n1 = -10;
  n2 = 10;

type
  array1dr = array[m1..m2] of ArbFloat;
  array2dr = array[m1..m2, n1..n2] of ArbFloat;

var
  v, nv, k, j, n, term: ArbInt;
  ca:   ArbFloat;
  a:    array2dr;
  b, x: array1dr;
begin
  Write('program results slegente ');
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
    Read(k, j, n);
    iomrem(input, a[k, j], n, n, n2 - n1 + 1);
    iomrev(input, b[k], n);
    slegen(n, n2 - n1 + 1, a[k, j], b[k], x[k], ca, term);
    writeln;
    writeln(' A =');
    iomwrm(output, a[k, j], n, n, n2 - n1 + 1, numdig);
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
