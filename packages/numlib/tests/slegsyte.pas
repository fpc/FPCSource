program slegsyte;

uses
  typ,
  iom,
  sle;

const
  m1 = -10;
  m2 = 10;
  n1 = -5;
  n2 = 10;

type
  array1dr = array[m1..m2] of ArbFloat;
  array2dr = array[m1..m2, n1..n2] of ArbFloat;

var
  t, nex, i, j, k, l, n, term: ArbInt;
  ca:   ArbFloat;
  a:    array2dr;
  b, x: array1dr;

begin
  Write('program results slegsyte ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;
  writeln;
  Read(randseed);
  writeln('  randseed = ', randseed: 15);
  Read(nex);
  writeln;
  writeln(' number of examples:', nex: 2);
  for t := 1 to nex do
  begin
    writeln;
    writeln(' example number : ', t: 1);
    Read(k, l, n);
    for i := 1 to n do
      iomrev(input, a[i + k - 1, l], i);
    iomrev(input, b[k], n);
    slegsy(n, n2 - n1 + 1, a[k, l], b[k], x[k], ca, term);
    writeln;
    writeln('A=');
    for i := 1 to n do
      for j := i + 1 to n do
        a[i + k - 1, j + l - 1] := a[j + k - 1, i + l - 1];
    iomwrm(output, a[k, l], n, n, n2 - n1 + 1, numdig);
    writeln;
    writeln('b=');
    iomwrv(output, b[k], n, numdig);
    writeln;
    writeln('term=', term: 2);
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
  Close(input);
  Close(output);
end.
program slegsyte;

uses
  typ,
  iom,
  sle;

const
  m1 = -10;
  m2 = 10;
  n1 = -5;
  n2 = 10;

type
  array1dr = array[m1..m2] of ArbFloat;
  array2dr = array[m1..m2, n1..n2] of ArbFloat;

var
  t, nex, i, j, k, l, n, term: ArbInt;
  ca:   ArbFloat;
  a:    array2dr;
  b, x: array1dr;

begin
  Write('program results slegsyte ');
  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;
  writeln;
  Read(randseed);
  writeln('  randseed = ', randseed: 15);
  Read(nex);
  writeln;
  writeln(' number of examples:', nex: 2);
  for t := 1 to nex do
  begin
    writeln;
    writeln(' example number : ', t: 1);
    Read(k, l, n);
    for i := 1 to n do
      iomrev(input, a[i + k - 1, l], i);
    iomrev(input, b[k], n);
    slegsy(n, n2 - n1 + 1, a[k, l], b[k], x[k], ca, term);
    writeln;
    writeln('A=');
    for i := 1 to n do
      for j := i + 1 to n do
        a[i + k - 1, j + l - 1] := a[j + k - 1, i + l - 1];
    iomwrm(output, a[k, l], n, n, n2 - n1 + 1, numdig);
    writeln;
    writeln('b=');
    iomwrv(output, b[k], n, numdig);
    writeln;
    writeln('term=', term: 2);
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
  Close(input);
  Close(output);
end.
