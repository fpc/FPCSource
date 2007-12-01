program eigsv1te;

uses
  typ,
  iom,
  eig;

const
  m1 = -2;
  m2 = 40;
  n1 = -3;
  n2 = 30;
  l1 = -2;
  l2 = 30;
  rw = n2 - n1 + 1;
var
  ex, nex, k, i, j, m, n, p, term, l: ArbInt;
  a: array[m1..m2, n1..n2] of ArbFloat;
  q: array[l1..l2] of ArbFloat;
begin
  Write(' program results eigsv1te');
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
    writeln;
    writeln('  example number :', ex: 2);
    Read(k, p, l, m, n);
    if ex < nex then
      iomrem(input, a[k, p], m, n, rw)
    else
      for i := 1 to m do
        for j := 1 to n do
          if i > j then
            a[k - 1 + i, p - 1 + j] := 0
          else
          if i = j then
            a[k - 1 + i, p - 1 + j] := 1
          else
            a[k - 1 + i, p - 1 + j] := -1;
    eigsv1(a[k, p], m, n, rw, q[l], term);
    writeln;
    writeln(' A =');
    iomwrm(output, a[k, p], m, n, rw, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('q=');
      iomwrv(output, q[l], n, numdig);
    end;
  end;
  Close(input);
  Close(output);
end.
program eigsv1te;

uses
  typ,
  iom,
  eig;

const
  m1 = -2;
  m2 = 40;
  n1 = -3;
  n2 = 30;
  l1 = -2;
  l2 = 30;
  rw = n2 - n1 + 1;
var
  ex, nex, k, i, j, m, n, p, term, l: ArbInt;
  a: array[m1..m2, n1..n2] of ArbFloat;
  q: array[l1..l2] of ArbFloat;
begin
  Write(' program results eigsv1te');
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
    writeln;
    writeln('  example number :', ex: 2);
    Read(k, p, l, m, n);
    if ex < nex then
      iomrem(input, a[k, p], m, n, rw)
    else
      for i := 1 to m do
        for j := 1 to n do
          if i > j then
            a[k - 1 + i, p - 1 + j] := 0
          else
          if i = j then
            a[k - 1 + i, p - 1 + j] := 1
          else
            a[k - 1 + i, p - 1 + j] := -1;
    eigsv1(a[k, p], m, n, rw, q[l], term);
    writeln;
    writeln(' A =');
    iomwrm(output, a[k, p], m, n, rw, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('q=');
      iomwrv(output, q[l], n, numdig);
    end;
  end;
  Close(input);
  Close(output);
end.
