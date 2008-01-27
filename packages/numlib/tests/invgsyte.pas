program invgsyte;

uses
  typ,
  iom,
  inv;

const
  m1 = -10;
  m2 = 10;
  n1 = -5;
  n2 = 10;
var
  t, aantal, kk, ii, jj, k, j, n, term: ArbInt;
  s: ArbFloat;
  u, h, a: array[m1..m2, n1..n2] of ArbFloat;
begin
  Write(' program results invgsyte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(aantal);
  writeln;
  writeln(' number of examples:', aantal: 3);
  for t := 1 to aantal do
  begin
    writeln;
    writeln('       example nr ', t: 3);
    Read(k, j, n);
    iomrem(input, a[k, j], n, n, n2 - n1 + 1);
    writeln;
    writeln('a =');
    iomwrm(output, a[k, j], n, n, n2 - n1 + 1, numdig);
    for ii := 1 to n do
      for jj := 1 to n do
        h[k - 1 + ii, j - 1 + jj] := a[k - 1 + ii, j - 1 + jj];
    invgsy(n, n2 - n1 + 1, a[k, j], term);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln('inv(a)=');
      iomwrm(output, a[k, j], n, n, n2 - n1 + 1, numdig);
      for ii := 1 to n do
        for jj := 1 to n do
        begin
          s := 0;
          for kk := 1 to n do
            s := s + h[k - 1 + ii, j - 1 + kk] * a[k - 1 + kk, j - 1 + jj];
          u[ii, jj] := s;
        end; {ii,jj}
      writeln;
      writeln('a x inv(a) =');
      iomwrm(output, u[1, 1], n, n, n2 - n1 + 1, numdig);
    end; {term=1}
  end; {t}
  Close(input);
  Close(output);
end.
program invgsyte;

uses
  typ,
  iom,
  inv;

const
  m1 = -10;
  m2 = 10;
  n1 = -5;
  n2 = 10;
var
  t, aantal, kk, ii, jj, k, j, n, term: ArbInt;
  s: ArbFloat;
  u, h, a: array[m1..m2, n1..n2] of ArbFloat;
begin
  Write(' program results invgsyte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(aantal);
  writeln;
  writeln(' number of examples:', aantal: 3);
  for t := 1 to aantal do
  begin
    writeln;
    writeln('       example nr ', t: 3);
    Read(k, j, n);
    iomrem(input, a[k, j], n, n, n2 - n1 + 1);
    writeln;
    writeln('a =');
    iomwrm(output, a[k, j], n, n, n2 - n1 + 1, numdig);
    for ii := 1 to n do
      for jj := 1 to n do
        h[k - 1 + ii, j - 1 + jj] := a[k - 1 + ii, j - 1 + jj];
    invgsy(n, n2 - n1 + 1, a[k, j], term);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln('inv(a)=');
      iomwrm(output, a[k, j], n, n, n2 - n1 + 1, numdig);
      for ii := 1 to n do
        for jj := 1 to n do
        begin
          s := 0;
          for kk := 1 to n do
            s := s + h[k - 1 + ii, j - 1 + kk] * a[k - 1 + kk, j - 1 + jj];
          u[ii, jj] := s;
        end; {ii,jj}
      writeln;
      writeln('a x inv(a) =');
      iomwrm(output, u[1, 1], n, n, n2 - n1 + 1, numdig);
    end; {term=1}
  end; {t}
  Close(input);
  Close(output);
end.
