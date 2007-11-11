program eigge1te;

uses
  typ,
  iom,
  eig;

const
  m1 = -9;
  m2 = 5;
  n1 = -10;
  n2 = 8;

var
  i, l, nex, i1, j1, n, term: ArbInt;
  a:   array[m1..m2, n1..n2] of ArbFloat;
  lam: array[m1..m2] of complex;
begin
  Write(' program results eigge1te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln('number of examples', nex: 2);
  writeln;
  for l := 1 to nex do
  begin
    writeln('example number', l: 2);
    writeln;
    Read(i1, j1, n);
    iomrem(input, a[i1, j1], n, n, n2 - n1 + 1);
    eigge1(a[i1, j1], n, n2 - n1 + 1, lam[i1], term);
    writeln;
    writeln('A=');
    writeln;
    iomwrm(output, a[i1, j1], n, n, n2 - n1 + 1, numdig);
    writeln('term=', term: 2);
    writeln;
    if term = 1 then
    begin
      writeln('lambda=');
      writeln(' ': 8, 'Re', ' ': 14, 'Im');
      for i := 1 to n do
        writeln(lam[i1 + i - 1].re: numdig, '  ', lam[i1 + i - 1].im: numdig);
      writeln;
    end;
    writeln('-------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eigge1te;

uses
  typ,
  iom,
  eig;

const
  m1 = -9;
  m2 = 5;
  n1 = -10;
  n2 = 8;

var
  i, l, nex, i1, j1, n, term: ArbInt;
  a:   array[m1..m2, n1..n2] of ArbFloat;
  lam: array[m1..m2] of complex;
begin
  Write(' program results eigge1te');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(nex);
  writeln;
  writeln('number of examples', nex: 2);
  writeln;
  for l := 1 to nex do
  begin
    writeln('example number', l: 2);
    writeln;
    Read(i1, j1, n);
    iomrem(input, a[i1, j1], n, n, n2 - n1 + 1);
    eigge1(a[i1, j1], n, n2 - n1 + 1, lam[i1], term);
    writeln;
    writeln('A=');
    writeln;
    iomwrm(output, a[i1, j1], n, n, n2 - n1 + 1, numdig);
    writeln('term=', term: 2);
    writeln;
    if term = 1 then
    begin
      writeln('lambda=');
      writeln(' ': 8, 'Re', ' ': 14, 'Im');
      for i := 1 to n do
        writeln(lam[i1 + i - 1].re: numdig, '  ', lam[i1 + i - 1].im: numdig);
      writeln;
    end;
    writeln('-------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
