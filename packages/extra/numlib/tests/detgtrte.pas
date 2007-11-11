program detgtrte;

uses
  typ,
  iom,
  det;

const
  c1 = -10;
  c2 = 10;
var
  k, p, n, term, vb, nvb: ArbInt;
  l, d, u: array[c1..c2] of ArbFloat;
  f: ArbFloat;
begin
  iom.npos := 1000;        {max. width of output to 1000, since this is piped}
  Write(' program results detgtrte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
    10: writeln('(Extended)');
  end;
  Read(nvb);
  writeln;
  writeln(' number of examples:', nvb: 4);
  for vb := 1 to nvb do
  begin
    writeln;
    writeln('example nr', vb: 2);
    Read(p, n);
    writeln;
    writeln(' n=', n: 2);
    iomrev(input, l[p + 1], n - 1);
    iomrev(input, d[p], n);
    iomrev(input, u[p], n - 1);
    detgtr(n, l[p + 1], d[p], u[p], f, k, term);
    writeln;
    writeln('lower diagonal of A =');
    iomwrv(output, l[p + 1], n - 1, numdig);
    writeln;
    writeln('diagonal of A =');
    iomwrv(output, d[p], n, numdig);
    writeln;
    writeln('upper diagonal of A =');
    iomwrv(output, u[p], n - 1, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 3 then
      writeln(' wrong input')
    else
    begin
      Write(' determinant of A =', f: numdig);
      if k <> 0 then
        Write(' * 8**', k: 3);
      writeln;
    end; {term=1}
    writeln('----------------------------------------------------');
  end;  {vb}
  Close(input);
  Close(output);
end.
program detgtrte;

uses
  typ,
  iom,
  det;

const
  c1 = -10;
  c2 = 10;
var
  k, p, n, term, vb, nvb: ArbInt;
  l, d, u: array[c1..c2] of ArbFloat;
  f: ArbFloat;
begin
  iom.npos := 1000;        {max. width of output to 1000, since this is piped}
  Write(' program results detgtrte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
    10: writeln('(Extended)');
  end;
  Read(nvb);
  writeln;
  writeln(' number of examples:', nvb: 4);
  for vb := 1 to nvb do
  begin
    writeln;
    writeln('example nr', vb: 2);
    Read(p, n);
    writeln;
    writeln(' n=', n: 2);
    iomrev(input, l[p + 1], n - 1);
    iomrev(input, d[p], n);
    iomrev(input, u[p], n - 1);
    detgtr(n, l[p + 1], d[p], u[p], f, k, term);
    writeln;
    writeln('lower diagonal of A =');
    iomwrv(output, l[p + 1], n - 1, numdig);
    writeln;
    writeln('diagonal of A =');
    iomwrv(output, d[p], n, numdig);
    writeln;
    writeln('upper diagonal of A =');
    iomwrv(output, u[p], n - 1, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 3 then
      writeln(' wrong input')
    else
    begin
      Write(' determinant of A =', f: numdig);
      if k <> 0 then
        Write(' * 8**', k: 3);
      writeln;
    end; {term=1}
    writeln('----------------------------------------------------');
  end;  {vb}
  Close(input);
  Close(output);
end.

