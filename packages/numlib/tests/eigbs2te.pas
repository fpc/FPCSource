program eigbs2te;

uses
  typ,
  iom,
  eig;

const
  n1 = -100;
  n2 = 100;
var
  ex, nex, nel, p, q, n, b, k1, k2, term: ArbInt;
  a:   array[n1..n2] of ArbFloat;
  lam: array[n1..n2] of ArbFloat;
begin
  Write(' program results eigbs2te');
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
    writeln('example number', ex: 2);
    writeln;
    Read(p, q, n, b, k1, k2);
    nel := n * (b + 1) - (b * (b + 1)) div 2;
    iomrev(input, a[p], nel);
    eigbs2(a[p], n, b, k1, k2, lam[q], term);
    writeln(' A = ');
    iomwrv(output, a[p], nel, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[q], k2 - k1 + 1, numdig);
    end
    else
      writeln(' wrong input');
    writeln;
    writeln('-------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eigbs2te;

uses
  typ,
  iom,
  eig;

const
  n1 = -100;
  n2 = 100;
var
  ex, nex, nel, p, q, n, b, k1, k2, term: ArbInt;
  a:   array[n1..n2] of ArbFloat;
  lam: array[n1..n2] of ArbFloat;
begin
  Write(' program results eigbs2te');
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
    writeln('example number', ex: 2);
    writeln;
    Read(p, q, n, b, k1, k2);
    nel := n * (b + 1) - (b * (b + 1)) div 2;
    iomrev(input, a[p], nel);
    eigbs2(a[p], n, b, k1, k2, lam[q], term);
    writeln(' A = ');
    iomwrv(output, a[p], nel, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[q], k2 - k1 + 1, numdig);
    end
    else
      writeln(' wrong input');
    writeln;
    writeln('-------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
