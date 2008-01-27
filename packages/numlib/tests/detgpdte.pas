program detgpdte;

uses
  typ,
  iom,
  det;

const
  n1  = -5;
  n2  = 10;
  rwa = n2 - n1 + 1;
var
  e, t, aantal, i, j, k, l, n, term: ArbInt;
  d: ArbFloat;
  a: array[n1..n2, n1..n2] of ArbFloat;
begin
  iom.npos := 1000;        {max. width of output to 1000, since this is piped}
  Write(' program results detgpdte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
    10: writeln('(Extended)');
  end;
  Read(aantal);
  writeln;
  writeln('  number of examples : ', aantal: 3);
  for t := 1 to aantal do
  begin
    writeln;
    writeln('       example nr ', t: 3);
    Read(k, l, n);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[k + i - 1, l + j - 1]);
    detgpd(n, rwa, a[k, l], d, e, term);
    writeln;
    writeln(' A =');
    for i := 1 to n do
      for j := 1 to i - 1 do
        a[k + j - 1, l + i - 1] := a[k + i - 1, l + j - 1];
    iomwrm(output, a[k, l], n, n, n2 - n1 + 1, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 3 then
      writeln(' wrong input')
    else
    if term = 2 then
      writeln(' matrix not pos-def')
    else
    begin
      Write(' det =', d: numdig);
      if e <> 0 then
        Write(' * 8**', e: 3);
      writeln;
    end; {term=1}
    writeln('------------------------------------------------------');
  end; {t}
  Close(input);
  Close(output);
end.
program detgpdte;

uses
  typ,
  iom,
  det;

const
  n1  = -5;
  n2  = 10;
  rwa = n2 - n1 + 1;
var
  e, t, aantal, i, j, k, l, n, term: ArbInt;
  d: ArbFloat;
  a: array[n1..n2, n1..n2] of ArbFloat;
begin
  iom.npos := 1000;        {max. width of output to 1000, since this is piped}
  Write(' program results detgpdte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
    10: writeln('(Extended)');
  end;
  Read(aantal);
  writeln;
  writeln('  number of examples : ', aantal: 3);
  for t := 1 to aantal do
  begin
    writeln;
    writeln('       example nr ', t: 3);
    Read(k, l, n);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[k + i - 1, l + j - 1]);
    detgpd(n, rwa, a[k, l], d, e, term);
    writeln;
    writeln(' A =');
    for i := 1 to n do
      for j := 1 to i - 1 do
        a[k + j - 1, l + i - 1] := a[k + i - 1, l + j - 1];
    iomwrm(output, a[k, l], n, n, n2 - n1 + 1, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 3 then
      writeln(' wrong input')
    else
    if term = 2 then
      writeln(' matrix not pos-def')
    else
    begin
      Write(' det =', d: numdig);
      if e <> 0 then
        Write(' * 8**', e: 3);
      writeln;
    end; {term=1}
    writeln('------------------------------------------------------');
  end; {t}
  Close(input);
  Close(output);
end.

