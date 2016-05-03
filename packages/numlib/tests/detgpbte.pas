program detgpbte;

{

1: 260
2: 64
3:

}
uses
  typ,
  iom,
  det;

const
  pmin = -10;
  pmax = 100;
var
  l, i, ind, rw, n, k, term, p, vb, nvb: ArbInt;
  f: ArbFloat;
  a: array[pmin..pmax] of ArbFloat;
begin
  iom.npos := 1000;        {max. width of output to 1000, since this is piped}
  Assign(input, ParamStr(1));
  reset(input);
  Assign(output, ParamStr(2));
  rewrite(output);
  Write(' program results detgpbte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
    10: writeln('(extended)');
  end;
  Read(nvb);
  writeln;
  writeln(' number of examples:', nvb: 3);
  for vb := 1 to nvb do
  begin
    writeln;
    writeln('example', vb: 2);
    Read(p, n, l);
    ind := p;
    writeln;
    writeln('  n=', n: 1, '  l=', l: 1);
    for i := 1 to n do
    begin
      if i <= l then
        rw := i
      else
        rw := l + 1;
      iomrev(input, a[ind], rw);
      Inc(ind, rw);
    end;
    detgpb(n, l, a[p], f, k, term);
    ind := p;
    writeln;
    writeln(' A (left-under) =');
    for i := 1 to n do
    begin
      if i <= l then
        rw := i
      else
        rw := l + 1;
      if i > l + 1 then
        Write('': (i - l - 1) * (numdig + 2));
      iomwrv(output, a[ind], rw, numdig);
      Inc(ind, rw);
    end;
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 3 then
      writeln(' wrong input')
    else
    if term = 2 then
      writeln(' matrix not pos-def.')
    else
    begin
      Write(' determinant of A =', f: numdig);
      {      if k <> 0 then } Write(' * 8**', k: 3);
      writeln;
    end; {term=1}
    writeln('---------------------------------------------');
  end; {vb}
  Close(input);
  Close(output);
end.
program detgpbte;

{

1: 260
2: 64
3:

}
uses
  typ,
  iom,
  det;

const
  pmin = -10;
  pmax = 100;
var
  l, i, ind, rw, n, k, term, p, vb, nvb: ArbInt;
  f: ArbFloat;
  a: array[pmin..pmax] of ArbFloat;
begin
  iom.npos := 1000;        {max. width of output to 1000, since this is piped}
  Assign(input, ParamStr(1));
  reset(input);
  Assign(output, ParamStr(2));
  rewrite(output);
  Write(' program results detgpbte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
    10: writeln('(extended)');
  end;
  Read(nvb);
  writeln;
  writeln(' number of examples:', nvb: 3);
  for vb := 1 to nvb do
  begin
    writeln;
    writeln('example', vb: 2);
    Read(p, n, l);
    ind := p;
    writeln;
    writeln('  n=', n: 1, '  l=', l: 1);
    for i := 1 to n do
    begin
      if i <= l then
        rw := i
      else
        rw := l + 1;
      iomrev(input, a[ind], rw);
      Inc(ind, rw);
    end;
    detgpb(n, l, a[p], f, k, term);
    ind := p;
    writeln;
    writeln(' A (left-under) =');
    for i := 1 to n do
    begin
      if i <= l then
        rw := i
      else
        rw := l + 1;
      if i > l + 1 then
        Write('': (i - l - 1) * (numdig + 2));
      iomwrv(output, a[ind], rw, numdig);
      Inc(ind, rw);
    end;
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 3 then
      writeln(' wrong input')
    else
    if term = 2 then
      writeln(' matrix not pos-def.')
    else
    begin
      Write(' determinant of A =', f: numdig);
      {      if k <> 0 then } Write(' * 8**', k: 3);
      writeln;
    end; {term=1}
    writeln('---------------------------------------------');
  end; {vb}
  Close(input);
  Close(output);
end.

