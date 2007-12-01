program slegpblt;

uses
  typ,
  iom,
  sle;

const
  c = 0;
  d = 10;

var
  l, i, p, q, n, term, rw, vb, nvb: ArbInt;
  ca:   ArbFloat;
  a:    array[c..d] of ^ArbFloat;
  b, x: array[c..d] of ArbFloat;
begin
  Assign(input, ParamStr(1));
  Reset(input);
  Assign(output, ParamStr(2));
  Rewrite(output);

  Write(' program results slegpblt');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(randseed);
  writeln;
  writeln('   randseed =', randseed: 6);
  writeln;
  Read(nvb);
  writeln(' number of examples:', nvb: 3);
  writeln;
  for vb := 1 to nvb do
  begin
    writeln('example', vb: 2);
    Read(p, q, n, l);
    writeln;
    writeln('   n=', n: 1, '   l=', l: 1);
    for i := 1 to n do
    begin
      if i <= l + 1 then
        rw := i
      else
        rw := l + 1;
      GetMem(a[i + p - 1], rw * sizeof(ArbFloat));
      iomrev(input, a[i + p - 1]^, rw);
    end;
    iomrev(input, b[q], n);
    slegpbl(n, l, a[p], b[q], x[q], ca, term);
    writeln;
    writeln(' A (left-under part) =  ');
    for i := 1 to n do
    begin
      if i <= l + 1 then
        rw := i
      else
      begin
        rw := l + 1;
        Write('': (i - l - 1) * (numdig + 2));
      end;
      iomwrv(output, a[i + p - 1]^, rw, numdig);
      FreeMem(a[i + p - 1], rw * sizeof(ArbFloat));
    end;
    writeln;
    writeln('b=');
    iomwrv(output, b[q], n, numdig);
    writeln;
    writeln('term=', term: 2);
    case term of
      1:
      begin
        writeln;
        writeln('x=');
        iomwrv(output, x[q], n, numdig);
        writeln;
        writeln(' ca=', ca: 12);
      end;
      2: writeln('solution not possible');
      3: writeln(' wrong input (l<0, r<0, l>n-1 or r>n-1)')
    end;
    writeln('---------------------------------------------');
  end; {vb}
end.
program slegpblt;

uses
  typ,
  iom,
  sle;

const
  c = 0;
  d = 10;

var
  l, i, p, q, n, term, rw, vb, nvb: ArbInt;
  ca:   ArbFloat;
  a:    array[c..d] of ^ArbFloat;
  b, x: array[c..d] of ArbFloat;
begin
  Assign(input, ParamStr(1));
  Reset(input);
  Assign(output, ParamStr(2));
  Rewrite(output);

  Write(' program results slegpblt');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(randseed);
  writeln;
  writeln('   randseed =', randseed: 6);
  writeln;
  Read(nvb);
  writeln(' number of examples:', nvb: 3);
  writeln;
  for vb := 1 to nvb do
  begin
    writeln('example', vb: 2);
    Read(p, q, n, l);
    writeln;
    writeln('   n=', n: 1, '   l=', l: 1);
    for i := 1 to n do
    begin
      if i <= l + 1 then
        rw := i
      else
        rw := l + 1;
      GetMem(a[i + p - 1], rw * sizeof(ArbFloat));
      iomrev(input, a[i + p - 1]^, rw);
    end;
    iomrev(input, b[q], n);
    slegpbl(n, l, a[p], b[q], x[q], ca, term);
    writeln;
    writeln(' A (left-under part) =  ');
    for i := 1 to n do
    begin
      if i <= l + 1 then
        rw := i
      else
      begin
        rw := l + 1;
        Write('': (i - l - 1) * (numdig + 2));
      end;
      iomwrv(output, a[i + p - 1]^, rw, numdig);
      FreeMem(a[i + p - 1], rw * sizeof(ArbFloat));
    end;
    writeln;
    writeln('b=');
    iomwrv(output, b[q], n, numdig);
    writeln;
    writeln('term=', term: 2);
    case term of
      1:
      begin
        writeln;
        writeln('x=');
        iomwrv(output, x[q], n, numdig);
        writeln;
        writeln(' ca=', ca: 12);
      end;
      2: writeln('solution not possible');
      3: writeln(' wrong input (l<0, r<0, l>n-1 or r>n-1)')
    end;
    writeln('---------------------------------------------');
  end; {vb}
end.
