program eiggg4te;

uses
  typ,
  iom,
  omv,
  eig;

const
  p1  = -9;
  p2  = 5;
  n1  = -10;
  n2  = 8;
  p3  = -10;
  p4  = 7;
  n3  = -11;
  n4  = 9;
  p5  = -8;
  p6  = 11;
  n5  = -12;
  n6  = 12;
  rwa = n2 - n1 + 1;
  rwb = n4 - n3 + 1;
  rwx = n6 - n5 + 1;
var
  i, j, l, m2, k1, k2, nex, n, term, k, m, i1, j1, i2, j2, i3, j3: ArbInt;
  r, s: ArbFloat;
  a:    array[p1..p2, n1..n2] of ArbFloat;
  b:    array[p3..p4, n3..n4] of ArbFloat;
  x, xt, xtb, xtbx: array[p5..p6, n5..n6] of ArbFloat;
  lam:  array[p1..p2] of ArbFloat;
begin
  Write(' program results eiggg4te');
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
    writeln('example number', l: 3);
    Read(i1, j1, i2, j2, i3, j3, n, k1, k2);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    for i := 1 to n do
      for j := 1 to i do
        Read(b[i2 + i - 1, j2 + j - 1]);
    eiggg4(a[i1, j1], n, rwa, k1, k2, b[i2, j2], rwb, lam[i1 + k1 - 1],
      x[i3, j3 + k1 - 1], rwx, m2, term);
    writeln;
    writeln('A=');
    writeln;
    for i := 1 to n do
      iomwrv(output, a[i1 + i - 1, j1], i, numdig);
    writeln;
    writeln('B=');
    writeln;
    for i := 1 to n do
      iomwrv(output, b[i2 + i - 1, j2], i, numdig);
    writeln;
    writeln(' k1=', k1: 2, '   k2=', k2: 2);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if (term = 1) or (term = 4) then
    begin
      writeln('lambda=', k1: 2, ' t/m', k2: 2, ' = ');
      iomwrv(output, lam[i1 + k1 - 1], k2 - k1 + 1, numdig);
      writeln;
      writeln(' m2 =', m2: 2);
      writeln;
      if m2 = k1 - 1 then
        writeln(' eigenvectors can not be determined')
      else
      begin
        writeln('eigenvectors', k1: 2, ' t/m', m2: 2, ':');
        iomwrm(output, x[i3, j3 + k1 - 1], n, m2 - k1 + 1, rwx, numdig);
        for i := 1 to n do
          for j := 1 to i - 1 do
            a[i1 + j - 1, j1 + i - 1] := a[i1 + i - 1, j1 + j - 1];
        for i := 1 to n do
          for j := 1 to i - 1 do
            b[i2 + j - 1, j2 + i - 1] := b[i2 + i - 1, j2 + j - 1];
        writeln('residuals:');
        for j := k1 to m2 do
        begin
          writeln('residual nr', j: 2);
          for i := 1 to n do
          begin
            r := 0;
            for k := 1 to n do
              r := r + a[i1 + i - 1, j1 + k - 1] * x[i3 + k - 1, j3 + j - 1];
            s := 0;
            for k := 1 to n do
              s := s + b[i2 + i - 1, j2 + k - 1] * x[i3 + k - 1, j3 + j - 1];
            r := r - s * lam[i1 + j - 1];
            Write(r: numdig, ' ');
          end; {i}
          writeln;
        end; {j}
        m := m2 - k1 + 1;
        writeln('xtbx =');
        omvtrm(x[i3, j3 + k1 - 1], n, m, rwx, xt[i3, j3], rwx);
        omvmmm(xt[i3, j3], m, n, rwx, b[i2, j2], n, rwb, xtb[i3, j3], rwx);
        omvmmm(xtb[i3, j3], m, n, rwx, x[i3, j3 + k1 - 1], m, rwx,
          xtbx[i3, j3], rwx);
        iomwrm(output, xtbx[i3, j3], m, m, rwx, numdig);
      end; {m2 > k1-1}
    end; {term=1 or term=4}
    writeln('----------------------------------------------------------');
  end; {l}
  Close(input);
  Close(output);
end.
program eiggg4te;

uses
  typ,
  iom,
  omv,
  eig;

const
  p1  = -9;
  p2  = 5;
  n1  = -10;
  n2  = 8;
  p3  = -10;
  p4  = 7;
  n3  = -11;
  n4  = 9;
  p5  = -8;
  p6  = 11;
  n5  = -12;
  n6  = 12;
  rwa = n2 - n1 + 1;
  rwb = n4 - n3 + 1;
  rwx = n6 - n5 + 1;
var
  i, j, l, m2, k1, k2, nex, n, term, k, m, i1, j1, i2, j2, i3, j3: ArbInt;
  r, s: ArbFloat;
  a:    array[p1..p2, n1..n2] of ArbFloat;
  b:    array[p3..p4, n3..n4] of ArbFloat;
  x, xt, xtb, xtbx: array[p5..p6, n5..n6] of ArbFloat;
  lam:  array[p1..p2] of ArbFloat;
begin
  Write(' program results eiggg4te');
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
    writeln('example number', l: 3);
    Read(i1, j1, i2, j2, i3, j3, n, k1, k2);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    for i := 1 to n do
      for j := 1 to i do
        Read(b[i2 + i - 1, j2 + j - 1]);
    eiggg4(a[i1, j1], n, rwa, k1, k2, b[i2, j2], rwb, lam[i1 + k1 - 1],
      x[i3, j3 + k1 - 1], rwx, m2, term);
    writeln;
    writeln('A=');
    writeln;
    for i := 1 to n do
      iomwrv(output, a[i1 + i - 1, j1], i, numdig);
    writeln;
    writeln('B=');
    writeln;
    for i := 1 to n do
      iomwrv(output, b[i2 + i - 1, j2], i, numdig);
    writeln;
    writeln(' k1=', k1: 2, '   k2=', k2: 2);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if (term = 1) or (term = 4) then
    begin
      writeln('lambda=', k1: 2, ' t/m', k2: 2, ' = ');
      iomwrv(output, lam[i1 + k1 - 1], k2 - k1 + 1, numdig);
      writeln;
      writeln(' m2 =', m2: 2);
      writeln;
      if m2 = k1 - 1 then
        writeln(' eigenvectors can not be determined')
      else
      begin
        writeln('eigenvectors', k1: 2, ' t/m', m2: 2, ':');
        iomwrm(output, x[i3, j3 + k1 - 1], n, m2 - k1 + 1, rwx, numdig);
        for i := 1 to n do
          for j := 1 to i - 1 do
            a[i1 + j - 1, j1 + i - 1] := a[i1 + i - 1, j1 + j - 1];
        for i := 1 to n do
          for j := 1 to i - 1 do
            b[i2 + j - 1, j2 + i - 1] := b[i2 + i - 1, j2 + j - 1];
        writeln('residuals:');
        for j := k1 to m2 do
        begin
          writeln('residual nr', j: 2);
          for i := 1 to n do
          begin
            r := 0;
            for k := 1 to n do
              r := r + a[i1 + i - 1, j1 + k - 1] * x[i3 + k - 1, j3 + j - 1];
            s := 0;
            for k := 1 to n do
              s := s + b[i2 + i - 1, j2 + k - 1] * x[i3 + k - 1, j3 + j - 1];
            r := r - s * lam[i1 + j - 1];
            Write(r: numdig, ' ');
          end; {i}
          writeln;
        end; {j}
        m := m2 - k1 + 1;
        writeln('xtbx =');
        omvtrm(x[i3, j3 + k1 - 1], n, m, rwx, xt[i3, j3], rwx);
        omvmmm(xt[i3, j3], m, n, rwx, b[i2, j2], n, rwb, xtb[i3, j3], rwx);
        omvmmm(xtb[i3, j3], m, n, rwx, x[i3, j3 + k1 - 1], m, rwx,
          xtbx[i3, j3], rwx);
        iomwrm(output, xtbx[i3, j3], m, m, rwx, numdig);
      end; {m2 > k1-1}
    end; {term=1 or term=4}
    writeln('----------------------------------------------------------');
  end; {l}
  Close(input);
  Close(output);
end.
