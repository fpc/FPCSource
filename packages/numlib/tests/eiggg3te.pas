program eiggg3te;

uses
  typ,
  iom,
  omv,
  eig;

const
  m1  = -10;
  m2  = 10;
  m3  = -4;
  m4  = 15;
  m5  = -5;
  m6  = 11;
  n1  = -5;
  n2  = 10;
  n3  = -3;
  n4  = 10;
  n5  = -7;
  n6  = 12;
  rwa = n2 - n1 + 1;
  rwb = n4 - n3 + 1;
  rwx = n6 - n5 + 1;

var
  i, j, l, nex, n, k, term, i1, j1, i2, j2, i3, j3: ArbInt;
  r, s: ArbFloat;
  a:    array[m1..m2, n1..n2] of ArbFloat;
  b:    array[m3..m4, n3..n4] of ArbFloat;
  x, xt, xtb, xtbx: array[m5..m6, n5..n6] of ArbFloat;
  lam:  array[m1..m2] of ArbFloat;

begin
  Write(' program results eiggg3te');
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
    Read(i1, j1, i2, j2, i3, j3, n);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    for i := 1 to n do
      for j := 1 to i do
        Read(b[i2 + i - 1, j2 + j - 1]);
    eiggg3(a[i1, j1], n, rwa, b[i2, j2], rwb, lam[i1], x[i3, j3], rwx, term);
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
    writeln('term=', term: 2);
    writeln;
    if term = 1 then
    begin
      writeln('lambda=');
      iomwrv(output, lam[i1], n, numdig);
      writeln;
      writeln('eigenvectors:');
      iomwrm(output, x[i3, j3], n, n, rwx, numdig);
      writeln;
      for i := 1 to n do
        for j := 1 to i - 1 do
          a[i1 + j - 1, j1 + i - 1] := a[i1 + i - 1, j1 + j - 1];
      for i := 1 to n do
        for j := 1 to i - 1 do
          b[i2 + j - 1, j2 + i - 1] := b[i2 + i - 1, j2 + j - 1];
      writeln('residuals:');
      for j := 1 to n do
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
      writeln('xtbx =');
      omvtrm(x[i3, j3], n, n, rwx, xt[i3, j3], rwx);
      omvmmm(xt[i3, j3], n, n, rwx, b[i2, j2], n, rwb, xtb[i3, j3], rwx);
      omvmmm(xtb[i3, j3], n, n, rwx, x[i3, j3], n, rwx, xtbx[i3, j3], rwx);
      iomwrm(output, xtbx[i3, j3], n, n, rwx, 17);
    end;
    writeln('--------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eiggg3te;

uses
  typ,
  iom,
  omv,
  eig;

const
  m1  = -10;
  m2  = 10;
  m3  = -4;
  m4  = 15;
  m5  = -5;
  m6  = 11;
  n1  = -5;
  n2  = 10;
  n3  = -3;
  n4  = 10;
  n5  = -7;
  n6  = 12;
  rwa = n2 - n1 + 1;
  rwb = n4 - n3 + 1;
  rwx = n6 - n5 + 1;

var
  i, j, l, nex, n, k, term, i1, j1, i2, j2, i3, j3: ArbInt;
  r, s: ArbFloat;
  a:    array[m1..m2, n1..n2] of ArbFloat;
  b:    array[m3..m4, n3..n4] of ArbFloat;
  x, xt, xtb, xtbx: array[m5..m6, n5..n6] of ArbFloat;
  lam:  array[m1..m2] of ArbFloat;

begin
  Write(' program results eiggg3te');
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
    Read(i1, j1, i2, j2, i3, j3, n);
    for i := 1 to n do
      for j := 1 to i do
        Read(a[i1 + i - 1, j1 + j - 1]);
    for i := 1 to n do
      for j := 1 to i do
        Read(b[i2 + i - 1, j2 + j - 1]);
    eiggg3(a[i1, j1], n, rwa, b[i2, j2], rwb, lam[i1], x[i3, j3], rwx, term);
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
    writeln('term=', term: 2);
    writeln;
    if term = 1 then
    begin
      writeln('lambda=');
      iomwrv(output, lam[i1], n, numdig);
      writeln;
      writeln('eigenvectors:');
      iomwrm(output, x[i3, j3], n, n, rwx, numdig);
      writeln;
      for i := 1 to n do
        for j := 1 to i - 1 do
          a[i1 + j - 1, j1 + i - 1] := a[i1 + i - 1, j1 + j - 1];
      for i := 1 to n do
        for j := 1 to i - 1 do
          b[i2 + j - 1, j2 + i - 1] := b[i2 + i - 1, j2 + j - 1];
      writeln('residuals:');
      for j := 1 to n do
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
      writeln('xtbx =');
      omvtrm(x[i3, j3], n, n, rwx, xt[i3, j3], rwx);
      omvmmm(xt[i3, j3], n, n, rwx, b[i2, j2], n, rwb, xtb[i3, j3], rwx);
      omvmmm(xtb[i3, j3], n, n, rwx, x[i3, j3], n, rwx, xtbx[i3, j3], rwx);
      iomwrm(output, xtbx[i3, j3], n, n, rwx, 17);
    end;
    writeln('--------------------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
