program eigsv3te;

uses
  typ,
  iom,
  omv,
  eig;

const
  m1  = -4;
  m2  = 20;
  n1  = -5;
  n2  = 10;
  r1  = -3;
  r2  = 18;
  s1  = -2;
  s2  = 18;
  x1  = -5;
  x2  = 22;
  y1  = -3;
  y2  = 21;
  l1  = -2;
  l2  = 17;
  rwa = n2 - n1 + 1;
  rwu = s2 - s1 + 1;
  rwv = y2 - y1 + 1;
var
  ex, nex, k, i, j, m, n, p, term, l, r, s, x, y: ArbInt;
  a, usvt, e: array[m1..m2, n1..n2] of ArbFloat;
  u, ut, utu, us: array[r1..r2, s1..s2] of ArbFloat;
  v, vt, vtv: array[x1..x2, y1..y2] of ArbFloat;
  q: array[l1..l2] of ArbFloat;
begin
  Write(' program results eigsv3te');
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
    Read(k, p, l, r, s, x, y, m, n);
    iomrem(input, a[k, p], m, n, rwa);
    eigsv3(a[k, p], m, n, rwa, q[l], u[r, s], rwu, v[x, y], rwv, term);
    writeln;
    writeln(' a =');
    iomwrm(output, a[k, p], m, n, rwa, 17);
    writeln;
    writeln(' term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln(' q =');
      iomwrv(output, q[l], n, numdig);
      writeln;
      writeln(' u =');
      iomwrm(output, u[r, s], m, n, rwu, numdig);
      writeln;
      writeln(' v =');
      iomwrm(output, v[x, y], n, n, rwv, numdig);
      writeln;
      writeln(' u(t) x u =');
      omvtrm(u[r, s], m, n, rwu, ut[r, s], rwu);
      omvmmm(ut[r, s], n, m, rwu, u[r, s], n, rwu, utu[r, s], rwu);
      iomwrm(output, utu[r, s], n, n, rwu, numdig);
      writeln;
      writeln(' v(t) x v =');
      omvtrm(v[x, y], n, n, rwv, vt[x, y], rwv);
      omvmmm(vt[x, y], n, n, rwv, v[x, y], n, rwv, vtv[x, y], rwv);
      iomwrm(output, vtv[x, y], n, n, rwv, numdig);
      writeln;
      writeln(' a - u x sigma x v(t) = ');
      for i := 1 to m do
        for j := 1 to n do
          us[r - 1 + i, s - 1 + j] := u[r - 1 + i, s - 1 + j] * q[l - 1 + j];
      omvmmm(us[r, s], m, n, rwu, vt[x, y], n, rwv, usvt[k, p], rwa);
      for i := 1 to m do
        for j := 1 to n do
          e[k - 1 + i, p - 1 + j] := a[k - 1 + i, p - 1 + j] - usvt[k - 1 + i, p - 1 + j];
      iomwrm(output, e[k, p], m, n, rwa, numdig);
    end;
  end;
  Close(input);
  Close(output);
end.
program eigsv3te;

uses
  typ,
  iom,
  omv,
  eig;

const
  m1  = -4;
  m2  = 20;
  n1  = -5;
  n2  = 10;
  r1  = -3;
  r2  = 18;
  s1  = -2;
  s2  = 18;
  x1  = -5;
  x2  = 22;
  y1  = -3;
  y2  = 21;
  l1  = -2;
  l2  = 17;
  rwa = n2 - n1 + 1;
  rwu = s2 - s1 + 1;
  rwv = y2 - y1 + 1;
var
  ex, nex, k, i, j, m, n, p, term, l, r, s, x, y: ArbInt;
  a, usvt, e: array[m1..m2, n1..n2] of ArbFloat;
  u, ut, utu, us: array[r1..r2, s1..s2] of ArbFloat;
  v, vt, vtv: array[x1..x2, y1..y2] of ArbFloat;
  q: array[l1..l2] of ArbFloat;
begin
  Write(' program results eigsv3te');
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
    Read(k, p, l, r, s, x, y, m, n);
    iomrem(input, a[k, p], m, n, rwa);
    eigsv3(a[k, p], m, n, rwa, q[l], u[r, s], rwu, v[x, y], rwv, term);
    writeln;
    writeln(' a =');
    iomwrm(output, a[k, p], m, n, rwa, 17);
    writeln;
    writeln(' term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln(' q =');
      iomwrv(output, q[l], n, numdig);
      writeln;
      writeln(' u =');
      iomwrm(output, u[r, s], m, n, rwu, numdig);
      writeln;
      writeln(' v =');
      iomwrm(output, v[x, y], n, n, rwv, numdig);
      writeln;
      writeln(' u(t) x u =');
      omvtrm(u[r, s], m, n, rwu, ut[r, s], rwu);
      omvmmm(ut[r, s], n, m, rwu, u[r, s], n, rwu, utu[r, s], rwu);
      iomwrm(output, utu[r, s], n, n, rwu, numdig);
      writeln;
      writeln(' v(t) x v =');
      omvtrm(v[x, y], n, n, rwv, vt[x, y], rwv);
      omvmmm(vt[x, y], n, n, rwv, v[x, y], n, rwv, vtv[x, y], rwv);
      iomwrm(output, vtv[x, y], n, n, rwv, numdig);
      writeln;
      writeln(' a - u x sigma x v(t) = ');
      for i := 1 to m do
        for j := 1 to n do
          us[r - 1 + i, s - 1 + j] := u[r - 1 + i, s - 1 + j] * q[l - 1 + j];
      omvmmm(us[r, s], m, n, rwu, vt[x, y], n, rwv, usvt[k, p], rwa);
      for i := 1 to m do
        for j := 1 to n do
          e[k - 1 + i, p - 1 + j] := a[k - 1 + i, p - 1 + j] - usvt[k - 1 + i, p - 1 + j];
      iomwrm(output, e[k, p], m, n, rwa, numdig);
    end;
  end;
  Close(input);
  Close(output);
end.
