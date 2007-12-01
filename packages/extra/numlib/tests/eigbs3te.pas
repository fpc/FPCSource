program eigbs3te;

uses
  typ,
  iom,
  eig,
  omv;

const
  n1  = -100;
  n2  = 100;
  i1  = -10;
  i2  = 10;
  rwx = i2 - i1 + 1;
var
  ex, nex, nel, ind, p, q, r, s, n, i, j, b, term: ArbInt;
  a:   array[n1..n2] of ArbFloat;
  lam: array[i1..i2] of ArbFloat;
  x, mat, e: array[i1..i2, i1..i2] of ArbFloat;
begin
  Write(' program results eigbs3te');
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
    Read(p, q, r, s, n, b);
    nel := n * (b + 1) - (b * (b + 1)) div 2;
    iomrev(input, a[p], nel);
    eigbs3(a[p], n, b, lam[q], x[r, s], rwx, term);
    writeln(' A = ');
    iomwrv(output, a[p], nel, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[q], n, numdig);
      writeln;
      writeln('X=');
      iomwrm(output, x[r, s], n, n, rwx, numdig);
      ind := p;
      for i := 1 to n do
        for j := 1 to i do
          if j < i - b then
            mat[i + r - 1, j + s - 1] := 0
          else
          begin
            mat[i + r - 1, j + s - 1] := a[ind];
            ind := ind + 1;
          end;
      for i := 1 to n do
        for j := i + 1 to n do
          mat[i + r - 1, j + s - 1] := mat[j + r - 1, i + s - 1];
      writeln;
      writeln(' matrix A =');
      iomwrm(output, mat[r, s], n, n, rwx, numdig);
      writeln;
      writeln('Ax-lambda.x = ');
      omvmmm(mat[r, s], n, n, rwx, x[r, s], n, rwx, e[r, s], rwx);
      for j := 1 to n do
        for i := 1 to n do
          e[i + r - 1, j + s - 1] := e[i + r - 1, j + s - 1] - lam[q + j - 1] * x[i + r - 1, j + s - 1];
      iomwrm(output, e[r, s], n, n, rwx, numdig);
    end;
    writeln;
    writeln('-------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
program eigbs3te;

uses
  typ,
  iom,
  eig,
  omv;

const
  n1  = -100;
  n2  = 100;
  i1  = -10;
  i2  = 10;
  rwx = i2 - i1 + 1;
var
  ex, nex, nel, ind, p, q, r, s, n, i, j, b, term: ArbInt;
  a:   array[n1..n2] of ArbFloat;
  lam: array[i1..i2] of ArbFloat;
  x, mat, e: array[i1..i2, i1..i2] of ArbFloat;
begin
  Write(' program results eigbs3te');
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
    Read(p, q, r, s, n, b);
    nel := n * (b + 1) - (b * (b + 1)) div 2;
    iomrev(input, a[p], nel);
    eigbs3(a[p], n, b, lam[q], x[r, s], rwx, term);
    writeln(' A = ');
    iomwrv(output, a[p], nel, numdig);
    writeln;
    writeln('term=', term: 2);
    if term = 1 then
    begin
      writeln;
      writeln('lambda=');
      iomwrv(output, lam[q], n, numdig);
      writeln;
      writeln('X=');
      iomwrm(output, x[r, s], n, n, rwx, numdig);
      ind := p;
      for i := 1 to n do
        for j := 1 to i do
          if j < i - b then
            mat[i + r - 1, j + s - 1] := 0
          else
          begin
            mat[i + r - 1, j + s - 1] := a[ind];
            ind := ind + 1;
          end;
      for i := 1 to n do
        for j := i + 1 to n do
          mat[i + r - 1, j + s - 1] := mat[j + r - 1, i + s - 1];
      writeln;
      writeln(' matrix A =');
      iomwrm(output, mat[r, s], n, n, rwx, numdig);
      writeln;
      writeln('Ax-lambda.x = ');
      omvmmm(mat[r, s], n, n, rwx, x[r, s], n, rwx, e[r, s], rwx);
      for j := 1 to n do
        for i := 1 to n do
          e[i + r - 1, j + s - 1] := e[i + r - 1, j + s - 1] - lam[q + j - 1] * x[i + r - 1, j + s - 1];
      iomwrm(output, e[r, s], n, n, rwx, numdig);
    end;
    writeln;
    writeln('-------------------------------------------');
  end;
  Close(input);
  Close(output);
end.
