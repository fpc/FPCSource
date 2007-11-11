program eigge3te;

uses
  typ,
  iom,
  eig;

const
  m1  = -9;
  m2  = 5;
  m3  = -11;
  m4  = 8;
  n1  = -10;
  n2  = 8;
  n3  = -9;
  n4  = 7;
  rwa = n2 - n1 + 1;
  rwx = n4 - n3 + 1;
var
  i, j, l, nex, n, term, i1, j1, i2, j2, k: ArbInt;
  r:   ArbFloat;
  a:   array[m1..m2, n1..n2] of ArbFloat;
  x:   array[m3..m4, n3..n4] of complex;
  lam: array[m1..m2] of complex;
begin
  Write(' program results eigge3te');
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
    Read(i1, j1, i2, j2, n);
    iomrem(input, a[i1, j1], n, n, rwa);
    eigge3(a[i1, j1], n, rwa, lam[i1], x[i2, j2], rwx, term);
    writeln;
    writeln('A=');
    writeln;
    iomwrm(output, a[i1, j1], n, n, rwa, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 1 then
    begin
      writeln('lambda=');
      writeln(' ': 10, 'Re', ' ': 10, 'Im');
      for i := 1 to n do
        writeln(lam[i1 + i - 1].re: numdig, ' ', lam[i1 + i - 1].im: numdig);
      writeln;
      writeln('eigenvectors:');
      for j := 1 to n do
      begin
        writeln('eig. vect. nr', j: 2);
        writeln(' ': 10, 'Re', ' ': 10, 'Im');
        for i := 1 to n do
        begin
          Write(x[i2 + i - 1, j2 + j - 1].re: numdig, ' ');
          writeln(x[i2 + i - 1, j2 + j - 1].im: numdig);
        end;  {i}
        writeln;
      end; {j}
      writeln('residuals:');
      for j := 1 to n do
      begin
        writeln('residual nr', j: 2);
        writeln(' ': 10, 'Re', ' ': 10, 'Im');
        for i := 1 to n do
        begin
          r := 0;
          for k := 1 to n do
            r := r + a[i1 + i - 1, j1 + k - 1] * x[i2 + k - 1, j2 + j - 1].re;
          r := r - lam[i1 + j - 1].re * x[i2 + i - 1, j2 + j - 1].re;
          r := r + lam[i1 + j - 1].im * x[i2 + i - 1, j2 + j - 1].im;
          Write(r: numdig, ' ');
          r := 0;
          for k := 1 to n do
            r := r + a[i1 + i - 1, j1 + k - 1] * x[i2 + k - 1, j2 + j - 1].im;
          r := r - lam[i1 + j - 1].re * x[i2 + i - 1, j2 + j - 1].im;
          r := r - lam[i1 + j - 1].im * x[i2 + i - 1, j2 + j - 1].re;
          writeln(r: numdig);
        end; {i}
        writeln;
      end; {j}
    end; {term=1}
    writeln('-------------------------------------------');
  end; {l}
  Close(input);
  Close(output);
end.
program eigge3te;

uses
  typ,
  iom,
  eig;

const
  m1  = -9;
  m2  = 5;
  m3  = -11;
  m4  = 8;
  n1  = -10;
  n2  = 8;
  n3  = -9;
  n4  = 7;
  rwa = n2 - n1 + 1;
  rwx = n4 - n3 + 1;
var
  i, j, l, nex, n, term, i1, j1, i2, j2, k: ArbInt;
  r:   ArbFloat;
  a:   array[m1..m2, n1..n2] of ArbFloat;
  x:   array[m3..m4, n3..n4] of complex;
  lam: array[m1..m2] of complex;
begin
  Write(' program results eigge3te');
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
    Read(i1, j1, i2, j2, n);
    iomrem(input, a[i1, j1], n, n, rwa);
    eigge3(a[i1, j1], n, rwa, lam[i1], x[i2, j2], rwx, term);
    writeln;
    writeln('A=');
    writeln;
    iomwrm(output, a[i1, j1], n, n, rwa, numdig);
    writeln;
    writeln('term=', term: 2);
    writeln;
    if term = 1 then
    begin
      writeln('lambda=');
      writeln(' ': 10, 'Re', ' ': 10, 'Im');
      for i := 1 to n do
        writeln(lam[i1 + i - 1].re: numdig, ' ', lam[i1 + i - 1].im: numdig);
      writeln;
      writeln('eigenvectors:');
      for j := 1 to n do
      begin
        writeln('eig. vect. nr', j: 2);
        writeln(' ': 10, 'Re', ' ': 10, 'Im');
        for i := 1 to n do
        begin
          Write(x[i2 + i - 1, j2 + j - 1].re: numdig, ' ');
          writeln(x[i2 + i - 1, j2 + j - 1].im: numdig);
        end;  {i}
        writeln;
      end; {j}
      writeln('residuals:');
      for j := 1 to n do
      begin
        writeln('residual nr', j: 2);
        writeln(' ': 10, 'Re', ' ': 10, 'Im');
        for i := 1 to n do
        begin
          r := 0;
          for k := 1 to n do
            r := r + a[i1 + i - 1, j1 + k - 1] * x[i2 + k - 1, j2 + j - 1].re;
          r := r - lam[i1 + j - 1].re * x[i2 + i - 1, j2 + j - 1].re;
          r := r + lam[i1 + j - 1].im * x[i2 + i - 1, j2 + j - 1].im;
          Write(r: numdig, ' ');
          r := 0;
          for k := 1 to n do
            r := r + a[i1 + i - 1, j1 + k - 1] * x[i2 + k - 1, j2 + j - 1].im;
          r := r - lam[i1 + j - 1].re * x[i2 + i - 1, j2 + j - 1].im;
          r := r - lam[i1 + j - 1].im * x[i2 + i - 1, j2 + j - 1].re;
          writeln(r: numdig);
        end; {i}
        writeln;
      end; {j}
    end; {term=1}
    writeln('-------------------------------------------');
  end; {l}
  Close(input);
  Close(output);
end.
