program roofnrte;

uses
  typ,
  spe,
  roo;

const
  num  = 3;
  nmax = 3;
var
  term, i, j, k, l, n: ArbInt;
  re, residu: ArbFloat;
  x: array[1..nmax] of ArbFloat;

  procedure f(var x0, fx: ArbFloat; var deff: boolean);
  var
    xloc: array[1..nmax] of ArbFloat absolute x0;
    f:    array[1..nmax] of ArbFloat absolute fx;
    x, y, z: ArbFloat;
  begin
    x := xloc[1];
    y := xloc[2];
    if n = 3 then
      z := xloc[3];
    case i of
      1:
      begin
        if j * 2 <= k then
          deff := x >= 0
        else
          deff := y >= 0;
        f[1] := x * x - y * y - 2;
        f[2] := x + y - 1;
      end;
      2:
      begin
        f[1] := exp(x) + exp(y) - exp(z);
        f[2] := sin(x) + cos(y) - z;
        f[3] := x * y - sqr(z);
      end;
      3: if (x > 0) and (y > 0) then
        begin
          f[1] := spepow(x, y) - spepow(y, x);
          f[2] := sin(x) - cos(y);
        end
        else
          deff := False
    end;
  end;

begin
  Write(' program results roofnrte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  writeln;
  writeln(' number of examples:', num: 3);
  for i := 1 to num do
  begin
    writeln;
    writeln('Locating the root of the equations ');
    case i of
      1:
      begin
        n := 2;
        writeln('x + y = 1');
        writeln('xý + yý = 2');
      end;
      2:
      begin
        n := 3;
        writeln('exp(x) + exp(y) = exp(z)');
        writeln('sin(x) + cos(y) = z');
        writeln('xy = zý');
      end;
      3:
      begin
        n := 2;
        writeln('xy = yx');
        writeln('sin(x) = cos(y)');
      end
    end;
    Read(k);
    for j := 1 to k do
    begin
      for l := 1 to n do
        Read(x[l]);
      Read(re);
      writeln(' starting values: (n=', n: 1, ')');
      for l := 1 to n do
        Write(x[l]: numdig, ' ');
      writeln(' re =', re: 8);
      roofnr(@f, n, x[1], residu, re, term);
      writeln;
      writeln(' term =', term: 2);
      if term < 3 then
      begin
        writeln(' solution vector');
        for l := 1 to n do
          Write(x[l]: numdig, ' ');
        writeln;
        writeln(' residu = ', residu: 8);
      end;
      writeln('-------------------------------------------------');
    end;
    writeln('======================================================');
  end;
  Close(input);
  Close(output);
end.
program roofnrte;

uses
  typ,
  spe,
  roo;

const
  num  = 3;
  nmax = 3;
var
  term, i, j, k, l, n: ArbInt;
  re, residu: ArbFloat;
  x: array[1..nmax] of ArbFloat;

  procedure f(var x0, fx: ArbFloat; var deff: boolean);
  var
    xloc: array[1..nmax] of ArbFloat absolute x0;
    f:    array[1..nmax] of ArbFloat absolute fx;
    x, y, z: ArbFloat;
  begin
    x := xloc[1];
    y := xloc[2];
    if n = 3 then
      z := xloc[3];
    case i of
      1:
      begin
        if j * 2 <= k then
          deff := x >= 0
        else
          deff := y >= 0;
        f[1] := x * x - y * y - 2;
        f[2] := x + y - 1;
      end;
      2:
      begin
        f[1] := exp(x) + exp(y) - exp(z);
        f[2] := sin(x) + cos(y) - z;
        f[3] := x * y - sqr(z);
      end;
      3: if (x > 0) and (y > 0) then
        begin
          f[1] := spepow(x, y) - spepow(y, x);
          f[2] := sin(x) - cos(y);
        end
        else
          deff := False
    end;
  end;

begin
  Write(' program results roofnrte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  writeln;
  writeln(' number of examples:', num: 3);
  for i := 1 to num do
  begin
    writeln;
    writeln('Locating the root of the equations ');
    case i of
      1:
      begin
        n := 2;
        writeln('x + y = 1');
        writeln('xý + yý = 2');
      end;
      2:
      begin
        n := 3;
        writeln('exp(x) + exp(y) = exp(z)');
        writeln('sin(x) + cos(y) = z');
        writeln('xy = zý');
      end;
      3:
      begin
        n := 2;
        writeln('xy = yx');
        writeln('sin(x) = cos(y)');
      end
    end;
    Read(k);
    for j := 1 to k do
    begin
      for l := 1 to n do
        Read(x[l]);
      Read(re);
      writeln(' starting values: (n=', n: 1, ')');
      for l := 1 to n do
        Write(x[l]: numdig, ' ');
      writeln(' re =', re: 8);
      roofnr(@f, n, x[1], residu, re, term);
      writeln;
      writeln(' term =', term: 2);
      if term < 3 then
      begin
        writeln(' solution vector');
        for l := 1 to n do
          Write(x[l]: numdig, ' ');
        writeln;
        writeln(' residu = ', residu: 8);
      end;
      writeln('-------------------------------------------------');
    end;
    writeln('======================================================');
  end;
  Close(input);
  Close(output);
end.
