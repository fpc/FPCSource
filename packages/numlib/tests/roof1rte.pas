program roof1rte;

uses
  typ,
  spe,
  roo;

const
  num = 4;
var
  term, i, j, n, p: ArbInt;
  a, b, ae, re, x:  ArbFloat;

  function f(x: ArbFloat): ArbFloat;
  begin
    case i of
      1: f := spepow(x - 1, 3);
      2: f := cos(x);
      3: f := sin(x) - x / 2;
      4: f := exp(x) - sqr(sqr(x))
    end;
  end;

begin
  Write(' program results roof1rte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  writeln;
  writeln(' number of examples:', num: 3);
  writeln;
  if sizeof(ArbFloat) = 6 then
    p := 8
  else
    p := 10;
  writeln(' ': 2, 'a', ' ': 5, 'b', ' ': 5, 'ae', ' ': p, 're',
    ' ': p - 2, 'term', ' ': 2, 'root', ' ': numdig - 2, 'f(root)');
  for i := 1 to num do
  begin
    Write('Locating the root of the equation ');
    case i of
      1: writeln('(x-1)**3 = 0');
      2: writeln('cos(x) = 0');
      3: writeln('sin(x) = x/2');
      4: writeln('exp(x)=x**4');
    end;
    Read(n);
    for j := 1 to n do
    begin
      Read(a, b, ae, re);
      roof1r(@f, a, b, ae, re, x, term);
      Write(a: 4: 1, ' ': 2, b: 4: 1, ' ': 2, ae: p, ' ': 2, re: p, ' ': 2, term: 1);
      if term < 3 then
        writeln(' ': 2, x: numdig, ' ': 2, f(x): numdig)
      else
        writeln;
    end;
  end;
  Close(input);
  Close(output);
end.
program roof1rte;

uses
  typ,
  spe,
  roo;

const
  num = 4;
var
  term, i, j, n, p: ArbInt;
  a, b, ae, re, x:  ArbFloat;

  function f(x: ArbFloat): ArbFloat;
  begin
    case i of
      1: f := spepow(x - 1, 3);
      2: f := cos(x);
      3: f := sin(x) - x / 2;
      4: f := exp(x) - sqr(sqr(x))
    end;
  end;

begin
  Write(' program results roof1rte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  writeln;
  writeln(' number of examples:', num: 3);
  writeln;
  if sizeof(ArbFloat) = 6 then
    p := 8
  else
    p := 10;
  writeln(' ': 2, 'a', ' ': 5, 'b', ' ': 5, 'ae', ' ': p, 're',
    ' ': p - 2, 'term', ' ': 2, 'root', ' ': numdig - 2, 'f(root)');
  for i := 1 to num do
  begin
    Write('Locating the root of the equation ');
    case i of
      1: writeln('(x-1)**3 = 0');
      2: writeln('cos(x) = 0');
      3: writeln('sin(x) = x/2');
      4: writeln('exp(x)=x**4');
    end;
    Read(n);
    for j := 1 to n do
    begin
      Read(a, b, ae, re);
      roof1r(@f, a, b, ae, re, x, term);
      Write(a: 4: 1, ' ': 2, b: 4: 1, ' ': 2, ae: p, ' ': 2, re: p, ' ': 2, term: 1);
      if term < 3 then
        writeln(' ': 2, x: numdig, ' ': 2, f(x): numdig)
      else
        writeln;
    end;
  end;
  Close(input);
  Close(output);
end.
