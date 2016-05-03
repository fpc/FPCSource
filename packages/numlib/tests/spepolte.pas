program spepolte;

uses
  spe,
  iom,
  typ;

const
  n1 = 0;
  n2 = 10;

var
  n, t: ArbInt;
  x: ArbFloat;
  h: string;
  a: array[n1..n2] of ArbFloat;
begin
  Write('program results spepolte');

  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) + 1;

  Read(n);
  iomrev(input, a[0], n + 1);
  Read(x);
  Write('coefficients of P:');
  iomwrv(output, a[0], n + 1, t);
  writeln;
  Write('x = ', x: t, '  ');
  writeln(' P(x) = ', spepol(x, a[0], n): t);
end.
program spepolte;

uses
  spe,
  iom,
  typ;

const
  n1 = 0;
  n2 = 10;

var
  n, t: ArbInt;
  x: ArbFloat;
  h: string;
  a: array[n1..n2] of ArbFloat;
begin
  Write('program results spepolte');

  case SizeOf(ArbFloat) of
    4: writeln('(single)');
    8: writeln('(double)');
    6: writeln('(real)');
  end;

  x := pi;
  Str(x, h);
  t := Length(h) + 1;

  Read(n);
  iomrev(input, a[0], n + 1);
  Read(x);
  Write('coefficients of P:');
  iomwrv(output, a[0], n + 1, t);
  writeln;
  Write('x = ', x: t, '  ');
  writeln(' P(x) = ', spepol(x, a[0], n): t);
end.
