program iomwrmte;

uses
  typ,
  iom;

const
  n1 = -5;
  n2 = 10;
  m1 = -3;
  m2 = 20;
  r  = m2 - m1 + 1;
  p  = 3;
  q  = 2;
  n  = 7;
  m  = 15;

var
  i, j, f, s: ArbInt;
  a: array[n1..n2, m1..m2] of ArbFloat;
begin
  Assign(output, ParamStr(2));
  rewrite(output);

  Write(output, ' program results iomwrmte');
  s := sizeof(ArbFloat);
  case s of
    4: writeln(output, '(single)');
    6: writeln(output, '(real)');
    8: writeln(output, '(double)')
  end;
  writeln(output);
  for i := 1 to n do
    for j := 1 to m do
      a[i + p - 1, j + q - 1] := i + j * 1e-3;
  for f := minform to maxform do
  begin
    writeln(output, 'A = (form=', f: 2, ')');
    iomwrm(output, a[p, q], n, m, r, f);
    writeln(output);
  end;
  Close(output);
end.
program iomwrmte;

uses
  typ,
  iom;

const
  n1 = -5;
  n2 = 10;
  m1 = -3;
  m2 = 20;
  r  = m2 - m1 + 1;
  p  = 3;
  q  = 2;
  n  = 7;
  m  = 15;

var
  i, j, f, s: ArbInt;
  a: array[n1..n2, m1..m2] of ArbFloat;
begin
  Assign(output, ParamStr(2));
  rewrite(output);

  Write(output, ' program results iomwrmte');
  s := sizeof(ArbFloat);
  case s of
    4: writeln(output, '(single)');
    6: writeln(output, '(real)');
    8: writeln(output, '(double)')
  end;
  writeln(output);
  for i := 1 to n do
    for j := 1 to m do
      a[i + p - 1, j + q - 1] := i + j * 1e-3;
  for f := minform to maxform do
  begin
    writeln(output, 'A = (form=', f: 2, ')');
    iomwrm(output, a[p, q], n, m, r, f);
    writeln(output);
  end;
  Close(output);
end.
