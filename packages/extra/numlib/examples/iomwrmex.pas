program iomwrmex;

uses
  typ, iom;

const
  m = 5;
  n = 13;
  
var
  i, j: integer;
  a:    array[1..m, 1..n] of ArbFloat;
  
begin
  Assign(output, ParamStr(1));
  rewrite(output);
  { genereren van de matrix A }
  for i := 1 to m do
    for j := 1 to n do
      a[i, j] := -(i + j / 10);
  writeln('program results iomwrm');
  writeln;
  { afdrukken van de deelmatrix }
  iomwrm(output, a[2, 3], 3, 6, n, 14);
  writeln;
  iomwrm(output, a[2, 3], 3, 6, n, 10);
  Close(output);
end.
