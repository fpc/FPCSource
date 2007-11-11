program iomwrvex;

uses
  typ, iom;

const
  n = 5;
  
var
  i: integer;
  v: array[1..n] of ArbFloat;
  
begin
  Assign(output, ParamStr(1));
  rewrite(output);
  writeln('program results iomwrv');
  writeln;
  {genereren van de vector v}
  randomize;
  for i := 1 to n do
    v[i] := 2 * random - 1;
  iomwrv(output, v[1], n, 15);
  {afdrukken van de vector}
  writeln;
  iomwrv(output, v[1], n, 12);
  Close(output);
end.
