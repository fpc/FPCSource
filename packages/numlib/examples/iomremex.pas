program iomremex;

uses
  typ, iom;

const
  m = 3;
  n = 2;
  
var
  a: array[1..m, 1..n] of ArbFloat;
  
begin
  Assign(input, ParamStr(1));
  reset(input);
  Assign(output, ParamStr(2));
  rewrite(output);
  {inlezen van de matrix}
  iomrem(input, a[1, 1], m, n, n);
  writeln('program results iomremex');
  writeln;
  writeln('A=');
  writeln;
  {afdrukken van de matrix}
  iomwrm(output, a[1, 1], m, n, n, 12);
  Close(input);
  Close(output);
end.
