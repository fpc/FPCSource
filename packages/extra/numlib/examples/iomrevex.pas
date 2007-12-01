program iomrevex;

uses
  typ, iom;

const
  n = 5;
  
var
  v: array[1..n] of ArbFloat;
  
begin
  Assign(input, ParamStr(1));
  reset(input);
  Assign(output, ParamStr(2));
  rewrite(output);
  {inlezen van de vector v}
  iomrev(input, v[1], n);
  writeln('program results iomrevex');
  writeln;
  {afdrukken van de vector}
  writeln('v =');
  writeln;
  iomwrv(output, v[1], n, 12);
  Close(input);
  Close(output);
end.
