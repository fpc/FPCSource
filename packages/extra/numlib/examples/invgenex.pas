program invgenex;

uses
  typ, iom, inv;

const
  n = 4;
  
var
  term: arbint;
  A:    array[1..n, 1..n] of arbfloat;
  
begin
  Assign(input, ParamStr(1));
  reset(input);
  Assign(output, ParamStr(2));
  rewrite(output);
  writeln('program results invgenex');
  { Read matrix A}
  iomrem(input, A[1, 1], n, n, n);
  { Print matrix A }
  writeln;
  writeln('A =');
  iomwrm(output, A[1, 1], n, n, n, numdig);
  { Calculate inverse of A}
  invgen(n, n, A[1, 1], term);
  writeln;
  writeln('term=', term: 2);
  if term = 1 then
    { Print inverse of matrix A}
  begin
    writeln;
    writeln('inverse of A =');
    iomwrm(output, A[1, 1], n, n, n, numdig);
  end; {term=1}
  Close(input);
  Close(output);
end.
