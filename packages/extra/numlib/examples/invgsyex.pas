program invgsyex;

uses
  typ, iom, inv;

const
  n = 4;
  
var
  i, j, term: ArbInt;
  A: array[1..n, 1..n] of ArbFloat;
  
begin
  Assign(input, ParamStr(1));
  reset(input);
  Assign(output, ParamStr(2));
  rewrite(output);
  writeln('program results invgsyex');
  { Read bottomleft part of matrix A }
  for i := 1 to n do
    iomrev(input, A[i, 1], i);
  { print matrix A }
  writeln;
  writeln('A =');
  for i := 1 to n do
    for j := 1 to i - 1 do
      A[j, i] := A[i, j];
  iomwrm(output, A[1, 1], n, n, n, numdig);
  { calculate inverse of matrix A}
  invgsy(n, n, A[1, 1], term);
  writeln;
  writeln('term=', term: 2);
  if term = 1 then
    { print inverse of matrix A }
  begin
    writeln;
    writeln('inverse of A =');
    iomwrm(output, A[1, 1], n, n, n, numdig);
  end; {term=1}
  Close(input);
  Close(output);
end.
