program iomrewrsex;

uses
  typ, iom, inv;

var
  A:    array[1..10, 1..10] of arbfloat;
  i:    longint;
  j:    longint;
  s:    ArbString;
  n, m: longint;
  
begin
  iomrems('{1 2}{3 4}{5 6}', A[1, 1], m, n, 10);

  for i := 1 to m do
    for j := 1 to n do
      writeln('A[', i, ',', j, '] = ', A[i, j]);

  { Print matrix A }
  iomwrms(s, A[1, 1], m, n, 2, 10);
  writeln('A = ', s);
end.
