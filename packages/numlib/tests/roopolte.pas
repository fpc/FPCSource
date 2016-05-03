program roopolte;

uses
  typ,
  roo;

const
  nn = 30;
var
  i, j, num, n, k, term: ArbInt;
  a: array[1..nn] of ArbFloat;
  z: array[1..nn] of complex;
begin
  Write(' program results roopolte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(num);
  writeln;
  writeln(' number of examples:', num: 3);
  for i := 1 to num do
  begin
    Read(n);
    writeln;
    writeln('n =', n: 2);
    writeln;
    writeln(' j    a[j]');
    writeln;
    for j := 1 to n do
    begin
      Read(a[j]);
      writeln(j: 2, ' ': 2, a[j]: numdig);
    end; {j}
    roopol(a[1], n, z[1], k, term);
    writeln;
    Write('term =', term: 2);
    if term = 1 then
    begin
      writeln('  k =', k: 2);
      writeln;
      writeln(' j', ' ': 4, 'Re(z[j])', ' ': 11, 'Im(z[j])');
      writeln;
      for j := 1 to k do
        writeln(j: 2, ' ': 2, z[j].Re: numdig, ' ': 2, z[j].imag: numdig);
    end; {term=1}
  end; {i}
  Close(input);
  Close(output);
end.
program roopolte;

uses
  typ,
  roo;

const
  nn = 30;
var
  i, j, num, n, k, term: ArbInt;
  a: array[1..nn] of ArbFloat;
  z: array[1..nn] of complex;
begin
  Write(' program results roopolte');
  case sizeof(ArbFloat) of
    4: writeln('(single)');
    6: writeln('(real)');
    8: writeln('(double)');
  end;
  Read(num);
  writeln;
  writeln(' number of examples:', num: 3);
  for i := 1 to num do
  begin
    Read(n);
    writeln;
    writeln('n =', n: 2);
    writeln;
    writeln(' j    a[j]');
    writeln;
    for j := 1 to n do
    begin
      Read(a[j]);
      writeln(j: 2, ' ': 2, a[j]: numdig);
    end; {j}
    roopol(a[1], n, z[1], k, term);
    writeln;
    Write('term =', term: 2);
    if term = 1 then
    begin
      writeln('  k =', k: 2);
      writeln;
      writeln(' j', ' ': 4, 'Re(z[j])', ' ': 11, 'Im(z[j])');
      writeln;
      for j := 1 to k do
        writeln(j: 2, ' ': 2, z[j].Re: numdig, ' ': 2, z[j].imag: numdig);
    end; {term=1}
  end; {i}
  Close(input);
  Close(output);
end.
