uses
{$ifdef go32v2}
dpmiexcp,
{$endif}
sysutils;
var i,j,k:real;
begin
  i:=100;
  read(j);
  try
    k:=i/j;
    writeln(k:5:3);
  except
    k:=0;
    writeln('Illegal Input');
  end;
end.
