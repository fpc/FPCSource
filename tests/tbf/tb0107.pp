{ %FAIL }

var
  i : longint;
  j,j2 : word;
  k : byte;

begin
  j:=5;
  byte(i):=j;
  longint(j):=12;
end.
