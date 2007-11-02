{ %fail }

{$mode tp}

var
  a: array[1..10] of byte;
  b:  byte;
begin
  b:=1;
  for a[b] := 1 to 10 do ;
end.
