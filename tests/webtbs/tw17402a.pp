{ %opt=-Ootailrec -Cs1000000 }

{$mode objfpc}
{ check if tail recursion optimization works, at least on 32 bit OSes }
function fac(i : int64) : int64;
  var
    a : array[0..100000] of longint;
  begin
    a[1]:=1;
    if i=0 then
      result:=1
    else
      result:=fac(i-1);
  end;

begin
  fac(4000000);
end.
