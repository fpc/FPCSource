{ %opt=-O-3 -Oonoconstprop }
{ %result=201 }

{$inline on}
{$r+}

procedure test(l: longint); inline;
begin
end;

var
  a: array[0..0] of byte;
  i: longint;
begin
  i:=1345;
  test(a[i]);
end.
