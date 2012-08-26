{ %opt=-O-4 }
{ %result=201 }

{$inline on}
{$r+}

var
  l: longint;

function f: longint;
begin
  l:=5;
  f:=2;
end;

procedure test(l: longint); inline;
begin
end;

var
  a: array[0..0] of byte;
begin
  test(a[f]);
end.
