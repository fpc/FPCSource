{ %fail }
{ test c style assignment operators }

{$COPERATORS OFF}

var
  i : Single;
begin
  i:=1234;
  i += 1;
end.
