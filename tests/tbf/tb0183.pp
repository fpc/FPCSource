{ %OPT=-Sew -vw }
{ %fail }

{$T-}

var
  i: integer;
  p: pointer;
begin
  p := @i+5;
end.
