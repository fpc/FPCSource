{ %OPT=-Sew }
{ %fail }

{$T-}

var
  i: integer;
  p: pointer;
begin
  p := pointer(@i)+5;
  i := integer(@i-p);
end.
