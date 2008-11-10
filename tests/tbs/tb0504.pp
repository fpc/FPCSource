{ %OPT=-Sew -vw }

{$T-}

var
  i: integer;
  p: pointer;
begin
  p := pointer(@i)+5;
  i := integer(p-p);
end.
