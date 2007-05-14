{$q+}
{$r+}

type
   range = 0..32;
var
   a,b : Cardinal;
   one : range;
begin
   a := $80000000;
   one := 1;
   b := a div one;
   WriteLn(b);
end.
