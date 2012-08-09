var
  b: Boolean;
begin
 b:=true;
 b := longbool(b = b);
 if not b then
   halt(1);
end.

