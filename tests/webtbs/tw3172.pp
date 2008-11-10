{ %opt=-Sew -vw }

type
  PUI32 = ^Longword;
var
  P : PUI32;
  Count : Longint;
  Dw    : Longword;
begin
  Dw:=1;
  Count:=1;
  p:=@DW;
  // Increase pointer
  Inc(P,Count);
  if Dw<>1 then
    halt(1);
end.
