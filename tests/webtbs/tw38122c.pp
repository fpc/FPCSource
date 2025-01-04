program tw38122c;
{$mode delphi}
uses sysutils;

 var
   j:integer;
begin
  j:=22;
  if pinteger(@j)^.tostring <> '22' then
    halt(1);
end.

