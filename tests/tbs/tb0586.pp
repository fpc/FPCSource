{ %NORUN }

program tb0586;

{$mode objfpc}

procedure Test;
var
  a, b: Boolean;
  obj: TObject;
begin
  if assigned(obj)<>(a=b) then
    ;
end;

begin

end.
