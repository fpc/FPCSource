program test;
{$mode objfpc}

function GetFalse: Boolean;
begin
  Result := False;
end;

var
  obj: TInterfacedObject;
  i: Integer;

begin
  if GetFalse and (i >= 0) and (i < obj.RefCount) then;
end.