{$mode objfpc}
type
  TObj = object
    function GetItem(const i :Integer) :Integer;
    property Items[i :Integer] :Integer read GetItem; default;
  end;

function TObj.GetItem(const i :Integer) :Integer;
begin
  Result := i;
end;

var
  Obj :TObj;

begin
  WriteLn(Obj[0],' ',Obj[10]);
end.