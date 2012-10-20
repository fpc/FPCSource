{ %NORUN }

program tb0584;

{$mode objfpc}

type
  TSomeObj = class
    function Test(s: String): TObject;
  end;

  TSomeOtherObj = class
  public
    function GetFoo: String;
  end;

function TSomeObj.Test(s: String): TObject;
begin

end;

function TSomeOtherObj.GetFoo: String;
begin

end;

var
  SomeObj: TSomeObj;

procedure Test;
var
  b: Boolean;
  obj: TSomeOtherObj;
begin
  b := not Assigned(SomeObj.Test(obj.GetFoo));
end;

begin

end.
