{ %wpoparas=optvmts }
{ %wpopasses=1 }


program test;

type
  PMyObj=^TMyObj;
  TMyObj = object
    constructor init;
    procedure dummy;virtual;abstract;
  end;

constructor TMyObj.init;
begin
end;

begin
  PMyObj(nil)^.init;
end.

