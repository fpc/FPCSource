{$MODE objfpc}

program bug7;

type
  TMyObj = class;
  TMyObjClass = class of TMyObj;

  TMyObj = class(TObject)
     function ClassType: TMyObjClass; reintroduce;
     class function test: string;
  end;	

  TMyObj2 = class(TMyObj)
  end;


var O: TObject;



function TMyObj.ClassType: TMyObjClass;
begin
   Result := TMyObjClass(inherited ClassType);
end;

class function tmyobj.test: string;
begin
  result:=inherited classname;
end;

function GetObj: TObject;
begin
   Result := O
end;

function GetMyObj: TMyObj;
begin
   Result:= TMyObj(GetObj)
end; 



begin
   O := TMyObj2.Create;
   WriteLn(GetMyObj.ClassName);
   WriteLn(GetMyObj.ClassType.ClassName);
   if (GetMyObj.ClassName<>'TMyObj2') or
      (GetMyObj.ClassType.ClassName<>'TMyObj2') then
     halt(1);
   writeln(tmyobj.test);
   if (tmyobj.test<>'TMyObj') then
     halt(2);
end.
