program FPTest;
{$mode delphi}

type
  iintf = interface(IUnknown)
    function GetIntf :iintf;
    procedure DoSomething;
  end; 

  tobj = class(TObject)
    fintf: iintf;
    procedure test1;
    procedure test2;
  end;

  tintf = class(TInterfacedObject,iintf)
    function GetIntf : iintf;
    procedure DoSomething;
  end;

procedure tobj.test1;
begin
  fintf.DoSomething;
end;

procedure tobj.test2;
begin
  fintf.GetIntf.GetIntf.DoSomething;
end;


function tintf.GetIntf : iintf;
  begin
    result:=self;
  end;
  
var
  refs : Integer;
  
procedure tintf.DoSomething;
  begin
    if RefCount<>refs then
      halt(1);
    writeln(RefCount);
  end;

var
  obj : tobj;
begin
  obj:=tobj.create;
  obj.fintf:=tintf.create;
  refs:=1;
  obj.test1;
  refs:=3;
  obj.test2;
  obj.free;
  writeln('ok');
end.                      
