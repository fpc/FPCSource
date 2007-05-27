{ %VERSION=1.1 }
{ %SKIPTARGET=macos }
{ On macos it crashes when run.}

{$mode objfpc}
type
  IInterface = interface(IUnknown)
     procedure mydo;
  end;

  TMyClass = class(TInterfacedObject, IInterface)
     procedure mydo;virtual;
  end;

  TMyClass2 = class(TMyClass)
     i : integer;
  end;

  TMyClass3 = class
    private
      fi: IInterface;
    public
      property intf: IInterface read fi write fi;
  end;



var
   l : longint;

procedure tmyclass.mydo;

  begin
     l:=1;
  end;

var
  c: TMyClass;
  c2 : TMyClass;
  c3 : TMyClass3;

begin
  c := TMyClass.Create;
  c3 := TMyClass3.Create;
  c3.intf := c;
  l:=0;
  c3.intf.mydo;
  if l<>1 then
    halt(1);
  c2 := TMyClass2.Create;
  c3.intf := c2;
  l:=0;
  c3.intf.mydo;
  if l<>1 then
    halt(1);
  c3.free;
end.
