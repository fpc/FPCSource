{ %VERSION=1.1 }

{$mode objfpc}
type
  IInterface = interface(IUnknown)
     procedure mydo;
  end;

  TMyClass = class(TInterfacedObject, IInterface)
     procedure mydo;virtual;
  end;

var
   l : longint;

procedure tmyclass.mydo;

  begin
     l:=1;
  end;

var
  c: TMyClass;
  i: IInterface;

begin
  c := TMyClass.Create;
  i := c;
  l:=0;
  i.mydo;
  if l<>1 then
    halt(1);
end.
