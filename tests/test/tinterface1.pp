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

var
   l : longint;

procedure tmyclass.mydo;

  begin
     l:=1;
  end;

var
  c: TMyClass;
  i: IInterface;
  c2 : TMyClass;

begin
  c := TMyClass.Create;
  i := c;
  l:=0;
  i.mydo;
  if l<>1 then
    halt(1);
  c2 := TMyClass2.Create;
  i := c2;
  l:=0;
  i.mydo;
  if l<>1 then
    halt(1);
end.
