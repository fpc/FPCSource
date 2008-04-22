{ %OPT=-gh }
{$ifdef fpc}
{$mode objfpc}
{$endif fpc}
uses
  classes;

type
  to2 = class(TObject,IInterface)
    fi : TInterfacedObject;
    property i : TInterfacedObject read fi implements IInterface;
  end;

  to1 = class(TObject,IInterface)
    fi : to2;
    property i : to2 read fi implements IInterface;
  end;

var
  o1 : to1;
  o2 : to2;
  i1,i2 : IInterface;
begin
  o2:=to2.create;
  o2.fi:=TInterfacedObject.Create;

  o1:=to1.create;
  o1.fi:=o2;
  writeln('o1, o2 and o2.fi created');
  i1:=o1;
  i1.QueryInterface(IInterface,i2);
  writeln('i2 queried the first time');
  if i2=nil then
    halt(1);
  writeln('releasing and setting o1.fi to nil');
  o1.fi.free;
  o1.fi:=nil;

  o1.free;

  writeln('ok');
end.

