{ %version=1.1 }
{$mode objfpc}
uses
  typinfo;
type
  tmyobject = class
      fs : single;
    published
      property s : single read fs write fs default 3.1415;
  end;

var
  myobject : tmyobject;

begin
  myobject:=tmyobject.create;
  writeln(GetFloatProp(myobject,'S'));
  myobject.free;
end.
