{ %version=1.1 }
{$mode objfpc}
{$M+}
uses
  typinfo;
type
  tmyobject = class
    protected
      fs : single;
    published
      property s : single read fs write fs default 3.1415;
  end;

var
  myobject : tmyobject;

begin
  myobject:=tmyobject.create;
  SetFloatProp(myobject,'s',3);
  if GetFloatProp(myobject,'s')<>3 then
    begin
      writeln('error');
      halt(1);
    end;
  myobject.free;
end.
