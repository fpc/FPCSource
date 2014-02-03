{ %OPT=-gh }
{$mode objfpc}{$h+}
{ Test that exception in object constructor does not cause memory leak }

uses sysutils;

type
  pobj=^obj;
  obj=object
    constructor init;
    destructor done; virtual;
  end;

constructor obj.init;
begin
  raise exception.create('oops!');
end;

destructor obj.done;
begin
end;

var
  p: pobj;

begin
  HaltOnNotReleased:=true;
  try
    new(p,init);
  except
  end;
end.
