{ %VERSION=1.1 }

{$mode objfpc}
type
  IMyInterface = interface
    function f : longint;
    procedure p(a : longint);
    property x : longint read f write p;
  end;

begin
end.
