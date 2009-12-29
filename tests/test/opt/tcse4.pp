{$mode objfpc}
type
  tr = record
    data1 : longint;
    data2 : longint;
  end;

  tc = class
    r : ^tr;
    function f : longint;
  end;

function tc.f : longint;
  begin
    result:=r^.data1+r^.data2*r^.data1;
  end;

begin
end.
