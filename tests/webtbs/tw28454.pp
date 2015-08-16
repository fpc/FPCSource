{$mode objfpc}


type
  tc = class(tinterfacedobject)
    l: longint;
    constructor create(f: longint);
  end;


constructor tc.create(f: longint);
  begin
    l:=f;
  end;


procedure test(out i1,i2: iinterface; k3,k4,k5,k6,k7,k8: longint; out i9,i10: iinterface); stdcall;
begin
  i1:=tc.create(1);
  i2:=tc.create(2);
  i9:=tc.create(9);
  i10:=tc.create(10);
end;

var
  i1,i2,i9,i10: iinterface;
begin
  test(i1,i2,3,4,5,6,7,8,i9,i10);
  if (i1 as tc).l<>1 then
    halt(1);
  if (i2 as tc).l<>2 then
    halt(2);
  if (i9 as tc).l<>9 then
    halt(3);
  if (i10 as tc).l<>10 then
    halt(4);
end.
