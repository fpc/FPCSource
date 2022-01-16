{ %opt=-gh }

{$mode objfpc}{$H+}

procedure clear(out x);
begin
  pointer(x):=nil;
end;

procedure test;
var
  ii1: iunknown;
begin
  ii1:=tinterfacedobject.create;
  clear(ii1);
end;

begin
  HaltOnNotReleased:=true;
  test;
end.
