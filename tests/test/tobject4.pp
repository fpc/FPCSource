{$mode objfpc}
type
  to1 = class
    a1 : array[0..100] of byte;
    s1 : ansistring;
    a2 : array[0..100] of byte;
    s2 : ansistring;
    s3 : ansistring;
  end;

var
  o1 : to1;

begin
  o1:=to1.create;
  o1.destroy;
end.
