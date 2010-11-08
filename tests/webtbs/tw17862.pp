{$mode objfpc}
type
 TField = record
   a,b,c: byte;
 end;
 tarray = bitpacked array[0..3] of tfield;

procedure test(var a: tfield);
begin
  if a.a<>3 then
    halt(1);
  if a.b<>4 then
    halt(2);
  if a.c<>5 then
    halt(3);
end;

var
  a: tarray;
begin
  a[1].a:=3;
  a[1].b:=4;
  a[1].c:=5;
  test(a[1]);
end.

