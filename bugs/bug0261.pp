program bug0261;

{ test for operator overloading }
{ Copyright (c) 1999 Lourens Veen }
{ why doesn't this work? }
uses
  bug0261a;


var a : mythingy;
    b : myotherthingy;
    c : mythirdthingy;
begin
  a.x:=55;
  a.y:=45;
  a.c:=7;
  b:=a;
  c:=a;
  if b.d<>c.e then
    Writeln('Error in assignment overloading');
  if b<>c then
    Writeln('Error in equal overloading');
  Writeln('Sizeof(mythirdthingy)=',sizeof(mythirdthingy));
  Writeln('Sizeof(mynewthingy)=',sizeof(mynewthingy));
end.

