{ %FAIL }

{$mode fpc}
program test_const_objects;

type
   tobj1 = object
     x,y : integer;
   end;

   tobj2 = object(tobj1)
     z : integer;
     constructor init(ax,ay,az : integer);
     procedure display; virtual;
     end;

   tobj3 = object(tobj2)
     t : integer;
     constructor init(ax,ay,az,at : integer);
     procedure display; virtual;
     end;

constructor tobj2.init(ax,ay,az : integer);
begin
  x:=ax;
  y:=ay;
  z:=az;
end;

procedure tobj2.display;
begin
  Writeln(x,' ',y,' ',z);
end;

constructor tobj3.init(ax,ay,az,at : integer);
begin
  x:=ax;
  y:=ay;
  z:=az;
  t:=at;
end;

procedure tobj3.display;
begin
  Writeln(x,' ',y,' ',z,' ',t);
end;

const
  Obj1 : Tobj1 = (x : 1; y : 2);
  Obj2 : TObj2 = (x : 3; y : 4; z : 5);
  Obj3 : Tobj3 = (x : 6; y : 7; z : 8; t : 9);

begin
  Obj2.Display;
  Obj3.display;
end.
