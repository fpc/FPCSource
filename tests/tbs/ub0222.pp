{ Old file: tbs0261a.pp }
{  }

unit ub0222;

{ test for operator overloading }
{ Copyright (c) 1999 Lourens Veen }
{ why doesn't this work? }

interface

type mythingy = record
       x, y : longint;
       c : byte;
       end;

     myotherthingy = record
       x, y : longint;
       d : byte;
       end;

     mythirdthingy = record
       x, y : longint;
       e : byte;
       end;

     mynewthingy = record
       x, y : longint;
       e,f : byte;
       end;

operator := (a : mythingy) r : myotherthingy;
operator := (a : mythingy) r : mythirdthingy;
operator = (b : myotherthingy;c : mythirdthingy) res : boolean;

implementation

operator := (a : mythingy) r : myotherthingy;
begin
  r.x := a.x;
  r.y := a.y;
  r.d := a.c;
end;

operator := (a : mythingy) r : mythirdthingy;
begin
  r.x := a.x;
  r.y := a.y;
  r.e := a.c;
end;

operator = (b : myotherthingy;c : mythirdthingy) res : boolean;
begin
  res:=(b.x=c.x) and (b.y=c.y) and (b.d=c.e);
end;

end.
