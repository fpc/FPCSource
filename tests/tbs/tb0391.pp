{ %version=1.1 }

{$ifdef fpc}{$mode objfpc}{$endif}
uses ub0391;

type
  tc1 = class
    procedure p1(l:longint);
    procedure p2;
  end;

procedure tc1.p1(l:longint);
begin
  writeln('longint: ',l);
end;


procedure tc1.p2;
var
  c2 : tc2;
begin
  c2:=tc2.create;
  { the next code should take tc1.p1(longint) as the tc2.p1 can not
    be seen from here! }
  with c2 do
   p1(100);
  c2.free;
end;


var
  c1 : tc1;
begin
  c1:=tc1.create;
  c1.p2;
  c1.free;
end.
