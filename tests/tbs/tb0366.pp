{$ifdef fpc}{$mode objfpc}{$endif}

uses
  ub0366;

type
  tc2=class
  public
    FHeight : integer;
    procedure p1;
  end;

procedure tc2.p1;
var
  c1 : tc1;
begin
  FHeight:=10;
  c1:=tc1.create;
  with c1 do
   begin
     Height:=FHeight;
   end;
  writeln('c1.Height: ',c1.Height,' (should be 10)');
  if c1.Height<>10 then
   begin
     writeln('ERROR!');
     halt(1);
   end;
  c1.free;
end;

var
  c2 : tc2;
begin
  c2:=tc2.create;
  c2.p1;
  c2.free;
end.
