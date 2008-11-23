program Project1;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type

  { TFoo }

  TFoo = Object
    a : integer;
    function b : TFoo;
  end;

  tfoo2 = object(tfoo)
  end;

{ TFoo }

function TFoo.b: TFoo;
begin
  result.a := 5;
  writeln(IntToStr(self.a));
  if (self.a<>2) then
    halt(1);
end;

procedure t;
var x : TFoo;
begin
  x.a := 2;
  x := tfoo2(x).b;
  writeln(IntToStr(x.a));
  if (x.a<>5) then
    halt(2);
end;

begin
  t;
end.
