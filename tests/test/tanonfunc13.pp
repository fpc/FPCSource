program tanonfunc13;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test variable capture }

type
  tintproc = reference to procedure(x: longint);

var
  acc: longint;

function foo(z: longint): tintproc;
var
  y: integer;
begin
  y := 100;
  result := procedure(x: longint) begin
    acc := x + y + z;
  end;
end;

var
  p: tintproc;
begin
  p := foo(20);
  p(3);
  if acc <> 123 then
    halt(1);
end.

