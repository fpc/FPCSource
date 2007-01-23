{ %opt=-gt }

program test;

{$MODE OBJFPC}

type
  xstr = interface(iunknown) end;

operator := (a: integer): xstr;
begin
  if ptruint(result) <> ptruint(nil) then
    halt(1);
  pointer(result) := nil;
end;

var
  x: xstr;
begin
  x := 42;
end.
