{ %opt=-g-t }

program Project1;

{$mode objfpc}{$H+}

procedure Foo;
var
  a: TObject;
begin
  if ptruint(a)<>$55555555 then
    halt(1);
end;

begin
  Foo;
end.
