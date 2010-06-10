{ %opt=-g-t }

program Project1;

{$mode objfpc}{$H+}

procedure Foo;
var
  a: TObject;
begin
{$ifdef cpu32}
  if ptruint(a)<>$55555555 then
    halt(1);
{$else}
  if ptruint(a)<>$5555555555555555 then
    halt(1);
{$endif}
end;

begin
  Foo;
end.
