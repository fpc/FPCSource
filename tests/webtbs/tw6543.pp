{$ifdef fpc}
{$mode objfpc}
{$endif}

program p;

type
  c = class
    a: array[boolean] of byte;
    property f: byte read a[false] write a[false];
  end;

var
  o: c;
begin
  o := c.Create;
  o.f := 1;
  if (o.a[false] <> 1) then
    halt(1);
end.
