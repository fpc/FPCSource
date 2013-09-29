{ %fail }
program Project1;
type
  PFoo = ^Integer;
var
  a: Pointer;
begin
  a:= @New(PFoo);
end.
