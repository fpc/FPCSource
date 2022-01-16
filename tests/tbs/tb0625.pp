program tb0625;

var
  i: Int64;
  b: Boolean;
begin
  i := 1;
  b := Boolean(i);
  if not b then
    Halt(1);
  i := 0;
  b := Boolean(i);
  if b then
    Halt(2);
  i := 42;
  b := Boolean(i);
  if not b then
    Halt(3);
  i := $ffffffffffffffff;
  b := Boolean(i);
  if not b then
    Halt(4);
  i := $ffffffffffffff00;
  b := Boolean(i);
  if b then
    Halt(5);
end.
