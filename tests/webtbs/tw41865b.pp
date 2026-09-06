{ negating a Currency value must not change its scaling }
program tw41865b;

{$mode objfpc}

var
  c: currency;
  d: double;
begin
  c := 10;
  if -c <> -10 then
    Halt(1);
  if -c + 1 <> -9 then
    Halt(2);
  if 1 + -c <> -9 then
    Halt(3);
  if -c - 1 <> -11 then
    Halt(4);
  if -c * 2 <> -20 then
    Halt(5);
  d := -c;
  if d <> -10 then
    Halt(6);
  if not (-c = -10) then
    Halt(7);
  WriteLn('ok');
end.
