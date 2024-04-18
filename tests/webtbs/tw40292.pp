var
  arr: array of longint;
  x: longint;
begin
  x:=12+random(0);
  setlength(arr,13+random(0));
  if (x < length(arr)) <> (x <= high(arr)) then
    halt(1);

  if (length(arr) > x) <> (x <= high(arr)) then
    halt(2);

  x:=13+random(0);
  if (x >= length(arr)) <> (x > high(arr)) then
    halt(3);

  if (length(arr) <= x) <> (x > high(arr)) then
    halt(4);
end.
