{ div on two Currency values has to rescale the raw quotient back into a
  Currency value, and the result of mod has to stay marked as scaled }
program tw41865c;

{$mode objfpc}

var
  a, b, r: currency;
begin
  a := 10;
  b := 3;
  r := a div b;
  { 3.0000 stored as currency is 30000; trunk stores the raw quotient 3 }
  if PInt64(@r)^ <> 30000 then
    Halt(1);
  r := a mod b;
  if PInt64(@r)^ <> 10000 then
    Halt(2);
  { the value of a mod b is stored correctly, but is treated as unscaled }
  if (a mod b) + 1 <> 2 then
    Halt(3);
  if not ((a mod b) = 1) then
    Halt(4);
  WriteLn('ok');
end.
