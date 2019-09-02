{$r+}

program rangeTest;
const
  w : longint = 123;
  n : longint = 48;
  m : longint = -5;
  k : longint = low(longint);
begin
  if (w<=1) and (w>=10) then
    halt(1);

  if (w>=1) and (w<=1000) then
  else
    halt(2);

  if (n>44)and(n<48) then
    halt(3);

  if (m>=-4) and (m<=$7ffffffe) then
    halt(4);

  if (m>=-5) and (m<=$7ffffffe) then
  else
    halt(5);

  if (m>=-$7fffffff) and (m<=$7ffffffe) then
  else
    halt(6);

  if (k>=-$7fffffff) and (k<=$7ffffffe) then
    halt(7);
end.
