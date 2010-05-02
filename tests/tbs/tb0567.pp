begin
  if (pred(-128)<>-129) or
     (succ(127)<>128) then
    halt(1);
  if (pred(0)<>-1) or
     (succ(255)<>256) then
    halt(2);
  if (pred(-32768)<>-32769) or
     (succ(32767)<>32768) then
    halt(3);
  if (succ(65535)<>65536) then
    halt(4);
  if (pred(-2147483648)<>-2147483649) or
     (succ(2147483647)<>2147483648) then
    halt(5);
  if (succ(4294967295)<>4294967296) then
    halt(6);

  if (pred(bytebool(false))<>bytebool(true)) then
    halt(7);
  if (succ(bytebool(true))<>bytebool(false)) then
    halt(8);
  if (pred(wordbool(false))<>wordbool(true)) then
    halt(9);
  if (succ(wordbool(true))<>wordbool(false)) then
    halt(10);
  if (pred(longbool(false))<>longbool(true)) then
    halt(11);
  if (succ(longbool(true))<>longbool(false)) then
    halt(12);
end.
