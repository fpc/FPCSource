{ the smallest falling test I ever found PM }
{ the code generated a shll #0,%edx on i386
  which was bad but harmless...
  but on m68K it generated a asl.l #0,%d2
  which is not correct ... }

var
  x : longint;
begin
  x:=5;
  x:=x*1;
end.
