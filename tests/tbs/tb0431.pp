{ the smallest falling test I ever found PM }
{ the code generated a shll #0,%edx on i386
  which was bad but harmless...
  but on m68K it generated a asl.l #0,%d2
  which is not correct ... }

const
  has_errors : boolean = false;
var
  x : longint;
begin
  x:=5;
  x:=x*1;
  if x<>5 then
    has_errors:=true;
  x:=5;
  x:=x shl 0;
  if x<>5 then
    has_errors:=true;
  x:=5;
  x:=x shr 0;
  if x<>5 then
    has_errors:=true;
  if has_errors then
    halt(1);
end.
