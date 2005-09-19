{ the test checks only if the syntax is possible }
var
  ca : array[0..1000] of char;
  p1 : pchar;

begin
  p1:=nil;
  if (ca-p1)=0 then
    halt(1); 
  p1:=ca;
end.
   