{ %cpu=i8086 }
{$MODE TP}
program tsegie;

var
  a: procedure;
  b: word;

begin
  a:=nil;
  b:=seg(a);
  if b<>0 then
    halt(1);
end.
