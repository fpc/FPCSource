function f(l1,l2,l3,l4,l5,l6,l7,l8:longint):longint;
begin
  f:=l1+l2+l3+l4+l5+l6+l7+l8;
end;

var
  l : longint;
begin
  l:=f(f(1,2,3,4,5,6,7,8),f(1,2,3,4,5,6,7,8),f(1,2,3,4,5,6,7,8),f(1,2,3,4,5,6,7,8),f(1,2,3,4,5,6,7,8),f(1,2,3,4,5,6,7,8),f(1,2,3,4,5,6,7,8),f(1,2,3,4,5,6,7,8));
  writeln('Got ',l,' expected ',8*(1+2+3+4+5+6+7+8));
  if l<>8*(1+2+3+4+5+6+7+8) then
    begin
      writeln('Error!');
      halt(1);
    end;
end.

