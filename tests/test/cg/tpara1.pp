function f(l1,l2,l3,l4,l5,l6,l7,l8,l9:longint):longint;
begin
  f:=l1+l2+l3+l4+l5+l6+l7+l8+l9;
end;

var
  l : longint;
begin
  l:=f(f(1,2,3,4,5,6,7,8,9),f(1,2,3,4,5,6,7,8,9),f(1,2,3,4,5,6,7,8,9),f(1,2,3,4,5,6,7,8,9),f(1,2,3,4,5,6,7,8,9),f(1,2,3,4,5,6,7,8,9),f(1,2,3,4,5,6,7,8,9),f(1,2,3,4,5,6,7,8,9),f(1,2,3,4,5,6,7,8,9));
  writeln('Got ',l,' expected ',9*(1+2+3+4+5+6+7+8+9));
  if l<>9*(1+2+3+4+5+6+7+8+9) then
    begin
      writeln('Error!');
      halt(1);
    end;
end.
