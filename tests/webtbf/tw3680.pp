{ %fail }

function c(x:char):char;
begin
c:=(char(succ(ord(x))));
end;

begin
if not c('y') in ['a','b'] then writeln(99);
end.
