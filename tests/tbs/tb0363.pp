{ %VERSION=1.1 }

procedure p1(const a:array of byte);
var
  l : longint;
begin
  l:=length(a);
  writeln('openarray length: ',l);
  if l<>9 then
   halt(1);
end;

var
  a : array[2..10] of byte;
  l : longint;
begin
  l:=length(a);
  writeln('length of a ',l);
  if l<>9 then
   halt(1);

  p1(a);
end.
