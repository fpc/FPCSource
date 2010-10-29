uses
  Math;

var
  l: longint;
  c: cardinal;
  i: int64;
  q: qword;
  s: single;
  d: double;
begin
  l:=-12345;
  c:=56789;
  i:=-56789;
  q:=12345;

  s:=l;
  if s<>-12345 then
    halt(1);
  s:=c;
  if s<>56789 then
    halt(2);
  s:=i;
  if s<>-56789 then
    halt(3);
  s:=q;
  if s<>12345 then
    halt(4);
  
  d:=l;
  if d<>-12345 then
    halt(5);
  d:=c;
  if d<>56789 then
    halt(6);
  d:=i;
  if d<>-56789 then
    halt(7);
  d:=q;
  if d<>12345 then
    halt(8);
end.
