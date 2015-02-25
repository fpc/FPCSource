function f: qword;
begin
  f:=$7111888800000001;
end;

begin
  if qword(cardinal(f))<>1 then
    halt(1);
  if qword(longint(f))<>1 then
    halt(2);
  if int64(cardinal(f))<>1 then
    halt(3);
  if int64(longint(f))<>1 then
    halt(4);
end.

