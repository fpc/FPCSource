type
  t1=longint;

procedure p;
type
  pt1=^t1;
  t1=string;
var
  t : t1;
  p : pt1;
begin
  p:=@t;
  p^:='test';
end;

begin
  p;
end.
