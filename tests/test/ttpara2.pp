{ Alignment test for stack passed parameters }
procedure p1(b1,b2,b3,b4,b5,b6,b7,b8:byte);
begin
  if b1<>1 then
    halt(1);
  if b2<>2 then
    halt(2);
  if b3<>3 then
    halt(1);
  if b4<>4 then
    halt(1);
  if b5<>5 then
    halt(1);
  if b6<>6 then
    halt(1);
  if b7<>7 then
    halt(1);
  if b8<>8 then
    halt(1);
end;

begin
  p1(1,2,3,4,5,6,7,8);
end.
