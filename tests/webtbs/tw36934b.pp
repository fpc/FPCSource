type
  TPointF = record
    x,y,z,v,u: single;
  end;

procedure test(pt1, pt2, pt3: TPointF);
begin
  if pt1.x<>1.0 then
    halt(1);
  if pt1.y<>2.0 then
    halt(2);
  if pt1.z<>3.0 then
    halt(3);
  if pt1.u<>4.0 then
    halt(4);
  if pt1.v<>5.0 then
    halt(5);
  if pt2.x<>6.0 then
    halt(6);
  if pt2.y<>7.0 then
    halt(7);
  if pt2.z<>8.0 then
    halt(8);
  if pt2.u<>9.0 then
    halt(9);
  if pt2.v<>10.0 then
    halt(10);
  if pt3.x<>11.0 then
    halt(11);
  if pt3.y<>12.0 then
    halt(12);
  if pt3.z<>13.0 then
    halt(13);
  if pt3.u<>14.0 then
    halt(14);
  if pt3.v<>15.0 then
    halt(15);
end;

var
  p1,p2,p3,p4,t1,t2,t3,t4: tpointf;
begin
  p1.x:=1.0;
  p1.y:=2.0;
  p1.z:=3.0;
  p1.u:=4.0;
  p1.v:=5.0;
  p2.x:=6.0;
  p2.y:=7.0;
  p2.z:=8.0;
  p2.u:=9.0;
  p2.v:=10.0;
  p3.x:=11.0;
  p3.y:=12.0;
  p3.z:=13.0;
  p3.u:=14.0;
  p3.v:=15.0;
  test(p1,p2,p3);
end.
