type
  TPointF = record
    x,y: single;
  end;

procedure test(pt1, pt2, pt3,
  pt4: TPointF; texture: tobject; tex1, tex2, tex3, tex4: TPointF);
begin
  if pt1.x<>1.0 then
    halt(1);
  if pt1.y<>2.0 then
    halt(2);
  if pt2.x<>3.0 then
    halt(3);
  if pt2.y<>4.0 then
    halt(4);
  if pt3.x<>5.0 then
    halt(5);
  if pt3.y<>6.0 then
    halt(6);
  if pt4.x<>7.0 then
    halt(7);
  if pt4.y<>8.0 then
    halt(8);
  if texture<>nil then
    halt(9);
  if tex1.x<>9.0 then
    halt(9);
  if tex1.y<>10.0 then
    halt(10);
  if tex2.x<>11.0 then
    halt(11);
  if tex2.y<>12.0 then
    halt(12);
  if tex3.x<>13.0 then
    halt(13);
  if tex3.y<>14.0 then
    halt(14);
  if tex4.x<>15.0 then
    halt(15);
  if tex4.y<>16.0 then
    halt(16);
end;

var
  p1,p2,p3,p4,t1,t2,t3,t4: tpointf;
begin
  p1.x:=1.0;
  p1.y:=2.0;
  p2.x:=3.0;
  p2.y:=4.0;
  p3.x:=5.0;
  p3.y:=6.0;
  p4.x:=7.0;
  p4.y:=8.0;
  t1.x:=9.0;
  t1.y:=10.0;
  t2.x:=11.0;
  t2.y:=12.0;
  t3.x:=13.0;
  t3.y:=14.0;
  t4.x:=15.0;
  t4.y:=16.0;
  test(p1,p2,p3,p4,nil,t1,t2,t3,t4);
end.
