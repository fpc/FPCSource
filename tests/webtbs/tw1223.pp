{ Source provided for Free Pascal Bug Report 1223 }
{ Submitted by "Denis Yarkovoy" on  2000-11-03 }
{ e-mail: gunky9@geocities.com }
 Type
      TPoint = record
       X, Y : integer;
      end;

 operator + (const p1, p2:TPoint) p : TPoint;
 begin
  p.X:=p1.X+p2.X;
  p.Y:=p1.Y+p2.Y;
 end;

 var d,d1:TPoint;

begin
  d.x:=5;d.y:=34;
  d1.x:=6;d1.y:=-50;
  d:=d+d1;
  if (d.x<>11) or (d.y<>-16) then
    begin
      Writeln('Error is operator overloading');
      Halt(1);
    end
  else
    Writeln('Operator overloading works correctly');
end.
