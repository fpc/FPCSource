{ %opt=-vw -Sew }
{ %norun }

{$mode objfpc}
{$modeswitch advancedrecords}

type
  tpoint = record
    x,y: longint;
    function add(const aPoint:Tpoint):TPoint;
    procedure setlocation(xx,yy: longint);
  end;

procedure tpoint.setlocation(xx,yy: longint);
begin
  x:=xx;
  y:=yy;
end;

function TPoint.Add(const aPoint:Tpoint):TPoint;
 begin
   Result.SetLocation(self.x+aPoint.X, self.Y+aPoint.Y);
 end;

begin
end.
