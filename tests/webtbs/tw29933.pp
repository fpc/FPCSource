{$mode objfpc}

type
  TPoint =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
    X : Longint;
    Y : Longint;
  end;


function Point(x,y : Integer) : TPoint; inline;
begin
  Point.x:=x;
  Point.y:=y;
end;

procedure test(p: tpoint);
begin
  if (p.x<>6) or
     (p.y<>4) then
    halt(1)
end;

var
  pt: tpoint;
  indent, secondy: longint;
begin
  indent:=5;
  secondy:=2;
  test(Point(Indent+1,secondy+2));
end.
