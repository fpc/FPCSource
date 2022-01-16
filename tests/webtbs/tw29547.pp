program Test;

{$mode objfpc}

type
    Point2D = record
      x, y: Single;
    end;

    Line = record
        start : Point2D;
        endP : Point2D;
    end;

function LineFrom(p1, p2: Point2D): Line;
begin
    result.start := p1;
    result.endP := p2;
end;

procedure Main();
var
    l: Line;
    pt1, pt2: Point2D;
begin
    pt1.x := 1.0;
    pt2.x := 2.0;
    l := LineFrom(pt1, pt2);
    if (l.start.x<>1.0) or
       (l.endp.x<>2.0) then
      halt(1);
end;

begin
    Main();
end.

