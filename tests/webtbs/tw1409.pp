{$MODE objfpc}
type
  TPoint = record
    x, y: Integer;
  end;

procedure Test(const Args: array of TPoint);
begin
  writeln(Args[0].x,',',Args[0].y);
  if (Args[0].x<>10) and (Args[0].y<>10) then
   halt(1);
end;

const
  p1: TPoint = (x: 10; y: 10);
begin
  Test([p1]);
end.
