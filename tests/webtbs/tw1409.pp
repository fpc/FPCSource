{$MODE objfpc}
type
  TPoint = record
    x, y: Integer;
  end;

procedure Test(const Args: array of TPoint);
begin
{$ifndef VER1_0}
  writeln(length(Args));
  if length(Args)<>2 then
   halt(1);
{$endif VER1_0}
  writeln(high(Args));
  if high(Args)<>1 then
   halt(1);
  writeln(Args[0].x,',',Args[0].y);
  if (Args[0].x<>10) or (Args[0].y<>20) then
   halt(1);
  writeln(Args[1].x,',',Args[1].y);
  if (Args[1].x<>30) or (Args[1].y<>40) then
   halt(1);
end;

const
  p1: TPoint = (x: 10; y: 20);
  p2: TPoint = (x: 30; y: 40);
begin
  Test([p1,p2]);
end.
