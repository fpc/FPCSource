{$MODE objfpc}
type
  TPoint = record
    x, y: Integer;
  end;

procedure Test(const Args: array of TPoint);
begin
end;

const
  p1: TPoint = (x: 10; y: 10);
begin
  Test([p1]);
end.
