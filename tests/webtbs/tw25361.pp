{ %norun }

type
  float64=record
    high,low: longint;
  end; 

procedure foo(x: double);
var
  a: float64 absolute x;
begin
  writeln(a.low);
end;

begin
end.

