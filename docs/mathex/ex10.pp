Program Example10;

{ Program to demonstrate the cycletorad function. }

Uses math;

begin
  writeln(cos(cycletorad(1/6))); // Should print 1/2
  writeln(cos(cycletorad(1/8))); // should be sqrt(2)/2
end.
