Program Example34;

{ Program to demonstrate the power function. }

Uses Math;

procedure dopower(x,y : float);

begin
  writeln(x:8:6,'^',y:8:6,' = ',power(x,y):8:6)
end;

begin
  dopower(2,2);
  dopower(2,-2);
  dopower(2,0.0);
end.
