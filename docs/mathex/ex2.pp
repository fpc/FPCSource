Program Example1;

{ Program to demonstrate the arcsin function. }

Uses math;

  Procedure WriteRadDeg(X : float);

  begin
    Writeln(X:8:5,' rad = ',radtodeg(x):8:5,' degrees.')
  end;

begin
  WriteRadDeg (arcsin(1));
  WriteRadDeg (arcsin(sqrt(3)/2));
  WriteRadDeg (arcsin(sqrt(2)/2));
  WriteRadDeg (arcsin(1/2));
  WriteRadDeg (arcsin(0));
  WriteRadDeg (arcsin(-1));
end.
