Program Example1;

{ Program to demonstrate the arccos function. }

Uses math;

  Procedure WriteRadDeg(X : float);

  begin
    Writeln(X:8:5,' rad = ',radtodeg(x):8:5,' degrees.')
  end;

begin
  WriteRadDeg (arccos(1));
  WriteRadDeg (arccos(sqrt(3)/2));
  WriteRadDeg (arccos(sqrt(2)/2));
  WriteRadDeg (arccos(1/2));
  WriteRadDeg (arccos(0));
  WriteRadDeg (arccos(-1));
end.
