Program Example6;

{ Program to demonstrate the arctan2 function. }

Uses math;

  Procedure WriteRadDeg(X : float);

  begin
    Writeln(X:8:5,' rad = ',radtodeg(x):8:5,' degrees.')
  end;

begin
  WriteRadDeg (arctan2(1,1));
end.
