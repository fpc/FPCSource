Program Example32;

{ Program to demonstrate the momentskewkurtosis function. }

Uses math;

Var
  DistArray : Array[1..1000] of float;
  I : longint;
  m1,m2,m3,m4,skew,kurtosis : float;

begin
  randomize;
  for I:=1 to 1000 do
    distarray[i]:=random;
  momentskewkurtosis(DistArray,m1,m2,m3,m4,skew,kurtosis);

  Writeln ('1st moment : ',m1:8:6);
  Writeln ('2nd moment : ',m2:8:6);
  Writeln ('3rd moment : ',m3:8:6);
  Writeln ('4th moment : ',m4:8:6);
  Writeln ('Skew       : ',skew:8:6);
  Writeln ('kurtosis   : ',kurtosis:8:6);
end.
