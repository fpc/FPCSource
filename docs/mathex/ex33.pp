Program Example33;

{ Program to demonstrate the norm function. }

Uses math;

Type
  TVector = Array[1..10] of Float;

Var
  AVector : Tvector;
  I : longint;

begin
 for I:=1 to 10 do
   Avector[i]:=Random;
 Writeln(Norm(AVector));
end.
