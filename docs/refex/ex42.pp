Program Example42;

{ Program to demonstrate the Move function. }

Var S1,S2 : String [30];

begin
  S1:='Hello World !';
  S2:='Bye, bye    !';
  Move (S1,S2,Sizeof(S1));
  Writeln (S2);
end.
