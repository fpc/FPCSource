Program Example68;

{ Program to demonstrate the Str function. }
Var S : String;

Function IntToStr (I : Longint) : String;

Var S : String;

begin
 Str (I,S);
 IntToStr:=S;
end;

begin
  S:='*'+IntToStr(-233)+'*';
  Writeln (S);
end.
