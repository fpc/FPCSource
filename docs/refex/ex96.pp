Program Example96;

{ Program to demonstrate the Assigned function. }

Var P : Pointer;

begin
  If Not Assigned(P) then
    Writeln ('Pointer is initially NIL');
  P:=@P;
  If Not Assigned(P) then
    Writeln('Internal inconsistency')
  else
    Writeln('All is well in FPC')
end.
