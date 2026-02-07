{ %opt=-Sew -Cr }
{$MODE OBJFPC}
type
   TTest = 10..20;
var
   A: TTest;
begin
   A := 10;
   Inc(A);
   Writeln(Succ(A)); // 12
   Writeln(Pred(A)); // 10
   Dec(A);
end.
