Program Example5;

{ This program demonstrates the IsValidDate function }

Uses SysUtils,DateUtils;

Var
  Y,M,D : Word;

Begin
  For Y:=2000 to 2004 do
   For M:=1 to 12 do
     For D:=1 to 31 do
       If Not IsValidDate(Y,M,D) then
         Writeln(D,' is not a valid day in ',Y,'/',M);
End.