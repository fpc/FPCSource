Program Example75;

{ This program demonstrates the IsValidIdent function }

Uses sysutils;

Procedure Testit (S : String);

begin
  Write ('"',S,'" is ');
  If not IsVAlidIdent(S) then
    Write('NOT ');
  Writeln ('a valid identifier');
end;

Begin
  Testit ('_MyObj');
  Testit ('My__Obj1');
  Testit ('My_1_Obj');
  Testit ('1MyObject');
  Testit ('My@Object');
  Testit ('M123');
End.