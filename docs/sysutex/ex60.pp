Program Example60;

{ This program demonstrates the AnsiStrUpper function }

Uses sysutils;

Procedure Testit (S : Pchar);

begin
 Writeln (S,' -> ',AnsiStrUpper(S))
end;

Begin
  Testit('AN UPPERCASE STRING');
  Testit('Some mixed STring');
  Testit('a lowercase string');
End.