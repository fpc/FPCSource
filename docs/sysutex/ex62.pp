Program Example62;

{ This program demonstrates the AppendStr function }

Uses sysutils;

Var S : AnsiString;

Begin
  S:='This is an ';
  AppendStr(S,'AnsiString');
  Writeln ('S = "',S,'"');
End.