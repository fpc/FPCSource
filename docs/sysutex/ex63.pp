Program Example63;

{ This program demonstrates the AssignStr function }
{$H+}

Uses sysutils;

Var P : PString;

Begin
 P:=NewStr('A first AnsiString');
 Writeln ('Before: P = "',P^,'"');
 AssignStr(P,'A Second ansistring');
 Writeln ('After : P = "',P^,'"');
 DisposeStr(P);
End.