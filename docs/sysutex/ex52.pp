Program Example52;

{ This program demonstrates the AnsiLastChar function }

Uses sysutils;

Var S : AnsiString;
    L : Longint;

Begin
  S:='This is an ansistring.';
  Writeln ('Last character of S is : ',AnsiLastChar(S));
  L:=Longint(AnsiLastChar(S))-Longint(@S[1])+1;
  Writeln ('Length of S is : ',L);
End.