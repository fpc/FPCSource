Program Example58;

{ This program demonstrates the AnsiStrLastChar function }

Uses sysutils;

Var P : Pchar;
    L : Longint;

Begin
  P:='This is an PChar string.';
  Writeln ('Last character of P is : ',AnsiStrLastChar(P));
  L:=Longint(AnsiStrLastChar(P))-Longint(P)+1;
  Writeln ('Length of P (',P,') is : ',L);
End.