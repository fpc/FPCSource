Program Example80;

{ This program demonstrates the StrFmt function }

Uses sysutils;

Var S : AnsiString;

Begin
  SetLEngth(S,80);
  Writeln (StrFmt (@S[1],'For some nice examples of fomatting see %s.',['Format']));
End.