Program Example70;

{ This program demonstrates the FmtStr function }

Uses sysutils;

Var S : AnsiString;

Begin
  S:='';
  FmtStr (S,'For some nice examples of fomatting see %s.',['Format']);
  Writeln (S);
End.