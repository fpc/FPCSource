Program Example78;

{ This program demonstrates the QuotedStr function }

Uses sysutils;

Var S : AnsiString;

Begin
  S:='He said ''Hello'' and walked on';
  Writeln (S);
  Writeln ('  becomes');
  Writeln (QuotedStr(S));
End.