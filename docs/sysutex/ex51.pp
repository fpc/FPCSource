Program Example51;

{ This program demonstrates the AnsiQuotedStr function }

Uses sysutils;

Var S : AnsiString;

Begin
  S:='He said "Hello" and walked on';
  S:=AnsiQuotedStr(Pchar(S),'"');
  Writeln (S);
  Writeln(AnsiExtractQuotedStr(Pchar(S),'"'));
End.