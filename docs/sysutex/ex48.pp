Program Example48;

{ This program demonstrates the AdjustLineBreaks function }

Uses sysutils;

Const
  S = 'This is a string'#13'with embedded'#10'linefeed and'+
       #13'CR characters';

Begin
  Writeln (AdjustLineBreaks(S));
End.