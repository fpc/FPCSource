Program Example79;

{ This program demonstrates the RightStr function }

Uses sysutils;

Begin
  Writeln (RightStr('abcdefghijklmnopqrstuvwxyz',20));
  Writeln (RightStr('abcdefghijklmnopqrstuvwxyz',15));
  Writeln (RightStr('abcdefghijklmnopqrstuvwxyz',1));
  Writeln (RightStr('abcdefghijklmnopqrstuvwxyz',200));
End.