Program Example76;

{ This program demonstrates the LeftStr function }

Uses sysutils;

Begin
  Writeln (LeftStr('abcdefghijklmnopqrstuvwxyz',20));
  Writeln (LeftStr('abcdefghijklmnopqrstuvwxyz',15));
  Writeln (LeftStr('abcdefghijklmnopqrstuvwxyz',1));
  Writeln (LeftStr('abcdefghijklmnopqrstuvwxyz',200));
End.