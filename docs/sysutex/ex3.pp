Program Example3;

{ This program demonstrates the DateTimeToStr function }

Uses sysutils;

Begin
  Writeln ('Today is : ',DateTimeToStr(Now));
  Writeln ('Today is : ',FormatDateTime('c',Now));
End.