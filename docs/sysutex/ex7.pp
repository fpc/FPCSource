Program Example7;

{ This program demonstrates the DateToStr function }

Uses sysutils;

Begin
  Writeln(Format ('Today is: %s',[DateToStr(Date)]));
End.