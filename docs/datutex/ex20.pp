Program Example20;

{ This program demonstrates the IsToday function }

Uses SysUtils,DateUtils;

Begin
  Writeln('Today     : ',IsToday(Today));
  Writeln('Tomorrow  : ',IsToday(Tomorrow));
  Writeln('Yesterday : ',IsToday(Yesterday));
End.