Program Example16;

{ This program demonstrates the IsLeapYear function }

Uses sysutils;

Var YY,MM,dd : Word;

Procedure TestYear (Y : Word);

begin
  Writeln (Y,' is leap year : ',IsLeapYear(Y));
end;

Begin
  DeCodeDate(Date,YY,mm,dd);
  TestYear(yy);
  TestYear(2000);
  TestYear(1900);
  TestYear(1600);
  TestYear(1992);
  TestYear(1995);
End.