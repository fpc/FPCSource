Program Example59;

{ This program demonstrates the HoursBetween function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime);

begin
 Write('Number of hours between ');
 Write(DateTimeToStr(AThen),' and ',DateTimeToStr(ANow));
 Writeln(' : ',HoursBetween(ANow,AThen));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Now;
  D2:=D1-(59*OneMinute);
  Test(D1,D2);
  D2:=D1-(61*OneMinute);
  Test(D1,D2);
  D2:=D1-(122*OneMinute);
  Test(D1,D2);
  D2:=D1-(306*OneMinute);
  Test(D1,D2);
  D2:=D1-(5.4*OneHour);
  Test(D1,D2);
  D2:=D1-(2.5*OneHour);
  Test(D1,D2);
End.