Program Example51;

{ This program demonstrates the WithinPastHours function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime; AHours : Integer);

begin
 Write(DateTimeToStr(AThen),' and ',DateTimeToStr(ANow));
 Write(' are within ',AHours,' hours: ');
 Writeln(WithinPastHours(ANow,AThen,AHours));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Now;
  D2:=D1-(59*OneMinute);
  Test(D1,D2,1);
  D2:=D1-(61*OneMinute);
  Test(D1,D2,1);
  D2:=D1-(122*OneMinute);
  Test(D1,D2,1);
  D2:=D1-(306*OneMinute);
  Test(D1,D2,5);
  D2:=D1-(5.4*OneHour);
  Test(D1,D2,5);
  D2:=D1-(2.5*OneHour);
  Test(D1,D2,1);
  Test(D1,D2,2);
  Test(D1,D2,3);
End.