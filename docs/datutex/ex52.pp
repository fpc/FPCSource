Program Example52;

{ This program demonstrates the WithinPastMinutes function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime; AMinutes : Integer);

begin
 Write(DateTimeToStr(AThen),' and ',DateTimeToStr(ANow));
 Write(' are within ',AMinutes,' Minutes: ');
 Writeln(WithinPastMinutes(ANow,AThen,AMinutes));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Now;
  D2:=D1-(59*OneSecond);
  Test(D1,D2,1);
  D2:=D1-(61*OneSecond);
  Test(D1,D2,1);
  D2:=D1-(122*OneSecond);
  Test(D1,D2,1);
  D2:=D1-(306*OneSecond);
  Test(D1,D2,5);
  D2:=D1-(5.4*OneMinute);
  Test(D1,D2,5);
  D2:=D1-(2.5*OneMinute);
  Test(D1,D2,1);
  Test(D1,D2,2);
  Test(D1,D2,3);
End.