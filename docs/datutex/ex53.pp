Program Example53;

{ This program demonstrates the WithinPastSeconds function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime; ASeconds : Integer);

begin
 Write(DateTimeToStr(AThen),' and ',DateTimeToStr(ANow));
 Write(' are within ',ASeconds,' seconds: ');
 Writeln(WithinPastSeconds(ANow,AThen,ASeconds));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Now;
  D2:=D1-(999*OneMilliSecond);
  Test(D1,D2,1);
  D2:=D1-(1001*OneMilliSecond);
  Test(D1,D2,1);
  D2:=D1-(2001*OneMilliSecond);
  Test(D1,D2,1);
  D2:=D1-(5001*OneMilliSecond);
  Test(D1,D2,5);
  D2:=D1-(5.4*OneSecond);
  Test(D1,D2,5);
  D2:=D1-(2.5*OneSecond);
  Test(D1,D2,1);
  Test(D1,D2,2);
  Test(D1,D2,3);
End.