Program Example54;

{ This program demonstrates the WithinPastMilliSeconds function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime; AMilliSeconds : Integer);

begin
 Write(TimeToStr(AThen),' and ',TimeToStr(ANow));
 Write(' are within ',AMilliSeconds,' milliseconds: ');
 Writeln(WithinPastMilliSeconds(ANow,AThen,AMilliSeconds));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Now;
  D2:=D1-(0.9*OneMilliSecond);
  Test(D1,D2,1);
  D2:=D1-(1.0*OneMilliSecond);
  Test(D1,D2,1);
  D2:=D1-(1.1*OneMilliSecond);
  Test(D1,D2,1);
  D2:=D1-(2.5*OneMilliSecond);
  Test(D1,D2,1);
  Test(D1,D2,2);
  Test(D1,D2,3);
End.