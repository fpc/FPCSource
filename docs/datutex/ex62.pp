Program Example62;

{ This program demonstrates the MilliSecondsBetween function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime);

begin
 Write('Number of milliseconds between ');
 Write(TimeToStr(AThen),' and ',TimeToStr(ANow));
 Writeln(' : ',MilliSecondsBetween(ANow,AThen));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Now;
  D2:=D1-(0.9*OneMilliSecond);
  Test(D1,D2);
  D2:=D1-(1.0*OneMilliSecond);
  Test(D1,D2);
  D2:=D1-(1.1*OneMilliSecond);
  Test(D1,D2);
  D2:=D1-(2.5*OneMilliSecond);
  Test(D1,D2);
End.