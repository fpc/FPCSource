Program Example61;

{ This program demonstrates the SecondsBetween function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime);

begin
 Write('Number of seconds between ');
 Write(TimeToStr(AThen),' and ',TimeToStr(ANow));
 Writeln(' : ',SecondsBetween(ANow,AThen));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Now;
  D2:=D1-(999*OneMilliSecond);
  Test(D1,D2);
  D2:=D1-(1001*OneMilliSecond);
  Test(D1,D2);
  D2:=D1-(2001*OneMilliSecond);
  Test(D1,D2);
  D2:=D1-(5001*OneMilliSecond);
  Test(D1,D2);
  D2:=D1-(5.4*OneSecond);
  Test(D1,D2);
  D2:=D1-(2.5*OneSecond);
  Test(D1,D2);
End.