Program Example58;

{ This program demonstrates the DaysBetween function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime);

begin
 Write('Number of days between ');
 Write(DateTimeToStr(AThen),' and ',DateTimeToStr(ANow));
 Writeln(' : ',DaysBetween(ANow,AThen));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Now;
  D2:=Today-23/24;
  Test(D1,D2);
  D2:=Today-1;
  Test(D1,D2);
  D2:=Today-25/24;
  Test(D1,D2);
  D2:=Today-26/24;
  Test(D1,D2);
  D2:=Today-5.4;
  Test(D1,D2);
  D2:=Today-2.5;
  Test(D1,D2);
End.