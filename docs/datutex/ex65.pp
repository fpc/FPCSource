Program Example57;

{ This program demonstrates the WeekSpan function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime);

begin
 Write('Number of weeks between ');
 Write(DateToStr(AThen),' and ',DateToStr(ANow));
 Writeln(' : ',WeekSpan(ANow,AThen));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Today;
  D2:=Today-7;
  Test(D1,D2);
  D2:=Today-8;
  Test(D1,D2);
  D2:=Today-14;
  Test(D1,D2);
  D2:=Today-35;
  Test(D1,D2);
  D2:=Today-36;
  Test(D1,D2);
  D2:=Today-17;
  Test(D1,D2);
End.