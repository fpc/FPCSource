Program Example64;

{ This program demonstrates the MonthSpan function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime);

begin
 Write('Number of months between ');
 Write(DateToStr(AThen),' and ',DateToStr(ANow));
 Writeln(' : ',MonthSpan(ANow,AThen));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Today;
  D2:=Today-364;
  Test(D1,D2);
  D2:=Today-365;
  Test(D1,D2);
  D2:=Today-366;
  Test(D1,D2);
  D2:=Today-390;
  Test(D1,D2);
  D2:=Today-368;
  Test(D1,D2);
  D2:=Today-1000;
  Test(D1,D2);
End.