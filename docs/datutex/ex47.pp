Program Example47;

{ This program demonstrates the WithinPastYears function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime; AYears : Integer);

begin
 Write(DateToStr(AThen),' and ',DateToStr(ANow));
 Write(' are within ',AYears,' years: ');
 Writeln(WithinPastYears(ANow,AThen,AYears));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Today;
  D2:=Today-364;
  Test(D1,D2,1);
  D2:=Today-365;
  Test(D1,D2,1);
  D2:=Today-366;
  Test(D1,D2,1);
  D2:=Today-390;
  Test(D1,D2,1);
  D2:=Today-368;
  Test(D1,D2,1);
  D2:=Today-1000;
  Test(D1,D2,1);
  Test(D1,D2,2);
  Test(D1,D2,3);
End.