Program Example48;

{ This program demonstrates the WithinPastMonths function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime; AMonths : Integer);

begin
 Write(DateToStr(AThen),' and ',DateToStr(ANow));
 Write(' are within ',AMonths,' months: ');
 Writeln(WithinPastMonths(ANow,AThen,AMonths));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Today;
  D2:=Today-364;
  Test(D1,D2,12);
  D2:=Today-365;
  Test(D1,D2,12);
  D2:=Today-366;
  Test(D1,D2,12);
  D2:=Today-390;
  Test(D1,D2,12);
  D2:=Today-368;
  Test(D1,D2,11);
  D2:=Today-1000;
  Test(D1,D2,31);
  Test(D1,D2,32);
  Test(D1,D2,33);
End.