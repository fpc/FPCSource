Program Example50;

{ This program demonstrates the WithinPastDays function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime; ADays : Integer);

begin
 Write(DateTimeToStr(AThen),' and ',DateTimeToStr(ANow));
 Write(' are within ',ADays,' days: ');
 Writeln(WithinPastDays(ANow,AThen,ADays));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Now;
  D2:=Today-23/24;
  Test(D1,D2,1);
  D2:=Today-1;
  Test(D1,D2,1);
  D2:=Today-25/24;
  Test(D1,D2,1);
  D2:=Today-26/24;
  Test(D1,D2,5);
  D2:=Today-5.4;
  Test(D1,D2,5);
  D2:=Today-2.5;
  Test(D1,D2,1);
  Test(D1,D2,2);
  Test(D1,D2,3);
End.