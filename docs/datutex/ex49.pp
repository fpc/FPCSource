Program Example49;

{ This program demonstrates the WithinPastWeeks function }

Uses SysUtils,DateUtils;

Procedure Test(ANow,AThen : TDateTime; AWeeks : Integer);

begin
 Write(DateToStr(AThen),' and ',DateToStr(ANow));
 Write(' are within ',AWeeks,' weeks: ');
 Writeln(WithinPastWeeks(ANow,AThen,AWeeks));
end;

Var
  D1,D2 : TDateTime;

Begin
  D1:=Today;
  D2:=Today-7;
  Test(D1,D2,1);
  D2:=Today-8;
  Test(D1,D2,1);
  D2:=Today-14;
  Test(D1,D2,1);
  D2:=Today-35;
  Test(D1,D2,5);
  D2:=Today-36;
  Test(D1,D2,5);
  D2:=Today-17;
  Test(D1,D2,1);
  Test(D1,D2,2);
  Test(D1,D2,3);
End.