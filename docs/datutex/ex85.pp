Program Example85;

{ This program demonstrates the DecodeDateMonthWeek function }

Uses SysUtils,DateUtils;

Var
  Y,M,Wom,Dow : Word;
  TS : TDateTime;

Begin
  DecodeDateMonthWeek(Now,Y,M,WoM,DoW);
  TS:=EncodeDateMonthWeek(Y,M,WoM,Dow);
  Writeln('Today is : ',DateToStr(TS));
End.