Program Example86;

{ This program demonstrates the TryEncodeDateMonthWeek function }

Uses SysUtils,DateUtils;

Var
  Y,M,Wom,Dow : Word;
  TS : TDateTime;

Begin
  DecodeDateMonthWeek(Now,Y,M,WoM,DoW);
  If TryEncodeDateMonthWeek(Y,M,WoM,Dow,TS) then
    Writeln('Today is : ',DateToStr(TS))
  else
    Writeln('Invalid year/month/week/dow indication');
End.