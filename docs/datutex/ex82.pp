Program Example82;

{ This program demonstrates the TryEncodeDateWeek function }

Uses SysUtils,DateUtils;

Var
  Y,W,Dow : Word;
  TS : TDateTime;

Begin
  DecodeDateWeek(Now,Y,W,Dow);
  If TryEncodeDateWeek(Y,W,TS,Dow) then
    Writeln('Today is : ',DateToStr(TS))
  else
    Writeln('Invalid date/week indication');
End.