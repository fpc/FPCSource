Program Example84;

{ This program demonstrates the TryEncodeDateDay function }

Uses SysUtils,DateUtils;

Var
  Y,DoY : Word;
  TS : TDateTime;

Begin
  DecodeDateDay(Now,Y,DoY);
  If TryEncodeDateDay(Y,DoY,TS) then
    Writeln('Today is : ',DateToStr(TS))
  else
    Writeln('Wrong year/day of year indication');
End.