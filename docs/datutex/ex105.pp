Program Example105;

{ This program demonstrates the DecodeDayOfWeekInMonth function }

Uses SysUtils,DateUtils;

Var
  Y,M,NDoW,DoW : Word;
  D : TDateTime;
Begin
  DecodeDayOfWeekInMonth(Date,Y,M,NDoW,DoW);
  D:=EncodeDayOfWeekInMonth(Y,M,NDoW,DoW);
  Write(DateToStr(D),' is the ',NDow,'-th ');
  Writeln(formatdateTime('dddd',D),' of the month.');
End.