Program Example81;

{ This program demonstrates the DecodeDateWeek function }

Uses SysUtils,DateUtils;

Var
  Y,W,Dow : Word;
  TS : TDateTime;

Begin
  DecodeDateWeek(Now,Y,W,Dow);
  TS:=EncodeDateWeek(Y,W,Dow);
  Writeln('Today is : ',DateToStr(TS));
End.