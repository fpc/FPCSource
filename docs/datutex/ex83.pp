Program Example83;

{ This program demonstrates the DecodeDateDay function }

Uses SysUtils,DateUtils;

Var
  Y,DoY : Word;
  TS : TDateTime;

Begin
  DecodeDateDay(Now,Y,DoY);
  TS:=EncodeDateDay(Y,DoY);
  Writeln('Today is : ',DateToStr(TS));
End.