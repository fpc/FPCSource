Program Example79;

{ This program demonstrates the DecodeDateTime function }

Uses SysUtils,DateUtils;

Var
  Y,Mo,D,H,Mi,S,MS : Word;
  TS : TDateTime;

Begin
  DecodeDateTime(Now,Y,Mo,D,H,Mi,S,MS);
  TS:=EncodeDateTime(Y,Mo,D,H,Mi,S,MS);
  Writeln('Now is : ',DateTimeToStr(TS));
End.