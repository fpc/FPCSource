Program Example79;

{ This program demonstrates the TryEncodeDateTime function }

Uses SysUtils,DateUtils;

Var
  Y,Mo,D,H,Mi,S,MS : Word;
  TS : TDateTime;

Begin
  DecodeDateTime(Now,Y,Mo,D,H,Mi,S,MS);
  If TryEncodeDateTime(Y,Mo,D,H,Mi,S,MS,TS) then
    Writeln('Now is : ',DateTimeToStr(TS))
  else
    Writeln('Wrong date/time indication');
End.