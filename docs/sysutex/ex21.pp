Program Example21;

{ This program demonstrates the StrToTime function }

Uses sysutils;

Procedure TestStr (S : String);

begin
  Writeln (S,' : ',TimeToStr(StrToTime(S)));
end;

Begin
  teststr (TimeToStr(Time));
  teststr ('12:00');
  teststr ('15:30');
  teststr ('3:30PM');
End.