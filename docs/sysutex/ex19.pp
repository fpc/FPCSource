Program Example19;

{ This program demonstrates the StrToDate function }

Uses sysutils;

Procedure TestStr (S : String);

begin
  Writeln (S,' : ',DateToStr(StrToDate(S)));
end;

Begin

  Writeln ('ShortDateFormat ',ShortDateFormat);
  TestStr(DateTimeToStr(Date));
  TestStr('05/05/1999');
  TestStr('5/5');
  TestStr('5');
End.