Program Example24;

{ This program demonstrates the TimeStampToDateTime function }

Uses sysutils;

Var TS : TTimeStamp;
    DT : TDateTime;

Begin
  TS:=DateTimeToTimeStamp (Now);
  With TS do
    begin
    Writeln ('Now is ',time,' millisecond past midnight');
    Writeln ('Today is ' ,Date,' days past 1/1/0001');
    end;
  DT:=TimeStampToDateTime(TS);
  Writeln ('Together this is : ',DateTimeToStr(DT));
End.