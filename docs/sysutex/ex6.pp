Program Example6;

{ This program demonstrates the DateTimeToTimeStamp function }

Uses sysutils;

Var TS : TTimeStamp;

Begin
  TS:=DateTimeToTimeStamp (Now);
  With TS do
    begin
    Writeln ('Now is ',time,' millisecond past midnight');
    Writeln ('Today is ' ,Date,' days past 1/1/0001');
    end;
End.