Program Example17;

{ This program demonstrates the MSecsToTimeStamp function }

Uses sysutils;

Var MS : Comp;
    TS : TTimeStamp;
    DT : TDateTime;
Begin
  TS:=DateTimeToTimeStamp(Now);
  Writeln ('Now in days since 1/1/0001      : ',TS.Date);
  Writeln ('Now in millisecs since midnight : ',TS.Time);
  MS:=TimeStampToMSecs(TS);
  Writeln ('Now in millisecs since 1/1/0001 : ',MS);
  MS:=MS-1000*3600*2;
  TS:=MSecsToTimeStamp(MS);
  DT:=TimeStampToDateTime(TS);
  Writeln ('Now minus 1 day : ',DateTimeToStr(DT));
End.