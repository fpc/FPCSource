Program Example3;

{ Program to demonstrate the EpochToLocal function. }

Uses oldlinux;

Var Year,month,day,hour,minute,seconds : Word;

begin
  EpochToLocal (GetEpochTime,Year,month,day,hour,minute,seconds);
  Writeln ('Current date : ',Day:2,'/',Month:2,'/',Year:4);
  Writeln ('Current time : ',Hour:2,':',minute:2,':',seconds:2);
end.
