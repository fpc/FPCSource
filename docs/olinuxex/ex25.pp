Program Example25;

{ Program to demonstrate the UTime function. }

Uses oldlinux;

Var utim : utimbuf;
    year,month,day,hour,minute,second : Word;

begin
  { Set access and modification time of executable source }
  GetTime (hour,minute,second);
  GetDate (year,month,day);
  utim.actime:=LocalToEpoch(year,month,day,hour,minute,second);
  utim.modtime:=utim.actime;
  if not Utime('ex25.pp',utim) then
    writeln ('Call to UTime failed !')
  else
    begin
    Write ('Set access and modification times to : ');
    Write (Hour:2,':',minute:2,':',second,', ');
    Writeln (Day:2,'/',month:2,'/',year:4);
    end;
end.
