Program Example6;

{ Program to demonstrate the GetDateTime function. }

Uses oldlinux;

Var Year, Month, Day, Hour, min, sec : Word;

begin
 GetDateTime (Year, Month, Day, Hour, min, sec);
 Writeln ('Date : ',Day:2,'/',Month:2,'/',Year:4);
 Writeln ('Time : ',Hour:2,':',Min:2,':',Sec:2);
end.
