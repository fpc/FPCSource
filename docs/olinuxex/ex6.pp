Program Example6;

{ Program to demonstrate the GetDate function. }

Uses oldlinux;

Var Year, Month, Day : Word;

begin
 GetDate (Year, Month, Day);
 Writeln ('Date : ',Day:2,'/',Month:2,'/',Year:4);
end.
