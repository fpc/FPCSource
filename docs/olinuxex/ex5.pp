Program Example5;

{ Program to demonstrate the GetTime function. }

Uses oldlinux;

Var Hour, Minute, Second : Word;

begin
  GetTime (Hour, Minute, Second);
  Writeln ('Time : ',Hour:2,':',Minute:2,':',Second:2);
end.
