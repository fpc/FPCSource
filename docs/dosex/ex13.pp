Program Example13;
uses Dos;

{ Program to demonstrate the EnvCount and EnvStr function. }

var
  i : Longint;
begin
  WriteLn('Current Environment is:');
  for i:=1to EnvCount do
   WriteLn(EnvStr(i));
end.
