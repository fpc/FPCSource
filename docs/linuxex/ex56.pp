program example56;

uses linux;

{ Program to demonstrate the Shell function }

Var S : Longint;

begin
  Writeln ('Output of ls -l *.pp');
  S:=Shell ('ls -l *.pp');
  Writeln ('Command exited wwith status : ',S);
end.
