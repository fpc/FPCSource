program example56;

uses Unix;

{ Program to demonstrate the Shell function }

Var S : Longint;

begin
  Writeln ('Output of ls -l *.pp');
  S:=fpSystem('ls -l *.pp');
  Writeln ('Command exited wwith status : ',S);
end.
