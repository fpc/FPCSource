program example56;

uses linux;

{ Program to demonstrate the Shell function }

begin
  writeln ('Output of ls -l *.pp');
  Shell ('ls -l *.pp');
end.
