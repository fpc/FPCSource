{ %OPT=-gw }
program Project1;

{$mode objfpc}{$H+}
Var
  S:String[100];
  C:Char absolute S[1];
begin
  s:='asdf';
  writeln(s);
end.
