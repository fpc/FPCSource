{ %opt=-Co }
program test_Co_option;

{$mode objfpc}{$H+}
var i: Integer;

begin
  i := -2;
  i := i * 2;
  writeln('ok');
end.
