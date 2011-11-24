{ %fail}
program tarrconstr2;

{$mode delphi}

type
  TAB = array of byte;
var
  D: TAB;
begin
  D := TAB.create(1+2, 'c');
end.

