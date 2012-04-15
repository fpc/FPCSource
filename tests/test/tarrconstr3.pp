{ %fail}
program tarrconstr3;

{$mode delphi}

type
  TAB = array of byte;
var
  D: TAB;
begin
  // CREATE expected but INIT found
  D := TAB.init(1, 1);
end.

