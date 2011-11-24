{%norun}
program udots.prog;

{$mode delphi}

uses
  udots;

begin
  // this should not fail although we have a namespace udots and a unit udots
  udots.dot.test := 1;
end.


