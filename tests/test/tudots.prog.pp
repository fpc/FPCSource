{%norun}
program tudots.prog;

{$mode delphi}

uses
  tudots;

begin
  // this should not fail although we have a namespace udots and a unit udots
  tudots.dot.test := 1;
end.


