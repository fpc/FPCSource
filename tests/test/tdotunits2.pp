{%norun}
program tdotunits2;

{$mode delphi}

uses
  udots.dot, udots.dot.next, udots;

begin
  // this identifier should be resolved to test variable from udots.dot unit
  udots.dot.test := 'c';
end.

