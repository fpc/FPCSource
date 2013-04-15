program tdotunits2;

{$mode delphi}

uses
  udots.dot, udots.dot.next, udots;

begin
  // this identifier should be resolved to test variable from udots.dot unit
  udots.dot.test := 'a';
  if udots.dot.test <> 'a' then
    halt(1);
  udots.dot.t;
  if udots.dot.test <> 'c' then
    halt(2);
end.

