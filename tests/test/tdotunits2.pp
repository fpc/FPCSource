program tdotunits2;

{$mode delphi}

uses
  tudots.dot, tudots.dot.next, tudots;

begin
  // this identifier should be resolved to test variable from udots.dot unit
  tudots.dot.test := 'a';
  if tudots.dot.test <> 'a' then
    halt(1);
  tudots.dot.t;
  if tudots.dot.test <> 'c' then
    halt(2);
end.

