{%fail}
{%norun}
program tdotunits1;

{$mode delphi}

uses
  tudots.dot.next, tudots;

begin
  // this identifier can't be resolved because namespace udots.dot hides the udots unit visibility
  tudots.dot.test := 1;
end.

