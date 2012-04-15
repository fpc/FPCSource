{%fail}
{%norun}
program tdotunits1;

{$mode delphi}

uses
  udots.dot.next, udots;

begin
  // this identifier can't be resolved because namespace udots.dot hides the udots unit visibility
  udots.dot.test := 1;
end.

