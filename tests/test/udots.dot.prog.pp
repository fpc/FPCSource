{%fail}
{%norun}
program udots.dot.prog;

{$mode delphi}

uses
  udots;

begin
  // this must fail because we have a namespace udots.dot and it has no unit test
  udots.dot.test := 1;
end.


