{%fail}
{%norun}
program tudots.dot.prog;

{$mode delphi}

uses
  tudots;

begin
  // this must fail because we have a namespace udots.dot and it has no unit test
  tudots.dot.test := 1;
end.


