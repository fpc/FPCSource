{%FAIL}
program toperator7;
{$mode objfpc}

uses
  Variants;
var
  AValue: Variant;
begin
  // this construction must fail
  AValue = 1;
end.