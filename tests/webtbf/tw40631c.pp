{ %fail }
program test;
{$mode objfpc} //$mode does not matter
{$H+}

var
  Ch: Char;
begin
  if Ch in [$FF, 'A'..'Z'] then;
end.
