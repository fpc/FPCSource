{ %fail }
program test;
{$mode objfpc} //$mode does not matter
{$H+}

const
  Chars1: set of char = [255, 254, 253, #0, #1]; 
begin
end.
