{ %FAIL}

// test that wrong type using for the for-in loop fails

program tforin11;

{$mode objfpc}{$H+}
{$apptype console}

var
  s: String;
  b: byte;
begin
  for b in s do
    write(b);
end.

