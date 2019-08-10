{ %FAIL }

program twide12;

var
  s: UnicodeString;
begin
  { this is greater than the highest defined Unicode code point }
  s := #&4200000;
end.
