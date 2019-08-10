{ %FAIL }

program twide13;

var
  s: UnicodeString;
begin
  { this is greater than the highest defined Unicode code point }
  s := #%100010000000000000000;
end.
