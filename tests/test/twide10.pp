{ %FAIL }

program twide10;

var
  s: UnicodeString;
begin
  { this is greater than the highest defined Unicode code point }
  s := #$110000;
end.
