{ %FAIL }

program twide11;

var
  s: UnicodeString;
begin
  { this is greater than the highest defined Unicode code point }
  s := #1114112;
end.
