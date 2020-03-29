{ %FAIL }

program twide9;

var
  u: UnicodeChar;
begin
  { fails, because a code point > $FFFF decodes to a surrogate pair, thus a string constant }
  u := #$10FFFF;
end.
