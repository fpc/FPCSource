{%FAIL}
program Project1;
{$R+}
const
  INTCHARS = ['0' .. '9'];
  HEXCHARS = ['a' .. 'f', 'A' .. 'F'] + INTCHARS;
type
  digit_set = set of 0 .. 9;
  digit_char_set = set of '0' .. '9';
 char_set = set of char;

var
  ds : digit_set;
  dcs : digit_char_set;
  cs : char_set;
begin
  ds:=[0,3,7];
  cs:=['1'..'5'];
  cs:=['8'..'h'];
  cs:=['c'..'z'];
  dcs:=['1'..'5'];
  dcs:=['8'..'h'];
  dcs:=['c'..'z'];
end.

