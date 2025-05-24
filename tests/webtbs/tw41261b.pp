program Project1;
{$R+}
const
  INTCHARS = ['0' .. '9'];
  HEXCHARS = ['a' .. 'f', 'A' .. 'F'] + INTCHARS;
type
  digit_set = set of 0 .. 9;
  digit_char_set = set of '0' .. '9';

var
  ds : digit_set;
  dcs : digit_char_set;
begin
  ds:=[0,3,7];
  dcs:=['1'..'5'];
  dcs:= dcs + ['a'..'d'];
end.

