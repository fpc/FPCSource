{$mode macpas}
{ Mac Pascal uses commas as separators between record fields in typed constants }
program ttyperecord4;
type RGB = record red, green, blue:	integer end;
const kRGBBlack: RGB = ( red: 0, green: 0, blue: 0);
begin
end.
