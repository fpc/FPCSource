{$mode objfpc}
{$h+}
uses
  TypInfo;
type
  Enum = (a, b, c);
var
  S : string;
begin
  S := 'Text';
  S := S + 'xyz' + GetEnumName(TypeInfo(Enum), 1); // This line generate error
  S := 'xyz' + GetEnumName(TypeInfo(Enum), 1); // This line is OK!
end.

