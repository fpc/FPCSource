{ %fail }

uses Strings;

Function PosEx(const SubStr, S: string; Offset: Cardinal): Integer;

var i : pchar;
begin
  if (offset<1) or (offset>length(s)) then exit(0);
  i:=strpos(@s[1],@substr[offset]);
  if i=nil then
    PosEx:=0
  else
  // This should be forbidden
    PosEx:=(i-s)+offset;
end;

begin
end.
