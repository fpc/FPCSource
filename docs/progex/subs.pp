{
  Example library
}
library subs;

function SubStr(CString: PChar;FromPos,ToPos: Longint): PChar;
   cdecl; export;

var
  Length: Integer;

begin
  Length := StrLen(CString);
  SubStr := CString + Length;
  if (FromPos > 0) and (ToPos >= FromPos) then
  begin
    if Length >= FromPos then
      SubStr := CString + FromPos - 1;
    if Length > ToPos then
    CString[ToPos] := #0;
  end;
end;

exports
  SubStr;

end.
