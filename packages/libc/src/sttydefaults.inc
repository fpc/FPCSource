
function CTRL(x: AnsiChar): AnsiChar;
begin
  Result:=AnsiChar(Ord(x) and $1F);
end;
