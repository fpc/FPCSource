{ %OPT=-Ooloopunroll -O2 -S2 -Cr }
const
  StrArray :array[0..1] of string = ('s1','s2');

function FindStr(const s :string) :Integer;
var
  i :Integer;
begin
  for i := High(StrArray) downto 0 do
    if StrArray[i] = s then Exit(i);
  Result := -1;
end;

begin
  FindStr('s1');
end.
