program twide8;

procedure Check(const aStr: UnicodeString; aIndex: LongInt);
const
  Char1 = #$DBFF;
  Char2 = #$DFFF;
begin
  if Length(aStr) <> 2 then
    Halt(aIndex * 3);
  if aStr[1] <> Char1 then
    Halt(aIndex * 3 + 1);
  if aStr[2] <> Char2 then
    Halt(aIndex * 3 + 2);
end;

var
  s: UnicodeString;
begin
  s := #$10FFFF;
  Check(s, 1);
  s := #1114111;
  Check(s, 2);
  s := #&4177777;
  Check(s, 3);
  s := #%100001111111111111111;
  Check(s, 4);
  Writeln('ok');
end.
