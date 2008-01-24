var
  i: Byte;
  w: word;
  l: cardinal;
  g: qword;
begin
  i := 128;
  if Byte(ByteBool(i))<>128 then
    halt(1);
  w := 32768;
  if Word(WordBool(w))<>32768 then
    halt(2);
  l := $80000000;
  if Cardinal(LongBool(l))<>$80000000 then
    halt(3);
  g := qword($8000000000000000);
  if qword(qwordBool(g))<>qword($8000000000000000) then
    halt(4);

  if Byte(ByteBool(w))<>high(byte) then
    halt(5);
  if Word(WordBool(l))<>high(word) then
    halt(6);
  l := $80000000;
  if Cardinal(LongBool(g))<>high(cardinal) then
    halt(7);
  g := qword($8000000000000000);
  if qword(qwordBool(i))<>high(qword) then
    halt(8);
end.
