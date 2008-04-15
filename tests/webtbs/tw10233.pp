var
  i: Byte;
  w: word;
  l: cardinal;
{$ifdef FPC}
  g: qword;
{$endif FPC}
begin
  i := $80;
  if Byte(ByteBool(i))<>$80 then
    halt(1);
  if Word(WordBool(i))<>$80 then
    halt(11);
  if LongInt(LongBool(i))<>$80 then
    halt(12);
  w := $8000;
  if Word(WordBool(w))<>$8000 then
    halt(2);
  l := $80000000;
  if Cardinal(LongBool(l))<>$80000000 then
    halt(3);
{$ifdef FPC}
  g := qword($8000000000000000);
  if qword(qwordBool(g))<>qword($8000000000000000) then
    halt(4);
{$endif FPC}

  if Byte(ByteBool(WordBool(w)))<>high(byte) then
    halt(5);
  if Byte(ByteBool(w))<>0 then
    halt(51);
  if Word(WordBool(LongBool(l)))<>high(word) then
    halt(6);
  if Word(WordBool(l))<>0 then
    halt(61);
{$ifdef FPC}
  if Cardinal(LongBool(qwordBool(g)))<>high(cardinal) then
    halt(7);
  if Cardinal(LongBool(g))<>0 then
    halt(7);
{$endif FPC}
  writeln('Test OK.');
end.
