program forrangecheck;

var
  i1: Word;
  i2: LongWord;
  i3: QWord;
  cnt: longint;
begin
  cnt:=0;
  for i1 :=  High(i1)-2 to High(i1) do
    inc(cnt);
  if cnt<>3 then
    halt(1);

  cnt:=0;
  for i2 := high(i2)-2 to High(i2) do
    inc(cnt);
  if cnt<>3 then
    halt(2);

{$ifdef cpu64}
  cnt:=0;
  for i3 := high(i3)-2 to High(i3) do
    inc(cnt);
  if cnt<>3 then
    halt(3);
{$endif}

  cnt:=0;
  for i1 := high(word)-2 to High(Word) do
    inc(cnt);
  if cnt<>3 then
    halt(4);

  cnt:=0;
  for i2 := high(longword)-2 to High(LongWord) do
    inc(cnt);
  if cnt<>3 then
    halt(5);

{$ifdef cpu64}
  cnt:=0;
  for i3 := high(qword)-2 to High(QWord) do
    inc(cnt);
  if cnt<>3 then
    halt(6);
{$endif}

  cnt:=0;
  for i1 := word($ffff)-2 to Word($FFFF) do
    inc(cnt);
  if cnt<>3 then
    halt(7);

  cnt:=0;
  for i2 := longword($ffffffff)-2 to LongWord($FFFFFFFF) do
    inc(cnt);
  if cnt<>3 then
    halt(8);

{$ifdef cpu64}
  cnt:=0;
  for i3 := QWord($FFFFFFFFFFFFFFFF)-2 to QWord($FFFFFFFFFFFFFFFF) do
    inc(cnt);
  if cnt<>3 then
    halt(9);
{$endif}
end.
