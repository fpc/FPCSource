{ %fail }

program forrangecheck;

var
  i1: Word;
  i2: LongWord;
  i3: QWord;
  cnt: longint;
begin
  cnt:=0;
  for i1 :=  High(longword)-2 to High(longword) do
    inc(cnt);
end.
