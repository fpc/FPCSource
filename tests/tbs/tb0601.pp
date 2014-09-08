program tb0601;

var
  i1, i2, i3: LongWord;
begin
  i1 := 42;
  i2 := 84;
  i3 := LongWord(i1 < i2);
  if i3 <> 1 then
    Halt(1);
end.
