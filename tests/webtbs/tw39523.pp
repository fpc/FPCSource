var
  w: Word;
  Code: Integer;
begin
  Val('-1',W,Code);
  writeln('Code: ',Code); //outputs: 2
  if Code<>1 then
    halt(1);
  Val('-0x1',w,Code);
  writeln('Code: ',Code); //outputs: 4
  if Code<>1 then
    halt(2);
end.
