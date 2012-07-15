{ %fail }

program test;
type
  trec = record
    i: int64;
  end;
var
   R: QWord;
begin
   R := QWord(trec.i);
end.

