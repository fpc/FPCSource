{ %FAIL }

program tatomic6;

var
  l: LongInt;
begin
  AtomicIncrement(l, 4.32);
end.
