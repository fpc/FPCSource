{ %FAIL }

program tatomic7;

var
  l: LongInt;
begin
  AtomicCmpExchange(l, 4, 4.32);
end.
