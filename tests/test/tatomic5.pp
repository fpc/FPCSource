{ %FAIL }

program tatomic5;

var
  i: LongInt = 42;
  x: LongInt;
begin
  AtomicCmpExchange(i, 42, 42, x);
end.

