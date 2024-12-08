{ %FAIL }

program tatomic2;

var
  f: Single = 42.0;
begin
  AtomicIncrement(f);
end.
