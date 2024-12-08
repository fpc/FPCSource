{ %FAIL }

program tatomic4;

type
  TEnum = (teOne, teTwo, teThree);

var
  e: TEnum = teOne;
begin
  AtomicIncrement(e);
end.
