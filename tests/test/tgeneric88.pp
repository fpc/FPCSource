{ %FAIL }

program tgeneric88;

{$mode objfpc}

type
  generic TTest<T> = record

  end;

  PTestLongInt = ^specialize TTest<LongInt>;
  PTestBoolean = ^specialize TTest<Boolean>;

begin

end.
