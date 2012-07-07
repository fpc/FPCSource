{ %FAIL }

program tgeneric87;

{$mode objfpc}

type
  generic TTest<T> = record

  end;

const
  TestLongIntNil: ^specialize TTest<LongInt> = Nil;
  TestBooleanNil: ^specialize TTest<Boolean> = Nil;

begin

end.
