{ %NORUN }

program tatomic9;

{$mode objfpc}

generic procedure TestInc<T>(var aArg: T);
begin
  AtomicIncrement(aArg);
end;

generic procedure TestAdd<T>(var aArg1: T; aArg2: T);
begin
  AtomicIncrement(aArg1, aArg2);
end;

generic procedure TestXchg<T>(var aArg1: T; aArg2: T);
begin
  AtomicExchange(aArg1, aArg2);
end;

generic procedure TestCmpXchg<T>(var aArg1: T; aArg2, aArg3: T);
begin
  AtomicCmpExchange(aArg1, aArg2, aArg3);
end;

generic procedure TestCmpXchg<T; U>(var aArg1: T; aArg2, aArg3: T; out aArg4: U);
begin
  AtomicCmpExchange(aArg1, aArg2, aArg3, aArg4);
end;

var
  l: LongInt;
  b: Boolean;
  b32: Boolean32;
  lb: LongBool;
begin
  l := 0;
  specialize TestInc<LongInt>(l);
  specialize TestAdd<LongInt>(l, 42);
  specialize TestXchg<LongInt>(l, 21);
  specialize TestCmpXchg<LongInt>(l, 21, 84);
  specialize TestCmpXchg<LongInt, Boolean>(l, 84, 42, b);
  specialize TestCmpXchg<LongInt, Boolean32>(l, 84, 42, b32);
  specialize TestCmpXchg<LongInt, LongBool>(l, 84, 42, lb);
end.
