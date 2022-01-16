{ correct match functions with array parameters of the generic type }

unit tgenfunc15;

{$mode objfpc}{$H+}

interface

type
  generic TStaticArray<T> = array[0..4] of T;

  TTest = class
    generic procedure Test<T>(aArg1: T; aArg2: array of T; aArg3: specialize TStaticArray<T>);
    generic procedure Test<T>(aArg1: T; aArg2: array of T; aArg3: specialize TStaticArray<T>; aArg4: LongInt);
  end;

generic procedure Test<T>(aArg1: T; aArg2: array of T; aArg3: specialize TStaticArray<T>);
generic procedure Test<T>(aArg1: T; aArg2: array of T; aArg3: specialize TStaticArray<T>; aArg4: LongInt);

implementation

generic procedure Test<T>(aArg1: T; aArg2: array of T; aArg3: specialize TStaticArray<T>);
begin
end;

generic procedure Test<T>(aArg1: T; aArg2: array of T; aArg3: specialize TStaticArray<T>; aArg4: LongInt);
begin
end;

generic procedure TTest.Test<T>(aArg1: T; aArg2: array of T; aArg3: specialize TStaticArray<T>);
begin
end;

generic procedure TTest.Test<T>(aArg1: T; aArg2: array of T; aArg3: specialize TStaticArray<T>; aArg4: LongInt);
begin
end;

var
  t: array[0..4] of LongInt = (0, 1, 2, 3, 4);
  s: array[0..4] of String = ('abc', 'def', 'ghi', 'jkl', 'mno');
initialization
  specialize Test<LongInt>(42, [32, 43], t);
  specialize Test<String>('FPC', ['Hello', 'World'], s, 42);
end.

