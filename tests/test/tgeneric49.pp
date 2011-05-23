{ %NORUN }

{ This tests whether the correct deprecated messages are printed. As I don't
  know of a way to check these inside a test this needs to be done by hand }
program tgeneric49;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = class

  end deprecated 'Message A';

  TTest = class

  end deprecated 'Message B';

  // this should print 'Message A'
  TTestInteger = TTest<Integer>;

  FooInt = Integer deprecated;

  // this should print that TTest<T> and FooInt are deprecated
  TTestFooInt = TTest<FooInt>;

var
  // this should print 'Message B'
  t: TTest;
  // this should print nothing
  t2: TTestInteger;
  // this should print that TTest<T> and FooInt are deprecated
  t3: TTest<FooInt>;
begin
  // this should print that TTest<T> and FooInt are deprecated
  t3 := TTest<FooInt>.Create;
end.
