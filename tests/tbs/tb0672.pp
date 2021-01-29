program tb0672;

{$mode objfpc}

type
  generic TTest<T> = class
    class function Test(aArg: T): LongInt;
  end;

class function TTest.Test(aArg: T): LongInt;
begin
  Result := Ord(aArg);
end;

type
  TEnum = (teOne, teTwo, teThree);

  TTestBoolean = specialize TTest<Boolean>;
  TTestChar = specialize TTest<Char>;
  TTestWideChar = specialize TTest<WideChar>;
  TTestEnum = specialize TTest<TEnum>;

begin
  if TTestBoolean.Test(True) <> Ord(True) then
    Halt(1);
  if TTestChar.Test(#42) <> Ord(#42) then
    Halt(2);
  if TTestWideChar.Test(#1234) <> Ord(#1234) then
    Halt(3);
  if TTestEnum.Test(teTwo) <> Ord(teTwo) then
    Halt(4);
  Writeln('ok');
end.
