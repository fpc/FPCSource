program tgeneric97;

{$mode objfpc}

type
  generic TTest<T> = class

  end;

  TTestLongInt = specialize TTest<LongInt>;
  TTestString = specialize TTest<AnsiString>;

begin
  if LowerCase(TTestLongInt.ClassName) <> 'ttest<system.longint>' then
    halt(1);
  if LowerCase(TTestString.ClassName) <> 'ttest<system.ansistring>' then
    halt(2);
end.
