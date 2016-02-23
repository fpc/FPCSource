{ test syntax of a generic method in mode objfpc }

program tgenfunc5;

{$mode objfpc}

type
  TTest = class
    generic function Add<T>(aLeft, aRight: T): T;
  end;

generic function TTest.Add<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

var
  t: TTest;
begin
  if t.specialize Add<LongInt>(2, 3) <> 5 then
    Halt(1);
  if t.specialize Add<String>('Hello', 'World') <> 'HelloWorld' then
    Halt(2);
end.
