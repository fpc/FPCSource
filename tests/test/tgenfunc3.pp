{ test syntax of a generic class function in mode objfpc }

program tgenfunc3;

{$mode objfpc}

type
  TTest = class
    generic class function Add<T>(aLeft, aRight: T): T;
  end;

generic class function TTest.Add<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

begin
  if TTest.specialize Add<LongInt>(2, 3) <> 5 then
    Halt(1);
  if TTest.specialize Add<String>('Hello', 'World') <> 'HelloWorld' then
    Halt(2);
end.
