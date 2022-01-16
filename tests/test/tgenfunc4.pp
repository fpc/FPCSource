{ test syntax of a generic class function in mode delphi }

program tgenfunc4;

{$mode delphi}

type
  TTest = class
    class function Add<T>(aLeft, aRight: T): T;
  end;

class function TTest.Add<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

begin
  if TTest.Add<LongInt>(2, 3) <> 5 then
    Halt(1);
  if TTest.Add<String>('Hello', 'World') <> 'HelloWorld' then
    Halt(2);
end.
