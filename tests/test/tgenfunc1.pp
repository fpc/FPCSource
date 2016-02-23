{ test syntax of a global generic function in mode objfpc }

program tgenfunc1;

{$mode objfpc}

generic function Add<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

begin
  if specialize Add<LongInt>(2, 3) <> 5 then
    Halt(1);
  if specialize Add<String>('Hello', 'World') <> 'HelloWorld' then
    Halt(2);
end.
