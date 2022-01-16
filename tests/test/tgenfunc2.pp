{ test syntax of a global generic function in mode delphi }

program tgenfunc2;

{$mode delphi}

function Add<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

begin
  if Add<LongInt>(2, 3) <> 5 then
    Halt(1);
  if Add<String>('Hello', 'World') <> 'HelloWorld' then
    Halt(2);
end.
