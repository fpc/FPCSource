{ test syntax of a generic method in mode delphi }

program tgenfunc6;

{$mode delphi}

type
  TTest = class
    function Add<T>(aLeft, aRight: T): T;
  end;

function TTest.Add<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

var
  t: TTest;
begin
  if t.Add<LongInt>(2, 3) <> 5 then
    Halt(1);
  if t.Add<String>('Hello', 'World') <> 'HelloWorld' then
    Halt(2);
end.
