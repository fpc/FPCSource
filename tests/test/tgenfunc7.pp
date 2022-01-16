{ generics in another unit work as well }

program tgenfunc7;

{$mode objfpc}

uses
  ugenfunc7;

var
  t: TTest;
begin
  if specialize Add<LongInt>(3, 4) <> 7 then
    Halt(1);
  if specialize Add<String>('Hello', 'World') <> 'HelloWorld' then
    Halt(2);
  if TTest.specialize AddClass<LongInt>(3, 4) <> 7 then
    Halt(3);
  if TTest.specialize AddClass<String>('Hello', 'World') <> 'HelloWorld' then
    Halt(4);
  if t.specialize Add<LongInt>(3, 4) <> 7 then
    Halt(5);
  if t.specialize Add<String>('Hello', 'World') <> 'HelloWorld' then
    Halt(6);
end.

