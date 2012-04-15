{ this tests that simple inline specializations work }
program tgeneric51;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest<T> = class
    function Test(a: T): T;
    class function ClassTest(a: T): T;
  end;

function TTest<T>.Test(a: T): T;
begin
  Result := a;
end;

class function TTest<T>.ClassTest(a: T): T;
begin
  Result := a;
end;

var
  t: TTest<Integer>;
  res: Integer;
begin
  t := TTest<Integer>.Create;
  res := t.Test(42);
  Writeln('t.Test: ', res);
  if res <> 42 then
    Halt(1);
  res := TTest<Integer>.ClassTest(42);
  Writeln('t.ClassTest: ', res);
  if res <> 42 then
    Halt(2);
  Writeln('ok');
end.
