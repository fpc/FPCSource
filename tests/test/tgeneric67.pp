{ %NORUN }

{ this tests that nested non-generic structured types can be used inside
  generics - here: class in object }
program tgeneric66;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = object
  type
    TTestSub = class
      class function Test(a: T): T;
    end;
  end;

class function TTest<T>.TTestSub.Test(a: T): T;
begin
  Result := a;
end;

var
  t: TTest<Integer>.TTestSub;
begin
  TTest<Integer>.TTestSub.Test(42);
end.
