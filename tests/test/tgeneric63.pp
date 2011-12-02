{ %NORUN }

{ this tests that nested non-generic structured types can be used inside
  generics - here: record in record }
program tgeneric63;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = record
  type
    TTestSub = record
      class function Test(a: T): T; static;
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
