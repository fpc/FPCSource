{ %FAIL }

{ this tests that the dummy symbol that is introduced for generic "overloads"
  can not be used when it shouldn't be - Test 4 }
program tgeneric58;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  generic TTest<T> = class

  end;

var
  t: TObject;
begin
  t := TTest.Create;
end.

