{ %FAIL }

{ this tests that the dummy symbol that is introduced for generic "overloads"
  can not be used when it shouldn't be - Test 1 }
program tgeneric55;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = class

  end;

var
  t: TTest;
begin
end.
