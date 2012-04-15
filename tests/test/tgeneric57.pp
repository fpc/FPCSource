{ %FAIL }

{ this tests that the dummy symbol that is introduced for generic "overloads"
  can not be used when it shouldn't be - Test 3 }
program tgeneric57;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = class

  end;

var
  t: TObject;
begin
  t := TTest.Create;
end.

