{ %FAIL }

{ helper types can not be used with default }
program tdefault13;

{$mode objfpc}

type
  TTestHelper = class helper for TObject
  end;

begin
  Default(TTestHelper);
end.
