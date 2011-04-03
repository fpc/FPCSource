{ %NORUN }

{ a helper may contain generic methods }
program thlp29;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class

  end;

  TTestHelper = class helper for TTest
    function Test<T>: T;
  end;

function TTestHelper.Test<T>: T;
begin

end;

begin

end.
