{ %FAIL }

{ it's forbidden for a class helper to extend a record }
program tchlp11;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record

  end;

  TTestHelper = class helper for TTest
  end;

begin

end.
