{ %FAIL }

{ it's not allowed for a record helper to extend a class }
program trhlp13;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTestHelper = record helper for TObject
  end;

begin
end.
