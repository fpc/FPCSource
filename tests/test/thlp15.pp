{ %FAIL }

{ helpers may not be referenced in any way - test 1 }
program thlp15;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

var
  o: TObjectHelper;
begin
end.

