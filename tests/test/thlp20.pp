{ %FAIL }

{ helpers may not be referenced in any way - test 6 }
program thlp20;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

  TObjectHelperHelper = class helper for TObjectHelper
  end;

begin
end.

