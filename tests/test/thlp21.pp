{ %FAIL }

{ helpers may not be referenced in any way - test 7 }
program thlp21;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

  TObjectHelperHelper = record helper for TObjectHelper
  end;

begin
end.

