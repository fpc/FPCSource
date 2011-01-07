{ %FAIL }

{ class helpers may not be referenced in any way - test 6 }
program tchlp21;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

  TObjectHelperHelper = class helper for TObjectHelper
  end;

begin
end.

