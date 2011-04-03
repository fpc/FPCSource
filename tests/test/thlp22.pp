{ %FAIL }

{ helpers may not be referenced in any way - test 8 }
program thlp22;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

  TObjectHelperSub = class(TObjectHelper)
  end;

begin
end.

