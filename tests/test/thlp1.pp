{ %NORUN }

{ tests the inheritance syntax of helpers }
program thlp1;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

  TObjectHelperSub = class helper(TObjectHelper) for TObject
  end;

begin
end.
