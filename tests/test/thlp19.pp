{ %FAIL }

{ helpers may not be referenced in any way - test 5 }
program thlp19;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

  TSomeRec = record
    helper: TObjectHelper;
  end;

begin
end.

