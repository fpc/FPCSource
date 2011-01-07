{ %FAIL }

{ class helpers may not be referenced in any way - test 5 }
program tchlp20;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

  TSomeRec = record
    helper: TObjectHelper;
  end;

begin
end.

