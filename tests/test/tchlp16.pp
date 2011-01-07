{ %FAIL }

{ class helpers may not be referenced in any way - test 1 }
program tchlp16;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

var
  o: TObjectHelper;
begin
end.

