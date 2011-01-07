{ %FAIL }

{ class helpers may not be referenced in any way - test 2 }
program tchlp17;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

begin
  with TObjectHelper.Create do ;
end.

