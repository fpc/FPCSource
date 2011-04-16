{ %FAIL }

{ helpers may not be referenced in any way - test 2 }
program thlp16;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

begin
  with TObjectHelper.Create do ;
end.

