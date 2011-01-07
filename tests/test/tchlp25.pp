{ %FAIL }

{ class helpers may not contain any fields }
program tchlp25;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
    Test: Integer;
  end;

begin
end.

