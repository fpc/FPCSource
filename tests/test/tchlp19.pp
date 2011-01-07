{ %FAIL }

{ class helpers may not be referenced in any way - test 4 }
program tchlp19;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

procedure SomeProc(aHelper: TObjectHelper);
begin

end;

begin
end.

