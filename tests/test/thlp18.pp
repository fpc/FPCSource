{ %FAIL }

{ helpers may not be referenced in any way - test 4 }
program thlp18;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
  end;

procedure SomeProc(aHelper: TObjectHelper);
begin

end;

begin
end.

