{ %FAIL }

{ helpers may not be referenced in any way - test 9 }
program thlp23;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
    procedure Test;
  end;

procedure TObjectHelper.Test;
begin

end;

var
  o: TObject;
begin
  TObjectHelper(o).Test;
end.
