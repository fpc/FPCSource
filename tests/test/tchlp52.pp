{ %FAIL }

{ class helpers may not be referenced in any way - test 7 }
program tchlp52;

{$mode objfpc}

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
