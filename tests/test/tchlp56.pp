{ %NORUN }

{ This tests that methods introduced by a helper can be found in 
  with-Statements as well - Case 2: class method in current helper }
program tchlp56;

{$mode objfpc}

type
  TObjectHelper = class helper for TObject
    class procedure Test;
  end;

class procedure TObjectHelper.Test;
begin
end;

begin
  with TObject do
    Test;
end.
