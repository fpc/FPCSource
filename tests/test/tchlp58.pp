{ %NORUN }

{ This tests that methods introduced by a helper can be found in 
  with-Statements as well - Case 4: class method in parent's helper }
program tchlp58;

{$mode objfpc}

type
  TObjectHelper = class helper for TObject
    class procedure Test;
  end;

  TTest = class
  end;

class procedure TObjectHelper.Test;
begin
end;

begin
  with TTest do
    Test;
end.
