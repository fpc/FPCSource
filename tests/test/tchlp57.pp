{ %NORUN }

{ This tests that methods introduced by a helper can be found in 
  with-Statements as well - Case 3: normal method in parent's helper }
program tchlp57;

{$mode objfpc}

type
  TObjectHelper = class helper for TObject
    procedure Test;
  end;

  TTest = class
  end;

procedure TObjectHelper.Test;
begin
end;

begin
  with TTest.Create do
    Test;
end.
