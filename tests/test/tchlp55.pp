{ %NORUN }

{ This tests that methods introduced by a helper can be found in 
  with-Statements as well - Case 1: normal method in current helper }
program tchlp55;

{$mode objfpc}

type
  TObjectHelper = class helper for TObject
    procedure Test;
  end;

procedure TObjectHelper.Test;
begin
end;

begin
  with TObject.Create do
    Test;
end.
