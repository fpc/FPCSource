{ %NORUN }

{ this tests that methods defined in the parent of a helper are available in the
  child helper as well }
program thlp44;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
    class procedure Test;
  end;

  TObjectHelperSub = class helper(TObjectHelper) for TObject
    class procedure DoTest;
  end;

class procedure TObjectHelper.Test;
begin

end;

class procedure TObjectHelperSub.DoTest;
begin
  Test;
end;

begin
  TObject.DoTest;
end.
