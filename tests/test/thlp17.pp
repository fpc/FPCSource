{ %FAIL }

{ helpers may not be referenced in any way - test 3 }
program thlp17;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TObjectHelper = class helper for TObject
    class procedure Test;
  end;

class procedure TObjectHelper.Test;
begin

end;

begin
  TObjectHelper.Test;
end.

