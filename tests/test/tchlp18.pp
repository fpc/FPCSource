{ %FAIL }

{ class helpers may not be referenced in any way - test 3 }
program tchlp18;

{$ifdef fpc}
  {$mode objfpc}
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

