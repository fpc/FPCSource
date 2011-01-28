{ %FAIL }

{ puplished members are not allowed in mode objfpc }
program tchlp41;

{$mode objfpc}

type
  {$M+}
  TFoo = class
  end;
  {$M-}

  TFooHelper = class helper for TFoo
  published
    function Test: Integer;
  end;

function TFooHelper.Test: Integer;
begin
  Result := 1;
end;

begin
end.
