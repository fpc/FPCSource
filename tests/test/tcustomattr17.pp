{ %FAIL }

program tcustomattr17;

{$mode objfpc}
{$modeswitch prefixedattributes}

type
  TTest = class(TCustomAttribute)
    constructor Create;
  end;

  [TTest]
  Int = Integer;

constructor TTest.Create;
begin

end;

begin
end.
