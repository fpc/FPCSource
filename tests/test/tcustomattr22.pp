{ %FAIL }

program tcustomattr22;

{$mode objfpc}
{$modeswitch prefixedattributes}

type
  TTestAttribute = class(TCustomAttribute)
    constructor Create(aArg: LongInt);
  end;

  [TTestAttribute(42), TTestAttribute]
  TMyTest = class

  end;

constructor TTestAttribute.Create(aArg: LongInt);
begin

end;

begin
end.
