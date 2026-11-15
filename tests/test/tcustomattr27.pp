{ %FAIL }

program tcustomattr27;

{ without the modeswitch a parameter takes no attribute }

{$mode objfpc}

type
  FlagAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

constructor FlagAttribute.Create;
begin
end;

procedure P([Flag] a: Integer);
begin
end;

begin
end.
