{ %FAIL }

{ constructors without arguments are not allowed }

program tthlp15;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TLongIntHelper = type helper for LongInt
    constructor Create;
  end;

constructor TLongIntHelper.Create;
begin
  Self := 2;
end;

begin
end.
