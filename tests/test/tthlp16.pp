{ %FAIL }

{ constructors with only default arguments are not allowed }

program tthlp16;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TLongIntHelper = type helper for LongInt
    constructor Create(aArg1: LongInt = 0; aArg2: LongInt = 0);
  end;

constructor TLongIntHelper.Create(aArg1: LongInt = 0; aArg2: LongInt = 0);
begin
  Self := 2;
end;

begin
end.
