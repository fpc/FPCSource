{ %FAIL }

{ type helpers are not parsed if modeswitch typehelpers is not set }

program tthlp20;

type
  TTest = type helper for LongInt
  end;

begin

end.
