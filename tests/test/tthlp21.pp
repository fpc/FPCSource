{ %FAIL }

{ type helpers are not parsed if modeswitch typehelpers is not set }

program tthlp21;

type
  TTest = type helper for LongInt
  end;

begin

end.
