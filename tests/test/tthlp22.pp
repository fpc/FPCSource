{ %NORUN }

{ type helpers are not parsed if modeswitch typehelpers is not set (mode Delphi) }

program tthlp22;

{$mode delphi}

type
  TTest = record helper for LongInt
  end;

begin

end.
