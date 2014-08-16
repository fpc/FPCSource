{ %FAIL }

{ this tests that procedure variables (among others) can not be extended }

program tthlp10;

{$mode objfpc}
{$modeswitch typehelpers}

type
  TProcedure = procedure;

  TProcedureHelper = type helper for TProcedure

  end;

begin

end.
