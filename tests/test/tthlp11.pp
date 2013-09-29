{ %FAIL }

{ this tests that procedure variables (among others) can not be extended }

program tthlp11;

{$mode delphi}

type
  TProcedure = procedure;

  TProcedureHelper = record helper for TProcedure

  end;

begin

end.
