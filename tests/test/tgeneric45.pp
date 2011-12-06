{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: procvars only
  Note: This tests a different code path than in the compiler than tgeneric39! }
program tgeneric45;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = procedure;

  TTest = procedure;

  TTest<T, S> = procedure;

begin

end.
