{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: procvars only }
program tgeneric39;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = procedure;

  TTest<T> = procedure;

  TTest<T, S> = procedure;

begin

end.
