{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: arrays only
  Note: This tests a different code path than in the compiler than tgeneric40! }
program tgeneric40;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = array of Integer;

  TTest = array of Integer;

  TTest<T, S> = array of Integer;

begin

end.
