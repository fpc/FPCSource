{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: method vars only
  Note: This tests a different code path than in the compiler than tgeneric39! }
program tgeneric47;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = procedure of object;

  TTest = procedure of object;

  TTest<T, S> = procedure of object;

begin

end.
