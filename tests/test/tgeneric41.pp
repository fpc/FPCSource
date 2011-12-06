{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: method vars only }
program tgeneric41;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = procedure of object;

  TTest<T> = procedure of object;

  TTest<T, S> = procedure of object;

begin

end.
