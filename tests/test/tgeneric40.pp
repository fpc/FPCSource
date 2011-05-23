{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: arrays only }
program tgeneric40;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = array of Integer;

  TTest<T> = array of Integer;

  TTest<T, S> = array of Integer;

begin

end.
