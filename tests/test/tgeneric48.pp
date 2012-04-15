{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: a mix }
program tgeneric48;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class
  end;

  TTest<T> = record
  end;

  TTest<T, S> = interface
  end;

  TTest<T, S, R> = procedure;

  TTest<T, S, R, Q> = array of Integer;

  TTest<T, S, R, Q, P> = procedure of object;

begin

end.
