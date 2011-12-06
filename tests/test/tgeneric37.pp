{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: record only }
program tgeneric37;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = record

  end;

  TTest<T> = record

  end;

  TTest<T, S> = record

  end;

begin

end.
