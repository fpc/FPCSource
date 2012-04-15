{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: interface only }
program tgeneric38;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = interface

  end;

  TTest<T> = interface

  end;

  TTest<T, S> = interface

  end;

begin

end.
