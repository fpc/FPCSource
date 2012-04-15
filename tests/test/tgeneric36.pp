{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: class only }
program tgeneric36;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest = class

  end;

  TTest<T> = class

  end;

  TTest<T, S> = class

  end;

begin

end.
