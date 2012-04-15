{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: class only
  Note: This tests a different code path than in the compiler than tgeneric36! }
program tgeneric42;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = class

  end;

  TTest = class

  end;

  TTest<T, S> = class

  end;

begin

end.
