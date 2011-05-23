{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: interface only
  Note: This tests a different code path than in the compiler than tgeneric38! }
program tgeneric44;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = interface

  end;

  TTest = interface

  end;

  TTest<T, S> = interface

  end;

begin

end.
