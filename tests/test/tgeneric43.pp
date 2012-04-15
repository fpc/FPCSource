{ %NORUN }

{ in mode Delphi generic types might be overloaded - here: record only
  Note: This tests a different code path than in the compiler than tgeneric37! }
program tgeneric43;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TTest<T> = record

  end;

  TTest = record

  end;

  TTest<T, S> = record

  end;

begin

end.
