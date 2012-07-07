{ %FAIL }

program tgeneric83;

{$mode delphi}

type
  TTest<T> = record
  end;

const
  Test: ^TTest = Nil;

begin

end.
