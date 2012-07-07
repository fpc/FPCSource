{ %FAIL }

program tgeneric85;

{$mode objfpc}

type
  generic TTest<T> = record
  end;

const
  Test: ^TTest = Nil;

begin

end.
