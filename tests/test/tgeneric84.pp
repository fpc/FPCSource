{ %FAIL }

program tgeneric84;

{$mode objfpc}

type
  generic TTest<T> = record
  end;

  PTest = ^TTest;

begin
end.
