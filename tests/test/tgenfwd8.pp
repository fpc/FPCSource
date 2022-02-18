{ %FAIL }

program tgenfwd8;

{$mode objfpc}

type
  generic TTest<T> = class;

  generic TTest<S> = class
  end;

begin

end.
