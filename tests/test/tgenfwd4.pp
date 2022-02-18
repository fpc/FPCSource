{ %FAIL }

program tgenfwd4;

{$mode objfpc}

type
  generic TTest<T: class> = class;

  generic TTest<T> = class
  end;

begin

end.
