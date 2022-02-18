{ %FAIL }

program tgenfwd3;

{$mode objfpc}

type
  generic TTest<T> = class;

  generic TTest<T: class> = class
  end;

begin

end.
