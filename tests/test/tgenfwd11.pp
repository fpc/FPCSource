{ %FAIL }

program tgenfwd11;

{$mode objfpc}

type
  generic TTest<const N: Integer> = class;

  generic TTest<const M: Integer> = class
  end;

begin

end.
