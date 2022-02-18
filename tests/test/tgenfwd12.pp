{ %FAIL }

program tgenfwd12;

{$mode objfpc}

type
  generic TTest<const N: Integer> = class;

  generic TTest<const N: Single> = class
  end;

begin

end.
