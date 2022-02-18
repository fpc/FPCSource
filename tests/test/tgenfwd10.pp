{ %FAIL }

program tgenfwd10;

{$mode objfpc}

type
  generic TTest<const N: Integer> = class;

  generic TTest<const N: Byte> = class
  end;

begin

end.
