{ %FAIL }

program tgenfwd7;

{$mode objfpc}

type
  generic TTest<T: TObject> = class;

  generic TTest<const N: Integer> = class
  end;

begin

end.
