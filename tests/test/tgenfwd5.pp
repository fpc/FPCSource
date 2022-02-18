{ %FAIL }

program tgenfwd5;

{$mode objfpc}

type
  TSomeClass = class
  end;

  generic TTest<T: TObject> = class;

  generic TTest<T: TSomeClass> = class
  end;

begin

end.
