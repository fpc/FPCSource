{ %FAIL }

program tgenfwd6;

{$mode objfpc}

type
  generic TTest<T: TObject> = class;

  generic TTest<T: IInterface> = class
  end;

begin

end.
