{ %NORUN }

program tgenfwd13;

{$mode objfpc}

type
  generic ITest<T> = interface;

  generic ISomeIntf<T> = interface
    procedure Something(aTest: specialize ITest<T>);
  end;

  generic ITest<T> = interface
  end;

begin

end.
