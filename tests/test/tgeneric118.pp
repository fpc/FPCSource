{ %NORUN }

program tgeneric117;

{$mode objfpc}

type
  generic TTest<T> = class
  public type
    TT = ^specialize TTest<T>;
  end;

  generic TTest2<T> = class
  public type
    TT2 = specialize TTest2<T>;
    TT = specialize TTest<TT2>;
  end;

begin

end.
