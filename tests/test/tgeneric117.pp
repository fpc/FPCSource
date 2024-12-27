{ %NORUN }

program tgeneric117;

{$mode delphi}

type
  TTest<T> = class
  public type
    TT = ^TTest<T>;
  end;

  TTest2<T> = class
  public type
    TT2 = TTest2<T>;
    TT = TTest<TT2>;
  end;

begin

end.
