{ %NORUN }

program tb0623;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  generic TTest<T> = record
  public type
    PSelf = ^specialize TTest<T>;
  public
    Next: PSelf;
  end;

  generic TTest2<T> = record
    Next: ^specialize TTest2<T>;
  end;

  TTestLongInt = specialize TTest<LongInt>;
  TTestString = specialize TTest<String>;

  TTest2LongInt = specialize TTest2<LongInt>;
  TTest2String = specialize TTest2<String>;

begin

end.
