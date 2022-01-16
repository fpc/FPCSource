{ %NORUN }

program tb0624;

{$mode delphi}

type
  TTest<T> = record
  public type
    PSelf = ^TTest<T>;
  public
    Next: PSelf;
  end;

  TTest2<T> = record
    Next: ^TTest<T>;
  end;

  TTestLongInt = TTest<LongInt>;
  TTestString = TTest<String>;

  TTest2LongInt = TTest2<LongInt>;
  TTest2String = TTest2<String>;

begin

end.
