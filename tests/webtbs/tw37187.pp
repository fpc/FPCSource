{ %NORUN }

program tw37187;

{$mode objfpc}

type
  generic TTest<T: class> = class
    arr: array[0..SizeOf(T)] of Byte;
  end;

  generic TTest2<T: class> = class
  public type
    TTestT = specialize TTest<T>;
  end;

begin

end.
