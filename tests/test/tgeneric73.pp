{ %NORUN }

{ This tests that nested types can reference each other inside a generic }
program tgeneric73;

{$mode objfpc}

type
  generic TTest<T> = class
  public type
    TSubClass1 = class

    end;

    TSubClass2 = class
      f: TSubClass1;
    end;
  end;

begin

end.
