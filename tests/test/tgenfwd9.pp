{ %NORUN }

program tgenfwd9;

{$mode objfpc}

type
  TFoo = class
    procedure Bar;
  end;

  TSomeClass = class
  public type
    generic TTest<T> = class;

    TSomeNestedClass = class
      f: specialize TTest<TFoo>;
    end;

    generic TTest<T> = class
      f: T;
    end;

  var
    s: TSomeNestedClass;
  end;

procedure TFoo.Bar;
begin
end;

var
  s: TSomeClass;
begin
  s.s.f.f.Bar;
end.
