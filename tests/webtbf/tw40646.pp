{ %fail }
program Project1;
type
  generic TFoo<_A: TObject> = class
    type x = type _A;
    type b = class(x)
    end;
  end;

  TBar = class sealed
  end;

  TAbc = specialize TFoo<TBar>;

begin
end.
