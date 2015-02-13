{ %NORUN }

program tw27424;

{$mode objfpc}

type
  TType = class(TObject)
  end;

  generic TTest<T1; T2: TType> = class(TObject)
  end;

  TFoo = class(TType)
  end;

  TBar = class(specialize TTest<string, TFoo>)
  end;

begin

end.
