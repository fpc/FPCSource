{ %NORUN }

program tgenfwd1;

{$mode objfpc}

type
  generic TGen1<T> = class;
  generic TGen2<T: class> = class;
  generic TGen3<const N: Integer> = class;
  generic TGen4<T; S: class; const N: Integer> = class;

  TTest = class
    f1: specialize TGen1<LongInt>;
    f2: specialize TGen2<TObject>;
    f3: specialize TGen3<42>;
    f4: specialize TGen4<LongInt, TObject, 42>;

    { this will reuse the above specializations }
    f5: specialize TGen1<LongInt>;
    f6: specialize TGen2<TObject>;
    f7: specialize TGen3<42>;
    f8: specialize TGen4<LongInt, TObject, 42>;
  end;

  generic TGen1<T> = class
  end;

  generic TGen2<T: class> = class
  end;

  generic TGen3<const N: Integer> = class
  end;

  generic TGen4<T; S: class; const N: Integer> = class
  end;

begin

end.
