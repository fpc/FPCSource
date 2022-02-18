program tgenfwd2;

{$mode delphi}

type
  TGen1<T> = class;
  TGen2<T: class> = class;
  TGen3<const N: Integer> = class;
  TGen4<T; S: class; const N: Integer> = class;

  TTest = class
    f1: TGen1<LongInt>;
    f2: TGen2<TObject>;
    f3: TGen3<42>;
    f4: TGen4<LongInt, TObject, 42>;

    { this will reuse the above specializations }
    f5: TGen1<LongInt>;
    f6: TGen2<TObject>;
    f7: TGen3<42>;
    f8: TGen4<LongInt, TObject, 42>;
  end;

  TGen1<T> = class
  end;

  TGen2<T: class> = class
  end;

  TGen3<const N: Integer> = class
  end;

  TGen4<T; S: class; const N: Integer> = class
  end;

begin

end.
