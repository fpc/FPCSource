program tw20995a;

{$mode delphi}{$H+}

type
  ITest<T> = interface
  end;

  TTest<T> = class
  type
    IGenTest = ITest<T>;
  private
    FData: array of IGenTest;
  end;

  TObjTest = TTest<TObject>;

begin
end.

