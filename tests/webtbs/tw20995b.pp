program tw20995b;

{$mode delphi}{$H+}

type
  ITest<T> = interface
  end;

  TTest<T> = class
  private
    FData: array of ITest<T>;
  end;

  TObjTest = TTest<TObject>;

begin
end.

