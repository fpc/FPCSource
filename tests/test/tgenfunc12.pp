program tgenfunc12;

{$mode objfpc}

type
  TTest = class
    generic function Test<T: class>: T;
  end;

generic function TTest.Test<T>: T;
begin
  Result := T.Create;
end;

generic function Test<T: IInterface>: T;
begin
  Result := TInterfacedObject.Create;
end;

var
  t: TTest;
begin
  t := TTest.Create;
  t.specialize Test<TObject>.Free;
  specialize Test<IInterface>;
end.
