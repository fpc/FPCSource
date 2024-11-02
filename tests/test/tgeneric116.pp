{$Mode ObjFPC}{$H+}

type
  TBase = class
  public
    function Foo:Integer;virtual;abstract;
  end;

  generic TTest<T:class> = class(T)
  public
  end;

  generic TTest2<T:class> = class(specialize TTest<T>)
  public
    function Foo:Integer;override;
  end;

function TTest2.Foo:Integer;
begin
  Result:=42;
end;

var
  b: TBase;
begin
  b:=specialize TTest2<TBase>.Create;
  if b.Foo<>42 then
    Halt(1);
  WriteLn('Ok');
end.
