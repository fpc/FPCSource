{$Mode ObjFPC}{$H+}

type
  generic TTest<T:class> = class(T)
  public
    function Foo:Integer;override;
  end;

function TTest.Foo:Integer;
begin
  Result:=42;
end;

begin
end.
