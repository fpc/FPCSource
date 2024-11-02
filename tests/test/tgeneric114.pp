{ %FAIL }
{$Mode ObjFPC}{$H+}

type
  TBase = class
  public
  end;

  generic TTest<T:class> = class(T)
  public
    function Foo:Integer;override;
  end;

function TTest.Foo:Integer;
begin
  Result:=42;
end;

var
  b: TBase;
begin
  b:=specialize TTest<TBase>.Create;
  if b.Foo<>42 then
    Halt(1);
  WriteLn('Ok');
end.
