unit ugeneric14;

{$mode objfpc}

interface

type
  generic TGTest<T> = class
  public
    data: T;
    procedure DoSomething;
  end;

implementation

function Foo: Integer;
begin
  writeln('foo');
  Result := 1;
end;

procedure TGTest.DoSomething;
begin
  data := Foo;
end;

end.
