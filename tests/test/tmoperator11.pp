program tmoperator11;

{$MODE DELPHI}

var
  i: Integer = 0;

type
  TFoo = record
    class operator Initialize(var aFoo: TFoo);
    procedure Foo;
  end;

class operator TFoo.Initialize(var aFoo: TFoo);
begin
  Inc(i);
end;

procedure TFoo.Foo;
begin
end;

var
  f: TFoo;
begin
  if i <> 1 then
    Halt(1);
  f.Foo;
end.