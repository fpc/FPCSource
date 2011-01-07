{ in mode ObjFPC overloading is enabled by default }
program tchlp23;

{$mode objfpc}

type
  TFoo = class
    procedure Test(const aTest: String);
  end;

  TFooHelper = class helper for TFoo
    procedure Test;
  end;

procedure TFoo.Test(const aTest: String);
begin

end;

procedure TFooHelper.Test;
begin

end;

var
  f: TFoo;
begin
  f := TFoo.Create;
  f.Test;
  f.Test('Foo');
end.

