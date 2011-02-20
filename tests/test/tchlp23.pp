{ %NORUN }

{ overloading needs to be enabled explicitly }
program tchlp23;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TFoo = class
    procedure Test(const aTest: String);
  end;

  TFooHelper = class helper for TFoo
    procedure Test; overload;
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

