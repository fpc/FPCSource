{ %FAIL }

program tw40653;

{$mode objfpc}
type

  TFoo = class
    type
    TBar = class(TFoo)
    end;
  public
    procedure p1; virtual;
  end;

  TTest = class(TFoo.tBar)
    procedure p2; virtual;
  end;

procedure TTest.p2;
begin {} end;

procedure TFoo.p1;
begin {} end;

var
  a: TFoo;

begin
  a := TTest.Create;
end.

