{ %fail}
{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  TClassWithFinalMethod = class
  public
    procedure TestFinal; final;
  end;

procedure TClassWithFinalMethod.TestFinal;
begin
end;

begin
end.