{ %fail}
{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  TClassWithFinalMethod = class
  public
    procedure TestFinal; virtual; final;
  end;

  TClassTryOverrideFinal = class(TClassWithFinalMethod)
  public
    procedure TestFinal; override;
  end;

procedure TClassWithFinalMethod.TestFinal;
begin
end;

procedure TClassTryOverrideFinal.TestFinal;
begin
end;

begin
end.