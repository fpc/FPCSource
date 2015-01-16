{ %NORUN }

program tgeneric98;

{$mode objfpc}

type
  generic TTest<T> = class
  public type
    TAlias = T;
  private
    fField: TAlias;
    procedure SetField(aValue: TAlias);
  public
    property Field: TAlias read fField write SetField;
    function CalcField: TAlias;
  end;

  generic TTest2<T> = class
  public type
    TTestT = specialize TTest<T>;
  private
    fField: TTestT.TAlias;
    procedure SetField(aValue: TTestT.TAlias);
  public
    property Field: TTestT.TAlias read fField write SetField;
    function CalcField: TTestT.TAlias;
  end;

procedure TTest.SetField(aValue: TAlias);
begin
end;

function TTest.CalcField: TAlias;
begin
  Result := Default(TAlias);
end;

procedure TTest2.SetField(aValue: TTestT.TAlias);
begin
end;

function TTest2.CalcField: TTestT.TAlias;
begin
  Result := Default(TTestT.TAlias);
end;

begin

end.
