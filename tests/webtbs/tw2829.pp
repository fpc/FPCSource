{ Source provided for Free Pascal Bug Report 2829 }
{ Submitted by "marco (the gory bugs department)" on  2003-12-04 }
{ e-mail:  }
{$ifdef fpc}{$mode Delphi}{$endif}

type
  TFloat64     = Double;
  TFloat64Func = function: TFloat64;

  TExprVirtMachOp = class end;
  TExprVirtMachNode= class end;
  TExprCallFloat64VmOp = class(TExprVirtMachOp)
  private
    FFunc: TFloat64Func;
  public
    constructor Create(AFunc: TFloat64Func);
  end;


    TExprCallFloat64VmNode = class(TExprVirtMachNode)
  private
    FFunc: TFloat64Func;
    FExprVmCode : TExprCallFloat64VMOp;
  public
    procedure GenCode;
  end;

constructor TExprCallFloat64VmOp.Create(AFunc:TFloat64Func);

begin

end;

procedure TExprCallFloat64VmNode.GenCode;
begin
  FExprVmCode := TExprCallFloat64VmOp.Create(FFunc);
end;

begin
end.
