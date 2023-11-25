{$mode objfpc}
{$modeswitch nestedprocvars}

unit unest6a;

interface

type
  TNestedProc = procedure is nested;

IInf = interface
    procedure InfMethod(AParam: TNestedProc);
end;

TObj = class(TInterfacedObject, IInf)
  procedure InfMethod(AParam: TNestedProc);
end;

implementation

procedure TObj.InfMethod(AParam: TNestedProc);
begin
  aParam
end;

end.
