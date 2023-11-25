{$mode objfpc}
{$modeswitch nestedprocvars}

type
  TNestedProc = procedure is nested;

IInf = interface
    procedure InfMethod(AParam: TNestedProc);
end;

TObj = class(TInterfacedObject, IInf)
  procedure InfMethod(AParam: TNestedProc);
end;

const
  ok: boolean = false;

procedure TObj.InfMethod(AParam: TNestedProc);
begin
  aParam
end;

procedure test(const i: IInf);

  procedure nest;
    begin
      ok:=true;
    end;

begin
  i.InfMethod(@nest);
end;

var
  i: IInf;
begin
  i:= tobj.create;
  test(i);
  i:=nil;
  halt(ord(not ok));
end.
