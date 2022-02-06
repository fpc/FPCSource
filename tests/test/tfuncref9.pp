program tfuncref9;

{$mode delphi}
{$modeswitch functionreferences}

{ test assigning global procedures, methods, and object methods to function references }

type
  TProc = reference to procedure;

procedure CallProc(AProc: TProc);
begin
  AProc();
end;

type
  TTest = class
    class procedure ClassMethod;
    procedure InstanceMethod;
  end;

var
  Acc: Integer;

procedure GlobalProc;
begin
  Inc(Acc);
end;

class procedure TTest.ClassMethod;
begin
  Inc(Acc, 10);
end;

procedure TTest.InstanceMethod;
begin
  Inc(Acc, 100);
end;

var
  Proc: TProc;
  Obj: TTest;
begin
  Proc := GlobalProc;
  Proc();
  CallProc(GlobalProc);

  Proc := TTest.ClassMethod;
  Proc();
  CallProc(TTest.ClassMethod);

  Obj := TTest.Create;
  Proc := Obj.InstanceMethod;
  Proc();
  CallProc(Obj.InstanceMethod);
  Obj.Free;

  if Acc <> 222 then
    halt(Acc);
end.

