{$MODE objfpc}

type
  IMyInterface = interface
    procedure Test(a, b: Integer);
  end;
  TMyBaseClass = class(TInterfacedObject, IMyInterface)
    procedure Test(a, b: Integer); virtual; abstract;
  end;
  TDescendent = class(TMyBaseClass)
    procedure Test(a, b: Integer); override;
  end;

var
  global_a, global_b: Integer;

procedure TDescendent.Test(a, b: Integer);
begin
  global_a := a;
  global_b := b;
end;

var
  q: IMyInterface;
begin
  q := TDescendent.Create;
  q.Test(18, 42);
  if (global_a <> 18) or (global_b <> 42) then
    halt(1);
  Writeln('Ok!');
end.
