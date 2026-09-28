{ Record composition: nested unnamed compositions with an interface field in the
  innermost record, finalizing a local outer record releases the interface }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TTestObj = class(TInterfacedObject)
  public
    destructor Destroy; override;
  end;

  TInnerRec = record
    Intf: IInterface;
  end;

  TMiddleRec = record
    M: Integer;
    contains TInnerRec;
  end;

  TOuterRec = record
    A: Integer;
    contains TMiddleRec;
    B: Integer;
  end;

var
  Destroyed: Boolean = False;

destructor TTestObj.Destroy;
begin
  Destroyed := True;
  inherited Destroy;
end;

procedure Test;
var
  c: TOuterRec;
begin
  c.A := 1;
  c.M := 2;
  c.Intf := TTestObj.Create;
  c.B := 3;
  if Destroyed then
    halt(1);
end;

begin
  Test;
  if not Destroyed then
  begin
    WriteLn('interface was not released');
    halt(2);
  end;
  WriteLn('ok');
end.
