{ Record composition: an unnamed composition of a record with an interface
  field, finalizing a local composing record releases the interface }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TTestObj = class(TInterfacedObject)
  public
    destructor Destroy; override;
  end;

  TChildRec = record
    Intf: IInterface;
  end;

  TComposed = record
    A: Integer;
    contains TChildRec;
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
  c: TComposed;
begin
  c.A := 1;
  c.Intf := TTestObj.Create;
  c.B := 2;
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
