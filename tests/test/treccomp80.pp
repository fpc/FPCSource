{ Record composition: a generic record composing its type parameter, specialized
  with a record with an interface field, copying increases the reference count
  and finalizing a local specialization releases the interface }
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

  generic TComposed<T> = record
    A: Integer;
    contains T;
    B: Integer;
  end;

  TSpecComposed = specialize TComposed<TChildRec>;

var
  Destroyed: Boolean = False;

destructor TTestObj.Destroy;
begin
  Destroyed := True;
  inherited Destroy;
end;

procedure Test;
var
  c1: TSpecComposed;
  c2: TSpecComposed;
  Obj: TTestObj;
begin
  Obj := TTestObj.Create;
  c1.Intf := Obj;
  c2 := c1;
  WriteLn('RefCount: ', Obj.RefCount);
  if Obj.RefCount<>2 then
    halt(1);
  c1.Intf := nil;
  if Destroyed then
    halt(2);
end;

begin
  Test;
  if not Destroyed then
  begin
    WriteLn('interface was not released');
    halt(3);
  end;
  WriteLn('ok');
end.
