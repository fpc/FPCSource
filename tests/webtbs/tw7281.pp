program test_intf;

{$mode objfpc}{$H+}

uses
  SysUtils;

type
  ITest = interface
    procedure DoIt(AMsg : string);
  end;

  { TTest }

  TTest = class(TInterfacedObject,ITest)
  protected
    procedure DoIt(AMsg : string);
  public
    constructor Create();
    destructor Destroy();override;
  end;

var InstancesCount : Integer = 0;

{ TTest }

procedure TTest.DoIt(AMsg: string);
begin
  WriteLn(AMsg);
end;

constructor TTest.Create();
begin
  Inherited;
  Inc(InstancesCount);
  WriteLn('Creating >>> ',HexStr(PtrUInt(self),sizeof(PtrUInt)*2));
end;

destructor TTest.Destroy();
begin
  Dec(InstancesCount);
  WriteLn('Destroying >>> ',HexStr(PtrUInt(self),sizeof(PtrUInt)*2));
  inherited Destroy();
end;

procedure proc1(ATest : ITest);
begin
  ATest.DoIt('  called in proc1');
end;

procedure test();
begin
  (TTest.Create() as ITest).DoIt('  called in test');
  proc1(TTest.Create() as ITest);
  proc1(TTest.Create() as ITest);
  proc1(TTest.Create() as ITest);
  proc1(TTest.Create() as ITest);
end;

begin
  test();
  WriteLn('Remaining instances ... ',InstancesCount);
  if InstancesCount<>0 then
    halt(1);
end.

