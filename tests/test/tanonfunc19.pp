program tanonfunc19;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ when the capture object is freed non-managed members aren't freed }

type
  TProc = reference to procedure;

  TTest1 = class
    f: LongInt;
    destructor Destroy; override;
  end;

  ITest = interface
    function GetValue: LongInt;
  end;

  TTest2 = class(TInterfacedObject, ITest)
    f: LongInt;
    function GetValue: LongInt;
    destructor Destroy; override;
  end;

var
  Test1Destr: Boolean = False;
  Test2Destr: Boolean = False;

destructor TTest1.Destroy;
begin
  Test1Destr := True;
  inherited;
end;

function TTest2.GetValue: LongInt;
begin
  Result := f;
end;

destructor TTest2.Destroy;
begin
  Test2Destr := True;
  inherited;
end;

var
  t: TTest1;

function Test: TProc;
var
  test1: TTest1;
  test2: TTest2;
  intf: ITest;
begin
  test1 := TTest1.Create;
  test1.f := 42;
  test2 := TTest2.Create;
  test2.f := 21;
  intf := test2;
  t := test1;
  Result := procedure
            begin
              test1.f := intf.GetValue;
            end;
end;

procedure DoTest;
var
  p: TProc;
begin
  p := Test;
  p();
  p := Nil;
end;

begin
  DoTest;
  if Test1Destr then
    Halt(1);
  if not Test2Destr then
    Halt(2);
  t.Free;
end.

